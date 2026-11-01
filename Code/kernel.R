# Kernel：项目级并发内核
#
#   计划管理        : kernel_plan()、kernel_off()、kernel_probe()、kernel_info()、
#                     kernel_active()、kernel_workers()
#   映射            : kernel_map()、kernel_map2()、kernel_imap()、kernel_pmap()、
#                     kernel_walk()
#
# 输入  : 无。用 source("Code/kernel.R", encoding = "UTF8") 加载。
# 输出  : 无。定义各函数和一个内部状态环境。
# 运行  : source("Code/kernel.R"); kernel_plan(workers = 4)
#         kernel_map(x, f); kernel_off()
# 策略  : 默认串行。并行路径使用 future + furrr，用 tryCatch()
#         包住每个任务，通过 do.call() 调用用户函数。串行路径
#         与 purrr 结果一致：值顺序、名字和 walk() 的不可见性。
# 修改 261101：kernel_plan() 的显式启用标志控制并行分派，
#   其他包安装的 future 计划因此保持串行（AGENTS 3.1 8）。
#   缺少 furrr 时后端候选链失败并回退到串行，
#   避免在第一次分派就中止。
#   失败任务的消息会在中止与告警文本中带引号列出。
# 风格  : yifan-r-code-style v3

# ---------- 常量 ----------------

# 支持的后端，按 kernel_plan() 尝试它们的顺序排列。
kernel_backends <- c("callr", "multisession", "sequential")

# 每个映射变体对应的 furrr 函数。walk 映射元素的方式与 map() 完全相同。
kernel_variant_fns <- c(
  map = "future_map",
  walk = "future_map",
  map2 = "future_map2",
  imap = "future_imap",
  pmap = "future_pmap"
)

# 一次映射调用允许的错误策略。
kernel_error_strategies <- c("stop", "warn", "collect")

# ---------- 内部状态 -----------------

# kernel_plan()、kernel_map() 和 kernel_off() 共用的计划簿记。
# option_backup 保存本文件改动的每个选项在启用内核之前的值。
# 只有 kernel_plan() 以并行后端成功之后 enabled 才为 TRUE，因此其他包
# 安装的计划无法自行打开内核。
.kernel_state <- new.env(parent = emptyenv())
.kernel_state$option_backup <- NULL
.kernel_state$enabled <- FALSE
.kernel_state$n_futures_last <- 0L
.kernel_state$elapsed_last <- 0
.kernel_state$payload_mb_last <- 0
.kernel_state$errors_last <- 0L

# ---------- 可用性与日志 -----------------------

# future 包已安装时返回 TRUE。
kernel_future_available <- function() {
  requireNamespace("future", quietly = TRUE)
}

# furrr 包已安装时返回 TRUE。
kernel_furrr_available <- function() {
  requireNamespace("furrr", quietly = TRUE)
}

# 一个后端在分派任何任务之前需要的包。sequential 不需要任何包。
kernel_backend_packages <- function(backend) {
  if (identical(backend, "callr")) {
    return(c("future.callr", "furrr"))
  }
  if (identical(backend, "multisession")) {
    return("furrr")
  }
  character(0)
}

# 一个必需包已安装时返回 TRUE。测试替换这个绑定点，
# 以便在不卸载包的前提下检验缺包回退。
kernel_package_available <- function(package) {
  requireNamespace(package, quietly = TRUE)
}

# 后端的包缺失时的说明文本；该后端可运行时返回 NULL。
kernel_backend_missing <- function(backend, available = kernel_package_available) {
  packages <- kernel_backend_packages(backend)
  missing <- packages[!vapply(packages, available, logical(1))]
  if (length(missing) == 0L) {
    return(NULL)
  }
  quoted <- paste(sprintf("'%s'", missing), collapse = " and ")
  if (length(missing) == 1L) {
    return(sprintf("package %s is not installed", quoted))
  }
  sprintf("packages %s are not installed", quoted)
}

# 要求输出进度行时返回 TRUE。内核默认静默，
# 以便调用方统计告警与消息条件。
kernel_verbose <- function() {
  isTRUE(getOption("pm25.kernel.verbose", FALSE))
}

# 把每个花括号加倍，让 str_glue() 按字面渲染文本。
kernel_escape_glue <- function(text) {
  text <- gsub("{", "{{", text, fixed = TRUE)
  gsub("}", "}}", text, fixed = TRUE)
}

# 已加载 Code/model.R 时用其中的 log_msg() 记一行日志；否则
# 回退到 message()。文本已格式化，所以花括号必须转义。
kernel_log <- function(level = c("INFO", "WARN", "ERROR"), text) {
  level <- match.arg(level)
  text <- kernel_escape_glue(text)
  if (exists("log_msg", mode = "function", inherits = TRUE)) {
    switch(
      level,
      INFO = log_msg(INFO, text),
      WARN = log_msg(WARN, text),
      ERROR = log_msg(ERROR, text)
    )
  } else {
    message(text)
  }
  invisible(NULL)
}

# ---------- 计划查看 ------------------

# 当前 future 计划的后端名称，归一化为受支持的名称。
kernel_backend_name <- function() {
  if (!kernel_future_available()) {
    return("sequential")
  }
  classes <- class(future::plan())
  if ("callr" %in% classes) {
    return("callr")
  }
  if ("multisession" %in% classes) {
    return("multisession")
  }
  if ("multicore" %in% classes) {
    return("multicore")
  }
  if ("cluster" %in% classes) {
    return("cluster")
  }
  if ("sequential" %in% classes || "uniprocess" %in% classes) {
    return("sequential")
  }
  tolower(classes[[1]])
}

# 实际生效的工作进程数。在 kernel 工作进程内部始终为 1。
kernel_workers <- function() {
  if (isTRUE(getOption("pm25.kernel.in_worker"))) {
    return(1L)
  }
  if (!kernel_future_available()) {
    return(1L)
  }
  as.integer(future::nbrOfWorkers())
}

# future 真正并行运行、内核已启用、当前不在工作进程内部时返回 TRUE。
# 其他包安装的计划在 kernel_plan() 运行之前保持未启用，
# 所以并发默认关闭（AGENTS 3.1 8）。
kernel_active <- function() {
  if (isTRUE(getOption("pm25.kernel.in_worker"))) {
    return(FALSE)
  }
  if (!isTRUE(.kernel_state$enabled)) {
    return(FALSE)
  }
  kernel_workers() > 1L
}

# 计划快照，加上最近一次 kernel_map() 系列调用的统计量。
kernel_info <- function() {
  list(
    backend = kernel_backend_name(),
    workers = kernel_workers(),
    active = kernel_active(),
    in_worker = isTRUE(getOption("pm25.kernel.in_worker")),
    n_futures_last = .kernel_state$n_futures_last,
    elapsed_last = .kernel_state$elapsed_last,
    payload_mb_last = .kernel_state$payload_mb_last,
    errors_last = .kernel_state$errors_last
  )
}

# ---------- 计划构建 --------------------

# 从参数、选项、环境变量或物理核数得到的工作进程整数。
kernel_resolve_workers <- function(workers) {
  source <- "argument workers"
  if (is.null(workers)) {
    workers <- getOption("pm25.kernel.workers")
    source <- "option pm25.kernel.workers"
  }
  if (is.null(workers)) {
    raw <- Sys.getenv("PM25_KERNEL_WORKERS", unset = "")
    if (nzchar(raw)) {
      workers <- suppressWarnings(as.integer(raw))
      source <- "environment variable PM25_KERNEL_WORKERS"
    }
  }
  if (is.null(workers)) {
    return(kernel_default_workers())
  }
  bad <- length(workers) != 1L || !is.numeric(workers) || is.na(workers) ||
    workers != floor(workers)
  if (bad) {
    cli::cli_abort(
      c(
        "Invalid worker count from {source}: {.val {workers}}.",
        "i" = "Expected one whole number."
      )
    )
  }
  as.integer(max(workers, 1L))
}

# 默认工作进程数：min(4, 物理核数)，带安全回退。
kernel_default_workers <- function() {
  cores <- suppressWarnings(parallel::detectCores(logical = FALSE))
  if (length(cores) != 1L || is.na(cores) || cores < 1L) {
    cores <- suppressWarnings(parallel::detectCores())
  }
  if (length(cores) != 1L || is.na(cores) || cores < 1L) {
    cores <- 1L
  }
  as.integer(min(4L, cores))
}

# 从参数或选项得到的后端名称，默认取 "callr"。
kernel_resolve_backend <- function(backend) {
  if (is.null(backend)) {
    backend <- getOption("pm25.kernel.backend")
  }
  if (is.null(backend)) {
    backend <- "callr"
  }
  valid <- is.character(backend) && length(backend) == 1L && !is.na(backend) &&
    backend %in% kernel_backends
  if (!valid) {
    cli::cli_abort(
      c(
        "Invalid kernel backend: {.val {backend}}.",
        "i" = "Expected one of {.val {kernel_backends}}."
      )
    )
  }
  backend
}

# 一次 kernel_plan() 调用要按顺序尝试的后端。
kernel_candidates <- function(backend, workers, fallback) {
  if (backend == "sequential" || workers <= 1L) {
    return("sequential")
  }
  if (!isTRUE(fallback)) {
    return(backend)
  }
  unique(c(backend, "multisession", "sequential"))
}

# 安装一个 future 计划。成功返回 NULL，否则返回错误消息。
kernel_set_plan <- function(backend, workers) {
  if (backend == "sequential" || workers <= 1L) {
    future::plan(future::sequential)
    return(NULL)
  }
  if (backend == "callr") {
    if (!requireNamespace("future.callr", quietly = TRUE)) {
      return("package 'future.callr' is not installed")
    }
    return(tryCatch(
      {
        future::plan(future.callr::callr, workers = workers)
        NULL
      },
      error = function(e) conditionMessage(e)
    ))
  }
  if (backend == "multisession") {
    return(tryCatch(
      {
        future::plan(future::multisession, workers = workers)
        NULL
      },
      error = function(e) conditionMessage(e)
    ))
  }
  sprintf("unknown backend \"%s\"", backend)
}

# 等待一个 future 完成，最多 `timeout` 秒。返回 TRUE 或 FALSE。
kernel_wait <- function(pending, timeout = 15) {
  deadline <- Sys.time() + timeout
  repeat {
    if (isTRUE(future::resolved(pending))) {
      return(TRUE)
    }
    if (Sys.time() > deadline) {
      return(FALSE)
    }
    Sys.sleep(0.02)
  }
}

# 启动一个简单 future 并收回结果。返回 list(ok, message, elapsed)。
# 沙箱内的启动失败在 future() 处暴露，所以在这里捕获。
kernel_smoke <- function(timeout = 15) {
  started <- Sys.time()
  status <- tryCatch(
    {
      pending <- future::future(TRUE, lazy = FALSE)
      if (!kernel_wait(pending, timeout)) {
        list(
          ok = FALSE,
          message = sprintf("smoke future did not resolve within %g s", timeout)
        )
      } else {
        value <- future::value(pending)
        if (identical(value, TRUE)) {
          list(ok = TRUE, message = "")
        } else {
          list(ok = FALSE, message = "smoke future returned an unexpected value")
        }
      }
    },
    error = function(e) list(ok = FALSE, message = conditionMessage(e))
  )
  status$elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  status
}

# 安装一个后端，并用一个 smoke future 验证。成功返回 NULL。
# 缺包即后端失败，因此 kernel_plan() 中的候选链
# 可以在任何分派运行之前回退到串行。
kernel_try_backend <- function(backend, workers) {
  missing <- kernel_backend_missing(backend)
  if (!is.null(missing)) {
    return(missing)
  }
  problem <- kernel_set_plan(backend, workers)
  if (!is.null(problem)) {
    return(problem)
  }
  if (backend == "sequential" || workers <= 1L) {
    return(NULL)
  }
  smoke <- kernel_smoke()
  if (isTRUE(smoke$ok)) {
    return(NULL)
  }
  sprintf("smoke test failed: %s", smoke$message)
}

# 提高 future 全局变量大小上限，并记住旧值供 kernel_off() 还原。
kernel_apply_options <- function() {
  if (is.null(.kernel_state$option_backup)) {
    .kernel_state$option_backup <- list(
      future.globals.maxSize = getOption("future.globals.maxSize")
    )
  }
  options(future.globals.maxSize = getOption("pm25.kernel.max_globals", 1e9))
  invisible(NULL)
}

# ---------- 公开计划 API --------------

# 设置项目的并发计划。只有这个函数能启用并行
# 分派；结果为串行时内核保持关闭。
#   workers   工作进程数。解析顺序：参数、
#             getOption("pm25.kernel.workers")、环境变量 PM25_KERNEL_WORKERS、
#             然后 min(4L, 物理核数)。workers <= 1 走串行。
#   backend   "callr"、"multisession" 或 "sequential"。解析顺序：
#             参数、getOption("pm25.kernel.backend")，然后取 "callr"。
#   fallback  为 TRUE 时回退到 multisession，再回退到串行，并给出一条
#             告警；为 FALSE 时停止。
# 在工作进程内部保持串行并告警。future 缺失
# 或没有后端启动成功时中止。以不可见方式返回 kernel_info()。
kernel_plan <- function(workers = NULL, backend = NULL, fallback = TRUE) {
# 在工作进程内部禁止创建嵌套进程：保持串行。
  if (isTRUE(getOption("pm25.kernel.in_worker"))) {
    cli::cli_warn(
      c(
        "!" = "kernel_plan() was called inside a kernel worker.",
        "i" = "The worker stays sequential; no nested processes are started."
      ),
      class = "kernel_worker_warning"
    )
    future::plan(future::sequential)
    .kernel_state$enabled <- FALSE
    return(invisible(kernel_info()))
  }
  if (!kernel_future_available()) {
    cli::cli_abort(
      c(
        "The {.pkg future} package is required by kernel_plan().",
        "i" = "Install it, or keep using the sequential plan."
      )
    )
  }

# 在任何任务运行之前解析两项设置并提高全局变量上限。
  workers <- kernel_resolve_workers(workers)
  backend <- kernel_resolve_backend(backend)
  kernel_apply_options()

# workers <= 1 是显式的串行请求。相应地把后端名改成 sequential，
# 这样下面不会为从未尝试过的后端触发回退告警。
  if (workers <= 1L) {
    backend <- "sequential"
  }

# 依次尝试每个候选后端，直到某个启动并通过 smoke future。
  candidates <- kernel_candidates(backend, workers, fallback)
  chosen <- NULL
  failures <- character(0)
  for (candidate in candidates) {
    problem <- kernel_try_backend(candidate, workers)
    if (is.null(problem)) {
      chosen <- candidate
      break
    }
    failures <- c(failures, sprintf("%s (%s)", candidate, problem))
  }
  if (is.null(chosen)) {
    future::plan(future::sequential)
    .kernel_state$enabled <- FALSE
    cli::cli_abort(
      c(
        "No kernel backend could be started.",
        "x" = "Requested {.val {backend}} with {workers} worker(s).",
        "i" = "Attempts: {paste(failures, collapse = '; ')}"
      ),
      class = "kernel_plan_error"
    )
  }

# 回退是真正的告警条件，调用方因此可以观察到它。
  if (chosen != backend) {
    cli::cli_warn(
      c(
        "Backend {.val {backend}} is unavailable; the kernel fell back to {.val {chosen}}.",
        "i" = "Attempts: {paste(failures, collapse = '; ')}"
      ),
      class = "kernel_fallback_warning"
    )
  }
# 只有这个函数可以启用并行分派，回退到串行
# 则让内核保持关闭。
  .kernel_state$enabled <- chosen != "sequential" && kernel_workers() > 1L
  if (kernel_verbose()) {
    kernel_log(
      "INFO",
      sprintf("kernel: %s backend, %d worker(s)", chosen, kernel_workers())
    )
  }
  invisible(kernel_info())
}

# 恢复串行计划，清除启用标志，并还原
# kernel_plan() 改动过的选项。以不可见方式返回 kernel_info()。
kernel_off <- function() {
  if (kernel_future_available()) {
    future::plan(future::sequential)
  }
  .kernel_state$enabled <- FALSE
  if (!is.null(.kernel_state$option_backup)) {
    options(.kernel_state$option_backup)
    .kernel_state$option_backup <- NULL
  }
  invisible(kernel_info())
}

# 试用一个后端但不保留它：设置请求的计划，运行一个 smoke
# future，然后恢复原来的计划。backend 默认 "callr"，workers
# 默认 2L。返回 list(ok, message, elapsed)，后端失败时
# 从不抛出错误，因此可以安全地探查沙箱或无头会话。
kernel_probe <- function(backend = "callr", workers = 2L) {
  if (!kernel_future_available()) {
    return(list(
      ok = FALSE,
      message = "package 'future' is not installed",
      elapsed = 0
    ))
  }
  backend <- kernel_resolve_backend(backend)
  workers <- kernel_resolve_workers(workers)

# 恢复原先生效的计划，即使探查失败也照做。
  previous <- future::plan()
  on.exit(future::plan(previous), add = TRUE)

  started <- Sys.time()
  problem <- kernel_try_backend(backend, workers)
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  if (is.null(problem)) {
    list(
      ok = TRUE,
      message = sprintf("%s backend started with %d worker(s)", backend, workers),
      elapsed = elapsed
    )
  } else {
    list(ok = FALSE, message = problem, elapsed = elapsed)
  }
}

# ---------- 控制参数 --------------------

# 默认分块大小为每个工作进程一块；否则必须是一个正整数。
kernel_resolve_chunk <- function(chunk_size, n, workers) {
  if (is.null(chunk_size)) {
    return(as.integer(max(1L, ceiling(n / workers))))
  }
  bad <- length(chunk_size) != 1L || !is.numeric(chunk_size) || is.na(chunk_size) ||
    chunk_size < 1 || chunk_size != floor(chunk_size)
  if (bad) {
    cli::cli_abort(
      c(
        "{.arg .chunk_size} must be one positive whole number.",
        "x" = "Got {.val {chunk_size}}."
      )
    )
  }
  as.integer(chunk_size)
}

# 错误策略取自参数或选项 pm25.kernel.on_error（默认 "stop"）。
kernel_resolve_error <- function(error) {
  if (is.null(error)) {
    error <- getOption("pm25.kernel.on_error", "stop")
  }
  valid <- is.character(error) && length(error) == 1L && !is.na(error) &&
    error %in% kernel_error_strategies
  if (!valid) {
    cli::cli_abort(
      c(
        "{.arg .error} must be one of {.val {kernel_error_strategies}}.",
        "x" = "Got {.val {error}}."
      )
    )
  }
  error
}

# 随机种子设置取自参数或选项 pm25.kernel.seed（默认 TRUE）。
kernel_resolve_seed <- function(seed) {
  if (is.null(seed)) {
    seed <- getOption("pm25.kernel.seed", TRUE)
  }
  valid <- length(seed) == 1L && !is.na(seed) &&
    (is.logical(seed) || (is.numeric(seed) && seed == floor(seed)))
  if (!valid) {
    cli::cli_abort(
      c(
        "{.arg .seed} must be TRUE, FALSE, or one whole number.",
        "x" = "Got {.val {seed}}."
      )
    )
  }
  if (is.logical(seed)) seed else as.integer(seed)
}

# 用于消息和错误表的标签。默认取任务下标。
kernel_resolve_labels <- function(labels, n) {
  if (is.null(labels)) {
    return(as.character(seq_len(n)))
  }
  labels <- as.character(labels)
  if (length(labels) != n) {
    cli::cli_abort(
      c(
        "{.arg .labels} must have one entry per task.",
        "x" = "Got {length(labels)} label(s) for {n} task(s)."
      )
    )
  }
  labels
}

# 映射调用的可读标签。默认取变体名。
kernel_resolve_label <- function(label, variant) {
  if (is.null(label)) {
    return(sprintf("kernel_%s", variant))
  }
  bad <- length(label) != 1L || is.na(label) || !is.character(label)
  if (bad) {
    cli::cli_abort("{.arg .label} must be one non-missing string.")
  }
  label
}

# ---------- 任务准备 -------------------

# 把长度为 1 的一侧循环到与另一侧等长，规则与 purrr 一致。
kernel_recycle2 <- function(x, y) {
  size_x <- length(x)
  size_y <- length(y)
  if (size_x == size_y) {
    return(list(x, y))
  }
  n <- max(size_x, size_y)
  if (size_x == 1L) {
    return(list(x[rep(1L, n)], y))
  }
  if (size_y == 1L) {
    return(list(x, y[rep(1L, n)]))
  }
  cli::cli_abort(
    c(
      "Cannot recycle {.arg .x} and {.arg .y}.",
      "x" = "{.arg .x} has size {size_x}; {.arg .y} has size {size_y}."
    )
  )
}

# 把 pmap 的每个分量循环到公共长度，规则与 purrr 一致。
kernel_recycle_pmap <- function(l) {
  if (length(l) == 0L) {
    return(l)
  }
  sizes <- vapply(l, length, integer(1))
  n <- max(sizes)
  bad <- sizes != n & sizes != 1L
  if (any(bad)) {
    position <- which(bad)[[1]]
    cli::cli_abort(
      c(
        "Cannot recycle {.arg .l} for pmap().",
        "x" = "Element {position} has size {sizes[[position]]}; expected 1 or {n}."
      )
    )
  }
  if (all(sizes == n)) {
    return(l)
  }
  lapply(l, function(column) {
    if (length(column) == n) column else column[rep(1L, n)]
  })
}

# 构造每个任务的位置参数，以及传给 furrr 函数的载荷。
kernel_prepare <- function(variant, payload) {
  if (variant == "map" || variant == "walk") {
    x <- payload$.x
    n <- length(x)
    tasks <- lapply(seq_len(n), function(i) list(x[[i]]))
    return(list(
      variant = "map",
      n = n,
      tasks = tasks,
      dispatch = list(.x = x),
      names = names(x)
    ))
  }
  if (variant == "map2") {
    recycled <- kernel_recycle2(payload$.x, payload$.y)
    x <- recycled[[1]]
    y <- recycled[[2]]
    n <- length(x)
    tasks <- lapply(seq_len(n), function(i) list(x[[i]], y[[i]]))
    return(list(
      variant = "map2",
      n = n,
      tasks = tasks,
      dispatch = list(.x = x, .y = y),
      names = names(x)
    ))
  }
  if (variant == "imap") {
    x <- payload$.x
    n <- length(x)
    index <- if (is.null(names(x))) seq_len(n) else names(x)
    tasks <- lapply(seq_len(n), function(i) list(x[[i]], index[[i]]))
    return(list(
      variant = "imap",
      n = n,
      tasks = tasks,
      dispatch = list(.x = x),
      names = names(x)
    ))
  }
  if (variant == "pmap") {
    l <- payload$.l
    if (is.data.frame(l)) {
      l <- as.list(l)
    }
    if (!is.list(l)) {
      cli::cli_abort("{.arg .l} must be a list or a data frame.")
    }
    l <- kernel_recycle_pmap(l)
    n <- if (length(l) == 0L) 0L else length(l[[1]])
    tasks <- lapply(seq_len(n), function(i) {
      args <- lapply(l, function(column) column[[i]])
      names(args) <- names(l)
      args
    })
    return(list(
      variant = "pmap",
      n = n,
      tasks = tasks,
      dispatch = list(.l = l),
      names = if (length(l) == 0L) NULL else names(l[[1]])
    ))
  }
  cli::cli_abort("Unknown kernel variant {.val {variant}}.")
}

# ---------- 任务执行 -----------------

# 失败调用的文本，用于错误表。
kernel_call_text <- function(error) {
  call <- conditionCall(error)
  if (is.null(call)) {
    return(NA_character_)
  }
  text <- paste(deparse(call), collapse = " ")
  if (nzchar(text)) text else NA_character_
}

# 运行一个任务，把任何错误转成一个数据包。任务出错时不抛出。
kernel_run_task <- function(.f, task_args, dots) {
  tryCatch(
    {
      value <- do.call(.f, c(task_args, dots))
      list(
        ok = TRUE,
        value = value,
        message = NA_character_,
        call = NA_character_,
        class = NA_character_
      )
    },
    error = function(e) {
      list(
        ok = FALSE,
        value = NULL,
        message = conditionMessage(e),
        call = kernel_call_text(e),
        class = paste(class(e), collapse = "/")
      )
    }
  )
}

# 工作进程侧的入口：标记工作进程，使嵌套的 kernel 调用保持串行，
# 退出时还原该选项。
kernel_run_task_in_worker <- function(.f, task_args, dots) {
  previous <- options(pm25.kernel.in_worker = TRUE)
  on.exit(options(previous), add = TRUE)
  kernel_run_task(.f, task_args, dots)
}

# 每个变体一个包装函数。闭包对每个 future 捕获一次 .f 和 dots。
kernel_wrapper <- function(variant, .f, dots) {
  if (variant == "map") {
    return(function(.element) {
      kernel_run_task_in_worker(.f, list(.element), dots)
    })
  }
  if (variant == "map2") {
    return(function(.x, .y) {
      kernel_run_task_in_worker(.f, list(.x, .y), dots)
    })
  }
  if (variant == "imap") {
    return(function(.value, .name) {
      kernel_run_task_in_worker(.f, list(.value, .name), dots)
    })
  }
  function(...) {
    kernel_run_task_in_worker(.f, list(...), dots)
  }
}

# 串行路径：同样的数据包，在主会话中计算。
kernel_dispatch_serial <- function(tasks, .f, dots) {
  lapply(tasks, function(task) kernel_run_task(.f, task, dots))
}

# 并行路径：每个变体一个 furrr 函数，通过 do.call() 调用。
kernel_dispatch_parallel <- function(prepared, .f, dots, ctl) {
  if (!kernel_furrr_available()) {
    cli::cli_abort(
      c(
        "The {.pkg furrr} package is required for parallel kernel calls.",
        "i" = "Install furrr, or run with a sequential plan."
      )
    )
  }
  fn_name <- kernel_variant_fns[[prepared$variant]]
  if (is.null(fn_name)) {
    cli::cli_abort("No furrr function for variant {.val {prepared$variant}}.")
  }
  furrr_fn <- get(fn_name, envir = asNamespace("furrr"), mode = "function")
  options <- furrr::furrr_options(
    seed = ctl$seed,
    chunk_size = ctl$chunk_size,
    stdout = TRUE,
    conditions = "condition",
    globals = TRUE
  )
  args <- c(
    prepared$dispatch,
    list(.f = kernel_wrapper(prepared$variant, .f, dots)),
    list(.options = options)
  )
  do.call(furrr_fn, args)
}

# ---------- 结果拼装 ------------------

# 一次映射调用的载荷总大小，单位 MB。
kernel_payload_mb <- function(payload) {
  bytes <- vapply(
    payload,
    function(element) as.numeric(object.size(element)),
    numeric(1)
  )
  sum(bytes) / 1e6
}

# 把一条错误消息压成一行，并截到 `width` 个字符。
kernel_short_message <- function(message, width = 120L) {
  if (length(message) != 1L || is.na(message)) {
    return("<no message>")
  }
  message <- gsub("[\r\n]+", " ", as.character(message))
  if (nchar(message) > width) {
    message <- paste0(substr(message, 1L, width), "...")
  }
  message
}

# 供 kernel_task_error 和 warn 策略使用的一行失败摘要。先列出前
# 三个失败的键，再列最多三条带引号的消息，
# 因此仅凭条件文本就能诊断真实的 Mortality() 失败。
kernel_failure_text <- function(ctl, n, failed, messages = NULL) {
  keys <- ctl$labels[failed]
  shown <- head(keys, 3L)
  suffix <- if (length(keys) > 3L) {
    sprintf(" (+%d more)", length(keys) - 3L)
  } else {
    ""
  }
  text <- sprintf(
    "%d of %d task(s) failed in \"%s\": %s%s",
    length(failed), n, ctl$label, paste(shown, collapse = ", "), suffix
  )
  if (length(messages) == 0L) {
    return(text)
  }
  n_show <- min(length(messages), 3L)
  detail <- sprintf(
    "%s: %s",
    keys[seq_len(n_show)],
    vapply(messages[seq_len(n_show)], kernel_short_message, character(1))
  )
  paste0(text, "; error(s): ", paste(detail, collapse = " | "))
}

# 错误表：每个失败任务一行；安装了 tibble 时返回 tibble。
kernel_error_table <- function(index, key, message, call, class) {
  table <- data.frame(
    index = index,
    key = key,
    message = message,
    call = call,
    class = class,
    stringsAsFactors = FALSE
  )
  if (requireNamespace("tibble", quietly = TRUE)) {
    return(tibble::as_tibble(table))
  }
  table
}

# 把任务数据包转成约定返回值，并记录调用统计量。
kernel_collect <- function(packets, prepared, ctl, n_futures, elapsed, payload_mb) {
  n <- prepared$n
  if (length(packets) != n) {
    cli::cli_abort(
      c(
        "Kernel dispatch returned the wrong number of results.",
        "x" = "Got {length(packets)} result(s) for {n} task(s)."
      )
    )
  }

# 预分配每个按任务的向量，使循环不再增长对象。
  values <- vector("list", n)
  failed <- integer(n)
  failed_n <- 0L
  error_message <- character(n)
  error_call <- character(n)
  error_class <- character(n)

  for (i in seq_len(n)) {
    packet <- packets[[i]]
    if (isTRUE(packet$ok)) {
      values[i] <- list(packet$value)
    } else {
      failed_n <- failed_n + 1L
      failed[[failed_n]] <- i
      error_message[[i]] <- packet$message
      error_call[[i]] <- packet$call
      error_class[[i]] <- packet$class
    }
  }
  failed <- failed[seq_len(failed_n)]
  names(values) <- prepared$names

# 在可能中止之前记录统计量，kernel_info() 因此保持准确。
  backend_now <- if (n_futures > 0L) kernel_backend_name() else "sequential"
  workers_now <- if (n_futures > 0L) kernel_workers() else 1L
  .kernel_state$n_futures_last <- as.integer(n_futures)
  .kernel_state$elapsed_last <- elapsed
  .kernel_state$payload_mb_last <- payload_mb
  .kernel_state$errors_last <- as.integer(failed_n)

  if (failed_n > 0L) {
    text <- kernel_failure_text(ctl, n, failed, error_message[failed])
    if (ctl$on_error == "stop") {
      cli::cli_abort("{text}", class = "kernel_task_error", call = NULL)
    }
    if (ctl$on_error == "warn") {
      cli::cli_warn("{text}", class = "kernel_task_warning")
    }
  }

  if (ctl$on_error == "collect") {
    result <- list(
      values = values,
      errors = kernel_error_table(
        index = failed,
        key = ctl$labels[failed],
        message = error_message[failed],
        call = error_call[failed],
        class = error_class[failed]
      ),
      n_tasks = as.integer(n),
      n_ok = as.integer(n - failed_n),
      elapsed = elapsed,
      workers = as.integer(workers_now),
      backend = backend_now,
      labels = ctl$labels
    )
    class(result) <- "kernel_result"
    return(result)
  }
  values
}

# ---------- 分发 ---------------

# 每个 kernel_map() 变体共用的实现。
kernel_dispatch <- function(variant, payload, .f, dots, .chunk_size, .error,
                            .seed, .label, .labels, .verbose) {
  if (!is.function(.f)) {
    cli::cli_abort(
      c(
        "{.arg .f} must be a function.",
        "x" = "Got {.cls {class(.f)}}."
      )
    )
  }

# 在任何任务运行之前解析输入和控制参数。
  prepared <- kernel_prepare(variant, payload)
  workers <- kernel_workers()
  ctl <- list(
    chunk_size = kernel_resolve_chunk(.chunk_size, prepared$n, workers),
    on_error = kernel_resolve_error(.error),
    seed = kernel_resolve_seed(.seed),
    label = kernel_resolve_label(.label, variant),
    labels = kernel_resolve_labels(.labels, prepared$n),
    verbose = if (is.null(.verbose)) kernel_verbose() else isTRUE(.verbose)
  )
  payload_mb <- kernel_payload_mb(payload)

# 分派：计划未启用、只有一个任务或零个任务时走串行。
  started <- Sys.time()
  parallel <- prepared$n > 1L && kernel_active()
  if (parallel) {
    n_futures <- as.integer(max(1L, ceiling(prepared$n / ctl$chunk_size)))
    if (ctl$verbose) {
      kernel_log(
        "INFO",
        sprintf(
          "kernel: %s ran %d task(s) in %d chunk(s) on %d worker(s)",
          ctl$label, prepared$n, n_futures, workers
        )
      )
    }
    packets <- kernel_dispatch_parallel(prepared, .f, dots, ctl)
  } else {
    n_futures <- 0L
    packets <- kernel_dispatch_serial(prepared$tasks, .f, dots)
  }
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  kernel_collect(packets, prepared, ctl, n_futures, elapsed, payload_mb)
}

# ---------- 公开映射 API -----------------

# 用项目并发内核映射一个向量。计划未启用或只有一个任务时
# 走串行；串行路径与 purrr 在结果顺序、名字和 walk() 的
# 不可见性上一致。
#   .x            向量或列表。
#   .f            接收一个元素和任意 ... 参数的函数。
#   ...           原样传给 .f 的额外参数。
#   .chunk_size   每个 future 的任务数；NULL 表示每个工作进程一块。
#   .error        "stop"、"warn" 或 "collect"；NULL 时读取
#                 getOption("pm25.kernel.on_error", "stop")。
#   .seed         传给 furrr::furrr_options() 的种子设置；NULL 时读取
#                 getOption("pm25.kernel.seed", TRUE)。
#   .label        日志和错误消息使用的标签。
#   .labels       任务键，每个任务一个。
#   .verbose      每次并行调用打印一行进度；NULL 时读取
#                 getOption("pm25.kernel.verbose", FALSE)，
#                 因此内核默认静默。
# 返回带 purrr::map() 语义的列表；.error = "collect" 时
# 返回 kernel_result。
kernel_map <- function(.x, .f, ..., .chunk_size = NULL, .error = NULL,
                       .seed = NULL, .label = NULL, .labels = NULL,
                       .verbose = NULL) {
  kernel_dispatch(
    variant = "map",
    payload = list(.x = .x),
    .f = .f,
    dots = list(...),
    .chunk_size = .chunk_size,
    .error = .error,
    .seed = .seed,
    .label = .label,
    .labels = .labels,
    .verbose = .verbose
  )
}

# 用项目并发内核映射两个向量。.x 和 .y 是等长的向量
# 或列表，长度为 1 的一侧会循环。其余参数
# 与 kernel_map() 共用。返回带 purrr::map2() 语义的列表。
kernel_map2 <- function(.x, .y, .f, ..., .chunk_size = NULL, .error = NULL,
                        .seed = NULL, .label = NULL, .labels = NULL,
                        .verbose = NULL) {
  kernel_dispatch(
    variant = "map2",
    payload = list(.x = .x, .y = .y),
    .f = .f,
    dots = list(...),
    .chunk_size = .chunk_size,
    .error = .error,
    .seed = .seed,
    .label = .label,
    .labels = .labels,
    .verbose = .verbose
  )
}

# 用项目并发内核按名字或下标映射一个向量。
# .f 接收 (value, name)；其余参数与 kernel_map() 共用。
# 返回带 purrr::imap() 语义的列表。
kernel_imap <- function(.x, .f, ..., .chunk_size = NULL, .error = NULL,
                        .seed = NULL, .label = NULL, .labels = NULL,
                        .verbose = NULL) {
  kernel_dispatch(
    variant = "imap",
    payload = list(.x = .x),
    .f = .f,
    dots = list(...),
    .chunk_size = .chunk_size,
    .error = .error,
    .seed = .seed,
    .label = .label,
    .labels = .labels,
    .verbose = .verbose
  )
}

# 用项目并发内核并行映射多个向量。.l 是
# 向量的列表或数据框，每个下标一个任务；其余参数
# 与 kernel_map() 共用。返回带 purrr::pmap() 语义的列表。
kernel_pmap <- function(.l, .f, ..., .chunk_size = NULL, .error = NULL,
                        .seed = NULL, .label = NULL, .labels = NULL,
                        .verbose = NULL) {
  kernel_dispatch(
    variant = "pmap",
    payload = list(.l = .l),
    .f = .f,
    dots = list(...),
    .chunk_size = .chunk_size,
    .error = .error,
    .seed = .seed,
    .label = .label,
    .labels = .labels,
    .verbose = .verbose
  )
}

# 用项目并发内核遍历一个向量以产生副作用。
# 其余参数与 kernel_map() 共用。以不可见方式返回 .x；
# .error = "collect" 时返回 kernel_result。
kernel_walk <- function(.x, .f, ..., .chunk_size = NULL, .error = NULL,
                        .seed = NULL, .label = NULL, .labels = NULL,
                        .verbose = NULL) {
  result <- kernel_dispatch(
    variant = "walk",
    payload = list(.x = .x),
    .f = .f,
    dots = list(...),
    .chunk_size = .chunk_size,
    .error = .error,
    .seed = .seed,
    .label = .label,
    .labels = .labels,
    .verbose = .verbose
  )
  if (inherits(result, "kernel_result")) {
    return(result)
  }
  invisible(.x)
}
