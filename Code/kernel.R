# Kernel: project-wide concurrency kernel
#
#   Plan management : kernel_plan(), kernel_off(), kernel_probe(), kernel_info(),
#                     kernel_active(), kernel_workers()
#   Mapping         : kernel_map(), kernel_map2(), kernel_imap(), kernel_pmap(),
#                     kernel_walk()
#
# Input : none. Loaded with source("Code/kernel.R", encoding = "UTF8").
# Output: none. Defines functions plus one internal state environment.
# Run   : source("Code/kernel.R"); kernel_plan(workers = 4)
#         kernel_map(x, f); kernel_off()
# Policy: sequential by default. The parallel path uses future + furrr, wraps every
#         task in tryCatch(), and calls user functions through do.call(). The serial
#         path mirrors purrr results: value order, names, and walk() invisibility.
# Style : yifan-r-code-style v3

# ---------- Constants ----------

# Supported backends, in the order kernel_plan() may try them.
kernel_backends <- c("callr", "multisession", "sequential")

# furrr function per mapping variant. Walk maps elements exactly like map().
kernel_variant_fns <- c(
  map = "future_map",
  walk = "future_map",
  map2 = "future_map2",
  imap = "future_imap",
  pmap = "future_pmap"
)

# Allowed error strategies for one mapping call.
kernel_error_strategies <- c("stop", "warn", "collect")

# ---------- Internal state ----------

# Plan bookkeeping shared by kernel_plan(), kernel_map(), and kernel_off().
# option_backup holds the pre-kernel values of every option this file changes.
.kernel_state <- new.env(parent = emptyenv())
.kernel_state$option_backup <- NULL
.kernel_state$n_futures_last <- 0L
.kernel_state$elapsed_last <- 0
.kernel_state$payload_mb_last <- 0
.kernel_state$errors_last <- 0L

# ---------- Availability and logging ----------

# TRUE when the future package is installed.
kernel_future_available <- function() {
  requireNamespace("future", quietly = TRUE)
}

# TRUE when the furrr package is installed.
kernel_furrr_available <- function() {
  requireNamespace("furrr", quietly = TRUE)
}

# TRUE when progression lines are requested. The kernel is silent by default so
# that callers can count warning and message conditions.
kernel_verbose <- function() {
  isTRUE(getOption("pm25.kernel.verbose", FALSE))
}

# Double every brace so that str_glue() renders the text literally.
kernel_escape_glue <- function(text) {
  text <- gsub("{", "{{", text, fixed = TRUE)
  gsub("}", "}}", text, fixed = TRUE)
}

# Log one line through log_msg() from Code/model.R when it is loaded; otherwise
# fall back to message(). The text is pre-formatted, so braces must be escaped.
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

# ---------- Plan inspection ----------

# Backend name of the current future plan, normalised to the supported names.
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

# Effective worker count. Inside a kernel worker the answer is always 1.
kernel_workers <- function() {
  if (isTRUE(getOption("pm25.kernel.in_worker"))) {
    return(1L)
  }
  if (!kernel_future_available()) {
    return(1L)
  }
  as.integer(future::nbrOfWorkers())
}

# TRUE when futures really run in parallel and we are not inside one.
kernel_active <- function() {
  if (isTRUE(getOption("pm25.kernel.in_worker"))) {
    return(FALSE)
  }
  kernel_workers() > 1L
}

# Snapshot of the plan plus the statistics of the last kernel_map() family call.
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

# ---------- Plan construction ----------

# Whole-number worker count from argument, option, environment, or physical cores.
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

# Default worker count: min(4, physical cores), with safe fallbacks.
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

# Backend name from argument, option, or the "callr" default.
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

# Backends to try, in order, for one kernel_plan() call.
kernel_candidates <- function(backend, workers, fallback) {
  if (backend == "sequential" || workers <= 1L) {
    return("sequential")
  }
  if (!isTRUE(fallback)) {
    return(backend)
  }
  unique(c(backend, "multisession", "sequential"))
}

# Install one future plan. Returns NULL on success, otherwise an error message.
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

# Wait for one future to resolve, up to `timeout` seconds. Returns TRUE or FALSE.
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

# Launch one trivial future and collect it. Returns list(ok, message, elapsed).
# Launch failures inside a sandbox show up at future(), so they are caught here.
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

# Install one backend and prove it with a smoke future. NULL on success.
kernel_try_backend <- function(backend, workers) {
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

# Raise the future globals limit and remember the previous value for kernel_off().
kernel_apply_options <- function() {
  if (is.null(.kernel_state$option_backup)) {
    .kernel_state$option_backup <- list(
      future.globals.maxSize = getOption("future.globals.maxSize")
    )
  }
  options(future.globals.maxSize = getOption("pm25.kernel.max_globals", 1e9))
  invisible(NULL)
}

# ---------- Public plan API ----------

#' Set the project concurrency plan
#'
#' @param workers Worker count. Resolution order: argument,
#'   `getOption("pm25.kernel.workers")`, environment variable
#'   `PM25_KERNEL_WORKERS`, then `min(4L, physical cores)`. `workers <= 1`
#'   switches to sequential.
#' @param backend One of `"callr"`, `"multisession"`, `"sequential"`. Resolution
#'   order: argument, `getOption("pm25.kernel.backend")`, then `"callr"`.
#' @param fallback When `TRUE`, a failing backend falls back to multisession and
#'   then to sequential, with one warning. When `FALSE`, it stops.
#' @return `kernel_info()`, invisibly.
kernel_plan <- function(workers = NULL, backend = NULL, fallback = TRUE) {
  # Inside a worker, nested process creation is forbidden: stay sequential.
  if (isTRUE(getOption("pm25.kernel.in_worker"))) {
    cli::cli_warn(
      c(
        "!" = "kernel_plan() was called inside a kernel worker.",
        "i" = "The worker stays sequential; no nested processes are started."
      ),
      class = "kernel_worker_warning"
    )
    future::plan(future::sequential)
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

  # Resolve both settings and raise the globals limit before any task runs.
  workers <- kernel_resolve_workers(workers)
  backend <- kernel_resolve_backend(backend)
  kernel_apply_options()

  # workers <= 1 is an explicit sequential request. Name the backend accordingly
  # so the fallback warning below is not raised for a backend never attempted.
  if (workers <= 1L) {
    backend <- "sequential"
  }

  # Try each candidate until one starts and passes the smoke future.
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
    cli::cli_abort(
      c(
        "No kernel backend could be started.",
        "x" = "Requested {.val {backend}} with {workers} worker(s).",
        "i" = "Attempts: {paste(failures, collapse = '; ')}"
      ),
      class = "kernel_plan_error"
    )
  }

  # A fallback is a real warning condition so that callers can observe it.
  if (chosen != backend) {
    cli::cli_warn(
      c(
        "Backend {.val {backend}} is unavailable; the kernel fell back to {.val {chosen}}.",
        "i" = "Attempts: {paste(failures, collapse = '; ')}"
      ),
      class = "kernel_fallback_warning"
    )
  }
  if (kernel_verbose()) {
    kernel_log(
      "INFO",
      sprintf("kernel: %s backend, %d worker(s)", chosen, kernel_workers())
    )
  }
  invisible(kernel_info())
}

#' Restore the sequential plan and the options kernel_plan() changed
#'
#' @return `kernel_info()`, invisibly.
kernel_off <- function() {
  if (kernel_future_available()) {
    future::plan(future::sequential)
  }
  if (!is.null(.kernel_state$option_backup)) {
    options(.kernel_state$option_backup)
    .kernel_state$option_backup <- NULL
  }
  invisible(kernel_info())
}

#' Probe one backend without keeping it
#'
#' Sets the requested plan, runs one smoke future, then restores the previous
#' plan. Returns `list(ok, message, elapsed)`; it never throws for a backend
#' failure, so a sandboxed or headless session can be inspected safely.
#'
#' @param backend Backend name, default `"callr"`.
#' @param workers Worker count, default `2L`.
#' @return `list(ok, message, elapsed)`.
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

  # Restore whatever plan was active, even when the probe fails.
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

# ---------- Control arguments ----------

# One chunk size per worker by default; otherwise one positive whole number.
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

# Error strategy from the argument or option pm25.kernel.on_error (default "stop").
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

# Seed control from the argument or option pm25.kernel.seed (default TRUE).
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

# Labels used in messages and in the errors table. Default is the task index.
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

# Human-readable label for the mapping call. Default is the variant name.
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

# ---------- Task preparation ----------

# Recycle a size-1 side to match the other side, mirroring purrr's rule.
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

# Recycle every pmap component to a common size, mirroring purrr's rule.
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

# Build per-task positional arguments plus the payload for the furrr function.
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

# ---------- Task execution ----------

# Text of the failing call, used in the errors table.
kernel_call_text <- function(error) {
  call <- conditionCall(error)
  if (is.null(call)) {
    return(NA_character_)
  }
  text <- paste(deparse(call), collapse = " ")
  if (nzchar(text)) text else NA_character_
}

# Run one task and turn any error into a packet. Never throws for task errors.
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

# Worker-side entry point: mark the worker so nested kernel calls stay sequential,
# then restore the option on exit.
kernel_run_task_in_worker <- function(.f, task_args, dots) {
  previous <- options(pm25.kernel.in_worker = TRUE)
  on.exit(options(previous), add = TRUE)
  kernel_run_task(.f, task_args, dots)
}

# One wrapper per variant. The closure captures .f and dots once per future.
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

# Serial path: the same packets, computed in the main session.
kernel_dispatch_serial <- function(tasks, .f, dots) {
  lapply(tasks, function(task) kernel_run_task(.f, task, dots))
}

# Parallel path: one furrr function per variant, called through do.call().
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

# ---------- Result assembly ----------

# Total payload size of one mapping call, in megabytes.
kernel_payload_mb <- function(payload) {
  bytes <- vapply(
    payload,
    function(element) as.numeric(object.size(element)),
    numeric(1)
  )
  sum(bytes) / 1e6
}

# One-line failure summary for kernel_task_error and the warn strategy.
kernel_failure_text <- function(ctl, n, failed) {
  keys <- ctl$labels[failed]
  shown <- head(keys, 3L)
  suffix <- if (length(keys) > 3L) {
    sprintf(" (+%d more)", length(keys) - 3L)
  } else {
    ""
  }
  sprintf(
    "%d of %d task(s) failed in \"%s\": %s%s",
    length(failed), n, ctl$label, paste(shown, collapse = ", "), suffix
  )
}

# errors table: one row per failed task, tibble when tibble is installed.
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

# Turn task packets into the documented return value and record call statistics.
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

  # Preallocate every per-task vector so the loop never grows an object.
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

  # Record statistics before a possible stop, so kernel_info() stays accurate.
  backend_now <- if (n_futures > 0L) kernel_backend_name() else "sequential"
  workers_now <- if (n_futures > 0L) kernel_workers() else 1L
  .kernel_state$n_futures_last <- as.integer(n_futures)
  .kernel_state$elapsed_last <- elapsed
  .kernel_state$payload_mb_last <- payload_mb
  .kernel_state$errors_last <- as.integer(failed_n)

  if (failed_n > 0L) {
    text <- kernel_failure_text(ctl, n, failed)
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

# ---------- Dispatch ----------

# Shared implementation of every kernel_map() variant.
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

  # Resolve inputs and control arguments before any task runs.
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

  # Dispatch: serial for an inactive plan, a single task, or zero tasks.
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

# ---------- Public mapping API ----------

#' Map over one vector with the project concurrency kernel
#'
#' @param .x A vector or list.
#' @param .f A function taking one element plus any `...` arguments.
#' @param ... Extra arguments passed to `.f` unchanged.
#' @param .chunk_size Tasks per future. `NULL` uses one chunk per worker.
#' @param .error One of `"stop"`, `"warn"`, `"collect"`. `NULL` reads
#'   `getOption("pm25.kernel.on_error", "stop")`.
#' @param .seed Seed control passed to `furrr::furrr_options()`. `NULL` reads
#'   `getOption("pm25.kernel.seed", TRUE)`.
#' @param .label Label for logs and error messages.
#' @param .labels Character vector of task keys, one per task.
#' @param .verbose Print one progress line per parallel call. `NULL` reads
#'   `getOption("pm25.kernel.verbose", FALSE)`, so the kernel is silent by default.
#' @return A list with `purrr::map()` semantics; a `kernel_result` when
#'   `.error = "collect"`.
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

#' Map over two vectors with the project concurrency kernel
#'
#' @inheritParams kernel_map
#' @param .x,.y Vectors or lists of the same length. A size-1 side is recycled.
#' @return A list with `purrr::map2()` semantics.
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

#' Map over one vector with its names or index
#'
#' @inheritParams kernel_map
#' @return A list with `purrr::imap()` semantics; `.f` receives `(value, name)`.
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

#' Map over several vectors in parallel
#'
#' @inheritParams kernel_map
#' @param .l A list or data frame of vectors, one task per index.
#' @return A list with `purrr::pmap()` semantics.
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

#' Walk over one vector with the project concurrency kernel
#'
#' @inheritParams kernel_map
#' @return `.x`, invisibly; a `kernel_result` when `.error = "collect"`.
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
