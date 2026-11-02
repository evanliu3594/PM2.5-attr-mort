# C-R 模型配置模块
#   - tell_Model() — 为输出文件名格式化当前 C-R 模型名
#   - set_Model()  — 设置全局 .CR_Model，并缓存标准化 RR 表
#   - RR_std()     — 从查找 xlsx 构建标准化 RR 表
#   修改 261026b：单位与范围元数据会沿
#     set_Model()/RR_std() 传递；新表会校验键、范围和 CI 顺序。
#   修改 261102：浓度网格固定为 0.1，删除 dgt_conc 参数；
#     RR_std() 拒绝分辨率细于 0.1 的查找表。
#   修改 261101：RR_std() 只补配置中声明、查找表里缺失的年龄列
#     （空格子保持 NA，供非有限值守卫使用），
#     并在设置 rr_normalized 标志之前把保留的 endpoint 转为小写。

library(tidyverse)
library(writexl)
library(readxl)
library(jsonlite)

log_msg <- function(level, ...) {
  level <- match.arg(
    as.character(substitute(level)),
    c("INFO", "WARN", "ERROR")
  )
  msg <- str_glue(..., .envir = parent.frame())
  switch(
    level,
    INFO = cli::cli_alert_info(msg),
    WARN = cli::cli_text(cli::style_bold(cli::col_blue(str_c("⚠ ", msg)))),
    ERROR = cli::cli_text(cli::style_bold(cli::col_red(msg)))
  )
}

# 输出文件名使用的 C-R 模型名。从 RR_std_config.json 读取 label；
# 配置或 label 不可用时回退到 .CR_Model。
tell_Model <- function() {
  cfg_path <- './Data/RR_std_config.json'
  if (file.exists(cfg_path)) {
    cfg <- jsonlite::fromJSON(cfg_path, simplifyVector = FALSE)
    entry <- cfg[[.CR_Model]]
    if (!is.null(entry$label)) return(entry$label)
  }
  .CR_Model
}

# 为本次会话启用一个 C-R 模型：设置全局变量 .CR_Model，
# 然后把标准化 RR 表缓存到 .RR_std_tbl。
#   Model     RR_std_config.json 中的 label，或自定义名称。
#   path      可选的自定义 RR 查找 xlsx；给出时，用 MEAN 工作表中
#             {endpoint}_{agegroup} 列生成一份标准化配置，该配置
#             必须人工复核后加入 Data/RR_std_config.json。
# 浓度网格固定为 0.1：RR 表的分辨率不得细于 0.1，否则中止。
set_Model <- function(Model, path = NULL) {
  assign(".CR_Model", Model, envir = globalenv())
  log_msg(INFO, "C-R Model \"{tell_Model()}\" is set.")

# 从自定义查找表自动生成标准化配置
  if (!is.null(path)) {
    if (!file.exists(path)) {
      stop("Custom RR lookup table not found: ", path)
    }

    raw <- read_xlsx(path, sheet = "MEAN")
    all_names <- names(raw)

# 识别浓度列：唯一不匹配
# {endpoint}_{age} 或 {endpoint}_ALL 模式的那一列。文件遵循标准命名
# 约定时默认取 "concentration"。
    mort_cols <- str_subset(all_names, '_[0-9]+$')
    all_cols <- str_subset(all_names, '_[Aa][Ll]{2}$')
    rr_cols <- union(mort_cols, all_cols)

    conc_col <- setdiff(all_names, rr_cols)
    if (length(conc_col) != 1) {
      conc_col <- "concentration"
      log_msg(
        WARN,
        "Could not auto-detect concentration column (found {length(conc_col)} candidates); ",
        "defaulting to \"{conc_col}\". Edit RR_std_config.json if needed."
      )
    }

    if (length(mort_cols) > 0) {
      parsed <- str_match(mort_cols, '^(.+)_([0-9]+)$')
      ep_ages <- tibble(
        endpoint = str_to_lower(parsed[, 2]),
        age = as.integer(parsed[, 3])
      ) |>
        group_by(endpoint) |>
        summarise(ages = list(sort(unique(age))), .groups = "drop")
    } else if (length(all_cols) > 0) {
      default_ages <- seq(0, 95, 5)
      ep_names <- str_to_lower(str_remove(all_cols, '_ALL$'))
      ep_ages <- tibble(
        endpoint = ep_names,
        ages = rep(list(default_ages), length(ep_names))
      )
      log_msg(
        INFO,
        "Only _ALL columns found; auto-generated ages ",
        "{min(default_ages)}-{max(default_ages)} ",
        "for {length(ep_names)} endpoint(s)."
      )
    } else {
      stop(
        "No {endpoint}_{age} or {endpoint}_ALL columns found in ",
        path,
        ". Expected columns like copd_25 or copd_ALL."
      )
    }

    log_msg(
      INFO,
      "Detected concentration column \"{conc_col}\" with ",
      "{length(rr_cols)} endpoint–age columns."
    )

    ep_entries <- ep_ages |>
      rowwise() |>
      summarise(
        json = str_glue(
          '      {{ "name": "{endpoint}", "ages": [{str_c(ages, collapse=", ")}] }}'
        ),
        .groups = "drop"
      ) |>
      pull(json)

    config_entry <- str_c(
      '  "',
      Model,
      '": {\n',
      '    "conc_col": "',
      conc_col,
      '",\n',
      '    "lookup": "',
      path,
      '",\n',
      '    "endpoints": [\n',
      str_c(ep_entries, collapse = ",\n"),
      '\n',
      '    ]\n',
      '  }'
    )

    log_msg(
      INFO,
      "Detected {nrow(ep_ages)} endpoints: ",
      str_c(ep_ages$endpoint, collapse = ", ")
    )
    log_msg(
      INFO,
      "Total age-group columns parsed: {sum(lengths(ep_ages$ages))}"
    )

# 预览并自动追加到 RR_std_config.json
    cat("\n---- Auto-generated RR_std config entry ----\n")
    cat(config_entry, "\n\n")

    cfg_path <- './Data/RR_std_config.json'
    if (file.exists(cfg_path)) {
      cfg_lines <- readLines(cfg_path)
      if (any(str_detect(cfg_lines, str_glue('^  "{Model}":')))) {
        log_msg(
          WARN,
          'Model "{Model}" already exists in RR_std_config.json — skipping append.'
        )
      } else {
# 插到 JSON 根对象结尾的 } 之前
        close_idx <- tail(which(str_detect(str_trim(cfg_lines), '^\\}$')), 1)
        if (length(close_idx) == 0) {
          close_idx <- length(cfg_lines)
        }
        new_lines <- c(cfg_lines[1:(close_idx - 1)], ",", config_entry, "}")
        writeLines(new_lines, cfg_path)
        log_msg(INFO, 'Appended "{Model}" to {cfg_path}.')
      }
    } else {
# 创建新的配置文件
      writeLines(str_c('{\n', config_entry, '\n}\n'), cfg_path)
      log_msg(INFO, 'Created {cfg_path} with "{Model}" entry.')
    }
  }

  # ---- 通过 RR_std() 构建标准化 RR 表 ---------------
  assign(".RR_std_tbl", RR_std(path), envir = globalenv())
}

# 构建标准化 C-R 查找表：读取 RR xlsx 的 MEAN/LOW/UP 工作表，
# 转为长格式，展开到 endpoint x agegroup 网格，该网格在
# RR_std_config.json 中声明，并返回长格式的 concentration、endpoint、agegroup、
# CI（MEAN/UP/LOW）、RR，同时携带元数据属性。
#   custom_path  可选的自定义 RR 查找 xlsx；NULL 时从
#                RR_std_config.json 解析。
# 浓度网格固定为 0.1：查找表的最小浓度步长必须不小于 0.1。
# 同时设置全局变量 .PWRR_branch 和 .RR_meta；set_Model() 把
# 结果存入 .RR_std_tbl。配置或查找表缺失、工作表缺失、
# 浓度非法，或网格、范围不一致时中止。
RR_std <- function(custom_path = NULL) {
# 在顶部读一次配置
  cfg_path <- './Data/RR_std_config.json'
  if (!file.exists(cfg_path)) stop("RR_std config not found: ", cfg_path)
  cfg_all <- jsonlite::fromJSON(cfg_path, simplifyVector = FALSE)
  CR <- get(".CR_Model", envir = globalenv())
  cfg <- cfg_all[[CR]]
  if (is.null(cfg) && str_detect(CR, '^IER')) cfg <- cfg_all[["IER"]]
  if (is.null(cfg)) stop("No RR_std config for model \"", CR, "\" in ", cfg_path)

# PWRR 分支模式："branch" 表示每个 CI 分支各用一个 PWRR；"mean" 表示所有分支共用
# MEAN PWRR（曲线很宽时仍保持 CI 有序）。
  pwr_branch <- cfg$pwr_branch %||% "branch"
  if (!pwr_branch %in% c("branch", "mean")) {
    stop("pwr_branch must be 'branch' or 'mean' for model ", CR, ".")
  }
  assign(".PWRR_branch", pwr_branch, envir = globalenv())

  conc_col <- cfg$conc_col %||% "concentration"
  conc_unit <- cfg$conc_unit %||% NULL
  conc_range <- cfg$conc_range %||% NULL
  assign(
    ".RR_meta",
    list(
      model = CR,
      conc_unit = conc_unit,
      conc_range = conc_range,
      pwr_branch = pwr_branch
    ),
    envir = globalenv()
  )

# 解析查找路径
  lookup <- if (!is.null(custom_path)) custom_path else cfg$lookup
  if (is.null(lookup)) stop("No lookup path for model \"", CR, "\" in ", cfg_path)
  if (!file.exists(lookup)) stop("RR lookup table not found: ", lookup)

# 读取全部工作表，把浓度列转成可匹配的字符串以便连接
  sheets <- readxl::excel_sheets(lookup)
  required_sheets <- c("MEAN", "LOW", "UP")
  missing_sheets <- setdiff(required_sheets, sheets)
  if (length(missing_sheets) > 0) {
    stop(
      "RR lookup is missing sheet(s): ", paste(missing_sheets, collapse = ", "),
      ". Found: ", paste(sheets, collapse = ", ")
    )
  }
  sheets <- required_sheets

  .raw <- set_names(
    map(sheets, function(sheet) {
      df <- readxl::read_excel(lookup, sheet = sheet)
      if (!conc_col %in% names(df)) {
        stop("RR sheet \"", sheet, "\" has no concentration column \"", conc_col, "\".")
      }
      conc_num <- as.numeric(df[[conc_col]])
      if (any(!is.finite(conc_num))) {
        stop("RR sheet \"", sheet, "\" has non-finite concentration values.")
      }
      if (anyDuplicated(conc_num)) {
        stop("RR sheet \"", sheet, "\" has duplicated concentration values.")
      }
      # 项目连接网格固定为 0.1：更细的源表会在取整后出现重复键，
      # 直接拒绝，避免静默丢精度。
      conc_step <- if (length(conc_num) > 1) min(diff(sort(conc_num))) else Inf
      if (is.finite(conc_step) && conc_step < 0.1 - 1e-8) {
        stop(
          "RR sheet \"", sheet, "\" has concentration steps finer than 0.1 ",
          "(min step = ", format(conc_step, digits = 12), "). ",
          "The project join grid is fixed at 0.1."
        )
      }
      df |>
        mutate(!!sym(conc_col) := matchable(conc_num, 1))
    }),
    sheets
  )

  conc_vectors <- map(.raw, ~ as.character(.x[[conc_col]]))
  if (!all(map_lgl(conc_vectors, identical, conc_vectors[[1]]))) {
    stop("RR sheets do not share the same concentration grid.")
  }

  if (!is.null(conc_range)) {
    actual_range <- as.numeric(conc_vectors[[1]]) |> range()
    expected_range <- as.numeric(unlist(conc_range))
    if (
      length(expected_range) != 2 ||
        any(!is.finite(expected_range)) ||
        any(abs(actual_range - expected_range) > 0.05)
    ) {
      stop(
        "RR concentration range mismatch: config expects ",
        paste(expected_range, collapse = " to "),
        ", lookup contains ",
        paste(actual_range, collapse = " to "),
        "."
      )
    }
  }

# 每个工作表独立处理（pivot → expand → join → fill → filter），
# 然后堆叠。这样避开 complete()，并处理缺失的年龄列（例如
# GEMM 的 85/90/95 用 80 岁的 RR），不填充空格子：配置中的年龄
# 若没有对应列，取前面最近可用年龄的 RR；若列存在
# 但格子为空，则保持 NA，交给下面的非有限值守卫。
  result <- imap_dfr(.raw, function(df, ci) {
    RR_one <- df |>
      pivot_longer(
        cols = -all_of(conc_col),
        names_to = c("endpoint", "agegroup"),
        names_sep = '_',
        values_to = "RR"
      ) |>
      mutate(endpoint = tolower(endpoint))

    if (nrow(RR_one) == 0) {
      stop("RR sheet \"", ci, "\" is empty after pivot.")
    }

    conc_vals <- df |> pull(!!sym(conc_col))
    present <- RR_one |> distinct(endpoint, agegroup)
    endpoint_specs <- map(cfg$endpoints, function(ep) {
      list(
        name = tolower(ep$name),
        ages = c('ALL', matchable(unlist(ep$ages), 0))
      )
    })
    fill_map <- map_dfr(endpoint_specs, function(spec) {
      available <- present$agegroup[present$endpoint == spec$name]
      source_age <- rep(NA_character_, length(spec$ages))
      previous <- NA_character_
      for (i in seq_along(spec$ages)) {
        if (spec$ages[i] %in% available) {
          source_age[i] <- spec$ages[i]
          previous <- spec$ages[i]
        } else {
          source_age[i] <- previous
        }
      }
      tibble(
        endpoint = spec$name,
        agegroup = spec$ages,
        source_agegroup = source_age
      )
    })
    map_dfr(endpoint_specs, function(spec) {
      expand_grid(
        endpoint = spec$name,
        agegroup = spec$ages,
        .conc = conc_vals
      )
    }) |>
      rename(!!conc_col := .conc) |>
      left_join(RR_one, by = c(conc_col, "endpoint", "agegroup")) |>
      left_join(fill_map, by = c("endpoint", "agegroup")) |>
      left_join(
        RR_one |>
          select(all_of(conc_col), endpoint, agegroup, RR_source = RR) |>
          rename(source_agegroup = agegroup),
        by = c(conc_col, "endpoint", "source_agegroup")
      ) |>
      mutate(RR = coalesce(RR, RR_source)) |>
      select(-source_agegroup, -RR_source) |>
      filter(agegroup != 'ALL') |>
      mutate(CI = ci)
  }) |>
    mutate(
      !!sym(conc_col) := matchable(as.numeric(!!sym(conc_col)), 1),
      agegroup = matchable(as.numeric(agegroup), 0),
      endpoint = tolower(endpoint)
    ) |>
    rename(concentration = !!sym(conc_col)) |>
    select(concentration, endpoint, agegroup, CI, RR)

  n_bad_rr <- sum(!is.finite(result$RR))
  if (n_bad_rr > 0) {
    log_msg(
      WARN,
      "{n_bad_rr} non-finite RR value(s) in {lookup}. ",
      "Legacy tables load unchanged; Mortality() drops affected rows."
    )
  }
  if (anyDuplicated(result[c("concentration", "endpoint", "agegroup", "CI")])) {
    stop("RR table contains duplicated concentration/endpoint/agegroup/CI keys.")
  }

  ci_order <- result |>
    select(concentration, endpoint, agegroup, CI, RR) |>
    pivot_wider(names_from = CI, values_from = RR)
  n_ci_order <- sum(ci_order$LOW > ci_order$MEAN + 1e-10, na.rm = TRUE) +
    sum(ci_order$MEAN > ci_order$UP + 1e-10, na.rm = TRUE)
  if (n_ci_order > 0) {
    log_msg(
      WARN,
      "{n_ci_order} RR row(s) violate LOW <= MEAN <= UP in {lookup}. ",
      "This legacy table is loaded unchanged; rebuilds must pass the strict guard."
    )
  }

  attr(result, "conc_unit") <- conc_unit
  attr(result, "conc_range") <- conc_range
  attr(result, "pwr_branch") <- pwr_branch
# 此处的键已规范化（concentration 和 agegroup 可匹配，endpoint
# 为小写，CI 已校验，无重复）。Mortality() 用这个标志
# 跳过 RR_std() 已构建的表的重复归一化。
  attr(result, "rr_normalized") <- TRUE

  log_msg(INFO, "RR table built: {lookup} ({nrow(result)} rows)")
  result
}
