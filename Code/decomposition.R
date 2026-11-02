# Decomposition 模块 — PM2.5 健康负担的驱动因素
#
#   decomp_sequences          — 24 条累计因子序列（按记录的排列顺序）
#   Decomposition()           — 单条序列的网格级贡献
#   Decomposition_summary()   — 单条序列的国家级贡献
#   aggregate_drivers()       — 全部 24 条序列；写出 Summary 与 Sequences
#
# state cache：24 条序列共用 16 个唯一 factor state。get_decomp_state()
# 与 decomp_state_summary() 按排序后的因子集合缓存它们。Code/kernel.R 计划
# 启用时，decomp_fill_cache() 用一批并发任务构建缺失的 state；没有计划时
# cache 路径保持串行。
#
# 输入 cache（v6.3）：各 factor state 只差在每个因子读取哪个情景列，因此
# decomp_input_cache() 每次运行只构建两个 RR 矩阵与四张 PWRR 表，
# state_grid_cached() 复用它们。
# 缺少向量化引擎（Code/mortality.R）时，或 harness 在自己的环境中替换
# Mortality() 时，模块保留 join 路径。
#
# 依赖：tidyverse、writexl。加载方：DrivingFactors.R。
# 注 261026：PWRR 按 domain × endpoint × agegroup 计算。
# 注 261027：通过 Code/kernel.R 并发构建 state（可选）。
# 修改 261101：差分前做网格集守卫（C1）；每次运行输出一份简洁的 NA 与丢格
#   报告（I7）；decomp_sequences 在加载时校验（I8）；
#   aggregate_drivers() 只缓存 state 摘要（I9），写文件前创建 Results/
#   （C2）。
# 修改 261101（v6.3）：与 state 无关的输入每次运行缓存一次
#   （decomp_input_cache()、state_grid_cached()）；16 个 state 只构建两个
#   RR 矩阵与四张 PWRR 表，不再各构建 16 份。数值保持逐位一致，因为
#   cache 路径调用同一批引擎函数。
# 修改 261101（v6.3 诊断）：cache 路径打印与 Mortality() 相同的 clamp、
#   orphan 与覆盖报告；这些报告 v6.2 按 state 打印，现在每次运行一次，
#   并沿用 v6.2 的标签（Conc_r / Conc_c）。aggregate_drivers() 对 16 份
#   摘要做一次网格集守卫，之后每次 Decomposition_summary() 调用都跳过
#   它。

# 24 条分解序列，按记录的排列顺序明文写出 ----
# step1..step4 列给出各因子的施加顺序。

decomp_sequences <- tribble(
  ~serie, ~step1, ~step2, ~step3, ~step4,
  1,  "PG",  "PA",  "EXP", "ORF",
  2,  "PG",  "PA",  "ORF", "EXP",
  3,  "PG",  "EXP", "PA",  "ORF",
  4,  "PG",  "EXP", "ORF", "PA",
  5,  "PG",  "ORF", "PA",  "EXP",
  6,  "PG",  "ORF", "EXP", "PA",
  7,  "PA",  "PG",  "EXP", "ORF",
  8,  "PA",  "PG",  "ORF", "EXP",
  9,  "PA",  "EXP", "PG",  "ORF",
  10, "PA",  "EXP", "ORF", "PG",
  11, "PA",  "ORF", "PG",  "EXP",
  12, "PA",  "ORF", "EXP", "PG",
  13, "EXP", "PG",  "PA",  "ORF",
  14, "EXP", "PG",  "ORF", "PA",
  15, "EXP", "PA",  "PG",  "ORF",
  16, "EXP", "PA",  "ORF", "PG",
  17, "EXP", "ORF", "PA",  "PG",
  18, "EXP", "ORF", "PG",  "PA",
  19, "ORF", "PG",  "PA",  "EXP",
  20, "ORF", "PG",  "EXP", "PA",
  21, "ORF", "PA",  "PG",  "EXP",
  22, "ORF", "PA",  "EXP", "PG",
  23, "ORF", "EXP", "PG",  "PA",
  24, "ORF", "EXP", "PA",  "PG"
)

# 在加载时失败，避免等 30 秒运行结束才发现：
# 序列行写错会让一个因子施加两次并漏掉另一个，
# 分解输出也不会留下这次错误的任何痕迹（I8）。
validate_decomp_sequences <- function(x = decomp_sequences) {
  required <- c("serie", "step1", "step2", "step3", "step4")
  missing_cols <- setdiff(required, names(x))
  if (length(missing_cols) > 0) {
    stop(
      "decomp_sequences is missing column(s): ",
      paste(missing_cols, collapse = ", "),
      "."
    )
  }
  if (nrow(x) != 24) {
    stop("decomp_sequences must have 24 rows, got ", nrow(x), ".")
  }
  duplicated_serie <- unique(x$serie[duplicated(x$serie)])
  if (length(duplicated_serie) > 0) {
    stop(
      "decomp_sequences$serie must be unique; duplicated value(s): ",
      paste(duplicated_serie, collapse = ", "),
      "."
    )
  }

  expected <- c("PG", "PA", "EXP", "ORF")
  step_cols <- c("step1", "step2", "step3", "step4")
  steps <- as.matrix(x[step_cols])
  is_permutation <- apply(steps, 1, function(row) {
    setequal(row, expected) && !anyDuplicated(row)
  })
  if (!all(is_permutation)) {
    bad_rows <- which(!is_permutation)
    bad_text <- x |>
      slice(bad_rows) |>
      mutate(across(all_of(step_cols), as.character)) |>
      unite("row", all_of(step_cols), sep = ", ") |>
      pull(row)
    stop(
      "decomp_sequences row(s) ",
      paste(bad_rows, collapse = ", "),
      " must each be a permutation of PG, PA, EXP, ORF; got: ",
      paste(bad_text, collapse = "; "),
      "."
    )
  }
  invisible(TRUE)
}

validate_decomp_sequences(decomp_sequences)

# 施加因子前缀后的 state ----
# PG  -> Pop (start -> end)
# PA  -> AgeStruc (start -> end)
# EXP -> Conc_c (start -> end)
# ORF -> Conc_r (start -> end) 与 MortRate (start -> end)

mort_state <- function(prefix, start.y, end.y) {
  list(
    pop = if ("PG" %in% prefix) end.y else start.y,
    ag = if ("PA" %in% prefix) end.y else start.y,
    conc_c = if ("EXP" %in% prefix) end.y else start.y,
    conc_r = if ("ORF" %in% prefix) end.y else start.y,
    mrate = if ("ORF" %in% prefix) end.y else start.y
  )
}

validate_scenario_columns <- function(start.y, end.y) {
  datasets <- list(
    Conc_real = Conc_real,
    Pop = Pop,
    MortRate = MortRate,
    AgeGroup = AgeGroup
  )
  for (name in names(datasets)) {
    missing_cols <- setdiff(c(start.y, end.y), names(datasets[[name]]))
    if (length(missing_cols) > 0) {
      stop(
        name, " is missing scenario column(s): ",
        paste(missing_cols, collapse = ", ")
      )
    }
  }
  invisible(TRUE)
}

add_domain <- function(mort, domain) {
  mort |>
    left_join(
      Grid_info |> select(x, y, all_of(domain)),
      by = c("x", "y")
    )
}

mort_at_prefix <- function(prefix, start.y, end.y, domain) {
  state <- mort_state(prefix, start.y, end.y)
  Mortality(
    Grids = Grid_info,
    CI = "MEAN",
    pop = Pop |> select(x, y, Pop = !!sym(state$pop)),
    ag = AgeGroup |>
      select(all_of("domain"), agegroup, AgeStruc = !!sym(state$ag)),
    Conc_c = Conc_real |>
      select(x, y, concentration = !!sym(state$conc_c)),
    Conc_r = Conc_real |>
      select(x, y, concentration = !!sym(state$conc_r)),
    mRate = getMortRate(state$mrate),
    domain = domain,
    validate_keys = FALSE
  )
}

# 返回 TRUE 表示 cache 输入路径可以运行。它需要 v6.3 引擎函数、
# 这些函数用于打印的诊断辅助函数，以及与 Mortality() 同作用域的 RR 表，
# 因为 cache 路径复现 Mortality() 的引擎内部逻辑。
# harness 在自己的环境中用 stub 替换 Mortality() 时保留 join 路径；
# 不含引擎的检出则平稳回退。
# `env` 是运行 cache 路径的作用域。
# 默认取调用方的帧。
decomp_engine_available <- function(env = parent.frame()) {
  engine_functions <- c(
    "mort_rr_matrix", "mort_pwr_table", "mort_grid_table",
    "mortality_clamp_report", "mortality_clamp_text",
    "mortality_orphan_report", "mortality_coverage_cached",
    "mortality_assert_rr_key"
  )
  present <- vapply(
    engine_functions,
    function(name) {
      exists(name, envir = env, mode = "function", inherits = TRUE)
    },
    logical(1)
  )
  if (!all(present) ||
    !exists("Mortality", envir = env, mode = "function", inherits = TRUE)) {
    return(FALSE)
  }
  rr_raw <- get0(
    ".RR_std_tbl",
    envir = env,
    inherits = TRUE,
    ifnotfound = NULL
  )
  if (is.null(rr_raw) || nrow(rr_raw) == 0) {
    return(FALSE)
  }
  # 同作用域：生产环境中四个函数都来自 globalenv。
  # 赋给测试环境的 stub Mortality() 与引擎不在同一作用域。
  identical(
    environment(get("Mortality", envir = env, inherits = TRUE)),
    environment(get("mort_grid_table", envir = env, inherits = TRUE))
  )
}

# 一组缓存输入的标识：情景名、domain，以及 RR 表的标识
# （模型名、行数）。浓度网格固定为 0.1，不再进 key。
decomp_input_key <- function(start.y, end.y, domain) {
  rr_raw <- get0(".RR_std_tbl", ifnotfound = NULL, inherits = TRUE)
  model <- get0(".CR_Model", ifnotfound = NA_character_, inherits = TRUE)
  str_c(
    start.y,
    end.y,
    str_c(as.character(domain), collapse = "+"),
    model %||% NA_character_,
    nrow(rr_raw) %||% 0L,
    sep = "::"
  )
}

# 在一次分解运行内缓存 state；排序后的前缀标识等价输入。
new_decomp_cache <- function() {
  cache <- new.env(parent = emptyenv())
  cache$states <- new.env(parent = emptyenv())
  cache$n_mortality <- 0L
  # 输入 cache，由 decomp_inputs_for() 在第一个 state 时惰性构建。
  cache$inputs <- NULL
  cache$engine <- exists("decomp_engine_available", mode = "function") &&
    decomp_engine_available()
  cache
}

decomp_state_key <- function(prefix, start.y, end.y, domain) {
  factors <- if (length(prefix) == 0) {
    "<start>"
  } else {
    str_c(sort(prefix), collapse = "+")
  }
  str_c(start.y, end.y, str_c(domain, collapse = "+"), factors, sep = "::")
}

# 与 state 无关的输入，以及每个 state 共用的 RR 矩阵与 PWRR 表。
# 每个（start.y、end.y、domain、RR 表）组合运行一次。
#
# 内存：一个 RR 矩阵是 n_grid x G 个 double；cache 保存两个这样的矩阵、
# 四张 G x D 的 PWRR 表、两个人口向量，以及 G x D 的分 domain 年龄结构
# 与死亡率矩阵。
# 在 68,302 个网格、15 个分组时约占 18 MB，再加约 33 MB 用于四份预先
# 展开的 G x n_grid 副本（仅当 n_grid x G 不超过 2e6 个单元时才缓存）。
# 在 0.1 度（约 800,000 个网格）时一个 RR 矩阵约 96 MB，cache 保持在两个
# RR 矩阵约 192 MB 的规模，分组矩阵按 state 展开。
decomp_input_cache <- function(start.y, end.y, domain) {
  rr_raw <- .RR_std_tbl
  if (is.null(rr_raw) || nrow(rr_raw) == 0) {
    stop("RR lookup table is empty. Run set_Model() first.")
  }
  require_columns(
    rr_raw,
    c("concentration", "endpoint", "agegroup", "CI", "RR"),
    "RR lookup table"
  )

  # ---- 与 Mortality() 相同的单位与归一化守卫 ----
  rr_meta <- get0(".RR_meta", envir = globalenv(), ifnotfound = list())
  conc_unit <- get0(".CONC_UNIT", envir = globalenv(), ifnotfound = NULL)
  rr_unit <- attr(rr_raw, "conc_unit", exact = TRUE) %||%
    rr_meta$conc_unit %||%
    NULL
  if (!is.null(conc_unit) && !is.null(rr_unit) && !identical(conc_unit, rr_unit)) {
    stop(
      "Concentration unit mismatch: RR model expects \"", rr_unit,
      "\", data were loaded as \"", conc_unit, "\"."
    )
  }
  if (!isTRUE(attr(rr_raw, "rr_normalized", exact = TRUE))) {
    rr_raw <- rr_raw |>
      mutate(
        concentration = matchable(as.numeric(concentration), 1),
        endpoint = tolower(endpoint),
        agegroup = matchable(as.numeric(agegroup), 0)
      )
    # RR_std() 在设置 rr_normalized 之前先对自己构建的表做键检查，
    # 因此昂贵的重复扫描只对手工构建的表运行。该检查与 Mortality() 共用，
    # 每个表标识运行一次。
    mortality_assert_rr_key(rr_raw)
  }
  if (!setequal(unique(rr_raw$CI), c("MEAN", "UP", "LOW"))) {
    stop("RR lookup table must contain exactly MEAN, UP, and LOW branches.")
  }
  rr_conc_range <- range(as.numeric(rr_raw$concentration), na.rm = TRUE)

  # ---- 情景列，每个不同的情景名一个条目 ----
  scenarios <- unique(c(start.y, end.y))
  require_columns(Grid_info, c("x", "y", domain), "Grids")
  require_columns(Pop, c("x", "y", scenarios), "pop")
  require_columns(Conc_real, c("x", "y", scenarios), "Conc_real")
  require_columns(AgeGroup, c("domain", "agegroup", scenarios), "AgeGroup")
  grid_domain <- Grid_info |>
    select(x, y, domain = all_of(domain))
  # 每个 state 网格都是 Grid_info 的行子集，因此这里做一次键检查就覆盖了
  # Mortality() 逐 state 做的 "Mortality output" 检查。
  assert_unique_key(grid_domain, c("x", "y"), "Grid_info")

  # 浓度：真实列与反事实列都来自 Conc_real，因此一个缓存向量可承担任一
  # 角色。一次 join 携带所有情景列；clamp 与 matchable() 仍按列进行，与
  # Mortality() 完全一致。
  #
  # 同一情景列在某些 factor state 中是 Conc_r，在另一些中是 Conc_c。v6.2
  # 按角色、按 state 各打印一份报告；去重后的文本每个角色一份，因此每列
  # 只 clamp 一次，用同一组计数描述两种角色。
  conc_frame <- Conc_real |> select(x, y, all_of(scenarios))
  clamp_reports <- character()
  for (at in scenarios) {
    report <- mortality_clamp_report(
      as.numeric(conc_frame[[at]]),
      rr_conc_range,
      "Conc_r"
    )
    conc_frame[[at]] <- matchable(report$value, 1)
    clamp_reports <- c(
      clamp_reports,
      report$text,
      mortality_clamp_text(report$n_lo, report$n_hi, rr_conc_range, "Conc_c")
    )
  }
  for (text in unique(clamp_reports)) {
    log_msg(WARN, "{text}")
  }
  conc_joined <- grid_domain |> left_join(conc_frame, by = c("x", "y"))
  conc <- map(scenarios, function(at) conc_joined[[at]]) |>
    set_names(scenarios)

  pop_frame <- Pop |> select(x, y, all_of(scenarios))
  for (at in scenarios) {
    values <- as.numeric(pop_frame[[at]])
    if (any(values < 0, na.rm = TRUE)) {
      stop("Population contains negative values.")
    }
    pop_frame[[at]] <- values
  }
  pop_joined <- grid_domain |> left_join(pop_frame, by = c("x", "y"))
  pop <- map(scenarios, function(at) pop_joined[[at]]) |>
    set_names(scenarios)

  # Conc_real 与 Pop 的网格集不随情景列变化，因此各做一份 orphan 报告
  # 即可覆盖所有 state；v6.2 曾按 state 重复同一段文本。show_n 取 6，
  # 因为 Decomposition() 没有 verbose 开关。
  orphan_reports <- c(
    mortality_orphan_report(
      grid_domain[c("x", "y")],
      conc_frame |> select(x, y) |> distinct(),
      Grid_info,
      domain,
      "concentration",
      6
    ),
    mortality_orphan_report(
      grid_domain[c("x", "y")],
      pop_frame |> select(x, y) |> distinct(),
      Grid_info,
      domain,
      "population",
      6
    )
  )
  for (text in orphan_reports) {
    log_msg(WARN, "{text}")
  }

  ag <- map(scenarios, function(at) {
    AgeGroup |>
      select(domain, agegroup, AgeStruc = !!sym(at)) |>
      mutate(
        agegroup = matchable(as.numeric(agegroup), 0),
        AgeStruc = as.numeric(AgeStruc)
      )
  }) |>
    set_names(scenarios)

  mrate <- map(scenarios, function(at) {
    getMortRate(at) |>
      mutate(
        endpoint = tolower(endpoint),
        agegroup = matchable(as.numeric(agegroup), 0),
        MortRate = as.numeric(MortRate)
      )
  }) |>
    set_names(scenarios)

  # ---- 分组、RR 矩阵、PWRR 表：共用的昂贵部分 ----
  mean_branch <- rr_raw[
    rr_raw$CI == "MEAN",
    c("concentration", "endpoint", "agegroup", "RR")
  ]
  groups <- mean_branch |> distinct(endpoint, agegroup)
  if (nrow(groups) == 0) {
    stop("RR lookup table has no endpoint-agegroup groups.")
  }
  # ---- 每个 state 可能读到的情景组合的覆盖报告 ----
  # 人口与反事实浓度决定网格行；死亡率表与年龄表决定被覆盖的
  # endpoint/agegroup 分组。报告只读 NA 掩码与覆盖集合，不读数值，因此
  # mortality_coverage_cached() 在这里把 16 个组合合并为一次计算。
  # v6.2 曾按 state 重复同一段文本；
  # 每种不同文本只打印一份。
  domain_vec <- grid_domain[["domain"]]
  coverage_reports <- character()
  for (conc_at in scenarios) {
    for (pop_at in scenarios) {
      base_c <- tibble(
        x = grid_domain$x,
        y = grid_domain$y,
        domain = domain_vec,
        concentration = conc[[conc_at]],
        Pop = pop[[pop_at]]
      )
      for (mrate_at in scenarios) {
        for (ag_at in scenarios) {
          usable_groups <- groups |>
            semi_join(
              mrate[[mrate_at]] |> distinct(endpoint, agegroup),
              by = c("endpoint", "agegroup")
            ) |>
            semi_join(
              ag[[ag_at]] |> distinct(agegroup),
              by = "agegroup"
            )
          # 没有可用分组时，Mortality() 在输出覆盖报告之前就已中止，
          # 因此该组合不打印任何内容。
          if (nrow(usable_groups) == 0) {
            next
          }
          coverage_reports <- c(
            coverage_reports,
            mortality_coverage_cached(
              base_c = base_c,
              usable_groups = usable_groups,
              mrate = mrate[[mrate_at]],
              ag = ag[[ag_at]],
              domain = domain,
              show_n = 6
            )
          )
        }
      }
    }
  }
  for (text in unique(coverage_reports)) {
    log_msg(WARN, "{text}")
  }

  value_names <- str_c(groups$endpoint, groups$agegroup, "MEAN", sep = "_")

  dom_names <- unique(as.character(domain_vec[!is.na(domain_vec)]))
  dom_code <- match(as.character(domain_vec), dom_names)

  rr <- map(scenarios, function(at) {
    mort_rr_matrix(conc[[at]], mean_branch, groups)
  }) |>
    set_names(scenarios)

  pwr <- list()
  for (pop_at in scenarios) {
    # 每个人口情景调用一次：分支版本一次性为两个浓度列拆分 domain 行与
    # 人口分母。
    tables <- mort_pwr_table_branches(
      rr[scenarios],
      pop[[pop_at]],
      domain_vec
    )
    for (index in seq_along(scenarios)) {
      pwr[[str_c(pop_at, "|", scenarios[[index]])]] <- tables[[index]]
    }
  }

  # 分 domain 的分组取值（G x D）按 state 展开为 G x n_grid；0.1 度网格
  # 下一个 G x n_grid 矩阵占 96 MB，这样处理让 cache 保持较小。
  group_domain_matrix <- function(table, by_columns, value_column) {
    out <- matrix(NA_real_, nrow = nrow(groups), ncol = length(dom_names))
    for (g in seq_len(nrow(groups))) {
      keep <- rep(TRUE, nrow(table))
      for (column in by_columns) {
        keep <- keep & table[[column]] == groups[[column]][[g]]
      }
      one <- table[keep, , drop = FALSE]
      out[g, ] <- one[[value_column]][match(dom_names, as.character(one$domain))]
    }
    out
  }
  ag_dom <- map(ag, function(table) {
    group_domain_matrix(table, "agegroup", "AgeStruc")
  }) |>
    set_names(scenarios)
  mrate_dom <- map(mrate, function(table) {
    group_domain_matrix(table, c("endpoint", "agegroup"), "MortRate")
  }) |>
    set_names(scenarios)

  # 四份 G x n_grid 矩阵规模较小时一次展开。
  # 在 68,302 个网格、15 个分组时约占 33 MB；
  # 在 0.1 度（约 800,000 个网格）时同样四份约需 384 MB，因此 cache 只保留
  # 分 domain 的表，在那里按 state 展开。
  # 两种方式数值一致；该上限把 cache 限制在两个 RR 矩阵的规模附近。
  expanded_cells <- nrow(grid_domain) * nrow(groups)
  if (expanded_cells <= 2e6) {
    ag_grid <- map(ag_dom, function(one) one[, dom_code, drop = FALSE]) |>
      set_names(scenarios)
    mrate_grid <- map(mrate_dom, function(one) one[, dom_code, drop = FALSE]) |>
      set_names(scenarios)
  } else {
    ag_grid <- NULL
    mrate_grid <- NULL
  }

  list(
    key = decomp_input_key(start.y, end.y, domain),
    start.y = start.y,
    end.y = end.y,
    domain = domain,
    grid_xy = grid_domain[c("x", "y")],
    domain_vec = domain_vec,
    groups = groups,
    value_names = value_names,
    dom_names = dom_names,
    dom_code = dom_code,
    conc = conc,
    rr = rr,
    pwr = pwr,
    pop = pop,
    ag_dom = ag_dom,
    mrate_dom = mrate_dom,
    ag_grid = ag_grid,
    mrate_grid = mrate_grid,
    n_rr = length(rr),
    n_pwr = length(pwr)
  )
}

# 为一个 decomp cache 惰性构建输入 cache 并复用它，直到情景名、domain
# 或 RR 表标识发生变化。
decomp_inputs_for <- function(cache, start.y, end.y, domain) {
  key <- decomp_input_key(start.y, end.y, domain)
  inputs <- cache$inputs
  if (is.null(inputs) || !identical(inputs$key, key)) {
    inputs <- decomp_input_cache(start.y, end.y, domain)
    cache$inputs <- inputs
  }
  inputs
}

# 按 Mortality() 的报告顺序逐 state 做 PWRR 守卫：Inf 或 PWRR < 1 中止，
# NaN 告警并丢弃受影响的行。逐 state 运行保持 join 路径的告警次数
# （每次 Mortality() 调用一条告警）。
decomp_check_pwrr <- function(pwr_tbl, groups, dom_names, show_n = 6) {
  skipped_keys <- character()
  for (g in seq_len(nrow(groups))) {
    one <- pwr_tbl[g, ]
    is_inf <- is.infinite(one)
    is_lt1 <- one < 1 & !is.na(one)
    is_nan <- is.nan(one)
    if (any(is_inf) || any(is_lt1)) {
      bad_pos <- which(is_inf | is_lt1)
      bad_keys <- str_c(
        dom_names[bad_pos],
        groups$endpoint[[g]],
        groups$agegroup[[g]],
        sep = "/"
      )
      stop(
        "Invalid PWRR detected for endpoint/agegroup ",
        groups$endpoint[[g]],
        "/",
        groups$agegroup[[g]],
        ". ",
        if (any(is_inf)) "Some domains have Inf PWRR. ",
        if (any(is_lt1)) "Some have PWRR < 1. ",
        "Check concentration and population for: ",
        trunc_fmt(unique(bad_keys), n = show_n)
      )
    }
    if (any(is_nan)) {
      nan_pos <- which(is_nan)
      skipped_keys <- c(
        skipped_keys,
        str_c(
          dom_names[nan_pos],
          groups$endpoint[[g]],
          groups$agegroup[[g]],
          sep = "/"
        )
      )
    }
  }
  skipped_keys <- unique(skipped_keys)
  if (length(skipped_keys) > 0) {
    log_msg(
      WARN,
      "{length(skipped_keys)} domain/endpoint/age combination(s) have no valid ",
      "PWRR; affected rows will be dropped: ",
      trunc_fmt(skipped_keys, n = show_n)
    )
  }
  invisible(NULL)
}

# 由缓存输入生成一个 state 网格。语义等同 decomp_state_grid()：相同的
# 分组、RR 查找、PWRR 分母、保留掩码与 add_domain() 收尾。
state_grid_from_inputs <- function(prefix, inputs, domain) {
  state <- mort_state(prefix, inputs$start.y, inputs$end.y)
  pwr <- inputs$pwr[[str_c(state$pop, "|", state$conc_r)]]
  decomp_check_pwrr(pwr, inputs$groups, inputs$dom_names)
  ag <- if (is.null(inputs$ag_grid)) {
    inputs$ag_dom[[state$ag]][, inputs$dom_code, drop = FALSE]
  } else {
    inputs$ag_grid[[state$ag]]
  }
  mrate <- if (is.null(inputs$mrate_grid)) {
    inputs$mrate_dom[[state$mrate]][, inputs$dom_code, drop = FALSE]
  } else {
    inputs$mrate_grid[[state$mrate]]
  }
  mort <- mort_grid_table(
    rr_c = inputs$rr[[state$conc_c]],
    rr_r = inputs$rr[[state$conc_r]],
    pwr = pwr,
    pop = inputs$pop[[state$pop]],
    ag = ag,
    mrate = mrate,
    groups = inputs$groups,
    domain = inputs$domain_vec,
    value_names = inputs$value_names,
    grid_xy = inputs$grid_xy
  )
  if (nrow(mort) == 0) {
    stop("Mortality result has 0 rows. Check input coverage.")
  }
  # 这里无需检查输出键：decomp_input_cache() 已校验 Grid_info，
  # 且每个输出行都是它的行之一。
  add_domain(mort, domain = domain)
}

# 复用共享输入 cache 的网格级 state。存在 v6.3 引擎时等同
# decomp_state_grid()。
state_grid_cached <- function(prefix, start.y, end.y, domain, cache = NULL) {
  if (is.null(cache)) {
    cache <- new_decomp_cache()
  }
  inputs <- decomp_inputs_for(cache, start.y, end.y, domain)
  state_grid_from_inputs(prefix, inputs, domain)
}

# 一个缓存 state 的国家级摘要。
state_summary_cached <- function(prefix, start.y, end.y, domain, cache) {
  state_grid_cached(prefix, start.y, end.y, domain, cache = cache) |>
    state_summary_from_grid(domain = domain)
}

# 一条序列的五个累计因子前缀：起点，然后每一步。
decomp_series_prefixes <- function(serie) {
  seq_row <- decomp_sequences |> filter(serie == .env$serie)
  if (nrow(seq_row) != 1) {
    stop("serie must be one of 1:24.")
  }
  steps <- seq_row |>
    select(step1:step4) |>
    unlist(use.names = FALSE)
  list(
    character(0),
    steps[1],
    steps[1:2],
    steps[1:3],
    steps[1:4]
  )
}

# 所请求序列需要的唯一 factor state，以 decomp_state_key() 为键。
# 24 条序列共用 16 个 state；一条序列需要 5 个。
decomp_unique_prefixes <- function(start.y, end.y, domain, RUN24 = TRUE) {
  series <- if (RUN24) 1:24 else 1
  prefixes <- series |>
    map(decomp_series_prefixes) |>
    unlist(recursive = FALSE)
  keys <- prefixes |>
    map_chr(
      decomp_state_key,
      start.y = start.y,
      end.y = end.y,
      domain = domain
    )
  prefixes[!duplicated(keys)] |>
    set_names(unique(keys))
}

# 一个 state 的国家级摘要：按 domain × Cause_Age 求和的归因死亡。
# 摘要携带该 state 的网格数与 (x, y) 键集合，因此
# Decomposition_summary() 可以在各 state 覆盖的网格不一致时中止（C1）。
# NA 保持 NA：守恒恒等式需要 NA 传播，na.rm 会破坏它。
#
# 在宽网格上做一次分组求和，再把小的 domain 级表堆叠一次，
# 所得数值与把网格转长表后按 (domain, Cause_Age) 求和相同：
# 每组按相同顺序加相同的网格行。
# 对两个分组键做 arrange() 可复现 group_by() 的输出顺序，
# 因此摘要在避开拖慢 state 循环的 800k 行长表的同时
# 保持逐位一致。
state_summary_from_grid <- function(mort, domain) {
  value_cols <- setdiff(names(mort), c("x", "y", domain))
  summary <- mort |>
    group_by(across(all_of(domain))) |>
    summarise(across(all_of(value_cols), sum), .groups = "drop") |>
    pivot_longer(
      cols = -all_of(domain),
      names_to = "Cause_Age",
      values_to = "Mort"
    ) |>
    arrange(.data[[domain]], Cause_Age)
  attr(summary, "decomp_n_grid") <- nrow(mort)
  attr(summary, "decomp_grid_keys") <- str_c(mort$x, mort$y, sep = "|")
  summary
}

# C1 网格集守卫，作用于 state 摘要：做差分之前，每个 state 必须携带相同的
# (x, y) 键集合。返回 invisible(TRUE)；不匹配时中止，
# 摘要不含键集合时告警。Decomposition_summary() 对自己的五份摘要调用它；
# aggregate_drivers() 对 16 个唯一 state 调用一次，
# 之后给每条序列传 check_keys = FALSE。
decomp_guard_summary_grids <- function(summaries, state_labels, caller) {
  key_sets <- map(summaries, function(summary) {
    attr(summary, "decomp_grid_keys", exact = TRUE)
  })
  if (!all(map_lgl(key_sets, is.character))) {
    log_msg(
      WARN,
      caller, ": state summaries carry no grid key set; the grid-set guard is ",
      "skipped. Build summaries with state_summary_from_grid() to enable it."
    )
    return(invisible(FALSE))
  }
  n_keys <- map_int(key_sets, length)
  same_keys <- all(map_lgl(
    key_sets,
    function(keys) setequal(keys, key_sets[[1]])
  ))
  if (length(unique(n_keys)) > 1 || !same_keys) {
    stop(
      caller, " aborted: the factor states cover different (x, y) grid sets (",
      str_c(state_labels, " = ", n_keys, " grids", collapse = ", "),
      "). Differencing them would sum over different grids and bias every ",
      "domain total. Rebuild the state summaries with ",
      "state_summary_from_grid() from one grid set."
    )
  }
  invisible(TRUE)
}

# 一个 kernel 任务：构建 state 网格，然后只返回它的小摘要。
state_summary_for_prefix <- function(prefix, start.y, end.y, domain) {
  mort_at_prefix(prefix, start.y, end.y, domain) |>
    add_domain(domain = domain) |>
    state_summary_from_grid(domain = domain)
}

# 唯一 state 只汇总一次。
# 串行路径构建一次共享输入 cache，供每个 state 复用；
# 并发分发保留下面的 Mortality() 路径，
# 否则每个 worker 都要按任务重建两个 RR 矩阵。
precompute_state_summaries <- function(
  states,
  start.y,
  end.y,
  domain,
  cache = NULL
) {
  if (is.null(cache)) {
    cache <- new_decomp_cache()
  }
  kernel_on <- exists("kernel_active", mode = "function") &&
    isTRUE(kernel_active())
  # v6.3：引擎可用时，串行输入 cache 比分发更快
  # （16 个 state 实测 6.4 秒对 12.3 秒），因此即使有计划启用也选 cache。
  # 不含引擎的检出仍保留 kernel 路径，
  # 其测试通过 fixture 环境继续覆盖它。
  if (isTRUE(cache$engine)) {
    if (kernel_on) {
      log_msg(
        INFO,
        "kernel: state assembly uses the serial input cache; ",
        "dispatch would be slower after v6.3."
      )
    }
    summaries <- map(states, function(prefix) {
      state_summary_cached(prefix, start.y, end.y, domain, cache = cache)
    })
    return(set_names(summaries, names(states)))
  }

  summaries <- kernel_map(
    states,
    state_summary_for_prefix,
    start.y = start.y,
    end.y = end.y,
    domain = domain,
    .labels = names(states),
    .label = "DrivingFactors states",
    .error = "stop"
  )
  set_names(summaries, names(states))
}

# 网格级 state：一个累计因子前缀对应的死亡率表。
decomp_state_grid <- function(prefix, start.y, end.y, domain) {
  mort_at_prefix(prefix, start.y, end.y, domain) |>
    add_domain(domain = domain)
}

# 填补 cache 中仍然缺失的条目，每个 state 一个 kernel 任务。
# 没有激活的 kernel 计划时不做任何事，因此串行路径不变。
decomp_fill_cache <- function(prefixes, start.y, end.y, domain, cache) {
  kernel_on <- exists("kernel_active", mode = "function") &&
    isTRUE(kernel_active())
  # cache 引擎路径在主进程中按需构建 state；
  # 并发填补会在每个 worker 中重建输入矩阵。
  if (isTRUE(cache$engine)) {
    return(invisible(cache))
  }
  if (!kernel_on) {
    return(invisible(cache))
  }

  keys <- prefixes |>
    map_chr(
      decomp_state_key,
      start.y = start.y,
      end.y = end.y,
      domain = domain
    )
  is_cached <- vapply(
    keys,
    function(key) exists(key, envir = cache$states, inherits = FALSE),
    logical(1)
  )
  if (all(is_cached)) {
    return(invisible(cache))
  }

  states <- prefixes[!is_cached] |> set_names(keys[!is_cached])
  grids <- kernel_map(
    states,
    decomp_state_grid,
    start.y = start.y,
    end.y = end.y,
    domain = domain,
    .labels = names(states),
    .label = "Decomposition states",
    .error = "stop"
  )

  # 把取回的网格按规范键存好，并统计 Mortality 调用次数。
  for (index in seq_along(grids)) {
    assign(
      names(grids)[[index]],
      list(grid = grids[[index]], summary = NULL),
      envir = cache$states
    )
  }
  cache$n_mortality <- cache$n_mortality + length(grids)
  invisible(cache)
}

get_decomp_state <- function(prefix, start.y, end.y, domain, cache) {
  key <- decomp_state_key(prefix, start.y, end.y, domain)
  if (exists(key, envir = cache$states, inherits = FALSE)) {
    return(get(key, envir = cache$states, inherits = FALSE))
  }

  mort <- if (isTRUE(cache$engine)) {
    state_grid_cached(prefix, start.y, end.y, domain, cache = cache)
  } else {
    decomp_state_grid(prefix, start.y, end.y, domain)
  }
  entry <- list(grid = mort, summary = NULL)
  assign(key, entry, envir = cache$states)
  cache$n_mortality <- cache$n_mortality + 1L
  entry
}

decomp_state_summary <- function(prefix, start.y, end.y, domain, cache) {
  entry <- get_decomp_state(prefix, start.y, end.y, domain, cache)
  if (is.null(entry$summary)) {
    entry$summary <- state_summary_from_grid(entry$grid, domain)
    key <- decomp_state_key(prefix, start.y, end.y, domain)
    assign(key, entry, envir = cache$states)
  }
  entry$summary
}

# 打印驱动因素合计；提供 state 问题时，再打印一份简洁报告，说明 NA 单元、
# 受影响的 domain 与丢格计数（I7）。
# issues：命名 list，每个 factor state 含 n_grid、n_na 与 na_domains。
print_decomp_drivers <- function(
  Decomp,
  steps,
  start.y,
  end.y,
  issues = NULL,
  domain = NULL,
  grid_total = NULL,
  report_only = FALSE
) {
  if (!report_only) {
    cat(str_c("Drivers Between ", start.y, " and ", end.y, ":\n"))
    for (step_name in steps) {
      cat(
        str_c(
          step_name,
          ":\t",
          sum(Decomp[[step_name]] |> sum() |> round())
        ),
        "\n"
      )
    }
  }
  if (is.null(issues)) {
    return(invisible(NULL))
  }

  labels <- names(issues)
  n_grid <- map_int(issues, ~ as.integer(.x$n_grid %||% NA_integer_))
  n_na <- map_int(issues, "n_na")
  na_domains <- sort(unique(unlist(map(issues, "na_domains"), use.names = FALSE)))
  na_states <- labels[n_na > 0]

  cat(str_c(
    "Decomposition state report (", start.y, " -> ", end.y,
    if (is.null(domain)) "" else str_c(", ", domain),
    "):\n"
  ))

  if (all(is.na(n_grid))) {
    coverage <- "grid counts unavailable"
    dropped <- "unavailable"
  } else {
    widest <- max(n_grid, na.rm = TRUE)
    short <- !is.na(n_grid) & n_grid < widest
    dropped <- if (any(short)) {
      str_c(
        labels[short],
        " (",
        format(n_grid[short], big.mark = ","),
        ")",
        collapse = ", "
      )
    } else {
      "none"
    }
    coverage <- str_c(
      str_c(
        format(sort(unique(n_grid[!is.na(n_grid)])), big.mark = ","),
        collapse = "/"
      ),
      " grid(s) per state",
      if (is.null(grid_total)) {
        ""
      } else {
        str_c(" of ", format(grid_total, big.mark = ","), " in Grid_info")
      }
    )
  }
  cat(str_c(
    "  coverage: ", coverage,
    "; dropped vs widest state: ", dropped,
    ".\n"
  ))

  na_detail <- if (length(na_states) == 0) {
    "none"
  } else {
    str_c(
      na_states,
      " (",
      format(n_na[match(na_states, labels)], big.mark = ","),
      " cell(s))",
      collapse = ", "
    )
  }
  domains_detail <- if (length(na_domains) == 0) {
    "none"
  } else {
    shown <- head(na_domains, 6)
    str_c(
      str_c(shown, collapse = ", "),
      if (length(na_domains) > length(shown)) {
        str_c(" (+", length(na_domains) - length(shown), " more)")
      } else {
        ""
      }
    )
  }
  cat(str_c(
    "  states with NA: ", na_detail,
    "; affected domains: ", domains_detail,
    ".\n"
  ))
  invisible(NULL)
}

Decomposition <- function(serie, start.y, end.y, domain = NULL, cache = NULL) {
  validate_scenario_columns(start.y, end.y)
  if (is.null(domain)) {
    domain <- detect_domain()
  }

  prefixes <- decomp_series_prefixes(serie)
  steps <- prefixes[[5]]

  if (is.null(cache)) {
    cache <- new_decomp_cache()
  }

  # 有计划启用时并发构建缺失的 state；否则不做任何事。
  decomp_fill_cache(prefixes, start.y, end.y, domain, cache)

  grids <- prefixes |>
    map(
      get_decomp_state,
      start.y = start.y,
      end.y = end.y,
      domain = domain,
      cache = cache
    ) |>
    map("grid")

  # C1：丢失网格的 state（NA 浓度、人口、死亡率或 PWRR）会让每个差值成为
  # 不同网格集上的求和。
  # 做差分之前中止，避免静默返回有偏的 domain 合计。
  # 保留为内联代码：模块 harness 解析一份固定的顶层名字清单，
  # 在这里新增辅助函数，它们看不到。
  state_labels <- c("<start>", steps)
  key_sets <- map(grids, function(grid) str_c(grid$x, grid$y, sep = "|"))
  n_keys <- map_int(key_sets, length)
  same_keys <- all(map_lgl(
    key_sets,
    function(keys) setequal(keys, key_sets[[1]])
  ))
  if (length(unique(n_keys)) > 1 || !same_keys) {
    stop(
      "Decomposition aborted: the factor states cover different (x, y) grid sets (",
      str_c(state_labels, " = ", n_keys, " grids", collapse = ", "),
      "). Differencing them would sum over different grids and bias every ",
      "domain total. Mortality() drops grids whose concentration, population, ",
      "mortality rate, or PWRR input is NA; inspect the scenario columns each ",
      "state reads."
    )
  }
  state_issues <- map(grids, function(grid) {
    mort_cols <- setdiff(names(grid), c("x", "y", domain))
    if (length(mort_cols) == 0) {
      return(list(
        n_grid = nrow(grid),
        n_na = 0L,
        na_domains = character(0)
      ))
    }
    na_cells <- is.na(as.matrix(grid[mort_cols]))
    list(
      n_grid = nrow(grid),
      n_na = sum(na_cells),
      na_domains = unique(grid[[domain]][rowSums(na_cells) > 0])
    )
  }) |>
    set_names(state_labels)

  Decomp <- grids |> set_names(str_c("Mort_", 0:4))

  Decomp <- Decomp |>
    imap_dfr(
      ~ .x |>
        pivot_longer(
          cols = -all_of(c("x", "y", domain)),
          names_to = "Cause_Age",
          values_to = "Mort"
        ) |>
        mutate(Step = .y)
    ) |>
    pivot_wider(names_from = "Step", values_from = "Mort") |>
    mutate(
      Start = Mort_0,
      !!steps[1] := Mort_1 - Mort_0,
      !!steps[2] := Mort_2 - Mort_1,
      !!steps[3] := Mort_3 - Mort_2,
      !!steps[4] := Mort_4 - Mort_3,
      End = Mort_4,
      .keep = "unused"
    )

  print_decomp_drivers(
    Decomp, steps, start.y, end.y,
    issues = state_issues,
    domain = domain,
    grid_total = nrow(Grid_info)
  )

  return(Decomp)
}

# 由缓存的 state 摘要构建国家级序列贡献。
# Decomposition() 保留网格级输出；aggregate_drivers() 使用这条路径。
# 提供 state_summaries 时，它替代 cache：必须以 decomp_state_key() 为键，
# 每个 state 含一张 state_summary_from_grid() 表。
# check_keys = FALSE 跳过 C1 网格集守卫；
# aggregate_drivers() 对其 16 份唯一摘要运行一次，并对全部 24 条序列传 FALSE。
Decomposition_summary <- function(
  serie,
  start.y,
  end.y,
  domain = NULL,
  cache = NULL,
  state_summaries = NULL,
  report = TRUE,
  check_keys = TRUE
) {
  validate_scenario_columns(start.y, end.y)
  if (is.null(domain)) {
    domain <- detect_domain()
  }

  prefixes <- decomp_series_prefixes(serie)
  steps <- prefixes[[5]]

  if (is.null(cache)) {
    cache <- new_decomp_cache()
  }

  if (is.null(state_summaries)) {
    # 有计划启用时并发构建缺失的 state；否则不做任何事。
    decomp_fill_cache(prefixes, start.y, end.y, domain, cache)
    summaries <- prefixes |>
      map(
        decomp_state_summary,
        start.y = start.y,
        end.y = end.y,
        domain = domain,
        cache = cache
      )
  } else {
    keys <- prefixes |>
      map_chr(
        decomp_state_key,
        start.y = start.y,
        end.y = end.y,
        domain = domain
      )
    missing_keys <- setdiff(keys, names(state_summaries))
    if (length(missing_keys) > 0) {
      stop(
        "state_summaries lacks ",
        length(missing_keys),
        " of ",
        length(keys),
        " state(s) needed by serie ",
        serie,
        "."
      )
    }
    summaries <- state_summaries[keys]
  }

  # C1：来自 state_summary_from_grid() 的摘要携带该 state 的 (x, y) 键集合。
  # 对不同网格上构建的 state 做差分会污染每个 domain 合计，因此相减之前
  # 中止。
  # 别处构建的摘要无法检查；此时告警，不假装守卫已运行。
  state_labels <- c("<start>", steps)
  if (check_keys) {
    decomp_guard_summary_grids(summaries, state_labels, "Decomposition_summary")
  }

  state_issues <- NULL
  if (report) {
    state_issues <- map(summaries, function(summary) {
      is_na <- is.na(summary$Mort)
      list(
        n_grid = attr(summary, "decomp_n_grid", exact = TRUE),
        n_na = sum(is_na),
        na_domains = unique(summary[[domain]][is_na])
      )
    }) |>
      set_names(state_labels)
  }

  Decomp <- summaries |> set_names(str_c("Mort_", 0:4))

  Decomp <- Decomp |>
    imap_dfr(
      ~ .x |>
        mutate(Step = .y)
    ) |>
    pivot_wider(names_from = "Step", values_from = "Mort") |>
    mutate(
      Start = Mort_0,
      !!steps[1] := Mort_1 - Mort_0,
      !!steps[2] := Mort_2 - Mort_1,
      !!steps[3] := Mort_3 - Mort_2,
      !!steps[4] := Mort_4 - Mort_3,
      End = Mort_4,
      .keep = "unused"
    )

  print_decomp_drivers(
    Decomp, steps, start.y, end.y,
    issues = state_issues,
    domain = domain,
    grid_total = nrow(Grid_info)
  )

  Decomp
}

aggregate_drivers <- function(
  RUN24 = TRUE,
  start.year = 'base2015',
  end.year = 'SSP1-Baseline_2030',
  domain = NULL
) {
  validate_scenario_columns(start.year, end.year)
  if (is.null(domain)) {
    domain <- detect_domain()
  }

  n <- if (RUN24) 1:24 else 1

  # 唯一 factor state 只汇总一次，并且只保留摘要（I9，261101）。旧的串行
  # 路径缓存了 16 张完整网格表（约 117 MB），实际只消费摘要；这里与并发
  # 路径保持一致。
  states <- decomp_unique_prefixes(
    start.year, end.year, domain,
    RUN24 = RUN24
  )
  kernel_on <- exists("kernel_active", mode = "function") &&
    isTRUE(kernel_active())
  cache <- new_decomp_cache()
  engine_on <- isTRUE(cache$engine)
  log_msg(
    INFO,
    if (engine_on) "" else if (kernel_on) "kernel: " else "",
    "summarising ",
    length(states),
    " unique factor state(s) for ",
    length(n),
    " sequence(s)",
    if (engine_on) {
      " on the serial input cache"
    } else if (kernel_on) {
      str_c(" on ", kernel_workers(), " worker(s)")
    } else {
      ""
    },
    "."
  )
  state_summaries <- precompute_state_summaries(
    states, start.year, end.year, domain, cache = cache
  )

  # C1 每次运行一次：在这里检查 16 份唯一摘要，然后让下面每次
  # Decomposition_summary() 调用跳过守卫（check_keys = FALSE）。
  # 放在序列内部会让同一个键集合比较重复 24 次。
  state_labels <- str_remove(names(state_summaries), ".*::")
  decomp_guard_summary_grids(
    state_summaries,
    state_labels,
    "aggregate_drivers"
  )

  # 整个运行只报告一次 NA 与丢格情况（I7）；下面逐序列的
  # Decomposition_summary() 调用使用 report = FALSE。
  state_issues <- map(state_summaries, function(summary) {
    is_na <- is.na(summary$Mort)
    list(
      n_grid = attr(summary, "decomp_n_grid", exact = TRUE),
      n_na = sum(is_na),
      na_domains = unique(summary[[domain]][is_na])
    )
  }) |>
    set_names(state_labels)
  print_decomp_drivers(
    Decomp = NULL,
    steps = character(0),
    start.y = start.year,
    end.y = end.year,
    issues = state_issues,
    domain = domain,
    grid_total = nrow(Grid_info),
    report_only = TRUE
  )

  ls1 <- n |>
    set_names() |>
    map(
      ~ Decomposition_summary(
        serie = .x,
        start.y = start.year,
        end.y = end.year,
        domain = domain,
        state_summaries = state_summaries,
        report = FALSE,
        check_keys = FALSE
      ) |>
        group_by(across(all_of(domain))) |>
        summarise(across(Start:End, sum))
    )

  ls1[["Summary"]] <- ls1 |>
    bind_rows() |>
    group_by(across(all_of(domain))) |>
    summarise(across(Start:End, mean))

  ls1[["Sequences"]] <- decomp_sequences

  # C2：Results/ 被 git 忽略，干净的检出中没有这个目录。
  dir.create("./Results", showWarnings = FALSE, recursive = TRUE)
  ls1 |>
    write_xlsx(str_glue(
      "./Results/{tell_Model()}_DrivingFactors_",
      "{start.year}-{end.year}_Build{format(Sys.Date(), '%y%m%d')}.xlsx"
    ))
}
