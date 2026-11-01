# 不确定性模块 —— 域级 PWE 的 PAF 敏感性路径
#   - Uncertainty() —— 在域级人口加权暴露 (PWE) 处查 RR，按 MEAN/UP/LOW
#     分支计算 PAF = 1 - 1/RR，给出 d_UP / d_LOW；可选叠加 ±Conc_ERR%
#     浓度扰动，按平方和开根号合成。
#   口径：
#     本函数是二级敏感性路径。主要不确定性输出来自 aggregate_sigma()：
#     它用格级 Mortality() 的 C-R 分支和同一 PWRR 公式给出界。
#     PAF(PWE) 是 GBD 负担权重 PAF（即 PWRR 估计量）的近似。只有 RR 对
#     浓度仿射，或域内暴露均匀时，两者相等；其他情形存在偏差。
#   修改 261102：按评审意见恢复并修正 PWE 路径：
#     1) PWE 先裁到 RR 查询范围，越界值不再静默落成 0；
#     2) 浓度精度取 RR 表属性 dgt_conc（回退 .DGT_CONC），不再写死 1；
#     3) 弃用 str_glue()/pivot_wider 的字符串键，domain 全程按列处理；
#     4) 弃用 na.omit()；缺失域保留 NA 行并告警；
#     5) 输出含中心估计 PAF 与 n_grid，按 domain × endpoint × agegroup
#        一行，不跨 endpoint 或 agegroup 聚合；
#     6) 组只保留 RR 表覆盖的 endpoint/agegroup，未覆盖的组告警后排除
#        （与 Mortality() 的 usable_groups 一致）；
#     7) 诊断只在 verbose = TRUE 时打印。

# ── 内部辅助 ──────────────────────────────────────────────────────────────────

# 与 mortality.R 的 mortality_clamp_text() 同格式；PWE 是域级标量，
# 计数单位换成 domain(s)，其余文本逐字一致。
uncertainty_clamp_text <- function(n_lo, n_hi, rr_range, label,
                                   unit = "grids") {
  if (n_lo + n_hi == 0) {
    return(character())
  }
  str_c(
    n_lo + n_hi,
    " ",
    unit,
    " in ",
    label,
    " have concentration outside RR lookup range [",
    rr_range[[1]],
    ", ",
    rr_range[[2]],
    "]. ",
    "Clamped to nearest boundary (",
    n_lo,
    " low, ",
    n_hi,
    " high)."
  )
}

# 与 mortality.R 的 mortality_clamp_report() 同契约：返回文本、截断后的
# 值，以及低/高越界计数。n_lo + n_hi 为 0 时原样返回值。
uncertainty_clamp_report <- function(conc_vec, rr_range, label,
                                     unit = "grids") {
  n_lo <- sum(conc_vec < rr_range[[1]], na.rm = TRUE)
  n_hi <- sum(conc_vec > rr_range[[2]], na.rm = TRUE)
  text <- uncertainty_clamp_text(n_lo, n_hi, rr_range, label, unit)
  if (length(text) > 0) {
    conc_vec <- pmin(pmax(conc_vec, rr_range[[1]]), rr_range[[2]])
  }
  list(text = text, value = conc_vec, n_lo = n_lo, n_hi = n_hi)
}

# 截断长列表，用于告警与报错文本。
uncertainty_shorten <- function(x, n = 6) {
  x <- as.character(x)
  str_c(str_c(head(x, n), collapse = ", "), if (length(x) > n) " ..." else "")
}

# 必需列守卫：缺列时中止，并列出实际列名。
uncertainty_require_columns <- function(df, columns, label) {
  if (!is.data.frame(df)) {
    cli::cli_abort("{label} must be a data frame, got {.cls {class(df)}}.")
  }
  missing <- setdiff(columns, names(df))
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "x" = "{label} is missing required column(s): {.val {missing}}.",
      "i" = "Found: {.val {names(df)}}."
    ))
  }
  invisible(TRUE)
}

# 唯一键守卫：重复键会让 left_join() 成倍复制行。
uncertainty_assert_unique <- function(df, columns, label) {
  dup <- duplicated(df[columns])
  if (any(dup)) {
    cli::cli_abort(
      "{label} has {sum(dup)} duplicated {.val {columns}} key row(s); ",
      "a join would multiply rows."
    )
  }
  invisible(TRUE)
}

# 数值化：先转字符再转数值，避免 factor 被当成水平编号。
# coercion 警告由调用方的坏值检查显式覆盖，因此这里静音。
uncertainty_numeric <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

# 年龄档键：与 Mortality() 一致，先数值化再按 0 位小数取字符串。
uncertainty_agegroup <- function(x) {
  num <- uncertainty_numeric(x)
  out <- matchable(num, 0)
  out[is.na(num)] <- NA_character_
  out
}

# 在 PWE 向量上查 RR，返回三个分支的 PAF 矩阵（行 = 域，列 = 组）。
#   pwe           已按域聚合的暴露（可含 NA）
#   rr            已规范化的 RR 表，含 MEAN/UP/LOW
#   groups        endpoint × agegroup
#   dgt_conc      浓度精度
#   rr_range      RR 表的浓度范围
#   clamp_report  clamp 函数（mortality.R 的实现或本地回退）
#   lookup_fn     mort_rr_matrix()
# bad 矩阵标记“该域该组没有可用 RR 行”：NA、Inf 或 0 以下 RR 都算。
uncertainty_paf_matrix <- function(pwe, rr, groups, dgt_conc, rr_range,
                                   clamp_report, lookup_fn) {
  clamped <- clamp_report(pwe, rr_range, "PWE")
  conc_strings <- rep(NA_character_, length(pwe))
  keep <- !is.na(clamped$value)
  conc_strings[keep] <- matchable(clamped$value[keep], dgt_conc)
  branch_tbl <- function(branch) {
    rr[rr$CI == branch, c("concentration", "endpoint", "agegroup", "RR")]
  }
  branch_names <- c("MEAN", "UP", "LOW")
  branch_matrix <- function(branch) {
    lookup_fn(conc_strings, branch_tbl(branch), groups)
  }
  rr_mats <- lapply(branch_names, branch_matrix)
  names(rr_mats) <- branch_names
  paf <- lapply(rr_mats, function(rr_mat) 1 - 1 / rr_mat)
  # NA、Inf 或 0 以下的 RR 都算“没有可用 RR 行”。
  flag_bad_rr <- function(rr_mat) {
    (is.na(rr_mat) | !is.finite(rr_mat) | rr_mat <= 0) & keep
  }
  bad <- lapply(rr_mats, flag_bad_rr)
  list(paf = paf, clamped = clamped, bad = bad)
}

# 域级 PWE 的 PAF 敏感性
#   Grids     网格表，含 x、y 与 domain 指定的域列
#   Conc_real x、y、concentration
#   Pop       x、y、Pop
#   age_struc domain、agegroup、AgeStruc
#   m_rate    domain、endpoint、agegroup、MortRate
#   domain    域列名，默认 "Country"
#   includeConc 是否叠加 ±Conc_ERR% 浓度扰动，默认 FALSE
#   Conc_ERR  浓度扰动百分比，取值 [0, 100]，默认 12
#   verbose   是否打印 PWE 诊断，默认 FALSE
# 返回 tibble：domain、endpoint、agegroup、PWE、PAF、d_UP、d_LOW、n_grid。
# 组取 m_rate × age_struc 与 RR 表覆盖的交集；PWE 输出裁到 RR 范围后的值。
# 口径：PAF = 1 - 1/RR；d_UP = PAF_UP - PAF_MEAN，d_LOW = PAF_MEAN - PAF_LOW；
#   includeConc 时两项再与浓度扰动的偏差按平方和开根号合成。
Uncertainty <- function(
  Grids, Conc_real, Pop, age_struc, m_rate,
  domain = "Country",
  includeConc = FALSE, Conc_ERR = 12, verbose = FALSE
) {
  # ---- 参数校验 --------------------------------------------------------
  domain_ok <- is.character(domain) && length(domain) == 1 &&
    !is.na(domain) && nzchar(domain)
  if (!domain_ok) {
    cli::cli_abort(
      "domain must be one non-empty column name, got {.val {domain}}."
    )
  }
  if (!is.logical(includeConc) || length(includeConc) != 1 || is.na(includeConc)) {
    cli::cli_abort(
      "includeConc must be one TRUE/FALSE value, got {.val {includeConc}}."
    )
  }
  conc_err_ok <- is.numeric(Conc_ERR) && length(Conc_ERR) == 1 &&
    !is.na(Conc_ERR) && is.finite(Conc_ERR) &&
    Conc_ERR >= 0 && Conc_ERR <= 100
  if (!conc_err_ok) {
    cli::cli_abort(
      "Conc_ERR must be one number between 0 and 100 (percent), ",
      "got {.val {Conc_ERR}}."
    )
  }
  if (!is.logical(verbose) || length(verbose) != 1 || is.na(verbose)) {
    cli::cli_abort("verbose must be one TRUE/FALSE value, got {.val {verbose}}.")
  }

  uncertainty_require_columns(Grids, c("x", "y", domain), "Grids")
  uncertainty_require_columns(
    Conc_real, c("x", "y", "concentration"), "Conc_real"
  )
  uncertainty_require_columns(Pop, c("x", "y", "Pop"), "Pop")
  uncertainty_require_columns(
    age_struc, c("domain", "agegroup", "AgeStruc"), "age_struc"
  )
  uncertainty_require_columns(
    m_rate, c("domain", "endpoint", "agegroup", "MortRate"), "m_rate"
  )

  uncertainty_assert_unique(Grids, c("x", "y"), "Grids")
  uncertainty_assert_unique(Conc_real, c("x", "y"), "Conc_real")
  uncertainty_assert_unique(Pop, c("x", "y"), "Pop")

  # ---- RR 表与浓度精度 ------------------------------------------------
  rr_raw <- get0(".RR_std_tbl", envir = globalenv(), ifnotfound = NULL)
  if (is.null(rr_raw) || nrow(rr_raw) == 0) {
    cli::cli_abort(c(
      "x" = "RR lookup table .RR_std_tbl is absent or empty.",
      "i" = "Run set_Model() before Uncertainty()."
    ))
  }
  uncertainty_require_columns(
    rr_raw,
    c("concentration", "endpoint", "agegroup", "CI", "RR"),
    "RR lookup table"
  )
  branches <- unique(as.character(rr_raw$CI))
  if (!setequal(branches, c("MEAN", "UP", "LOW"))) {
    cli::cli_abort(
      "RR lookup table must contain exactly MEAN, UP, and LOW branches; ",
      "found {.val {branches}}."
    )
  }

  dgt_conc <- attr(rr_raw, "dgt_conc", exact = TRUE)
  if (is.null(dgt_conc)) {
    dgt_conc <- get0(".DGT_CONC", envir = globalenv(), ifnotfound = 1)
  }
  if (is.null(dgt_conc)) {
    dgt_conc <- 1
  }
  dgt_ok <- is.numeric(dgt_conc) && length(dgt_conc) == 1 &&
    !is.na(dgt_conc) && is.finite(dgt_conc) && dgt_conc >= 0 &&
    dgt_conc == round(dgt_conc)
  if (!dgt_ok) {
    cli::cli_abort(
      "dgt_conc must be one non-negative integer, got {.val {dgt_conc}}."
    )
  }
  dgt_conc <- as.integer(dgt_conc)

  # ---- 取调用方可见的实现 --------------------------------------------
  caller <- parent.frame()
  # PWE 每个域一个值，clamp 计数按 domain(s) 报；文本其余部分与
  # mortality.R 的 clamp 报告一致。
  clamp_report <- function(conc_vec, rr_range, label) {
    uncertainty_clamp_report(conc_vec, rr_range, label, unit = "domain(s)")
  }
  lookup_fn <- get0(
    "mort_rr_matrix",
    envir = caller,
    mode = "function",
    inherits = TRUE,
    ifnotfound = NULL
  )
  if (is.null(lookup_fn)) {
    cli::cli_abort(c(
      "x" = "mort_rr_matrix() is not available in the caller's scope.",
      "i" = "Source Code/mortality.R before calling Uncertainty()."
    ))
  }

  # ---- 规范化 RR 键（与 Mortality() 的路径一致） ----------------------
  rr_conc_num <- suppressWarnings(as.numeric(rr_raw$concentration))
  bad_rr_conc <- is.na(rr_conc_num) & !is.na(rr_raw$concentration)
  if (any(bad_rr_conc)) {
    cli::cli_abort(
      "RR lookup table has {sum(bad_rr_conc)} non-numeric concentration ",
      "value(s), e.g. {.val {uncertainty_shorten(unique(rr_raw$concentration[bad_rr_conc]))}}."
    )
  }
  if (isTRUE(attr(rr_raw, "rr_normalized", exact = TRUE))) {
    rr <- rr_raw |>
      mutate(
        concentration = as.character(concentration),
        endpoint = as.character(endpoint),
        agegroup = as.character(agegroup)
      )
  } else {
    rr <- rr_raw |>
      mutate(
        concentration = matchable(rr_conc_num, dgt_conc),
        endpoint = tolower(as.character(endpoint)),
        agegroup = uncertainty_agegroup(agegroup)
      )
  }
  rr_key_missing <- anyNA(rr$concentration) || anyNA(rr$endpoint) ||
    anyNA(rr$agegroup) || anyNA(rr$RR)
  if (rr_key_missing) {
    cli::cli_abort("RR lookup table contains missing keys or RR values.")
  }
  if (anyDuplicated(rr[c("concentration", "endpoint", "agegroup", "CI")])) {
    cli::cli_abort(
      "RR lookup table has duplicated concentration/endpoint/agegroup/CI ",
      "key(s)."
    )
  }
  rr_range <- range(rr_conc_num, na.rm = TRUE)
  if (!all(is.finite(rr_range))) {
    cli::cli_abort("RR lookup table has no finite concentration values.")
  }

  # ---- 分组：endpoint × agegroup --------------------------------------
  # 先取 m_rate 与 age_struc 的共同覆盖，再限制到 RR 表实际覆盖的组。
  # 与 Mortality() 的 usable_groups 一致：RR 未覆盖的组不输出，但要告警。
  ag <- age_struc |>
    transmute(
      domain = as.character(domain),
      agegroup = uncertainty_agegroup(agegroup)
    )
  if (anyNA(ag$agegroup)) {
    cli::cli_abort(
      "age_struc has {sum(is.na(ag$agegroup))} non-numeric or missing agegroup value(s)."
    )
  }
  mr <- m_rate |>
    transmute(
      domain = as.character(domain),
      endpoint = tolower(as.character(endpoint)),
      agegroup = uncertainty_agegroup(agegroup)
    )
  if (anyNA(mr$endpoint) || anyNA(mr$agegroup)) {
    cli::cli_abort(
      "m_rate has missing endpoint or non-numeric agegroup values."
    )
  }
  candidate_groups <- mr |>
    distinct(endpoint, agegroup) |>
    semi_join(ag |> distinct(agegroup), by = "agegroup")
  if (nrow(candidate_groups) == 0) {
    cli::cli_abort(
      "No endpoint × agegroup group is covered by both m_rate and age_struc."
    )
  }
  rr_groups <- rr |> distinct(endpoint, agegroup)
  groups <- candidate_groups |>
    semi_join(rr_groups, by = c("endpoint", "agegroup"))
  uncovered_groups <- candidate_groups |>
    anti_join(rr_groups, by = c("endpoint", "agegroup"))
  if (nrow(uncovered_groups) > 0) {
    uncovered_text <- uncovered_groups |>
      mutate(label = str_c(endpoint, agegroup, sep = "/")) |>
      pull(label) |>
      uncertainty_shorten()
    log_msg(
      WARN,
      "{nrow(uncovered_groups)} endpoint/agegroup group(s) in m_rate × ",
      "age_struc are absent from the RR table and are excluded: ",
      "{uncovered_text}."
    )
  }
  if (nrow(groups) == 0) {
    cli::cli_abort(
      "No endpoint × agegroup group is jointly covered by the RR table, ",
      "m_rate, and age_struc."
    )
  }

  # ---- 网格表：Conc_real → Pop → Grids --------------------------------
  grid_tbl <- Grids |>
    transmute(
      x = as.character(x),
      y = as.character(y),
      domain = as.character(.data[[domain]])
    )
  conc_tbl <- Conc_real |>
    transmute(
      x = as.character(x),
      y = as.character(y),
      concentration = uncertainty_numeric(concentration)
    )
  bad_conc <- is.na(conc_tbl$concentration) & !is.na(Conc_real$concentration)
  if (any(bad_conc)) {
    cli::cli_abort(
      "Conc_real has {sum(bad_conc)} non-numeric concentration value(s), ",
      "e.g. {.val {uncertainty_shorten(unique(Conc_real$concentration[bad_conc]))}}."
    )
  }
  conc_bad_finite <- !is.finite(conc_tbl$concentration) &
    !is.na(conc_tbl$concentration)
  if (any(conc_bad_finite)) {
    cli::cli_abort("Conc_real has non-finite concentration value(s).")
  }
  pop_tbl <- Pop |>
    transmute(
      x = as.character(x),
      y = as.character(y),
      Pop = uncertainty_numeric(Pop)
    )
  bad_pop <- is.na(pop_tbl$Pop) & !is.na(Pop$Pop)
  if (any(bad_pop)) {
    cli::cli_abort(
      "Pop has {sum(bad_pop)} non-numeric value(s), ",
      "e.g. {.val {uncertainty_shorten(unique(Pop$Pop[bad_pop]))}}."
    )
  }
  if (any(!is.finite(pop_tbl$Pop) & !is.na(pop_tbl$Pop))) {
    cli::cli_abort("Pop has non-finite value(s).")
  }
  if (any(pop_tbl$Pop < 0, na.rm = TRUE)) {
    cli::cli_abort("Pop contains negative values.")
  }

  frame <- conc_tbl |>
    left_join(pop_tbl, by = c("x", "y")) |>
    left_join(grid_tbl, by = c("x", "y"))

  # ---- 显式告警后丢弃无效行，不静默丢域 ------------------------------
  missing_conc <- grid_tbl |>
    distinct(x, y) |>
    anti_join(conc_tbl |> distinct(x, y), by = c("x", "y"))
  if (nrow(missing_conc) > 0) {
    log_msg(
      WARN,
      "{nrow(missing_conc)} grid(s) in Grids have no concentration record ",
      "and are excluded from PWE."
    )
  }

  n_na_domain <- sum(is.na(frame$domain))
  if (n_na_domain > 0) {
    log_msg(
      WARN,
      "{n_na_domain} grid(s) in Conc_real have no matching (x, y) domain in ",
      "Grids (or an NA domain value) and are dropped."
    )
  }

  na_conc_rows <- frame |> filter(!is.na(domain), is.na(concentration))
  if (nrow(na_conc_rows) > 0) {
    conc_na_by_domain <- na_conc_rows |>
      count(domain, name = "n_grid") |>
      arrange(desc(n_grid)) |>
      mutate(label = str_c(domain, " (", n_grid, ")")) |>
      pull(label)
    log_msg(
      WARN,
      "{nrow(na_conc_rows)} grid(s) in {length(conc_na_by_domain)} domain(s) ",
      "have NA concentration and are dropped: ",
      "{uncertainty_shorten(conc_na_by_domain)}."
    )
  }

  frame <- frame |> filter(!is.na(domain), !is.na(concentration))

  n_bad_pop <- sum(is.na(frame$Pop) | frame$Pop <= 0)
  if (n_bad_pop > 0) {
    log_msg(
      WARN,
      "{n_bad_pop} grid(s) have NA or non-positive Pop and are excluded from PWE."
    )
  }

  # ---- PWE 按域计算 ---------------------------------------------------
  pwe_summary <- frame |>
    filter(!is.na(Pop), Pop > 0) |>
    group_by(domain) |>
    summarise(
      PWE = sum(Pop * concentration) / sum(Pop),
      n_grid = n(),
      .groups = "drop"
    )

  # 域清单来自 Grids，因此完全没有可用网格的域仍保留一行。
  domain_names <- grid_tbl$domain
  domain_names <- unique(domain_names[!is.na(domain_names)])
  if (length(domain_names) == 0) {
    cli::cli_abort(
      "Grids has no non-NA value in the domain column {.val {domain}}."
    )
  }

  pwe_tbl <- tibble(domain = domain_names) |>
    left_join(pwe_summary, by = "domain") |>
    mutate(n_grid = coalesce(n_grid, 0L))

  na_pwe_domains <- pwe_tbl$domain[is.na(pwe_tbl$PWE)]
  if (length(na_pwe_domains) > 0) {
    log_msg(
      WARN,
      "Domain(s) ({length(na_pwe_domains)}) have no grid with non-NA ",
      "concentration and positive population; PWE and PAF are NA ",
      "(n_grid = 0): {uncertainty_shorten(na_pwe_domains)}."
    )
  }

  n_dom <- nrow(pwe_tbl)
  n_group <- nrow(groups)
  log_msg(
    INFO,
    "Uncertainty(): {n_dom} domain(s), {n_group} endpoint/agegroup ",
    "group(s), includeConc = {includeConc}."
  )

  # ---- 基线的 RR 查询与 PAF -------------------------------------------
  util <- uncertainty_paf_matrix(
    pwe_tbl$PWE, rr, groups, dgt_conc, rr_range, clamp_report, lookup_fn
  )
  if (length(util$clamped$text) > 0) {
    log_msg(WARN, "{util$clamped$text}")
  }
  if (verbose) {
    log_msg(INFO, "PWE by domain (PWE_used = clamped to the RR range):")
    print(
      pwe_tbl |>
        mutate(PWE_used = util$clamped$value) |>
        select(domain, PWE, PWE_used, n_grid),
      n = Inf
    )
  }

  report_bad_lookup <- function(found, context) {
    bad_mat <- found$bad$MEAN | found$bad$UP | found$bad$LOW
    if (!any(bad_mat)) {
      return(invisible(NULL))
    }
    bad_rows <- which(rowSums(bad_mat) > 0)
    bad_cols <- which(colSums(bad_mat) > 0)
    domain_text <- uncertainty_shorten(pwe_tbl$domain[bad_rows])
    group_text <- uncertainty_shorten(
      str_c(groups$endpoint[bad_cols], "/", groups$agegroup[bad_cols])
    )
    cli::cli_abort(c(
      "x" = str_c(
        "{sum(bad_mat)} domain/endpoint/agegroup combination(s) ",
        "have no usable RR row {context}."
      ),
      "i" = "Domain(s) ({length(bad_rows)}): {domain_text}.",
      "i" = "Endpoint/agegroup ({length(bad_cols)}): {group_text}.",
      "i" = "Check RR table coverage and dgt_conc = {dgt_conc}."
    ))
  }
  report_bad_lookup(util, "after clamping PWE to the RR concentration range")

  paf <- util$paf
  d_up <- paf$UP - paf$MEAN
  d_low <- paf$MEAN - paf$LOW

  # ---- 可选：±Conc_ERR% 浓度扰动，按平方和开根号合成 ------------------
  # 扰动作用在裁到 RR 范围后的 PWE 上；这样输出的 PWE 与扰动基准一致。
  if (includeConc && Conc_ERR > 0) {
    lookup_perturbed <- function(factor) {
      uncertainty_paf_matrix(
        util$clamped$value * factor, rr, groups, dgt_conc, rr_range,
        clamp_report, lookup_fn
      )
    }
    perturbed <- lapply(
      c(UP = 1 + Conc_ERR / 100, LOW = 1 - Conc_ERR / 100),
      lookup_perturbed
    )
    report_bad_lookup(
      perturbed$UP,
      "after the concentration UP perturbation"
    )
    report_bad_lookup(
      perturbed$LOW,
      "after the concentration LOW perturbation"
    )
    if (verbose) {
      if (length(perturbed$UP$clamped$text) > 0) {
        log_msg(INFO, "includeConc UP: {perturbed$UP$clamped$text}")
      }
      if (length(perturbed$LOW$clamped$text) > 0) {
        log_msg(INFO, "includeConc LOW: {perturbed$LOW$clamped$text}")
      }
    }
    d_up <- sqrt(d_up^2 + (perturbed$UP$paf$MEAN - paf$MEAN)^2)
    d_low <- sqrt(d_low^2 + (paf$MEAN - perturbed$LOW$paf$MEAN)^2)
  }

  # ---- 输出：每域 × 每 endpoint × 每 agegroup 一行 --------------------
  # PWE 取裁到 RR 范围后的值，使 PAF 与输出的 PWE 自洽。
  out <- tibble(
    domain = rep(pwe_tbl$domain, times = n_group),
    endpoint = rep(groups$endpoint, each = n_dom),
    agegroup = rep(groups$agegroup, each = n_dom),
    PWE = rep(util$clamped$value, times = n_group),
    PAF = as.vector(paf$MEAN),
    d_UP = as.vector(d_up),
    d_LOW = as.vector(d_low),
    n_grid = rep(pwe_tbl$n_grid, times = n_group)
  ) |>
    arrange(domain, endpoint, agegroup)

  out
}
