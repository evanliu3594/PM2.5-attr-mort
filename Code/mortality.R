# 死亡计算模块 —— PWRR 归一化的格级归因死亡
#   - Mortality()      —— 公式：Mort = Pop × AgeStruc × MortRate × (RR−1) / PWRR_{domain,endpoint,age}
#   - Mortality_at()   —— 指定年份与情景的便捷封装
#   - detect_domain()  —— 由 MortRate × Grid_info 的重叠自动识别 PWRR 域
#   - join_report()    —— left_join，另附可选的分层 NA 诊断报告
#   修改 260618：三阶段 PWRR 守卫（Inf/<1→中止，NA→跳过，全部丢弃→中止），
#     新增 verbose/debug 参数，trunc_fmt 接受 n=Inf 以输出完整列表。
#   修改 261026：PWRR 按域 × 终点 × 年龄档计算；RANGE 各分支使用
#     对应的 PWRR；NA PWRR 只丢弃受影响的行，不丢弃整个域。
#   修改 261026：Mortality() 按终点 × 年龄档分组遍历；每组只做一次
#     格级 join，避免生成网格 × 终点 × 年龄档的长表。
#     compute_group() 无副作用，可直接交给 future_map2()。
#   修改 261026：pwr_branch 配置（"branch" 或 "mean"）决定
#     RANGE 使用分支各自的 PWRR 还是共用的 MEAN PWRR。
#   修改 261026b：单分支复用 RANGE 的分母；只检查
#     必需列；增加键守卫；检查单位；缺失的分组保持 NA。
#   修改 261027：复用 RR_std() 的键归一化（rr_normalized 标记）；
#     仅当 Conc_c 与 Conc_r 不同时才检查 Conc_c。
#   修改 261101：join_report() 按 join 后的数据帧解析
#     分解层级，因此 debug = TRUE 不再中止；报告因
#     MortRate/AgeGroup 覆盖缺失或浓度为 NA 而丢失的网格；RR 表
#     属性优先于全局的单位与分支设置；detect_domain() 接受
#     (Grids, mRate) 参数，Mortality() 传入自己的输入。
#   修改 261101（v6.3）：MEAN/RANGE 走向量化引擎
#     （mort_rr_matrix()、mort_pwr_table()、mort_grid_table()）。PWRR 求和仍在
#     R 层按行序调用 sum()，因此数值与 join 路径保持逐位
#     一致。mortality_join_path() 保留为 debug = TRUE 的参照路径。
#   修改 261101（v6.3 诊断）：clamp、orphan 与覆盖报告
#     改为纯函数，返回 v6.2 的原文。Mortality() 每次调用都打印
#     它们；decomp_input_cache() 每次运行只打印一份，不再每个
#     state 打印一份。RR 键检查与覆盖报告按输入身份
#     缓存。

# ── 辅助函数 ─────────────────────────────────────────────────────────────────────
trunc_fmt <- function(x, n = 6) {
  str_c(str_c(head(x, n), collapse = ", "), if (length(x) > n) " ..." else "")
}

require_columns <- function(df, columns, label) {
  missing_cols <- setdiff(columns, names(df))
  if (length(missing_cols) > 0) {
    stop(
      label, " is missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }
  invisible(TRUE)
}

# ── 向量化引擎（v6.3） ──────────────────────────────────────────────────────────
# 以下四个函数取代按组执行的 dplyr join + weighted.mean() 循环。
# 它们是纯函数：不读取任何全局对象，因此工作进程可以重建它们。数值
# 与 join 路径保持逐位一致，因此 PWRR 求和仍在 R 层通过 tapply()
# 按行序调用 sum()；rowsum() 用 C 累加，会改动末几位。

# RR 值矩阵：n 个网格 × G 个终点/年龄档分组，单个 CI 分支。
# 组内浓度不重复是调用方的约定：RR_std() 断言
# （浓度、终点、年龄档、CI）键唯一；若该约定被破坏，match() 保留
# 第一行，而 join 路径会直接中止。
# rr_tbl 是单分支的 tibble(concentration, endpoint, agegroup, RR)，带
# 规范键；conc 存放可匹配的浓度字符串。分支中不存在的
# 浓度保持 NA_real_，与 left_join() 的结果完全一致。
mort_rr_matrix <- function(conc, rr_tbl, groups) {
  n_group <- nrow(groups)
  out <- matrix(NA_real_, nrow = length(conc), ncol = n_group)
  for (g in seq_len(n_group)) {
    rows <- rr_tbl$endpoint == groups$endpoint[[g]] &
      rr_tbl$agegroup == groups$agegroup[[g]]
    out[, g] <- unname(rr_tbl$RR[rows][match(conc, rr_tbl$concentration[rows])])
  }
  out
}

# 一个或多个取值分支的 PWRR 表：由 G 个分组 × D 个域矩阵组成的列表，
# 共享域轴。每个单元是该域内各网格按行序的 sum(RR * Pop) / sum(Pop)，
# 即恰好等于 weighted.mean(RR, Pop)。
# 没有可用网格的域保持 NA（left_join() 也会留下 NA）；权重和为 0 的域
# 得到 NaN，Mortality() 将其报告为无效。
# 域表之外的网格贡献精确的零，不被丢弃，
# 因此求和保持原行序，与 weighted.mean() 逐位一致。
mort_pwr_table_branches <- function(rr_list, pop, domain) {
  n_group <- ncol(rr_list[[1]])
  dom_names <- unique(as.character(domain[!is.na(domain)]))
  n_dom <- length(dom_names)
  out <- map(rr_list, function(one) {
    matrix(
      NA_real_,
      nrow = n_group,
      ncol = n_dom,
      dimnames = list(NULL, dom_names)
    )
  })
  if (n_dom == 0L) {
    return(out)
  }
  dom_code <- match(as.character(domain), dom_names)
  # 只拆分一次：各分支复用同一批域分组。
  dom_sets <- split(seq_along(pop), factor(dom_code, levels = seq_len(n_dom)))
  base_ok <- !is.na(dom_code) & !is.na(pop)
  base_weights <- pop
  base_weights[!base_ok] <- 0
  # 无 NA 的 RR 分支可以使用所有可 join 的网格，因此分母对所有这类分支
  # 是同一个向量；只计算一次。
  base_den <- vapply(dom_sets, function(ix) sum(base_weights[ix]), numeric(1))
  for (g in seq_len(n_group)) {
    for (i in seq_along(rr_list)) {
      rr <- rr_list[[i]][, g]
      if (anyNA(rr)) {
        usable <- base_ok & !is.na(rr)
        if (!any(usable)) {
          next
        }
        weights <- pop
        weights[!usable] <- 0
        den <- vapply(dom_sets, function(ix) sum(weights[ix]), numeric(1))
      } else {
        usable <- base_ok
        weights <- base_weights
        den <- base_den
      }
      weighted_rr <- rr
      weighted_rr[!usable] <- 0
      weighted_rr <- weighted_rr * weights
      num <- vapply(dom_sets, function(ix) sum(weighted_rr[ix]), numeric(1))
      value <- num / den
      value[tabulate(dom_code[usable], nbins = n_dom) == 0L] <- NA_real_
      out[[i]][g, ] <- as.numeric(value)
    }
  }
  out
}

# 单分支封装：保留给只计算一组取值列的调用方。
mort_pwr_table <- function(rr_mat, pop, domain) {
  mort_pwr_table_branches(list(rr_mat), pop, domain)[[1]]
}

# join 路径的必需列掩码：只有当 domain、浓度、Pop、RR、MortRate、AgeStruc 与 PWRR
# 全部非 NA，并且数值输入都有限时，该网格才进入结果。
# 对数值列来说，非 NA 与有限这两个条件合并成 is.finite()；
# 浓度不需要单独的判断项，因为缺失的浓度永远匹配不到 RR 表，
# 那里的 RR 就是 NA。
# rr_list 与 pwr_list 每个取值分支各带一个元素。RANGE 一次传入全部三个，
# 因此掩码只是一次共享的 pass：compute_group() 对每个组只把 join 后的
# 数据帧过滤一次，一次覆盖该组的所有取值列。
mort_keep_mask <- function(rr_list, pwr_list, pop, domain, ag, mrate) {
  n <- nrow(rr_list[[1]])
  n_group <- ncol(rr_list[[1]])
  dom_code <- match(as.character(domain), colnames(pwr_list[[1]]))
  # 网格属于某个域且人口有限时为 TRUE。
  out <- matrix(!is.na(domain) & is.finite(pop), nrow = n, ncol = n_group)
  # 把每个 PWRR 行一次展开到网格行，再对整矩阵取与：各分支的
  # pass 变成整矩阵运算，无需逐组的向量 pass。
  for (i in seq_along(rr_list)) {
    out <- out & is.finite(rr_list[[i]]) &
      t(is.finite(pwr_list[[i]])[, dom_code, drop = FALSE])
  }
  out & t(is.finite(ag)) & t(is.finite(mrate))
}

# 一个或多个取值分支的宽格式网格结果：tibble(x, y, 各取值列)。
# 行是各组掩码按组顺序取的并集，也等于各组丢弃不同网格时
# dplyr::full_join() 产出的行集合；某个组丢弃了某网格，
# 该组就在那里保持 NA。
# 除 groups/domain/grid_xy 之外，每个参数都按分支各一个元素，
# 因此 RANGE 只对分组跑一遍：行集合的并集、pop/ag/mrate 的
# 取子集以及分支循环都共享。rr_r 是 pwr 背后按真实浓度算出的 RR
# 矩阵；此处校验它，并让调用方可以重建该表。
mort_grid_table_branches <- function(rr_c, rr_r, pwr, pop, ag, mrate, groups,
                                     domain, value_names, grid_xy, keep = NULL) {
  n <- length(pop)
  n_group <- ncol(rr_c[[1]])
  n_branch <- length(rr_c)
  if (length(rr_r) != n_branch || length(pwr) != n_branch ||
    length(value_names) != n_branch) {
    stop(
      "mort_grid_table_branches(): rr_c, rr_r, pwr, and value_names ",
      "need one element per branch."
    )
  }
  for (i in seq_len(n_branch)) {
    if (!identical(dim(rr_c[[i]]), dim(rr_r[[i]]))) {
      stop(
        "mort_grid_table_branches(): rr_c and rr_r must have the same shape."
      )
    }
    if (nrow(pwr[[i]]) != n_group || nrow(groups) != n_group) {
      stop(
        "mort_grid_table_branches(): groups, rr_c, and pwr must agree ",
        "on the group count."
      )
    }
    if (length(value_names[[i]]) != n_group) {
      stop(
        "mort_grid_table_branches(): one value name per group is required."
      )
    }
  }
  if (is.null(keep)) {
    keep <- mort_keep_mask(rr_c, pwr, pop, domain, ag, mrate)
  }
  if (!identical(dim(keep), dim(rr_c[[1]]))) {
    stop("mort_grid_table_branches(): keep must have the shape of rr_c.")
  }
  dom_idx <- match(as.character(domain), colnames(pwr[[1]]))
  # 行集合：各组掩码按组顺序取的并集。
  selected <- vector("list", n_group)
  taken <- logical(n)
  for (g in seq_len(n_group)) {
    selected[[g]] <- which(keep[, g] & !taken)
    taken <- taken | keep[, g]
  }
  rows <- unlist(selected, use.names = FALSE)
  out <- tibble(x = grid_xy$x[rows], y = grid_xy$y[rows])
  for (g in seq_len(n_group)) {
    ok <- keep[, g]
    has_ok <- any(ok)
    if (has_ok) {
      base <- pop[ok] * ag[g, ok] * mrate[g, ok]
      dom_ok <- dom_idx[ok]
    }
    # 每组一个取值缓冲：各分支只写入保留下来的位置，
    # 其余位置对该丢弃了网格的分支保持 NA。
    value <- rep(NA_real_, n)
    for (i in seq_len(n_branch)) {
      if (has_ok) {
        pw <- pwr[[i]][g, ][dom_ok]
        value[ok] <- base * (rr_c[[i]][ok, g] - 1) / pw / 1e5
      }
      out[[value_names[[i]][[g]]]] <- value[rows]
    }
  }
  out
}

# 单分支封装：保留给只计算一组取值列的调用方。
mort_grid_table <- function(rr_c, rr_r, pwr, pop, ag, mrate, groups, domain,
                            value_names, grid_xy, keep = NULL) {
  mort_grid_table_branches(
    rr_c = list(rr_c),
    rr_r = list(rr_r),
    pwr = list(pwr),
    pop = pop,
    ag = ag,
    mrate = mrate,
    groups = groups,
    domain = domain,
    value_names = list(value_names),
    grid_xy = grid_xy,
    keep = keep
  )
}

# ── join_report ────────────────────────────────────────────────────────────────
# left_join + 分层 NA 报告。breakdown 列出要报告的 join 层级
# （例如 c("domain", "concentration")）；join 后数据帧中不存在的层级
# 会被忽略，因此过时的 breakdown 不会让诊断调用中止。
# 报告内容：L1 = 第 1 层取值完全缺失 → L2 = 第 1/2 层完全缺失
# → L3 = 其余层级的取值缺失。
join_report <- function(
  x,
  y,
  label,
  breakdown = NULL,
  relationship = NULL,
  verbose = FALSE,
  debug = FALSE
) {
  by <- intersect(names(x), names(y))

  if (!debug) {
    return(left_join(x, y, by = by, relationship = relationship))
  }

  new_cols <- setdiff(names(y), names(x))
  x <- x |> mutate(.key_ok = rowSums(across(all_of(by), is.na)) == 0)
  result <- left_join(x, y, by = by, relationship = relationship)
  show_n <- if (verbose) Inf else 6
  # 格级数据帧有 domain 与浓度，但没有终点/年龄档列；
  # 只有 join 后存在的层级才能传给 distinct()。
  bd <- intersect(breakdown %||% character(), names(result))
  for (col in new_cols) {
    na_rows <- result |> filter(.key_ok, is.na(.data[[col]]))
    n_na <- nrow(na_rows)
    if (n_na == 0) {
      next
    }
    if (length(bd) == 0) {
      log_msg(
        WARN,
        "{n_na} row(s) with NA in \"{col}\" after joining {label}. These rows will be dropped by the required-column filter."
      )
      next
    }
    matched <- result |> filter(.key_ok, !is.na(.data[[col]]))
    # L1 —— 第 1 层中零匹配的取值
    na_lvl1 <- na_rows |> distinct(across(all_of(bd[1]))) |> pull(1)
    ok_lvl1 <- matched |> distinct(across(all_of(bd[1]))) |> pull(1)
    l1 <- setdiff(na_lvl1, ok_lvl1)
    # L2 / L3 —— 第 1 层中部分匹配的取值
    partial <- intersect(na_lvl1, ok_lvl1)
    if (length(bd) >= 3 && length(partial) > 0) {
      ok_ep <- matched |>
        filter(.data[[bd[1]]] %in% partial) |>
        distinct(across(all_of(bd[1:2])))
      na_ep <- na_rows |>
        filter(.data[[bd[1]]] %in% partial) |>
        distinct(across(all_of(bd)))
      na_ep2 <- na_ep |> distinct(across(all_of(bd[1:2])))
      l2 <- anti_join(na_ep2, ok_ep, by = bd[1:2])
      l3 <- semi_join(na_ep, ok_ep, by = bd[1:2])
    } else {
      l2 <- tibble()
      l3 <- na_rows |>
        filter(.data[[bd[1]]] %in% partial) |>
        distinct(across(all_of(bd)))
    }
    # 格式化
    l1_label <- str_c(bd[1], "(s) entirely missing")
    l2_label <- str_c(str_c(bd[1:2], collapse = "/"), "(s) entirely missing")
    l3_label <- str_c(str_c(bd, collapse = "/"), "(s) missing")
    parts <- character()
    if (length(l1) > 0) {
      parts <- c(
        parts,
        str_c(length(l1), " ", l1_label, ": ", trunc_fmt(l1, n = show_n))
      )
    }
    if (nrow(l2) > 0) {
      s <- apply(l2, 1, str_c, collapse = "/")
      parts <- c(
        parts,
        str_c(nrow(l2), " ", l2_label, ": ", trunc_fmt(s, n = show_n))
      )
    }
    if (nrow(l3) > 0) {
      s <- apply(l3, 1, str_c, collapse = "/")
      parts <- c(
        parts,
        str_c(nrow(l3), " ", l3_label, ": ", trunc_fmt(s, n = show_n))
      )
    }
    log_msg(
      WARN,
      "{n_na} row(s) with NA in \"{col}\" after joining {label}. ",
      str_c(parts, collapse = "; ")
    )
  }
  result |> select(-.key_ok)
}

# ── 诊断报告与输入身份缓存（v6.3） ──────────────────────────────────────────────
# Mortality() 每次调用都会计算并打印五项输入诊断。分解缓存
# 绕过了 Mortality()，并且在 v6.3 诊断之前只打印它自己的 clamp 报告，
# 因此 v6.2 的诊断从分解快照中消失了。下面的辅助函数
# 逐字节返回 v6.2 的文本，
# 自身不打印任何内容：
#   Mortality()               每次调用打印一份；
#   decomp_input_cache()      每次运行按不同文本各打印一份。
# 缓存环境还记忆化了两个代价较高、由输入识别的步骤：
# RR 键检查（每次 Mortality() 调用约 0.21 s）与覆盖报告
# （约 0.05 s）。键用 rlang::hash() 计算，它来自 tidyverse 依赖，
# 作为廉价的内容指纹；改动输入会改变哈希，从而重跑该步骤。
# 缓存存在于同一个 R 进程中，从不写入磁盘。

.mortality_cache <- new.env(parent = emptyenv())

# 清空缓存及其测试计数器。
mortality_cache_reset <- function() {
  .mortality_cache$rr_keys <- new.env(parent = emptyenv())
  .mortality_cache$coverage <- new.env(parent = emptyenv())
  .mortality_cache$rr_asserted <- 0L
  .mortality_cache$coverage_computed <- 0L
  .mortality_cache$coverage_reused <- 0L
  invisible(TRUE)
}

mortality_cache_reset()

# 标准化 RR 表的身份：模型名、行数，以及键检查所读
# 五列的内容哈希。模型名
# 回退到固定标签，绝不回退到 NA：否则所有未设置的名称会
# 共用同一个缓存条目。
mortality_rr_identity <- function(rr_tbl) {
  model <- get0(".CR_Model", envir = globalenv(), ifnotfound = NULL)
  model <- if (is.null(model) || length(model) != 1L || is.na(model)) {
    "<unset>"
  } else {
    as.character(model)
  }
  str_c(
    model,
    nrow(rr_tbl),
    rlang::hash(rr_tbl[c("concentration", "endpoint", "agegroup", "CI", "RR")]),
    sep = "::"
  )
}

# 对 RR 表调用 assert_unique_key()，每个表身份一次。Mortality() 与
# decomp_input_cache() 都会调用它，因此即使两条路径都跑，一张表在
# 每次会话中也只扫描一次。
mortality_assert_rr_key <- function(rr_tbl) {
  key <- mortality_rr_identity(rr_tbl)
  if (exists(key, envir = .mortality_cache$rr_keys, inherits = FALSE)) {
    return(invisible(TRUE))
  }
  assert_unique_key(
    rr_tbl,
    c("concentration", "endpoint", "agegroup", "CI"),
    "RR lookup table"
  )
  assign(key, TRUE, envir = .mortality_cache$rr_keys)
  .mortality_cache$rr_asserted <- .mortality_cache$rr_asserted + 1L
  invisible(TRUE)
}

# v6.2 的 clamp 文本；所有浓度都在范围内时返回 character(0)。
mortality_clamp_text <- function(n_lo, n_hi, rr_range, label) {
  if (n_lo + n_hi == 0) {
    return(character())
  }
  str_c(
    n_lo + n_hi,
    " grids in ",
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

# 把一个浓度向量截到 RR 范围内，并描述这次改动。
# 计数一并返回，这样把同一向量同时用于两个角色的调用方（分解缓存
# 会把每个情景列同时传给 Conc_r 与 Conc_c）无需重算就能
# 打印两个标签。
mortality_clamp_report <- function(conc_vec, rr_range, label) {
  n_lo <- sum(conc_vec < rr_range[[1]], na.rm = TRUE)
  n_hi <- sum(conc_vec > rr_range[[2]], na.rm = TRUE)
  text <- mortality_clamp_text(n_lo, n_hi, rr_range, label)
  if (length(text) > 0) {
    conc_vec <- pmax(conc_vec, rr_range[[1]])
    conc_vec <- pmin(conc_vec, rr_range[[2]])
  }
  list(text = text, value = conc_vec, n_lo = n_lo, n_hi = n_hi)
}

# 针对 Grids 中没有配对行的网格的 v6.2 文本。kind = "concentration" 时
# 检查 Conc_r 的网格，kind = "population" 时检查 pop 的网格。
mortality_orphan_report <- function(grid_xy, source_xy, Grids, domain, kind,
                                    show_n) {
  orphan <- anti_join(grid_xy, source_xy, by = c("x", "y"))
  if (nrow(orphan) == 0) {
    return(character())
  }
  orphan_domains <- orphan |>
    left_join(Grids, by = c("x", "y")) |>
    distinct(across(all_of(domain))) |>
    pull(1)
  matched <- switch(
    kind,
    concentration = "have no matching concentration in Conc_r",
    population = "have no matching population in pop",
    stop("Unknown orphan report kind: ", kind)
  )
  str_c(
    nrow(orphan),
    " grid(s) in Grid_info ",
    matched,
    " (",
    length(orphan_domains),
    " domain(s): ",
    trunc_fmt(orphan_domains, n = show_n),
    "). They will be dropped by the required-column filter."
  )
}

# 针对一组网格、率、年龄、浓度与人口输入的 v6.2 覆盖文本。
# base_c 沿用 Mortality() 的约定：域列名为
# "domain"。返回 0 到 2 个字符串：先从所有组中被丢弃的域，
# 再从部分组中被丢弃的域。
mortality_coverage_report <- function(base_c, usable_groups, mrate, ag, show_n) {
  grid_domain <- base_c |>
    filter(!is.na(domain)) |>
    count(domain, name = "n_grid")
  if (nrow(grid_domain) == 0) {
    return(character())
  }
  group_coverage <- usable_groups |>
    inner_join(
      mrate |> distinct(domain, endpoint, agegroup),
      by = c("endpoint", "agegroup"),
      relationship = "many-to-many"
    ) |>
    semi_join(ag |> distinct(domain, agegroup), by = c("domain", "agegroup")) |>
    count(domain, name = "n_covered")
  domain_coverage <- grid_domain |>
    left_join(group_coverage, by = "domain") |>
    mutate(n_covered = coalesce(n_covered, 0L))
  n_group_total <- nrow(usable_groups)
  uncovered_all <- domain_coverage |> filter(n_covered == 0L)
  uncovered_some <- domain_coverage |>
    filter(n_covered > 0L, n_covered < n_group_total)
  grids_with_data <- base_c |>
    filter(!is.na(domain), !is.na(concentration), !is.na(Pop))
  grids_with_data_per_domain <- grids_with_data |>
    count(domain, name = "n_with_data")
  reports <- character()
  if (nrow(uncovered_all) > 0) {
    examples <- uncovered_all |>
      left_join(grids_with_data_per_domain, by = "domain") |>
      mutate(n_with_data = coalesce(n_with_data, 0L)) |>
      arrange(desc(n_with_data), desc(n_grid)) |>
      filter(n_with_data > 0) |>
      mutate(label = str_c(domain, " (", n_with_data, ")")) |>
      pull(label)
    if (length(examples) == 0) {
      examples <- uncovered_all |> arrange(desc(n_grid)) |> pull(domain)
    }
    reports <- c(reports, str_c(
      nrow(uncovered_all),
      " domain(s) in Grids have no MortRate/AgeGroup ",
      "coverage for any endpoint/agegroup group: ",
      sum(uncovered_all$n_grid),
      " ",
      "grid(s) in total, of which ",
      sum(grids_with_data$domain %in% uncovered_all$domain),
      " have ",
      "concentration data and are dropped from the result. Domains ",
      "(dropped grid count): ",
      trunc_fmt(examples, n = show_n),
      "."
    ))
  }
  if (nrow(uncovered_some) > 0) {
    examples <- uncovered_some |>
      left_join(grids_with_data_per_domain, by = "domain") |>
      mutate(n_with_data = coalesce(n_with_data, 0L)) |>
      arrange(desc(n_with_data), desc(n_grid)) |>
      filter(n_with_data > 0) |>
      mutate(label = str_c(domain, " (", n_with_data, ")")) |>
      pull(label)
    if (length(examples) == 0) {
      examples <- uncovered_some |> arrange(desc(n_grid)) |> pull(domain)
    }
    reports <- c(reports, str_c(
      nrow(uncovered_some),
      " domain(s) lack MortRate/AgeGroup coverage for ",
      "some endpoint/agegroup groups: ",
      sum(grids_with_data$domain %in% uncovered_some$domain),
      " grid(s) are ",
      "dropped from those groups and keep NA there. Domains ",
      "(dropped grid count): ",
      trunc_fmt(examples, n = show_n),
      "."
    ))
  }
  reports
}

# 上面报告的输入身份。只有报告真正读取的部分进入键：
# 域标签、浓度与人口的 NA 掩码，以及率表与年龄表中
# 去重后的覆盖集合。浓度与人口的数值大小
# 从不进入文本，因此改动它们不会使该条目失效。
mortality_coverage_key <- function(base_c, usable_groups, mrate, ag, domain,
                                   show_n) {
  rate_coverage <- mrate |> distinct(domain, endpoint, agegroup)
  age_coverage <- ag |> distinct(domain, agegroup)
  str_c(
    as.character(domain),
    show_n,
    rlang::hash(list(
      as.character(base_c$domain),
      is.na(base_c$concentration),
      is.na(base_c$Pop),
      as.character(usable_groups$endpoint),
      as.character(usable_groups$agegroup),
      as.character(rate_coverage$domain),
      as.character(rate_coverage$endpoint),
      as.character(rate_coverage$agegroup),
      as.character(age_coverage$domain),
      as.character(age_coverage$agegroup)
    )),
    sep = "::"
  )
}

# 带缓存的覆盖报告：每个输入身份只计算一次。调用方每次调用都打印
# 返回的字符串，因此文本仍然在每次 Mortality()
# 调用中出现一次。
mortality_coverage_cached <- function(base_c, usable_groups, mrate, ag, domain,
                                      show_n) {
  key <- mortality_coverage_key(
    base_c, usable_groups, mrate, ag, domain, show_n
  )
  if (exists(key, envir = .mortality_cache$coverage, inherits = FALSE)) {
    .mortality_cache$coverage_reused <- .mortality_cache$coverage_reused + 1L
    return(get(key, envir = .mortality_cache$coverage, inherits = FALSE))
  }
  reports <- mortality_coverage_report(
    base_c, usable_groups, mrate, ag, show_n
  )
  assign(key, reports, envir = .mortality_cache$coverage)
  .mortality_cache$coverage_computed <- .mortality_cache$coverage_computed + 1L
  reports
}

# 格级 PM2.5 归因死亡，PWRR 归一化。
#
# mRate 是域级调查数据，每个域一个取值。按相对风险
# 原理 I_g / I_domain = RR_e,a(C_g) / PWRR_domain,e,a，零 PM2.5
# 基线在同一域 × 终点 × 年龄组内一致：
#   I_0 = I_domain / PWRR_domain,e,a
#   PWRR_domain,e,a = sum_g(Pop_g * RR_e,a(C_g)) / sum_g(Pop_g)
# 各网格相对 TMREL 的归因死亡：
#   Mort_g = Pop_g * AgeStruc_a * (I_g - I_0)
#          = Pop_g * AgeStruc_a * MortRate_domain,e,a * (RR_e,a(C_g) - 1)
#            / PWRR_domain,e,a
# AgeStruc 在同一域 × 年龄组内为常数，因此在 PWRR 中约掉。
# 每个域只用一份跨终点或跨年龄平均的 PWRR 是无效的：它把 RR
# 水平不同的曲线混在一起，并把每个终点的偏差推向相反方向。
# CI = "RANGE" 时各分支使用自己的 PWRR（MEAN/UP/LOW）。格级 PAF
# (RR_g - 1) / RR_g 意味着每格 I_0 = MortRate / RR_g；PWRR 路径则在
# 每个域 × 终点 × 年龄组上保持一个物理上自洽的 I_0。
#
#   Grids    含 x、y 与域列的网格表。
#   Conc_r   真实浓度，x/y/concentration；决定 PWRR。
#   Conc_c   RR 分子用的反事实浓度；默认为
#            Conc_r。
#   ag       按域与年龄组的年龄结构比例。
#   mRate    按域、终点与年龄组的基线率。
#   pop      网格人口，x/y/Pop。
#   CI       "MEAN"（默认）、"UP"、"LOW" 或 "RANGE"。
#   domain   空间列，例如 "Country"；NULL 时自动识别。
#   verbose  FALSE 时警告中的域名截断为 6 个；TRUE 时全部显示。
#   debug    FALSE 用普通 left_join；TRUE 增加 join_report() 的 NA
#            诊断。
#   validate_keys  TRUE（默认）检查输入键的 NA 与重复；仅在输入已由
#            read_files() 检查过时才设为 FALSE。列、RR
#            键、输出键、精度与单位检查仍然生效。
#
# 返回宽格式数据帧，每个网格单元每个终点-年龄组-CI 一列，
# CI = "RANGE" 时加 _MEAN、_UP、_LOW 后缀。
Mortality <- function(
  Grids,
  Conc_r,
  Conc_c = NULL,
  ag,
  mRate,
  pop,
  CI = "MEAN",
  domain = NULL,
  verbose = FALSE,
  debug = FALSE,
  validate_keys = TRUE
) {
  show_n <- if (verbose) Inf else 6
  CI <- match.arg(CI, c("MEAN", "UP", "LOW", "RANGE"))

  if (is.null(Conc_c)) {
    Conc_c <- Conc_r
  }

  if (is.null(domain)) {
    domain <- detect_domain(Grids = Grids, mRate = mRate)
  }
  if (length(domain) != 1) {
    stop("domain must be one Grid_info column name.")
  }

  # ---- 守卫：必需列与键唯一性 --------------------------------------
  require_columns(Grids, c("x", "y", domain), "Grids")
  require_columns(Conc_r, c("x", "y", "concentration"), "Conc_r")
  require_columns(Conc_c, c("x", "y", "concentration"), "Conc_c")
  require_columns(pop, c("x", "y", "Pop"), "pop")
  require_columns(mRate, c("domain", "endpoint", "agegroup", "MortRate"), "mRate")
  require_columns(ag, c("domain", "agegroup", "AgeStruc"), "ag")

  if (validate_keys) {
    assert_unique_key(Grids, c("x", "y"), "Grids")
    assert_unique_key(Conc_r, c("x", "y"), "Conc_r")
    # Conc_c 默认取 Conc_r；这种情况下一次检查即可覆盖两者。
    if (!identical(Conc_c, Conc_r)) {
      assert_unique_key(Conc_c, c("x", "y"), "Conc_c")
    }
    assert_unique_key(pop, c("x", "y"), "pop")
  }

  mRate <- mRate |>
    mutate(
      endpoint = tolower(endpoint),
      agegroup = matchable(as.numeric(agegroup), 0),
      MortRate = as.numeric(MortRate)
    )
  ag <- ag |>
    mutate(
      agegroup = matchable(as.numeric(agegroup), 0),
      AgeStruc = as.numeric(AgeStruc)
    )
  pop <- pop |>
    mutate(Pop = as.numeric(Pop))
  if (validate_keys) {
    assert_unique_key(
      mRate,
      c("domain", "endpoint", "agegroup"),
      "mRate"
    )
    assert_unique_key(ag, c("domain", "agegroup"), "ag")
  }

  if (any(pop$Pop < 0, na.rm = TRUE)) {
    stop("Population contains negative values.")
  }

  # ---- 取标准化的 RR 表 --------------------------------------------
  RR_raw <- .RR_std_tbl
  if (is.null(RR_raw) || nrow(RR_raw) == 0) {
    stop("RR lookup table is empty. Run set_Model() first.")
  }
  require_columns(
    RR_raw,
    c("concentration", "endpoint", "agegroup", "CI", "RR"),
    "RR lookup table"
  )

  # RR 表的元数据优先于全局设置，这样手工赋值的表不会带着
  # 其他模型的单位或 PWRR 分支运行。
  rr_meta <- get0(".RR_meta", envir = globalenv(), ifnotfound = list())
  conc_unit <- get0(".CONC_UNIT", envir = globalenv(), ifnotfound = NULL)
  rr_unit <- attr(RR_raw, "conc_unit", exact = TRUE) %||%
    rr_meta$conc_unit %||%
    NULL
  if (!is.null(conc_unit) && !is.null(rr_unit) && !identical(conc_unit, rr_unit)) {
    stop(
      "Concentration unit mismatch: RR model expects \"", rr_unit,
      "\", data were loaded as \"", conc_unit, "\"."
    )
  }

  # RR_std() 返回规范键与无重复的表，因此由 set_Model() 构建的表
  # 跳过再次归一化。手工赋值的表按固定 0.1 网格重新规范化。
  if (!isTRUE(attr(RR_raw, "rr_normalized", exact = TRUE))) {
    RR_raw <- RR_raw |>
      mutate(
        concentration = matchable(as.numeric(concentration), 1),
        endpoint = tolower(endpoint),
        agegroup = matchable(as.numeric(agegroup), 0)
      )
  }
  # 键检查是一次调用中最大的固定开销（约 0.21 s），而 RR_std() 的输出
  # 很少变化，因此每个表身份只跑一次。
  mortality_assert_rr_key(RR_raw)
  if (!setequal(unique(RR_raw$CI), c("MEAN", "UP", "LOW"))) {
    stop("RR lookup table must contain exactly MEAN, UP, and LOW branches.")
  }

  rr_conc_range <- range(as.numeric(RR_raw$concentration), na.rm = TRUE)
  pwr_branch <- attr(RR_raw, "pwr_branch", exact = TRUE) %||%
    get0(".PWRR_branch", envir = globalenv(), ifnotfound = "branch")
  if (!pwr_branch %in% c("branch", "mean")) {
    stop("PWRR branch mode must be 'branch' or 'mean'.")
  }

  # ---- 把浓度截到 RR 查找表范围内 ----------------------------------
  clamp_r <- mortality_clamp_report(
    as.numeric(Conc_r$concentration),
    rr_conc_range,
    "Conc_r"
  )
  if (length(clamp_r$text) > 0) {
    log_msg(WARN, "{clamp_r$text}")
  }
  Conc_r <- Conc_r |>
    mutate(concentration = matchable(clamp_r$value, 1))

  clamp_c <- mortality_clamp_report(
    as.numeric(Conc_c$concentration),
    rr_conc_range,
    "Conc_c"
  )
  if (length(clamp_c$text) > 0) {
    log_msg(WARN, "{clamp_c$text}")
  }
  Conc_c <- Conc_c |>
    mutate(concentration = matchable(clamp_c$value, 1))

  # ---- 守卫：坐标重叠 ----------------------------------------------
  grid_xy <- Grids |> select(x, y) |> distinct()
  conc_xy <- Conc_r |> select(x, y) |> distinct()
  pop_xy <- pop |> select(x, y) |> distinct()

  conc_orphan <- mortality_orphan_report(
    grid_xy, conc_xy, Grids, domain, "concentration", show_n
  )
  if (length(conc_orphan) > 0) {
    log_msg(WARN, "{conc_orphan}")
  }

  pop_orphan <- mortality_orphan_report(
    grid_xy, pop_xy, Grids, domain, "population", show_n
  )
  if (length(pop_orphan) > 0) {
    log_msg(WARN, "{pop_orphan}")
  }

  # ---- 基础表：每格一行 --------------------------------------------
  # base_c 用反事实/真实浓度算 RR 分子；
  # base_r 用真实浓度算 PWRR。
  base_xy <- Grids |>
    select(x, y, domain = all_of(domain))
  base_c <- base_xy |>
    left_join(Conc_c, by = c("x", "y")) |>
    left_join(pop, by = c("x", "y"))
  base_r <- base_xy |>
    left_join(Conc_r, by = c("x", "y")) |>
    left_join(pop, by = c("x", "y"))

  # ---- 按终点 × 年龄档分组 -----------------------------------------
  # RANGE 覆盖全部分支；单个 CI 只覆盖它自己的分支。两组键
  # 及其顺序都与旧的 RR_all |> distinct() 一致（pivot_wider() 保持
  # 相同键的首次出现顺序）。
  groups <- if (CI == "RANGE") {
    RR_raw |> distinct(endpoint, agegroup)
  } else {
    RR_raw |> filter(CI == !!CI) |> distinct(endpoint, agegroup)
  }
  if (nrow(groups) == 0) {
    stop("RR lookup table has no endpoint-agegroup groups.")
  }

  available_mort_groups <- mRate |>
    distinct(endpoint, agegroup)
  available_age_groups <- ag |>
    mutate(agegroup = matchable(as.numeric(agegroup), 0)) |>
    distinct(agegroup)
  usable_groups <- groups |>
    semi_join(available_mort_groups, by = c("endpoint", "agegroup")) |>
    semi_join(available_age_groups, by = "agegroup")

  if (nrow(usable_groups) == 0) {
    stop(
      "No endpoint/agegroup group is jointly covered by RR, MortRate, and AgeGroup. ",
      "Check model endpoints (", paste(unique(groups$endpoint), collapse = ", "),
      ") against MortRate endpoints (",
      paste(unique(mRate$endpoint), collapse = ", "), ")."
    )
  }
  if (nrow(usable_groups) < nrow(groups)) {
    missing_groups <- anti_join(groups, usable_groups, by = c("endpoint", "agegroup"))
    log_msg(
      WARN,
      "{nrow(missing_groups)} endpoint/agegroup group(s) lack MortRate or ",
      "AgeGroup coverage and will remain NA: ",
      trunc_fmt(str_c(missing_groups$endpoint, missing_groups$agegroup, sep = "/"))
    )
  }

  # ---- 报告必需列过滤导致的静默网格丢弃 ----------------------------
  # 下面的过滤会静默丢弃网格行。这里报告两种输入
  # 原因；取值、行序与 NA 模式保持不变。该
  # 报告每个输入身份计算一次，每次调用打印一次。
  coverage_reports <- mortality_coverage_cached(
    base_c = base_c,
    usable_groups = usable_groups,
    mrate = mRate,
    ag = ag,
    domain = domain,
    show_n = show_n
  )
  for (report in coverage_reports) {
    log_msg(WARN, "{report}")
  }

  conc_na <- base_c |>
    semi_join(Conc_c |> select(x, y) |> distinct(), by = c("x", "y")) |>
    filter(!is.na(domain), is.na(concentration))
  if (nrow(conc_na) > 0) {
    examples <- conc_na |>
      count(domain, name = "n_grid") |>
      arrange(desc(n_grid)) |>
      mutate(label = str_c(domain, " (", n_grid, ")")) |>
      pull(label)
    log_msg(
      WARN,
      "{nrow(conc_na)} grid(s) in {length(examples)} domain(s) have NA ",
      "concentration in Conc_c and are dropped: ",
      trunc_fmt(examples, n = show_n), "."
    )
  }

  # ---- 计算：向量化引擎，或 debug/诊断用的 join 路径 ----------------
  # debug = TRUE 时保留 join 链，以便 join_report() 打印同样的
  # 诊断。这是 mortality_join_path() 唯一的调用方；它保留为
  # 长期存在的参照路径。
  use_engine <- !isTRUE(debug)
  result <- if (use_engine) {
    mortality_engine(
      base_c = base_c,
      base_r = base_r,
      rr_raw = RR_raw,
      groups = groups,
      mrate = mRate,
      ag = ag,
      ci = CI,
      pwr_branch = pwr_branch,
      show_n = show_n
    )
  } else {
    mortality_join_path(
      base_c = base_c,
      base_r = base_r,
      rr_raw = RR_raw,
      groups = groups,
      mrate = mRate,
      ag = ag,
      ci = CI,
      pwr_branch = pwr_branch,
      verbose = verbose,
      debug = debug,
      show_n = show_n
    )
  }

  if (nrow(result) == 0) {
    stop("Mortality result has 0 rows. Check input coverage.")
  }
  assert_unique_key(result, c("x", "y"), "Mortality output")

  result
}

# ── 引擎路径（v6.3） ────────────────────────────────────────────────────────────
# 每个分支一张 RR 矩阵，每个分母分支一张 PWRR 表，然后每个取值分支
# 组装一次网格表。不使用 dplyr join，也不按组过滤。
mortality_engine <- function(base_c, base_r, rr_raw, groups, mrate, ag, ci,
                             pwr_branch, show_n) {
  if (nrow(base_c) != nrow(base_r)) {
    stop("Mortality(): Conc_c and Conc_r must keep one row per grid.")
  }
  n_group <- nrow(groups)
  grid_xy <- base_c[c("x", "y")]
  grid_domain <- base_c$domain
  grid_pop <- base_c$Pop
  conc_c <- base_c$concentration
  conc_r <- base_r$concentration

  branch_tbl <- function(branch) {
    rr_raw[rr_raw$CI == branch, c("concentration", "endpoint", "agegroup", "RR")]
  }

  # 每个分支一组取值列；每个取值列一个 PWRR 分母：pwr_branch = "branch" 时
  # RANGE 使用对应的分支，否则使用 MEAN 分支；单个 CI 在
  # pwr_branch = "mean" 时使用 MEAN 分支。
  value_branches <- if (ci == "RANGE") c("MEAN", "UP", "LOW") else ci
  pwr_for_value <- if (ci == "RANGE") {
    if (identical(pwr_branch, "branch")) {
      value_branches
    } else {
      rep("MEAN", length(value_branches))
    }
  } else if (identical(pwr_branch, "mean")) {
    "MEAN"
  } else {
    ci
  }
  pwr_branches <- unique(pwr_for_value)

  # Conc_c 默认取 Conc_r，因此两张 RR 矩阵的记录相同。
  same_conc <- identical(conc_c, conc_r)
  rr_num <- map(value_branches, function(branch) {
    mort_rr_matrix(conc_c, branch_tbl(branch), groups)
  }) |>
    set_names(value_branches)
  rr_den <- map(pwr_branches, function(branch) {
    if (same_conc && branch %in% value_branches) {
      rr_num[[branch]]
    } else {
      mort_rr_matrix(conc_r, branch_tbl(branch), groups)
    }
  }) |>
    set_names(pwr_branches)

  # 任一分支在其浓度上的取值为 NA 时，compute_group() 会把该网格从所有 PWRR
  # 分支中丢弃；这里复现同一套共享行过滤。
  if (length(pwr_branches) > 1) {
    den_ok <- !is.na(rr_den[[1]])
    for (branch in pwr_branches[-1]) {
      den_ok <- den_ok & !is.na(rr_den[[branch]])
    }
    for (branch in pwr_branches) {
      rr_den[[branch]][!den_ok] <- NA_real_
    }
  }

  pwr_tbl <- mort_pwr_table_branches(rr_den, grid_pop, grid_domain) |>
    set_names(pwr_branches)
  dom_names <- colnames(pwr_tbl[[1]])
  dom_code <- match(as.character(grid_domain), dom_names)

  # 年龄结构与死亡率都是域级的。用一次小的域查找把它们展开，
  # 无需对每个网格、每个组做 match()。
  expand_by_domain <- function(values, domain_values) {
    values[match(dom_names, as.character(domain_values))][dom_code]
  }
  ag_mat <- do.call(rbind, map(groups$agegroup, function(age) {
    one <- ag[ag$agegroup == age, , drop = FALSE]
    expand_by_domain(one$AgeStruc, one$domain)
  }))
  mr_mat <- do.call(rbind, map2(groups$endpoint, groups$agegroup, function(ep, age) {
    one <- mrate[mrate$endpoint == ep & mrate$agegroup == age, , drop = FALSE]
    expand_by_domain(one$MortRate, one$domain)
  }))

  # ---- 守卫：PWRR 必须有限且 >= 1，按 join 路径的报告顺序 ------------
  n_dom <- length(dom_names)
  n_pwr <- length(pwr_branches)
  skipped_keys <- character()
  for (g in seq_len(n_group)) {
    is_inf <- matrix(FALSE, nrow = n_dom, ncol = n_pwr)
    is_lt1 <- matrix(FALSE, nrow = n_dom, ncol = n_pwr)
    is_nan <- matrix(FALSE, nrow = n_dom, ncol = n_pwr)
    for (j in seq_along(pwr_branches)) {
      one <- pwr_tbl[[pwr_branches[[j]]]][g, ]
      is_inf[, j] <- is.infinite(one)
      is_lt1[, j] <- one < 1 & !is.na(one)
      # 用 is.nan()，不用 is.na()：NA 单元表示该域没有可用网格，
      # join 路径从不把这种域放进 pwr，因此也从不报告。NaN 才是它
      # 会报告的零权重域（"no valid PWRR"）。
      is_nan[, j] <- is.nan(one)
    }
    if (any(is_inf) || any(is_lt1)) {
      # t() + which() 按域优先、分支次之的顺序遍历，与 pivot_longer() 一致。
      bad_pos <- which(t(is_inf | is_lt1))
      bad_keys <- str_c(
        dom_names[((bad_pos - 1L) %/% n_pwr) + 1L],
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
      nan_pos <- which(t(is_nan))
      skipped_keys <- c(
        skipped_keys,
        str_c(
          dom_names[((nan_pos - 1L) %/% n_pwr) + 1L],
          groups$endpoint[[g]],
          groups$agegroup[[g]],
          sep = "/"
        )
      )
    }
  }

  # ---- 取值：一份共享 keep 掩码，对分支集合遍历一遍 ------------------
  keep <- mort_keep_mask(
    rr_num[value_branches],
    pwr_tbl[pwr_for_value],
    grid_pop,
    grid_domain,
    ag_mat,
    mr_mat
  )

  # 一次调用，对终点/年龄档分组只遍历一遍。列顺序为
  # 分组优先，然后是分支顺序，与 join 路径一致。
  result <- mort_grid_table_branches(
    rr_c = rr_num[value_branches],
    rr_r = map(pwr_for_value, function(branch) rr_den[[branch]]),
    pwr = pwr_tbl[pwr_for_value],
    pop = grid_pop,
    ag = ag_mat,
    mrate = mr_mat,
    groups = groups,
    domain = grid_domain,
    value_names = map(value_branches, function(branch) {
      str_c(groups$endpoint, groups$agegroup, branch, sep = "_")
    }),
    grid_xy = grid_xy,
    keep = keep
  )

  skipped_keys <- unique(skipped_keys)
  if (length(skipped_keys) > 0) {
    log_msg(
      WARN,
      "{length(skipped_keys)} domain/endpoint/age combination(s) have no valid ",
      "PWRR; affected rows will be dropped: ",
      trunc_fmt(skipped_keys, n = show_n)
    )
  }

  result
}

# ── join 路径（v6.3 前的参照实现） ──────────────────────────────────────────────
# 长期保留的 debug = TRUE 路径：join_report() 打印每次 join 的 NA 诊断，
# 引擎不复制这部分。这是原始实现：每个终点/年龄档分组
# 一条 dplyr join 链，当各组保留不同网格时再用
# full_join() 归约。
mortality_join_path <- function(base_c, base_r, rr_raw, groups, mrate, ag, ci,
                                pwr_branch, verbose, debug, show_n) {
  use_branch_pwr <- ci == "RANGE" && identical(pwr_branch, "branch")

  if (ci == "RANGE") {
    RR_all <- rr_raw |>
      pivot_wider(names_from = CI, values_from = RR) |>
      rename(RR = MEAN, RR_UP = UP, RR_LOW = LOW)
    pwr_source <- if (use_branch_pwr) {
      RR_all |>
        select(
          concentration, endpoint, agegroup,
          PWRR_MEAN = RR, PWRR_UP = RR_UP, PWRR_LOW = RR_LOW
        )
    } else {
      RR_all |>
        select(concentration, endpoint, agegroup, PWRR = RR)
    }
    rr_cols <- c("RR", "RR_UP", "RR_LOW")
  } else {
    RR_all <- rr_raw |>
      filter(CI == !!ci) |>
      select(concentration, endpoint, agegroup, RR)
    denominator_ci <- if (identical(pwr_branch, "mean")) "MEAN" else ci
    pwr_source <- rr_raw |>
      filter(CI == denominator_ci) |>
      select(concentration, endpoint, agegroup, PWRR = RR)
    rr_cols <- "RR"
  }
  pwr_cols <- if (use_branch_pwr) {
    c("PWRR_MEAN", "PWRR_UP", "PWRR_LOW")
  } else {
    "PWRR"
  }

  compute_group <- function(ep, age) {
    rr_g <- RR_all |>
      filter(endpoint == ep, agegroup == age) |>
      select(concentration, all_of(rr_cols))

    pwr_source_g <- pwr_source |>
      filter(endpoint == ep, agegroup == age) |>
      select(concentration, all_of(pwr_cols))

    pwr_values <- base_r |>
      select(domain, concentration, Pop) |>
      filter(!is.na(domain), !is.na(concentration), !is.na(Pop)) |>
      left_join(
        pwr_source_g,
        by = "concentration",
        relationship = "many-to-one"
      ) |>
      filter(if_all(all_of(pwr_cols), ~ !is.na(.x)))

    pwr <- pwr_values |>
      group_by(domain) |>
      summarise(
        across(
          all_of(pwr_cols),
          ~ weighted.mean(.x, Pop),
          .names = "{.col}"
        ),
        .groups = "drop"
      )

    pwr_long <- pwr |>
      pivot_longer(
        all_of(pwr_cols),
        names_to = "branch",
        values_to = "PWRR_value"
      )

    bad_inf <- is.infinite(pwr_long$PWRR_value)
    bad_lt1 <- pwr_long$PWRR_value < 1 & !is.na(pwr_long$PWRR_value)
    if (any(bad_inf) || any(bad_lt1)) {
      bad_keys <- pwr_long |>
        filter(bad_inf | bad_lt1) |>
        mutate(key = str_c(domain, ep, age, sep = "/")) |>
        pull(key) |>
        unique()
      stop(
        "Invalid PWRR detected for endpoint/agegroup ",
        ep,
        "/",
        age,
        ". ",
        if (any(bad_inf)) "Some domains have Inf PWRR. ",
        if (any(bad_lt1)) "Some have PWRR < 1. ",
        "Check concentration and population for: ",
        trunc_fmt(bad_keys, n = show_n)
      )
    }

    na_pwrr <- is.na(pwr_long$PWRR_value)
    skipped <- character()
    if (any(na_pwrr)) {
      skipped <- pwr_long |>
        filter(na_pwrr) |>
        mutate(key = str_c(domain, ep, age, sep = "/")) |>
        pull(key) |>
        unique()
      pwr <- pwr |>
        filter(if_all(all_of(pwr_cols), ~ !is.na(.x)))
    }

    mrate_g <- mrate |>
      filter(endpoint == ep, agegroup == age) |>
      select(domain, MortRate)
    ag_g <- ag |>
      filter(agegroup == age) |>
      select(domain, AgeStruc)

    value_cols <- if (ci == "RANGE") {
      c(
        str_c(ep, age, "MEAN", sep = "_"),
        str_c(ep, age, "UP", sep = "_"),
        str_c(ep, age, "LOW", sep = "_")
      )
    } else {
      str_c(ep, age, ci, sep = "_")
    }

    mort <- base_c |>
      join_report(
        rr_g,
        "RR table",
        breakdown = c("domain", "concentration"),
        relationship = "many-to-one",
        verbose = verbose,
        debug = debug
      ) |>
      join_report(
        mrate_g,
        "mortality rate",
        breakdown = "domain",
        verbose = verbose,
        debug = debug
      ) |>
      join_report(
        ag_g,
        "age structure",
        breakdown = "domain",
        verbose = verbose,
        debug = debug
      ) |>
      join_report(
        pwr,
        "PWRR",
        breakdown = "domain",
        verbose = verbose,
        debug = debug
      ) |>
      filter(
        if_all(
          all_of(c("domain", "concentration", "Pop", rr_cols, "MortRate", "AgeStruc", pwr_cols)),
          ~ !is.na(.x)
        )
      ) |>
      filter(
        if_all(
          all_of(c("Pop", rr_cols, "MortRate", "AgeStruc", pwr_cols)),
          ~ is.finite(.x)
        )
      )

    if (nrow(mort) == 0) {
      empty <- base_c |>
        slice(0) |>
        select(x, y)
      for (col in value_cols) {
        empty[[col]] <- numeric()
      }
      return(list(data = empty, skipped = skipped))
    }

    if (ci == "RANGE") {
      if (use_branch_pwr) {
        mort <- mort |>
          mutate(
            !!sym(value_cols[1]) :=
              Pop * AgeStruc * MortRate * (RR - 1) / PWRR_MEAN / 1e5,
            !!sym(value_cols[2]) :=
              Pop * AgeStruc * MortRate * (RR_UP - 1) / PWRR_UP / 1e5,
            !!sym(value_cols[3]) :=
              Pop * AgeStruc * MortRate * (RR_LOW - 1) / PWRR_LOW / 1e5
          )
      } else {
        mort <- mort |>
          mutate(
            !!sym(value_cols[1]) :=
              Pop * AgeStruc * MortRate * (RR - 1) / PWRR / 1e5,
            !!sym(value_cols[2]) :=
              Pop * AgeStruc * MortRate * (RR_UP - 1) / PWRR / 1e5,
            !!sym(value_cols[3]) :=
              Pop * AgeStruc * MortRate * (RR_LOW - 1) / PWRR / 1e5
          )
      }
      mort <- mort |> select(x, y, all_of(value_cols))
    } else {
      mort <- mort |>
        mutate(
          !!sym(value_cols) :=
            Pop * AgeStruc * MortRate * (RR - 1) / PWRR / 1e5
        ) |>
        select(x, y, all_of(value_cols))
    }

    list(data = mort, skipped = skipped)
  }

  group_results <- map2(
    groups$endpoint,
    groups$agegroup,
    compute_group
  )

  skipped_keys <- group_results |>
    map("skipped") |>
    unlist(use.names = FALSE) |>
    unique()

  if (length(skipped_keys) > 0) {
    log_msg(
      WARN,
      "{length(skipped_keys)} domain/endpoint/age combination(s) have no valid ",
      "PWRR; affected rows will be dropped: ",
      trunc_fmt(skipped_keys, n = show_n)
    )
  }

  data_list <- map(group_results, "data")
  # 各组通常保留完全相同的 (x, y) 行，因此宽结果是
  # 列绑定。某个组丢弃或重排了行时，退回 full_join。
  key_cols <- c("x", "y")
  keys_ref <- data_list[[1]][key_cols]
  same_keys <- all(map_lgl(
    data_list[-1],
    function(one) identical(one[key_cols], keys_ref)
  ))
  if (same_keys) {
    bind_cols(
      keys_ref,
      bind_cols(map(data_list, function(one) one[setdiff(names(one), key_cols)]))
    )
  } else {
    reduce(
      data_list,
      full_join,
      by = key_cols,
      relationship = "one-to-one"
    )
  }
}

# 由 Grids × mRate 的重叠自动识别 PWRR 域列。
# 默认值让 detect_domain() 可以无参调用（Mortality_at()、
# Decomposition() 与 aggregate_drivers()）；Mortality() 传入自己的输入，
# 因此过时的全局 Grid_info/MortRate 不会改变它使用的域列。
detect_domain <- function(Grids = Grid_info, mRate = MortRate) {
  # 把每个 mRate 列与每个 Grids 非坐标列配对尝试。
  # 选取唯一取值重叠最多的一对。
  geo_candidates <- setdiff(names(Grids), c("x", "y"))
  mort_candidates <- setdiff(names(mRate), c("endpoint", "agegroup"))

  best_geo <- NULL
  best_mort <- NULL
  best_hit <- 0

  for (gcol in geo_candidates) {
    geo_vals <- unique(Grids[[gcol]])
    for (mcol in mort_candidates) {
      mort_vals <- unique(mRate[[mcol]])
      hits <- length(intersect(mort_vals, geo_vals))
      if (hits > best_hit) {
        best_hit <- hits
        best_geo <- gcol
        best_mort <- mcol
      }
    }
  }

  if (is.null(best_geo) || best_hit == 0) {
    stop(
      "Cannot auto-detect PWRR domain. No column pair between MortRate (",
      str_c(mort_candidates, collapse = ", "),
      ") and Grid_info (",
      str_c(geo_candidates, collapse = ", "),
      ") shares matching values. Specify domain explicitly."
    )
  }

  n_mort <- length(unique(mRate[[best_mort]]))
  if (best_hit < n_mort * 0.8) {
    log_msg(
      WARN,
      "Only ",
      best_hit,
      "/",
      n_mort,
      " MortRate$\"",
      best_mort,
      "\" values match Grid_info$\"",
      best_geo,
      "\". ",
      "Specify domain explicitly if data is at a finer resolution."
    )
  }

  log_msg(
    INFO,
    "  PWRR domain: Grid_info$\"{best_geo}\" <- MortRate$\"{best_mort}\" ",
    "({best_hit} domains matched)"
  )
  best_geo
}

# 单个已加载年份/情景的格级归因死亡。薄封装：
# 读取已加载的全局对象，自动识别 PWRR 域，然后调用 Mortality()。
#   at       年份/情景，例如 "base2015"。
#   CI       "MEAN"（默认）、"UP"、"LOW" 或 "RANGE"。
#   domain   网格域列；NULL（默认）时由
#            Grid_info × MortRate 的重叠自动识别。
#   verbose  FALSE 时警告中的域名截断为 6 个；TRUE 时全部显示。
#   debug    FALSE 用普通 left_join；TRUE 增加 join_report() 诊断。
#   validate_keys  TRUE（默认）重新检查输入键；仅在
#            read_files() 检查过之后才设为 FALSE。
# 返回 Mortality() 的宽格式网格数据帧。
Mortality_at <- function(
  at,
  CI = "MEAN",
  domain = NULL,
  verbose = FALSE,
  debug = FALSE,
  validate_keys = TRUE
) {
  if (is.null(domain)) {
    domain <- detect_domain()
  }
  Mortality(
    Grids = Grid_info,
    Conc_r = getConc_real(at),
    Conc_c = NULL,
    pop = getPop(at),
    ag = getAgeGroup(at),
    mRate = getMortRate(at),
    CI = CI,
    domain = domain,
    verbose = verbose,
    debug = debug,
    validate_keys = validate_keys
  )
}
