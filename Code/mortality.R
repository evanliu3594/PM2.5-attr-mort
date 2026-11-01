# Mortality calculation module — PWRR-normalised grid-level attributable deaths
#   - Mortality()      — core formula: Mort = Pop × AgeStruc × MortRate × (RR−1) / PWRR_{domain,endpoint,age}
#   - Mortality_at()   — convenience wrapper for a given year/scenario
#   - detect_domain()  — auto-detect PWRR domain from MortRate × Grid_info overlap
#   - join_report()    — left_join with optional hierarchical NA diagnostic report
#   Modified 260618: three-phase PWRR guard (Inf/<1→stop, NA→skip, all-dropped→stop),
#     verbose/debug parameters, trunc_fmt accepts n=Inf for unbounded output.
#   Modified 261026: PWRR per domain × endpoint × agegroup; RANGE branches use
#     matching PWRR; NA PWRR drops only affected rows, not whole domains.
#   Modified 261026: Mortality() iterates endpoint × agegroup groups; each group
#     is a grid-level join, avoiding a grid × endpoint × agegroup long table.
#     compute_group() is side-effect free and ready for future_map2().
#   Modified 261026: pwr_branch config ("branch" or "mean") controls whether
#     RANGE uses branch-specific or common MEAN PWRR.
#   Modified 261026b: single branches reuse the RANGE denominator; required
#     columns only; key guards; dgt_conc and unit checks; missing groups stay NA.
#   Modified 261027: reuse the RR_std() key normalisation (rr_normalized flag);
#     check Conc_c only when it differs from Conc_r.
#   Modified 261101: join_report() resolves the breakdown against the joined
#     frame, so debug = TRUE no longer aborts; grid drops from missing
#     MortRate/AgeGroup coverage or NA concentrations are reported; RR table
#     attributes win over global unit/branch settings; detect_domain() takes
#     (Grids, mRate) arguments and Mortality() passes its own inputs.
#   Modified 261101 (v6.3): MEAN/RANGE compute through the vectorised engine
#     (mort_rr_matrix(), mort_pwr_table(), mort_grid_table()). PWRR sums stay
#     R-level sum() in row order, so values remain bit-identical to the join
#     path. mortality_join_path() stays as the debug = TRUE reference path.
#   Modified 261101 (v6.3 diagnostics): the clamp, orphan, and coverage reports
#     are pure helpers that return the v6.2 text. Mortality() prints them on
#     every call; decomp_input_cache() prints one copy per run instead of one per
#     state. The RR key check and the coverage report are cached per input
#     identity.

# ── helpers ─────────────────────────────────────────────────────────────────────
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

# ── vectorised engine (v6.3) ───────────────────────────────────────────────────
# The four functions below replace the per-group dplyr join + weighted.mean()
# loop. They are pure: they read no global, so a worker can rebuild them. Values
# stay bit-identical to the join path, so PWRR sums use R-level sum() through
# tapply() in row order; rowsum() accumulates in C and changes the last bits.

# RR value matrix: n grids x G endpoint/agegroup groups, one CI branch.
# Duplicate concentrations within a group are the caller's contract: RR_std()
# asserts unique (concentration, endpoint, agegroup, CI) keys, and match() keeps
# the first row if that contract is broken (the join path would abort instead).
# rr_tbl is a single-branch tibble(concentration, endpoint, agegroup, RR) with
# canonical keys; conc holds the matchable concentration strings. Concentrations
# absent from the branch stay NA_real_, exactly as left_join() leaves them.
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

# PWRR tables for one or more value branches: a list of G groups x D domains
# matrices sharing the domain axis. Each cell is sum(RR * Pop) / sum(Pop) over
# the grids of that domain in row order, i.e. exactly weighted.mean(RR, Pop).
# A domain with no usable grid keeps NA (left_join() would leave it NA); a domain
# whose weights sum to zero gives NaN, which Mortality() reports as invalid.
# Grids outside the domain table contribute exact zeros instead of being dropped,
# so the sums keep their row order and stay bit-identical to weighted.mean().
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
  # Split once: every branch reuses the same domain groups.
  dom_sets <- split(seq_along(pop), factor(dom_code, levels = seq_len(n_dom)))
  base_ok <- !is.na(dom_code) & !is.na(pop)
  base_weights <- pop
  base_weights[!base_ok] <- 0
  # Branches without NA RR use every joinable grid, so the denominator is one
  # vector for all of them; compute it once.
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

# Single-branch wrapper kept for callers that compute one value column set.
mort_pwr_table <- function(rr_mat, pop, domain) {
  mort_pwr_table_branches(list(rr_mat), pop, domain)[[1]]
}

# Required-column mask of the join path: a grid enters the result only when
# domain, concentration, Pop, RR, MortRate, AgeStruc, and PWRR are all non-NA and
# the numeric inputs are finite. For a numeric column, non-NA and finite collapse
# to is.finite(); concentration needs no term of its own, because a missing
# concentration never matches the RR table, so RR is NA there.
# rr_list and pwr_list carry one element per value branch. RANGE passes all three
# so the mask is one shared pass: compute_group() filtered the joined frame once
# for every value column of a group.
mort_keep_mask <- function(rr_list, pwr_list, pop, domain, ag, mrate) {
  n <- nrow(rr_list[[1]])
  n_group <- ncol(rr_list[[1]])
  dom_code <- match(as.character(domain), colnames(pwr_list[[1]]))
  # TRUE where the grid belongs to a domain and has finite population.
  out <- matrix(!is.na(domain) & is.finite(pop), nrow = n, ncol = n_group)
  # Expand each PWRR row to grid rows once, then AND whole matrices: the branch
  # passes become full-matrix operations instead of per-group vector passes.
  for (i in seq_along(rr_list)) {
    out <- out & is.finite(rr_list[[i]]) &
      t(is.finite(pwr_list[[i]])[, dom_code, drop = FALSE])
  }
  out & t(is.finite(ag)) & t(is.finite(mrate))
}

# Wide grid result for one or more value branches: tibble(x, y, value columns).
# Rows are the union of the group masks in group order, which is the row set
# dplyr::full_join() produced when groups dropped different grids; groups that
# dropped a grid keep NA there.
# Every argument except groups/domain/grid_xy carries one element per branch, so
# RANGE runs one pass over the groups: the row-set union, the pop/ag/mrate
# subsetting, and the branch loop are shared. rr_r is the real-concentration RR
# matrix behind pwr; it is validated here and lets a caller rebuild the table.
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
  # Row set: the union of the group masks in group order.
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
    # One value buffer per group: every branch writes only the kept positions,
    # the rest stay NA for the branch that dropped the grid.
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

# Single-branch wrapper kept for callers that compute one value column set.
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
# left_join + hierarchical NA report.  breakdown lists the join levels to report
# (e.g. c("domain", "concentration")); levels absent from the joined frame are
# ignored, so a stale breakdown cannot abort a diagnostic call.
# Reports: L1 = level-1 value entirely missing → L2 = level-1/2 entirely missing
# → L3 = remaining level values missing.
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
  # The grid-level frame has domain and concentration but no endpoint/agegroup
  # columns; only levels present after the join can feed distinct().
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
    # L1 — level-1 values with zero matches
    na_lvl1 <- na_rows |> distinct(across(all_of(bd[1]))) |> pull(1)
    ok_lvl1 <- matched |> distinct(across(all_of(bd[1]))) |> pull(1)
    l1 <- setdiff(na_lvl1, ok_lvl1)
    # L2 / L3 — partially matching level-1 values
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
    # format
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

# ── diagnostic reports and input-identity cache (v6.3) ─────────────────────────
# Mortality() computed and printed five input diagnostics on every call. The
# decomposition cache bypasses Mortality() and, before v6.3 diagnostics, printed
# only its own clamp report, so the v6.2 diagnostics disappeared from the
# decomposition snapshots. The helpers below return the v6.2 text byte for byte
# and print nothing:
#   Mortality()               prints one copy on every call;
#   decomp_input_cache()      prints one copy per distinct text per run.
# The cache environment also memoises the two expensive input-identified steps:
# the RR key check (about 0.21 s per Mortality() call) and the coverage report
# (about 0.05 s). Keys use rlang::hash(), a tidyverse dependency, as the cheap
# content fingerprint; editing an input changes its hash and reruns the step.
# The cache lives in one R process and is never written to disk.

.mortality_cache <- new.env(parent = emptyenv())

# Clear the caches and their test counters.
mortality_cache_reset <- function() {
  .mortality_cache$rr_keys <- new.env(parent = emptyenv())
  .mortality_cache$coverage <- new.env(parent = emptyenv())
  .mortality_cache$rr_asserted <- 0L
  .mortality_cache$coverage_computed <- 0L
  .mortality_cache$coverage_reused <- 0L
  invisible(TRUE)
}

mortality_cache_reset()

# Identity of the standardised RR table: model name, row count, concentration
# precision, and a content hash of the five columns the key check reads. The
# model name falls back to a fixed label, never to NA: every unset name would
# otherwise share one cache entry.
mortality_rr_identity <- function(rr_tbl) {
  model <- get0(".CR_Model", envir = globalenv(), ifnotfound = NULL)
  model <- if (is.null(model) || length(model) != 1L || is.na(model)) {
    "<unset>"
  } else {
    as.character(model)
  }
  dgt <- attr(rr_tbl, "dgt_conc", exact = TRUE) %||%
    get0(".DGT_CONC", envir = globalenv(), ifnotfound = 1L) %||%
    1L
  str_c(
    model,
    nrow(rr_tbl),
    as.integer(dgt),
    rlang::hash(rr_tbl[c("concentration", "endpoint", "agegroup", "CI", "RR")]),
    sep = "::"
  )
}

# assert_unique_key() on the RR table, once per table identity. Mortality() and
# decomp_input_cache() both call this, so one table is scanned once per session
# even when both paths run.
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

# v6.2 clamp text; character(0) when every concentration is inside the range.
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

# Clamp one concentration vector to the RR range and describe the edit. The
# counts come back so a caller that feeds the same vector to both roles (the
# decomposition cache gives every scenario column to Conc_r and Conc_c) can print
# both labels without recounting.
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

# v6.2 text for grids in Grids that have no partner row. kind = "concentration"
# checks the grids of Conc_r, kind = "population" checks the grids of pop.
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

# v6.2 coverage text for one set of grid, rate, age, concentration, and
# population inputs. base_c follows Mortality()'s convention: the domain column is
# named "domain". Returns 0-2 strings: domains dropped from every group, then
# domains dropped from some groups.
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

# Identity of the report above. Only the parts the report reads enter the key:
# domain labels, the NA masks of concentration and population, and the distinct
# coverage sets of the rate and age tables. Concentration and population
# magnitudes never reach the text, so editing them does not invalidate the entry.
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

# Cached coverage report: computed once per input identity. The caller prints the
# returned strings on every call, so the text still appears once per Mortality()
# call.
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

# Gridded PM2.5 attributable mortality, PWRR-normalised.
#
# mRate is domain-level survey data, not a grid-level rate. Under the
# relative-risk principle I_g / I_domain = RR_e,a(C_g) / PWRR_domain,e,a, so the
# zero-PM2.5 baseline is uniform within a domain x endpoint x age group:
#   I_0 = I_domain / PWRR_domain,e,a
#   PWRR_domain,e,a = sum_g(Pop_g * RR_e,a(C_g)) / sum_g(Pop_g)
# Attributable deaths per grid against the TMREL:
#   Mort_g = Pop_g * AgeStruc_a * (I_g - I_0)
#          = Pop_g * AgeStruc_a * MortRate_domain,e,a * (RR_e,a(C_g) - 1)
#            / PWRR_domain,e,a
# AgeStruc is constant within a domain x age group, so it cancels from PWRR. One
# PWRR per domain, averaged across endpoints or ages, is invalid: it mixes curves
# with different RR levels and biases each endpoint in the opposite direction.
# CI = "RANGE" gives each branch its own PWRR (MEAN/UP/LOW). The grid-level PAF
# (RR_g - 1) / RR_g implies I_0 = MortRate / RR_g per grid; the PWRR route keeps
# one physically consistent I_0 per domain x endpoint x age group.
#
#   Grids    grid table with x, y, and the domain column.
#   Conc_r   real concentration, x/y/concentration; defines PWRR.
#   Conc_c   counterfactual concentration for the RR numerator; defaults to
#            Conc_r.
#   ag       age structure proportions by domain and age group.
#   mRate    baseline rates by domain, endpoint, and age group.
#   pop      grid population, x/y/Pop.
#   CI       "MEAN" (default), "UP", "LOW", or "RANGE".
#   domain   spatial column such as "Country"; NULL auto-detects it.
#   verbose  FALSE truncates domain names in warnings to 6; TRUE shows all.
#   debug    FALSE uses a plain left_join; TRUE adds join_report() NA
#            diagnostics.
#   validate_keys  TRUE (default) checks input keys for NA and duplicates; set
#            FALSE only for input already checked by read_files(). Column, RR
#            key, output key, precision, and unit checks stay active.
#
# Returns a wide frame with one column per endpoint-agegroup-CI per grid cell,
# suffixed _MEAN, _UP, _LOW when CI = "RANGE".
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

  # ---- Guard: required columns and key uniqueness ----
  require_columns(Grids, c("x", "y", domain), "Grids")
  require_columns(Conc_r, c("x", "y", "concentration"), "Conc_r")
  require_columns(Conc_c, c("x", "y", "concentration"), "Conc_c")
  require_columns(pop, c("x", "y", "Pop"), "pop")
  require_columns(mRate, c("domain", "endpoint", "agegroup", "MortRate"), "mRate")
  require_columns(ag, c("domain", "agegroup", "AgeStruc"), "ag")

  if (validate_keys) {
    assert_unique_key(Grids, c("x", "y"), "Grids")
    assert_unique_key(Conc_r, c("x", "y"), "Conc_r")
    # Conc_c defaults to Conc_r; one check covers both in that case.
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

  # ---- Get standardised RR table ----
  RR_raw <- .RR_std_tbl
  if (is.null(RR_raw) || nrow(RR_raw) == 0) {
    stop("RR lookup table is empty. Run set_Model() first.")
  }
  require_columns(
    RR_raw,
    c("concentration", "endpoint", "agegroup", "CI", "RR"),
    "RR lookup table"
  )

  conc_dgt <- get0(".DGT_CONC", envir = globalenv(), ifnotfound = 1)
  rr_dgt <- attr(RR_raw, "dgt_conc", exact = TRUE) %||% conc_dgt
  if (!identical(as.integer(conc_dgt), as.integer(rr_dgt))) {
    stop(
      "RR table precision (", rr_dgt, ") differs from .DGT_CONC (",
      conc_dgt, "). Rebuild the RR table with the requested precision."
    )
  }

  # RR table metadata wins over the globals so a hand-assigned table cannot run
  # with the unit or PWRR branch of a different model.
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

  # RR_std() returns canonical keys and a duplicate-free table, so tables built
  # by set_Model() skip the re-normalisation. Tables assigned by hand keep the
  # original path.
  if (!isTRUE(attr(RR_raw, "rr_normalized", exact = TRUE))) {
    RR_raw <- RR_raw |>
      mutate(
        concentration = matchable(as.numeric(concentration), conc_dgt),
        endpoint = tolower(endpoint),
        agegroup = matchable(as.numeric(agegroup), 0)
      )
  }
  # The key check is the largest fixed cost of a call (about 0.21 s), and
  # RR_std() output rarely changes, so run it once per table identity.
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

  # ---- Clamp concentrations to RR lookup table range ----
  clamp_r <- mortality_clamp_report(
    as.numeric(Conc_r$concentration),
    rr_conc_range,
    "Conc_r"
  )
  if (length(clamp_r$text) > 0) {
    log_msg(WARN, "{clamp_r$text}")
  }
  Conc_r <- Conc_r |>
    mutate(concentration = matchable(clamp_r$value, conc_dgt))

  clamp_c <- mortality_clamp_report(
    as.numeric(Conc_c$concentration),
    rr_conc_range,
    "Conc_c"
  )
  if (length(clamp_c$text) > 0) {
    log_msg(WARN, "{clamp_c$text}")
  }
  Conc_c <- Conc_c |>
    mutate(concentration = matchable(clamp_c$value, conc_dgt))

  # ---- Guard: coordinate overlap ----
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

  # ---- Base tables: one row per grid ----
  # base_c uses the counterfactual/real concentration for the RR numerator;
  # base_r uses the real concentration for PWRR.
  base_xy <- Grids |>
    select(x, y, domain = all_of(domain))
  base_c <- base_xy |>
    left_join(Conc_c, by = c("x", "y")) |>
    left_join(pop, by = c("x", "y"))
  base_r <- base_xy |>
    left_join(Conc_r, by = c("x", "y")) |>
    left_join(pop, by = c("x", "y"))

  # ---- Per endpoint × agegroup groups ----
  # RANGE covers every branch; a single CI covers only its own branch. Both key
  # sets and their order match the old RR_all |> distinct() (pivot_wider() keeps
  # the first-appearance order of the same keys).
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

  # ---- Report silent grid drops from the required-column filter ----
  # The filter below drops grid rows without a word. Two input causes are
  # reported here; values, row order, and the NA pattern stay unchanged. The
  # report is computed once per input identity and printed once per call.
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

  # ---- Compute: vectorised engine, or the join path for debug/diagnostics ----
  # debug = TRUE keeps the join chain so join_report() prints the same
  # diagnostics. That is the only caller of mortality_join_path(); it stays as
  # the permanent reference path.
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

# ── engine path (v6.3) ─────────────────────────────────────────────────────────
# One RR matrix per branch and one PWRR table per denominator branch, then one
# grid-table assembly per value branch. No dplyr join, no per-group filter.
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

  # Value columns per branch; PWRR denominator per value column: RANGE uses the
  # matching branch when pwr_branch = "branch", the MEAN branch otherwise, and a
  # single CI uses the MEAN branch when pwr_branch = "mean".
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

  # Conc_c defaults to Conc_r, so the two RR matrices are the same records.
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

  # compute_group() dropped a grid from every PWRR branch when any branch value
  # was NA at its concentration; mirror that shared row filter.
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

  # Age structure and mortality rate are domain-level. Expand them through one
  # small domain lookup instead of a per-group match() over every grid.
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

  # ---- Guard: PWRR must be finite and >= 1, in the join path's report order --
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
      # is.nan(), not is.na(): an NA cell means the domain has no usable grid,
      # which the join path never puts into pwr and so never reports. NaN is the
      # zero-weight domain it does report ("no valid PWRR").
      is_nan[, j] <- is.nan(one)
    }
    if (any(is_inf) || any(is_lt1)) {
      # t() + which() walks domain-major, branch-minor, as pivot_longer() did.
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

  # ---- Values: one shared keep mask and one pass over the branch set ----------
  keep <- mort_keep_mask(
    rr_num[value_branches],
    pwr_tbl[pwr_for_value],
    grid_pop,
    grid_domain,
    ag_mat,
    mr_mat
  )

  # One call, one pass over the endpoint/agegroup groups. Column order is
  # group-major, then the branch order, matching the join path.
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

# ── join path (pre-v6.3 reference) ─────────────────────────────────────────────
# Permanent debug = TRUE path: join_report() prints the per-join NA diagnostics
# that the engine does not reproduce. It is the original implementation: one
# dplyr join chain per endpoint/agegroup group plus a full_join() reduce when
# groups keep different grids.
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
  # Groups normally keep exactly the same (x, y) rows, so the wide result is a
  # column bind. Fall back to a full join when a group dropped or reordered rows.
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

# Auto-detect the PWRR domain column from Grids × mRate overlap.
# The defaults keep detect_domain() callable with no arguments (Mortality_at(),
# Decomposition() and aggregate_drivers()); Mortality() passes its own inputs so
# a stale global Grid_info/MortRate cannot change the domain column it uses.
detect_domain <- function(Grids = Grid_info, mRate = MortRate) {
  # Try each mRate column against each Grids non-coordinate column.
  # Pick the pair with the most overlapping unique values.
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

# Grid-level attributable mortality for one loaded year/scenario. Thin wrapper:
# pulls the loaded globals, auto-detects the PWRR domain, and calls Mortality().
#   at       year/scenario, e.g. "base2015".
#   CI       "MEAN" (default), "UP", "LOW", or "RANGE".
#   domain   grid domain column; NULL (default) auto-detects it from the
#            Grid_info x MortRate overlap.
#   verbose  FALSE truncates domain names in warnings to 6; TRUE shows all.
#   debug    FALSE uses a plain left_join; TRUE adds join_report() diagnostics.
#   validate_keys  TRUE (default) re-checks input keys; FALSE only after
#            read_files() validated them.
# Returns the Mortality() wide grid frame.
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
