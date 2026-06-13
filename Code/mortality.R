# Mortality calculation module — PWRR-normalised grid-level attributable deaths
#   - Mortality()      — core formula: Mort = Pop × AgeStruc × MortRate × (RR−1) / PWRR
#   - Mortality_at()   — convenience wrapper for a given year/scenario
#   - detect_domain()  — auto-detect PWRR domain from MortRate × Grid_info overlap
#   Modified 260610-260613: CI=RANGE mode (single-pass MEAN+UP+LOW), _MEAN/_UP/_LOW
#     column suffixes, RR→CI rename, concentration clamping, auto domain detection,
#     unified mort_data construction with pivot_branch().

#' Calculate gridded PM2.5 attributed mortality
#'
#' **PAF methodology — PWRR normalization:**
#'
#' Baseline mortality rates (`mRate`) are domain-level survey data (e.g., national or
#' provincial statistics), not grid-specific rates. Under the relative-risk principle:
#'
#'   I_g / I_domain = RR(C_g) / PWRR_domain
#'
#' where `I_g` is the mortality rate at grid concentration C_g, and `I_domain` is the
#' observed domain-average rate. This gives a uniform zero-PM2.5 baseline within each domain:
#'
#'   I_0 = I_domain / PWRR_domain    (same for all grids in the domain)
#'
#' Attributable mortality for grid g vs. zero-PM2.5 (TMREL):
#'
#'   ΔMort_g = Pop_g × AgeStruc × (I_g − I_0)
#'           = Pop_g × AgeStruc × MortRate_domain × (RR(C_g) − 1) / PWRR_domain
#'
#' Compared to the standard grid-level PAF `(RR_g−1)/RR_g`, which implies I_0 = MortRate/RR_g
#' (varying per grid), the PWRR approach gives a physically consistent I_0 within each domain.
#'
#' @param Grids a vector of grid coords
#' @param Conc_r a 3-column `data.frame` stores real PM2.5 concentration of each grid, used for PWRR calculation
#' @param Conc_c a 3-column `data.frame` stores counterfactual PM2.5 concentration of each grid, used for the RR numerator. Defaults to Conc_r
#' @param ag proportions of 20 age-groups inside the population structure
#' @param mRate the domain-level baseline mortality rates per endpoint and age group
#' @param pop a 3-column dataframe stores population volume of each grid
#' @param CI RR branch: "MEAN" (default), "UP", "LOW", or "RANGE"
#' @param domain the spatial aggregation level (e.g., "Country", "Province") for PWRR computation
#'
#' @return When CI is "MEAN"/"UP"/"LOW", a wide table of death estimates per endpoint-agegroup per grid.
#'   When CI is "RANGE", the table also includes _UP and _LOW suffixed columns for the 95% CI bounds.
#'
#' @examples
Mortality <- function(
  Grids,
  Conc_r,
  Conc_c = NULL,
  ag,
  mRate,
  pop,
  CI = "MEAN",
  domain = NULL
) {
  # ---- Guard: input presence ----
  if (is.null(Conc_c)) {
    Conc_c <- Conc_r
  }

  if (is.null(domain)) {
    domain <- detect_domain()
  }

  # ---- Guard: domain column exists ----
  if (!all(domain %in% names(Grids))) {
    stop(
      "Domain column \"",
      paste(setdiff(domain, names(Grids)), collapse = "\", \""),
      "\" not found in Grid_info. Available: ",
      paste(names(Grids), collapse = ", ")
    )
  }

  # ---- Build RR table(s) ----
  if (CI == "RANGE") {
    # Pre-combine MEAN/UP/LOW into one table for a single join pass.
    # PWRR is always computed from MEAN RR.
    RR_all <- RR_std("MEAN") |>
      rename(RR = RR) |>
      left_join(
        RR_std("UP") |> rename(RR_UP = RR),
        by = c("concentration", "endpoint", "agegroup")
      ) |>
      left_join(
        RR_std("LOW") |> rename(RR_LOW = RR),
        by = c("concentration", "endpoint", "agegroup")
      )

    if (nrow(RR_all) == 0) {
      stop("Combined RR lookup table (MEAN+UP+LOW) is empty.")
    }

    rr_conc_range <- range(as.numeric(RR_all$concentration), na.rm = TRUE)
  } else {
    # Single RR branch
    RR_tbl <- RR_std(CI)

    if (nrow(RR_tbl) == 0) {
      stop(
        "RR lookup table is empty for index \"",
        CI,
        "\". Check that RR_table[[\"",
        CI,
        "\"]] exists and contains data."
      )
    }

    rr_conc_range <- range(as.numeric(RR_tbl$concentration), na.rm = TRUE)
  }

  # ---- Clamp concentrations to RR lookup table range ----
  conc_range_r <- range(as.numeric(Conc_r$concentration), na.rm = TRUE)
  conc_range_c <- range(as.numeric(Conc_c$concentration), na.rm = TRUE)

  clamp_conc <- function(conc_vec, rr_range, label) {
    n_lo <- sum(conc_vec < rr_range[1], na.rm = TRUE)
    n_hi <- sum(conc_vec > rr_range[2], na.rm = TRUE)
    if (n_lo + n_hi > 0) {
      conc_vec <- pmax(conc_vec, rr_range[1])
      conc_vec <- pmin(conc_vec, rr_range[2])
      log_msg(
        WARN,
        n_lo + n_hi,
        " grids in ",
        label,
        " have concentration outside RR lookup range [",
        rr_range[1],
        ", ",
        rr_range[2],
        "]. ",
        "Clamped to nearest boundary (",
        n_lo,
        " low, ",
        n_hi,
        " high)."
      )
    }
    conc_vec
  }

  Conc_r <- Conc_r |>
    mutate(
      concentration = clamp_conc(
        as.numeric(concentration),
        rr_conc_range,
        "Conc_r"
      ) |>
        matchable(1)
    )
  Conc_c <- Conc_c |>
    mutate(
      concentration = clamp_conc(
        as.numeric(concentration),
        rr_conc_range,
        "Conc_c"
      ) |>
        matchable(1)
    )

  # ---- Guard: coordinate overlap ----
  grid_xy <- Grids |> select(x, y) |> distinct()
  conc_xy <- Conc_r |> select(x, y) |> distinct()
  pop_xy <- pop |> select(x, y) |> distinct()

  orphan_grids <- grid_xy |> anti_join(conc_xy, by = c("x", "y"))
  if (nrow(orphan_grids) > 0) {
    log_msg(
      WARN,
      nrow(orphan_grids),
      " grid(s) in Grid_info have no matching concentration in Conc_r. ",
      "They will be dropped by na.omit."
    )
  }

  orphan_pop <- grid_xy |> anti_join(pop_xy, by = c("x", "y"))
  if (nrow(orphan_pop) > 0) {
    log_msg(
      WARN,
      nrow(orphan_pop),
      " grid(s) in Grid_info have no matching population in pop. ",
      "They will be dropped by na.omit."
    )
  }

  # ---- PWRR calculation (always uses MEAN RR) ----
  rr_for_pwr <- if (CI == "RANGE") {
    RR_all |> select(concentration, endpoint, agegroup, RR)
  } else {
    RR_tbl
  }

  PWRR_pre <- list(Grids, Conc_r, pop, rr_for_pwr) |> reduce(left_join)
  n_before <- nrow(PWRR_pre)
  PWRR_data <- PWRR_pre |> na.omit()
  n_after <- nrow(PWRR_data)

  if (n_after == 0) {
    stop(
      "PWRR step: all ",
      n_before,
      " rows dropped by na.omit. ",
      "Check that concentration values fall within the RR lookup table range, ",
      "and that Grid_info, Conc_r, and pop share common (x, y) coordinates."
    )
  }

  if (n_after < n_before * 0.5) {
    log_msg(
      WARN,
      "PWRR step: ",
      n_before - n_after,
      " of ",
      n_before,
      " rows (",
      round(100 * (1 - n_after / n_before)),
      "%) dropped by na.omit. ",
      "Check concentration range vs RR lookup and coordinate consistency."
    )
  }

  PWRR <- PWRR_data |>
    group_by(pick(all_of(domain))) |>
    summarise(PWRR = weighted.mean(RR, Pop, na.rm = TRUE), .groups = "drop")

  # ---- Guard: PWRR validity ----
  if (
    any(is.na(PWRR$PWRR)) || any(is.infinite(PWRR$PWRR)) || any(PWRR$PWRR < 1)
  ) {
    stop(
      "Invalid PWRR detected. ",
      if (any(is.na(PWRR$PWRR))) {
        "Some domains have NA PWRR (no valid grids after na.omit). "
      },
      if (any(is.infinite(PWRR$PWRR))) "Some domains have Inf PWRR. ",
      if (any(PWRR$PWRR < 1)) {
        "Some domains have PWRR < 1 (RR cannot be < 1 for PM2.5). "
      },
      "Check concentration and population data for these domains: ",
      paste(
        PWRR[[domain]][
          is.na(PWRR$PWRR) | is.infinite(PWRR$PWRR) | PWRR$PWRR < 1
        ],
        collapse = ", "
      )
    )
  }

  # ---- Main mortality calculation ----
  # RR table to use: combined (RANGE) or single branch
  rr_table <- if (CI == "RANGE") RR_all else RR_tbl

  mort_data <- list(
    Grids,
    Conc_c,
    pop,
    rr_table,
    mRate |> rename({{ domain }} := domain),
    ag |> rename({{ domain }} := domain),
    PWRR
  ) |>
    reduce(left_join) |>
    na.omit()

  if (nrow(mort_data) == 0) {
    stop(
      "Mortality step: all rows dropped by na.omit. ",
      "Check age groups, endpoint names, and domain values in input data."
    )
  }

  if (CI == "RANGE") {
    # Pivot each branch separately, then join on x, y
    mort_long <- mort_data |>
      mutate(
        Mort = Pop * AgeStruc * MortRate * (RR - 1) / PWRR / 1e5,
        Mort_UP = Pop * AgeStruc * MortRate * (RR_UP - 1) / PWRR / 1e5,
        Mort_LOW = Pop * AgeStruc * MortRate * (RR_LOW - 1) / PWRR / 1e5
      ) |>
      select(x, y, endpoint, agegroup, Mort, Mort_UP, Mort_LOW)

    pivot_branch <- function(data, value_col, suffix) {
      data |>
        select(x, y, endpoint, agegroup, {{ value_col }}) |>
        pivot_wider(
          names_from = c('endpoint', 'agegroup'),
          names_sep = '_',
          values_from = {{ value_col }}
        ) |>
        rename_with(~ str_c(.x, suffix), matches('_[0-9]+$'))
    }

    result <- pivot_branch(mort_long, Mort, "_MEAN") |>
      left_join(pivot_branch(mort_long, Mort_UP, "_UP"), by = c("x", "y")) |>
      left_join(pivot_branch(mort_long, Mort_LOW, "_LOW"), by = c("x", "y"))
  } else {
    result <- mort_data |>
      mutate(Mort = Pop * AgeStruc * MortRate * (RR - 1) / PWRR / 1e5) |>
      select(x, y, endpoint, agegroup, Mort) |>
      pivot_wider(
        names_from = c('endpoint', 'agegroup'),
        names_sep = '_',
        values_from = 'Mort'
      ) |>
      rename_with(~ str_c(.x, "_", CI), matches('_[0-9]+$'))
  }

  # ---- Guard: result not empty ----
  if (nrow(result) == 0) {
    stop("Mortality result has 0 rows. Check pivot_wider inputs.")
  }

  return(result)
}


#' Calculate Attributable Mortality at a certain year/scenario
#'
#' @param at year/scenario
#' @param CI RR branch: "MEAN" (default), "UP", "LOW", or "RANGE"
#' @param domain grid domain
#'
#' @return data frame of grid-level mortality estimates
#' @export
#'
#' @examples
# Auto-detect the PWRR domain by matching MortRate domain values against
# Grid_info columns. Returns the Grid_info column with the best overlap.
detect_domain <- function() {
  # Try each MortRate column against each Grid_info non-coordinate column.
  # Pick the pair with the most overlapping unique values.
  geo_candidates <- setdiff(names(Grid_info), c("x", "y"))
  mort_candidates <- setdiff(names(MortRate), c("endpoint", "agegroup"))

  best_geo <- NULL
  best_mort <- NULL
  best_hit <- 0

  for (gcol in geo_candidates) {
    geo_vals <- unique(Grid_info[[gcol]])
    for (mcol in mort_candidates) {
      mort_vals <- unique(MortRate[[mcol]])
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

  n_mort <- length(unique(MortRate[[best_mort]]))
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

Mortality_at <- function(at, CI = "MEAN", domain = NULL) {
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
    domain = domain
  )
}

#' Uncertainties Calculation
#'
#' @param m_Rate 死亡率数据
#' @param PWE 分地区人口加权浓度
#' @param aggr_pop 分区汇总人口
#' @param age_struc 分区年龄结构
#' @param includeConc 浓度不确定性开关
#' @param Conc_ERR 浓度数据相对不确定性（百分比），默认 12 表示 +/-12%
#'
#' @export
#'
#' @return 返回对应模式不同终端的汇总不确定性范围
#'
#' @examples
