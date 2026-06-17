# Mortality calculation module — PWRR-normalised grid-level attributable deaths
#   - Mortality()      — core formula: Mort = Pop × AgeStruc × MortRate × (RR−1) / PWRR
#   - Mortality_at()   — convenience wrapper for a given year/scenario
#   - detect_domain()  — auto-detect PWRR domain from MortRate × Grid_info overlap
#   - join_report()    — left_join with optional hierarchical NA diagnostic report
#   Modified 260618: three-phase PWRR guard (Inf/<1→stop, NA→skip, all-dropped→stop),
#     verbose/debug parameters, trunc_fmt accepts n=Inf for unbounded output.

# ── helpers ─────────────────────────────────────────────────────────────────────
trunc_fmt <- function(x, n = 6) {
  str_c(str_c(head(x, n), collapse = ", "), if (length(x) > n) " ..." else "")
}

# ── join_report ────────────────────────────────────────────────────────────────
# left_join + hierarchical NA report.  breakdown = c(domain, endpoint, agegroup).
# Reports: L1 = domain entirely missing → L2 = domain/endpoint missing → L3 = rest.
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
  for (col in new_cols) {
    na_rows <- result |> filter(.key_ok, is.na(.data[[col]]))
    n_na <- nrow(na_rows)
    if (n_na == 0) {
      next
    }
    if (is.null(breakdown) || length(breakdown) < 2) {
      log_msg(
        WARN,
        "{n_na} row(s) with NA in \"{col}\" after joining {label}. These rows will be dropped by na.omit."
      )
      next
    }
    bd <- breakdown
    matched <- result |> filter(.key_ok, !is.na(.data[[col]]))
    # L1 — domains with zero matches
    na_dom <- na_rows |> distinct(across(all_of(bd[1]))) |> pull(1)
    ok_dom <- matched |> distinct(across(all_of(bd[1]))) |> pull(1)
    l1 <- setdiff(na_dom, ok_dom)
    # L2 / L3 — partially-matching domains
    partial <- intersect(na_dom, ok_dom)
    if (length(partial) > 0 && length(bd) >= 3) {
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
    l2_label <- str_c(str_c(bd[1:2], collapse = "/"), "(s) entirely missing")
    l3_label <- str_c(str_c(bd, collapse = "/"), "(s) missing")
    parts <- character()
    if (length(l1) > 0) {
      parts <- c(
        parts,
        str_c(
          length(l1),
          " ",
          bd[1],
          "(s) entirely missing: ",
          trunc_fmt(l1, n = show_n)
        )
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
#' @param Grids grid information data frame with x, y, and domain columns
#' @param Conc_r data frame with columns x, y, concentration — real PM2.5, used for PWRR calculation
#' @param Conc_c data frame with columns x, y, concentration — counterfactual PM2.5 for the RR numerator. Defaults to \code{Conc_r}.
#' @param ag age structure proportions by domain and age group
#' @param mRate baseline mortality rates by domain, endpoint, and age group
#' @param pop population counts by grid (columns x, y, Pop)
#' @param CI RR branch: \code{"MEAN"} (default), \code{"UP"}, \code{"LOW"}, or \code{"RANGE"}
#' @param domain spatial aggregation column name (e.g. \code{"Country"}). Auto-detected when \code{NULL}.
#' @param verbose when \code{FALSE} (default), truncate domain names in warnings to 6; \code{TRUE} shows all
#' @param debug when \code{FALSE} (default), use plain \code{left_join}; \code{TRUE} enables \code{join_report()} NA diagnostics
#'
#' @return Wide data frame with one column per endpoint–agegroup–CI combination per grid cell.
#'   When \code{CI = "RANGE"}, columns are suffixed \verb{_MEAN}, \verb{_UP}, \verb{_LOW}.
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
  debug = FALSE
) {
  show_n <- if (verbose) Inf else 6
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

  # ---- Get standardised RR table ----
  # .RR_std_tbl: concentration, endpoint, agegroup, CI, RR (built by set_Model())
  RR_all <- .RR_std_tbl

  if (nrow(RR_all) == 0) {
    stop("RR lookup table is empty.")
  }

  rr_conc_range <- range(as.numeric(RR_all$concentration), na.rm = TRUE)

  if (CI == "RANGE") {
    # Pivot CI branches to columns: RR, RR_UP, RR_LOW
    RR_all <- RR_all |>
      pivot_wider(names_from = CI, values_from = RR) |>
      rename(RR = MEAN, RR_UP = UP, RR_LOW = LOW)
  } else {
    # Filter to single CI branch, drop CI column
    RR_all <- RR_all |>
      filter(CI == !!CI) |>
      select(-CI)
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
    orphan_conc_dom <- orphan_grids |>
      left_join(Grids, by = c("x", "y")) |>
      distinct(across(all_of(domain))) |>
      pull(1)
    log_msg(
      WARN,
      nrow(orphan_grids),
      " grid(s) in Grid_info have no matching concentration in Conc_r ",
      "({length(orphan_conc_dom)} domain(s): ",
      trunc_fmt(orphan_conc_dom, n = show_n),
      "). They will be dropped by na.omit."
    )
  }

  orphan_pop <- grid_xy |> anti_join(pop_xy, by = c("x", "y"))
  if (nrow(orphan_pop) > 0) {
    orphan_pop_dom <- orphan_pop |>
      left_join(Grids, by = c("x", "y")) |>
      distinct(across(all_of(domain))) |>
      pull(1)
    log_msg(
      WARN,
      nrow(orphan_pop),
      " grid(s) in Grid_info have no matching population in pop ",
      "({length(orphan_pop_dom)} domain(s): ",
      trunc_fmt(orphan_pop_dom, n = show_n),
      "). They will be dropped by na.omit."
    )
  }

  # ---- PWRR calculation (always uses MEAN RR) ----
  rr_for_pwr <- RR_all |> select(concentration, endpoint, agegroup, RR)

  PWRR_pre <- Grids |>
    left_join(Conc_r, by = c("x", "y")) |>
    left_join(pop, by = c("x", "y")) |>
    join_report(
      rr_for_pwr,
      "RR table (PWRR)",
      breakdown = c(domain, "concentration"),
      relationship = "many-to-many",
      verbose = verbose,
      debug = debug
    )
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
  # Phase 1: hard errors for genuinely invalid PWRR values
  bad_inf <- is.infinite(PWRR$PWRR)
  bad_lt1 <- PWRR$PWRR < 1 & !is.na(PWRR$PWRR)
  if (any(bad_inf) || any(bad_lt1)) {
    stop(
      "Invalid PWRR detected. ",
      if (any(bad_inf)) "Some domains have Inf PWRR. ",
      if (any(bad_lt1)) {
        "Some domains have PWRR < 1 (RR cannot be < 1 for PM2.5). "
      },
      "Check concentration and population data for these domains: ",
      paste(PWRR[[domain]][bad_inf | bad_lt1], collapse = ", ")
    )
  }

  # Phase 2: domains with NA PWRR have no valid grids after na.omit
  # (expected for remote territories with no PM2.5/population data).
  # Warn and remove them so the main calculation can proceed.
  na_pwrr <- is.na(PWRR$PWRR)
  if (any(na_pwrr)) {
    skipped <- PWRR[[domain]][na_pwrr]
    log_msg(
      WARN,
      "{length(skipped)} domain(s) have no valid grids after na.omit ",
      "(no PM2.5 or population data in these regions). ",
      "They will be excluded from the calculation: ",
      trunc_fmt(skipped, n = show_n)
    )
    PWRR <- PWRR |> filter(!na_pwrr)
  }

  if (nrow(PWRR) == 0) {
    stop(
      "PWRR step: all domains dropped. ",
      "Check that concentration values fall within the RR lookup table range, ",
      "and that Grid_info, Conc_r, and pop share common (x, y) coordinates."
    )
  }

  # ---- Main mortality calculation ----
  rr_table <- RR_all

  mort_data <- Grids |>
    left_join(Conc_c, by = c("x", "y")) |>
    left_join(pop, by = c("x", "y")) |>
    join_report(
      rr_table,
      "RR table",
      relationship = "many-to-many",
      verbose = verbose,
      debug = debug
    ) |>
    join_report(
      mRate |> rename({{ domain }} := domain),
      "mortality rate",
      breakdown = c(domain, "endpoint", "agegroup"),
      verbose = verbose,
      debug = debug
    ) |>
    join_report(
      ag |> rename({{ domain }} := domain),
      "age structure",
      breakdown = c(domain, "agegroup"),
      verbose = verbose,
      debug = debug
    ) |>
    join_report(
      PWRR,
      "PWRR",
      breakdown = c(domain, "endpoint"),
      verbose = verbose,
      debug = debug
    ) |>
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


#' Auto-detect the PWRR domain column
#'
#' Matches unique values in MortRate domain columns against Grid_info
#' non-coordinate columns. Returns the Grid_info column with the most
#' overlapping unique values.
#'
#' @return character, the name of the best-matching Grid_info column
#' @keywords internal
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

#' Calculate Attributable Mortality at a certain year/scenario
#'
#' Convenience wrapper that auto-detects the PWRR domain and calls
#' \code{Mortality()} with the currently loaded global data.
#'
#' @param at year/scenario
#' @param CI RR branch: \code{"MEAN"} (default), \code{"UP"}, \code{"LOW"}, or \code{"RANGE"}
#' @param domain grid domain column. Auto-detected from Grid_info and MortRate
#'   overlap when \code{NULL} (default).
#' @param verbose when \code{FALSE} (default), truncate domain names in warnings to 6; \code{TRUE} shows all
#' @param debug when \code{FALSE} (default), use plain \code{left_join}; \code{TRUE} enables \code{join_report()} diagnostics
#'
#' @return data frame of grid-level mortality estimates
#' @export
#'
#' @examples
#' grid_mean <- Mortality_at(at = "base2015", CI = "MEAN")
Mortality_at <- function(at, CI = "MEAN", domain = NULL, verbose = FALSE, debug = FALSE) {
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
    debug = debug
  )
}
