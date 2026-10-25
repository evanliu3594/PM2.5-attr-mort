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
#'   I_g / I_domain = RR_e,a(C_g) / PWRR_domain,e,a
#'
#' where `I_g` is the mortality rate at grid concentration C_g, and `I_domain` is the
#' observed domain-average rate for endpoint e and age group a. This gives a uniform
#' zero-PM2.5 baseline within each domain × endpoint × age group:
#'
#'   I_0 = I_domain / PWRR_domain,e,a    (same for all grids in the domain)
#'
#'   PWRR_domain,e,a = Σ_g Pop_g × RR_e,a(C_g) / Σ_g Pop_g
#'
#' Attributable mortality for grid g vs. zero-PM2.5 (TMREL):
#'
#'   ΔMort_g = Pop_g × AgeStruc_a × (I_g − I_0)
#'           = Pop_g × AgeStruc_a × MortRate_domain,e,a × (RR_e,a(C_g) − 1) / PWRR_domain,e,a
#'
#' Age structure is constant within domain × age group, so it cancels from PWRR.
#' A single PWRR per domain (averaged across endpoints/ages) is invalid: it mixes
#' curves with different RR levels and biases each endpoint in opposite directions.
#' For `CI = "RANGE"`, each branch uses its own PWRR (MEAN/UP/LOW).
#'
#' Compared to the standard grid-level PAF `(RR_g−1)/RR_g`, which implies I_0 = MortRate/RR_g
#' (varying per grid), the PWRR approach gives a physically consistent I_0 within each
#' domain × endpoint × age group.
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
  RR_all <- .RR_std_tbl

  if (nrow(RR_all) == 0) {
    stop("RR lookup table is empty.")
  }

  rr_conc_range <- range(as.numeric(RR_all$concentration), na.rm = TRUE)

  if (CI == "RANGE") {
    RR_all <- RR_all |>
      pivot_wider(names_from = CI, values_from = RR) |>
      rename(RR = MEAN, RR_UP = UP, RR_LOW = LOW)
  } else {
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

  # ---- Base tables: one row per grid ----
  # base_c uses the counterfactual/real concentration for the RR numerator;
  # base_r uses the real concentration for PWRR.
  base_c <- Grids |>
    rename(domain = !!sym(domain)) |>
    left_join(Conc_c, by = c("x", "y")) |>
    left_join(pop, by = c("x", "y"))

  base_r <- Grids |>
    rename(domain = !!sym(domain)) |>
    left_join(Conc_r, by = c("x", "y")) |>
    left_join(pop, by = c("x", "y"))

  # ---- Per endpoint × agegroup groups ----
  # One group at a time keeps the long table at grid level (not grid × endpoint
  # × agegroup). compute_group() is side-effect free; replace map2() with
  # future_map2() to parallelize one endpoint-agegroup per worker.
  groups <- RR_all |> distinct(endpoint, agegroup)

  if (nrow(groups) == 0) {
    stop("RR lookup table has no endpoint-agegroup groups.")
  }

  pwr_cols <- if (CI == "RANGE") c("RR", "RR_UP", "RR_LOW") else "RR"
  pwr_value_cols <- if (CI == "RANGE") {
    c("PWRR_MEAN", "PWRR_UP", "PWRR_LOW")
  } else {
    "PWRR"
  }

  compute_group <- function(ep, age) {
    rr_g <- RR_all |>
      filter(endpoint == ep, agegroup == age) |>
      select(concentration, all_of(pwr_cols))

    pwr <- base_r |>
      left_join(rr_g, by = "concentration", relationship = "many-to-one") |>
      drop_na() |>
      group_by(domain) |>
      summarise(
        across(
          all_of(pwr_cols),
          ~ weighted.mean(.x, Pop, na.rm = TRUE),
          .names = "PWRR_{.col}"
        ),
        .groups = "drop"
      )

    if (CI == "RANGE") {
      pwr <- pwr |>
        rename(
          PWRR_MEAN = PWRR_RR,
          PWRR_UP = PWRR_RR_UP,
          PWRR_LOW = PWRR_RR_LOW
        )
    } else {
      pwr <- pwr |> rename(PWRR = PWRR_RR)
    }

    pwr_long <- pwr |>
      pivot_longer(
        all_of(pwr_value_cols),
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
        filter(if_all(all_of(pwr_value_cols), ~ !is.na(.x)))
    }

    mrate_g <- mRate |> filter(endpoint == ep, agegroup == age)
    ag_g <- ag |> filter(agegroup == age)

    value_cols <- if (CI == "RANGE") {
      c(
        str_c(ep, age, "MEAN", sep = "_"),
        str_c(ep, age, "UP", sep = "_"),
        str_c(ep, age, "LOW", sep = "_")
      )
    } else {
      str_c(ep, age, CI, sep = "_")
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
        breakdown = c("domain", "endpoint", "agegroup"),
        verbose = verbose,
        debug = debug
      ) |>
      join_report(
        ag_g,
        "age structure",
        breakdown = c("domain", "agegroup"),
        verbose = verbose,
        debug = debug
      ) |>
      join_report(
        pwr,
        "PWRR",
        breakdown = c("domain", "endpoint", "agegroup"),
        verbose = verbose,
        debug = debug
      ) |>
      drop_na()

    if (nrow(mort) == 0) {
      empty <- tibble(x = character(), y = character())
      for (col in value_cols) {
        empty[[col]] <- numeric()
      }
      return(list(data = empty, skipped = skipped))
    }

    if (CI == "RANGE") {
      mort <- mort |>
        mutate(
          !!sym(value_cols[1]) :=
            Pop * AgeStruc * MortRate * (RR - 1) / PWRR_MEAN / 1e5,
          !!sym(value_cols[2]) :=
            Pop * AgeStruc * MortRate * (RR_UP - 1) / PWRR_UP / 1e5,
          !!sym(value_cols[3]) :=
            Pop * AgeStruc * MortRate * (RR_LOW - 1) / PWRR_LOW / 1e5
        ) |>
        select(x, y, all_of(value_cols))
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

  group_results <- map2(groups$endpoint, groups$agegroup, compute_group)

  skipped_keys <- group_results |>
    map("skipped") |>
    unlist(use.names = FALSE) |>
    unique()

  if (length(skipped_keys) > 0) {
    log_msg(
      WARN,
      "{length(skipped_keys)} domain/endpoint/age combination(s) have no valid ",
      "PWRR after na.omit; affected rows will be dropped: ",
      trunc_fmt(skipped_keys, n = show_n)
    )
  }

  data_list <- map(group_results, "data")
  result <- reduce(data_list, full_join, by = c("x", "y"))

  if (nrow(result) == 0) {
    stop("Mortality result has 0 rows. Check pivot_wider inputs.")
  }

  return(result)
}

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
