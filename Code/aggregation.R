# Aggregation & output module
#   - aggregate_sigma()  — total attributable mortality plus PWRR-consistent
#                         uncertainty bounds derived from Mortality() branches.
#   - aggregate_range()  — aggregate Mortality() output by geography and
#                         endpoint/agegroup, preserving NA mortality.
#   Modified 261026b: aggregation now sums only mortality columns. Missing
#     components remain NA. Numeric metadata no longer enters totals.
#     aggregate_sigma() uses the PWRR estimator and supports total only.
#   Modified 261101: aggregate_sigma() uses caller-supplied _UP/_LOW columns,
#     computes character scenarios with one RANGE call, takes an explicit PWRR
#     domain (never the spatial column at), and validates the ... arguments.
#   Modified 261101b (v6.3): aggregate_range() builds one long table per call
#     and sums it with grouped R-level sum() instead of dplyr group_by() per
#     aggregation level. Sums stay in long-table row order, so results are
#     bit-identical to the previous implementation; the grid table must be
#     unique on (x, y), as read_files() enforces.

# ---------- shared validation ----------

mortality_columns <- function(df, label = "Mortality output") {
  cols <- str_subset(names(df), "^.+_[0-9]+_(MEAN|UP|LOW)$")
  if (length(cols) == 0) {
    cli::cli_abort(
      "{label} has no mortality columns matching endpoint_agegroup_BRANCH."
    )
  }

  parsed <- str_match(cols, "^(.*)_([0-9]+)_(MEAN|UP|LOW)$")
  tibble(
    column = cols,
    endpoint = parsed[, 2],
    agegroup = parsed[, 3],
    CI = parsed[, 4]
  )
}

normalise_mortality_list <- function(x) {
  if (is.data.frame(x)) {
    return(list(scenario = x))
  }

  if (!is.list(x) || length(x) == 0) {
    cli::cli_abort("x must be a data frame or a non-empty named list.")
  }

  if (!all(map_lgl(x, is.data.frame))) {
    cli::cli_abort("Every element of x must be a data frame.")
  }

  scenario_names <- names(x)
  if (is.null(scenario_names) || any(!nzchar(scenario_names))) {
    cli::cli_abort("x must be a named list with one name per scenario.")
  }
  if (anyDuplicated(scenario_names)) {
    cli::cli_abort("Scenario names in x must be unique.")
  }

  first_names <- names(x[[1]])
  for (scenario in scenario_names) {
    if (!identical(names(x[[scenario]]), first_names)) {
      cli::cli_abort(
        "Scenario {.val {scenario}} has different columns from the first scenario."
      )
    }
    if (anyDuplicated(x[[scenario]][c("x", "y")])) {
      cli::cli_abort(
        "Scenario {.val {scenario}} has duplicated (x, y) keys."
      )
    }
  }

  x
}

domain_columns <- function(at_cols) {
  missing_cols <- setdiff(at_cols, names(Grid_info))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "Aggregation columns not found in Grid_info: {.val {missing_cols}}."
    )
  }
  at_cols
}

# ---------- aggregate_sigma ----------

#' Aggregate total mortality with PWRR-consistent uncertainty
#'
#' Computes the total attributable mortality and uncertainty bounds for one
#' spatial domain column. The bounds combine RR-branch differences from
#' `Mortality(CI = "RANGE")` with optional concentration uncertainty. Both
#' components use the same PWRR-normalised estimator as `Mortality()`.
#'
#' `by` is restricted to `"total"`. Use [aggregate_range()] for endpoint or
#' agegroup breakdowns.
#'
#' @param full_result A named list of grid-level mortality data frames, or a
#'   character vector of scenario names. List elements must carry the full
#'   RANGE set (`_MEAN`, `_UP`, `_LOW`); the supplied `_UP`/`_LOW` columns are
#'   used as-is for the bounds. A `_MEAN`-only element aborts, because bounds
#'   cannot be derived from it: pass scenario names instead so the bounds are
#'   computed from the loaded session.
#' @param at A single Grid_info column used for spatial aggregation.
#' @param by Must be `"total"`.
#' @param write Write the result to `./Results/`.
#' @param pwr_domain PWRR domain column passed to `Mortality()` and
#'   `Mortality_at()`. `NULL` (default) auto-detects it. It is never `at`:
#'   `at` only controls spatial aggregation.
#' @param ... Named extras only: `includeConc` (logical; adds a concentration
#'   perturbation computed from the loaded scenario data), `Conc_ERR` (percent
#'   concentration perturbation in `[0, 100]`), and `verbose` (logical).
#'   Unknown names abort.
#'
#' @return A data frame with `year`, the domain column, `total_MEAN`, `total_UP`,
#'   and `total_LOW`.
#' @export
aggregate_sigma <- function(
  full_result,
  at = "Country",
  by = "total",
  write = TRUE,
  pwr_domain = NULL,
  ...
) {
  if (!identical(by, "total")) {
    cli::cli_abort(
      "aggregate_sigma() supports by = 'total' only. ",
      "Use aggregate_range() for endpoint or agegroup breakdowns."
    )
  }
  if (length(at) != 1 || identical(at, "grid")) {
    cli::cli_abort(
      "aggregate_sigma() requires one spatial column in at, such as 'Country'."
    )
  }
  domain_columns(at)

  # Named extras only: a misspelled or unnamed `...` would silently fall back
  # to the defaults and change the result.
  extra_args <- list(...)
  allowed_args <- c("includeConc", "Conc_ERR", "verbose")
  if (length(extra_args) > 0) {
    extra_names <- names(extra_args)
    if (is.null(extra_names) || any(!nzchar(extra_names))) {
      cli::cli_abort("All arguments in ... must be named: {.val {allowed_args}}.")
    }
    unknown_args <- setdiff(extra_names, allowed_args)
    if (length(unknown_args) > 0) {
      cli::cli_abort(c(
        "Unknown argument(s) in ...: {.val {unknown_args}}.",
        "i" = "Supported: {.val {allowed_args}}."
      ))
    }
  }
  include_conc <- extra_args[["includeConc"]] %||% FALSE
  conc_err <- extra_args[["Conc_ERR"]] %||% 12
  verbose <- extra_args[["verbose"]] %||% FALSE

  if (!is.logical(include_conc) || length(include_conc) != 1 || is.na(include_conc)) {
    cli::cli_abort(
      "includeConc must be one TRUE/FALSE value, got {.val {include_conc}}."
    )
  }
  conc_err_ok <- is.numeric(conc_err) && length(conc_err) == 1 &&
    !is.na(conc_err) && is.finite(conc_err) &&
    conc_err >= 0 && conc_err <= 100
  if (!conc_err_ok) {
    cli::cli_abort(
      "Conc_ERR must be one number between 0 and 100 (percent), ",
      "got {.val {conc_err}}."
    )
  }
  if (!is.logical(verbose) || length(verbose) != 1 || is.na(verbose)) {
    cli::cli_abort("verbose must be one TRUE/FALSE value, got {.val {verbose}}.")
  }

  if (is.character(full_result)) {
    scenarios <- full_result
    if (length(scenarios) == 0 || any(!nzchar(scenarios)) ||
      anyDuplicated(scenarios) > 0) {
      cli::cli_abort("Scenario names must be non-empty and unique.")
    }
    compute_bounds <- TRUE
  } else if (is.list(full_result) && !is.data.frame(full_result)) {
    scenarios <- names(full_result)
    if (is.null(scenarios) || any(!nzchar(scenarios))) {
      cli::cli_abort("full_result must be a named list.")
    }
    if (anyDuplicated(scenarios) > 0) {
      cli::cli_abort("Scenario names in full_result must be unique.")
    }
    if (!all(map_lgl(full_result, is.data.frame))) {
      cli::cli_abort("Every element of full_result must be a data frame.")
    }
    compute_bounds <- FALSE
  } else {
    cli::cli_abort(
      "full_result must be a character vector or a named list of data frames."
    )
  }

  log_msg(
    INFO,
    "aggregate_sigma: at = \"{at}\", {length(scenarios)} scenario(s)"
  )
  if (include_conc) {
    log_msg(INFO, "Including concentration uncertainty at +/-{conc_err}%.")
  }

  # Split one grid table into its MEAN/UP/LOW branches. Supplied UP/LOW columns
  # are authoritative; no Mortality() call is made for list input.
  extract_branches <- function(df, scenario) {
    if (!all(c("x", "y") %in% names(df))) {
      cli::cli_abort("Scenario {.val {scenario}} must contain x and y columns.")
    }
    mean_cols <- str_subset(names(df), "_MEAN$")
    up_cols <- str_subset(names(df), "_UP$")
    low_cols <- str_subset(names(df), "_LOW$")
    if (length(mean_cols) == 0) {
      cli::cli_abort("Scenario {.val {scenario}} has no MEAN mortality columns.")
    }
    if (length(up_cols) == 0 && length(low_cols) == 0) {
      cli::cli_abort(c(
        "Scenario {.val {scenario}} has MEAN columns only.",
        "i" = paste0(
          "The uncertainty bounds are not in the data frame. ",
          "Pass Mortality(CI = \"RANGE\") output, or pass scenario names ",
          "as a character vector so the bounds are computed from the session."
        )
      ))
    }
    if (length(up_cols) == 0 || length(low_cols) == 0) {
      cli::cli_abort(
        "Scenario {.val {scenario}} must contain both _UP and _LOW columns, ",
        "or neither."
      )
    }
    list(
      mean = df |> select(x, y, all_of(mean_cols)),
      up = df |> select(x, y, all_of(up_cols)),
      low = df |> select(x, y, all_of(low_cols))
    )
  }

  aggregate_total <- function(df, label) {
    out <- aggregate_range(
      list(scenario = df),
      at = at,
      by = "total",
      write = FALSE
    )[[1]]
    value_cols <- setdiff(names(out), at)
    if (length(value_cols) != 1) {
      cli::cli_abort("Could not identify the aggregated mortality column for {label}.")
    }
    out |>
      select(all_of(at), total := all_of(value_cols))
  }

  rows <- map(scenarios, function(scenario) {
    grids <- if (compute_bounds) {
      Mortality_at(at = scenario, CI = "RANGE", domain = pwr_domain)
    } else {
      full_result[[scenario]]
    }
    branches <- extract_branches(grids, scenario)

    mean_total <- aggregate_total(
      branches$mean,
      str_c(scenario, " MEAN")
    ) |>
      rename(total_MEAN = total)
    up_total <- aggregate_total(branches$up, str_c(scenario, " UP")) |>
      rename(total_UP_raw = total)
    low_total <- aggregate_total(branches$low, str_c(scenario, " LOW")) |>
      rename(total_LOW_raw = total)

    out <- mean_total |>
      left_join(up_total, by = at) |>
      left_join(low_total, by = at) |>
      mutate(
        ci_up_cr = abs(total_UP_raw - total_MEAN),
        ci_low_cr = abs(total_MEAN - total_LOW_raw)
      )

    if (include_conc) {
      conc <- getConc_real(scenario) |>
        mutate(concentration = as.numeric(concentration))
      conc_up <- conc |>
        mutate(concentration = concentration * (1 + conc_err / 100))
      conc_low <- conc |>
        mutate(concentration = concentration * (1 - conc_err / 100))

      run_perturbed <- function(conc_data) {
        Mortality(
          Grids = Grid_info,
          Conc_r = conc_data,
          Conc_c = NULL,
          pop = getPop(scenario),
          ag = getAgeGroup(scenario),
          mRate = getMortRate(scenario),
          CI = "MEAN",
          domain = pwr_domain,
          verbose = verbose
        )
      }

      conc_up_total <- run_perturbed(conc_up) |>
        aggregate_total(str_c(scenario, " concentration UP")) |>
        rename(total_UP_conc = total)
      conc_low_total <- run_perturbed(conc_low) |>
        aggregate_total(str_c(scenario, " concentration LOW")) |>
        rename(total_LOW_conc = total)

      out <- out |>
        left_join(conc_up_total, by = at) |>
        left_join(conc_low_total, by = at) |>
        mutate(
          ci_up_conc = abs(total_UP_conc - total_MEAN),
          ci_low_conc = abs(total_LOW_conc - total_MEAN)
        )
    } else {
      out <- out |>
        mutate(ci_up_conc = 0, ci_low_conc = 0)
    }

    out |>
      mutate(
        total_UP = total_MEAN + sqrt(ci_up_cr^2 + ci_up_conc^2),
        total_LOW = total_MEAN - sqrt(ci_low_cr^2 + ci_low_conc^2)
      ) |>
      select(all_of(at), total_MEAN, total_UP, total_LOW) |>
      add_column(year = scenario, .before = 1)
  }) |>
    bind_rows()

  if (nrow(rows) == 0) {
    cli::cli_abort("Uncertainty aggregation produced 0 rows.")
  }

  if (write) {
    outpath <- str_glue(
      "./Results/{tell_Model()}_{at}_bytotal_sigma_",
      "{head(scenarios, 1)}-{tail(scenarios, 1)}_",
      "Build{format(Sys.Date(), '%y%m%d')}.xlsx"
    )
    dir.create("./Results", showWarnings = FALSE, recursive = TRUE)
    writexl::write_xlsx(rows, outpath)
    log_msg(INFO, "  Written: ", outpath)
  }

  rows
}

# ---------- aggregate_range ----------

#' Aggregate mortality results
#'
#' Accepts output from `Mortality()` with any CI branch (MEAN, UP, LOW, or
#' RANGE). Only mortality columns are summed. Missing mortality remains NA.
#'
#' @param x A data frame from `Mortality()`, or a named list of them.
#' @param at Aggregation level: `"grid"`, `"geo"`, or one or more Grid_info
#'   column names.
#' @param by `"total"`, `"endpoint"`, `"agegroup"`, or `"all"`.
#' @param write Write each result to `./Results/`.
#'
#' @return A named list of data frames, one per `at` value.
#' @export
aggregate_range <- function(x, at = "Country", by = "total", write = FALSE) {
  x <- normalise_mortality_list(x)
  scenarios <- names(x)

  valid_by <- c("total", "endpoint", "agegroup", "all")
  if (length(by) != 1 || !by %in% valid_by) {
    cli::cli_abort(
      "by must be one of {.val {valid_by}}. Got {.val {by}}."
    )
  }
  by_cols <- switch(
    by,
    total = character(0),
    endpoint = "endpoint",
    agegroup = "agegroup",
    all = c("endpoint", "agegroup")
  )

  geo_cols <- setdiff(names(Grid_info), c("x", "y"))
  at_list <- if (identical(at, "geo")) {
    c(list(c("x", "y")), as.list(geo_cols))
  } else if (identical(at, "grid")) {
    list(c("x", "y"))
  } else {
    as.list(at)
  }

  spec <- mortality_columns(x[[1]])
  log_msg(
    INFO,
    "aggregate_range: at = \"{at}\", by = \"{by}\", {length(x)} scenario(s)"
  )

  # One grid table for all `at` values: scenario rows are stacked, the
  # aggregation columns are attached once at grid level, and the mortality
  # columns are elongated once. Every aggregation level below reuses this long
  # table instead of rebuilding it. Grid_info must be unique on (x, y)
  # (read_files() enforces that), so the grid join cannot multiply rows.
  grid_rows <- map_int(x, nrow)
  n_value <- length(spec$column)
  n_grid <- sum(grid_rows)
  geo_lookup <- Grid_info |>
    select(all_of(c("x", "y", geo_cols))) |>
    distinct()
  grid <- bind_rows(map(x, ~ select(.x, x, y, all_of(spec$column)))) |>
    mutate(scenario = rep(scenarios, times = grid_rows)) |>
    left_join(geo_lookup, by = c("x", "y"))

  # The long table: one row per (grid row, mortality column), grid rows and
  # value columns in input order. Its row order is what the grouped sums below
  # reproduce, so the floating-point accumulation is unchanged.
  long <- grid |>
    select(all_of(spec$column)) |>
    pivot_longer(
      cols = everything(),
      names_to = NULL,
      values_to = "mort"
    )
  # The mortality columns are now in `long`; drop them from the grid table so
  # the two structures do not both hold the values.
  grid <- grid |> select(x, y, scenario, all_of(geo_cols))

  # Per-value-column key codes. The long table repeats `spec` once per value
  # column, so a long row's endpoint/agegroup/CI follow from its value-column
  # position; deriving them from the `spec` table avoids matching 4.8M strings
  # back to their labels. Only the codes' bijection matters, never their order,
  # because the output order comes from group_by() below.
  ci_code <- match(spec$CI, c("MEAN", "UP", "LOW"))
  endpoint_levels <- unique(spec$endpoint)
  agegroup_levels <- unique(spec$agegroup)
  endpoint_code <- match(spec$endpoint, endpoint_levels)
  agegroup_code <- match(spec$agegroup, agegroup_levels)
  n_endpoint <- length(endpoint_levels)
  n_agegroup <- length(agegroup_levels)
  key_value <- switch(
    by,
    total = ci_code,
    endpoint = endpoint_code + n_endpoint * (ci_code - 1L),
    agegroup = agegroup_code + n_agegroup * (ci_code - 1L),
    all = endpoint_code +
      n_endpoint * (agegroup_code - 1L + n_agegroup * (ci_code - 1L))
  )
  n_key_value <- max(key_value)
  # One key code per scenario block, so each `at` group is split into n_key
  # parts; group ids below are at code + n_at * key index.
  n_key <- n_key_value * length(x)
  # 0-based group index of every long row within its `at` group: the long table
  # is ordered scenario, grid row, value column.
  key_index <- rep(key_value - 1L, times = n_grid) +
    rep(n_key_value * (seq_along(x) - 1L), times = grid_rows * n_value)
  key_labels <- tibble(
    kv = key_value,
    endpoint = spec$endpoint,
    agegroup = spec$agegroup,
    CI = spec$CI
  ) |>
    select(kv, all_of(c(by_cols, "CI"))) |>
    distinct()

  aggregate_one <- function(at_cols, by_cols) {
    domain_columns(at_cols)

    missing_geo <- reduce(
      map(at_cols, ~ is.na(grid[[.x]])),
      `|`
    )
    if (any(missing_geo)) {
      first_bad <- grid$scenario[[which(missing_geo)[[1]]]]
      n_missing <- sum(missing_geo & grid$scenario == first_bad)
      cli::cli_abort(c(
        "{n_missing} grid(s) in scenario {.val {first_bad}} have no ",
        "matching Grid_info aggregation columns."
      ))
    }

    # One integer code per grid row for the requested aggregation columns.
    at_code <- integer(n_grid)
    n_at <- 1L
    for (col in at_cols) {
      at_levels <- unique(grid[[col]])
      at_code <- at_code * length(at_levels) + (match(grid[[col]], at_levels) - 1L)
      n_at <- n_at * length(at_levels)
    }
    at_code <- at_code + 1L

    # Grouped sums with R-level sum(), in long-table row order. Stable radix
    # order keeps each group's rows in long-table order, so the floating-point
    # accumulation matches the previous grouped summarise() exactly.
    row_group <- rep(at_code, each = n_value) + n_at * key_index
    group_size <- tabulate(row_group, nbins = n_at * n_key)
    group_ord <- order(row_group, method = "radix")
    rm(row_group)
    group_ends <- cumsum(group_size)
    group_starts <- group_ends - group_size + 1L
    group_ids <- which(group_size > 0L)
    # sum() of an empty slice keeps the value type summarise() would produce.
    empty_sum <- sum(long$mort[0L])
    group_sums <- vapply(
      seq_along(group_ids),
      function(i) {
        group <- group_ids[[i]]
        sum(
          long$mort[group_ord[group_starts[[group]]:group_ends[[group]]]]
        )
      },
      empty_sum
    )

    # Group keys, ordered the way group_by()/summarise() ordered them.
    at_first <- match(seq_len(n_at), at_code)
    at_labels <- grid[at_first, at_cols]
    key_parts <- tibble(
      at_id = ((group_ids - 1L) %% n_at) + 1L,
      key_id = ((group_ids - 1L) %/% n_at) + 1L
    ) |>
      mutate(
        kv = ((key_id - 1L) %% n_key_value) + 1L,
        scen = ((key_id - 1L) %/% n_key_value) + 1L
      ) |>
      left_join(key_labels, by = "kv") |>
      mutate(scenario = scenarios[scen])
    group_keys <- bind_cols(
      at_labels[key_parts$at_id, , drop = FALSE],
      key_parts[c(by_cols, "scenario", "CI")]
    )
    group_cols <- c(at_cols, by_cols, "scenario", "CI")
    group_order <- group_keys |>
      group_by(across(all_of(group_cols))) |>
      group_data() |>
      pull(.rows) |>
      unlist(use.names = FALSE)
    summarised <- bind_cols(
      group_keys[group_order, , drop = FALSE],
      tibble(mort = group_sums[group_order])
    )

    out <- summarised |>
      unite("key", all_of(c("scenario", "CI")), sep = "_") |>
      pivot_wider(names_from = "key", values_from = "mort")

    if (write) {
      dir.create("./Results", showWarnings = FALSE, recursive = TRUE)
      outpath <- file.path(
        "./Results",
        str_glue(
          "{tell_Model()}_{str_c(at_cols, collapse='+')}_{str_c(by_cols, collapse='+')}_",
          "Build{format(Sys.Date(), '%y%m%d')}.xlsx"
        )
      )
      if (length(by_cols) == 0) {
        outpath <- file.path(
          "./Results",
          str_glue(
            "{tell_Model()}_{str_c(at_cols, collapse='+')}_total_",
            "Build{format(Sys.Date(), '%y%m%d')}.xlsx"
          )
        )
      }
      writexl::write_xlsx(out, outpath)
      log_msg(INFO, "Written: ", outpath)
    }

    out
  }

  result_list <- lapply(at_list, function(at_cols) {
    aggregate_one(at_cols = at_cols, by_cols = by_cols)
  })
  names(result_list) <- map_chr(at_list, paste, collapse = "+")
  invisible(result_list)
}

# Backward-compatible aliases
aggregate_ci <- aggregate_range
summarise_ci <- aggregate_range
