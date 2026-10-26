# Aggregation & output module
#   - aggregate_sigma()  — total attributable mortality plus PWRR-consistent
#                         uncertainty bounds derived from Mortality() branches.
#   - aggregate_range()  — aggregate Mortality() output by geography and
#                         endpoint/agegroup, preserving NA mortality.
#   Modified 261026b: aggregation now sums only mortality columns. Missing
#     components remain NA. Numeric metadata no longer enters totals.
#     aggregate_sigma() uses the PWRR estimator and supports total only.

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
#'   character vector of scenario names. If a list contains RANGE columns, only
#'   the `_MEAN` columns are used for the central estimate.
#' @param at A single Grid_info column used for spatial aggregation.
#' @param by Must be `"total"`.
#' @param write Write the result to `./Results/`.
#' @param ... `includeConc`, `Conc_ERR`, and `verbose`.
#'
#' @return A data frame with `year`, the domain column, `total_MEAN`, `total_UP`,
#'   and `total_LOW`.
#' @export
aggregate_sigma <- function(
  full_result,
  at = "Country",
  by = "total",
  write = TRUE,
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

  extra_args <- list(...)
  include_conc <- extra_args[["includeConc"]] %||% FALSE
  conc_err <- extra_args[["Conc_ERR"]] %||% 12
  verbose <- extra_args[["verbose"]] %||% FALSE

  if (is.character(full_result)) {
    scenarios <- full_result
    if (any(!nzchar(scenarios)) || anyDuplicated(scenarios)) {
      cli::cli_abort("Scenario names must be non-empty and unique.")
    }
  } else if (is.list(full_result) && !is.data.frame(full_result)) {
    scenarios <- names(full_result)
    if (is.null(scenarios) || any(!nzchar(scenarios))) {
      cli::cli_abort("full_result must be a named list.")
    }
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

  extract_mean <- function(df, scenario) {
    if (all(c("_MEAN", "_UP", "_LOW") |> map_lgl(~ any(str_detect(names(df), .x))))) {
      mean_cols <- str_subset(names(df), "_MEAN$")
      if (length(mean_cols) == 0) {
        cli::cli_abort("Scenario {.val {scenario}} has no MEAN mortality columns.")
      }
      return(df |> select(x, y, all_of(mean_cols)))
    }

    if (length(str_subset(names(df), "_(UP|LOW)$")) > 0) {
      cli::cli_abort(
        "Scenario {.val {scenario}} must contain MEAN mortality or a full RANGE set."
      )
    }
    df
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

  mean_grids <- list()
  for (scenario in scenarios) {
    if (is.character(full_result)) {
      mean_grids[[scenario]] <- Mortality_at(at = scenario, CI = "MEAN")
    } else {
      mean_grids[[scenario]] <- extract_mean(full_result[[scenario]], scenario)
    }
  }

  rows <- map(scenarios, function(scenario) {
    mean_total <- aggregate_total(
      mean_grids[[scenario]],
      str_c(scenario, " MEAN")
    ) |>
      rename(total_MEAN = total)

    up_grid <- Mortality_at(at = scenario, CI = "UP")
    low_grid <- Mortality_at(at = scenario, CI = "LOW")
    up_total <- aggregate_total(up_grid, str_c(scenario, " UP")) |>
      rename(total_UP_raw = total)
    low_total <- aggregate_total(low_grid, str_c(scenario, " LOW")) |>
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
          domain = at,
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

  aggregate_one <- function(at_cols, by_cols) {
    domain_columns(at_cols)

    geo_lookup <- Grid_info |>
      select(all_of(c("x", "y", at_cols))) |>
      distinct()

    long <- imap_dfr(x, function(df, scenario) {
      joined <- df |>
        select(x, y, all_of(spec$column)) |>
        left_join(geo_lookup, by = c("x", "y"))

      missing_geo <- joined |>
        filter(if_any(all_of(at_cols), is.na)) |>
        nrow()
      if (missing_geo > 0) {
        cli::cli_abort(
          "{missing_geo} grid(s) in scenario {.val {scenario}} have no ",
          "matching Grid_info aggregation columns."
        )
      }

      joined |>
        pivot_longer(
          cols = all_of(spec$column),
          names_to = c("endpoint", "agegroup", "CI"),
          names_pattern = "^(.*)_([0-9]+)_(MEAN|UP|LOW)$",
          values_to = "mort"
        ) |>
        mutate(scenario = scenario)
    })

    group_cols <- c(at_cols, by_cols, "scenario", "CI")
    summarised <- long |>
      group_by(across(all_of(group_cols))) |>
      summarise(mort = sum(mort), .groups = "drop")

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
