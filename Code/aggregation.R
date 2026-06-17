# Aggregation & output module
#   - aggregate_sigma()  — aggregate MEAN mortality by geographic domain; CI bounds
#                         are derived via error propagation (Uncertainty()), not by
#                         aggregating RR-substitution UP/LOW branches.
#   - aggregate_range()  — RR-substitution CI aggregation: takes Mortality(CI=RANGE)
#                         output, aggregates by at (geo level) and by (endpoint/agegroup),
#                         writes single-xlsx output with all scenarios as columns
#   Modified 260610-260613: single-sheet xlsx output, scenario-combining with
#     imap+rename_with, at='geo'/'grid'/'x'/'y' support, by='total' alias,
#     auto-detect CI branch suffixes, Mort_Aggregate→aggregate_sigma rename.

aggregate_sigma <- function(
  full_result,
  at = "Country",
  by = "total",
  write = TRUE,
  ...
) {
  log_msg(
    INFO,
    "aggregate_sigma: at = \"{at}\", by = \"{by}\", {length(full_result)} scenario(s)"
  )

  #  Auto-detect: character vector = scenario names, compute internally
  if (is.character(full_result)) {
    scenarios <- full_result
    log_msg(
      INFO,
      "Computing grid-level mortality for {length(scenarios)} scenarios..."
    )
    full_result <- scenarios |>
      set_names() |>
      map(
        ~ Mortality_at(at = .x, CI = "MEAN")
      )
  } else if (is.list(full_result)) {
    scenarios <- names(full_result)
    if (is.null(scenarios) || any(scenarios == "")) {
      stop(
        "full_result must be a NAMED list. Use set_names() when creating it."
      )
    }
  } else {
    stop(
      "full_result must be either a character vector of scenario names ",
      "or a named list of data frames. Got: ",
      class(full_result)[1]
    )
  }

  if (length(full_result) == 0) {
    stop("No scenarios to process. full_result is empty.")
  }

  #  Normalize and validate `by`
  valid_by <- c("total", "endpoint", "agegroup", "all")
  if (!by %in% valid_by) {
    stop(
      "by must be one of: \"",
      paste(valid_by, collapse = "\", \""),
      "\". Got: \"",
      by,
      "\""
    )
  }
  by_val <- if (by == "total") {
    NULL
  } else if (by == "all") {
    c("endpoint", "agegroup")
  } else {
    by
  }

  #  Resolve at → domain (column vector for pivot/group_by)
  is_grid <- identical(at, "grid")
  domain <- if (is_grid) c("x", "y") else at

  #  Guard: domain validity
  if (!is_grid && !all(domain %in% names(Grid_info))) {
    stop(
      "Domain column \"",
      paste(setdiff(domain, names(Grid_info)), collapse = "\", \""),
      "\" not found in Grid_info. Available: ",
      paste(names(Grid_info), collapse = ", ")
    )
  }

  #  Detect mortality columns.  aggregate_sigma only operates on MEAN mortality;
  #  uncertainty (UP / LOW) is derived via error propagation (Uncertainty()), not
  #  by aggregating the RR-substitution CI branches.
  sample_names <- names(full_result[[1]])
  mean_suffixed <- str_subset(sample_names, '_[0-9]+_MEAN$')
  up_low_suffixed <- str_subset(sample_names, '_[0-9]+_(UP|LOW)$')
  # Backward compat: old-format columns without suffix (e.g. copd_25)
  old_style <- str_subset(sample_names, '_[1-9]?(0|5)$') |>
    str_subset('_(MEAN|UP|LOW)$', negate = TRUE)

  if (length(up_low_suffixed) > 0) {
    log_msg(
      WARN,
      "{length(up_low_suffixed)} UP/LOW columns detected. ",
      "aggregate_sigma only uses MEAN mortality; uncertainty is computed via error ",
      "propagation (Uncertainty()). UP/LOW columns will be ignored."
    )
  }

  if (length(mean_suffixed) > 0) {
    mort_cols <- mean_suffixed
    ci_branches <- "MEAN"
  } else {
    mort_cols <- old_style
    ci_branches <- character(0)
  }
  log_msg(INFO, "  {length(mort_cols)} mortality columns detected")

  #  Aggregate
  pre_aggr <- if (is_grid & is.null(by_val)) {
    full_result
  } else if (is_grid & !is.null(by_val)) {
    full_result |>
      map(function(df) {
        if (length(ci_branches) > 0) {
          df |>
            pivot_longer(
              cols = any_of(mort_cols),
              values_to = 'Mort',
              names_to = c('endpoint', 'agegroup', '.branch'),
              names_sep = '_'
            ) |>
            group_by(pick(all_of(c(domain, by_val, '.branch')))) |>
            summarise(Mort = sum(Mort), .groups = "drop") |>
            unite("key", any_of(c(by_val, '.branch')), sep = "_") |>
            pivot_wider(names_from = 'key', values_from = 'Mort')
        } else {
          df |>
            pivot_longer(
              cols = any_of(mort_cols),
              values_to = 'Mort',
              names_to = c('endpoint', 'agegroup'),
              names_sep = '_'
            ) |>
            group_by(pick(all_of(c(domain, by_val)))) |>
            summarise(Mort = sum(Mort), .groups = "drop") |>
            pivot_wider(names_from = by_val, values_from = 'Mort')
        }
      })
  } else if (!is_grid & is.null(by_val)) {
    full_result |>
      map(
        ~ left_join(.x, Grid_info) |>
          group_by(pick(all_of(domain))) |>
          summarise(across(any_of(mort_cols), sum), .groups = "drop")
      )
  } else if (!is_grid & !is.null(by_val)) {
    full_result |>
      map(function(df) {
        if (length(ci_branches) > 0) {
          df |>
            left_join(Grid_info) |>
            pivot_longer(
              cols = any_of(mort_cols),
              values_to = 'Mort',
              names_to = c('endpoint', 'agegroup', '.branch'),
              names_sep = '_'
            ) |>
            group_by(pick(all_of(c(domain, by_val, '.branch')))) |>
            summarise(Mort = sum(Mort), .groups = "drop") |>
            unite("key", any_of(c(by_val, '.branch')), sep = "_") |>
            pivot_wider(names_from = 'key', values_from = 'Mort')
        } else {
          df |>
            left_join(Grid_info) |>
            pivot_longer(
              cols = any_of(mort_cols),
              values_to = 'Mort',
              names_to = c('endpoint', 'agegroup'),
              names_sep = '_'
            ) |>
            group_by(pick(all_of(c(domain, by_val)))) |>
            summarise(Mort = sum(Mort), .groups = "drop") |>
            pivot_wider(names_from = by_val, values_from = 'Mort')
        }
      })
  }

  #  Add Total column(s) per CI branch
  if (length(ci_branches) > 0) {
    pre_aggr <- pre_aggr |>
      map(function(df) {
        df_out <- df
        for (br in ci_branches) {
          br_cols <- str_subset(names(df_out), str_c("_", br, "$"))
          total_name <- str_c("total_", br)
          df_out <- df_out |>
            mutate(
              !!total_name := rowSums(pick(any_of(br_cols)), na.rm = TRUE)
            )
        }
        df_out |>
          relocate(
            starts_with("total_"),
            .after = if (is_grid) y else all_of(domain)
          )
      })
  } else {
    pre_aggr <- pre_aggr |>
      map(
        if (is_grid) {
          ~ .x |>
            mutate(
              total = rowSums(pick(any_of(mort_cols)), na.rm = TRUE),
              .after = x:y
            )
        } else {
          ~ .x |>
            mutate(
              total = rowSums(pick(-all_of(!!domain)), na.rm = TRUE),
              .after = !!domain
            )
        }
      )
  }

  #  When by = "total", drop per-endpoint/agegroup columns — keep only totals
  if (by == "total") {
    pre_aggr <- pre_aggr |> map(~ {
      keep <- c(if (is_grid) c("x", "y") else domain,
                str_subset(names(.x), "^total(_MEAN)?$"))
      .x |> select(any_of(keep))
    })
  }

  log_msg(INFO, "  Aggregated to {nrow(pre_aggr[[1]])} rows")

  #  CI: error propagation (Uncertainty) or grid metadata
  extra_args <- list(...)
  inc_conc <- extra_args[["includeConc"]] %||% FALSE
  conc_err <- extra_args[["Conc_ERR"]] %||% 12
  verb_flag <- extra_args[["verbose"]] %||% FALSE

  CI <- if (is_grid) {
    log_msg(INFO, "  Grid-level: skipping uncertainty (use aggregate_range for CI)")
    full_result |>
      map(
        ~ Grid_info |> select(x:y, any_of(c("Country", "Region", "Province")))
      )
  } else {
    log_msg(INFO, "  Computing error-propagation uncertainty...")
    scenarios |>
      set_names() |>
      map(
        ~ Uncertainty(
          includeConc = inc_conc,
          Conc_ERR = conc_err,
          verbose = verb_flag,
          m_Rate = getMortRate(.x),
          aggr_pop = Grid_info |>
            left_join(getPop(.x)) |>
            group_by(pick(all_of(domain))) |>
            summarise(Pop = sum(Pop, na.rm = TRUE)) |>
            na.omit(),
          age_struc = getAgeGroup(.x),
          PWE = list(Grid_info, getConc_real(.x), getPop(.x)) |>
            reduce(left_join) |>
            na.omit() |>
            group_by(pick(all_of(domain))) |>
            summarise(
              PWE = weighted.mean(as.numeric(concentration), Pop, na.rm = TRUE)
            )
        ) |>
          rename_with(~domain, where(is.character))
      )
  }

  #  Join CI with aggregated results
  aggr_result <- if (is_grid) {
    map2(CI, pre_aggr, ~ left_join(.x, .y))
  } else {
    map2(CI, pre_aggr, ~ left_join(.x, .y, by = domain)) |>
      map(~ {
        total_col <- if ("total_MEAN" %in% names(.x)) "total_MEAN" else "total"
        .x |>
          mutate(
            total_UP  = .data[[total_col]] + CI_UP,
            total_LOW = .data[[total_col]] - CI_LOW,
            .after = all_of(total_col)
          ) |>
          select(-CI_UP, -CI_LOW)
      })
  }

  if (!is_grid) {
    aggr_result <- aggr_result |>
      imap_dfr(~ .x |> add_column(year = .y, .before = TRUE))
  }

  if (nrow(aggr_result) == 0) {
    stop(
      "Aggregation produced 0 rows. Check domain/region mapping consistency."
    )
  }

  if (write) {
    outpath <- str_glue(
      "./Result/{tell_Model()}_{str_c(at, collapse='+')}_by{by}_",
      "{head(scenarios, 1)}-{tail(scenarios, 1)}_",
      "Build{format(Sys.Date(), '%y%m%d')}.xlsx"
    )
    dir.create("./Result", showWarnings = FALSE, recursive = TRUE)
    aggr_result |> write_xlsx(outpath)
    log_msg(INFO, "  Written: ", outpath)
  }

  return(aggr_result)
}

#  Mortality result aggregation
# Takes any Mortality() output (with or without CI branch suffixes) and
# aggregates by user-specified geographic level (at) and breakdown (by).

#' Aggregate mortality results
#'
#' Accepts output from Mortality() with any CI branch (MEAN, UP, LOW, or
#' RANGE). Auto-detects branch suffixes from column names and aggregates
#' each branch independently.
#'
#' @param x A data frame from Mortality(), or a named list of them (one per scenario).
#' @param at Aggregation level. \code{"grid"} keeps grid-level (adds total columns).
#'   \code{"geo"} aggregates to all geographic columns in Grid_info.
#'   A specific column name (e.g. \code{"Country"}) or vector of names.
#' @param by Breakdown dimension. Must be one of \code{"total"}
#'   (total only, no breakdown), \code{"endpoint"}, \code{"agegroup"},
#'   or \code{"all"} (both endpoint and agegroup).
#' @param write If \code{FALSE}, no output written. If \code{TRUE}, writes to
#'   \code{./Result/}.
#'
#' @return A named list of data frames, one per \code{at} value.
#'   Each data frame has one row per \code{at} × \code{by} combination
#'   and one column per scenario × CI branch. Use \code{pluck()} to extract.
#'
#' @export
#'
#' @examples
#' grid_ci <- Mortality_at(at = "base2015", CI = "RANGE", domain = "Country")
#' result <- aggregate_range(grid_ci, at = "Country", by = "total")
# Dispatch: resolve at → at_list, aggregate each combo.
aggregate_range <- function(x, at = "Country", by = "total", write = FALSE) {
  if (is.data.frame(x)) {
    x <- list(scenario = x)
  }

  log_msg(
    INFO,
    "aggregate_range: at = \"{at}\", by = \"{by}\", {length(x)} scenario(s)"
  )

  # Normalize and validate `by`
  valid_by <- c("total", "endpoint", "agegroup", "all")
  if (!by %in% valid_by) {
    stop(
      "by must be one of: \"",
      paste(valid_by, collapse = "\", \""),
      "\". Got: \"",
      by,
      "\""
    )
  }

  geo_cols <- names(Grid_info)
  grid_col <- c("x", "y")

  # Resolve at → list of column vectors
  at_list <- if (identical(at, "geo")) {
    c(list(grid_col), as.list(geo_cols))
  } else if (identical(at, "grid")) {
    list(grid_col)
  } else {
    as.list(at)
  }

  # Resolve by → single value
  by_val <- if (identical(by, "all")) c("endpoint", "agegroup") else by

  # ---- aggregate_one: pivot → group → bind → spread for one at×by combo ----
  aggregate_one <- function(at_val, by_val) {
    mort_cols <- str_subset(names(x[[1]]), '_[0-9]+_(MEAN|UP|LOW)$')
    if (length(mort_cols) == 0) {
      stop("No mortality columns found.")
    }

    # Pivot pattern: extract by dimensions + CI; .value keeps collapsed dims as cols
    if (identical(by_val, "total")) {
      ptrn <- "(.+)_(MEAN|UP|LOW)"
      to_cols <- c(".value", "CI")
      group_vals <- c(at_val, "scenario", "CI")
    } else if (identical(by_val, "endpoint")) {
      ptrn <- "(.+)_([0-9]+)_(MEAN|UP|LOW)$"
      to_cols <- c("endpoint", ".value", "CI")
      group_vals <- c(at_val, "endpoint", "scenario", "CI")
    } else if (identical(by_val, "agegroup")) {
      ptrn <- "(.+)_([0-9]+)_(MEAN|UP|LOW)$"
      to_cols <- c(".value", "agegroup", "CI")
      group_vals <- c(at_val, "agegroup", "scenario", "CI")
    } else {
      # by_val = c("endpoint", "agegroup") — both to rows, no .value
      ptrn <- "(.+)_([0-9]+)_(MEAN|UP|LOW)$"
      to_cols <- c("endpoint", "agegroup", "CI")
      group_vals <- c(at_val, "endpoint", "agegroup", "scenario", "CI")
    }

    out <- imap(x, \(df, scen) {
      df <- df |>
        left_join(Grid_info, by = c("x", "y")) |>
        pivot_longer(
          cols = any_of(mort_cols),
          names_pattern = ptrn,
          names_to = to_cols
        ) |>
        mutate(scenario = scen)

      # .value cols → rowSums; no .value → rename value column
      if ("value" %in% names(df)) {
        df <- df |> rename(mort = value)
      } else {
        df <- df |> mutate(mort = rowSums(pick(where(is.numeric))))
      }

      df |>
        select(all_of(group_vals), mort) |>
        group_by(pick(all_of(group_vals))) |>
        summarise(mort = sum(mort, na.rm = TRUE), .groups = "drop")
    }) |>
      bind_rows() |>
      pivot_wider(
        names_from = c("scenario", "CI"),
        names_sep = "_",
        values_from = "mort",
        values_fill = NA
      )

    if (write) {
      out_dir <- "./Result"
      dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
      outpath <- file.path(
        out_dir,
        str_glue(
          "{tell_Model()}_{str_c(at_val, collapse='+')}_{str_c(by_val, collapse='+')}_",
          "Build{format(Sys.Date(), '%y%m%d')}.xlsx"
        )
      )
      writexl::write_xlsx(out, outpath)
      log_msg(INFO, "Written: ", outpath)
    }

    print(out)
  }

  result_list <- lapply(at_list, function(a) {
    aggregate_one(at_val = a, by_val = by_val)
  })
  names(result_list) <- sapply(at_list, paste, collapse = "+")
  invisible(result_list)
}

# Backward-compatible aliases
aggregate_ci <- aggregate_range
summarise_ci <- aggregate_range
