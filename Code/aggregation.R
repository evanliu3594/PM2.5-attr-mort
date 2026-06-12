# Aggregation & output module
#   - Mort_Aggregate()  — aggregate grid results by geographic domain, with optional
#                         error-propagation CI via Uncertainty()
#   - aggregate_mort()  — RR-substitution CI aggregation: takes Mortality(CI=RANGE)
#                         output, aggregates by at (geo level) and by (endpoint/agegroup),
#                         writes single-xlsx output with all scenarios as columns
#   Modified 260610-260613: single-sheet xlsx output, scenario-combining with
#     imap+rename_with, at='geo'/'grid'/'x'/'y' support, by='total' alias,
#     auto-detect CI branch suffixes, summarise_ci→aggregate_mort rename.

Mort_Aggregate <- function(
  full_result,
  domain = 'Grid',
  by = NULL,
  write = TRUE,
  ...
) {
  # ---- Auto-detect: character vector = scenario names, compute internally ----
  if (is.character(full_result)) {
    scenarios <- full_result
    cat(
      "Computing grid-level mortality for",
      length(scenarios),
      "scenarios...\n"
    )
    full_result <- scenarios %>%
      set_names %>%
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

  # ---- Guard: domain validity ----
  if (domain != 'Grid') {
    if (!all(domain %in% names(Grid_info))) {
      stop(
        "Domain column \"",
        paste(setdiff(domain, names(Grid_info)), collapse = "\", \""),
        "\" not found in Grid_info. Available: ",
        paste(names(Grid_info), collapse = ", ")
      )
    }
  }

  # ---- Guard: by column validity ----
  valid_by <- c("endpoint", "agegroup", names(Grid_info))
  if (!is.null(by) && !by %in% valid_by) {
    stop(
      "by = \"",
      by,
      "\" is not a valid aggregation dimension. ",
      "Use \"endpoint\", \"agegroup\", or a column name from Grid_info: ",
      paste(setdiff(names(Grid_info), c("x", "y")), collapse = ", ")
    )
  }

  # ---- Determine which columns to aggregate ----
  # Columns now have CI branch suffixes: copd_25_MEAN, copd_25_UP, copd_25_LOW
  sample_names <- names(full_result[[1]])
  ci_suffixed <- str_subset(sample_names, '_[0-9]+_(MEAN|UP|LOW)$')
  # Backward compat: old-format columns without suffix (e.g. copd_25)
  old_style <- str_subset(sample_names, '_[1-9]?(0|5)$') %>%
    str_subset('_(MEAN|UP|LOW)$', negate = TRUE)

  if (length(ci_suffixed) > 0) {
    mort_cols <- ci_suffixed
    ci_branches <- str_extract(mort_cols, '(MEAN|UP|LOW)$') %>% unique()
  } else {
    mort_cols <- old_style
    ci_branches <- character(0)
  }

  # ---- Aggregate ----
  pre_aggr <- if (domain == 'Grid' & is.null(by)) {
    full_result
  } else if (domain == 'Grid' & !is.null(by)) {
    full_result %>%
      map(function(df) {
        if (length(ci_branches) > 0) {
          df %>%
            pivot_longer(
              cols = any_of(mort_cols),
              values_to = 'Mort',
              names_to = c('endpoint', 'agegroup', '.branch'),
              names_sep = '_'
            ) %>%
            group_by(pick(all_of(c(domain, by, '.branch')))) %>%
            summarise(Mort = sum(Mort), .groups = "drop") %>%
            unite("key", any_of(c(by, '.branch')), sep = "_") %>%
            pivot_wider(names_from = 'key', values_from = 'Mort')
        } else {
          df %>%
            pivot_longer(
              cols = any_of(mort_cols),
              values_to = 'Mort',
              names_to = c('endpoint', 'agegroup'),
              names_sep = '_'
            ) %>%
            group_by(pick(all_of(c(domain, by)))) %>%
            summarise(Mort = sum(Mort), .groups = "drop") %>%
            pivot_wider(names_from = by, values_from = 'Mort')
        }
      })
  } else if (domain != 'Grid' & is.null(by)) {
    full_result %>%
      map(
        ~ left_join(.x, Grid_info) %>%
          group_by(pick(all_of(domain))) %>%
          summarise(across(any_of(mort_cols), sum), .groups = "drop")
      )
  } else if (domain != 'Grid' & !is.null(by)) {
    full_result %>%
      map(function(df) {
        if (length(ci_branches) > 0) {
          df %>%
            left_join(Grid_info) %>%
            pivot_longer(
              cols = any_of(mort_cols),
              values_to = 'Mort',
              names_to = c('endpoint', 'agegroup', '.branch'),
              names_sep = '_'
            ) %>%
            group_by(pick(all_of(c(domain, by, '.branch')))) %>%
            summarise(Mort = sum(Mort), .groups = "drop") %>%
            unite("key", any_of(c(by, '.branch')), sep = "_") %>%
            pivot_wider(names_from = 'key', values_from = 'Mort')
        } else {
          df %>%
            left_join(Grid_info) %>%
            pivot_longer(
              cols = any_of(mort_cols),
              values_to = 'Mort',
              names_to = c('endpoint', 'agegroup'),
              names_sep = '_'
            ) %>%
            group_by(pick(all_of(c(domain, by)))) %>%
            summarise(Mort = sum(Mort), .groups = "drop") %>%
            pivot_wider(names_from = by, values_from = 'Mort')
        }
      })
  }

  # ---- Add Total column(s) per CI branch ----
  if (length(ci_branches) > 0) {
    pre_aggr <- pre_aggr %>%
      map(function(df) {
        df_out <- df
        for (br in ci_branches) {
          br_cols <- str_subset(names(df_out), str_c("_", br, "$"))
          total_name <- str_c("Total_", br)
          df_out <- df_out %>%
            mutate(
              !!total_name := rowSums(select(., any_of(br_cols)), na.rm = TRUE)
            )
        }
        df_out %>%
          relocate(
            starts_with("Total_"),
            .after = if (domain == 'Grid') y else all_of(domain)
          )
      })
  } else {
    pre_aggr <- pre_aggr %>%
      map(
        if (domain == 'Grid') {
          ~ .x %>%
            mutate(
              Total = rowSums(select(., any_of(mort_cols)), na.rm = TRUE),
              .after = x:y
            )
        } else {
          ~ .x %>%
            mutate(
              Total = rowSums(select(., -all_of(!!domain)), na.rm = TRUE),
              .after = !!domain
            )
        }
      )
  }

  # ---- CI: error propagation (Uncertainty) or grid metadata ----
  extra_args <- list(...)
  inc_conc <- extra_args[["includeConc"]] %||% FALSE
  conc_err <- extra_args[["Conc_ERR"]] %||% 12
  verb_flag <- extra_args[["verbose"]] %||% FALSE

  CI <- if (domain == 'Grid') {
    full_result %>%
      map(
        ~ Grid_info %>% select(x:y, any_of(c("Country", "Region", "Province")))
      )
  } else {
    scenarios %>%
      set_names %>%
      map(
        ~ Uncertainty(
          includeConc = inc_conc,
          Conc_ERR = conc_err,
          verbose = verb_flag,
          m_Rate = getMortRate(.x),
          aggr_pop = Grid_info %>%
            left_join(getPop(.x)) %>%
            group_by(pick(all_of(domain))) %>%
            summarise(Pop = sum(Pop, na.rm = TRUE)) %>%
            na.omit,
          age_struc = getAgeGroup(.x),
          PWE = list(Grid_info, getConc_real(.x), getPop(.x)) %>%
            reduce(left_join) %>%
            na.omit %>%
            group_by(pick(all_of(domain))) %>%
            summarise(
              PWE = weighted.mean(as.numeric(concentration), Pop, na.rm = TRUE)
            )
        ) %>%
          rename_with(~domain, where(is.character))
      )
  }

  # ---- Join CI with aggregated results ----
  aggr_result <- if (domain == 'Grid') {
    map2(CI, pre_aggr, ~ left_join(.x, .y))
  } else {
    map2(CI, pre_aggr, ~ left_join(.x, .y, by = domain))
  }

  if (domain %in% c("Country", "Province", "Region")) {
    aggr_result <- aggr_result %>%
      imap_dfr(~ .x %>% add_column(year = .y, .before = TRUE))
  }

  if (nrow(aggr_result) == 0) {
    stop(
      "Aggregation produced 0 rows. Check domain/region mapping consistency."
    )
  }

  if (write) {
    by_label <- if (is.null(by)) {
      "Everything"
    } else {
      str_replace(by, "^.{1}", toupper)
    }
    outpath <- str_glue(
      "./Result/{tell_Model()}_{domain}_by{by_label}_",
      "{head(scenarios, 1)}-{tail(scenarios, 1)}_",
      "Build{format(Sys.Date(), '%y%m%d')}.xlsx"
    )
    dir.create("./Result", showWarnings = FALSE, recursive = TRUE)
    aggr_result %>% write_xlsx(outpath)
    cat("Result written to: ", outpath, "\n")
  }

  return(aggr_result)
}

# ---- Mortality result aggregation ----
# Takes any Mortality() output (with or without CI branch suffixes) and
# aggregates by user-specified geographic level (at) and breakdown (by).

#' Aggregate mortality results
#'
#' Accepts output from Mortality() with any CI branch (MEAN, UP, LOW, or
#' RANGE). Auto-detects branch suffixes from column names and aggregates
#' each branch independently.
#'
#' @param x A data frame from Mortality(), or a named list of them (one per scenario).
#' @param at Aggregation level. \code{"grid"} keeps grid-level (adds Total columns).
#'   \code{"geo"} aggregates to all geographic columns in Grid_info.
#'   A specific column name (e.g. \code{"Country"}) or vector of names.
#' @param by Breakdown dimension. \code{NULL} for Total only (no breakdown).
#'   \code{"all"} for both endpoint and agegroup. \code{"endpoint"} or
#'   \code{"agegroup"} for a single dimension.
#' @param write If \code{FALSE}, no output written. If \code{TRUE}, writes to
#'   \code{./Result/}. If a character string, writes to that directory.
#'
#' @return A nested list: result[[level]][[breakdown]][[scenario]].
#'   Each leaf is a data frame with branch-specific value columns.
#'
#' @export
#'
#' @examples
#' grid_ci <- Mortality_at(at = "base2015", CI = "RANGE", domain = "Country")
#' all <- aggregate_mort(grid_ci, at = "Country", by = "all")
aggregate_mort <- function(
  x,
  at = c("Country"),
  by = NULL,
  write = FALSE
) {
  # Normalise to named list of data frames
  if (is.data.frame(x)) {
    x <- list(scenario = x)
  }
  scenarios <- names(x)
  if (is.null(scenarios)) {
    scenarios <- seq_along(x)
  }

  # ---- Auto-detect CI branch columns ----
  # Suffixed:  copd_25_MEAN, copd_25_UP, copd_25_LOW
  # Unsuffixed (legacy): copd_25
  sample_nm <- names(x[[1]])
  suffixed <- str_subset(sample_nm, '_[0-9]+_(MEAN|UP|LOW)$')
  unsuffixed <- str_subset(sample_nm, '_[1-9]?(0|5)$') %>%
    str_subset('_(MEAN|UP|LOW)$', negate = TRUE)

  if (length(suffixed) > 0) {
    mort_cols <- suffixed
    branches <- str_extract(suffixed, '(MEAN|UP|LOW)$') %>% unique()
  } else if (length(unsuffixed) > 0) {
    mort_cols <- unsuffixed
    branches <- character(0) # no suffix → single branch, no branch column
  } else {
    stop(
      "No mortality columns found. Expected patterns like 'copd_25' or 'copd_25_MEAN'."
    )
  }

  # Attach geo info
  x <- x %>% map(~ left_join(.x, Grid_info, by = c("x", "y")))

  # ---- Resolve 'at' — each column is a separate aggregation level ----
  if (identical(at, "geo")) {
    at_levels <- as.list(names(Grid_info))
  } else if (identical(at, "grid")) {
    at_levels <- list(character(0))
  } else {
    at_levels <- lapply(at, function(a) {
      if (!a %in% names(Grid_info)) {
        stop(
          "Column \"",
          a,
          "\" not found in Grid_info. Available: ",
          paste(names(Grid_info), collapse = ", ")
        )
      }
      a
    })
  }
  names(at_levels) <- if (identical(at, "grid")) {
    "Grid"
  } else {
    sapply(at_levels, function(a) if (length(a)) a else "Grid")
  }

  # ---- Resolve 'by' ----
  if (is.null(by) || any(str_to_lower(by) == "total")) {
    by <- character(0)
  } else if (identical(by, "all")) {
    by <- c("endpoint", "agegroup")
  }
  if (length(by) > 0) {
    by <- match.arg(
      by,
      several.ok = TRUE,
      choices = c("endpoint", "agegroup", "total")
    )
  }

  # ---- Internal: aggregate one data frame ----
  do_aggregate <- function(df, group_vars) {
    if (length(group_vars) == 0) {
      # Grid-level: add Total columns, drop per-endpoint-age columns
      if (length(branches) > 0) {
        for (br in branches) {
          br_cols <- str_subset(names(df), str_c("_", br, "$"))
          total_nm <- str_c("Total_", br)
          df <- df %>%
            mutate(
              !!total_nm := rowSums(select(., any_of(br_cols)), na.rm = TRUE)
            )
        }
        df %>%
          select(x, y, starts_with("Total_"))
      } else {
        df %>%
          mutate(
            Total = rowSums(select(., any_of(mort_cols)), na.rm = TRUE),
            .after = y
          )
      }
    } else if (length(branches) > 0) {
      # Suffixed columns: pivot with endpoint/agegroup/branch
      df %>%
        pivot_longer(
          cols = any_of(mort_cols),
          names_to = c("endpoint", "agegroup", "branch"),
          names_sep = "_"
        ) %>%
        group_by(pick(all_of(c(group_vars, "branch")))) %>%
        summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
        pivot_wider(
          names_from = "branch",
          values_from = "value",
          values_fill = 0
        )
    } else {
      # Legacy unsuffixed: aggregate Total by group_vars
      df %>%
        mutate(Total = rowSums(select(., any_of(mort_cols)), na.rm = TRUE)) %>%
        select(all_of(c(group_vars, "Total"))) %>%
        group_by(pick(all_of(group_vars))) %>%
        summarise(Total = sum(Total, na.rm = TRUE), .groups = "drop")
    }
  }

  # ---- Build result: all geo levels × breakdowns → single named list ----
  result <- list()

  for (lv_name in names(at_levels)) {
    gv_geo <- at_levels[[lv_name]]

    for (br_key in c("Total", str_c("by_", by))) {
      gv <- if (br_key == "Total") {
        gv_geo
      } else {
        c(gv_geo, str_remove(br_key, "^by_"))
      }

      # Compute per scenario, then combine: rename value cols with scenario prefix, join
      per_scen <- x %>% map(~ do_aggregate(.x, gv))

      id_cols <- str_subset(names(per_scen[[1]]), '_(MEAN|UP|LOW)$', negate = TRUE)
      if (length(id_cols) == 0) id_cols <- names(per_scen[[1]])  # fallback: keep all

      combined <- per_scen %>%
        imap(function(df, scen) {
          rename_with(
            df,
            function(cols) str_c(scen, "_", cols),
            -all_of(id_cols)
          )
        }) %>%
        reduce(left_join, by = id_cols)

      sheet_name <- str_c(lv_name, "_", br_key)
      result[[sheet_name]] <- combined
    }
  }

  # ---- Write: one xlsx, one sheet per (geo level × breakdown) ----
  if (!isFALSE(write)) {
    out_dir <- if (isTRUE(write)) "./Result" else write
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    outpath <- file.path(
      out_dir,
      str_glue("{tell_Model()}_Build{format(Sys.Date(), '%y%m%d')}.xlsx")
    )
    result %>% write_xlsx(outpath)
    cat("Written:", outpath, "\n")
  }

  return(result)
}

# Backward-compatible aliases
aggregate_ci <- aggregate_mort
summarise_ci <- aggregate_mort
