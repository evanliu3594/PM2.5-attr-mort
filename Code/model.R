# C-R Model configuration module
#   - tell_Model() — format the active C-R model name for output filenames
#   - set_Model()  — set the global .CR_Model and cache the standardised RR table
#   - RR_std()     — build the standardised RR table from the lookup xlsx

library(tidyverse)
library(writexl)
library(readxl)
library(jsonlite)

log_msg <- function(level, ...) {
  level <- match.arg(
    as.character(substitute(level)),
    c("INFO", "WARN", "ERROR")
  )
  msg <- str_glue(..., .envir = parent.frame())
  switch(
    level,
    INFO = cli::cli_alert_info(msg),
    WARN = cli::cli_text(cli::style_bold(cli::col_blue(str_c("⚠ ", msg)))),
    ERROR = cli::cli_text(cli::style_bold(cli::col_red(msg)))
  )
}

#' detect CRF model name and generate a name for file output
#'
#' Reads the label from RR_std_config.json. Falls back to .CR_Model
#' if the config or label is unavailable.
#'
#' @return formatted string of CRF name
#' @export
#'
tell_Model <- function() {
  cfg_path <- './Data/RR_std_config.json'
  if (file.exists(cfg_path)) {
    cfg <- jsonlite::fromJSON(cfg_path, simplifyVector = FALSE)
    entry <- cfg[[.CR_Model]]
    if (!is.null(entry$label)) return(entry$label)
  }
  .CR_Model
}

#' set CRF for calculation
#'
#' @param Model string — model name matching an entry in `RR_std_config.json`,
#'   or a custom name (pair with `path` for auto-config).
#' @param path Optional path to a custom RR lookup table (xlsx). When provided,
#'   the MEAN sheet is read and its `{endpoint}_{agegroup}` columns are parsed
#'   to auto-generate a standardisation config. Review and add to
#'   `Data/RR_std_config.json`.
#'
#' @export
set_Model <- function(Model, path = NULL) {
  assign(".CR_Model", Model, envir = globalenv())
  log_msg(INFO, "C-R Model \"{tell_Model()}\" is set.")

  # Auto-generate standardisation config from custom lookup table
  if (!is.null(path)) {
    if (!file.exists(path)) {
      stop("Custom RR lookup table not found: ", path)
    }

    raw <- read_xlsx(path, sheet = "MEAN")
    all_names <- names(raw)

    # Detect concentration column: the one column that does NOT match
    # the {endpoint}_{age} or {endpoint}_ALL pattern. Default to "concentration"
    # if the file follows the standard naming convention.
    mort_cols <- str_subset(all_names, '_[0-9]+$')
    all_cols <- str_subset(all_names, '_[Aa][Ll]{2}$')
    rr_cols <- union(mort_cols, all_cols)

    conc_col <- setdiff(all_names, rr_cols)
    if (length(conc_col) != 1) {
      conc_col <- "concentration"
      log_msg(
        WARN,
        "Could not auto-detect concentration column (found {length(conc_col)} candidates); ",
        "defaulting to \"{conc_col}\". Edit RR_std_config.json if needed."
      )
    }

    if (length(mort_cols) > 0) {
      parsed <- str_match(mort_cols, '^(.+)_([0-9]+)$')
      ep_ages <- tibble(
        endpoint = str_to_lower(parsed[, 2]),
        age = as.integer(parsed[, 3])
      ) |>
        group_by(endpoint) |>
        summarise(ages = list(sort(unique(age))), .groups = "drop")
    } else if (length(all_cols) > 0) {
      default_ages <- seq(0, 95, 5)
      ep_names <- str_to_lower(str_remove(all_cols, '_ALL$'))
      ep_ages <- tibble(
        endpoint = ep_names,
        ages = rep(list(default_ages), length(ep_names))
      )
      log_msg(
        INFO,
        "Only _ALL columns found; auto-generated ages ",
        "{min(default_ages)}-{max(default_ages)} ",
        "for {length(ep_names)} endpoint(s)."
      )
    } else {
      stop(
        "No {endpoint}_{age} or {endpoint}_ALL columns found in ",
        path,
        ". Expected columns like copd_25 or copd_ALL."
      )
    }

    log_msg(
      INFO,
      "Detected concentration column \"{conc_col}\" with ",
      "{length(rr_cols)} endpoint–age columns."
    )

    ep_entries <- ep_ages |>
      rowwise() |>
      summarise(
        json = str_glue(
          '      {{ "name": "{endpoint}", "ages": [{str_c(ages, collapse=", ")}] }}'
        ),
        .groups = "drop"
      ) |>
      pull(json)

    config_entry <- str_c(
      '  "',
      Model,
      '": {\n',
      '    "conc_col": "',
      conc_col,
      '",\n',
      '    "endpoints": [\n',
      str_c(ep_entries, collapse = ",\n"),
      '\n',
      '    ]\n',
      '  }'
    )

    log_msg(
      INFO,
      "Detected {nrow(ep_ages)} endpoints: ",
      str_c(ep_ages$endpoint, collapse = ", ")
    )
    log_msg(
      INFO,
      "Total age-group columns parsed: {sum(lengths(ep_ages$ages))}"
    )

    # Preview & auto-append to RR_std_config.json
    cat("\n---- Auto-generated RR_std config entry ----\n")
    cat(config_entry, "\n\n")

    cfg_path <- './Data/RR_std_config.json'
    if (file.exists(cfg_path)) {
      cfg_lines <- readLines(cfg_path)
      if (any(str_detect(cfg_lines, str_glue('^  "{Model}":')))) {
        log_msg(
          WARN,
          'Model "{Model}" already exists in RR_std_config.json — skipping append.'
        )
      } else {
        # Insert before the closing } of the JSON root object
        close_idx <- tail(which(str_detect(str_trim(cfg_lines), '^\\}$')), 1)
        if (length(close_idx) == 0) {
          close_idx <- length(cfg_lines)
        }
        new_lines <- c(cfg_lines[1:(close_idx - 1)], ",", config_entry, "}")
        writeLines(new_lines, cfg_path)
        log_msg(INFO, 'Appended "{Model}" to {cfg_path}.')
      }
    } else {
      # Create new config file
      writeLines(str_c('{\n', config_entry, '\n}\n'), cfg_path)
      log_msg(INFO, 'Created {cfg_path} with "{Model}" entry.')
    }
  }

  # ---- Build standardised RR table via RR_std() ----
  assign(".RR_std_tbl", RR_std(path), envir = globalenv())
}

#' Build the standardised C-R lookup table
#'
#' Reads the RR lookup xlsx (sheets MEAN/LOW/UP), pivots to long format,
#' expands to the endpoint × agegroup grid defined in
#' \code{RR_std_config.json}, and returns a table with columns
#' \code{concentration}, \code{endpoint}, \code{agegroup},
#' \code{CI} (\code{"MEAN"}, \code{"UP"}, \code{"LOW"}), \code{RR}.
#'
#' Called automatically by \code{set_Model()}. The result is stored in
#' the global variable \code{.RR_std_tbl} for downstream consumers.
#'
#' @param custom_path Optional path to a custom RR lookup xlsx.
#'   When \code{NULL} (default), resolves from \code{RR_std_config.json}.
#' @return A data frame in long format.
#' @export
RR_std <- function(custom_path = NULL) {
  # Resolve lookup path
  if (!is.null(custom_path)) {
    lookup <- custom_path
  } else {
    cfg_path <- './Data/RR_std_config.json'
    if (!file.exists(cfg_path)) {
      stop("RR_std config not found: ", cfg_path)
    }
    cfg_all <- jsonlite::fromJSON(cfg_path, simplifyVector = FALSE)
    CR <- get(".CR_Model", envir = globalenv())
    entry <- cfg_all[[CR]]
    if (is.null(entry) && str_detect(CR, '^IER')) {
      entry <- cfg_all[["IER"]]
    }
    if (is.null(entry) || is.null(entry$lookup)) {
      stop("No lookup path for model \"", CR, "\" in ", cfg_path)
    }
    lookup <- entry$lookup
  }

  if (!file.exists(lookup)) {
    stop("RR lookup table not found: ", lookup)
  }

  # Read standardisation config
  cfg_path <- './Data/RR_std_config.json'
  cfg_all <- jsonlite::fromJSON(cfg_path, simplifyVector = FALSE)
  CR <- get(".CR_Model", envir = globalenv())
  cfg <- cfg_all[[CR]]
  conc_col <- cfg$conc_col %||% "concentration"

  # Load all sheets, convert concentration column to string (matchable) for join
  sheets <- readxl::excel_sheets(lookup)
  .raw <- set_names(
    map(sheets, ~ readxl::read_excel(lookup, sheet = .x) |>
          mutate(!!sym(conc_col) := matchable(as.numeric(!!sym(conc_col)), 1))),
    sheets
  )

  # Process each sheet independently (pivot → expand → join → fill → filter),
  # then stack. This avoids the need for complete() and handles missing age
  # columns (e.g. GEMM 85/90/95 use age 80's RR via fill) naturally.
  result <- imap_dfr(.raw, function(df, ci) {
    RR_one <- df |>
      pivot_longer(
        cols = -all_of(conc_col),
        names_to = c("endpoint", "agegroup"),
        names_sep = '_',
        values_to = "RR"
      ) |>
      mutate(endpoint = tolower(endpoint))

    if (nrow(RR_one) == 0) {
      stop("RR sheet \"", ci, "\" is empty after pivot.")
    }

    conc_vals <- df |> pull(!!sym(conc_col))
    lapply(cfg$endpoints, function(ep) {
      ages <- c('ALL', matchable(unlist(ep$ages), 0))
      expand_grid(
        endpoint = ep$name,
        agegroup = ages,
        concentration = conc_vals
      )
    }) |>
      bind_rows() |>
      left_join(RR_one, by = c("concentration", "endpoint", "agegroup")) |>
      group_by(concentration, endpoint) |>
      fill(RR) |>
      ungroup() |>
      filter(agegroup != 'ALL') |>
      mutate(CI = ci)
  }) |>
    mutate(
      concentration = matchable(as.numeric(concentration), 1),
      agegroup = matchable(as.numeric(agegroup), 0)
    ) |>
    select(concentration, endpoint, agegroup, CI, RR)

  log_msg(INFO, "RR table built: {lookup} ({nrow(result)} rows)")
  return(result)
}
