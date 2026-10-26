# C-R Model configuration module
#   - tell_Model() — format the active C-R model name for output filenames
#   - set_Model()  — set the global .CR_Model and cache the standardised RR table
#   - RR_std()     — build the standardised RR table from the lookup xlsx
#   Modified 261026b: dgt_conc, unit, and range metadata propagate through
#     set_Model()/RR_std(); new tables are checked for key, range, and CI order.

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
#' @param dgt_conc Non-negative integer concentration precision. Defaults to the
#'   current `.DGT_CONC` value, which starts at 1.
#'
#' @export
set_Model <- function(Model, path = NULL, dgt_conc = NULL) {
  if (is.null(dgt_conc)) {
    dgt_conc <- get0(".DGT_CONC", envir = globalenv(), ifnotfound = 1)
  }
  if (length(dgt_conc) != 1 || dgt_conc < 0 || dgt_conc != as.integer(dgt_conc)) {
    stop("dgt_conc must be one non-negative integer.")
  }

  assign(".CR_Model", Model, envir = globalenv())
  assign(".DGT_CONC", dgt_conc, envir = globalenv())
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
      '    "lookup": "',
      path,
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
  assign(".RR_std_tbl", RR_std(path, dgt_conc = dgt_conc), envir = globalenv())
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
#' @param dgt_conc Optional concentration precision. Defaults to `.DGT_CONC`.
#' @return A data frame in long format with `dgt_conc` and metadata attributes.
#' @export
RR_std <- function(custom_path = NULL, dgt_conc = NULL) {
  if (is.null(dgt_conc)) {
    dgt_conc <- get0(".DGT_CONC", envir = globalenv(), ifnotfound = 1)
  }
  if (length(dgt_conc) != 1 || dgt_conc < 0 || dgt_conc != as.integer(dgt_conc)) {
    stop("dgt_conc must be one non-negative integer.")
  }

  # Read config once at the top
  cfg_path <- './Data/RR_std_config.json'
  if (!file.exists(cfg_path)) stop("RR_std config not found: ", cfg_path)
  cfg_all <- jsonlite::fromJSON(cfg_path, simplifyVector = FALSE)
  CR <- get(".CR_Model", envir = globalenv())
  cfg <- cfg_all[[CR]]
  if (is.null(cfg) && str_detect(CR, '^IER')) cfg <- cfg_all[["IER"]]
  if (is.null(cfg)) stop("No RR_std config for model \"", CR, "\" in ", cfg_path)

  # PWRR branch mode: "branch" = one PWRR per CI branch; "mean" = common
  # MEAN PWRR for all branches (keeps CI ordered for very wide curves).
  pwr_branch <- cfg$pwr_branch %||% "branch"
  if (!pwr_branch %in% c("branch", "mean")) {
    stop("pwr_branch must be 'branch' or 'mean' for model ", CR, ".")
  }
  assign(".PWRR_branch", pwr_branch, envir = globalenv())

  conc_col <- cfg$conc_col %||% "concentration"
  conc_unit <- cfg$conc_unit %||% NULL
  conc_range <- cfg$conc_range %||% NULL
  assign(
    ".RR_meta",
    list(
      model = CR,
      conc_unit = conc_unit,
      conc_range = conc_range,
      pwr_branch = pwr_branch
    ),
    envir = globalenv()
  )

  # Resolve lookup path
  lookup <- if (!is.null(custom_path)) custom_path else cfg$lookup
  if (is.null(lookup)) stop("No lookup path for model \"", CR, "\" in ", cfg_path)
  if (!file.exists(lookup)) stop("RR lookup table not found: ", lookup)

  # Load all sheets, convert concentration column to string (matchable) for join
  sheets <- readxl::excel_sheets(lookup)
  required_sheets <- c("MEAN", "LOW", "UP")
  missing_sheets <- setdiff(required_sheets, sheets)
  if (length(missing_sheets) > 0) {
    stop(
      "RR lookup is missing sheet(s): ", paste(missing_sheets, collapse = ", "),
      ". Found: ", paste(sheets, collapse = ", ")
    )
  }
  sheets <- required_sheets

  .raw <- set_names(
    map(sheets, function(sheet) {
      df <- readxl::read_excel(lookup, sheet = sheet)
      if (!conc_col %in% names(df)) {
        stop("RR sheet \"", sheet, "\" has no concentration column \"", conc_col, "\".")
      }
      conc_num <- as.numeric(df[[conc_col]])
      if (any(!is.finite(conc_num))) {
        stop("RR sheet \"", sheet, "\" has non-finite concentration values.")
      }
      if (anyDuplicated(conc_num)) {
        stop("RR sheet \"", sheet, "\" has duplicated concentration values.")
      }
      df |>
        mutate(!!sym(conc_col) := matchable(conc_num, dgt_conc))
    }),
    sheets
  )

  conc_vectors <- map(.raw, ~ as.character(.x[[conc_col]]))
  if (!all(map_lgl(conc_vectors, identical, conc_vectors[[1]]))) {
    stop("RR sheets do not share the same concentration grid.")
  }

  if (!is.null(conc_range)) {
    actual_range <- as.numeric(conc_vectors[[1]]) |> range()
    expected_range <- as.numeric(unlist(conc_range))
    if (
      length(expected_range) != 2 ||
        any(!is.finite(expected_range)) ||
        any(abs(actual_range - expected_range) > 10^(-dgt_conc))
    ) {
      stop(
        "RR concentration range mismatch: config expects ",
        paste(expected_range, collapse = " to "),
        ", lookup contains ",
        paste(actual_range, collapse = " to "),
        "."
      )
    }
  }

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
        .conc = conc_vals
      ) |>
        rename(!!conc_col := .conc)
    }) |>
      bind_rows() |>
      left_join(RR_one, by = c(conc_col, "endpoint", "agegroup")) |>
      group_by(!!sym(conc_col), endpoint) |>
      fill(RR) |>
      ungroup() |>
      filter(agegroup != 'ALL') |>
      mutate(CI = ci)
  }) |>
    mutate(
      !!sym(conc_col) := matchable(as.numeric(!!sym(conc_col)), dgt_conc),
      agegroup = matchable(as.numeric(agegroup), 0)
    ) |>
    rename(concentration = !!sym(conc_col)) |>
    select(concentration, endpoint, agegroup, CI, RR)

  n_bad_rr <- sum(!is.finite(result$RR))
  if (n_bad_rr > 0) {
    log_msg(
      WARN,
      "{n_bad_rr} non-finite RR value(s) in {lookup}. ",
      "Legacy tables load unchanged; Mortality() drops affected rows."
    )
  }
  if (anyDuplicated(result[c("concentration", "endpoint", "agegroup", "CI")])) {
    stop("RR table contains duplicated concentration/endpoint/agegroup/CI keys.")
  }

  ci_order <- result |>
    select(concentration, endpoint, agegroup, CI, RR) |>
    pivot_wider(names_from = CI, values_from = RR)
  n_ci_order <- sum(ci_order$LOW > ci_order$MEAN + 1e-10, na.rm = TRUE) +
    sum(ci_order$MEAN > ci_order$UP + 1e-10, na.rm = TRUE)
  if (n_ci_order > 0) {
    log_msg(
      WARN,
      "{n_ci_order} RR row(s) violate LOW <= MEAN <= UP in {lookup}. ",
      "This legacy table is loaded unchanged; rebuilds must pass the strict guard."
    )
  }

  attr(result, "dgt_conc") <- dgt_conc
  attr(result, "conc_unit") <- conc_unit
  attr(result, "conc_range") <- conc_range
  attr(result, "pwr_branch") <- pwr_branch

  log_msg(INFO, "RR table built: {lookup} ({nrow(result)} rows)")
  result
}
