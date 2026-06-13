# C-R Model configuration module
#   - tell_Model() — format the active C-R model name for output filenames
#   - set_Model()  — set the global .CR_Model (IER, GEMM, MRBRT, O3, NO2)

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
    mort_cols <- str_subset(names(raw), '_[0-9]+$')

    if (length(mort_cols) > 0) {
      parsed <- str_match(mort_cols, '^(.+)_([0-9]+)$')
      ep_ages <- tibble(
        endpoint = str_to_lower(parsed[, 2]),
        age = as.integer(parsed[, 3])
      ) %>%
        group_by(endpoint) %>%
        summarise(ages = list(sort(unique(age))), .groups = "drop")
    } else {
      all_cols <- str_subset(names(raw), '_ALL$')
      if (length(all_cols) == 0) {
        stop(
          "No {endpoint}_{age} or {endpoint}_ALL columns found in ",
          path,
          ". Expected columns like copd_25 or copd_ALL."
        )
      }
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
    }

    ep_entries <- ep_ages %>%
      rowwise() %>%
      summarise(
        json = str_glue(
          '      {{ "name": "{endpoint}", "ages": [{str_c(ages, collapse=", ")}] }}'
        ),
        .groups = "drop"
      ) %>%
      pull(json)

    config_entry <- str_c(
      '  "',
      Model,
      '": {\n',
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
}
