# C-R Model configuration module
#   - tell_Model() — format the active C-R model name for output filenames
#   - set_Model()  — set the global .CR_Model (IER, GEMM, MRBRT, O3, NO2)

library(tidyverse)
library(writexl)
library(readxl)
library(jsonlite)

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
  cat(str_glue("C-R Model \"{tell_Model()}\" is set.\n"))

  # ---- Auto-generate standardisation config from custom lookup table ----
  if (!is.null(path)) {
    if (!file.exists(path))
      stop("Custom RR lookup table not found: ", path)

    raw <- read_xlsx(path, sheet = "MEAN")
    mort_cols <- str_subset(names(raw), '_[0-9]+$')
    if (length(mort_cols) == 0)
      stop("No {endpoint}_{age} columns found in ", path,
           ". Expected pattern like copd_25 or ncd+lri_30.")

    parsed <- str_match(mort_cols, '^(.+)_([0-9]+)$')
    ep_ages <- tibble(
      endpoint = str_to_lower(parsed[, 2]),
      age = as.integer(parsed[, 3])
    ) %>%
      group_by(endpoint) %>%
      summarise(ages = list(sort(unique(age))), .groups = "drop")

    ep_entries <- ep_ages %>% rowwise() %>%
      summarise(
        json = str_glue('      {{ "name": "{endpoint}", "ages": [{str_c(ages, collapse=", ")}] }}'),
        .groups = "drop"
      ) %>% pull(json)

    config_entry <- str_c(
      '  "', Model, '": {\n',
      '    "endpoints": [\n',
      str_c(ep_entries, collapse = ",\n"), '\n',
      '    ]\n',
      '  }'
    )

    cat("\n---- Auto-generated RR_std config entry ----\n")
    cat("Review and add the following to Data/RR_std_config.json:\n\n")
    cat(config_entry, "\n\n")
    cat(str_glue("Detected {nrow(ep_ages)} endpoints: ",
                 str_c(ep_ages$endpoint, collapse = ", "), "\n"))
    cat(str_glue("Total age-group columns parsed: {sum(lengths(ep_ages$ages))}\n\n"))

    assign(".CR_Config", list(
      endpoints = lapply(seq_len(nrow(ep_ages)), function(i) {
        list(name = ep_ages$endpoint[i], ages = ep_ages$ages[[i]])
      })
    ), envir = globalenv())
  }
}
