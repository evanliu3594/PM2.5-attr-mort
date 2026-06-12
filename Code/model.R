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
#' @param Model string, one of the built-in models (`IER`, `NCD+LRI`, `5COD`,
#'   `MRBRT`, `O3`, `NO2`) or a custom name.
#' @param base For custom models: which built-in model's endpoint and agegroup
#'   structure to reuse (e.g. `base = "NCD+LRI"` for a custom GEMM table).
#'   Ignored when `Model` is a built-in.
#' @param path Optional path to a custom RR lookup table (xlsx). When provided,
#'   the MEAN sheet is read and its `{endpoint}_{agegroup}` columns are parsed
#'   to auto-generate a standardisation config entry. The user should review
#'   and add it to `Data/RR_std_config.json`.
#'
#' @export
set_Model <- function(Model, base = NULL, path = NULL) {
  assign(".CR_Model", Model, envir = globalenv())

  builtins <- c("IER", "NCD+LRI", "5COD", "MRBRT", "O3", "NO2",
                "IER2010", "IER2013", "IER2015", "IER2017")
  is_builtin <- Model %in% builtins

  if (is_builtin) {
    assign(".CR_Base", Model, envir = globalenv())
    cat(str_glue("C-R Model \"{tell_Model()}\" is set as the default methodology.\n"))
  } else if (!is.null(base) && base %in% builtins) {
    assign(".CR_Base", base, envir = globalenv())
    cat(str_glue("Custom C-R Model \"{Model}\" (base: {base}) is set.\n"))
  } else {
    assign(".CR_Base", Model, envir = globalenv())
    warning(str_glue("\"{Model}\" is not a built-in CR model. ",
      "Pass base = \"NCD+LRI\" (or similar) so RR_std() knows the ",
      "endpoint/agegroup structure to use. ",
      "You must also provide a custom RR_table via read_files(RR_table_path = ...)."))
  }

  # ---- Auto-generate standardisation config from custom lookup table ----
  if (!is.null(path)) {
    if (!file.exists(path))
      stop("Custom RR lookup table not found: ", path)

    raw <- read_xlsx(path, sheet = "MEAN")
    # Parse columns like copd_25, ncd+lri_30, allcause_15 → endpoint_age
    mort_cols <- str_subset(names(raw), '_[0-9]+$')
    if (length(mort_cols) == 0)
      stop("No {endpoint}_{age} columns found in ", path,
           ". Expected pattern like copd_25 or ncd+lri_30.")

    # Extract endpoint name and age from each column
    parsed <- str_match(mort_cols, '^(.+)_([0-9]+)$')
    ep_ages <- tibble(
      endpoint = str_to_lower(parsed[, 2]),
      age = as.integer(parsed[, 3])
    ) %>%
      group_by(endpoint) %>%
      summarise(ages = list(sort(unique(age))), .groups = "drop")

    # Build config entry
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

    # Also store the parsed structure so RR_std can use it without the JSON file
    assign(".CR_Config", list(
      endpoints = lapply(seq_len(nrow(ep_ages)), function(i) {
        list(name = ep_ages$endpoint[i], ages = ep_ages$ages[[i]])
      })
    ), envir = globalenv())
  }
}
