# C-R Model configuration module
#   - tell_Model() — format the active C-R model name for output filenames
#   - set_Model()  — set the global .CR_Model (IER, GEMM, MRBRT, O3, NO2)

library(tidyverse)
library(writexl)
library(readxl)

#' detect CRF model name and generate a name for file output
#'
#' @return formatted string of CRF name
#' @export
#'
tell_Model <- function() {
  return(
    if (.CR_Model %>% str_detect('IER')) {
      str_c(.CR_Model)
    } else if (.CR_Model %in% c('5COD', 'NCD+LRI')) {
      str_c('PM2.5_GEMM', .CR_Model, sep = '_')
    } else if (.CR_Model == 'MRBRT') {
      str_c("PM2.5_", .CR_Model)
    } else if (.CR_Model == 'O3') {
      #新增
      str_c(.CR_Model)
    } else if (.CR_Model == 'NO2') {
      str_c(.CR_Model)
    } #新增
  )
}

#' set CRF for calculation
#'
#' @param Model string, one of the built-in models (`IER`, `NCD+LRI`, `5COD`,
#'   `MRBRT`, `O3`, `NO2`) or a custom name.
#' @param base For custom models: which built-in model's endpoint and agegroup
#'   structure to reuse (e.g. `base = "NCD+LRI"` for a custom GEMM table).
#'   Ignored when `Model` is a built-in.
#'
#' @export
set_Model <- function(Model, base = NULL) {
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
}
