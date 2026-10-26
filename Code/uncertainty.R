# Uncertainty module
#   Modified 261026b: the old PAF-at-PWE implementation was removed. It did not
#   match the PWRR estimator. Uncertainty is now computed by aggregate_sigma()
#   from grid-level Mortality() branches and the same PWRR formula.

#' Retired PAF/PWE uncertainty function
#'
#' @param ... Ignored.
#'
#' @return This function always aborts.
#' @export
Uncertainty <- function(...) {
  cli::cli_abort(c(
    "x" = "Uncertainty() has been retired.",
    "i" = paste0(
      "It used RR at population-weighted exposure and PAF = 1 - 1/RR, ",
      "which does not match the PWRR estimator."
    ),
    "i" = paste0(
      "Use aggregate_sigma() with scenario names. It computes bounds from ",
      "grid-level Mortality() branches."
    )
  ))
}
