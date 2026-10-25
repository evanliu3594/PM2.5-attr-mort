# Build the GBD 2023 PM2.5 RR lookup table for the MRBRT2023 model.
#
# Input: GBD 2023 air pollution RR zip (see zip_path below).
# Source: GHDx GBD 2023 Air Pollution Exposure Estimates and Risk Curves
#   https://ghdx.healthdata.org/record/ihme-data/
#   gbd-2023-air-pollution-exposure-estimates-1990-2023
# Output: MR-BRT lookup workbook (see out_path below).
#
# Run this script by sourcing it from the project root.
#
# Notes:
#   - The source files use two scales: RR for COPD, diabetes, and lung cancer;
#     log(RR) for IHD, stroke, and LRI. The script detects the scale from the
#     first positive-exposure mean.
#   - The source grid is 0-10 by 0.1 and 11-2500 by 1. The script interpolates
#     log(RR) linearly to 0-300 by 0.1, matching the project's join grid.
#   - Only the six mortality endpoints used by MRBRT2021 are compiled.
#     Dementia, birth weight, and gestational age are not included.

library(tidyverse)
library(writexl)

# ---------- config ----------

zip_path <- "./Data/IHME_GBD_2023_AIR_POLLUTION_1990_2021_RR_CURVES.zip"
out_path <- "./Data/RR_index/MRBRT2023_Lookup_Table_Build_261026.xlsx"
target_dose <- seq(0, 300, 0.1)

endpoint_patterns <- c(
  copd = "PM_RR_COPD_MEAN",
  dm2 = "PM_RR_DIABETES_MEAN",
  ihd = "PM_RR_ISCHEMIC_HEART_DISEASE_MEAN",
  lc = "PM_RR_LUNG_CANCER_MEAN",
  lri = "PM_RR_LOWER_RESPIRATORY_INFECTIONS_MEAN",
  stroke = "PM_RR_STROKE_MEAN"
)

# ---------- input check ----------

if (!file.exists(zip_path)) {
  cli::cli_abort(c(
    "x" = "GBD 2023 RR zip not found: {zip_path}",
    "i" = "Download it from the GHDx record listed at the top of this script."
  ))
}

zip_files <- unzip(zip_path, list = TRUE)$Name

# Match one MEAN file per endpoint.
match_curve_file <- function(pattern) {
  hit <- str_subset(zip_files, pattern)
  if (length(hit) != 1) {
    cli::cli_abort(
      "Expected one file matching \"{pattern}\", found {length(hit)}."
    )
  }
  hit
}

curve_files <- map_chr(endpoint_patterns, match_curve_file)

# ---------- read ----------

# Extract only the six small MEAN files.
tmp_dir <- tempfile("gbd2023_rr_")
dir.create(tmp_dir)
on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
unzip(zip_path, files = curve_files, exdir = tmp_dir)

read_curve <- function(file) {
  raw <- read_csv(file.path(tmp_dir, file), show_col_types = FALSE)

  # Old files carry a `cause` column; new files use `risk` as exposure.
  if ("cause" %in% names(raw)) {
    raw <- raw |> select(exposure, mean, lower, upper)
  } else {
    raw <- raw |>
      rename(exposure = risk) |>
      select(exposure, mean, lower, upper)
  }

  raw |>
    mutate(across(c(exposure, mean, lower, upper), as.numeric)) |>
    filter(!is.na(exposure)) |>
    arrange(exposure)
}

curves <- map(curve_files, read_curve)

# Detect RR vs log(RR). Old files start near 1; new files start near 0.
is_log_curve <- function(df) {
  first_mean <- df |> filter(exposure > 0) |> slice(1) |> pull(mean)
  first_mean < 0.5
}

is_log <- map_lgl(curves, is_log_curve)

if (any(is_log)) {
  cli::cli_inform(
    "Log-scale curves: {str_c(names(is_log)[is_log], collapse = \", \")}"
  )
}

to_log_scale <- function(df, log_scale) {
  if (log_scale) {
    df |>
      mutate(
        log_mean = mean,
        log_lower = lower,
        log_upper = upper
      )
  } else {
    df |>
      mutate(
        log_mean = log(mean),
        log_lower = log(lower),
        log_upper = log(upper)
      )
  }
}

curves <- map2(curves, is_log, to_log_scale)

# ---------- interpolate ----------

# Interpolate log(RR) so the result stays positive and smooth.
interp_log <- function(df, column) {
  approx(
    x = df$exposure,
    y = df[[column]],
    xout = target_dose,
    method = "linear",
    rule = 2
  )$y
}

build_sheet <- function(stat) {
  make_col <- function(df, endpoint) {
    tibble(!!str_c(endpoint, "_ALL") := exp(interp_log(df, stat)))
  }
  cols <- imap(curves, make_col)
  reduce(cols, bind_cols, .init = tibble(dose = target_dose))
}

sheets <- list(
  LOW = build_sheet("log_lower"),
  MEAN = build_sheet("log_mean"),
  UP = build_sheet("log_upper")
)

# ---------- validate ----------

expected_cols <- c("dose", str_c(names(curves), "_ALL"))

check_sheet <- function(df) {
  stopifnot(identical(names(df), expected_cols))
  stopifnot(nrow(df) == length(target_dose))
  stopifnot(all(df$dose == target_dose))
  stopifnot(all(is.finite(as.matrix(df[-1]))))
  stopifnot(all(as.matrix(df[-1]) >= 1))
}

walk(sheets, check_sheet)

stopifnot(all(abs(sheets$MEAN[sheets$MEAN$dose == 0, -1] - 1) < 1e-10))

# ---------- write ----------

write_xlsx(sheets, out_path)
cli::cli_inform(
  "Wrote {out_path} ({nrow(sheets$MEAN)} rows)."
)
cli::cli_inform(
  "Add model \"MRBRT2023\" to Data/RR_std_config.json if absent."
)
