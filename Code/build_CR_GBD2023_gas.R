# Build GBD 2023 O3 and NO2 RR lookup tables for the project.
#
# Input:
#   Data/IHME_GBD_2023_AIR_POLLUTION_1990_2021_RR_CURVES.zip
# Output:
#   Data/RR_index/O3_Lookup_Table_GBD2023_Build_261026.xlsx
#   Data/RR_index/NO2_Lookup_Table_GBD2023_Build_261026.xlsx
#
# Run:
#   source('./Code/build_CR_GBD2023_gas.R')
#
# Notes:
#   - GBD 2023 O3 and NO2 source files are on the log(RR) scale.
#   - Source concentration units are ppb. The output concentration column is
#     also ppb; input concentration data must use ppb.
#   - GBD 2023 O3 covers 0-50 ppb; NO2 covers 0-100 ppb. Values outside the
#     lookup range are clamped by Mortality().
#   - The NO2 lower confidence bound falls below RR = 1 at low/high exposure.
#     It is floored at 1 so the project's RR >= 1 and PWRR guard remain valid.
#   - GBD 2023 NO2 is an asthma curve. GBD 2023 O3 is a COPD curve.

library(tidyverse)
library(writexl)

# ---------- config ----------

zip_path <- './Data/IHME_GBD_2023_AIR_POLLUTION_1990_2021_RR_CURVES.zip'

gas_specs <- tribble(
  ~name,  ~pattern,                    ~endpoint,    ~max_conc, ~out_path,
  'O3',   'OZONE_RR_COPD_MEAN',        'copd',       50,        './Data/RR_index/O3_Lookup_Table_GBD2023_Build_261026.xlsx',
  'NO2',  'NO2_RR_ASTHMA_MEAN',        'asthma',     100,       './Data/RR_index/NO2_Lookup_Table_GBD2023_Build_261026.xlsx'
)

# ---------- input check ----------

if (!file.exists(zip_path)) {
  cli::cli_abort(c(
    'x' = 'GBD 2023 RR zip not found: {zip_path}',
    'i' = 'Download it from the GHDx record listed in Data/RR_index/README.md.'
  ))
}

zip_files <- unzip(zip_path, list = TRUE)$Name

# ---------- build one gas lookup ----------

build_gas_lookup <- function(name, pattern, endpoint, max_conc, out_path) {
  hit <- str_subset(zip_files, pattern)
  if (length(hit) != 1) {
    cli::cli_abort('Expected one file matching "{pattern}", found {length(hit)}.')
  }

  tmp_dir <- tempfile('gbd2023_gas_')
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  unzip(zip_path, files = hit, exdir = tmp_dir)

  raw <- read_csv(file.path(tmp_dir, hit), show_col_types = FALSE) |>
    mutate(across(c(exposure, mean, lower, upper), as.numeric)) |>
    arrange(exposure)

  # Source is log(RR). Interpolate in log space to the project's 0.1 grid.
  target <- seq(0, max_conc, 0.1)
  interp <- function(y) {
    approx(
      x = raw$exposure,
      y = y,
      xout = target,
      method = 'linear',
      rule = 2
    )$y
  }

  # Floor all branches at RR = 1. The NO2 lower bound is below 1 at some
  # exposures; the project's PWRR guard and RR >= 1 assumption require this.
  rr_mean <- pmax(exp(interp(raw$mean)), 1)
  rr_low <- pmax(exp(interp(raw$lower)), 1)
  rr_up <- pmax(exp(interp(raw$upper)), 1)

  col_name <- str_c(endpoint, 'ALL', sep = '_')
  make_sheet <- function(rr) {
    out <- tibble(concentration = target)
    out[[col_name]] <- rr
    out
  }

  sheets <- list(
    LOW = make_sheet(rr_low),
    MEAN = make_sheet(rr_mean),
    UP = make_sheet(rr_up)
  )

  stopifnot(nrow(sheets$MEAN) == length(target))
  stopifnot(identical(names(sheets$MEAN), c('concentration', col_name)))
  stopifnot(all(is.finite(as.matrix(sheets$MEAN[-1]))))
  stopifnot(all(as.matrix(sheets$MEAN[-1]) >= 1))
  stopifnot(all(as.matrix(sheets$LOW[-1]) <= as.matrix(sheets$MEAN[-1]) + 1e-12))
  stopifnot(all(as.matrix(sheets$MEAN[-1]) <= as.matrix(sheets$UP[-1]) + 1e-12))
  stopifnot(all(abs(sheets$MEAN[sheets$MEAN$concentration == 0, -1] - 1) < 1e-10))

  write_xlsx(sheets, out_path)
  cli::cli_inform(
    'Wrote {out_path} ({nrow(sheets$MEAN)} rows, endpoint {endpoint}, 0-{max_conc} ppb).'
  )
}

# ---------- run ----------

pwalk(gas_specs, build_gas_lookup)
