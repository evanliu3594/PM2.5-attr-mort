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
  source_range <- range(raw$exposure)
  if (min(target) < source_range[1] || max(target) > source_range[2]) {
    cli::cli_abort(
      "Target range 0-{max_conc} ppb exceeds source range ",
      "{source_range[1]}-{source_range[2]} ppb."
    )
  }
  interp <- function(y) {
    approx(
      x = raw$exposure,
      y = y,
      xout = target,
      method = 'linear',
      rule = 1
    )$y
  }

  # Only the NO2 lower bound is approved to be floored at RR = 1. Other
  # branches must already satisfy the project's RR >= 1 guard.
  rr_mean <- exp(interp(raw$mean))
  rr_low <- exp(interp(raw$lower))
  rr_up <- exp(interp(raw$upper))

  if (identical(name, "NO2")) {
    n_floored <- sum(rr_low < 1)
    if (n_floored > 0) {
      cli::cli_inform(
        "Floored {n_floored} NO2 LOW value(s) at RR = 1."
      )
    }
    rr_low <- pmax(rr_low, 1)
  }

  below_one <- c(
    LOW = sum(rr_low < 1),
    MEAN = sum(rr_mean < 1),
    UP = sum(rr_up < 1)
  )
  if (any(below_one > 0)) {
    cli::cli_abort(
      "Unapproved RR < 1 values: {str_c(names(below_one)[below_one > 0], '=', below_one[below_one > 0], collapse = ', ')}."
    )
  }

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
  for (sheet in sheets) {
    values <- as.matrix(sheet[-1])
    stopifnot(all(is.finite(values)))
    stopifnot(all(values >= 1))
  }
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
