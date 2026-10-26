#====================================================#
# retrieve driving forces to the PM2.5 health burden #
#====================================================#
# Entry script. The decomposition functions live in Code/decomposition.R.
# Edit the configuration block, then source this file or run it with Rscript.
# NOTE 261026: PWRR is computed per domain × endpoint × agegroup; decomposition
# results produced before this date are invalid.
# NOTE 261027: when a concurrent plan is active, missing factor states are built
# in parallel by Code/decomposition.R. Default is sequential.

# Core Module Load ----

library(tidyverse)
library(writexl)
library(readxl)
source('./Code/model.R', encoding = 'UTF8')
source('./Code/data.R', encoding = 'UTF8')
source('./Code/mortality.R', encoding = 'UTF8')
source('./Code/uncertainty.R', encoding = 'UTF8')
source('./Code/aggregation.R', encoding = 'UTF8')
source('./Code/kernel.R', encoding = 'UTF8')
source('./Code/decomposition.R', encoding = 'UTF8')

# Configuration ----

# C-R model: 'IER', 'NCD+LRI', '5COD', 'MRBRT', 'MRBRT2023', 'O3', 'NO2'
MODEL <- 'NCD+LRI'

# Decomposition window and spatial unit.
START_YEAR <- 'base2015'
END_YEAR <- 'SSP1-Baseline_2030'
RESULT_AT <- 'Country'

# Run all 24 sequences (TRUE) or sequence 1 only (FALSE).
RUN24 <- TRUE

# FALSE loads data and functions without running the decomposition.
RUN_DRIVERS <- tolower(Sys.getenv("PM25_RUN_DRIVERS", unset = "true")) == "true"

# Worker count for Code/kernel.R; 1 keeps the sequential plan.
WORKERS <- as.integer(Sys.getenv("PM25_KERNEL_WORKERS", unset = "1"))

# Input data ----
# Edit these paths to point at another instance. Paths are relative to the
# project root.

GRIDS_FILE      <- "./Data/Grid_info_instance_260617.xlsx"
POP_FILE        <- "./Data/GridPop_instance_260617.xlsx"
CONC_REAL_FILE  <- "./Data/GridPM25_instance_260617.xlsx"
MORT_RATE_FILE  <- "./Data/GBD_mortality_instance_260617.xlsx"
AGE_GROUP_FILE  <- "./Data/GBD_agestructure_instance_260617.xlsx"

# C-R function setting ----

set_Model(MODEL)

# Data load ----

read_files(
  Grids = GRIDS_FILE,
  Pop = POP_FILE,
  Conc_real = CONC_REAL_FILE,
  MortRate = MORT_RATE_FILE,
  AgeGroup = AGE_GROUP_FILE,
  conc_unit = "ug/m3"
)

# Concurrency (optional) ----
# Sequential by default. The plan stays active for this R session.
if (!is.na(WORKERS) && WORKERS > 1) {
  kernel_plan(workers = WORKERS)
}

# Run ----

if (RUN_DRIVERS) {
  aggregate_drivers(
    RUN24 = RUN24,
    start.year = START_YEAR,
    end.year = END_YEAR,
    domain = RESULT_AT
  )
}