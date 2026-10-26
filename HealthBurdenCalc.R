#======================================================================================#
#      oooooooooo oooo     oooo                                                        #
#      888    888 8888o   888   ooooo    8888888                                       #
#     888oooo88  88 888o8 88  o   o88   8oooooo                                        #
#    888        88  888  88     o8          o88                                        #
#  o888o      o88o  8  o88o o88ooo8 88 88oo88                                          #
#                                                                                      #
#        o        o8     o8                 oooo     oooo                        o8    #
#       888     o888oo o888oo oo oooooo     8888o   888   ooooooo  oo oooooo  o888oo   #
#      8  88    888    888    888     88   88 888o8 88 888     888 888    888 888      #
#     8ooo888  888    888    888          88  888  88 888     888 888        888       #
#   o8o  o888o 888o   888o o888o        o88o  8  o88o  88ooo88  o888o        888o      #
#--------------------------------------------------------------------------------------#
#       -- An Integrated Calculate Program for PM2.5 Health Burden Assessment --       #
#                                                                                      #
#  Created By Yifan LIU on 20210505                                                    #
#       update 210525: replace matrix calculation by tidy-R Grammar                    #
#       update 220602: simplify code                                                   #
#       *Add GEMM model support                                                        #
#       *Add better uncertainty assessments and improved the result return             #
#       update 260610: add custom CRF support                                          #
#       update 260618: modular engine refactor, verbose/debug params                   #
#       update 261026: PWRR per domain × endpoint × agegroup; CI branches use          #
#                      matching PWRR; old single-domain PWRR results invalid            #
#                                                                                      #
#======================================================================================#

# Install dependencies (run once) ----
# install.packages(c("tidyverse", "jsonlite", "writexl", "readxl"))

# Core functions load ----

library(tidyverse)
library(writexl)
library(readxl)
source('./Code/model.R', encoding = 'UTF8')
source('./Code/data.R', encoding = 'UTF8')
source('./Code/mortality.R', encoding = 'UTF8')
source('./Code/uncertainty.R', encoding = 'UTF8')
source('./Code/aggregation.R', encoding = 'UTF8')

# Concurrency (optional) ----
# Sequential by default. Export PM25_KERNEL_WORKERS = 8 to run Mortality()
# groups in parallel. The plan stays active for this R session; kernel_off()
# restores the sequential plan.

worker_n <- as.integer(Sys.getenv("PM25_KERNEL_WORKERS", unset = "1"))
if (!is.na(worker_n) && worker_n > 1) {
  kernel_plan(workers = worker_n)
}

# C-R Model setting ----
# this section used to choose C-R Model for Health Impact Calculation.
# Supported C-R functions: 'IER', 'NCD+LRI'(Part of GEMM), '5COD'(Part of GEMM), 'MRBRT', 'MRBRT2023', 'O3', 'NO2'

# set_Model('MRBRT')       # GBD 2023 default alias
# set_Model('MRBRT2023')   # GBD 2023 MR-BRT (explicit)
set_Model('NCD+LRI')
# set_Model('5COD')
# set_Model('IER')
# set_Model('O3')          # GBD 2023 O3 (COPD)
# set_Model('NO2')         # GBD 2023 NO2 (asthma)

# Data load ----

instance_files <- resolve_instance_files()
read_files(
  Grids = instance_files$Grids,
  Pop = instance_files$Pop,
  Conc_real = instance_files$Conc_real,
  Conc_cf = instance_files$Conc_cf,
  MortRate = instance_files$MortRate,
  AgeGroup = instance_files$AgeGroup,
  dgt_grid = 2,
  conc_unit = "ug/m3"
)

scenarios <- get_scenarios()

# Grid-level calculation with 95% CI (MEAN + UP + LOW in one pass) ----
grid_ci <- map(set_names(scenarios), ~ Mortality_at(at = .x, CI = "RANGE"))
# grid_mean <- map(set_names(scenarios), ~ Mortality_at(at = .x, CI = "MEAN"))
# grid_low <- map(set_names(scenarios), ~ Mortality_at(at = .x, CI = "LOW"))
# grid_up <- map(set_names(scenarios), ~ Mortality_at(at = .x, CI = "UP"))

# Result output ----
# Set WRITE_RESULTS = FALSE when stepping through the script without writing.
WRITE_RESULTS <- TRUE
RESULT_AT <- "Country"
if (WRITE_RESULTS) {
  walk(
    c("total", "endpoint", "agegroup", "all"),
    ~ aggregate_range(
      grid_ci,
      at = RESULT_AT,
      by = .x,
      write = TRUE
    )
  )
}

# Aggregation ----
# Uncomment the level(s) and breakdown(s) you need.
# at  = "grid" | "geo" | "x" | "y" | "Country" | or any column in Grid_info
# by  = "total" (Total only) | "all" | "endpoint" | "agegroup"
# write = FALSE | TRUE (write to ./Results/xxx)

## Grid-level ----
# aggregate_range(grid_ci, at = "grid", by = "total", write = FALSE)
# aggregate_range(grid_ci, at = "grid", by = "endpoint", write = FALSE)
# aggregate_range(grid_ci, at = "grid", by = "agegroup", write = FALSE)
# aggregate_range(grid_ci, at = "grid", by = "all", write = FALSE)

## Zonal / Meridional ----
# aggregate_range(grid_ci, at = "x", by = "total", write = FALSE)
# aggregate_range(grid_ci, at = "x", by = "endpoint", write = FALSE)
# aggregate_range(grid_ci, at = "x", by = "agegroup", write = FALSE)
# aggregate_range(grid_ci, at = "x", by = "all", write = FALSE)

# aggregate_range(grid_ci, at = "y", by = "total", write = FALSE)
# aggregate_range(grid_ci, at = "y", by = "endpoint", write = FALSE)
# aggregate_range(grid_ci, at = "y", by = "agegroup", write = FALSE)
# aggregate_range(grid_ci, at = "y", by = "all", write = FALSE)

## National ----
# aggregate_range(grid_ci, at = "Country", by = "total", write = FALSE)
# aggregate_range(grid_ci, at = "Country", by = "endpoint", write = FALSE)
# aggregate_range(grid_ci, at = "Country", by = "agegroup", write = FALSE)
# aggregate_range(grid_ci, at = "Country", by = "all", write = FALSE)

## Provincial (if available)----
# aggregate_range(grid_ci, at = "Province", by = "total", write = FALSE)
# aggregate_range(grid_ci, at = "Province", by = "endpoint", write = FALSE)
# aggregate_range(grid_ci, at = "Province", by = "agegroup", write = FALSE)
# aggregate_range(grid_ci, at = "Province", by = "all", write = FALSE)

# # Regional (if available)----
# aggregate_range(grid_ci, at = "Region", by = "total", write = FALSE)
# aggregate_range(grid_ci, at = "Region", by = "endpoint", write = FALSE)
# aggregate_range(grid_ci, at = "Region", by = "agegroup", write = FALSE)
# aggregate_range(grid_ci, at = "Region", by = "all", write = FALSE)

##  All geo levels and all group field at once ----
# aggregate_range(grid_ci, at = "geo", by = "total", write = FALSE)
# aggregate_range(grid_ci, at = "geo", by = "all", write = FALSE)

# PWRR-consistent uncertainty ====
# aggregate_sigma() recomputes MEAN/UP/LOW from the PWRR estimator.
# sigma_result <- aggregate_sigma(grid_ci, at = "Country", by = "total", write = FALSE)
