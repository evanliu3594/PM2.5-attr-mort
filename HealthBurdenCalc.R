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
#       update 260610: add costume CRF support                                         #
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

# C-R Model setting ----
# this section used to choose C-R Model for Health Impact Calculation.
# Supported C-R functions:
#     'IER', 'NCD+LRI'(Part of GEMM), '5COD'(Part of GEMM), 'MRBRT', 'O3', 'NO2'
set_Model('NCD+LRI')

# Data load ----

read_files(
  Grids = './Data/GRID_information_instance_220628.xlsx',
  Pop = './Data/GridPop_instance_231220.xlsx',
  Conc_real = './Data/GridPM25_instance_231220.xlsx',
  Conc_cf = './Data/GridPM25_cf_instance_260325.xlsx', # PM_cf works only in counter-fact scenario
  MortRate = "./Data/GBD_mortality_instance_231220.xlsx",
  AgeGroup = './Data/GBD_agestructure_instance_231220.xlsx',
  dgt_grid = 2
)

scenarios <- names(Conc_real)[c(-1:-2)]

# Grid-level calculation with 95% CI (MEAN + UP + LOW in one pass) ----
grid_ci <- map(set_names(scenarios), ~ Mortality_at(at = .x, CI = "RANGE"))
# grid_mean <- map(set_names(scenarios), ~ Mortality_at(at = .x, CI = "MEAN"))
# grid_low <- map(set_names(scenarios), ~ Mortality_at(at = .x, CI = "LOW"))
# grid_up <- map(set_names(scenarios), ~ Mortality_at(at = .x, CI = "UP"))

# Aggregation ----
# Uncomment the level(s) and breakdown(s) you need.
# at  = "grid" | "geo" | "x" | "y" | "Country" | or any column in Grid_info
# by  = "total" (Total only) | "all" | "endpoint" | "agegroup"
# write = FALSE | TRUE (-> ./Result/) | "./my/path"

## Grid-level ----
aggregate_range(grid_ci, at = "grid", by = "total", write = FALSE) # by Total
# aggregate_range(grid_ci, at = "grid", by = "endpoint", write = FALSE) # by endpoint
# aggregate_range(grid_ci, at = "grid", by = "agegroup", write = FALSE) # by agegroup
# aggregate_range(grid_ci, at = "grid", by = "all", write = FALSE) # by endpoint and agegroup

## Zonal / Meridional ----
# aggregate_range(grid_ci, at = "x", by = "total", write = FALSE) # by Total
aggregate_range(grid_ci, at = "x", by = "endpoint", write = FALSE) # by endpoint
# aggregate_range(grid_ci, at = "x", by = "agegroup", write = FALSE) # by agegroup
# aggregate_range(grid_ci, at = "x", by = "all", write = FALSE) # by endpoint and agegroup

# aggregate_range(grid_ci, at = "y", by = "total", write = FALSE) # by Total
# aggregate_range(grid_ci, at = "y", by = "endpoint", write = FALSE) # by endpoint
aggregate_range(grid_ci, at = "y", by = "agegroup", write = FALSE) # by agegroup
# aggregate_range(grid_ci, at = "y", by = "all", write = FALSE) # by endpoint and agegroup

## National ----
# aggregate_range(grid_ci, at = "Country", by = "total",    write = FALSE) # by Total
# aggregate_range(grid_ci, at = "Country", by = "endpoint", write = FALSE) # by endpoint
# aggregate_range(grid_ci, at = "Country", by = "agegroup", write = FALSE) # by agegroup
# aggregate_range(grid_ci, at = "Country", by = "all", write = FALSE) # by endpoint and agegroup

# Provincial (if available)----
# aggregate_range(grid_ci, at = "Province", by = "total",    write = FALSE) # Total
# aggregate_range(grid_ci, at = "Province", by = "endpoint", write = FALSE) # endpoint
# aggregate_range(grid_ci, at = "Province", by = "agegroup", write = FALSE) # agegroup
# aggregate_range(grid_ci, at = "Province", by = "all",      write = FALSE) # by endpoint and agegroup

## Regional (if available)----
# aggregate_range(grid_ci, at = "Region", by = "total",    write = FALSE) # Total
# aggregate_range(grid_ci, at = "Region", by = "endpoint", write = FALSE) # endpoint
# aggregate_range(grid_ci, at = "Region", by = "agegroup", write = FALSE) # agegroup
# aggregate_range(grid_ci, at = "Region", by = "all",      write = FALSE) # by endpoint and agegroup

##  All geo levels and all group field at once ----
# aggregate_range(grid_ci, at = "geo", by = "total", write = TRUE)
# aggregate_range(grid_ci, at = "geo", by = "all", write = TRUE)

# Uncertainty propagation algorithm ====
grid_mean <- map(set_names(scenarios), ~ Mortality_at(at = .x, CI = "MEAN"))
aggregate_sigma(grid_mean, at = "Country", write = FALSE)
