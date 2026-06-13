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
grid_ci <- set_names(scenarios) |>
  map(~ Mortality_at(at = .x, CI = "RANGE"))

# Aggregation ----
# Uncomment the level(s) and breakdown(s) you need.
# at  = "grid" | "geo" | "x" | "y" | "Country" | or any column in Grid_info
# by  = NULL | "total" (Total only) | "all" | "endpoint" | "agegroup"
# write = FALSE | TRUE (-> ./Result/) | "./my/path"

## Grid-level ----
# aggregate_mort(grid_ci, at = "grid", by = NULL,       write = FALSE)
# aggregate_mort(grid_ci, at = "grid", by = "endpoint", write = FALSE)
# aggregate_mort(grid_ci, at = "grid", by = "agegroup", write = FALSE)
# aggregate_mort(grid_ci, at = "grid", by = "all",      write = FALSE)

## Zonal / Meridional ----
# aggregate_mort(grid_ci, at = "x", write = FALSE)   # sum along latitude bands -> by longitude
# aggregate_mort(grid_ci, at = "y", write = FALSE)   # sum along longitude bands -> by latitude

## National ----
# aggregate_mort(grid_ci, at = "Country", by = NULL,       write = FALSE) # Total
# aggregate_mort(grid_ci, at = "Country", by = "endpoint", write = FALSE) # x endpoint
# aggregate_mort(grid_ci, at = "Country", by = "agegroup", write = FALSE) # x agegroup
# aggregate_mort(grid_ci, at = "Country", by = "all",      write = FALSE) # all above

## Provincial (if available)----
# aggregate_mort(grid_ci, at = "Province", by = NULL,       write = FALSE) # Total
# aggregate_mort(grid_ci, at = "Province", by = "endpoint", write = FALSE) # x endpoint
# aggregate_mort(grid_ci, at = "Province", by = "agegroup", write = FALSE) # x agegroup
# aggregate_mort(grid_ci, at = "Province", by = "all",      write = FALSE) # all above

## Regional (if available)----
# aggregate_mort(grid_ci, at = "Region", by = "all", write = FALSE)

##  All geo levels and all group field at once ----
aggregate_mort(grid_ci, at = "geo", by = "all", write = FALSE)
