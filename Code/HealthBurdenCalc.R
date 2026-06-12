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
#       *Add GEMM model support                                                        #
#       *Add better uncertainty assessments and improved the result return             #
#       update 210525: replace matrix calculation by tidy-R Grammar                    #
#       update 220602: simplify code                                                   #
#       update 260610: compact to core_260610                                          #
#                                                                                      #
#======================================================================================#

# Core functions load ----

source('./Code/Core.R', encoding = 'UTF8')

# C-R Model setting ----
# this section used to choose C-R Model for Health Impact Calculation.
# Supported C-R functions:
#              'IER', 'NCD+LRI'(Part of GEMM), '5COD'(Part of GEMM), 'MRBRT', 'O3'

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

# ---- Grid-level with 95% CI (MEAN + UP + LOW in one pass) ----
# CI = "RANGE" computes all three RR branches in a single join.
# Output columns: copd_25_MEAN, copd_25_UP, copd_25_LOW, ...

grid_ci <- map(
  scenarios,
  ~ Mortality_at(
    at = .x,
    CI = "RANGE",
    domain = "Country"
  )
) %>%
  set_names(scenarios)

# ---- Grid-level without CI (single branch, fast) ----
# CI = "MEAN" / "UP" / "LOW" computes one branch.
grid_mean <- Mortality_at(at = scenarios[1], CI = "MEAN", domain = "Country")

# ---- Grid Aggregation ----
# Mort_Aggregate accepts either scenario names (auto-computes MEAN internally)
# or a pre-computed list of data frames. It auto-detects CI branch suffixes.

grid_aggr <- Mort_Aggregate(scenarios, domain = 'Grid', write = FALSE)

# grid_edpt <- Mort_Aggregate(scenarios, domain = 'Grid', by = 'endpoint')

# grid_age <- Mort_Aggregate(scenarios, domain = 'Grid', by = 'agegroup')

# ---- Province Aggregation ----

# prov_aggr <- Mort_Aggregate(scenarios, domain = 'Province')

# ---- Nation Aggregation from RANGE (direct summarise, no Mort_Aggregate needed) ----
# grid_ci already has _MEAN / _UP / _LOW columns. A simple group_by + sum suffices.

nation_from_ci <- grid_ci %>%
  map(~ left_join(.x, Grid_info %>% select(x, y, Country))) %>%
  map(~ group_by(.x, Country)) %>%
  map(~ summarise(.x, across(matches('_[0-9]+_(MEAN|UP|LOW)$'), sum), .groups = "drop")) %>%
  map(~ mutate(.x,
    Total_MEAN = rowSums(select(., matches('_MEAN$')), na.rm = TRUE),
    Total_UP   = rowSums(select(., matches('_UP$')),   na.rm = TRUE),
    Total_LOW  = rowSums(select(., matches('_LOW$')),  na.rm = TRUE),
    CI_UP  = Total_UP  - Total_MEAN,
    CI_LOW = Total_MEAN - Total_LOW
  ))

# ---- Nation Aggregation (convenience wrapper with error-propagation CI) ----
# Mort_Aggregate(auto-computes MEAN) is useful when you don't need RANGE and
# want a quick aggregation. Pass includeConc=TRUE for concentration uncertainty.

nation_aggr <- Mort_Aggregate(scenarios, domain = 'Country', write = FALSE)

nation_edpt <- Mort_Aggregate(
  scenarios,
  domain = 'Country',
  by = 'endpoint',
  includeConc = TRUE,
  Conc_ERR = 10,
  write = FALSE
)
