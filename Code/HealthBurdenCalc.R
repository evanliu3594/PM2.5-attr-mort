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
grid_ci <- scenarios %>%
  set_names() %>%
  map(~ Mortality_at(at = .x, CI = "RANGE", domain = "Country"))

# ---- Aggregation ----
# Uncomment the level(s) and breakdown(s) you need.
# at  = "grid" | "geo" | "Country" | "Province" | "Region"
# by  = NULL (Total) | "all" | "endpoint" | "agegroup"
# write = FALSE | TRUE (-> ./Result/) | "./my/path"

# ---- Grid-level ----
# aggregate_mort(grid_ci, at = "grid", write = FALSE)

# ---- National ----
# aggregate_mort(grid_ci, at = "Country", by = NULL,      write = FALSE)   # Total
# aggregate_mort(grid_ci, at = "Country", by = "endpoint", write = FALSE)   # x endpoint
# aggregate_mort(grid_ci, at = "Country", by = "agegroup", write = FALSE)   # x agegroup
# aggregate_mort(grid_ci, at = "Country", by = "all",      write = FALSE)   # all above

# ---- Provincial ----
# aggregate_mort(grid_ci, at = "Province", by = "all", write = FALSE)

# ---- Regional ----
# aggregate_mort(grid_ci, at = "Region", by = "all", write = FALSE)

# ---- All geo levels at once ----
# aggregate_mort(grid_ci, at = "geo", by = "all", write = FALSE)

# ---- Convenience: Mort_Aggregate with error-propagation CI ----
# nation_aggr <- Mort_Aggregate(scenarios, domain = 'Country', write = FALSE)
# nation_edpt <- Mort_Aggregate(scenarios, domain = 'Country', by = 'endpoint',
#                               includeConc = TRUE, Conc_ERR = 10, write = FALSE)
