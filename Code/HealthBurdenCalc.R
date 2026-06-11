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

# Grid Full Result & Aggregation ----
# Mort_Aggregate accepts scenario names directly and computes MEAN/UP/LOW internally.

scenarios <- names(Conc_real)[c(-1:-2)]

grid_aggr <- Mort_Aggregate(scenarios, domain = 'Grid', write = FALSE)

# grid_edpt <- Mort_Aggregate(scenarios, domain = 'Grid', by = 'endpoint')

# grid_age <- Mort_Aggregate(scenarios, domain = 'Grid', by = 'agegroup')

# Province Aggregation ----

# prov_aggr <- Mort_Aggregate(scenarios, domain = 'Province')

# prov_edpt <- Mort_Aggregate(scenarios, domain = 'Province', by = 'endpoint')

# prov_age <- Mort_Aggregate(scenarios, domain = 'Province', by = 'agegroup')

# Nation Aggregation ----

nation_aggr <- Mort_Aggregate(scenarios, domain = 'Country', write = FALSE)

nation_edpt <- Mort_Aggregate(
  scenarios,
  domain = 'Country',
  by = 'endpoint',
  includeConc = TRUE,
  Conc_ERR = 10,
  write = FALSE
)

nation_age_CR <- Mort_Aggregate(
  scenarios,
  domain = 'Country',
  by = 'agegroup',
  verbose = TRUE,
  includeConc = T,
  Conc_ERR = 18,
  write = F
)

nation_age_CR_noConc <- Mort_Aggregate(
  scenarios,
  domain = 'Country',
  by = 'agegroup',
  write = FALSE
)
