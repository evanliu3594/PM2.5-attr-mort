#======================================================================================#
#      oooooooooo oooo     oooo                                                        #
#      888    888 8888o   888   ooooo    8888888                                       #
#     888oooo88  88 888o8 88  o   o88   8oooooo                                        #
#    888        88  888  88     o8     o    o88                                        #
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
#                                                                                      #
#======================================================================================#

# Core functions load ----

source('./Code/Core.R', encoding = 'UTF8')

# C-R function setting ----
# this section used to choose C-R function for Health Impact Calculation.
# Supported C-R functions:  
#              'IER', 'NCD+LRI'(Part of GEMM), '5COD'(Part of GEMM), 'MRBRT'

use_CR('NCD+LRI')

# Data load ----

read_files(
  GRID = './Data/GRID_information_sample.xlsx',
  Pop = './Data/GridPop_sample.xlsx',
  PM_real = './Data/GridPM25_sample.xlsx',
  PM_cf = './Data/PM_Ctrl.csv', # PM_cf works only in counter-fact scenario
  MortRate = './Data/GBD_incidence_China_2000-2019.csv',
  AgeGroup = './Data/GBD_agestructure_China_2000-2017.csv'
)

# Grid Full Result ----

grid_full <- names(PM_real) %>% str_subset('\\d{4}') %>% set_names %>% 
  map(function(year) Mortality(
    Grids = Grid_info %>% select(x:y),
    RR = RR_table$MEAN,
    PM_r = PM_real %>% select(x:y, concentration = !!year),
    pop = Pop %>% select(x:y, Pop = !!year),
    ag = AgeGroup %>% select(agegroup, AgeStruc = !!year),
    mRate = mortrate_std(MortRate, year)
))

# Grid Aggregation ----

grid_aggr <- Mort_Aggregate(grid_full, domain = 'Grid')

# the below 2 aggregation at grid level is time-hungry, not recommended.

grid_edpt <- Mort_Aggregate(grid_full, domain = 'Grid', by = 'endpoint')

grid_age <- Mort_Aggregate(grid_full, domain = 'Grid', by = 'agegroup')

# Province Aggregation ----

prov_aggr <- Mort_Aggregate(grid_full, domain = 'Province')

prov_edpt <- Mort_Aggregate(grid_full, domain = 'Province', by = 'endpoint')

prov_age <- Mort_Aggregate(grid_full, domain = 'Province', by = 'agegroup')

# Nation Aggregation ----

nation_aggr <- Mort_Aggregate(grid_full, domain = 'Country')

nation_edpt <- Mort_Aggregate(grid_full, domain = 'Country', by = 'endpoint')

nation_age <- Mort_Aggregate(grid_full, domain = 'Country', by = 'agegroup') 
