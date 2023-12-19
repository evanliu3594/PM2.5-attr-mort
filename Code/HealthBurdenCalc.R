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

source('./Code/Core-dev.R', encoding = 'UTF8')

# C-R Model setting ----
# this section used to choose C-R Model for Health Impact Calculation.
# Supported C-R functions:  
#              'IER', 'NCD+LRI'(Part of GEMM), '5COD'(Part of GEMM), 'MRBRT', 'O3'

set_Model('O3')

# Data load ----

read_files(
  Grids = './Data/check231219/Gridinfo.csv',
  Pop = './Data/check231219/Gridpop.csv',
  Conc_real = './Data/check231219/GridO3.csv',
  Conc_cf = './Data/PM_Ctrl.csv',  # PM_cf works only in counter-fact scenario
  MortRate = "./Data/check231219/ISO_GBD2019_basemortal_country_2019.csv",
  AgeGroup = './Data/check231219/ISO_GBD2019_agestructure_country_2015.csv'
)

# Grid Full Result ----

grid_full <- names(Conc_real)[c(-1:-2)] %>% set_names %>% 
  map(~ Mortality(
    Grids = Grid_info,
    RR = RR_table$MEAN,
    Conc_r = Conc_real %>% select(x:y, concentration = !!.x),
    Conc_c = NULL,
    pop = Pop %>% select(x:y, Pop = !!.x),
    ag = AgeGroup %>% select(domain, agegroup, AgeStruc = !!.x),
    mRate = mortrate_std(MortRate, .x),
    domain = "Country"
))

# Grid Aggregation ----
 
grid_aggr <- Mort_Aggregate(grid_full, domain = 'Grid', write = F)


# the below 2 aggregation at grid level is time-hungry, not recommended.

# grid_edpt <- Mort_Aggregate(grid_full, domain = 'Grid', by = 'endpoint')

# grid_age <- Mort_Aggregate(grid_full, domain = 'Grid', by = 'agegroup')

# Province Aggregation ----


# prov_aggr <- Mort_Aggregate(grid_full, domain = 'Province')

# prov_edpt <- Mort_Aggregate(grid_full, domain = 'Province', by = 'endpoint')

# prov_age <- Mort_Aggregate(grid_full, domain = 'Province', by = 'agegroup')

# Nation Aggregation ----

nation_aggr <- Mort_Aggregate(grid_full, domain = 'Country', includeConc = F)

nation_edpt <- Mort_Aggregate(grid_full, domain = 'Country', by = 'endpoint', includeConc = T, Conc_RMSE = 2)

nation_age <- Mort_Aggregate(grid_full, domain = 'Country', by = 'agegroup', Conc_sigma = .08)
