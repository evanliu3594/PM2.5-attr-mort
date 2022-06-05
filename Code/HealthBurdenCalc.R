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
#       -- An Integrated Calculate Program for PM2.5 health burden assessment --       #
#                                                                                      #
#  Created By Yifan LIU on 20210505                                                    #
#       *Add GEMM model support                                                        #
#       *Add better uncertainty assessments and improved the result return             #
#       update 210525: replace matrix calculation by tidyr Grammar                     #
#       update 220602: simplify code                                                   #
#                                                                                      #
#======================================================================================#

# C-R function Settings ----
# this section used to choose C-R function for Health Impact Calculation.
# Supported C-R functions:  
#              'IER', 'NCD+LRI'(Part of GEMM), '5COD'(Part of GEMM), 'MRBRT'

CR_fun = 'MRBRT'

## Data Load ----

readfile <- list(
  FID = './Data/FID_information.xlsx',
  Pop = './Data/GridPop.csv',
  PM_real = './Data/GridPM25.csv',
  PM_cf = './Data/PM_Ctrl.csv', # PM_cf works only in counter-fact scenario
  MortRate = './Data/GBD_incidence_China_2000-2019.csv',
  AgeGroup = './Data/GBD_agestructure_China_2000-2017.csv',
  CR = if (CR_fun == 'MRBRT') './Data/RR_index/MRBRT2019_Lookup_Table_LYF220601.xlsx'
  else if (CR_fun %in% c('NCD+LRI', '5COD')) './Data/RR_index/GEMM_Lookup_Table_Build_220601.xlsx'
  else if (CR_fun %in% c('IER', 'IER2017')) './Data/RR_index/IER2017_Lookup_Table_Build_220601.xlsx'
  else if (CR_fun == 'IER2015') './Data/RR_index/IER2015_Lookup_Table_Build_220601.xlsx'
  else if (CR_fun == 'IER2013') './Data/RR_index/IER2013_Lookup_Table_Build_220601.xlsx'
  else if (CR_fun == 'IER2010') './Data/RR_index/IER2010_Lookup_Table_Build_220601.xlsx'
)

source('./Code/DataLoad.R', encoding = 'UTF8')

## Core Module Load ----

source('./Code/Core.R', encoding = 'UTF8')

# Grid Full Result ----

grid_full <- names(PM_real) %>% str_subset('\\d{4}') %>% set_names %>% map(
  ~ Mortality(
    FID = FID_info %>% pull(FID),
    RR = RR_table$MEAN,  
    PM_r = PM_real %>% select(FID, concentration = !!as.name(.x)),
    pop = Pop %>% select(FID, Pop = !!as.name(.x)),
    ag = AgeGroup %>% select(agegroup, AgeStruc = !!as.name(.x)),
    mRate = MortRate %>% select(endpoint, agegroup, MortRate = !!as.name(.x)),
    CR = CR_fun
))

# Grid Aggregation ----

grid_aggr <- Mort_Aggregate(grid_full, domain = 'FID')

grid_edpt <- Mort_Aggregate(grid_full, domain = 'FID', by = 'endpoint')

grid_age <- Mort_Aggregate(grid_full, domain = 'FID', by = 'agegroup')

# Province Aggregation ----

prov_aggr <- Mort_Aggregate(grid_full, domain = 'Province')

prov_edpt <- Mort_Aggregate(grid_full, domain = 'Province', by = 'endpoint')

prov_age <- Mort_Aggregate(grid_full, domain = 'Province', by = 'agegroup')

# Nation Aggregation ----

nation_aggr <- Mort_Aggregate(grid_full, domain = 'Country') %>% 
  imap_dfr(~.x %>% add_column(year = .y, .before  = TRUE))

nation_edpt <- Mort_Aggregate(grid_full, domain = 'Country', by = 'endpoint') %>% 
  imap_dfr(~.x %>% add_column(year = .y, .before  = TRUE))

nation_age <- Mort_Aggregate(grid_full, domain = 'Country', by = 'agegroup') %>% 
  imap_dfr(~.x %>% add_column(year = .y, .before  = TRUE))