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
#          -- An Intergrated Calculate Program for Both IER and GEMM Model --          #
#                                                                                      #
#  Created By Yifan LIU on 20210505                                                    #
#       *Add GEMM model support                                                        #
#       *Add better uncertainty assessments and improved the result deliviery          #
#       update 210525: replace matrix calculation by dplyr&tidyr syntax                #
#                                                                                      #
#======================================================================================#

require(tidyverse)
require(readxl)
require(writexl)

rstudioapi::getActiveDocumentContext()$path %>% strsplit('/') %>%
  unlist %>% head(-2) %>% paste(collapse = '/') %>% setwd

# Calc Settings ----

 mode='NCD+LRI'  ;RealPM=T
# mode='5COD'     ;RealPM=T
# mode='IER'      ;RealPM=T
# mode='NCD+LRI'  ;RealPM=F
# mode='5COD'     ;RealPM=F
# mode='IER'      ;RealPM=F

## Data Import ----

{
  FID_info <- read_csv('./Data/FID_Prov.info_20201229.csv') %>%
    mutate(FID = FID %>% as.integer %>% as.character)
  
  Pop <-
    read_csv('./Data/Grid.Pop.csv') %>%
    mutate(FID = FID %>% as.integer %>% as.character)
  
  PM_real <- read_csv('./Data/Gird.PM25.txt') %>%
    mutate(FID = FID %>% as.integer %>% as.character) %>%
    mutate_at(vars(-FID), ~ round(., 1))
  
  if (!RealPM) {
    PM_cf <- read_csv('./Data/PM_Ctrl.csv') %>%
    mutate(FID = FID %>% as.integer %>% as.character) %>%
    mutate_at(vars(-FID), ~ round(., 1)) # used for only counter-fact scenario.
  } else {
    PM_cf <- PM_real
  }
  
  incidence <-
    read_csv('./Data/GBD_incidence_China_2000-2017.csv') %>%
    mutate(year = year %>% as.integer %>% as.character)  %>% pivot_longer(
      cols = c(-year,-endpoint),
      names_to = 'agegroup',
      values_to = 'mort.rate'
    )
  
  agegroup <-
    read_csv('./Data/GBD_agestructure_China_2000-2017.csv') %>%
    mutate_at(vars(year),~.x %>% as.integer %>% as.character) %>%
    pivot_longer(cols = c(-year),
                 names_to = 'agegroup',
                 values_to = 'age.structure') %>%
    pivot_wider(names_from = year,
                values_from = 'age.structure') %>%
    mutate_at(vars(-agegroup), ~prop.table(.x))
  
  if (mode == 'IER') {
    RR_table <- list(
      MEAN = read_excel('./Data/GBD2017_RR_LYF_2020-05-06.xlsx', sheet = 'MEAN') %>%
        mutate(concentration = concentration %>% round(1)),
      LOW = read_excel('./Data/GBD2017_RR_2020-05-06.xlsx', sheet = 'LOW') %>%
        mutate(concentration = concentration %>% round(1)),
      UP = read_excel('./Data/GBD2017_RR_2020-05-06.xlsx', sheet = 'UP') %>%
        mutate(concentration = concentration %>% round(1))
    )
  } else if (mode %in% c('5COD', 'NCD+LRI')) {
    RR_table <- list(
      MEAN = read_excel('./Data/GEMM_RR_2020-03-01.xlsx', sheet = 'MEAN') %>%
        mutate(concentration = concentration %>% round(1)),
      LOW = read_excel('./Data/GEMM_RR_2020-03-01.xlsx', sheet = 'LOW') %>%
        mutate(concentration = concentration %>% round(1)),
      UP = read_excel('./Data/GEMM_RR_2020-03-01.xlsx', sheet = 'UP') %>%
        mutate(concentration = concentration %>% round(1))
    )
  }
}

## Core Module Load ----

source('./Code/Core.R')

# Grid Full Result ----

grid_full <- names(PM_real)[-1] %>% map(function(y) {
  Mortality(
    inci = filter(incidence, year == y),
    pop = select(Pop, FID, pop = all_of(y)),
    PM_r = select(PM_real, FID, concentration = all_of(y)),
    PM_c = select(PM_cf, FID, concentration = all_of(y)),
    ag = select(agegroup, agegroup, agstruc = all_of(y)),
    mode = mode
  )
}) %>% `names<-`(names(PM_real)[-1])

# Grid Aggregation ----

grid_edpt <- Grid_Aggr(grid_full, by = 'endpoint',mode = mode)

grid_edpt <- Grid_Aggr(grid_full, by = 'agegroup',mode = mode)

# Region Aggregation ----

region_edpt <- Region_Aggr(grid_full, by='endpoint',mode = mode)

region_agegroup <- Region_Aggr(grid_full, by='agegroup',mode = mode)


# Nation Aggregation By endpoint ----

nation_edpt <- Nation_by.edpt(grid_full, mode = mode)

# Nation By Ages ----

nation_by.age <- Nation_by.age(full_result = grid_full,mode = mode)