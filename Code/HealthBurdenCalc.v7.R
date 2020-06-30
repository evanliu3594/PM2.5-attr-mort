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
#  Created By Yifan LIU on 20200505                                                    #
#       *Add GEMM model support                                                        #
#       *Add better uncertainty assessments and improved the result deliviery          #
#                                                                                      #
#======================================================================================#

a<-head(unlist(strsplit(rstudioapi::getActiveDocumentContext()$path,split = '/')),-1)
# the above commend only works in Rstudio
setwd(paste(a[1:(length(a)-1)],collapse = '/'))
rm(a)

library(tidyverse)
library(readxl)
library(writexl)
source('./Code/BasicFunctions.R')

# Set Calc Condition ----

 mode='NCD+LRI'  ;RealPM=T
# mode='5COD'     ;RealPM=T
# mode='IER'      ;RealPM=T
# mode='NCD+LRI'  ;RealPM=F
# mode='5COD'     ;RealPM=F
# mode='IER'      ;RealPM=F

## Data Import ----

{
  Pop <- read_csv('./Data/Grid.Pop.csv') %>% arrange(FID)
  
  PM_real <- read_csv('./Data/Gird.PM25.txt') %>% arrange(FID)
  PM_real <- bind_cols(PM_real[, 1], round(PM_real[,-1], 1))
  
  if (!RealPM) {
    PM_cf <- read_csv('./Data/PM_Ctrl.csv') %>% arrange(FID)
    PM_cf <- bind_cols(PM_cf[, 1], round(PM_cf[,-1], 1))
  }

  inci <- read_csv('./Data/GBD_incidence_China_2000-2017.csv')
  
  agegroup <- read_csv('./Data/GBD_agestructure_China_2000-2017.csv')
  
  agegroup <-
    data.frame(prop.table(as.matrix(agegroup[, -1]), 1)) %>% `rownames<-`(c(agegroup$year))
  
  if (mode=='IER') {
    RR_table <- list(
      MEAN = read_excel('./Data/GBD2017_RR_LYF_percentile.xlsx', sheet = 'MEAN'),
      LOW = read_excel('./Data/GBD2017_RR_LYF_percentile.xlsx', sheet = 'LOW'),
      UP = read_excel('./Data/GBD2017_RR_LYF_percentile.xlsx', sheet = 'UP')
    )
  } else if (mode=='5COD' | mode=='NCD+LRI'){
    RR_table <- list(
      MEAN = read_excel('./Data/GEMM_RR_2020-03-01.xlsx', sheet = 'MEAN'),
      LOW = read_excel('./Data/GEMM_RR_2020-03-01.xlsx', sheet = 'LOW'),
      UP = read_excel('./Data/GEMM_RR_2020-03-01.xlsx', sheet = 'UP')
    )
  }
  
  RR_table[['MEAN']]$concentration %>% round(1) -> RR_table[['MEAN']]$concentration
  RR_table[['LOW']]$concentration %>% round(1) -> RR_table[['LOW']]$concentration
  RR_table[['UP']]$concentration %>% round(1) -> RR_table[['UP']]$concentration
  
  AbsRisk <- list(
    MEAN = bind_cols(RR_table[['MEAN']][, 1] %>% round(1), RR_table[['MEAN']][,-1] - 1),
    LOW = bind_cols(RR_table[['LOW']][, 1] %>% round(1), RR_table[['LOW']][,-1] - 1),
    UP = bind_cols(RR_table[['UP']][, 1] %>% round(1), RR_table[['UP']][,-1] - 1)
  )
}

# Grid Full Result ----

grid_full <- Grid_Mort(mode = mode, RealPM = RealPM)

# Grid Aggregation ----

grid_edpt <- Grid_Aggr(grid_full, mode = mode)

write_xlsx(
  grid_edpt,
  paste0(
    './Result/',
    if_else(mode=='NCD+LRI'|mode=='5COD',paste0('GEMM-',mode),'IER'),
    '.Grid_by.edpt_',
    Sys.Date(),
    '.xlsx'
  )
)
# Region Aggregation ----

region_edpt <- Region_Aggr(grid_full, mode = mode)

write_xlsx(
  region_edpt,
  paste0(
    './Result/',
    if_else(mode=='NCD+LRI'|mode=='5COD',paste0('GEMM-',mode),'IER'),
    '.Region_by.edpt_',
    Sys.Date(),
    '.xlsx'
  )
)


# Nation Aggregation ----

nation_edpt <- Nation_by.edpt(grid_full, mode = mode)

write_csv(
  nation_edpt,
  paste0(
    './Result/',
    if_else(mode=='NCD+LRI'|mode=='5COD',paste0('GEMM-',mode),'IER'),
    '.Nation_by.edpt_',
    Sys.Date(),
    '.csv'
  )
)

# Nation By Ages ----

nation_by.age <- Nation_by.age(full_result = grid_full,mode = mode)
write_csv(
  nation_by.age,
  paste0(
    './Result/',
    if_else(mode == 'NCD+LRI' | mode == '5COD', paste0('GEMM-', mode), 'IER'),
    '.Nation_by.age_',
    Sys.Date(),
    '.csv'
  )
)

