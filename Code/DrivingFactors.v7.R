#====================================================#
# retrieve driving forces to the PM2.5 health burden #
#                              Depending: Core-v1    #
#====================================================#

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
  FID_info <- read_csv('./Data/FID_info.csv') %>%
    mutate(FID = FID %>% as.integer %>% as.character)
  
  Pop <-
    read_csv('./Data/GridPop.csv') %>%
    mutate(FID = FID %>% as.integer %>% as.character)
  
  PM_real <- read_csv('./Data/GridPM25.csv') %>%
    mutate(FID = FID %>% as.integer %>% as.character) %>%
    mutate_at(vars(-FID), ~ round(., 1))
  
  if (!RealPM) {
    PM_cf <- read_csv('./Data/PM_Ctrl.csv') %>% # used for only counter-fact scenario.
    mutate(FID = FID %>% as.integer %>% as.character) %>%
    mutate_at(vars(-FID), ~ round(., 1)) 
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


Decomposition<-function(serie,y.a,y.b,mode){
  
  # Calc Steps for Each Serie
  # 1    PG    PA    EXP   ORF
  # 2    PG    PA    ORF   EXP
  # 3    PG    EXP   PA    ORF
  # 4    PG    EXP   ORF   PA
  # 5    PG    ORF   PA    EXP
  # 6    PG    ORF   EXP   PA
  # 7    PA    PG    EXP   ORF
  # 8    PA    PG    ORF   EXP
  # 9    PA    EXP   PG    ORF
  # 10   PA    EXP   ORF   PG
  # 11   PA    ORF   PG    EXP
  # 12   PA    ORF   EXP   PG
  # 13   EXP   PG    PA    ORF
  # 14   EXP   PG    ORF   PA
  # 15   EXP   PA    PG    ORF
  # 16   EXP   PA    ORF   PG
  # 17   EXP   ORF   PA    PG
  # 18   EXP   ORF   PG    PA
  # 19   ORF   PG    PA    EXP
  # 20   ORF   PG    EXP   PA
  # 21   ORF   PA    PG    EXP
  # 22   ORF   PA    EXP   PG
  # 23   ORF   EXP   PG    PA
  # 24   ORF   EXP   PA    PG
  
  serie.step <- matrix(c(
    'PG','PA','EXP','ORF','PG','PA','ORF','EXP','PG','EXP','PA','ORF',
    'PG','EXP','ORF','PA','PG','ORF','PA','EXP','PG','ORF','EXP','PA',
    'PA','PG','EXP','ORF','PA','PG','ORF','EXP','PA','EXP','PG','ORF',
    'PA','EXP','ORF','PG','PA','ORF','PG','EXP','PA','ORF','EXP','PG',
    'EXP','PG','PA','ORF','EXP','PG','ORF','PA','EXP','PA','PG','ORF',
    'EXP','PA','ORF','PG','EXP','ORF','PA','PG','EXP','ORF','PG','PA',
    'ORF','PG','PA','EXP','ORF','PG','EXP','PA','ORF','PA','PG','EXP',
    'ORF','PA','EXP','PG','ORF','EXP','PG','PA','ORF','EXP','PA','PG'
    ), ncol = 4,byrow = T
  )[serie, ]

  # Mort.Start ----
  Mort_0 <- Mortality(
    inci = filter(incidence, year == y.a),
    pop = select(Pop, FID, pop = all_of(y.a)),
    PM_r = select(PM_real, FID, concentration = all_of(y.a)),
    PM_c = select(PM_cf, FID, concentration = all_of(y.a)),
    ag = select(agegroup, agegroup, agstruc = all_of(y.a)),
    mode = mode
  )

  # Mort.1----
  
  if (serie.step[1]=='PG') {
    Mort_1 <- Mortality(
      inci = filter(incidence, year == y.a),
      pop = select(Pop, FID, pop = all_of(y.b)),
      PM_r = select(PM_real, FID, concentration = all_of(y.a)),
      PM_c = select(PM_cf, FID, concentration = all_of(y.a)),
      ag = select(agegroup, agegroup, agstruc = all_of(y.a)),
      mode = mode
    )
  } else if (serie.step[1]=='PA') {
    Mort_1 <-  Mortality(
      inci = filter(incidence, year == y.a),
      pop = select(Pop, FID, pop = all_of(y.a)),
      PM_r = select(PM_real, FID, concentration = all_of(y.a)),
      PM_c = select(PM_cf, FID, concentration = all_of(y.a)),
      ag = select(agegroup, agegroup, agstruc = all_of(y.b)),
      mode = mode
    )
  } else if (serie.step[1]=='EXP') {
    Mort_1 <-  Mortality(
      inci = filter(incidence, year == y.a),
      pop = select(Pop, FID, pop = all_of(y.a)),
      PM_r = select(PM_real, FID, concentration = all_of(y.a)),
      PM_c = select(PM_cf, FID, concentration = all_of(y.b)),
      ag = select(agegroup, agegroup, agstruc = all_of(y.a)),
      mode = mode
    )
  } else if (serie.step[1]=='ORF') {
    Mort_1 <-  Mortality(
      inci = filter(incidence, year == y.b),
      pop = select(Pop, FID, pop = all_of(y.a)),
      PM_r = select(PM_real, FID, concentration = all_of(y.a)),
      PM_c = select(PM_cf, FID, concentration = all_of(y.a)),
      ag = select(agegroup, agegroup, agstruc = all_of(y.a)),
      mode = mode
    )
  }

  # Mort.2----
  
  if (all(serie.step[1:2] %in% c('PG', 'PA'))) {
    # PG  PA
    Mort_2 <- Mortality(
      inci = filter(incidence, year == y.a),
      pop = select(Pop, FID, pop = all_of(y.b)),
      PM_r = select(PM_real, FID, concentration = all_of(y.a)),
      PM_c = select(PM_cf, FID, concentration = all_of(y.a)),
      ag = select(agegroup, agegroup, agstruc = all_of(y.b)),
      mode = mode
    )
  } else if (all(serie.step[1:2] %in% c('PG', 'EXP'))) {
    #  PG  EXP
    Mort_2 <-  Mortality(
      inci = filter(incidence, year == y.a),
      pop = select(Pop, FID, pop = all_of(y.b)),
      PM_r = select(PM_real, FID, concentration = all_of(y.a)),
      PM_c = select(PM_cf, FID, concentration = all_of(y.b)),
      ag = select(agegroup, agegroup, agstruc = all_of(y.a)),
      mode = mode
    )
  } else if (all(serie.step[1:2] %in% c('PG', 'ORF')) ) {
    #  PG  ORF
    Mort_2 <- Mortality(
      inci = filter(incidence, year == y.b),
      pop = select(Pop, FID, pop = all_of(y.b)),
      PM_r = select(PM_real, FID, concentration = all_of(y.b)),
      PM_c = select(PM_cf, FID, concentration = all_of(y.a)),
      ag = select(agegroup, agegroup, agstruc = all_of(y.a)),
      mode = mode
    )
  } else if (all(serie.step[1:2] %in% c('PA', 'EXP'))) {
    #  PA  EXP
    Mort_2 <-  Mortality(
      inci = filter(incidence, year == y.a),
      pop = select(Pop, FID, pop = all_of(y.a)),
      PM_r = select(PM_real, FID, concentration = all_of(y.a)),
      PM_c = select(PM_cf, FID, concentration = all_of(y.b)),
      ag = select(agegroup, agegroup, agstruc = all_of(y.b)),
      mode = mode
    )
  } else if (all(serie.step[1:2] %in% c('PA', 'ORF'))) {
    #  PA  ORF
    Mort_2 <-  Mortality(
      inci = filter(incidence, year == y.b),
      pop = select(Pop, FID, pop = all_of(y.a)),
      PM_r = select(PM_real, FID, concentration = all_of(y.b)),
      PM_c = select(PM_cf, FID, concentration = all_of(y.a)),
      ag = select(agegroup, agegroup, agstruc = all_of(y.b)),
      mode = mode
    )
  } else if (all(serie.step[1:2] %in% c('EXP', 'ORF'))) {
    #  EXP ORF
    Mort_2 <-  Mortality(
      inci = filter(incidence, year == y.b),
      pop = select(Pop, FID, pop = all_of(y.a)),
      PM_r = select(PM_real, FID, concentration = all_of(y.b)),
      PM_c = select(PM_cf, FID, concentration = all_of(y.b)),
      ag = select(agegroup, agegroup, agstruc = all_of(y.a)),
      mode = mode
    )
  }

  # Mort.3----

  if (all(serie.step[1:3] %in% c('PG', 'PA','EXP'))) {
    # PG  PA EXP
    Mort_3 <-  Mortality(
      inci = filter(incidence, year == y.a),
      pop = select(Pop, FID, pop = all_of(y.b)),
      PM_r = select(PM_real, FID, concentration = all_of(y.a)),
      PM_c = select(PM_cf, FID, concentration = all_of(y.b)),
      ag = select(agegroup, agegroup, agstruc = all_of(y.b)),
      mode = mode
    )
    
  } else if (all(serie.step[1:3] %in% c('PG', 'PA','ORF')) ) {
    # PG  PA ORF
    Mort_3 <-  Mortality(
      inci = filter(incidence, year == y.b),
      pop = select(Pop, FID, pop = all_of(y.b)),
      PM_r = select(PM_real, FID, concentration = all_of(y.b)),
      PM_c = select(PM_cf, FID, concentration = all_of(y.a)),
      ag = select(agegroup, agegroup, agstruc = all_of(y.b)),
      mode = mode
    )
    
  } else if (all(serie.step[1:3] %in% c('PG', 'EXP','ORF')) ) {
    #  PG	EXP	ORF
    Mort_3 <-  Mortality(
      inci = filter(incidence, year == y.b),
      pop = select(Pop, FID, pop = all_of(y.b)),
      PM_r = select(PM_real, FID, concentration = all_of(y.b)),
      PM_c = select(PM_cf, FID, concentration = all_of(y.b)),
      ag = select(agegroup, agegroup, agstruc = all_of(y.a)),
      mode = mode
    )
    
  } else if (all(serie.step[1:3] %in% c('PA', 'EXP','ORF')) ) {
    #  PA	EXP	ORF
    Mort_3 <-  Mortality(
      inci = filter(incidence, year == y.b),
      pop = select(Pop, FID, pop = all_of(y.a)),
      PM_r = select(PM_real, FID, concentration = all_of(y.b)),
      PM_c = select(PM_cf, FID, concentration = all_of(y.b)),
      ag = select(agegroup, agegroup, agstruc = all_of(y.b)),
      mode = mode
    )
  }
  
  # Mort.End ----
  
  Mort_4 <- Mortality(
    inci = filter(incidence, year == y.b),
    pop = select(Pop, FID, pop = all_of(y.b)),
    PM_r = select(PM_real, FID, concentration = all_of(y.b)),
    PM_c = select(PM_cf, FID, concentration = all_of(y.b)),
    ag = select(agegroup, agegroup, agstruc = all_of(y.b)),
    mode = mode
  )
  
  # Print Result ----
  
  cat(paste0('Drivers Between ',y.a,' and ',y.b,':\n'))
  cat(paste0(serie.step[1],':\t'))
  cat(sum(select(Mort_1, -FID)) - sum(select(Mort_0, -FID)))
  cat('\n')
  cat(paste0(serie.step[2],':\t'))
  cat(sum(select(Mort_2, -FID)) - sum(select(Mort_1, -FID)))
  cat('\n')
  cat(paste0(serie.step[3],':\t'))
  cat(sum(select(Mort_3, -FID)) - sum(select(Mort_2, -FID)))
  cat('\n')
  cat(paste0(serie.step[4],':\t'))
  cat(sum(select(Mort_4, -FID)) - sum(select(Mort_3, -FID)))
  cat('\n')

  return(
    data.frame(
      Mort_0['FID'],
      rowSums(select(Mort_0, -FID)),
      rowSums(select(Mort_1, -FID) - select(Mort_0, -FID)),
      rowSums(select(Mort_2, -FID) - select(Mort_1, -FID)),
      rowSums(select(Mort_3, -FID) - select(Mort_2, -FID)),
      rowSums(select(Mort_4, -FID) - select(Mort_3, -FID)),
      rowSums(select(Mort_4, -FID))
    ) %>% `names<-`(c('FID', 'Start', serie.step, 'End'))
  )
  
  rm(Mort_0,Mort_1,Mort_2,Mort_3,Mort_4,serie.step)

}



# USAGE: 

1:24 %>% map(~Decomposition(serie = .x,y.a = '2005',y.b = '2015',mode=mode))
