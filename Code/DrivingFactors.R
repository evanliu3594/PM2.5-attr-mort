#====================================================#
# retrieve driving forces to the PM2.5 health burden #
#                         Depending: Core-V220603    #
#====================================================#

require(tidyverse)
require(readxl)
require(writexl)

# C-R function Settings ----
# this section used to choose C-R function for Health Impact Calculation. 
# All core functions would set method according to this global-environment variable.
# Supported C-R functions:  
#              'IER', 'NCD+LRI'(Part of GEMM), '5COD' (Part of GEMM), 'MRBRT'

CR_fun = 'NCD+LRI'

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

# Core Module Load ----

source('./Code/Core.R',encoding = 'UTF8')

Decomposition <- function(serie, y.a, y.b) {
  
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
  
  serie.step <- expand_grid(
    step1 = c('PA','PG','EXP','ORF'), step2 = c('PA','PG','EXP','ORF'),
    step3 = c('PA','PG','EXP','ORF'), step4 = c('PA','PG','EXP','ORF'),
  ) %>% filter(
    step1 != step2 & step2 != step3 & step3 != step4 & 
    step4 != step1 & step1 != step3 & step2 != step4
  ) %>% slice(serie) %>% unlist
  
  Decomp <- list(
  # Mort.Start ----
    Mort_0 = Mortality(
      FID = FID_info %>% pull(FID),
      RR = RR_table$MEAN,  
      PM_r = PM_real %>% select(FID, concentration = !!as.name(y.a)),
      PM_c = PM_real %>% select(FID, concentration = !!as.name(y.a)),
      pop = Pop %>% select(FID, Pop = !!as.name(y.a)),
      ag = AgeGroup %>% select(agegroup, AgeStruc = !!as.name(y.a)),
      mRate = MortRate %>% select(endpoint, agegroup, MortRate = !!as.name(y.a))
    ),
    # Mort.1----
    Mort_1 = if (serie.step[1] == 'PG') {
      Mortality(
        FID = FID_info %>% pull(FID),
        RR = RR_table$MEAN,  
        PM_r = PM_real %>% select(FID, concentration = !!as.name(y.a)),
        PM_c = PM_real %>% select(FID, concentration = !!as.name(y.a)),
        pop = Pop %>% select(FID, Pop = !!as.name(y.b)),
        ag = AgeGroup %>% select(agegroup, AgeStruc = !!as.name(y.a)),
        mRate = MortRate %>% select(endpoint, agegroup, MortRate = !!as.name(y.a))
      )
    } else if (serie.step[1] == 'PA') {
      Mortality(
        FID = FID_info %>% pull(FID),
        RR = RR_table$MEAN,  
        PM_r = PM_real %>% select(FID, concentration = !!as.name(y.a)),
        PM_c = PM_real %>% select(FID, concentration = !!as.name(y.a)),
        pop = Pop %>% select(FID, Pop = !!as.name(y.a)),
        ag = AgeGroup %>% select(agegroup, AgeStruc = !!as.name(y.b)),
        mRate = MortRate %>% select(endpoint, agegroup, MortRate = !!as.name(y.a))
      )
    } else if (serie.step[1] == 'EXP') {
      Mortality(
        FID = FID_info %>% pull(FID),
        RR = RR_table$MEAN,  
        PM_r = PM_real %>% select(FID, concentration = !!as.name(y.a)),
        PM_c = PM_real %>% select(FID, concentration = !!as.name(y.b)),
        pop = Pop %>% select(FID, Pop = !!as.name(y.a)),
        ag = AgeGroup %>% select(agegroup, AgeStruc = !!as.name(y.a)),
        mRate = MortRate %>% select(endpoint, agegroup, MortRate = !!as.name(y.a))
      )
    } else if (serie.step[1] == 'ORF') {
      Mortality(
        FID = FID_info %>% pull(FID),
        RR = RR_table$MEAN,  
        PM_r = PM_real %>% select(FID, concentration = !!as.name(y.b)),
        PM_c = PM_real %>% select(FID, concentration = !!as.name(y.a)),
        pop = Pop %>% select(FID, Pop = !!as.name(y.a)),
        ag = AgeGroup %>% select(agegroup, AgeStruc = !!as.name(y.a)),
        mRate = MortRate %>% select(endpoint, agegroup, MortRate = !!as.name(y.b))
      )
    },
    # Mort.2----
    Mort_2 = if (all(serie.step[1:2] %in% c('PG','PA'))) {
      # PG  PA
      Mortality(
        FID = FID_info %>% pull(FID),
        RR = RR_table$MEAN,  
        PM_r = PM_real %>% select(FID, concentration = !!as.name(y.a)),
        PM_c = PM_real %>% select(FID, concentration = !!as.name(y.a)),
        pop = Pop %>% select(FID, Pop = !!as.name(y.b)),
        ag = AgeGroup %>% select(agegroup, AgeStruc = !!as.name(y.b)),
        mRate = MortRate %>% select(endpoint, agegroup, MortRate = !!as.name(y.a))
      )
    } else if (all(serie.step[1:2] %in% c('PG','EXP'))) {
      #  PG  EXP
      Mortality(
        FID = FID_info %>% pull(FID),
        RR = RR_table$MEAN,  
        PM_r = PM_real %>% select(FID, concentration = !!as.name(y.a)),
        PM_c = PM_real %>% select(FID, concentration = !!as.name(y.b)),
        pop = Pop %>% select(FID, Pop = !!as.name(y.b)),
        ag = AgeGroup %>% select(agegroup, AgeStruc = !!as.name(y.a)),
        mRate = MortRate %>% select(endpoint, agegroup, MortRate = !!as.name(y.a))
      )
    } else if (all(serie.step[1:2] %in% c('PG', 'ORF'))) {
      #  PG  ORF
      Mortality(
        FID = FID_info %>% pull(FID),
        RR = RR_table$MEAN,  
        PM_r = PM_real %>% select(FID, concentration = !!as.name(y.b)),
        PM_c = PM_real %>% select(FID, concentration = !!as.name(y.a)),
        pop = Pop %>% select(FID, Pop = !!as.name(y.b)),
        ag = AgeGroup %>% select(agegroup, AgeStruc = !!as.name(y.a)),
        mRate = MortRate %>% select(endpoint, agegroup, MortRate = !!as.name(y.b))
      )
    } else if (all(serie.step[1:2] %in% c('PA', 'EXP'))) {
      #  PA  EXP
      Mortality(
        FID = FID_info %>% pull(FID),
        RR = RR_table$MEAN,  
        PM_r = PM_real %>% select(FID, concentration = !!as.name(y.a)),
        PM_c = PM_real %>% select(FID, concentration = !!as.name(y.b)),
        pop = Pop %>% select(FID, Pop = !!as.name(y.a)),
        ag = AgeGroup %>% select(agegroup, AgeStruc = !!as.name(y.b)),
        mRate = MortRate %>% select(endpoint, agegroup, MortRate = !!as.name(y.a))
      )
    } else if (all(serie.step[1:2] %in% c('PA', 'ORF'))) {
      #  PA  ORF
      Mortality(
        FID = FID_info %>% pull(FID),
        RR = RR_table$MEAN,  
        PM_r = PM_real %>% select(FID, concentration = !!as.name(y.b)),
        PM_c = PM_real %>% select(FID, concentration = !!as.name(y.a)),
        pop = Pop %>% select(FID, Pop = !!as.name(y.a)),
        ag = AgeGroup %>% select(agegroup, AgeStruc = !!as.name(y.b)),
        mRate = MortRate %>% select(endpoint, agegroup, MortRate = !!as.name(y.b))
      )
    } else if (all(serie.step[1:2] %in% c('EXP', 'ORF'))) {
      #  EXP ORF
      Mortality(
        FID = FID_info %>% pull(FID),
        RR = RR_table$MEAN,  
        PM_r = PM_real %>% select(FID, concentration = !!as.name(y.b)),
        PM_c = PM_real %>% select(FID, concentration = !!as.name(y.b)),
        pop = Pop %>% select(FID, Pop = !!as.name(y.a)),
        ag = AgeGroup %>% select(agegroup, AgeStruc = !!as.name(y.a)),
        mRate = MortRate %>% select(endpoint, agegroup, MortRate = !!as.name(y.b))
      )
    },
    # Mort.3----
    Mort_3 = if (all(serie.step[1:3] %in% c('PG', 'PA', 'EXP'))) {
      # PG PA EXP
      Mortality(
        FID = FID_info %>% pull(FID),
        RR = RR_table$MEAN,  
        PM_r = PM_real %>% select(FID, concentration = !!as.name(y.a)),
        PM_c = PM_real %>% select(FID, concentration = !!as.name(y.b)),
        pop = Pop %>% select(FID, Pop = !!as.name(y.b)),
        ag = AgeGroup %>% select(agegroup, AgeStruc = !!as.name(y.b)),
        mRate = MortRate %>% select(endpoint, agegroup, MortRate = !!as.name(y.a))
      )
    } else if (all(serie.step[1:3] %in% c('PG', 'PA', 'ORF'))) {
      # PG  PA ORF
      Mortality(
        FID = FID_info %>% pull(FID),
        RR = RR_table$MEAN,  
        PM_r = PM_real %>% select(FID, concentration = !!as.name(y.b)),
        PM_c = PM_real %>% select(FID, concentration = !!as.name(y.a)),
        pop = Pop %>% select(FID, Pop = !!as.name(y.b)),
        ag = AgeGroup %>% select(agegroup, AgeStruc = !!as.name(y.b)),
        mRate = MortRate %>% select(endpoint, agegroup, MortRate = !!as.name(y.b))
      )
    } else if (all(serie.step[1:3] %in% c('PG', 'EXP', 'ORF'))) {
      #  PG	EXP	ORF
      Mortality(
        FID = FID_info %>% pull(FID),
        RR = RR_table$MEAN,  
        PM_r = PM_real %>% select(FID, concentration = !!as.name(y.b)),
        PM_c = PM_real %>% select(FID, concentration = !!as.name(y.b)),
        pop = Pop %>% select(FID, Pop = !!as.name(y.b)),
        ag = AgeGroup %>% select(agegroup, AgeStruc = !!as.name(y.a)),
        mRate = MortRate %>% select(endpoint, agegroup, MortRate = !!as.name(y.b))
      )
    } else if (all(serie.step[1:3] %in% c('PA', 'EXP', 'ORF'))) {
      #  PA	EXP	ORF
      Mortality(
        FID = FID_info %>% pull(FID),
        RR = RR_table$MEAN,  
        PM_r = PM_real %>% select(FID, concentration = !!as.name(y.b)),
        PM_c = PM_real %>% select(FID, concentration = !!as.name(y.b)),
        pop = Pop %>% select(FID, Pop = !!as.name(y.a)),
        ag = AgeGroup %>% select(agegroup, AgeStruc = !!as.name(y.b)),
        mRate = MortRate %>% select(endpoint, agegroup, MortRate = !!as.name(y.b))
      )
    },
    # Mort.End ----
    Mort_4 = Mortality(
      FID = FID_info %>% pull(FID),
      RR = RR_table$MEAN,  
      PM_r = PM_real %>% select(FID, concentration = !!as.name(y.b)),
      PM_c = PM_real %>% select(FID, concentration = !!as.name(y.b)),
      pop = Pop %>% select(FID, Pop = !!as.name(y.b)),
      ag = AgeGroup %>% select(agegroup, AgeStruc = !!as.name(y.b)),
      mRate = MortRate %>% select(endpoint, agegroup, MortRate = !!as.name(y.b))
    )
  )
  
  # Clean Result ----
  
  Decomp <- Decomp %>% imap_dfr(
    ~ .x %>% pivot_longer(
      cols = -FID, names_to = "Cause_Age",values_to = 'Mort'
    ) %>% mutate(Step = .y)
  ) %>% pivot_wider(names_from = 'Step', values_from = 'Mort') %>% mutate(
    Start = Mort_0,
    !!serie.step[1] := Mort_1 - Mort_0,
    !!serie.step[2] := Mort_2 - Mort_1,
    !!serie.step[3] := Mort_3 - Mort_2,
    !!serie.step[4] := Mort_4 - Mort_3,
    End = Mort_4,
    .keep = 'unused'
  )
  
  # Print Result ----
  {
    cat(str_c('Drivers Between', y.a, 'and', y.b, ':\n', sep = ' '))
    cat(str_c(serie.step[1], ':\t', sum(Decomp %>% pull(PA) %>% sum %>% round)),'\n')
    cat(str_c(serie.step[2], ':\t', sum(Decomp %>% pull(PG) %>% sum %>% round)),'\n')
    cat(str_c(serie.step[3], ':\t', sum(Decomp %>% pull(EXP) %>% sum %>% round)),'\n')
    cat(str_c(serie.step[4], ':\t', sum(Decomp %>% pull(ORF) %>% sum %>% round)),'\n')
  }
  
  return(Decomp)

}

# Usage ---- 

Drivers05_15 <- 1:24 %>% map(~ Decomposition(serie = .x, y.a = 2005, y.b = 2015))

## provincial aggregation----
Drivers05_15 %>% imap_dfr(
  ~ .x %>% left_join(FID_info) %>% group_by(Province) %>% 
    summarise(across(Start:End,sum)) %>% mutate(seq = .y, .after = Province)
) %>% split(.$Province) 

## simple national aggregation ----
Drivers05_15 %>% map_dfr(~ .x %>% summarise(across(Start:End,sum)))
