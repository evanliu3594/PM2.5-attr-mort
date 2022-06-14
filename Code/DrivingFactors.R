#====================================================#
# retrieve driving forces to the PM2.5 health burden #
#                         Depending: Core-V220603    #
#====================================================#

# Core Module Load ----

source('./Code/Core.R',encoding = 'UTF8')

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

Decomposition <- function(serie, start.y, end.y) {
  
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
      RR = RR_table$MEAN,  
      Grids = Grid_info %>% select(x:y),
      pop = Pop %>% select(x:y, Pop = !!start.y),
      ag = AgeGroup %>% select(agegroup, AgeStruc = !!start.y),
      PM_c = PM_real %>% select(x:y, concentration = !!start.y),
      PM_r = PM_real %>% select(x:y, concentration = !!start.y),
      mRate = mortrate_std(MortRate, start.y)
    ),
    # Mort.1----
    Mort_1 = if (serie.step[1] == 'PG') {
      Mortality(RR = RR_table$MEAN,  
                Grids = Grid_info %>% select(x:y),
                pop = Pop %>% select(x:y, Pop = !!end.y),
                ag = AgeGroup %>% select(agegroup, AgeStruc = !!start.y),
                PM_c = PM_real %>% select(x:y, concentration = !!start.y),
                PM_r = PM_real %>% select(x:y, concentration = !!start.y),
                mRate = mortrate_std(MortRate, start.y))
    } else if (serie.step[1] == 'PA') {
      Mortality(RR = RR_table$MEAN,  
                Grids = Grid_info %>% select(x:y),
                pop = Pop %>% select(x:y, Pop = !!start.y),
                ag = AgeGroup %>% select(agegroup, AgeStruc = !!end.y),
                PM_c = PM_real %>% select(x:y, concentration = !!start.y),
                PM_r = PM_real %>% select(x:y, concentration = !!start.y),
                mRate = mortrate_std(MortRate, start.y))
    } else if (serie.step[1] == 'EXP') {
      Mortality(RR = RR_table$MEAN,  
                Grids = Grid_info %>% select(x:y),
                pop = Pop %>% select(x:y, Pop = !!start.y),
                ag = AgeGroup %>% select(agegroup, AgeStruc = !!start.y),
                PM_c = PM_real %>% select(x:y, concentration = !!end.y),
                PM_r = PM_real %>% select(x:y, concentration = !!start.y),
                mRate = mortrate_std(MortRate, start.y))
    } else if (serie.step[1] == 'ORF') {
      Mortality(RR = RR_table$MEAN,
                Grids = Grid_info %>% select(x:y),
                pop = Pop %>% select(x:y, Pop = !!start.y),
                ag = AgeGroup %>% select(agegroup, AgeStruc = !!start.y),
                PM_c = PM_real %>% select(x:y, concentration = !!start.y),
                PM_r = PM_real %>% select(x:y, concentration = !!end.y),
                mRate = mortrate_std(MortRate, end.y))
    },
    # Mort.2----
    Mort_2 = if (all(serie.step[1:2] %in% c('PG','PA'))) {
      # PG  PA
      Mortality(RR = RR_table$MEAN,
                Grids = Grid_info %>% select(x:y),
                pop = Pop %>% select(x:y, Pop = !!end.y),
                ag = AgeGroup %>% select(agegroup, AgeStruc = !!end.y),
                PM_c = PM_real %>% select(x:y, concentration = !!start.y),
                PM_r = PM_real %>% select(x:y, concentration = !!start.y),
                mRate = mortrate_std(MortRate, start.y))
    } else if (all(serie.step[1:2] %in% c('PG','EXP'))) {
      #  PG  EXP
      Mortality(RR = RR_table$MEAN,
                Grids = Grid_info %>% select(x:y),
                pop = Pop %>% select(x:y, Pop = !!end.y),
                ag = AgeGroup %>% select(agegroup, AgeStruc = !!start.y),
                PM_c = PM_real %>% select(x:y, concentration = !!end.y),
                PM_r = PM_real %>% select(x:y, concentration = !!start.y),
                mRate = mortrate_std(MortRate, start.y))
    } else if (all(serie.step[1:2] %in% c('PG', 'ORF'))) {
      #  PG  ORF
      Mortality(RR = RR_table$MEAN,
                Grids = Grid_info %>% select(x:y),
                pop = Pop %>% select(x:y, Pop = !!end.y),
                ag = AgeGroup %>% select(agegroup, AgeStruc = !!start.y),
                PM_c = PM_real %>% select(x:y, concentration = !!start.y),
                PM_r = PM_real %>% select(x:y, concentration = !!end.y),
                mRate = mortrate_std(MortRate, end.y))
    } else if (all(serie.step[1:2] %in% c('PA', 'EXP'))) {
      #  PA  EXP
      Mortality(RR = RR_table$MEAN,
                Grids = Grid_info %>% select(x:y),
                pop = Pop %>% select(x:y, Pop = !!start.y),
                ag = AgeGroup %>% select(agegroup, AgeStruc = !!end.y),
                PM_c = PM_real %>% select(x:y, concentration = !!end.y),
                PM_r = PM_real %>% select(x:y, concentration = !!start.y),
                mRate = mortrate_std(MortRate, start.y))
    } else if (all(serie.step[1:2] %in% c('PA', 'ORF'))) {
      #  PA  ORF
      Mortality(RR = RR_table$MEAN,
                Grids = Grid_info %>% select(x:y),
                pop = Pop %>% select(x:y, Pop = !!start.y),
                ag = AgeGroup %>% select(agegroup, AgeStruc = !!end.y),
                PM_c = PM_real %>% select(x:y, concentration = !!start.y),
                PM_r = PM_real %>% select(x:y, concentration = !!end.y),
                mRate = mortrate_std(MortRate, end.y))
    } else if (all(serie.step[1:2] %in% c('EXP', 'ORF'))) {
      #  EXP ORF
      Mortality(RR = RR_table$MEAN,
                Grids = Grid_info %>% select(x:y),
                pop = Pop %>% select(x:y, Pop = !!start.y),
                ag = AgeGroup %>% select(agegroup, AgeStruc = !!start.y),
                PM_c = PM_real %>% select(x:y, concentration = !!end.y),
                PM_r = PM_real %>% select(x:y, concentration = !!end.y),
                mRate = mortrate_std(MortRate, end.y))
    },
    # Mort.3----
    Mort_3 = if (all(serie.step[1:3] %in% c('PG', 'PA', 'EXP'))) {
      # PG PA EXP
      Mortality(RR = RR_table$MEAN,
                Grids = Grid_info %>% select(x:y),
                pop = Pop %>% select(x:y, Pop = !!end.y),
                ag = AgeGroup %>% select(agegroup, AgeStruc = !!end.y),
                PM_c = PM_real %>% select(x:y, concentration = !!end.y),
                PM_r = PM_real %>% select(x:y, concentration = !!start.y),
                mRate = mortrate_std(MortRate, start.y))
    } else if (all(serie.step[1:3] %in% c('PG', 'PA', 'ORF'))) {
      # PG  PA ORF
      Mortality(RR = RR_table$MEAN,  
                Grids = Grid_info %>% select(x:y),
                pop = Pop %>% select(x:y, Pop = !!end.y),
                ag = AgeGroup %>% select(agegroup, AgeStruc = !!end.y),
                PM_c = PM_real %>% select(x:y, concentration = !!start.y),
                PM_r = PM_real %>% select(x:y, concentration = !!end.y),
                mRate = mortrate_std(MortRate, end.y))
    } else if (all(serie.step[1:3] %in% c('PG', 'EXP', 'ORF'))) {
      #  PG	EXP	ORF
      Mortality(RR = RR_table$MEAN,
                Grids = Grid_info %>% select(x:y),
                pop = Pop %>% select(x:y, Pop = !!end.y),
                ag = AgeGroup %>% select(agegroup, AgeStruc = !!start.y),
                PM_c = PM_real %>% select(x:y, concentration = !!end.y),
                PM_r = PM_real %>% select(x:y, concentration = !!end.y),
                mRate = mortrate_std(MortRate, end.y))
    } else if (all(serie.step[1:3] %in% c('PA', 'EXP', 'ORF'))) {
      #  PA	EXP	ORF
      Mortality(RR = RR_table$MEAN,
                Grids = Grid_info %>% select(x:y),
                pop = Pop %>% select(x:y, Pop = !!start.y),
                ag = AgeGroup %>% select(agegroup, AgeStruc = !!end.y),
                PM_c = PM_real %>% select(x:y, concentration = !!end.y),
                PM_r = PM_real %>% select(x:y, concentration = !!end.y),
                mRate = mortrate_std(MortRate, end.y))
    },
    # Mort.End ----
    Mort_4 = Mortality(
      RR = RR_table$MEAN,
      Grids = Grid_info %>% select(x:y),
      pop = Pop %>% select(x:y, Pop = !!end.y),
      ag = AgeGroup %>% select(agegroup, AgeStruc = !!end.y),
      PM_c = PM_real %>% select(x:y, concentration = !!end.y),
      PM_r = PM_real %>% select(x:y, concentration = !!end.y),
      mRate = mortrate_std(MortRate, end.y)
    )
  )
  
  # Clean Result ----
  
  Decomp <- Decomp %>% imap_dfr(
    ~ .x %>% pivot_longer(
      cols = -c(x,y), names_to = "Cause_Age",values_to = 'Mort'
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
    cat(str_c('Drivers Between', start.y, 'and', end.y, ':\n', sep = ' '))
    cat(str_c(serie.step[1], ':\t', sum(Decomp %>% pull(PA) %>% sum %>% round)),'\n')
    cat(str_c(serie.step[2], ':\t', sum(Decomp %>% pull(PG) %>% sum %>% round)),'\n')
    cat(str_c(serie.step[3], ':\t', sum(Decomp %>% pull(EXP) %>% sum %>% round)),'\n')
    cat(str_c(serie.step[4], ':\t', sum(Decomp %>% pull(ORF) %>% sum %>% round)),'\n')
  }
  
  return(Decomp)

}

# Usage ---- 

Drivers05_15 <- 1:24 %>% map(~ Decomposition(serie = .x, start.y = '2005', end.y = '2015'))

## provincial aggregation----
Drivers05_15 %>% imap_dfr(
  ~ .x %>% left_join(Grid_info) %>% group_by(Province) %>% 
    summarise(across(Start:End,sum)) %>% mutate(seq = .y, .after = Province)
) %>% group_nest(Province) %>% deframe

## simple national aggregation ----
Drivers05_15 %>% map_dfr(~ .x %>% summarise(across(Start:End,sum)))
