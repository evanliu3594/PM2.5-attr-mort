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
  GRID = './Data/GRID_information_instance_220628.xlsx',
  Pop = './Data/GridPop_instance_220628.xlsx',
  PM_real = './Data/GridPM25_instance_220628.xlsx',
  PM_cf = './Data/PM_Ctrl.csv', # PM_cf works only in counter-fact scenario
  MortRate = './Data/GBD_mortality_instance_220628.xlsx',
  AgeGroup = './Data/GBD_agestructure_instance_220628.xlsx'
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
    Mort_0 = Mortality(Grids = Grid_info,
                       RR = RR_table$MEAN,
                       pop = Pop %>% select(x:y, Pop = !!start.y),
                       ag = AgeGroup %>% select(domain, agegroup, AgeStruc = !!start.y),
                       PM_c = PM_real %>% select(x:y, concentration = !!start.y),
                       PM_r = PM_real %>% select(x:y, concentration = !!start.y),
                       mRate = mortrate_std(MortRate, start.y)), 
    # Mort.1----
    Mort_1 = if (serie.step[1] == 'PG') {
      Mortality(Grids = Grid_info,
                RR = RR_table$MEAN,
                pop = Pop %>% select(x:y, Pop = !!end.y),
                ag = AgeGroup %>% select(domain, agegroup, AgeStruc = !!start.y),
                PM_c = PM_real %>% select(x:y, concentration = !!start.y),
                PM_r = PM_real %>% select(x:y, concentration = !!start.y),
                mRate = mortrate_std(MortRate, start.y))
    } else if (serie.step[1] == 'PA') {
      Mortality(Grids = Grid_info,
                RR = RR_table$MEAN,
                pop = Pop %>% select(x:y, Pop = !!start.y),
                ag = AgeGroup %>% select(domain, agegroup, AgeStruc = !!end.y),
                PM_c = PM_real %>% select(x:y, concentration = !!start.y),
                PM_r = PM_real %>% select(x:y, concentration = !!start.y),
                mRate = mortrate_std(MortRate, start.y))
    } else if (serie.step[1] == 'EXP') {
      Mortality(Grids = Grid_info,
                RR = RR_table$MEAN,
                pop = Pop %>% select(x:y, Pop = !!start.y),
                ag = AgeGroup %>% select(domain, agegroup, AgeStruc = !!start.y),
                PM_c = PM_real %>% select(x:y, concentration = !!end.y),
                PM_r = PM_real %>% select(x:y, concentration = !!start.y),
                mRate = mortrate_std(MortRate, start.y))
    } else if (serie.step[1] == 'ORF') {
      Mortality(Grids = Grid_info,
                RR = RR_table$MEAN,
                pop = Pop %>% select(x:y, Pop = !!start.y),
                ag = AgeGroup %>% select(domain, agegroup, AgeStruc = !!start.y),
                PM_c = PM_real %>% select(x:y, concentration = !!start.y),
                PM_r = PM_real %>% select(x:y, concentration = !!end.y),
                mRate = mortrate_std(MortRate, end.y))
    },
    # Mort.2----
    Mort_2 = if (all(serie.step[1:2] %in% c('PG','PA'))) {
      # PG  PA
      Mortality(Grids = Grid_info,
                RR = RR_table$MEAN,
                pop = Pop %>% select(x:y, Pop = !!end.y),
                ag = AgeGroup %>% select(domain, agegroup, AgeStruc = !!end.y),
                PM_c = PM_real %>% select(x:y, concentration = !!start.y),
                PM_r = PM_real %>% select(x:y, concentration = !!start.y),
                mRate = mortrate_std(MortRate, start.y))
    } else if (all(serie.step[1:2] %in% c('PG','EXP'))) {
      #  PG  EXP
      Mortality(Grids = Grid_info,
                RR = RR_table$MEAN,
                pop = Pop %>% select(x:y, Pop = !!end.y),
                ag = AgeGroup %>% select(domain, agegroup, AgeStruc = !!start.y),
                PM_c = PM_real %>% select(x:y, concentration = !!end.y),
                PM_r = PM_real %>% select(x:y, concentration = !!start.y),
                mRate = mortrate_std(MortRate, start.y))
    } else if (all(serie.step[1:2] %in% c('PG', 'ORF'))) {
      #  PG  ORF
      Mortality(Grids = Grid_info,
                RR = RR_table$MEAN,
                pop = Pop %>% select(x:y, Pop = !!end.y),
                ag = AgeGroup %>% select(domain, agegroup, AgeStruc = !!start.y),
                PM_c = PM_real %>% select(x:y, concentration = !!start.y),
                PM_r = PM_real %>% select(x:y, concentration = !!end.y),
                mRate = mortrate_std(MortRate, end.y))
    } else if (all(serie.step[1:2] %in% c('PA', 'EXP'))) {
      #  PA  EXP
      Mortality(Grids = Grid_info,
                RR = RR_table$MEAN,
                pop = Pop %>% select(x:y, Pop = !!start.y),
                ag = AgeGroup %>% select(domain, agegroup, AgeStruc = !!end.y),
                PM_c = PM_real %>% select(x:y, concentration = !!end.y),
                PM_r = PM_real %>% select(x:y, concentration = !!start.y),
                mRate = mortrate_std(MortRate, start.y))
    } else if (all(serie.step[1:2] %in% c('PA', 'ORF'))) {
      #  PA  ORF
      Mortality(Grids = Grid_info,
                RR = RR_table$MEAN,
                pop = Pop %>% select(x:y, Pop = !!start.y),
                ag = AgeGroup %>% select(domain, agegroup, AgeStruc = !!end.y),
                PM_c = PM_real %>% select(x:y, concentration = !!start.y),
                PM_r = PM_real %>% select(x:y, concentration = !!end.y),
                mRate = mortrate_std(MortRate, end.y))
    } else if (all(serie.step[1:2] %in% c('EXP', 'ORF'))) {
      #  EXP ORF
      Mortality(Grids = Grid_info,
                RR = RR_table$MEAN,
                pop = Pop %>% select(x:y, Pop = !!start.y),
                ag = AgeGroup %>% select(domain, agegroup, AgeStruc = !!start.y),
                PM_c = PM_real %>% select(x:y, concentration = !!end.y),
                PM_r = PM_real %>% select(x:y, concentration = !!end.y),
                mRate = mortrate_std(MortRate, end.y))
    },
    # Mort.3----
    Mort_3 = if (all(serie.step[1:3] %in% c('PG', 'PA', 'EXP'))) {
      # PG PA EXP
      Mortality(Grids = Grid_info,
                RR = RR_table$MEAN,
                pop = Pop %>% select(x:y, Pop = !!end.y),
                ag = AgeGroup %>% select(domain, agegroup, AgeStruc = !!end.y),
                PM_c = PM_real %>% select(x:y, concentration = !!end.y),
                PM_r = PM_real %>% select(x:y, concentration = !!start.y),
                mRate = mortrate_std(MortRate, start.y))
    } else if (all(serie.step[1:3] %in% c('PG', 'PA', 'ORF'))) {
      # PG  PA ORF
      Mortality(Grids = Grid_info,
                RR = RR_table$MEAN,
                pop = Pop %>% select(x:y, Pop = !!end.y),
                ag = AgeGroup %>% select(domain, agegroup, AgeStruc = !!end.y),
                PM_c = PM_real %>% select(x:y, concentration = !!start.y),
                PM_r = PM_real %>% select(x:y, concentration = !!end.y),
                mRate = mortrate_std(MortRate, end.y))
    } else if (all(serie.step[1:3] %in% c('PG', 'EXP', 'ORF'))) {
      #  PG	EXP	ORF
      Mortality(Grids = Grid_info,
                RR = RR_table$MEAN,
                pop = Pop %>% select(x:y, Pop = !!end.y),
                ag = AgeGroup %>% select(domain, agegroup, AgeStruc = !!start.y),
                PM_c = PM_real %>% select(x:y, concentration = !!end.y),
                PM_r = PM_real %>% select(x:y, concentration = !!end.y),
                mRate = mortrate_std(MortRate, end.y))
    } else if (all(serie.step[1:3] %in% c('PA', 'EXP', 'ORF'))) {
      #  PA	EXP	ORF
      Mortality(Grids = Grid_info,
                RR = RR_table$MEAN,
                pop = Pop %>% select(x:y, Pop = !!start.y),
                ag = AgeGroup %>% select(domain, agegroup, AgeStruc = !!end.y),
                PM_c = PM_real %>% select(x:y, concentration = !!end.y),
                PM_r = PM_real %>% select(x:y, concentration = !!end.y),
                mRate = mortrate_std(MortRate, end.y))
    },
    # Mort.End ----
    Mort_4 = Mortality(Grids = Grid_info,
                       RR = RR_table$MEAN,
                       pop = Pop %>% select(x:y, Pop = !!end.y),
                       ag = AgeGroup %>% select(domain, agegroup, AgeStruc = !!end.y),
                       PM_c = PM_real %>% select(x:y, concentration = !!end.y),
                       PM_r = PM_real %>% select(x:y, concentration = !!end.y),
                       mRate = mortrate_std(MortRate, end.y))
  )
  
  # Clean Result ----
  
  Decomp <- Decomp %>% imap_dfr(
    ~ .x %>% pivot_longer(
      cols = -c(x,y,Country), names_to = "Cause_Age",values_to = 'Mort'
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

aggregate_drivers <- function(RUN24 = T, start.year = 'base2015', end.year = 'SSP1-Baseline_2030') {
  
  n <- if (RUN24) 1:24 else 1
  
  ls1 <- n %>% set_names() %>% map(
    ~ Decomposition(serie = .x, start.y = start.year, end.y = end.year) %>% 
      group_by(Country) %>% summarise(across(Start:End, sum))
  )
  
  ls1[["Summary"]] <- ls1 %>% bind_rows %>% 
    group_by(Country) %>% summarise(across(Start:End, mean)) 
  
  ls1 %>% write_xlsx(str_glue(
    "./Result/{tell_CR()}_DrivingFactors\\
    _{start.year}-{end.year}_\\
    Build{format(Sys.Date(), '%y%m%d')}.xlsx"
  ))
}

# Usage ---- 


# Drivers_ <- 1:24 %>% map(~ Decomposition(serie = .x, start.y = 'base2015', end.y = 'SSP1-Baseline_2030'))
# 
## simple national aggregation ----
# Drivers_ %>% map_dfr(~ .x) %>% group_by(Country) %>% summarise(across(Start:End,))

aggregate_drivers(start.year = 'base2015', end.year = 'SSP1-Baseline_2030')
