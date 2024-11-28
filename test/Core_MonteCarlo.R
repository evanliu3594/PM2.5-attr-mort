# PM25 Health Impact Calculate Core
# 
#        - Monte Carlo Uncertainty Analysis
#   WARNING: Monte Carlo Analysis IS Extremely Time-Consuming
# 
#                    By Yifan LIU 2022/5/22

library(tidyverse)
library(furrr)

tell.mode <- function(mode = mode) case_when(
  mode %in% c('5COD', 'NCD+LRI') ~ str_glue('GEMM-{mode}'),
  mode %>% str_detect('IER') ~ mode,
  mode %>% str_detect('MRBRT') ~ mode
)

draw.RR <- function(x) x %>% mutate(
  RR = map2_dbl(MEAN, (UP - LOW) / 2, ~ rnorm(n = 1, mean = .x, sd = .y)),
  .keep = 'unused')

draw.PM <- function(x, uncert = .12) x %>% mutate(
  concentration = map_chr(as.numeric(concentration), ~ rnorm(1, .x, .x * uncert/1.96) %>% matchable(1))
)

matchable <- function(num, digit = 1) num %>% round(digit) %>% str_c

mortrate_std <- function(x) x %>% mutate(endpoint = tolower(endpoint))

RR_std_for_mtcl <- function(RR_table, mode) {
  
  RR_tbl <- RR_table %>% imap_dfr(
    ~ .x %>% mutate(CI = .y) %>% 
      rename_with(~'UP', matches('UP')) %>% 
      rename_with(~'LOW', matches('LOW')) %>% 
      pivot_longer(
        cols = -c(concentration, CI), values_to = "RR",
        names_to = c("endpoint", "agegroup"),names_sep = '_')
  ) %>% pivot_wider(names_from = 'CI',values_from = 'RR') %>% 
    mutate(endpoint = tolower(endpoint))
  
  RR_reshape <- if (mode == 'NCD+LRI') {
    
    expand_grid(
      concentration = RR_table[[1]] %>% pull(concentration),
      endpoint = c('ncd+lri'),
      agegroup = c('ALL', seq(25, 95, 5) %>% matchable(0))
    ) %>% left_join(
      RR_tbl, by = c('concentration', 'endpoint', 'agegroup')
    ) %>% group_by(concentration, endpoint) %>% fill(MEAN, LOW, UP) %>%
      filter(agegroup != 'ALL') %>% ungroup
    
  } else if (mode == '5COD') {
    
    expand_grid(
      concentration = RR_table[[1]] %>% pull(concentration),
      endpoint = c('copd', 'ihd', 'lc', 'lri', 'stroke'),
      agegroup = c('ALL', seq(25, 95, 5) %>% matchable(0))
    ) %>% left_join(
      RR_tbl, by = c('concentration', 'endpoint', 'agegroup')
    ) %>% group_by(concentration, endpoint) %>% fill(MEAN, LOW, UP) %>%
      filter(agegroup != 'ALL') %>% ungroup
    
  } else if (mode %>% str_detect('IER')) {
    
    expand_grid(
      concentration = RR_table[[1]] %>% pull(concentration),
      endpoint = c('copd', 'ihd', 'lc', 'stroke', 'lri'),
      agegroup = c('ALL', seq(0, 95, 5) %>% matchable(0))
    ) %>% left_join(
      RR_tbl, by = c('concentration', 'endpoint', 'agegroup')
    ) %>% group_by(concentration, endpoint) %>%
      fill(MEAN, LOW, UP) %>% filter(agegroup != 'ALL') %>%
      filter(
        endpoint %in% c('copd', 'ihd', 'lc', 'stroke') &
          as.integer(agegroup) >= 25 | endpoint == 'lri'
      ) %>% ungroup
  } else if (mode == 'MRBRT') {
    
    expand_grid(
      concentration = RR_table[[1]] %>% pull(concentration),
      endpoint = c('copd', 'dm', 'ihd', 'lc', 'lri', 'stroke'),
      agegroup = c('ALL', seq(0, 95, 5) %>% matchable(0))
    ) %>% left_join(
      RR_tbl, by = c('concentration', 'endpoint', 'agegroup')
    ) %>% group_by(concentration, endpoint) %>%
      fill(MEAN, LOW, UP) %>% filter(agegroup != 'ALL') %>% filter(
        endpoint %in% c('copd', 'dm', 'ihd', 'lc', 'stroke') &
          as.integer(agegroup) >= 25 | endpoint == 'lri'
      ) %>% ungroup
  }
}

Mortality_mtcl <- function(RR, PM_r, PM_c, ag, mRate, pop, core = 10, n = 10000, mode){
  
  
  plan(multisession, workers = core)
  
  MTCL <- 1:n %>% future_map(.options = furrr_options(seed = NULL), ~ {
    
    RR_tbl <- RR_std_for_mtcl(RR_table = RR, mode = mode) %>% draw.RR 
    
    PM_r %>% 
      left_join(RR_tbl, by = 'concentration') %>% 
      left_join(pop, by = 'FID') %>% 
      group_by(endpoint, agegroup) %>%
      mutate(PWRR_real = weighted.mean(RR, Pop)) %>% 
      ungroup %>% 
      rename(RR_cf = RR) %>% 
      left_join(ag, by = 'agegroup') %>%
      left_join(mRate %>% mortrate_std, by  = c('agegroup', 'endpoint')) %>%
      mutate(
        Mort = Pop * AgeStruc * MortRate * (RR_cf - 1) / PWRR_real / 1e5,
        .keep = 'unused'
      ) %>% summarise(Mort = sum(Mort)) %>% deframe
  })
  
  plan(sequential)
  
  return(MTCL)
}

# test ----

# y = 2005

result <- Mortality_mtcl(
  mode = mode,
  RR = RR_table,
  PM_r = PM_real %>% select(FID, concentration = !!as.name(y)),
  PM_c = PM_cf %>% select(FID, concentration = !!as.name(y)),
  pop = Pop %>% select(FID, Pop = !!as.name(y)),
  ag = AgeGroup %>% select(agegroup, AgeStruc = !!as.name(y)),
  mRate = MortRate %>% select(endpoint, agegroup, MortRate = !!as.name(y))
)

result <- Mortality_mtcl(
  mode = mode,
  RR = RR_table,
  PM_r = left_join(
    PM_real %>% select(FID, concentration = !!as.name(y)),
    Pop %>% select(FID, Pop = !!as.name(y)),
    by = 'FID'
  ) %>% summarise(concentration = weighted.mean(as.numeric(concentration),Pop) %>% matchable) %>% 
    mutate(FID = '000'),
  PM_c = left_join(
    PM_cf %>% select(FID, concentration = !!as.name(y)),
    Pop %>% select(FID, Pop = !!as.name(y)),
    by = 'FID'
  ) %>% summarise(concentration = weighted.mean(as.numeric(concentration),Pop) %>% matchable) %>% 
    mutate(FID = '000'),
  pop = Pop %>% summarise(Pop = !!as.name(y) %>% sum) %>% mutate(FID = '000'),
  mRate = MortRate %>% select(endpoint, agegroup, MortRate = !!as.name(y)),
  ag =  AgeGroup %>% select(agegroup, AgeStruc = !!as.name(y))
)

result %>% unlist %>% quantile(c(.025, .5, .975))

