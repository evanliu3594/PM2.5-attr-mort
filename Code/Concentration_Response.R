# Scripts for Concentration-RR relation generation

library(tidyverse)
library(readxl)
library(writexl)

simpledate <- \() format(Sys.Date(),'%y%m%d')

matchable <- \(num, dgt = 1) num %>% round(digits = dgt) %>% str_c

# utils ----

## tidyverse ----
compute_RR_index <- function(ERL, write = F) {
  
  para.file <- case_when(
    ERL == 'IER2010' ~ './Data/CRF_coefficients/IER2010_parameters.csv',
    ERL == 'IER2013' ~ './Data/CRF_coefficients/IER2013_parameters.csv',
    ERL == 'IER2015' ~ './Data/CRF_coefficients/IER2015_parameters.csv',
    ERL == 'IER2017' ~ './Data/CRF_coefficients/IER2017_parameters.csv',
    ERL == 'GEMM' ~ './Data/CRF_coefficients/GEMM-parameters.csv'
  )
  
  iter_conc <- seq(0, 300, .1)
  
  ier_iter_conc <- \(alpha, beta, gamma, tmrel, ...) iter_conc %>% set_names %>% 
    map_dbl( ~ alpha * (1 - exp(-1 * beta * max(.x - tmrel, 0) ^ gamma)) + 1)
  
  RR_table <- if (str_detect(ERL, 'IER')) {
    
    read_csv(para.file) %>%
      mutate(age = str_replace(age, '(A|a)ll ?(A|a)ge', 'ALL'),
             cause = str_replace(cause , 'ALRI', 'LRI')) %>%
      rename_with( ~ 'tmrel', matches('zcf')) %>%
      rename_with( ~ 'gamma', matches('delta')) %>%
      mutate(RR = pmap(select(., alpha, beta, gamma, tmrel), ier_iter_conc)) %>%
      unnest_longer(RR, values_to = 'RR', indices_to = 'concentration') %>%
      group_by(cause, age, concentration) %>%
      summarise(MEAN = mean(RR), LOW = quantile(RR, .025), UP = quantile(RR, .975))
    
  } else if (str_detect(ERL, 'GEMM')) {
    
      expand_grid(
        concentration = iter_conc, read_csv(para.file)
      ) %>% mutate(
        Causes = str_replace(Causes , 'ALRI', 'LRI'),
        Age = str_replace(Age, '(A|a)ll ?(A|a)ge', 'ALL'),
        z = pmax(concentration - 2.4, 0),
        concentration = concentration  %>% matchable
      ) %>% mutate(
        .keep = 'unused',
        MEAN = exp((theta) * log(1 + z / alpha) / (1 + exp((mu - z) / gama))),
        UP = exp((theta + 1.96 * SE.theta) * log(1 + z / alpha) / (1 + exp((mu - z) / gama))),
        LOW = exp((theta - 1.96 * SE.theta) * log(1 + z / alpha) / (1 + exp((mu - z) / gama)))
      ) %>% rename(cause = Causes, age = Age)
  }
  
  RR_index <- RR_table %>% 
    pivot_longer(cols = c('MEAN', 'LOW', 'UP'), names_to = 'CI', values_to = 'RR') %>% 
    pivot_wider(names_from = c('cause', 'age'), names_sep = '_',values_from = 'RR') %>% 
    split(.$CI) %>% map( ~ select(.x, -CI))
  
  if (write) RR_index %>% 
    write_xlsx(str_glue('./Data/RR_index/{ERL}_Lookup_Table_Build_{simpledate()}.xlsx'))
  
  return(RR_index)
}

## tidytable ----

compute_RR_index. <- function(ERL, write = F) {
  
  para.file <- case_when(
    ERL == 'IER2010' ~ './Data/CRF_coefficients/IER2010_parameters.csv',
    ERL == 'IER2013' ~ './Data/CRF_coefficients/IER2013_parameters.csv',
    ERL == 'IER2015' ~ './Data/CRF_coefficients/IER2015_parameters.csv',
    ERL == 'IER2017' ~ './Data/CRF_coefficients/IER2017_parameters.csv',
    ERL == 'GEMM' ~ './Data/CRF_coefficients/GEMM-parameters.csv'
  )
  
  iter_conc <- seq(0, 300, .1)
  
  ier_iter_conc. <- \(alpha, beta, gamma, tmrel, ...) {
    iter_conc %>% set_names %>%
      map_dbl.(~ alpha * (1 - exp(-1 * beta * max(.x - tmrel, 0) ^ gamma)) + 1)
  }
  
  RR_table <- if (str_detect(ERL, 'IER')) {
    expand_grid.(Concentration = iter_conc, read_csv(para.file)) %>% 
      mutate.(
        age = str_replace(age, '(A|a)ll ?(A|a)ge', 'ALL'),
        cause = str_replace(cause , 'ALRI', 'LRI')) %>% 
      rename_with.(~ 'tmrel', matches('zcf')) %>% 
      rename_with.(~ 'gamma', matches('delta')) %>% 
      mutate.(
        .keep = 'unused',
        RR = alpha * (1 - exp(-1 * beta * pmax(Concentration - tmrel, 0) ^ gamma)) + 1,
        concentration = Concentration %>% matchable ) %>% 
      summarise.(
        .by = c(cause, age, concentration),
        MEAN = mean(RR),
        LOW = quantile(RR, .025),
        UP = quantile(RR, .975))
  } else if (str_detect(ERL, 'GEMM')) {
    expand_grid.(
      concentration = iter_conc,
      read_csv(para.file)
    ) %>% mutate.(
      Causes = str_replace(Causes , 'ALRI', 'LRI'),
      Age = str_replace(Age, '(A|a)ll ?(A|a)ge', 'ALL'),
      z = pmax(concentration - 2.4, 0),
      concentration = concentration  %>% matchable
    ) %>% mutate.(
      .keep = 'unused',
      MEAN = exp((theta) * log(1 + z / alpha) / (1 + exp((mu - z) / gama))),
      UP = exp((theta + 1.96 * SE.theta) * log(1 + z / alpha) / (1 + exp((mu - z) / gama))),
      LOW = exp((theta - 1.96 * SE.theta) * log(1 + z / alpha) / (1 + exp((mu - z) / gama)))
    ) %>% rename.(cause = Causes, age = Age)
  }
  
  RR_index <- RR_table %>%
    pivot_longer.(
      cols = c('MEAN', 'LOW', 'UP'),
      names_to = 'CI',
      values_to = 'RR'
    ) %>%
    pivot_wider.(
      names_from = c('cause', 'age'),
      names_sep = '_',
      values_from = 'RR'
    ) %>%
    split(.$CI) %>% map.(~ select(.x,-CI))
  
  if (write)
    RR_index %>% write_xlsx(str_glue(
      './Data/RR_index/{ERL}_Lookup_Table_Build_{simpledate()}.xlsx'
    ))
}
# Usage ----

compute_RR_index('GEMM', write = F)

CRFs <- c('IER2010', "IER2013", "IER2015", "IER2017", "GEMM")

CRFs %>% map(compute_RR_index, write = F)

compute_RR_index.('GEMM', write = F)

CRFs <- c('IER2010', "IER2013", "IER2015", "IER2017", "GEMM")

CRFs %>% walk.(compute_RR_index., write = F)

# efficient test ----

## test dplyr ----

system.time(
  compute_RR_index("GEMM")
)

system.time(
  compute_RR_index("IER2017")
)

gc()

## test tidytable ----
system.time(
  compute_RR_index.("GEMM")
)


system.time(
  compute_RR_index.("IER2017")
)

# try multicore ----

## doParallel ----

# library(doParallel)
# 
# cl <- makeCluster(length(CRFs))
# 
# registerDoParallel(cl)
# 
# foreach(CRF = CRFs, .packages = c('tidyverse','readxl','writexl')) %dopar% {
#   compute_RR_index(ERL = CRF)
# }
# 
# stopCluster(cl)

## furrr ----

library(furrr)

plan(multisession, workers = length(CRFs))

result <- CRFs %>% future_map(compute_RR_index)

plan(sequential)

