# PM25 Health Impact Calc Core
# By Yifan LIU 2021/05/15
# Modify by Yifan LIU 2022/04/09
# Modified By yifan liu on 23/12/19

library(tidyverse)
library(writexl)
library(readxl)

#' detect CRF model name and generate a name for file output
#'
#' @return formatted string of CRF name
#'
tell_Model <- function() {
  return(
    if (.CR_Model %>% str_detect('IER'))
      str_c(.CR_Model)
    else if (.CR_Model %in% c('5COD', 'NCD+LRI'))
      str_c('PM2.5_GEMM', .CR_Model, sep = '_')
    else if (.CR_Model == 'MRBRT')
      str_c("PM2.5_", .CR_Model)
    else if (.CR_Model == 'O3')
      str_c(.CR_Model) 
    else if (.CR_Model == 'NO2')
      str_c(.CR_Model) 
  )
}

#' set CRF for calculation
#'
#' @param Model string, one of `IER`, `NCD+LRI`, `5COD`, `MRBRT`, `O3` or `NO2`
#'
#' @export
#' 
set_Model <- function(Model) {
  assign(".CR_Model", Model, envir = globalenv())
  
  if (str_detect(Model, "IER|NCD\\+LRI|5COD|MRBRT|O3|NO2")) {
    cat(str_glue(
      "C-R Model \"{tell_Model()}\" is set as the default methodology"
    ))
  } else {
    warning(str_glue(
      "\"{Model}\" is not a bnuilt-in CR model, \\
      please provide a corrosponding `RR_table` after `read_file()`"
    ))
  }
}

#' format numbers to a string at a specified digit 
#'
#' @param num input number
#' @param dgt int, refers to rounding digit
#'
#' @return string of number
#' @export
#'
#' @examples
#' matchable(3.33333, 1)
matchable <- function(num, dgt = 2) {
  as.character(round(num, dgt))
}

#' standardize CR look-up table
#'
#' @param index string, specifying witch RR table to use, by default the "MEAN" RR.
#'
#' @return a formatted RR table
#' @export
#'
RR_std <- function(index = "MEAN") {
  
  CR <-  if (!exists('.CR_Model', envir = globalenv())) {
    stop("Please specify a C-R function with `set_Model()`")
  } else get(".CR_Model", envir = globalenv())
  
  RR <- if (.CR_Model == 'MRBRT') {
    MRBRT_Lookup_Table
  } else if (.CR_Model %in% c('NCD+LRI', '5COD')) {
    GEMM_Lookup_Table
  } else if (.CR_Model %in% c('IER', 'IER2017')) {
    IER2017_Lookup_Table
  } else if (.CR_Model == 'IER2015') {
    IER2015_Lookup_Table
  } else if (.CR_Model == 'IER2013') {
    IER2013_Lookup_Table
  } else if (.CR_Model == 'IER2010') {
    IER2010_Lookup_Table
  } else if (.CR_Model == "O3") {
    O3_CR_Lookup_Table
  } else if (.CR_Model == "NO2") {
    NO2_CR_Lookup_Table
  } else {
    NA_character_
  }
  
  RR_tbl <- RR[[index]] %>% pivot_longer(
    cols = -dose,
    values_to = "RR",
    names_to = c("endpoint", "age"),
    names_sep = '_'
  ) %>% mutate(endpoint = tolower(endpoint))
  
  RR_reshape <- if (CR == '5COD') {
    expand_grid(
      dose = RR[[index]] %>% pull(dose), 
      endpoint = c('copd', 'ihd', 'lc', 'lri', 'stroke'),
      age = c('ALL', seq(25, 95, 5) %>% matchable(0))
    ) %>% left_join(RR_tbl) %>% 
      group_by(dose, endpoint) %>% fill(RR) %>% ungroup() %>% 
      filter(age != 'ALL')
    
  } else if (CR == 'NCD+LRI') {
    expand_grid(
      dose = RR[[index]] %>% pull(dose),
      endpoint = c('ncd+lri'),
      age = c('ALL', seq(25, 95, 5) %>% matchable(0))
    ) %>% left_join(RR_tbl) %>% 
      group_by(dose, endpoint) %>% fill(RR) %>% ungroup() %>% 
      filter(age != 'ALL')
    
  } else if (str_detect(CR, 'IER')) {
    expand_grid(
      dose = RR[[index]] %>% pull(dose),
      endpoint = c('copd', 'ihd', 'lc', 'stroke', 'lri'),
      age = c('ALL', seq(0, 95, 5) %>% matchable(0))
    ) %>% left_join(RR_tbl) %>% 
      group_by(dose, endpoint) %>% fill(RR) %>% ungroup() %>% 
      filter((str_detect(endpoint, 'copd|ihd|lc|stroke') & as.integer(age) >= 25) | 
               (endpoint == 'lri' & as.integer(age) < 5)) 
    
  } else if (CR == 'MRBRT') {
    expand_grid(
      dose = RR[[index]] %>% pull(dose),
      endpoint = c('copd', 'dm', 'ihd', 'lc', 'lri', 'stroke'),
      age = c('ALL', seq(0, 95, 5) %>% matchable(0))
    ) %>% left_join(RR_tbl) %>% 
      group_by(dose, endpoint) %>% fill(RR) %>% ungroup() %>% 
      filter((str_detect(endpoint, 'copd|dm|ihd|lc|stroke') & as.integer(age) >= 25) | 
               (endpoint == 'lri' & as.integer(age) < 5))

  } else if (CR == "O3") {
    expand_grid(
      dose = RR[[index]] %>% pull(dose),
      endpoint = 'copd',
      age = c('ALL', seq(25, 95, 5) %>% matchable(0))
    ) %>% left_join(RR_tbl) %>% fill(RR) %>% filter(age != 'ALL')
    
  } else if (CR == "NO2") {
    expand_grid(
      dose = RR[[index]] %>% pull(dose),
      endpoint = 'cause', #ALL CAUSE
      age = c('ALL', seq(15, 95, 5) %>% matchable(0))
    ) %>% left_join(RR_tbl) %>% fill(RR) %>% filter(age != 'ALL')
  }
  
  return(RR_reshape)
}

#' filter data at specific scenario/year
#'
#' @param x the input data
#' @param where specify the scenario/year column
#' @param at the year/scenario to choose
#'
#' @return data.frame, contains joining keys and the corrosponding data
#'
get_at <- function(x, where = scenario, at) {
  x %>% filter({{where}} == at) %>% select(-{{where}})
}

#' Calculate girded PM2.5 attributed mortality
#'
#' @param field attribution, deciding the calculating field
#' @param dose_cf does data, a data.frame with column(s) of joining key(s) and a column of counter-fact dose data
#' @param dose_real does data, a data.frame with column(s) of joining key(s) and a column of dose data
#' @param pop population data, a data.frame with column(s) of joining key(s) and a column of population
#' @param age population age-structure, a data.frame with column(s) of joining key(s) and a column of population
#' @param mort mortality data, a data.frame with column(s) of joining key(s) and a column of population
#' @param lvl level of resolution, must corresponds to the `mort` data, if NULL, the calculations will be implement at no calibration on mort data.
#' @param RR param passed to `RR_std()`
#'
#' @return data.frame. death estimates for each endpoint & age-groups(columns) for every grids(rows)
#'
Mortality <- function(field, dose_real, dose_cf = NULL, pop, age, mort, lvl = NULL, RR = "MEAN") {
  
  dose_real <- dose_real %>%  mutate(across(c(is.numeric & dose), ~ matchable(.x, 1)))
  
  if (is.null(dose_cf)) dose_cf <- dose_real
    
  RR_tbl <- RR_std(RR)
  
  list(field, dose_real, RR_tbl) %>% reduce(left_join) %>% nrow() %>% 
    (\(x) if (x == 0) stop("parsing fail! check `field`, `dose_real` please!"))
  
  list(field, pop, age, mort) %>% reduce(left_join) %>% nrow() %>% 
    (\(x) if (x == 0) stop("parsing fail! check `pop`, `age` and `mort` please!"))
  
  if (is.null(lvl)) {
    list(field, dose_real, RR_tbl, age, mort, pop) %>% 
      reduce(left_join) %>% na.omit %>% 
      mutate(Mort = pop * prop * mortrate * (RR - 1) / RR / 1e5, .keep = 'unused') %>% 
      pivot_wider(
        names_from = c('endpoint', 'age'),
        names_sep = '_',
        values_from = 'Mort'
      )
  } else if (lvl %in%  names(mort)) {
    PWRR <- list(field, dose_real, pop, RR_tbl) %>% reduce(left_join) %>% na.omit %>% 
      group_by(pick(!!lvl)) %>% summarise(PWRR = weighted.mean(RR, pop, na.rm = T))
    
    list(field, dose_cf, pop, RR_tbl, mort, age, PWRR) %>% 
      reduce(left_join) %>% na.omit %>% 
      mutate(Mort = Pop * AgeStruc * MortRate * (RR - 1) / PWRR / 1e5, .keep = 'unused') %>% 
      pivot_wider(
        names_from = c('endpoint', 'age'),
        names_sep = '_',
        values_from = 'Mort'
      )
  } else {
    warning("`lvl` is not the key of any field, calculation will regard the field as one")
    
    PWRR <- list(field, dose_real, pop, RR_tbl) %>% reduce(left_join) %>% 
      na.omit %>% group_by(endpoint, age) %>% 
      summarise(PWRR = weighted.mean(RR, pop, na.rm = T)) %>% ungroup()
    
    list(field, pop, mort, age) %>% 
      reduce(left_join) %>% group_by(endpoint, age) %>% 
      summarise(mort = sum(pop * prop * mortrate / 1E5, na.rm = T)) %>% 
      ungroup() %>% left_join(PWRR) %>% na.omit() %>% 
      mutate(Mort = mort * (1 - 1 / PWRR), .keep = "unused") %>%
      pivot_wider(
        names_from = c('endpoint', 'age'),
        names_sep = '_',
        values_from = 'Mort'
      )
  }
}

#' Aggregated mortality calculation and attach uncertainty.
#'
#' @param field attribution, deciding the calculating field
#' @param dose_cf does data, a data.frame with column(s) of joining key(s) and a column of counter-fact dose data
#' @param dose_real does data, a data.frame with column(s) of joining key(s) and a column of dose data
#' @param pop population data, a data.frame with column(s) of joining key(s) and a column of population
#' @param age population age-structure, a data.frame with column(s) of joining key(s) and a column of population
#' @param mort mortality data, a data.frame with column(s) of joining key(s) and a column of population
#' @param lvl the aggregate level, corresponds to the field's keys, string.
#' @param uncertain incorporate uncertainty calculation in aggregation
#' @param doseRSME uncertainty of dose data
#' @param aggr_by additional keys to aggregate, age (group) or endpoint
#'
#' @return the aggregated burden
#' @export
#'
Mortality_Aggr <- function(field, dose_real, dose_cf = NULL, pop, age, mort, doseRSME = 0,
                           lvl = NULL, aggr_by = NULL, uncertain = F, write = T) {
  
  if (is.null(lvl)) {
    warning("`lvl` set to NULL, aggregation based on full fields will be implement.")
  } else if (!lvl %in% names(field)) {
    stop("supplied `lvl` not found in the field.")
  }
  
  if (is.null(aggr_by)) 
    warning("`aggr_by` set to NULL, output will aggregate both age and caouse.")
  else if (!aggr_by %in% names(mort)) {
    stop("supplied `aggr_by` not found in the mortality data.")
  }
  
  if (is.null(dose_cf)) dose_cf <- dose_real

  PWE <- list(field, dose_real, pop) %>% reduce(left_join) %>% 
    rename_with(~"domain", !!lvl) %>% group_by(domain) %>% 
    summarise(dose = weighted.mean(as.numeric(dose), pop, na.rm = T)) %>% 
    ungroup() %>% na.omit()
  
  Mort <- list(field, pop, age, mort) %>% reduce(left_join) %>% 
    rename_with(~"domain", !!lvl) %>% 
    mutate(M = pop * prop * mortrate / 1e5, endpoint = tolower(endpoint)) %>% 
    group_by(domain, age, endpoint) %>% summarise(M = sum(M, na.rm = T)) %>%
    ungroup() %>% na.omit()
  
  PAF <- PWE %>% mutate(dose = matchable(dose, 1)) %>% 
    left_join(RR_std('MEAN'), relationship = "many-to-many") %>% 
    mutate(PAF = 1 - 1 / RR) %>% select(-dose, -RR)
  
  AttrMort <- left_join(PAF, Mort) %>% mutate(AM = M * PAF, .keep = "unused") %>% 
    group_by(domain, pick(!!aggr_by)) %>% summarise(AM = sum(AM)) %>% ungroup()

  if (uncertain) {
    
    PAF_up <- PWE %>% mutate(dose = matchable(dose, 1)) %>% 
      left_join(RR_std('UP'), relationship = "many-to-many") %>% 
      select(-dose) %>% mutate(PAF_test = 1 - 1 / RR, .keep = "unused") %>% 
      mutate(varname = paste("test_CR", domain, endpoint, age, sep = "_")) %>% 
      pivot_wider(names_from = 'varname',values_from = 'PAF_test')
    
    PAF_low <- PWE %>% mutate(dose = matchable(dose, 1)) %>% 
      left_join(RR_std('LOW'), relationship = "many-to-many") %>% 
      select(-dose) %>% mutate(PAF_test = 1 - 1 / RR, .keep = "unused") %>% 
      mutate(varname = paste("test_CR", domain, endpoint, age, sep = "_")) %>% 
      pivot_wider(names_from = 'varname',values_from = 'PAF_test')
    
    if (doseRSME >= 0) {
      PAF_up <- PAF_up %>% left_join(
        PWE %>% mutate(dose = matchable(dose + doseRSME, 1)) %>% 
          left_join(RR_std('MEAN'), relationship = "many-to-many") %>% 
          select(-dose) %>% mutate(PAF_test = 1 - 1 / RR, .keep = 'unused') %>% 
          mutate(varname = paste("test_Pollu", domain, "Pollu_Pollu", sep = "_")) %>% 
          pivot_wider(names_from = 'varname', values_from = 'PAF_test') 
      )
      
      PAF_low <- PAF_low %>% left_join(
        PWE %>% mutate(dose = matchable(pmax(dose - doseRSME, 0), 1)) %>% 
          left_join(RR_std('MEAN'), relationship = "many-to-many") %>% 
          select(-dose) %>% mutate(PAF_test = 1 - 1 / RR, .keep = 'unused') %>% 
          mutate(varname = paste("test_Pollu", domain, "Pollu_Pollu", sep = "_")) %>% 
          pivot_wider(names_from = 'varname', values_from = 'PAF_test')
      )
    }
    
    Sensi_up <- PAF_up %>% left_join(PAF) %>% 
      mutate(across(matches("^test"), ~ replace(.x, is.na(.x), PAF[is.na(.x)]))) %>% 
      left_join(Mort) %>% mutate(across(matches("^test"), ~ M * abs(.x - PAF))) %>% 
      summarise(across(matches("^test"), ~ sum(.x, na.rm = T))) %>% 
      pivot_longer(
        everything(),
        names_to = c('item', 'domain', 'endpoint','age'),
        names_pattern = "test_(.+)_(.+)_(.+)_(.+)",
        values_to = 'Sensi'
      )
    
    Sensi_low <- PAF_up %>% left_join(PAF) %>% 
      mutate(across(matches("^test"), ~ replace(.x, is.na(.x), PAF[is.na(.x)]))) %>% 
      left_join(Mort) %>% mutate(across(matches("^test_"), ~ M * abs(.x - PAF))) %>% 
      summarise(across(matches("^test"), ~sum(.x, na.rm = T))) %>% 
      pivot_longer(
        everything(),
        names_to = c('item', 'domain', 'endpoint','age'),
        names_pattern = "test_(.+)_(.+)_(.+)_(.+)",
        values_to = 'Sensi'
      )
    
    
    test_sigma_up <- left_join(PAF, PAF_up) %>%
      mutate(across(matches("^test"), ~ abs(.x - PAF))) %>%
      summarise(across(matches("^test"), ~ sum(.x, na.rm = T))) %>%
      pivot_longer(
        everything(),
        names_to = c('item', 'domain', 'endpoint', 'age'),
        names_pattern = "test_(.+)_(.+)_(.+)_(.+)",
        values_to = 'sigma'
      )
    
    test_sigma_low <- left_join(PAF, PAF_low) %>%
      mutate(across(matches("^test"), ~ abs(.x - PAF))) %>%
      summarise(across(matches("^test"), ~ sum(.x, na.rm = T))) %>%
      pivot_longer(
        everything(),
        names_to = c('item', 'domain', 'endpoint', 'age'),
        names_pattern = "test_(.+)_(.+)_(.+)_(.+)",
        values_to = 'sigma'
      )
    
    CI <- left_join(
      left_join(Sensi_up, test_sigma_up) %>% group_by(domain, pick(!!aggr_by)) %>%
        summarise(CI_UP = sqrt(sum(Sensi ^ 2 * sigma ^ 2))) %>% ungroup(),
      left_join(Sensi_low, test_sigma_low) %>% group_by(domain, pick(!!aggr_by)) %>%
        summarise(CI_LOW = sqrt(sum(Sensi ^ 2 * sigma ^ 2))) %>% ungroup()
    )
    
    AttrMort <- left_join(AttrMort, CI)
    
  }

  return(AttrMort)
}
