# PM25 Health Impact Calc Core
# By Yifan LIU 2021/05/15
# Modify by Yifan LIU 2022/04/09

library(tidyverse)

tell.CR <- function(CR) return(
  if (CR %>% str_detect('IER')) str_c(CR)
  else if (CR %in% c('5COD', 'NCD+LRI'))  str_c('GEMM', CR, sep = '_')
  else if (CR == 'MRBRT') str_c(CR)
)

matchable <- function(num, dgt = 1) num %>% round(dgt) %>% str_c

mortrate_std <- function(x) x %>% mutate(endpoint = tolower(endpoint))

RR_std <- function(RR_index, CR = CR_fun) {
  
  RR_tbl <- RR_index %>% pivot_longer(
    cols = -concentration,
    values_to = "RR",
    names_to = c("endpoint", "agegroup"),
    names_sep = '_'
  ) %>% mutate(endpoint = tolower(endpoint))
  
  RR_reshape <- if (CR == '5COD') {
    expand_grid(
      concentration = RR_index %>% pull(concentration),
      endpoint = c('copd', 'ihd', 'lc', 'lri', 'stroke'),
      agegroup = c('ALL', seq(25, 95, 5) %>% matchable(0))
    ) %>% left_join(RR_tbl) %>% 
      group_by(concentration, endpoint) %>% fill(RR) %>% ungroup %>% filter(agegroup != 'ALL')
    
  } else if (CR == 'NCD+LRI') {
    expand_grid(
      concentration = RR_index %>% pull(concentration),
      endpoint = c('ncd+lri'),
      agegroup = c('ALL', seq(25, 95, 5) %>% matchable(0))
    ) %>% left_join(RR_tbl) %>% 
      group_by(concentration, endpoint) %>% fill(RR) %>% ungroup %>% filter(agegroup != 'ALL')
    
  } else if (CR %>% str_detect('IER')) {
    expand_grid(
      concentration = RR_index %>% pull(concentration),
      endpoint = c('copd', 'ihd', 'lc', 'stroke', 'lri'),
      agegroup = c('ALL', seq(0, 95, 5) %>% matchable(0))
    ) %>% left_join(RR_tbl) %>% 
      group_by(concentration, endpoint) %>% fill(RR) %>% filter(agegroup != 'ALL') %>% 
      filter(
        (endpoint  %>% str_detect('copd|ihd|lc|stroke') & as.integer(agegroup) >= 25) | 
          endpoint == 'lri') %>% ungroup
  } else if (CR == 'MRBRT') {
    expand_grid(
      concentration = RR_index %>% pull(concentration),
      endpoint = c('copd', 'dm', 'ihd', 'lc', 'lri', 'stroke'),
      agegroup = c('ALL', seq(0, 95, 5) %>% matchable(0))
    ) %>% left_join(RR_tbl) %>% 
      group_by(concentration, endpoint) %>% fill(RR) %>% filter(agegroup != 'ALL') %>% 
      filter(
        (endpoint %>% str_detect('copd|dm|ihd|lc|stroke') & as.integer(agegroup) >= 25) | 
          endpoint == 'lri') %>% ungroup
  }
  
  return(RR_reshape)
}

#' Calculate gridded PM2.5 attributed mortality
#'
#' @param FID a vector of grid IDs
#' @param PM_r a 2-column `data.frame` stores real PM2.5 concentration of each grid
#' @param PM_c a 2-column `data.frame` stores virtual PM2.5 concentration of each grid, equals PM_r at default
#' @param ag proportions of 20 age-groups inside the population structure
#' @param mRate the mortality rates of each endpoints and each age group
#' @param pop a 2-column dataframe stores population volume of each grid
#' @param RR the lookup-table of Concentration-Response functions of PM2.5 exposure
#' @param CR a character string instructs the name of the C-R function
#'
#' @return a table of death estimates for each endpoint & age-groups(columns) for every grids(rows)
#'
#' @examples
Mortality <- function(FID, PM_r, PM_c = NULL, ag, mRate, pop, RR, CR = CR_fun) {
  
  if (is.null(PM_c)) PM_c <- PM_r
  
  RR_tbl <- RR_std(RR, CR)
  
  PWRR <- left_join(PM_r, pop) %>% summarise(
    concentration = weighted.mean(as.numeric(concentration), Pop, na.rm = T) %>% matchable
    ) %>% left_join(RR_tbl) %>% select(-concentration) %>% rename(PWRR_real = RR)
  
  list(tibble(FID = FID), PM_c, pop) %>% reduce(left_join) %>% na.omit %>%
    expand_grid(PWRR) %>% list(mRate %>% mortrate_std, ag) %>% reduce(left_join) %>% 
    left_join(RR_tbl) %>% select(-concentration) %>% rename(RR_cf = RR) %>% 
    mutate(Mort = Pop * AgeStruc * MortRate * (RR_cf - 1) / PWRR_real / 1e5,.keep = 'unused') %>% 
    pivot_wider(names_from = c('endpoint', 'agegroup'),names_sep = '_',values_from = 'Mort')
  
}

#' Uncertainties Calculation
#'
#' @param PM 人口加权浓度，以单列dataframe的形式呈现
#' @param Popu 人口，数字或者表格都可以
#' @param CR CR函数，目前支持 '5COD' 'NCD+LRI' 'IER' 'MRBRT' 四种输入
#' @param includePM 是否包含浓度不确定性，默认包含
#'
#' @return 返回对应模式不同终端的汇总不确定性范围
#'
#' @examples
Uncertainty <- function(PWPM, Agg.Pop, m_Rate, A_Group, 
                        CR = CR_fun, includePM = T, PM25Scaler = .12) {
  
  PWPM <- PWPM %>% na.omit %>% 
    rename_with(~'domain', where(is.character)) %>% 
    rename_with(~'concentration', where(is.numeric))
  
  Agg.Pop <- Agg.Pop %>% na.omit %>% 
    rename_with(~'domain', where(is.character))
  
  RR_base <- PWPM %>% mutate(concentration = matchable(concentration, 1)) %>%
    left_join(RR_std(RR_table[['MEAN']], CR)) %>% 
    mutate(PAF_base = 1 - 1 / RR) %>% select(-concentration, -RR)
  
  RR_test_up <- PWPM %>% mutate(concentration = matchable(concentration, 1)) %>%
    left_join(RR_std(RR_table[['UP']], CR)) %>%
    mutate(PAF_test = 1 - 1 / RR) %>% select(-concentration, -RR)
  
  RR_test_low <- PWPM %>% mutate(concentration = matchable(concentration, 1)) %>%
    left_join(RR_std(RR_table[['LOW']], CR)) %>% 
    mutate(PAF_test = 1 - 1 / RR) %>% select(-concentration,-RR)

  test_PAF_up <- left_join(RR_base, RR_test_up) %>% 
    mutate(varname = str_glue("test_CR_{domain}_{endpoint}_{agegroup}")) %>% 
    pivot_wider(names_from = 'varname',values_from = 'PAF_test') %>% 
    mutate(across(
      matches("^test_CR"), ~ replace(.x, is.na(.x), PAF_base[is.na(.x)])
    ))
  
  test_PAF_low <- left_join(RR_base, RR_test_low) %>% 
    mutate(varname = str_glue("test_CR_{domain}_{endpoint}_{agegroup}")) %>% 
    pivot_wider(names_from = 'varname',values_from = 'PAF_test') %>% 
    mutate(across(
      matches("^test_CR"), ~ replace(.x,is.na(.x),PAF_base[is.na(.x)])
    ))
  
  if (includePM) {
    test_PAF_up <- test_PAF_up %>% left_join(
      PWPM %>% mutate(concentration = matchable(concentration * (1 + PM25Scaler), 1)) %>%
        left_join(RR_std(RR_table[['MEAN']], CR)) %>% select(-concentration) %>% 
        mutate(PAF_test = 1 - 1 / RR, .keep = 'unused') %>% left_join(RR_base) %>% 
        mutate(varname = str_glue("test_PM_{domain}_PM_25")) %>%
        pivot_wider(names_from = 'varname',values_from = 'PAF_test') %>%
        mutate(across(
          matches("^test_PM"), ~ replace(.x,is.na(.x),PAF_base[is.na(.x)])
        )) %>% select(-PAF_base)
    )
    
    test_PAF_low <- left_join(
      test_PAF_low,
      PWPM %>% mutate(concentration = matchable(concentration * (1 - PM25Scaler), 1)) %>%
        left_join(RR_std(RR_table[['MEAN']], CR)) %>% select(-concentration) %>% 
        mutate(PAF_test = 1 - 1 / RR, .keep = 'unused') %>% left_join(RR_base) %>% 
        mutate(varname = str_glue("test_PM_{domain}_PM_25")) %>%
        pivot_wider(names_from = 'varname',values_from = 'PAF_test') %>%
        mutate(across(
          matches("^test_PM"), ~ replace(.x,is.na(.x),PAF_base[is.na(.x)])
        )) %>% select(-PAF_base)
    )
  }

  
  Sensi_up <- list(test_PAF_up, Agg.Pop, A_Group, m_Rate) %>% reduce(left_join) %>% 
    mutate(across(matches('^test'), ~ Pop * MortRate * AgeStruc * abs(.x - PAF_base) / 1e5)) %>% 
    select(matches('^test')) %>% map_df(sum, na.rm = T) %>% 
    pivot_longer(
      matches('^test'),
      names_to = c("item",'domain', 'endpoint','agegroup'),
      names_pattern = "test_(.+)_(.+)_(.+)_(.+)",
      values_to = 'Sensi'
    )
  
  Sensi_low <- list(test_PAF_low, Agg.Pop, A_Group, m_Rate) %>% reduce(left_join) %>%
    mutate(across(matches('^test'), ~ Pop * MortRate * AgeStruc * abs(.x - PAF_base) / 1e5)) %>% 
    select(matches('^test')) %>% map_df(sum,na.rm = T) %>%
    pivot_longer(
      matches('^test'),
      names_to = c("item",'domain', 'endpoint','agegroup'),
      names_pattern = "test_(.+)_(.+)_(.+)_(.+)",
      values_to = 'Sensi'
    )
  
  test_sigma_up <- bind_rows(
    left_join(RR_base, RR_test_up) %>% 
      mutate(sigma = abs(PAF_test - PAF_base),.keep = 'unused') %>% 
      add_column(item = 'CR'),
    PWPM %>% bind_cols(item = 'PM',endpoint = "PM",agegroup = '25') %>%
      mutate(sigma = concentration * PM25Scaler, .keep = 'unused')
  )
  
  test_sigma_low <- bind_rows(
    left_join(RR_base, RR_test_low) %>% 
      mutate(sigma = abs(PAF_test - PAF_base),.keep = 'unused') %>% 
      add_column(item = 'CR'),
    PWPM %>% bind_cols(item = 'PM',endpoint = "PM",agegroup = '25') %>%
      mutate(sigma = concentration * PM25Scaler, .keep = 'unused')
  )
  
  left_join(
    left_join(Sensi_up, test_sigma_up) %>% group_by(domain) %>%
        summarise(CI_UP = sqrt(sum(Sensi ^ 2 * sigma ^ 2))),
    left_join(Sensi_low, test_sigma_low) %>% group_by(domain) %>%
        summarise(CI_LOW = sqrt(sum(Sensi ^ 2 * sigma ^ 2)))
  ) %>% return()

}

Mort_Aggregate <- function(full_result, domain = 'Country', by = NULL, CR = CR_fun) {
  
  ls1 <- if (is.null(by)) {
    full_result %>% map(
      ~ left_join(.x, FID_info) %>%
        group_by(!!as.name(domain)) %>%
        summarise(across(matches('_[1-9]?(0|5)$'), sum)) %>% ungroup
    )
  } else {
    full_result %>% map(
      ~ .x %>% left_join(FID_info) %>% pivot_longer(
        cols = matches('_[1-9]?(0|5)$'),
        names_to = c('endpoint', 'agegroup'),
        names_sep = '_',
        values_to = 'Mort'
      ) %>% group_by(!!as.name(domain), !!as.name(by)) %>%
        summarise(Mort = sum(Mort)) %>% ungroup %>%
        pivot_wider(names_from = by, values_from = 'Mort')
    )
  }
  
  ls1 <- ls1 %>%  map(
    ~ .x %>% mutate(Total = select(., -!!domain) %>% rowSums, .after = !!domain)
  )
  
  CI <- if (domain == 'FID') {
    full_result %>% map(~FID_info)
  } else {
    names(full_result) %>% set_names %>% map(function(y) {
      Uncertainty(
        PM25Scaler = .12,
        m_Rate = mortrate_std(MortRate) %>% select(endpoint, agegroup, MortRate = !!as.name(y)),
        A_Group = AgeGroup %>% select(agegroup, AgeStruc = !!as.name(y)),
        Agg.Pop = FID_info %>% left_join(Pop %>% select(FID, Pop = !!as.name(y)), by = 'FID') %>%
          group_by(!!as.name(domain)) %>% summarise(Pop = sum(Pop, na.rm = T)) %>% na.omit,
        PWPM = list(
          FID_info,
          PM_real %>% select(FID, concentration = !!as.name(y)),
          Pop %>% select(FID, Pop = !!as.name(y))
        ) %>% reduce(left_join) %>% group_by(!!as.name(domain)) %>%
          summarise(PWPM = weighted.mean(as.numeric(concentration), Pop, na.rm = T)) %>% na.omit
      ) %>% rename_with( ~ domain, where(is.character))
    })
  }
  
  map2(CI, ls1, ~ left_join(.x, .y)) %>% return()

}
