# PM25 Health Impact Calc Core
# By Yifan LIU 2021/05/15
# Modify by Yifan LIU 2022/04/09

library(tidyverse)
library(writexl)
library(readxl)

tell_CR <- function(CR = .CR_fun) return(
  if (CR %>% str_detect('IER')) str_c(CR)
  else if (CR %in% c('5COD', 'NCD+LRI'))  str_c('GEMM', CR, sep = '_')
  else if (CR == 'MRBRT') str_c(CR)
)

use_CR <- function(CR_fun) {
  assign(".CR_fun", CR_fun, envir = globalenv())
  if (str_detect(CR_fun, "IER|NCD\\+LRI|5COD|MRBRT")) {
    cat(str_glue("C-R function \"{tell_CR(CR_fun)}\" is set as the default methodology"))
  } else {
    warning(str_glue("an exogenous C-R function \"{CR_fun}\" was specified, \\
                     please provide a corrosponding `RR_table` after `read_file()`"))
  }
}

matchable <- function(num, dgt = 1) num %>% round(dgt) %>% str_c

# dataload modual

read_files <- function(
    GRID = './Data/GRID_information_sample.xlsx',
    Pop = './Data/GridPop_sample.xlsx',
    PM_real = './Data/GridPM25_sample.xlsx',
    PM_cf = './Data/PM_Ctrl.csv', # PM_cf works only in counter-fact scenario
    MortRate = './Data/GBD_incidence_China_2000-2019.csv',
    AgeGroup = './Data/GBD_agestructure_China_2000-2017.csv') {
  
  fuse_read <- function(filename) filename %>% {
    if (str_detect(., 'csv$')) read_csv(.) 
    else if (str_detect(., 'xlsx$')) read_xlsx(.)
  }
  
  assign(
    'Grid_info', 
    envir = globalenv(), 
    fuse_read(GRID) %>% 
      mutate(across(where(is.numeric) & x:y, matchable, dgt = 2))
  )
  
  assign(
    "Pop", envir = globalenv(),
    fuse_read(Pop) %>% 
      mutate(across(where(is.numeric) & x:y, matchable, dgt = 2))
  )
  
  assign(
    'PM_real', envir = globalenv(), 
    fuse_read(PM_real) %>% mutate(
      across(where(is.numeric) & x:y, matchable, dgt = 2),
      across(where(is.numeric) & matches('^\\d{4}'), matchable, dgt = 1)
    )
  )
  
  # Specify UNREAL PM2.5 data, used for only counter-fact scenario.
  
  assign(
    'PM_cf', 
    envir = globalenv(),
    if (file.exists(PM_cf)) {
      fuse_read(PM_cf) %>% mutate(
        across(where(is.numeric) & x:y, matchable, dgt = 2),
        across(where(is.numeric) & matches('^\\d{4}'), matchable, dgt = 1)
      )
    } else NULL
  )
  
  assign(
    'MortRate', 
    envir = globalenv(),
    fuse_read(MortRate) %>% pivot_longer(
      cols = c(-year,-endpoint), names_to = 'agegroup',values_to = 'MortRate'
    ) %>% pivot_wider(
      names_from = 'year', values_from = 'MortRate'
    )
  )
  
  assign(
    "AgeGroup", 
    envir = globalenv(),
    fuse_read(AgeGroup) %>% pivot_longer(
      cols = -`year`, names_to = 'agegroup', values_to = 'AgeStruc'
    ) %>% pivot_wider(
      names_from = 'year', values_from = 'AgeStruc'
    ) %>% mutate(across(-agegroup, prop.table))
  )
  
  CR_file <- if (!exists('.CR_fun', envir = globalenv())) {
    warning("Please specify a C-R function with `use_CR()`")
  } else if (.CR_fun == 'MRBRT') {
    './Data/RR_index/MRBRT2019_Lookup_Table_LYF220601.xlsx'
  } else if (.CR_fun %in% c('NCD+LRI', '5COD')) {
    './Data/RR_index/GEMM_Lookup_Table_Build_220601.xlsx'
  } else if (.CR_fun %in% c('IER', 'IER2017')) {
    './Data/RR_index/IER2017_Lookup_Table_Build_220601.xlsx'
  } else if (.CR_fun == 'IER2015') {
    './Data/RR_index/IER2015_Lookup_Table_Build_220601.xlsx'
  } else if (.CR_fun == 'IER2013') {
    './Data/RR_index/IER2013_Lookup_Table_Build_220601.xlsx'
  } else if (.CR_fun == 'IER2010') {
    './Data/RR_index/IER2010_Lookup_Table_Build_220601.xlsx'
  } else {
    NA_character_
  }
  
  assign(
    "RR_table", envir = globalenv(),
    if (is.na(CR_file)) NA_character_
    else expand_grid(excel_sheets(CR_file), CR_file) %>% deframe %>% imap(
      ~ read_excel(.x, sheet = .y) %>% 
        mutate(across(where(is.numeric) & concentration, matchable, dgt = 1)))
  )
}

mortrate_std <- function(x, year) x %>% 
  mutate(endpoint = tolower(endpoint)) %>% 
  select(endpoint, agegroup, MortRate = !!year)

RR_std <- function(RR_index) {
  
  CR <-  if (!exists('.CR_fun', envir = globalenv())) {
    stop("Please specify a C-R function with `use_CR()`")
  } else get(".CR_fun", envir = globalenv())
  
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
#' @param Grids a vector of grid coords
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
Mortality <- function(Grids, PM_r, PM_c = NULL, ag, mRate, pop, RR) {
  
  if (is.null(PM_c)) PM_c <- PM_r
  
  RR_tbl <- RR_std(RR)
  
  PWRR <- list(Grids, PM_r, pop) %>% reduce(left_join) %>% summarise(
    concentration = weighted.mean(as.numeric(concentration), Pop, na.rm = T) %>% matchable
  ) %>% left_join(RR_tbl) %>% select(-concentration) %>% rename(PWRR_real = RR)
  
  list(Grids, PM_c, pop) %>% reduce(left_join) %>% na.omit %>%
    expand_grid(PWRR) %>% list(mRate, ag, RR_tbl) %>% 
    reduce(left_join) %>% select(-concentration) %>% rename(RR_cf = RR) %>% 
    mutate(Mort = Pop * AgeStruc * MortRate * (RR_cf - 1) / PWRR_real / 1e5,.keep = 'unused') %>% 
    pivot_wider(names_from = c('endpoint', 'agegroup'),names_sep = '_',values_from = 'Mort')
  
}

#' Uncertainties Calculation
#'
#' @param PWPM 人口加权浓度，双列data.frame的形式，包含归属地和对应加权浓度
#' @param Agg.Pop 人口，数字或者表格都可以
#' @param A_Group 人口年龄结构
#' @param m_Rate 死亡率数据
#' @param includePM 是否包含浓度不确定性，默认包含
#' @param PM25Scaler 若`includePM`设置为 `TRUE`, 则使用Scaler确定PM数据不确定性的波动率
#'
#' @return 返回对应模式不同终端的汇总不确定性范围
#'
#' @examples
Uncertainty <- function(PWPM, Agg.Pop, m_Rate, A_Group,
                        includePM = T, PM25Scaler = .12) {
  
  PWPM <- PWPM %>% na.omit %>% 
    rename_with(~'domain', where(is.character)) %>% 
    rename_with(~'concentration', where(is.numeric))
  
  Agg.Pop <- Agg.Pop %>% na.omit %>% 
    rename_with(~'domain', where(is.character))
  
  RR_base <- PWPM %>% mutate(concentration = matchable(concentration, 1)) %>%
    left_join(RR_std(RR_table[['MEAN']])) %>% 
    mutate(PAF_base = 1 - 1 / RR) %>% select(-concentration, -RR)
  
  RR_test_up <- PWPM %>% mutate(concentration = matchable(concentration, 1)) %>%
    left_join(RR_std(RR_table[['UP']])) %>%
    mutate(PAF_test = 1 - 1 / RR) %>% select(-concentration, -RR)
  
  RR_test_low <- PWPM %>% mutate(concentration = matchable(concentration, 1)) %>%
    left_join(RR_std(RR_table[['LOW']])) %>% 
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
        left_join(RR_std(RR_table[['MEAN']])) %>% select(-concentration) %>% 
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
        left_join(RR_std(RR_table[['MEAN']])) %>% select(-concentration) %>% 
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

Mort_Aggregate <- function(full_result, domain = 'Country', by = NULL, write = F) {
  
  pre_aggr_result <- if (domain == 'Grid' & is.null(by)) {
    full_result
  } else if (domain == 'Grid' & !is.null(by)) {
    full_result %>% map(
      ~ .x %>% pivot_longer(
        cols = matches('_[1-9]?(0|5)$'),
        values_to = 'Mort',
        names_to = c('endpoint', 'agegroup'),
        names_sep = '_'
      ) %>% group_by(x, y, !!as.name(by)) %>% summarise(Mort = sum(Mort)) %>%
        ungroup %>% pivot_wider(names_from = by, values_from = 'Mort')
    )
  } else if (domain != 'Grid' & is.null(by)) {
    full_result %>% map(
      ~ left_join(.x, Grid_info) %>%
        group_by(!!as.name(domain)) %>%
        summarise(across(matches('_[1-9]?(0|5)$'), sum)) %>% ungroup
    )
  } else if (domain != 'Grid' & !is.null(by)) {
    full_result %>% map(
      ~ .x %>% left_join(Grid_info) %>% pivot_longer(
        cols = matches('_[1-9]?(0|5)$'),
        names_to = c('endpoint', 'agegroup'),
        names_sep = '_',
        values_to = 'Mort'
      ) %>% group_by(!!as.name(domain),!!as.name(by)) %>%
        summarise(Mort = sum(Mort)) %>% ungroup %>%
        pivot_wider(names_from = by, values_from = 'Mort')
    )
  }
  
  pre_aggr_result <- pre_aggr_result %>% map(
    if (domain == 'Grid') {
      ~ .x %>% mutate(Total = select(., -c(x:y)) %>% rowSums, .after = x:y)
    } else {
      ~ .x %>% mutate(Total = select(., -!!domain) %>% rowSums, .after = !!domain)
    }
  )
  
  CI <- if (domain == 'Grid') {
    full_result %>% map(~ Grid_info %>% select(x:y))
  } else {
    names(full_result) %>% set_names %>% 
      map(function(year) Uncertainty(
        m_Rate = mortrate_std(MortRate) %>% select(endpoint, agegroup, MortRate = !!year),
        A_Group = AgeGroup %>% select(agegroup, AgeStruc = !!year),
        Agg.Pop = Grid_info %>% left_join(Pop %>% select(x:y, Pop = !!year)) %>%
          group_by(!!as.name(domain)) %>% summarise(Pop = sum(Pop, na.rm = T)) %>% na.omit,
        PWPM = list(
          Grid_info, 
          PM_real %>% select(x:y, concentration = !!year), 
          Pop %>% select(x:y, Pop = !!year)
        ) %>% reduce(left_join) %>% group_by(!!as.name(domain)) %>%
          summarise(PWPM = weighted.mean(as.numeric(concentration), Pop, na.rm = T)) %>% na.omit
        ) %>% rename_with(~ domain, where(is.character))
      )
  }
  
  aggr_result <- map2(CI, pre_aggr_result, ~ left_join(.x, .y)) %>% {
    if (domain == 'Country') imap_dfr(., ~ .x %>% add_column(year = .y, .before = T)) 
    else .
  }
  
  if (write) {
    aggr_result %>% write_xlsx(
      str_glue("./Result/{tell_CR()}_{domain}_\\
               {head(names(full_result),1)}-\\
               {tail(names(full_result),1)}_\\
               Build{format(Sys.Date(), '%y%m%d')}.xlsx"))
  }
  
  return(aggr_result)

}
