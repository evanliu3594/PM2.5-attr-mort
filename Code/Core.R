# PM25 Health Impact Calc Core
# By Yifan LIU 2021/05/15
# Modify by Yifan LIU 2022/04/09
# Modified B yifan liu on 23/12/19

library(tidyverse)
library(writexl)
library(readxl)

tell_Model <- function(Model = .CR_Model) return(
  if (Model %>% str_detect('IER')) str_c(Model)
  else if (Model %in% c('5COD', 'NCD+LRI'))  str_c('PM2.5_GEMM', Model, sep = '_')
  else if (Model == 'MRBRT') str_c("PM2.5_", Model)
  else if (Model == 'O3') str_c(Model) #新增
  else if (Model == 'NO2') str_c(Model) #新增
)

set_Model <- function(Model) {
  
  assign(".CR_Model", Model, envir = globalenv())
  
  if (str_detect(Model, "IER|NCD\\+LRI|5COD|MRBRT|O3|NO2")) {
    cat(str_glue(
      "C-R Model \"{tell_Model(Model)}\" is set as the default methodology"
    ))
  } else {
    warning(str_glue(
      "an exogenous C-R function \"{Model}\" was specified, \\
      please provide a corrosponding `RR_table` after `read_file()`"
    ))
  }
}

matchable <- function(num, dgt = 1) num %>% round(dgt) %>% str_c

# dataload modual

read_files <- function(Grids,
                       Pop,
                       Conc_real,
                       Conc_cf,
                       MortRate,
                       AgeGroup,
                       dgt_grid = 2,
                       dgt_conc = 1) {
  
  fuse_read <- function(filename) {
    if (str_detect(filename, 'csv$'))
      read_csv(filename)
    else if (str_detect(filename, 'xlsx$'))
      read_xlsx(filename)
  }
  
  assign(
    'Grid_info', 
    envir = globalenv(), 
    fuse_read(Grids) %>% 
      mutate(across(where(is.numeric) & x:y, ~ matchable(.x, dgt = dgt_grid)))
  )
  
  assign(
    "Pop", envir = globalenv(),
    fuse_read(Pop) %>% 
      mutate(across(where(is.numeric) & x:y, ~ matchable(.x, dgt = dgt_grid)))
  )
  
  assign(
    'Conc_real', envir = globalenv(), 
    fuse_read(Conc_real) %>% mutate(
      across(where(is.numeric) & x:y, ~ matchable(.x, dgt = dgt_grid)),
      across(where(is.numeric) & -x:-y, ~ matchable(.x, dgt = dgt_conc))
    )
  )
  
  # Specify UNREAL PM2.5 data, used for only counter-fact scenario.
  
  assign(
    'Conc_cf', 
    envir = globalenv(),
    if (file.exists(Conc_cf)) {
      fuse_read(Conc_cf) %>% mutate(
        across(where(is.numeric) & x:y, ~ matchable(.x, dgt = dgt_grid)),
        across(where(is.numeric) & -x:-y, ~ matchable(.x, dgt = dgt_conc))
      )
    } else NULL
  )
  
  assign(
    'MortRate', 
    envir = globalenv(),
    fuse_read(MortRate) %>% 
      mutate(across(where(is.numeric) & agegroup, ~ matchable(.x, dgt = 0)))
  )
  
  assign(
    "AgeGroup", 
    envir = globalenv(),
    fuse_read(AgeGroup) %>% 
      mutate(across(where(is.numeric) & agegroup,  ~ matchable(.x, dgt = 0)))
  )
  
  CR_file <- if (!exists('.CR_Model', envir = globalenv())) {
    warning("Please specify a C-R function with `use_CR()`")
  } else if (.CR_Model == 'MRBRT') {
    './Data/RR_index/MRBRT2019_Lookup_Table_LYF220601.xlsx'
  } else if (.CR_Model %in% c('NCD+LRI', '5COD')) {
    './Data/RR_index/GEMM_Lookup_Table_Build_220601.xlsx'
  } else if (.CR_Model %in% c('IER', 'IER2017')) {
    './Data/RR_index/IER2017_Lookup_Table_Build_220601.xlsx'
  } else if (.CR_Model == 'IER2015') {
    './Data/RR_index/IER2015_Lookup_Table_Build_220601.xlsx'
  } else if (.CR_Model == 'IER2013') {
    './Data/RR_index/IER2013_Lookup_Table_Build_220601.xlsx'
  } else if (.CR_Model == 'IER2010') {
    './Data/RR_index/IER2010_Lookup_Table_Build_220601.xlsx'
  } else if (.CR_Model == "O3") {
    "./Data/RR_index/O3_CR_Lookup_Table.xlsx"
  } else if (.CR_Model == "NO2") {
    "./Data/RR_index/NO2_CR_Lookup_Table.xlsx"
  } else {
    NA_character_
  }
  
  assign(
    "RR_table", envir = globalenv(),
    if (is.na(CR_file)) NA_character_
    else expand_grid(excel_sheets(CR_file), CR_file) %>% deframe %>% 
      imap( ~ read_excel(.x, sheet = .y) %>% 
              mutate(across(where(is.numeric) & concentration, 
                            ~ matchable(.x, dgt = dgt_conc))))
  )
}

mortrate_std <- function(x, year) x %>% 
  mutate(endpoint = tolower(endpoint)) %>% 
  select(domain, endpoint, agegroup, MortRate = !!year)

RR_std <- function(RR_index) {
  
  CR <-  if (!exists('.CR_Model', envir = globalenv())) {
    stop("Please specify a C-R function with `use_CR()`")
  } else get(".CR_Model", envir = globalenv())
  
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
    
  } else if (str_detect(CR, 'IER')) {
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
  } else if (CR == "O3") {
    expand_grid(
      concentration = RR_index %>% pull(concentration),
      endpoint = 'copd',
      agegroup = c('ALL', seq(25, 95, 5) %>% matchable(0))
    ) %>% left_join(RR_tbl) %>% fill(RR) %>% filter(agegroup != 'ALL')
  } else if (CR == "NO2") {
    expand_grid(
      concentration = RR_index %>% pull(concentration),
      endpoint = 'cause', #ALL CAUSE
      agegroup = c('ALL', seq(15, 95, 5) %>% matchable(0))
    ) %>% left_join(RR_tbl) %>% fill(RR) %>% filter(agegroup != 'ALL')
  }
  
  return(RR_reshape)
}

#' Calculate gridded PM2.5 attributed mortality
#'
#' @param Grids a vector of grid coords
#' @param Conc_r a 2-column `data.frame` stores real PM2.5 concentration of each grid
#' @param Conc_c a 2-column `data.frame` stores virtual PM2.5 concentration of each grid, equals Conc_r at default
#' @param ag proportions of 20 age-groups inside the population structure
#' @param mRate the mortality rates of each endpoints and each age group
#' @param pop a 2-column dataframe stores population volume of each grid
#' @param RR the lookup-table of Concentration-Response functions of PM2.5 exposure
#' @param CR a character string instructs the name of the C-R function
#'
#' @return a table of death estimates for each endpoint & age-groups(columns) for every grids(rows)
#'
#' @examples
Mortality <- function(Grids, Conc_r, Conc_c = NULL, ag, mRate, pop, RR, domain = NULL) {
  
  if (is.null(Conc_c)) Conc_c <- Conc_r
  
  if (is.null(domain)) stop("no grouping domain specified, can't calculate PWRR.")
  
  RR_tbl <- RR_std(RR)
  
  PWRR <- list(Grids, Conc_r, pop, RR_tbl) %>% reduce(left_join) %>% 
    na.omit %>% group_by(!!as.name(domain)) %>% 
    summarise(PWRR = weighted.mean(RR, Pop, na.rm = T)) 
  
  list(
    Grids, Conc_c, pop, RR_tbl,
    mRate %>% rename(Country = domain),
    ag %>% rename(Country = domain),
    PWRR
  ) %>% 
    reduce(left_join) %>% na.omit %>% 
    mutate(Mort = Pop * AgeStruc * MortRate * (RR - 1) / PWRR / 1e5, .keep = 'unused') %>% 
    pivot_wider(
      names_from = c('endpoint', 'agegroup'),
      names_sep = '_',
      values_from = 'Mort'
    )
  
}

Mortality_debug <- function(Grids, Conc_r, Conc_c = NULL, ag, mRate, pop, RR ) {
  
  if (is.null(Conc_c)) Conc_c <- Conc_r
  
  RR_tbl <- RR_std(RR)
  
  Grids %>% 
    left_join(Conc_c) %>% 
    left_join(pop) %>% 
    left_join(RR_tbl) %>% 
    left_join(mRate %>% rename(Country = domain)) %>% 
    left_join(ag %>% rename(Country = domain))
  
}

#' Uncertainties Calculation
#'
#' @param m_Rate 死亡率数据
#' @param PWE 分地区人口加权浓度
#' @param aggr_pop 分区汇总人口
#' @param age_struc 分区年龄结构
#' @param includeConc 浓度不确定性开关
#' @param Conc_RMSE 浓度数据RMSE
#'
#' @return 返回对应模式不同终端的汇总不确定性范围
#'
#' @examples
Uncertainty <- function(PWE, aggr_pop, age_struc, m_Rate, 
                        includeConc = F, Conc_RMSE = 26.3) {
  
  PWE <- PWE %>% na.omit %>% 
    rename_with(~'domain', where(is.character)) %>% 
    rename_with(~'concentration', where(is.numeric))
  
  aggr_pop <- aggr_pop %>% na.omit %>% 
    rename_with(~'domain', where(is.character))
  
  RR_base <- PWE %>% mutate(concentration = matchable(concentration, 1)) %>%
    left_join(RR_std(RR_table[['MEAN']])) %>% 
    mutate(PAF_base = 1 - 1 / RR) %>% select(-concentration, -RR)
  
  RR_test_up <- PWE %>% mutate(concentration = matchable(concentration, 1)) %>%
    left_join(RR_std(RR_table[['UP']])) %>%
    mutate(PAF_test = 1 - 1 / RR) %>% select(-concentration, -RR)
  
  RR_test_low <- PWE %>% mutate(concentration = matchable(concentration, 1)) %>%
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
  
  if (includeConc) {
    test_PAF_up <- test_PAF_up %>% left_join(
      PWE %>% mutate(concentration = matchable(concentration + Conc_RMSE, 1)) %>%
        left_join(RR_std(RR_table[['MEAN']])) %>% select(-concentration) %>% 
        mutate(PAF_test = 1 - 1 / RR, .keep = 'unused') %>% left_join(RR_base) %>% 
        mutate(varname = str_glue("test_Pollu_{domain}_Pollu_Pollu")) %>%
        pivot_wider(names_from = 'varname',values_from = 'PAF_test') %>%
        mutate(across(
          matches("^test_Pollu"), ~ replace(.x, is.na(.x), PAF_base[is.na(.x)])
        )) %>% select(-PAF_base)
    )
    
    test_PAF_low <- left_join(
      test_PAF_low,
      PWE %>% mutate(concentration = matchable(concentration - Conc_RMSE, 1)) %>%
        left_join(RR_std(RR_table[['MEAN']])) %>% select(-concentration) %>% 
        mutate(PAF_test = 1 - 1 / RR, .keep = 'unused') %>% left_join(RR_base) %>% 
        mutate(varname = str_glue("test_Pollu_{domain}_Pollu_Pollu")) %>%
        pivot_wider(names_from = 'varname',values_from = 'PAF_test') %>%
        mutate(across(
          matches("^test_Pollu"), ~ replace(.x,is.na(.x),PAF_base[is.na(.x)])
        )) %>% select(-PAF_base)
    )
  }
  
  
  Sensi_up <- list(test_PAF_up, aggr_pop, age_struc, m_Rate) %>% reduce(left_join) %>% 
    mutate(across(matches('^test'), ~ Pop * MortRate * AgeStruc * abs(.x - PAF_base) / 1e5)) %>% 
    select(matches('^test')) %>% map_df(sum, na.rm = T) %>% 
    pivot_longer(
      matches('^test'),
      names_to = c("item",'domain', 'endpoint','agegroup'),
      names_pattern = "test_(.+)_(.+)_(.+)_(.+)",
      values_to = 'Sensi'
    )
  
  Sensi_low <- list(test_PAF_low, aggr_pop, age_struc, m_Rate) %>% reduce(left_join) %>%
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
      mutate(sigma = abs(PAF_test - PAF_base), .keep = 'unused') %>% 
      add_column(item = 'CR'),
    PWE %>% bind_cols(item = 'Pollu', endpoint = "Pollu", agegroup = 'Pollu') %>%
      mutate(sigma = Conc_RMSE, .keep = 'unused')
  )
  
  test_sigma_low <- bind_rows(
    left_join(RR_base, RR_test_low) %>% 
      mutate(sigma = abs(PAF_test - PAF_base),.keep = 'unused') %>% 
      add_column(item = 'CR'),
    PWE %>% bind_cols(item = 'Pollu', endpoint = "Pollu", agegroup = 'Pollu') %>%
      mutate(sigma = Conc_RMSE, .keep = 'unused')
  )
  
  left_join(
    left_join(Sensi_up, test_sigma_up) %>% group_by(domain) %>%
      summarise(CI_UP = sqrt(sum(Sensi ^ 2 * sigma ^ 2))),
    left_join(Sensi_low, test_sigma_low) %>% group_by(domain) %>%
      summarise(CI_LOW = sqrt(sum(Sensi ^ 2 * sigma ^ 2)))
  ) %>% return()
  
}

Mort_Aggregate <- function(full_result,
                           domain = 'Grid',
                           by = NULL,
                           write = T,
                           ...
) {
  
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
      ~ .x %>% mutate(Total = rowSums(select(., matches('_[1-9]?(0|5)$'))), .after = x:y)
    } else {
      ~ .x %>% mutate(Total = rowSums(select(., -all_of(!!domain))), .after = !!domain)
    }
  )
  
  CI <- if (domain == 'Grid') {
    full_result %>% map(~ Grid_info %>% select(x:y, any_of(c("Country", "Region", "Province"))))
  } else {
    names(full_result) %>% set_names %>% 
      map(function(year) Uncertainty(
        ...,
        m_Rate = mortrate_std(MortRate, year),
        aggr_pop = Grid_info %>% left_join(Pop %>% select(x:y, Pop = !!year)) %>%
          group_by(!!as.name(domain)) %>% summarise(Pop = sum(Pop, na.rm = T)) %>% na.omit,
        age_struc = AgeGroup %>% select(domain, agegroup, AgeStruc = !!year),
        PWE = list(
          Grid_info, 
          Conc_real %>% select(x:y, concentration = !!year), 
          Pop %>% select(x:y, Pop = !!year)
        ) %>% reduce(left_join) %>% na.omit %>% group_by(!!as.name(domain)) %>%
          summarise(PWE = weighted.mean(as.numeric(concentration), Pop, na.rm = T)) 
      ) %>% rename_with(~ domain, where(is.character))
      )
  }
  
  aggr_result <- map2(CI, pre_aggr_result, ~ left_join(.x, .y)) %>% {
    if (domain %in% c("Country","Province","Region")) 
      imap_dfr(. , ~ .x %>% add_column(year = .y, .before = T)) 
    else .
  }
  
  if (write) {
    by <- if (is.null(by)) "Everything" else str_replace(by,"^.{1}", toupper)
    aggr_result %>% write_xlsx(str_glue(
      "./Result/{tell_Model()}_{domain}_by{by}_\\
       {head(names(full_result),1)}-\\
       {tail(names(full_result),1)}_\\
       Build{format(Sys.Date(), '%y%m%d')}.xlsx"
    ))
  }
  
  return(aggr_result)
  
}
