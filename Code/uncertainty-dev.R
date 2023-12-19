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
          matches("^test_PM"), ~ replace(.x, is.na(.x), PAF_base[is.na(.x)])
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
      mutate(sigma = abs(PAF_test - PAF_base) / PAF_base, .keep = 'unused') %>% 
      add_column(item = 'CR'),
    PWPM %>% bind_cols(item = 'PM',endpoint = "PM",agegroup = '25') %>%
      mutate(sigma = PM25Scaler, .keep = 'unused')
  )
  
  test_sigma_low <- bind_rows(
    left_join(RR_base, RR_test_low) %>% 
      mutate(sigma = abs(PAF_test - PAF_base) / PAF_base,.keep = 'unused') %>% 
      add_column(item = 'CR'),
    PWPM %>% bind_cols(item = 'PM',endpoint = "PM",agegroup = '25') %>%
      mutate(sigma = PM25Scaler, .keep = 'unused')
  )
  
  left_join(
    left_join(Sensi_up, test_sigma_up) %>% group_by(domain) %>%
      summarise(CI_UP = sqrt(sum(Sensi ^ 2 * sigma ^ 2))),
    left_join(Sensi_low, test_sigma_low) %>% group_by(domain) %>%
      summarise(CI_LOW = sqrt(sum(Sensi ^ 2 * sigma ^ 2)))
  ) %>% return()
  
}