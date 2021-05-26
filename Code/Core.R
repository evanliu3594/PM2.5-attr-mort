# PM25 Health Impact Calc Core v3
# By Yifan LIU 2021/05/15

tell.mode<-function(mode=mode) {
  return(if_else(mode %in% c('NCD+LRI','5COD'), paste0('GEMM-', mode), 'IER'))
}


RR_standardise <- function(x,mode){
  
  if (mode == '5COD') {
    df1 <- x %>% pivot_longer(
      -concentration,
      names_to = c('endpoint', 'agegroup'),
      names_sep = '[.]',
      values_to = 'RR'
    ) %>% filter(
      endpoint %in% c('COPD', 'IHD', 'LC', 'LRI', 'Stroke', 'STROKE')
    ) %>% mutate_at(vars('endpoint'), ~ str_replace(.x, 'STROKE', 'Stroke'))
      
    df1 %>% add_row(
      replicate(15, filter(df1, endpoint %in% c('COPD', 'LC', 'LRI')), simplify = F) %>%
        map_dfr(~ .x, .id = 'LOOP') %>%
        mutate(agegroup = as.character(20 + 5 * as.integer(LOOP))) %>% select(-LOOP)
    ) %>% add_row(
      replicate(3, filter(
        df1, endpoint %in% c('Stroke', 'IHD') & agegroup == '80'
      ), simplify = F) %>%
        map_dfr(~ .x, .id = 'LOOP') %>%
        mutate(agegroup = as.character(80 + 5 * as.integer(LOOP))) %>% select(-LOOP)
    ) %>% filter(agegroup != 'ALL') %>% return()
    
  } else if (mode == 'NCD+LRI') {
    df1 <- x %>% pivot_longer(
      -concentration,
      names_to = c('endpoint', 'agegroup'),
      names_sep = '\\.',
      values_to = 'RR'
    ) %>% filter(endpoint == 'NCD+LRI' & agegroup != 'ALL')
    
    df1 %>% add_row(
      replicate(3, filter(df1, agegroup == '80'), simplify = F) %>%
        map_dfr( ~ .x, .id = 'LOOP') %>%
        mutate(agegroup = as.character(80 + 5 * as.integer(LOOP))) %>% select(-LOOP)
    ) %>% return()
    
  } else if (mode == 'IER') {
    df1 <- x %>% pivot_longer(
      -concentration,
      names_to = c('endpoint', 'agegroup', 'CI95'),
      names_sep = '\\.',
      values_to = 'RR'
    ) %>% select(-CI95) %>% filter(
      endpoint %in% c('COPD', 'IHD', 'LC', 'LRI', 'Stroke', 'STROKE')
    ) %>% mutate_at(vars('endpoint'), ~str_replace(.x, 'STROKE', 'Stroke'))
    
    
    df1 %>% add_row(
      replicate(15, filter(df1, endpoint %in% c('COPD', 'LC')), simplify = F) %>%
        map_dfr( ~ .x, .id = 'LOOP') %>%
        mutate(agegroup = as.character(20 + 5 * as.integer(LOOP))) %>% select(-LOOP)
    ) %>% add_row(
      filter(df1, endpoint == 'LRI') %>% mutate(agegroup = '0')
    ) %>% filter(agegroup != 'ALL') %>% return()
  }
}

#' 不确定性计算模块
#'
#' @param PM 人口加权浓度，以单列dataframe的形式呈现
#' @param Popu 人口，数字或者表格都可以
#' @param mode 模式，目前有 5COD NCD+LRI IER 三种
#' @param bound 上下边界 UP 或 LOW
#' @param includePM 是否包含浓度不确定性，默认包含
#'
#' @return 返回对应模式不同终端的汇总不确定性范围
#' @export
#'
#' @examples
Uncertainty <- function(PWPM,Agg.P,y=y,mode=mode,bound='UP',includePM = T,PM25Scaler=.12) {
  
  RR_test <- PWPM %>% left_join(
      RR_standardise(RR_table[[bound]],mode = mode),
      by = 'concentration') %>% mutate(PAF_test = 1 - 1 / RR)
  
  RR_base <- PWPM %>% left_join(
    RR_standardise(RR_table[['MEAN']],mode = mode),
    by = 'concentration') %>% mutate(PAF_base = 1 - 1 / RR)
  
  test_PAF <- RR_base %>% add_column(
    RR_base$PAF_base %>%
      rep(nrow(RR_base)) %>%
      matrix(ncol = nrow(RR_base)) %>%
      `diag<-`(RR_test$PAF_test) %>%
      as_tibble
  ) %>% add_column(prefix = 'test', .before = T) %>% unite(
    'name',
    c(prefix, region, endpoint, agegroup),
    remove = F,
    sep = '.'
  )  %>% `names<-`(
    c(head(colnames(.),8),.$name)
  ) %>% select(-name,-prefix)
 
  if (includePM) {
    test_PAF <- test_PAF %>% add_column(
      PWPM %>% mutate_at(
        vars('concentration'),
        ~round(.x * if_else(bound=='UP',1+ PM25Scaler,1 - PM25Scaler), 1)
      ) %>% left_join(
        RR_standardise(RR_table[['MEAN']], mode = mode),
        by = 'concentration'
      ) %>% mutate(PAF_test = 1 - 1 / RR) %>% 
        group_by(region) %>% group_split %>% map(
        ~ .x %>% select(PAF_test) %>% `names<-`(paste0('test.', unique(.x$region), '.PM.25'))
      ) %>% bind_rows %>% map_df(~ if_else(is.na(.x), RR_base$PAF_base, .x))
    )
  } 
  
  test_PAF <- test_PAF %>% left_join(
    Agg.P, by = 'region'
  ) %>% left_join(
    select(agegroup, agegroup, agstruc = all_of(y)),
    by = 'agegroup'
  ) %>% left_join(
    incidence %>% filter(year == y) %>% select(-year),
    by = c('agegroup', 'endpoint')
  )
  
  Sensi <- test_PAF %>% mutate_at(
    vars(starts_with('test')),
    function(x) .$P * .$mort.rate * .$agstruc * (x - .$PAF_base) / 1e5
  ) %>% select(starts_with('test')) %>% map_df(~ sum(.x)) %>%
    pivot_longer(
      everything(),
      names_to = c('region', 'endpoint', 'agegroup'),
      names_sep = '[.]',
      names_prefix = 'test.',
      values_to = 'Sensi'
    )
  
  test_sigma <- bind_rows(
    left_join(
      RR_base, RR_test,by = c('region', 'endpoint', 'agegroup')
    ) %>% mutate(
      sigma = abs(1 - PAF_base / PAF_test)
    ) %>% select(region, endpoint, agegroup, sigma),
    PWPM %>% bind_cols(endpoint = 'PM', agegroup = '25') %>%
      mutate(concentration = concentration * PM25Scaler) %>%
      rename('sigma' = concentration)
  )
  
  bind_rows(
    left_join(
      Sensi, test_sigma, by = c('region', 'endpoint', 'agegroup')
      ) %>% mutate(ROOT2 = Sensi ^ 2 * sigma ^ 2) %>%
      group_by(region, endpoint) %>% summarise(CI = sqrt(sum(ROOT2))),
    data.frame(
      endpoint = 'ALL',
      left_join(
        Sensi, test_sigma, by = c('region','endpoint', 'agegroup')
        ) %>% group_by(region) %>%
        summarise(CI = sqrt(sum(Sensi ^ 2 * sigma ^ 2)))
    )
  ) %>% return()
}

Mortality <- function(PM_r, PM_c, ag, inci, pop, mode) {

  PWRR <- FID_info %>%
    left_join(pop, by = 'FID') %>%
    left_join(PM_r, by = 'FID') %>%
    left_join(RR_standardise(RR_table[['MEAN']], mode = mode),
              by = "concentration") %>% na.omit %>%
    rename(P = pop, PM_r = concentration, RR = RR) %>%
    group_by(endpoint, agegroup) %>%
    summarise(PWRR = weighted.mean(RR, P, na.rm = T))
  
  RR_c <- FID_info %>%
    select(-x,-y,-province) %>%
    left_join(pop, by = 'FID') %>%
    left_join(PM_c, by = 'FID') %>%
    left_join(RR_standardise(RR_table[['MEAN']], mode = mode),
              by = "concentration") %>% na.omit %>%
    rename(PM_c = concentration, RR_c = RR)
  
  RR_c %>% left_join(
    PWRR,
    by = c('endpoint', 'agegroup')
  ) %>% left_join(
    inci %>% select(-year),
    by = c('endpoint','agegroup')
  ) %>% left_join(
    ag,
    by = 'agegroup'
  ) %>% mutate(
    Mort = pop * agstruc * mort.rate * (RR_c - 1) / PWRR / 1e5
  ) %>% select(FID, endpoint, agegroup, Mort) %>% pivot_wider(
    names_from = c('endpoint', 'agegroup'),
    names_sep = '.',
    values_from = 'Mort'
  ) %>% return()
  
}

 

Grid_Aggr <- function(full_result, by, mode) {
  
  if (by == 'endpoint') {
    
    names(full_result) %>% map(function(y) {
      full_result[[y]] %>% pivot_longer(
        cols = -FID,
        names_to = c('endpoint', 'agegroup'),
        names_sep = '\\.',
        values_to = 'Mort'
      ) %>% group_by(FID, endpoint) %>%
        summarise(Mort = sum(Mort, na.rm = T)) %>%
        pivot_wider(names_from = 'endpoint',
                    values_from = 'Mort')
    }) %>%  `names<-`(names(full_result)) %>% return()
    
  } else if (by == 'agegroup') {
    names(full_result) %>% map(function(y) {
      full_result[[y]] %>% pivot_longer(
        cols = -FID,
        names_to = c('endpoint', 'agegroup'),
        names_sep = '\\.',
        values_to = 'Mort'
      ) %>% group_by(FID, agegroup) %>%
        summarise(Mort = sum(Mort, na.rm = T))  %>%
        pivot_wider(names_from = 'agegroup',
                    values_from = 'Mort')
    }) %>% `names<-`(names(full_result)) %>% return()
  }
}


Region_Aggr<-function(full_result, by,mode){
  
  if (by == 'endpoint') {
    
    names(full_result) %>% map_dfr(function(y) {
      
      df1 <- full_result[[y]] %>% left_join(
        select(FID_info, FID, region = province),
        by = 'FID'
      ) %>% pivot_longer(
        cols = c(-FID,-region),
        names_to = c('endpoint', 'agegroup'),
        names_sep = '\\.',
        values_to = 'Mort'
      ) %>% group_by(region, endpoint) %>%
        summarise(Mort_MEAN = sum(Mort))
      
      df2 <- bind_rows(
        df1,
        df1 %>% group_by(region) %>%
          summarise(Mort_MEAN = sum(Mort_MEAN)) %>%
          bind_cols(endpoint = 'ALL')
      ) 
      
      CI_LOW <- Uncertainty(
        bound = 'LOW',
        PM25Scaler = .1,
        y = y,
        Agg.P = FID_info %>% left_join(
          select(Pop, FID, pop = all_of(y)),
          by = 'FID') %>% na.omit %>% 
          group_by(province) %>% 
          summarise(P=sum(pop)) %>% 
          rename(region = province),
        PWPM = FID_info %>% left_join(
          select(PM_real, FID, pm = all_of(y)),
          by = 'FID') %>% left_join(
            select(Pop, FID, pop = all_of(y)),
            by = 'FID') %>% na.omit %>% group_by(province) %>%
          summarise(
            concentration = weighted.mean(pm, pop) %>% round(1)
          ) %>% rename(region = province),
        mode = mode,
        includePM = T
      ) %>% rename(CI_LOW=CI)
      
      CI_UP <- Uncertainty(
        bound = 'UP',
        PM25Scaler = .1,
        y = y,
        Agg.P = FID_info %>% left_join(
          select(Pop, FID, pop = all_of(y)),
          by = 'FID') %>% na.omit %>% 
          group_by(province) %>% 
          summarise(P=sum(pop)) %>% 
          rename(region = province),
        PWPM = FID_info %>% left_join(
          select(PM_real, FID, pm = all_of(y)),
          by = 'FID') %>% left_join(
            select(Pop, FID, pop = all_of(y)),
            by = 'FID') %>% na.omit %>% group_by(province) %>%
          summarise(
            concentration = weighted.mean(pm, pop) %>% round(1)
          ) %>% rename(region = province),
        mode = mode,
        includePM = T
      ) %>% rename(CI_UP=CI)
      
      left_join(df2,CI_LOW,by=c('region','endpoint')) %>% 
        left_join(CI_UP,by=c('region','endpoint')) %>%  mutate(
          year = y,
          CI_LOW = Mort_MEAN - CI_LOW,
          CI_UP = Mort_MEAN + CI_UP
        ) %>% pivot_longer(
          cols = -c(endpoint, region, year),
          names_to = c('Pre','CI95'),
          names_sep = '_',
          values_to = 'Mort'
        ) %>% select(-Pre) %>% 
        pivot_wider(
          names_from = 'CI95',
          values_from = 'Mort'
        ) %>% return()
        
    })
  } else if (by =='agegroup') {
    
    names(full_result) %>% map_dfr(function(y) {
      full_result[[y]] %>% left_join(
        select(FID_info, FID, province),
        by = 'FID'
      ) %>% pivot_longer(
        cols = c(-FID,-province),
        names_to = c('endpoint', 'agegroup'),
        names_sep = '\\.',
        values_to = 'Mort'
      ) %>% group_by(province, agegroup) %>%
        summarise(Mort_MEAN = sum(Mort)) %>%
        pivot_wider(
          names_from = 'agegroup',
          names_prefix = "SUM_",
          values_from = 'Mort_MEAN'
        ) %>% mutate(year = y)
    }) %>% return()
  }
   
}

Nation_by.edpt <- function(full_result,mode) {
  
  names(full_result) %>% map_dfr(function(y){

    df1 <- full_result[[y]] %>% pivot_longer(
      cols = -FID,
      names_to = c('endpoint', 'agegroup'),
      names_sep = '\\.',
      values_to = 'Mort'
    ) %>% group_by(endpoint) %>% 
      summarise(Mort_MEAN = sum(Mort, na.rm = T)) %>%
      bind_rows(data.frame(endpoint = 'ALL', Mort_MEAN = sum(.$Mort_MEAN))) %>% 
      mutate(region='nation')
      
    CI_LOW <- Uncertainty(
      y = y,
      Agg.P = data.frame(
        region = 'nation',
        P = select(Pop, pop = all_of(y)) %>% select(pop) %>% sum
      ),
      PWPM = data.frame(
        region = 'nation',
        FID_info %>% left_join(
          select(PM_real, FID, pm = all_of(y)),
          by = 'FID'
        ) %>% left_join(
          select(Pop, FID, pop = all_of(y)),
          by = 'FID'
        ) %>% na.omit %>% 
          summarise(concentration = weighted.mean(pm, pop) %>% round(1))
      ),
      mode = mode,
      includePM = T,
      bound = 'LOW',
      PM25Scaler = .1
    ) %>% rename(CI_LOW = CI) 
    
    CI_UP <- Uncertainty(
      y = y,
      Agg.P = data.frame(
        region = 'nation',
        P = select(Pop, pop = all_of(y)) %>% select(pop) %>% sum
      ),
      PWPM = data.frame(
        region = 'nation',
        FID_info %>% left_join(
          select(PM_real, FID, pm = all_of(y)),
          by = 'FID'
        ) %>% left_join(
          select(Pop, FID, pop = all_of(y)),
          by = 'FID'
        ) %>% na.omit %>% 
          summarise(concentration = weighted.mean(pm, pop) %>% round(1))
      ),
      mode = mode,
      includePM = T,
      bound = 'UP',
      PM25Scaler = .1
    ) %>% rename(CI_UP = CI)
    
    left_join(df1, CI_LOW, by = c('region', 'endpoint')) %>%
      left_join(CI_UP, by = c('region', 'endpoint')) %>%  mutate(
        year = y,
        CI_LOW = Mort_MEAN - CI_LOW,
        CI_UP = Mort_MEAN + CI_UP) %>% 
      pivot_longer(
        cols = -c(endpoint, region, year),
        names_to = c('Pre','CI95'),
        names_sep = '_',
        values_to = 'Mort'
      ) %>% select(-Pre) %>% 
      pivot_wider(
        names_from = 'CI95',
        values_from = 'Mort'
      ) 
  }) %>% return()
}

Nation_by.age<-function(full_result,mode){
  
  names(full_result) %>% map_dfr(function(y){
    full_result[[y]] %>% pivot_longer(
      cols = -FID,
      names_to = c('endpoint', 'agegroup'),
      names_sep = '\\.',
      values_to = 'Mort'
    ) %>% group_by(endpoint) %>% 
      group_by(agegroup) %>% summarise(
        Mort=sum(Mort)
      ) %>% bind_rows(
        data.frame(agegroup='ALL',Mort=sum(.$Mort))
      ) %>% pivot_wider(
        everything(),
        names_from = agegroup,
        names_prefix = "SUM_",
        values_from = Mort
      ) %>% mutate(year = y) 
  }) %>% return()
  
}
