#Patched with PM2.5 Attr Mort v7

PAF_standardise <- function(AF, mode = mode) {
  if (mode == '5COD') {
    return(
      bind_cols(
        coordinate = AF[,1],
        bind_cols(replicate(15, select(AF, contains('COPD')), simplify = F)),
        select(AF, contains('IHD'),-contains('ALL')),
        bind_cols(replicate(3, select(AF, IHD.80), simplify = F)),
        bind_cols(replicate(15, select(AF, contains('LC')), simplify = F)),
        bind_cols(replicate(15, select(AF, contains('LRI') & -contains('NCD')), simplify = F)),
        select(AF, contains('Stroke'),-contains('ALL')),
        bind_cols(replicate(3, select(AF, Stroke.80), simplify = F))
      ) %>% `colnames<-`(c(
        names(AF)[1],
        paste('COPD', seq(25, 95, 5), sep = '.'),
        paste('IHD', seq(25, 95, 5), sep = '.'),
        paste('LC', seq(25, 95, 5), sep = '.'),
        paste('LRI', seq(25, 95, 5), sep = '.'),
        paste('Stroke', seq(25, 95, 5), sep = '.')
      ))
    )
  } else if (mode == 'IER') {
    return(
      bind_cols(
        coordinate = AF[, 1],
        bind_cols(replicate(20, select(AF, contains('COPD')), simplify = F)),
        select(AF, contains('IHD') & -contains('ALL')),
        bind_cols(replicate(20, select(AF, contains('LC')), simplify = F)),
        bind_cols(replicate(20, select(AF, contains('LRI')), simplify = F)),
        select(AF, contains('Stroke') & -contains('ALL'))
      ) %>% `colnames<-`(c(
        names(AF)[1],
        paste('COPD', seq(0, 95, 5), sep = '.'),
        paste('IHD', seq(0, 95, 5), sep = '.'),
        paste('LC', seq(0, 95, 5), sep = '.'),
        paste('LRI', seq(0, 95, 5), sep = '.'),
        paste('Stroke', seq(0, 95, 5), sep = '.')
      ))
    )
  } else if (mode == 'NCD+LRI') {
    return(bind_cols(
      coordinate = AF[, 1],
      select(AF, contains('NCD+LRI'),-contains('ALL')),
      bind_cols(replicate(3, select(AF, contains('NCD+LRI.80')), simplify = F))
    ) %>% `colnames<-`(c(
      names(AF)[1],
      paste('NCD+LRI', seq(25, 95, 5), sep = '.')
    )))
  }
}

Inci_standardise<-function(mode,y){
  if (mode == '5COD') {
    incidence <- bind_cols(
      filter(inci, year == y, endpoint == 'COPD')[, -1:-7],
      filter(inci, year == y, endpoint == 'IHD')[, -1:-7],
      filter(inci, year == y, endpoint == 'LC')[, -1:-7],
      filter(inci, year == y, endpoint == 'LRI')[, -1:-7],
      filter(inci, year == y, endpoint == 'Stroke')[, -1:-7]
    ) %>% `colnames<-`(c(
      paste('COPD', seq(25, 95, 5), sep = '.'),
      paste('IHD', seq(25, 95, 5), sep = '.'),
      paste('LC', seq(25, 95, 5), sep = '.'),
      paste('LRI', seq(25, 95, 5), sep = '.'),
      paste('Stroke', seq(25, 95, 5), sep = '.')
    ))
  } else if (mode == 'NCD+LRI') {
    incidence <-
      filter(inci, year == y, endpoint == 'NCD+LRI')[, -1:-7] %>%
      `colnames<-`(paste('NCD+LRI', seq(25, 95, 5), sep = '.'))
  } else if (mode == 'IER') {
    incidence <- bind_cols(
      filter(inci, year == y, endpoint == 'COPD')[, -1:-2],
      filter(inci, year == y, endpoint == 'IHD')[, -1:-2],
      filter(inci, year == y, endpoint == 'LC')[, -1:-2],
      filter(inci, year == y, endpoint == 'LRI')[, -1:-2],
      filter(inci, year == y, endpoint == 'Stroke')[, -1:-2]
    ) %>% `colnames<-`(c(
      paste('COPD', seq(0, 95, 5), sep = '.'),
      paste('IHD', seq(0, 95, 5), sep = '.'),
      paste('LC', seq(0, 95, 5), sep = '.'),
      paste('LRI', seq(0, 95, 5), sep = '.'),
      paste('Stroke', seq(0, 95, 5), sep = '.')
    ))
  }
  return(incidence)
  rm(incidence)
}

Result_standardise<-function(result,mode){
  df1 <- result
  if (mode == 'IER') {
    df2 <- data.frame(
      df1[1], 
      COPD_SUM = rowSums(select(df1, contains('COPD'))[, -1:-5]),
      IHD_SUM = rowSums(select(df1, contains('IHD'))[, -1:-5]),
      LC_SUM = rowSums(select(df1, contains('LC'))[, -1:-5]),
      ALRI_SUM = rowSums(select(df1, contains('LRI.0'))),
      Stroke_SUM = rowSums(select(df1, contains('Stroke'))[, -1:-5])
    )
    df2$SUM_mort <- rowSums(df2[,-1])
  } else if (mode == 'NCD+LRI') {
    df2 <- data.frame(
      df1[1], 
      SUM_Mort = rowSums(select(df1, contains('NCD+LRI')))
    )
  } else if (mode == '5COD') {
    df2 <- data.frame(
      df1[1],
      COPD_SUM = rowSums(select(df1, contains('COPD'))),
      IHD_SUM = rowSums(select(df1, contains('IHD'))),
      LC_SUM = rowSums(select(df1, contains('LC'))),
      LRI_SUM = rowSums(select(df1, contains('LRI'))),
      Stroke_SUM = rowSums(select(df1, contains('Stroke')))
    )
    df2$SUM_mort <- rowSums(df2[,-1])
  }
  return(df2)
  rm(df1,df2)
}

AgeMatrix<-function(Popu,agestruc,mode){
  if (mode == '5COD') {
    return(bind_cols(replicate(
      5, as.data.frame(as.matrix(Popu) %*% as.matrix(agestruc[, -1:-5]))
    )))
  } else if (mode == 'NCD+LRI') {
    return(as.data.frame(as.matrix(Popu) %*% as.matrix(agestruc[, -1:-5])))
  } else if (mode == 'IER'){
    return(bind_cols(replicate(
      5, as.data.frame(as.matrix(Popu) %*% as.matrix(agestruc[, ]))
    )))
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
uncertainty <- function(PM,Popu,mode,bound,includePM = T,y) {
  RR_test <- left_join(PM, RR_table[[bound]], by = 'concentration') %>%
    PAF_standardise(mode = mode)
  
  RR_base <- left_join(PM, RR_table[['MEAN']], by = 'concentration') %>% 
    PAF_standardise(mode = mode)
  
  test_RR <- list(Ctrl = bind_cols(RR_base[, 1], 1 - 1 / RR_base[, -1]))
  
  for (case in names(RR_test[, -1])) {
    df <- RR_base
    df[case] = RR_test[case]
    test_RR[[case]] <- bind_cols(df[, 1], 1 - 1 / df[, -1])
  }
  
  if (includePM) {
    if (bound == 'UP') {
      test_RR[['PM']] <- left_join(
        round(PM * 1.1, 1),
        bind_cols(RR_table[['MEAN']][, 1], 1 - 1 / RR_table[['MEAN']][, -1]),
        by = 'concentration') %>% PAF_standardise(mode = mode)
      test_RR
    } else if (bound == 'LOW') {
      test_RR[['PM']] <- left_join(
        round(PM * 0.9, 1),
        bind_cols(RR_table[['MEAN']][, 1], 1 - 1 / RR_table[['MEAN']][, -1]),
        by = 'concentration') %>% PAF_standardise(mode = mode)
    }
  }

  BaseMort<-as.matrix(AgeMatrix(Popu = Popu,agestruc = agegroup[y,],mode = mode)
  ) %*% diag(Inci_standardise(mode = mode,y = y)) /1E5
  
  Sensi <- matrix(nrow = nrow(PM),
                  ncol = length(test_RR) - 1,
                  dimnames = list(rownames(PM), names(test_RR[-1])))
  
  for (case in names(test_RR[-1])) {
    if (bound == 'UP') {
      Sensi[, case] <-
        rowSums(BaseMort * (test_RR[[case]][, -1] - test_RR[['Ctrl']][, -1]))
      Sensi <- as.data.frame(Sensi)
      test_sigma <- left_join(PM, bind_cols(
        RR_table[['MEAN']][, 1],((RR_table[['UP']][,-1]) / (RR_table[['MEAN']][,-1]))),
        by = 'concentration') %>% PAF_standardise(mode = mode)
    } else if (bound == 'LOW') {
      Sensi[, case] <-
        rowSums(BaseMort * (test_RR[['Ctrl']][, -1] - test_RR[[case]][, -1]))
      Sensi <- as.data.frame(Sensi)
      test_sigma <- left_join(PM, bind_cols(
        RR_table[['MEAN']][, 1], ((RR_table[['MEAN']][,-1]) / (RR_table[['LOW']][,-1]))),
        by = 'concentration') %>% PAF_standardise(mode = mode)
    }
  }

  if (includePM) {
    test_sigma$PM <- PM * 0.1
  }
  
  if (mode == '5COD') {
    return(data.frame(
      COPD_SUM = sqrt(rowSums(select(Sensi, contains('COPD')) ^ 2 *
                                select(test_sigma, contains('COPD')) ^ 2)),
      IHD_SUM = sqrt(rowSums(select(Sensi, contains('IHD')) ^ 2 *
                               select(test_sigma, contains('IHD')) ^ 2)),
      LC_SUM = sqrt(rowSums(select(Sensi, contains('LC')) ^ 2 *
                              select(test_sigma, contains('LC')) ^ 2)),
      LRI_SUM = sqrt(rowSums(select(Sensi, contains('LRI')) ^ 2 *
                               select(test_sigma, contains('LRI')) ^ 2)),
      Stroke_SUM = sqrt(rowSums(select(Sensi, contains('Stroke')) ^ 2 *
                                  select(test_sigma, contains('Stroke')) ^ 2)),
      SUM_Mort = sqrt(rowSums(Sensi ^ 2 * test_sigma[,-1] ^ 2))
    ))
  } else if (mode == 'NCD+LRI') {
    return(data.frame(
      `SUM_Mort` = sqrt(rowSums(Sensi ^ 2 * test_sigma[, -1] ^ 2))
    ))
  } else if (mode=='IER'){
    Sensi <- data.frame(
      select(Sensi, contains('COPD'))[, -1:-5],
      select(Sensi, contains('IHD'))[, -1:-5],
      select(Sensi, contains('LC'))[, -1:-5],
      select(Sensi, contains('LRI'))[1],
      select(Sensi, contains('Stroke'))[, -1:-5],
      select(Sensi,PM)
    )
    
    test_sigma <- data.frame(
      select(test_sigma, contains('COPD'))[, -1:-5],
      select(test_sigma, contains('IHD'))[, -1:-5],
      select(test_sigma, contains('LC'))[, -1:-5],
      select(test_sigma, contains('LRI'))[1],
      select(test_sigma, contains('Stroke'))[, -1:-5],
      select(test_sigma, PM)
    )
    
    return(data.frame(
      COPD_SUM = sqrt(rowSums(select(Sensi, contains('COPD')) ^ 2 * 
                                select(test_sigma, contains('COPD')) ^ 2)),
      IHD_SUM = sqrt(rowSums(select(Sensi, contains('IHD')) ^ 2 * 
                               select(test_sigma, contains('IHD')) ^ 2)),
      LC_SUM = sqrt(rowSums(select(Sensi, contains('LC')) ^ 2 * 
                              select(test_sigma, contains('LC')) ^ 2)),
      ALRI_SUM = sqrt(rowSums(select(Sensi, contains('LRI')) ^ 2 * 
                                select(test_sigma, contains('LRI')) ^ 2)),
      Stroke_SUM = sqrt(rowSums(select(Sensi, contains('Stroke')) ^ 2 * 
                                  select(test_sigma, contains('Stroke')) ^ 2)),
      SUM_Mort = sqrt(rowSums(Sensi ^ 2 * test_sigma ^ 2))
      )
    )
  }
  rm(RR_test,RR_base,test_RR,df,AgeStruct,BaseMort,Sensi,test_sigma)
}

Mortality<-function(PM_r,PM_c,Popu,agestr,y.I,y.P,mode){
  AR <- list(
    real = left_join(PM_r,AbsRisk[['MEAN']], by = "concentration"),
    cf = left_join(PM_c,AbsRisk[['MEAN']], by = "concentration")
  )

  PWRR <- apply(AR[['real']][,-1:-2] + 1, 2, function(x) {
    1 / weighted.mean(x, select(Pop, p = all_of(y.P))$p)
  })
  
  PAF<-data.frame(AR[['cf']][, 1:2],
                  as.data.frame(as.matrix(AR[['cf']][,-1:-2]) %*% diag(PWRR))) %>% 
    `names<-`(names(AR[['cf']])) %>% PAF_standardise(mode = mode)
  
  BaseMort <- as.matrix(AgeMatrix(Popu,agestr,mode = mode)) %*%
    diag(Inci_standardise(mode = mode, y = y.I)) / 1E5
  
  Mort <- bind_cols(PAF[, 1], as.data.frame(BaseMort * PAF[,-1])) %>% 
    `names<-`(c(names(PAF)))
  return(Mort)
  rm(AR,PWRR,PAF,BaseMort,Mort)
}

Grid_Mort <- function(mode,RealPM) {
  
  ls1 <- list()
  
  for (y in colnames(PM_real)[-1]) {
    if (RealPM) {
      ls1[[y]] <- Mortality(
        y.I = y,
        y.P = y,
        PM_r = select(PM_real, FID, concentration = all_of(y)),
        PM_c = select(PM_real, FID, concentration = all_of(y)),
        Popu = select(Pop,concentration = all_of(y)),
        agestr = agegroup[y,],
        mode = mode
      )
    } else {
      ls1[[y]] <- Mortality(
        y.I = y,
        y.P = y,
        PM_r = select(PM_real, FID, concentration = all_of(y)),
        PM_c = select(PM_cf, FID, concentration = all_of(y)),
        Popu = select(Pop,concentration = all_of(y)),
        agestr = agegroup[y,],
        mode = mode
      )
    }
    
  }
  return(ls1)
  rm(ls1)
}

Grid_Aggr <- function(full_result, mode) {
  grid_by.edpt <- list()
  for (y in names(PM_real)[-1]) {
    grid_by.edpt[[y]] <- Result_standardise(full_result[[y]], mode = mode)
  }
  return(grid_by.edpt)
}

Region_Aggr<-function(full_result,mode){
  
  region_by.edpt <- NULL
  
  for (y in colnames(PM_real)[-1]) {
    data <- full_result[[y]] %>%
      `names<-`(c('FID', names(grid_full[[y]])[-1])) %>%
      left_join(select(FID_info, FID, province), by = 'FID')
    
    data <- data[, c('FID', 'province', names(grid_full[[y]])[-1])]
    
    region <- matrix(
      ncol = length(data[, -1:-2]),
      nrow = length(levels(as.factor(data$province))),
      dimnames = list(levels(as.factor(data$province)), names(data)[-1:-2])
    )
    
    for (case in names(data[, -1:-2])) {
      region[, case] <-
        tapply(data[, case], data[, 'province'], sum)
    }
    
    region <- bind_cols(province = rownames(region),
                        as.data.frame(region))
    
    region <- Result_standardise(region, mode = mode)
    
    df <- select(Pop, FID, pop = all_of(y)) %>%
      left_join(select(PM_real, FID, pm = all_of(y)), by = 'FID') %>%
      left_join(select(FID_info, FID, province), by = 'FID')
    
    PWPM = data.frame(concentration = round(
      tapply(df$pm * df$pop, df$province, sum) / tapply(df$pop, df$province, sum),
      1
    ))
    
    SUMPop = tapply(df$pop, df$province, sum)
    
    Mort_UP <- region[,-1] + uncertainty(
      y = y, 
      PM = PWPM,
      Popu = SUMPop,
      mode = mode,
      includePM = T,
      bound = 'UP')
    Mort_LOW <- region[, -1] - uncertainty(
      y = y, 
      PM = PWPM,
      Popu = SUMPop,
      mode = mode,
      includePM = T,
      bound = 'LOW')
    
    region_by.edpt[[y]] <- bind_cols(
      region,
      Mort_UP,
      Mort_LOW
    ) %>% `names<-`(c(
      'province',
      paste0(names(region)[-1], '_MEAN'),
      paste0(names(region)[-1], '_UP'),
      paste0(names(region)[-1], '_LOW')
    ))
    rm(data,region,df,PWPM,SUMPop,Mort_UP,Mort_LOW)
  }
  return(region_by.edpt)
  rm(region_by.edpt)
}

Nation_by.edpt <- function(full_result,mode) {
  
  nation_by.edpt <- NULL
  
  for (y in colnames(PM_real)[-1]) {
    
    nation <- Result_standardise(full_result[[y]],mode=mode)
    
    nation <- bind_rows(colSums(nation[-1])) 
    
    PWPM <- data.frame(
      concentration = round(weighted.mean(
        select(PM_real, pm = all_of(y)), select(Pop, pop = all_of(y))
      ),1))
    
    Mort_LOW <- nation - uncertainty(
      y = y, 
      PM = PWPM,
      Popu = sum(select(Pop, all_of(y))),
      mode = mode,
      includePM = T,
      bound = 'LOW'
    )
    
    Mort_UP <- nation + uncertainty(
      y = y, 
      PM = PWPM,
      Popu =  sum(select(Pop, all_of(y))),
      mode = mode,
      includePM = T,
      bound = 'UP'
    )
    nation_by.edpt <- bind_rows(
      nation_by.edpt,
      bind_cols(
        year = c(y, y, y),
        CI95 = c('MEAN', 'LOW', 'UP'),
        bind_rows(
          nation,
          Mort_LOW,
          Mort_UP
        )))
    
    rm(nation, PWPM,Mort_UP,Mort_LOW)
  }
  return(nation_by.edpt)
  rm(nation_by.edpt)
}

Nation_by.age<-function(full_result,mode){
  nation_by.age <- NULL
  for (y in colnames(Pop)[-1]) {
    data <- full_result[[y]]
    if (mode=='IER') {
      df <- colSums(
        bind_cols(
          SUM_0 = select(data, contains('LRI.0')),
          SUM_25 = rowSums(select(data, contains('.25')&-contains('LRI'))),
          SUM_30 = rowSums(select(data, contains('.30')&-contains('LRI'))),
          SUM_35 = rowSums(select(data, contains('.35')&-contains('LRI'))),
          SUM_40 = rowSums(select(data, contains('.40')&-contains('LRI'))),
          SUM_45 = rowSums(select(data, contains('.45')&-contains('LRI'))),
          SUM_50 = rowSums(select(data, contains('.50')&-contains('LRI'))),
          SUM_55 = rowSums(select(data, contains('.55')&-contains('LRI'))),
          SUM_60 = rowSums(select(data, contains('.60')&-contains('LRI'))),
          SUM_65 = rowSums(select(data, contains('.65')&-contains('LRI'))),
          SUM_70 = rowSums(select(data, contains('.70')&-contains('LRI'))),
          SUM_75 = rowSums(select(data, contains('.75')&-contains('LRI'))),
          SUM_80 = rowSums(select(data, contains('.80')&-contains('LRI'))),
          SUM_85 = rowSums(select(data, contains('.85')&-contains('LRI'))),
          SUM_90 = rowSums(select(data, contains('.90')&-contains('LRI'))),
          SUM_95 = rowSums(select(data, contains('.95')&-contains('LRI')))
        )
      )
    } else if (mode=='NCD+LRI'|mode=='5COD') {
      df <- colSums(
        bind_cols(
          SUM_25 = rowSums(select(data, contains('.25'))),
          SUM_30 = rowSums(select(data, contains('.30'))),
          SUM_35 = rowSums(select(data, contains('.35'))),
          SUM_40 = rowSums(select(data, contains('.40'))),
          SUM_45 = rowSums(select(data, contains('.45'))),
          SUM_50 = rowSums(select(data, contains('.50'))),
          SUM_55 = rowSums(select(data, contains('.55'))),
          SUM_60 = rowSums(select(data, contains('.60'))),
          SUM_65 = rowSums(select(data, contains('.65'))),
          SUM_70 = rowSums(select(data, contains('.70'))),
          SUM_75 = rowSums(select(data, contains('.75'))),
          SUM_80 = rowSums(select(data, contains('.80'))),
          SUM_85 = rowSums(select(data, contains('.85'))),
          SUM_90 = rowSums(select(data, contains('.90'))),
          SUM_95 = rowSums(select(data, contains('.95')))
        )
      )
    }
    nation_by.age <-
      bind_rows(nation_by.age, c(year = y, df, SUM_ALL = sum(df)))
  }
  return(nation_by.age)
  rm(df,data,nation_by.age)
}
# 凑个整，开心---- 
