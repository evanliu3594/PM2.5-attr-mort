a<-head(unlist(strsplit(rstudioapi::getActiveDocumentContext()$path,split = '/')),-1)
# the above commend only works in Rstudio
setwd(paste(a[1:(length(a)-1)],collapse = '/'))
rm(a)

library(tidyverse)
library(readxl)
library(writexl)
library(reshape2)
source('./Code/BasicFunctions.R')

# Set Calc Condition ----

 mode='NCD+LRI'  ;RealPM=T
# mode='5COD'     ;RealPM=T
# mode='IER'      ;RealPM=T
# mode='NCD+LRI'  ;RealPM=F
# mode='5COD'     ;RealPM=F
# mode='IER'      ;RealPM=F

## Data Import ----

{
  Pop <- read_csv('./Data/Grid.Pop.csv') %>% arrange(FID)
  
  PM_real <- read_csv('./Data/Gird.PM25.txt') %>% arrange(FID)
  PM_real <- bind_cols(PM_real[, 1], round(PM_real[,-1], 1))
  
  if (!RealPM) {
    PM_cf <- read_csv('./Data/PM_Ctrl.csv') %>% arrange(FID)
    PM_cf <- bind_cols(PM_cf[, 1], round(PM_cf[,-1], 1))
  }
  
  inci <- read_csv('./Data/GBD_incidence_China_2000-2017.csv')
  
  agegroup <- read_csv('./Data/GBD_agestructure_China_2000-2017.csv')
  
  agegroup <- data.frame(
    prop.table(as.matrix(agegroup[,-1]), 1)
  ) %>% `rownames<-`(c(agegroup$year))
  
  if (mode=='IER') {
    RR_table <- list(
      MEAN = read_excel('./Data/GBD2017_RR_LYF_percentile.xlsx', sheet = 'MEAN'),
      LOW = read_excel('./Data/GBD2017_RR_LYF_percentile.xlsx', sheet = 'LOW'),
      UP = read_excel('./Data/GBD2017_RR_LYF_percentile.xlsx', sheet = 'UP')
    )
  } else if (mode=='5COD' | mode=='NCD+LRI'){
    RR_table <- list(
      MEAN = read_excel('./Data/GEMM_RR_2020-03-01.xlsx', sheet = 'MEAN'),
      LOW = read_excel('./Data/GEMM_RR_2020-03-01.xlsx', sheet = 'LOW'),
      UP = read_excel('./Data/GEMM_RR_2020-03-01.xlsx', sheet = 'UP')
    )
  }
  
  RR_table[['MEAN']]$concentration %>% round(1) -> RR_table[['MEAN']]$concentration
  RR_table[['LOW']]$concentration %>% round(1) -> RR_table[['LOW']]$concentration
  RR_table[['UP']]$concentration %>% round(1) -> RR_table[['UP']]$concentration
  
  AbsRisk <- list(
    MEAN = bind_cols(RR_table[['MEAN']][, 1] %>% round(1), RR_table[['MEAN']][,-1] - 1),
    LOW = bind_cols(RR_table[['LOW']][, 1] %>% round(1), RR_table[['LOW']][,-1] - 1),
    UP = bind_cols(RR_table[['UP']][, 1] %>% round(1), RR_table[['UP']][,-1] - 1)
  )
}

# Basic Data Processing ----

Decomposition<-function(serie,y.a,y.b,mode){
  
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
  
  serie.step <- matrix(c(
    'PG','PA','EXP','ORF','PG','PA','ORF','EXP','PG','EXP','PA','ORF',
    'PG','EXP','ORF','PA','PG','ORF','PA','EXP','PG','ORF','EXP','PA',
    'PA','PG','EXP','ORF','PA','PG','ORF','EXP','PA','EXP','PG','ORF',
    'PA','EXP','ORF','PG','PA','ORF','PG','EXP','PA','ORF','EXP','PG',
    'EXP','PG','PA','ORF','EXP','PG','ORF','PA','EXP','PA','PG','ORF',
    'EXP','PA','ORF','PG','EXP','ORF','PA','PG','EXP','ORF','PG','PA',
    'ORF','PG','PA','EXP','ORF','PG','EXP','PA','ORF','PA','PG','EXP',
    'ORF','PA','EXP','PG','ORF','EXP','PG','PA','ORF','EXP','PA','PG'
    ), ncol = 4,byrow = T
  ) [serie, ]

  # Mort.Start ----
  Mort_Start<-Mortality(
    Popu = select(Pop,concentration = all_of(y.a)),
    agestr = agegroup[y.a,],
    y.I = y.a,
    PM_c = select(PM_real, FID, concentration = all_of(y.a)),
    PM_r = select(PM_real, FID, concentration = all_of(y.a)),
    y.P = y.a,
    mode = mode
  ) %>% Result_standardise(mode=mode)

  # Mort.1----
  
  if (serie.step[1]=='PG') {
    Mort_1 <- Mortality(
      Popu = select(Pop,concentration = all_of(y.b)),
      agestr = agegroup[y.a,],
      y.I = y.a,
      PM_c = select(PM_real, FID, concentration = all_of(y.a)),
      PM_r = select(PM_real, FID, concentration = all_of(y.a)),
      y.P = y.b,
      mode = mode
    ) %>% Result_standardise(mode=mode)
  } else if (serie.step[1]=='PA') {
    Mort_1 <- Mortality(
      Popu = select(Pop,concentration = all_of(y.a)),
      agestr = agegroup[y.b,],
      y.I = y.a,
      PM_c = select(PM_real, FID, concentration = all_of(y.a)),
      PM_r = select(PM_real, FID, concentration = all_of(y.a)),
      y.P = y.a,
      mode = mode
    ) %>% Result_standardise(mode=mode)
  } else if (serie.step[1]=='EXP') {
    Mort_1 <- Mortality(
      Popu = select(Pop,concentration = all_of(y.a)),
      agestr = agegroup[y.a,],
      y.I = y.a,
      PM_c = select(PM_real, FID, concentration = all_of(y.b)),
      PM_r = select(PM_real, FID, concentration = all_of(y.a)),
      y.P = y.a,
      mode = mode
    ) %>% Result_standardise(mode=mode)
  } else if (serie.step[1]=='ORF') {
    Mort_1 <- Mortality(
      Popu = select(Pop,concentration = all_of(y.a)),
      agestr = agegroup[y.a,],
      y.I = y.b,
      PM_c = select(PM_real, FID, concentration = all_of(y.a)),
      PM_r = select(PM_real, FID, concentration = all_of(y.b)),
      y.P = y.a,
      mode = mode
    ) %>% Result_standardise(mode=mode)
  }

  # Mort.2----
  
  if (sum(serie.step[1:2] %in% c('PG', 'PA')) == 2) {
    # PG  PA
    Mort_2 <- Mortality(
      Popu = select(Pop, concentration = all_of(y.b)),
      agestr = agegroup[y.b, ],
      y.I = y.a,
      PM_c = select(PM_real, FID, concentration = all_of(y.a)),
      PM_r = select(PM_real, FID, concentration = all_of(y.a)),
      y.P = y.b,
      mode = mode
    ) %>% Result_standardise(mode = mode)
  } else if (sum(serie.step[1:2] %in% c('PG', 'EXP')) == 2) {
    #  PG  EXP
    Mort_2 <- Mortality(
      Popu = select(Pop, concentration = all_of(y.b)),
      agestr = agegroup[y.a, ],
      y.I = y.a,
      PM_c = select(PM_real, FID, concentration = all_of(y.b)),
      PM_r = select(PM_real, FID, concentration = all_of(y.a)),
      y.P = y.b,
      mode = mode
    ) %>% Result_standardise(mode = mode)
  } else if (sum(serie.step[1:2] %in% c('PG', 'ORF')) == 2) {
    #  PG  ORF
    Mort_2 <- Mortality(
      Popu = select(Pop, concentration = all_of(y.b)),
      agestr = agegroup[y.a, ],
      y.I = y.b,
      PM_c = select(PM_real, FID, concentration = all_of(y.a)),
      PM_r = select(PM_real, FID, concentration = all_of(y.b)),
      y.P = y.b,
      mode = mode
    ) %>% Result_standardise(mode = mode)
  } else if (sum(serie.step[1:2] %in% c('PA', 'EXP')) == 2) {
    #  PA  EXP
    Mort_2 <- Mortality(
      Popu = select(Pop, concentration = all_of(y.a)),
      agestr = agegroup[y.b, ],
      y.I = y.a,
      PM_c = select(PM_real, FID, concentration = all_of(y.b)),
      PM_r = select(PM_real, FID, concentration = all_of(y.a)),
      y.P = y.a,
      mode = mode
    ) %>% Result_standardise(mode = mode)
  } else if (sum(serie.step[1:2] %in% c('PA', 'ORF')) == 2) {
    #  PA  ORF
    Mort_2 <- Mortality(
      Popu = select(Pop, concentration = all_of(y.a)),
      agestr = agegroup[y.b, ],
      y.I = y.b,
      PM_c = select(PM_real, FID, concentration = all_of(y.a)),
      PM_r = select(PM_real, FID, concentration = all_of(y.b)),
      y.P = y.a,
      mode = mode
    ) %>% Result_standardise(mode = mode)
  } else if (sum(serie.step[1:2] %in% c('EXP', 'ORF')) == 2) {
    #  EXP ORF
    Mort_2 <- Mortality(
      Popu = select(Pop, concentration = all_of(y.a)),
      agestr = agegroup[y.a, ],
      y.I = y.b,
      PM_c = select(PM_real, FID, concentration = all_of(y.b)),
      PM_r = select(PM_real, FID, concentration = all_of(y.b)),
      y.P = y.a,
      mode = mode
    ) %>% Result_standardise(mode = mode)
  } 

  # Mort.3----

  if (sum(serie.step[1:3] %in% c('PG', 'PA','EXP')) == 3) {
    # PG  PA EXP
    Mort_3 <- Mortality(
      Popu = select(Pop, concentration = all_of(y.b)),
      agestr = agegroup[y.b,],
      y.I = y.a,
      PM_c = select(PM_real, FID, concentration = all_of(y.b)),
      PM_r = select(PM_real, FID, concentration = all_of(y.a)),
      y.P = y.b,
      mode = mode
    ) %>% Result_standardise(mode = mode)
    
  } else if (sum(serie.step[1:3] %in% c('PG', 'PA','ORF')) == 3) {
    # PG  PA ORF
    Mort_3 <- Mortality(
      Popu = select(Pop, concentration = all_of(y.b)),
      agestr = agegroup[y.b,],
      y.I = y.b,
      PM_c = select(PM_real, FID, concentration = all_of(y.a)),
      PM_r = select(PM_real, FID, concentration = all_of(y.b)),
      y.P = y.b,
      mode = mode
    ) %>% Result_standardise(mode = mode)
    
  } else if (sum(serie.step[1:3] %in% c('PG', 'EXP','ORF')) == 3) {
    #  PG	EXP	ORF
    Mort_3 <- Mortality(
      Popu = select(Pop, concentration = all_of(y.b)),
      agestr = agegroup[y.a,],
      y.I = y.b,
      PM_c = select(PM_real, FID, concentration = all_of(y.b)),
      PM_r = select(PM_real, FID, concentration = all_of(y.b)),
      y.P = y.b,
      mode = mode
    ) %>% Result_standardise(mode = mode)
    
  } else if (sum(serie.step[1:3] %in% c('PA', 'EXP','ORF')) == 3) {
    #  PA	EXP	ORF
    Mort_3 <- Mortality(
      Popu = select(Pop, concentration = all_of(y.a)),
      agestr = agegroup[y.b,],
      y.I = y.b,
      PM_c = select(PM_real, FID, concentration = all_of(y.b)),
      PM_r = select(PM_real, FID, concentration = all_of(y.b)),
      y.P = y.a,
      mode = mode
    ) %>% Result_standardise(mode = mode)
  }
  
  # Mort.End ----
  
  Mort_End <- Mortality(
    y.I = y.b,
    y.P = y.b,
    PM_r = select(PM_real, FID, concentration = all_of(y.b)),
    PM_c = select(PM_real, FID, concentration = all_of(y.b)),
    Popu = select(Pop,concentration = all_of(y.b)),
    agestr = agegroup[y.b,],
    mode = mode
  ) %>% Result_standardise(mode=mode)
  
  # Print Result ----
  
  cat(paste0('Drivers Between Year ',y.a,' and Year ',y.b,':\n'))
  cat(paste0(serie.step[1],':\t'))
  cat(sum(select(Mort_1, contains('Mort'))) - sum(select(Mort_Start, contains('Mort'))))
  cat('\n')
  cat(paste0(serie.step[2],':\t'))
  cat(sum(select(Mort_2, contains('Mort'))) - sum(select(Mort_1, contains('Mort'))))
  cat('\n')
  cat(paste0(serie.step[3],':\t'))
  cat(sum(select(Mort_3, contains('Mort'))) - sum(select(Mort_2, contains('Mort'))))
  cat('\n')
  cat(paste0(serie.step[4],':\t'))
  cat(sum(select(Mort_End, contains('Mort'))) - sum(select(Mort_3, contains('Mort'))))
  cat('\n')

  return(
    data.frame(
      Mort_Start['FID'],
      select(Mort_Start, contains('Mort')),
      select(Mort_1, contains('Mort')) - select(Mort_Start, contains('Mort')),
      select(Mort_2, contains('Mort')) - select(Mort_1, contains('Mort')),
      select(Mort_3, contains('Mort')) - select(Mort_2, contains('Mort')),
      select(Mort_End, contains('Mort')) - select(Mort_3, contains('Mort')),
      select(Mort_End, contains('Mort'))
    ) %>% `names<-`(c('FID', 'Start', serie.step, 'End'))
  )
  rm(Mort_Start,Mort_1,Mort_2,Mort_3,Mort_End,serie.step)
}



# Claculating S&D of Rates----

Decompose0412 <- NULL

for (i in 1:24) {
  Decompose0412[[paste0(i)]] <-
    Decomposition(
      serie = i,
      y.a = '2004',
      y.b = '2012',
      mode = mode
    )
}

prov_Decomp0412 <- list(
  Start = data.frame(
    province = levels(as.factor(
      left_join(PM_real['FID'], FID_info[c('FID', 'province')], by = 'FID')$province)),
    sapply(Decompose0412, function(x) {
    df1 <- left_join(select(x, FID, Start), select(FID_info, FID, province), by = 'FID')
    return(aggregate(df1['Start'], df1['province'], sum)[-1])
    rm(df1)
  })),
  PG = data.frame(province = levels(as.factor(
    left_join(PM_real['FID'], FID_info[c('FID', 'province')], by = 'FID')$province)),
    sapply(Decompose0412, function(x) {
    df1 <- left_join(select(x, FID, PG), select(FID_info, FID, province), by = 'FID')
    return(aggregate(df1['PG'], df1['province'], sum)[-1])
    rm(df1)
  })),
  PA = data.frame(province = levels(as.factor(
    left_join(PM_real['FID'], FID_info[c('FID', 'province')], by = 'FID')$province)),
    sapply(Decompose0412, function(x) {
    df1 <- left_join(select(x, FID, PA), select(FID_info, FID, province), by = 'FID')
    return(aggregate(df1['PA'], df1['province'], sum)[-1])
    rm(df1)
  })),
  EXP = data.frame(province = levels(as.factor(
    left_join(PM_real['FID'], FID_info[c('FID', 'province')], by = 'FID')$province)),
    sapply(Decompose0412, function(x) {
    df1 <- left_join(select(x, FID, EXP), select(FID_info, FID, province), by = 'FID')
    return(aggregate(df1['EXP'], df1['province'], sum)[-1])
    rm(df1)
  })),
  ORF = data.frame(province = levels(as.factor(
    left_join(PM_real['FID'], FID_info[c('FID', 'province')], by = 'FID')$province)),
    sapply(Decompose0412, function(x) {
    df1 <- left_join(select(x, FID, ORF), select(FID_info, FID, province), by = 'FID')
    return(aggregate(df1['ORF'], df1['province'], sum)[-1])
    rm(df1)
  })),
  End = data.frame(province = levels(as.factor(
    left_join(PM_real['FID'], FID_info[c('FID', 'province')], by = 'FID')$province)),
    sapply(Decompose0412, function(x) {
    df1 <- left_join(select(x, FID, End), select(FID_info, FID, province), by = 'FID')
    return(aggregate(df1['End'], df1['province'], sum)[-1])
    rm(df1)
  }))
)


write_xlsx(
  prov_Decomp0412,
  paste0('./Result/Decomp_Prov_0412_', mode, '_', Sys.Date(), '.xlsx')
)



Nation_Decomp0412<-data.frame(
  Start = colSums(sapply(Decompose0412, function(x) {x$Start})),
  PG = colSums(sapply(Decompose0412, function(x) {x$PG})),
  PA = colSums(sapply(Decompose0412, function(x) {x$PA})),
  EXP = colSums(sapply(Decompose0412, function(x) {x$EXP})),
  ORF = colSums(sapply(Decompose0412, function(x) {x$ORF})),
  End = colSums(sapply(Decompose0412, function(x) {x$End}))
)


Decompose1217 <- NULL

for (i in 1:24) {
  Decompose1217[[paste0(i)]] <-
    Decomposition(
      serie = i,
      y.a = '2012',
      y.b = '2017',
      mode = mode
    )
}

prov_Decomp1217 <- list(
  Start = data.frame(
    province = levels(as.factor(
      left_join(PM_real['FID'], FID_info[c('FID', 'province')], by = 'FID')$province)),
    sapply(Decompose1217, function(x) {
      df1 <- left_join(select(x, FID, Start), select(FID_info, FID, province), by = 'FID')
      return(aggregate(df1['Start'], df1['province'], sum)[-1])
      rm(df1)
    })),
  PG = data.frame(province = levels(as.factor(
    left_join(PM_real['FID'], FID_info[c('FID', 'province')], by = 'FID')$province)),
    sapply(Decompose1217, function(x) {
      df1 <- left_join(select(x, FID, PG), select(FID_info, FID, province), by = 'FID')
      return(aggregate(df1['PG'], df1['province'], sum)[-1])
      rm(df1)
    })),
  PA = data.frame(province = levels(as.factor(
    left_join(PM_real['FID'], FID_info[c('FID', 'province')], by = 'FID')$province)),
    sapply(Decompose1217, function(x) {
      df1 <- left_join(select(x, FID, PA), select(FID_info, FID, province), by = 'FID')
      return(aggregate(df1['PA'], df1['province'], sum)[-1])
      rm(df1)
    })),
  EXP = data.frame(province = levels(as.factor(
    left_join(PM_real['FID'], FID_info[c('FID', 'province')], by = 'FID')$province)),
    sapply(Decompose1217, function(x) {
      df1 <- left_join(select(x, FID, EXP), select(FID_info, FID, province), by = 'FID')
      return(aggregate(df1['EXP'], df1['province'], sum)[-1])
      rm(df1)
    })),
  ORF = data.frame(province = levels(as.factor(
    left_join(PM_real['FID'], FID_info[c('FID', 'province')], by = 'FID')$province)),
    sapply(Decompose1217, function(x) {
      df1 <- left_join(select(x, FID, ORF), select(FID_info, FID, province), by = 'FID')
      return(aggregate(df1['ORF'], df1['province'], sum)[-1])
      rm(df1)
    })),
  End = data.frame(province = levels(as.factor(
    left_join(PM_real['FID'], FID_info[c('FID', 'province')], by = 'FID')$province)),
    sapply(Decompose1217, function(x) {
      df1 <- left_join(select(x, FID, End), select(FID_info, FID, province), by = 'FID')
      return(aggregate(df1['End'], df1['province'], sum)[-1])
      rm(df1)
    }))
)
write_xlsx(
  prov_Decomp1217,
  paste0('./Result/Decomp_Prov_1217_', mode, '_', Sys.Date(), '.xlsx')
)


Nation_Decomp1217<-data.frame(
  Start = colSums(sapply(Decompose1217, function(x) {x$Start})),
  PG = colSums(sapply(Decompose1217, function(x) {x$PG})),
  PA = colSums(sapply(Decompose1217, function(x) {x$PA})),
  EXP = colSums(sapply(Decompose1217, function(x) {x$EXP})),
  ORF = colSums(sapply(Decompose1217, function(x) {x$ORF})),
  End = colSums(sapply(Decompose1217, function(x) {x$End}))
)


Decompose0417 <- NULL

for (i in 1:24) {
  Decompose0417[[paste0(i)]] <-
    Decomposition(
      serie = i,
      y.a = '2004',
      y.b = '2017',
      mode = mode
    )
}

Nation_Decomp0417<-data.frame(
  Start = colSums(sapply(Decompose0417, function(x) {x$Start})),
  PG = colSums(sapply(Decompose0417, function(x) {x$PG})),
  PA = colSums(sapply(Decompose0417, function(x) {x$PA})),
  EXP = colSums(sapply(Decompose0417, function(x) {x$EXP})),
  ORF = colSums(sapply(Decompose0417, function(x) {x$ORF})),
  End = colSums(sapply(Decompose0417, function(x) {x$End}))
)

prov_Decomp0417 <- list(
  Start = data.frame(
    province = levels(as.factor(
      left_join(PM_real['FID'], FID_info[c('FID', 'province')], by = 'FID')$province)),
    sapply(Decompose0417, function(x) {
      df1 <- left_join(select(x, FID, Start), select(FID_info, FID, province), by = 'FID')
      return(aggregate(df1['Start'], df1['province'], sum)[-1])
      rm(df1)
    })),
  PG = data.frame(province = levels(as.factor(
    left_join(PM_real['FID'], FID_info[c('FID', 'province')], by = 'FID')$province)),
    sapply(Decompose0417, function(x) {
      df1 <- left_join(select(x, FID, PG), select(FID_info, FID, province), by = 'FID')
      return(aggregate(df1['PG'], df1['province'], sum)[-1])
      rm(df1)
    })),
  PA = data.frame(province = levels(as.factor(
    left_join(PM_real['FID'], FID_info[c('FID', 'province')], by = 'FID')$province)),
    sapply(Decompose0417, function(x) {
      df1 <- left_join(select(x, FID, PA), select(FID_info, FID, province), by = 'FID')
      return(aggregate(df1['PA'], df1['province'], sum)[-1])
      rm(df1)
    })),
  EXP = data.frame(province = levels(as.factor(
    left_join(PM_real['FID'], FID_info[c('FID', 'province')], by = 'FID')$province)),
    sapply(Decompose0417, function(x) {
      df1 <- left_join(select(x, FID, EXP), select(FID_info, FID, province), by = 'FID')
      return(aggregate(df1['EXP'], df1['province'], sum)[-1])
      rm(df1)
    })),
  ORF = data.frame(province = levels(as.factor(
    left_join(PM_real['FID'], FID_info[c('FID', 'province')], by = 'FID')$province)),
    sapply(Decompose0417, function(x) {
      df1 <- left_join(select(x, FID, ORF), select(FID_info, FID, province), by = 'FID')
      return(aggregate(df1['ORF'], df1['province'], sum)[-1])
      rm(df1)
    })),
  End = data.frame(province = levels(as.factor(
    left_join(PM_real['FID'], FID_info[c('FID', 'province')], by = 'FID')$province)),
    sapply(Decompose0417, function(x) {
      df1 <- left_join(select(x, FID, End), select(FID_info, FID, province), by = 'FID')
      return(aggregate(df1['End'], df1['province'], sum)[-1])
      rm(df1)
    }))
)

write_xlsx(
  prov_Decomp0417,
  paste0('./Result/Decomp_Prov_0417_', mode, '_', Sys.Date(), '.xlsx')
)

