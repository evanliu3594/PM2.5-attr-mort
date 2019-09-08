########################################################################################
#                         Calculate_PM2.5_attributed_mortality                         #
########################################################################################
#                                                                                      #
#  参数说明：                                                                          #
#  RR：选择相对风险参数的置信上界（up）、均值（mean）和置信下界（low）                 #
#  mode：计算RR置信区间的方式，有普通t检验（t.test），蒙特卡洛（montecarlo）及BOOTSTRAP#
#  output：选择是否输出对应的结果，对应参数分别是：                                    #
#     output_full=T（输出全网格全终端全年龄死亡人数）                                  #
#     output_allage=T (输出全网格全终端死亡人数)                                       #
#     output_allage_rign=T (输出各省份各终端死亡人数)                                  #
#     output_nation=T (输出全国分终端死亡人数)                                         #
#  本函数返回1个list，包含：                                                           #
#  04-17年间全网格全终端全年龄死亡人数（full_grid）、                                  #
#  04-17年间全网格全终端死亡人数（edpt_grid）、                                        #
#  04-17年间各省份全终端死亡人数（edpt_rign）、                                        #
#  以及04-17年间全国分终端死亡人数（nation）是dataframe                                #
#                                                                                      #
#  Created By YfLIU 20190904                                                           #
#                                                                                      #
########################################################################################

choose.dir()

install.packages('tidyverse')
install.packages('readxl')
install.packages('writexl')

library(tidyverse);library(readxl);library(writexl)

Attri_Mort<-function(RR='up',mode='montecarlo',output_full=F,output_allage=F,
                     output_allage_rign=F,output_nation=F){
  
  #################################################################
  #                     module1. Reading Data                     #
  #################################################################
  
  FID_info<-read_csv('.//Data//FID_infomation.txt')
  Pop<-read_csv('.//Data//Gridded Population.txt')
  agegroup<-read_csv('.//Data//Age Structure_China_GBD.csv')
  agroup<-data.frame(agegroup[,1],
                       t(apply(as.matrix(agegroup[-1]),1,FUN = function(x) { x=x/colSums(agegroup[,-1])}))) %>% `names<-`(names(agegroup))
  inci<-read_csv('.//Data//Baseline incidence_China_GBD.csv')
  PM_raw<-read_csv('.//Result//PM2.5_concentration.csv')
  RR_table<-read_excel(paste0(".\\Result\\PAF_IER_2017_",mode,".xlsx"),sheet = RR)
  
  #################################################################
  #                    module2. PAF Calculate                     #
  #################################################################
  PAF_lookup<-data.frame(concentration=round(seq(0,300,0.1),digits = 1),(RR_table[,-1]-1)/RR_table[,-1])
  
  PAF<-NULL
  
  for (y in paste0(2004:2017)){
    
    PM<-select(PM_raw,FID,concentration=y)
    PAF_grid<-left_join(PM,PAF_lookup,by = "concentration")
    
    PAF[[y]]$COPD<-bind_cols(PAF_grid[,1],replicate(20,select(PAF_grid,contains('COPD')),simplify = FALSE))
    names(PAF[[y]]$COPD)<-c('FID',paste0('COPD_',seq(0,95,5),'_',RR))
    
    PAF[[y]]$LC<-bind_cols(PAF_grid[,1],replicate(20,select(PAF_grid,contains('LC')),simplify = FALSE))
    names(PAF[[y]]$LC)<-c('FID',paste0('LC_',seq(0,95,5),'_',RR))
    
    PAF[[y]]$LRI<-bind_cols(PAF_grid[,1],replicate(20,select(PAF_grid,contains('LRI')),simplify = FALSE))
    names(PAF[[y]]$LRI)<-c('FID',paste0('LRI_',seq(0,95,5),'_',RR))
    
    PAF[[y]]$IHD<-bind_cols(PAF_grid[,1],select(PAF_grid,contains('IHD')))
    names(PAF[[y]]$IHD)<-c('FID',paste0('IHD_',seq(0,95,5),'_',RR))
    
    PAF[[y]]$Stroke<-bind_cols(PAF_grid[,1],select(PAF_grid,contains('Stroke')))
    names(PAF[[y]]$Stroke)<-c('FID',paste0('STROKE_',seq(0,95,5),'_',RR))
  }
  
  #################################################################
  #                 module3. Full range attr mort                 #
  #################################################################
  
  full_grid<-NULL
  for (y in paste0(2004:2017)){
    PAF_annual<-PAF[[y]]
    
    mort<-NULL
    for (edpt in unique(inci$endpoint)){
      mort[[edpt]]<- PAF_annual[[edpt]][,-1] * as.data.frame( 
        as.matrix(Pop[,y]) %*% t(as.matrix(agroup[,y])) %*%  diag(filter(inci,year == y & endpoint == edpt)[,-1:-2])) /1e5
    }
    full_grid[[y]]<-cbind(FID=Pop[,1],select(bind_cols(mort),-contains("LRI")),ALRI=mort[['LRI']][,1])
  }
  
  if (output_full) {
    write_xlsx(full_grid,paste0('.//Result//Result_Full_',RR,'_',mode,'.xlsx'))
  }
  
  #################################################################
  #                module4. aggregation                           #
  #################################################################
  
  allage_grid<-NULL
  for (y in paste0(2004:2017)){
    data<-full_grid[[y]]
    df<-data.frame(FID=data[,1],
                     COPD_SUM=rowSums(select(data,contains('COPD'))),
                     IHD_SUM=rowSums(select(data,contains('IHD'))),
                     LC_SUM=rowSums(select(data,contains('LC'))),
                     ALRI_SUM=rowSums(select(data,contains('ALRI'))),
                     Stroke_SUM=rowSums(select(data,contains('Stroke'))))
    df$SUM_mort<-rowSums(df[,2:6])
    allage_grid[[y]]<-df
  }
  
  if (output_allage) {
    write_xlsx(allage_grid,paste0('.//Result//Result_allage_grid_',RR,'_',mode,'.xlsx'))
  }
  
  allage_rign<-NULL
  for (y in paste0(2004:2017)){
    data<-allage_grid[[y]]
    data<-left_join(data,FID_info,by='FID')
    df<-data.frame(sum_ALL = tapply(data$SUM_mort, data$province, sum,na.rm=T),
                   sum_COPD = tapply(data$COPD_SUM, data$province, sum,na.rm=T),
                   sum_IHD = tapply(data$IHD_SUM, data$province, sum,na.rm=T),
                   sum_LC = tapply(data$LC_SUM, data$province, sum,na.rm=T),
                   sum_ALRI = tapply(data$ALRI_SUM, data$province, sum,na.rm=T),
                   sum_Stroke = tapply(data$Stroke_SUM, data$province, sum,na.rm=T))
    df<-cbind(province=rownames(df),df)
    allage_rign[[y]]<-df
  }
  
  if (output_allage_rign) {
    write_xlsx(allage_rign,paste0('.//Result//Result_allage_rign_',RR,'_',mode,'.xlsx'))
  }
  
  nation<-NULL
  for (y in paste0(2004:2017)){
    data<-allage_rign[[y]]
    nation<-rbind(nation,data.frame(year = y,
                                    sum_ALL = sum(data$sum_ALL,na.rm = T),
                                    sum_COPD = sum(data$sum_COPD,na.rm = T),
                                    sum_IHD = sum(data$sum_IHD,na.rm = T),
                                    sum_LC = sum(data$sum_LC,na.rm = T),
                                    sum_ALRI = sum(data$sum_ALRI,na.rm = T),
                                    sum_Stroke = sum(data$sum_Stroke,na.rm = T)))
  }
  
  if (output_nation) {
    write_xlsx(nation,paste0('.//Result//Result_National_',RR,'_',mode,'.xlsx'))
  }
  
  return(list(FULL=full_grid,ALLAGE=allage_grid,REGION=allage_rign,NATION=nation))
  
}

#  RR='mean'    'up'   'low'

#  mode='t.test'   'montecarlo'   'BOOTSTRAP'     'manual'

#  output_full = T ,output_allage = T,output_allage_rign = T , output_nation = T

mean<-Attri_Mort(RR='mean',mode = 'montecarlo', output_nation = T)

up<-Attri_Mort(RR='up',mode = 'montecarlo', output_nation = T)

low<-Attri_Mort(RR='low',mode = 'montecarlo', output_nation = T)
