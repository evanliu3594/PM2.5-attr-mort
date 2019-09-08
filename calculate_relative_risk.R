#################################################
#             Calculate Relativ Risk            #
#################################################

choose.dir() #choose working directory

library(tidyverse);library(readxl);library(writexl)

RelativeRisk <- function(mode='t.test') {
  IER_para<-read_csv('.\\Data\\IER2017_parameters.csv')
  
  IER_curve<-function(x,alpha,beta,gamma,tmrel) {
    if (x>tmrel) {
      1 +  alpha * ( 1 - exp( -  beta * ( x -  tmrel ) ^ gamma))
    }
    else  1
  }
  
  if (mode=='montecarlo') {
    CI_mode<-function(data,n=50000,CI=0.95){
      set.seed(seed)
      df<-round(runif(n,min = 1,max = 1000),digits = 0)
      df<-data[df]
      if (max(df)==min(df)) {
        data.frame(mean=unique(df),low=unique(df),up=unique(df))
      } else {
        t<-t.test(df,conf.level = CI)
        data.frame(mean=t$estimate[1],low=t$conf.int[1],up=t$conf.int[2])
      }
    }
  } else if (mode=='t.test') {
      CI_mode<-function(x,CI=0.95){
        if (max(x)==min(x)) {
          data.frame(mean=unique(x),low=unique(x),up=unique(x))
        } else {
          t<-t.test(x,conf.level = CI)
          data.frame(mean=t$estimate[1],low=t$conf.int[1],up=t$conf.int[2])
        } 
      }
  } else  if (mode=='bootstrap') {
        CI_mode<-function(data,times=1000,alpha=0.05){
          x_bar<-mean(data)
          x_sd<-sd(data)
          n<-length(data)
          M=matrix(rep(NA,times*n),nrow = times,ncol=n)
          for(i in 1:times){
            M[i,]<-sample(data,n,replace = T)
          }
          t_boot<-function(x){(mean(x)-x_bar)/sd(x)}
          t_vector<-apply(M,1,t_boot)
         
          up_t_b<-sort(t_vector)[times*(1-alpha/2)] 
          down_t_b<-sort(t_vector)[times*alpha/2]
          up_t<-x_bar+up_t_b*x_sd
          down_t<-x_bar+down_t_b*x_sd
          data.frame(mean=x_bar,low=down_t,up=up_t)
        }
  } else if (mode=='manual') {
        CI_mode<-function(x,sigma=-1,alpha=0.05){
          n<-length(x)
          a<-mean(x)
          if(sigma>=0){
            b<-sigma/sqrt(n)*qnorm(1-alpha/2);df<-n
          }
          else{
            b<-sd(x)/sqrt(n)*qt(1-alpha/2,n-1);df<- n-1
          }
          low=a-b;up=a+b
          data.frame(mean=a,low=low,up=up)
        }
  }
  
  concentration<-round((seq(0,300,0.1)),digits = 1)
  
  RRlist<-data.frame(concentration)
  
  for (n in 0:32) {
    from=n*1000+1; to=(n+1)*1000
    data<-Reduce(merge,list(data.frame(IER_para[from:to,]),data.frame(concentration=concentration)))
    result<-matrix(
      with(data,mapply(IER_curve,alpha=alpha,beta=beta,gamma=gamma,tmrel=tmrel,x=concentration)),nrow = 3001,byrow = T)
    RR_aggregate<-do.call(rbind,apply(result, 1 ,CI_mode))
    division<-paste(unique(data$cause),unique(data$age),sep='_')
    names(RR_aggregate)<-paste(division,c('mean','low','up'),sep = '_')
    RRlist<-cbind(RRlist,RR_aggregate)
  }
  
  idle<-data.frame(matrix(rep(1,30010),nrow = 3001,ncol = 10)) %>% `colnames<-`(c(paste0('IHD_',seq(0,20,5)),paste0('STROKE_',seq(0,20,5))))
  
  arrangement<-c("COPD_ALL","LC_ALL","LRI_ALL",
                 "IHD_0","IHD_5","IHD_10","IHD_15","IHD_20","IHD_25","IHD_30","IHD_35","IHD_40","IHD_45","IHD_50",
                 "IHD_55","IHD_60","IHD_65","IHD_70","IHD_75","IHD_80","IHD_85","IHD_90","IHD_95",
                 "STROKE_0","STROKE_5","STROKE_10","STROKE_15","STROKE_20","STROKE_25","STROKE_30","STROKE_35","STROKE_40","STROKE_45","STROKE_50",
                 "STROKE_55","STROKE_60","STROKE_65","STROKE_70","STROKE_75","STROKE_80","STROKE_85","STROKE_90","STROKE_95")
  
  RR_mean<-data.frame(`colnames<-`(idle,paste0(names(idle),'_mean')),concentration,select(RRlist,contains('mean')))
  RR_mean<-RR_mean[c('concentration',paste0(arrangement,'_mean'))]
  
  RR_up<-data.frame(`colnames<-`(idle,paste0(names(idle),'_up')),concentration,select(RRlist,contains('up')))
  RR_up<-RR_up[c('concentration',paste0(arrangement,'_up'))]
  
  RR_low<-data.frame(`colnames<-`(idle,paste0(names(idle),'_low')),concentration,select(RRlist,contains('low')))
  RR_low<-RR_low[c('concentration',paste0(arrangement,'_low'))]
  
  RRlist<-list(mean=RR_mean,up=RR_up,low=RR_low)
  
  write_xlsx(RRlist,paste0(".\\Result\\PAF_IER_2017_",mode,".xlsx"))
  
  return(RRlist)
}

################T E S T I N G#######################
#mode=(t.test       montecarlo     manual      bootstrap)

RR<-RelativeRisk(mode = 't.test')

