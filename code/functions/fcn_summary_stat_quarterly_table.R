fcn_summary_stat_quarterly_table <- function(qtrsumstat , scaleVset, option) {  

  filename = fcn_filename_maker(scaleVset=scaleVset, option=option )
  load(file=filename) 
  est = firm_TS_data
  
  
  ### quarterly average of statistics
  print_set= matrix( rbind(apply(qtrsumstat$ye_stat,1,mean,na.rm=T),
  apply(qtrsumstat$yf_stat,1,mean,na.rm=T),  apply(qtrsumstat$yef_stat,1,mean,na.rm=T)), nrow=3) 
  
  print(cbind(round(100*print_set[,1],4), round(100*print_set[,2],3),round(print_set[,3:4],2),
        round(100*print_set[,5:9],3), round(print_set[,13],3), round(100*print_set[,c(11,12,10)],1)     ))
  
  print(format(sum(est$no_obs  ,na.rm=T), big.mark = "," ))

  
  sample.dataframe = cbind( 100*print_set[,1] ,  100*print_set[,2] ,print_set[,3:4],
         100*print_set[,5:9], print_set[,13], 100*print_set[,c(11,12,10)]     )
  
  sample.dataframe = data.frame( sample.dataframe)
  row.names(sample.dataframe) = c("y","yhat","FE=y-yhat")
  colnames(sample.dataframe) = c("Mean","S.D","Skew","Kurt","5%","25%","50%","75%","95%","AR coef","(-)","0","(+)")
  
  
  write.csv(x = sample.dataframe, file = "Table1A_quarterly.csv" )
  
  
  
  ### pooled  
  zzz = est$ye_firm_deflated
  print_set1  = c(mean(zzz,na.rm=T),sd(zzz,na.rm=T),skewness(zzz,na.rm=T),kurtosis(zzz,na.rm=T),quantile(zzz  ,c(0.05,0.25,0.5,0.75,0.95) ,na.rm=T)
                  , arima(zzz,c(1,0,0))$coef[1],mean(zzz<0,na.rm=T),mean(zzz==0,na.rm=T),mean(zzz>0,na.rm=T)) 
  
  zzz = est$yf_firm_deflated
  print_set2 = c(mean(zzz,na.rm=T),sd(zzz,na.rm=T),skewness(zzz,na.rm=T),kurtosis(zzz,na.rm=T),quantile(zzz  ,c(0.05,0.25,0.5,0.75,0.95) ,na.rm=T)
                 , arima(zzz,c(1,0,0))$coef[1],mean(zzz<0,na.rm=T),mean(zzz==0,na.rm=T),mean(zzz>0,na.rm=T)) 
  
  
 
  pdataset = list( y=est$ye_firm_deflated- est$yf_firm_deflated , 
                   begin_idx =est$firm_begin_deflated, end_idx = est$firm_end_deflated)
  zzz = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
  
  
  m1=arima(zzz,c(1,0,0))
  lowball = mean(zzz==0,na.rm=T)/2+mean(zzz>0,na.rm=T)
  print_set3  = c(mean(zzz,na.rm=T),sd(zzz,na.rm=T),skewness(zzz,na.rm=T),kurtosis(zzz,na.rm=T),quantile(zzz  ,c(0.05,0.25,0.5,0.75,0.95) ,na.rm=T)
                  , m1$coef[1],mean(zzz<0,na.rm=T),mean(zzz==0,na.rm=T),mean(zzz>0,na.rm=T)) 
  print_set4  = 1/100*c(mean(zzz,na.rm=T)/sqrt(m1$var.coef[2,2]),NA,NA,NA,NA,NA,median(zzz,na.rm=T)/sqrt(m1$var.coef[2,2]),
                  NA,NA, 100*m1$coef[1]/sqrt(m1$var.coef[1,1]),NA,NA,
                  (lowball-0.5)/sqrt(lowball*(1-lowball)/(sum(!is.na(zzz))-1) )     )
  print_set= matrix(rbind(print_set1, print_set2, print_set3, print_set4) ,nrow=4)
  print(cbind(round(100*print_set[,1],4), round(100*print_set[,2],3),round(print_set[,3:4],2),
              round(100*print_set[,5:9],3), round(print_set[,10],3), round(100*print_set[,11:13],1)    ))
  
  print(format(sum(est$no_obs_deflated,na.rm=T), big.mark = "," ))
  print( sd(zzz,na.rm=T)/sqrt(sum(!is.na(zzz)))   )
  print(sqrt(arima(zzz,c(1,0,0))$var.coef[1]))
  
  
  sample.dataframe = cbind( 100*print_set[,1] ,  100*print_set[,2] ,print_set[,3:4],
                            100*print_set[,5:9], print_set[,10], 100*print_set[,c(11,12,13)]     )
  
  sample.dataframe = data.frame( sample.dataframe)
  row.names(sample.dataframe) = c("y","yhat","FE=y-yhat","t-stat")
  colnames(sample.dataframe) = c("Mean","S.D","Skew","Kurt","5%","25%","50%","75%","95%","AR coef","(-)","0","(+)")
  
  write.csv(x = sample.dataframe, file = "Table1A_pooled.csv" )
   
  

  yrho_firms =rep(NA, length(est$firm_begin_deflated ))    
  yhatrho_firms =rep(NA, length(est$firm_begin_deflated ))    
  rho_firms =rep(NA, length(est$firm_begin_deflated ))    
  for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
  {  
    if (!is.na(est$firm_begin_deflated[jj])) {
      
      firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj] 
      yyy1  =  est$ye_firm_deflated[firm_idx_temp]    
      yyy2  =  est$yf_firm_deflated[firm_idx_temp]  
      
      if (  !(jj %in% c(2549,2959))) {  
        m1=arima(yyy1, order=c(1,0,0), method = c("ML"),optim.control = list(maxit = 300000 )) 
        yrho_firms[jj] = m1$coef[1]
      }
       #m1=arima(yyy2, order=c(1,0,0), method = c("ML"),optim.control = list(maxit = 300000 ))         
        #yhatrho_firms[jj] = m1$coef[1]
        m1=arima(yyy1-yyy2, order=c(1,0,0), method = c("ML"),optim.control = list(maxit = 300000 )) 
        rho_firms[jj] = m1$coef[1]
       
    } 
  }
    
  
  zzz = yrho_firms
  print_set1  =  c(mean(zzz,na.rm=T),sd(zzz,na.rm=T),skewness(zzz,na.rm=T),kurtosis(zzz,na.rm=T),quantile(zzz  ,c(0.05,0.25,0.5,0.75,0.95) ,na.rm=T)
                   ,  mean(zzz<0,na.rm=T),mean(zzz==0,na.rm=T),mean(zzz>0,na.rm=T)) 
  
  
  zzz = ((yrho_firms*(est$no_obs_deflated-1)+1)/(est$no_obs_deflated-4)) 
  print_set2  =  c(mean(zzz,na.rm=T),sd(zzz,na.rm=T),skewness(zzz,na.rm=T),kurtosis(zzz,na.rm=T),quantile(zzz  ,c(0.05,0.25,0.5,0.75,0.95) ,na.rm=T)
                   ,  mean(zzz<0,na.rm=T),mean(zzz==0,na.rm=T),mean(zzz>0,na.rm=T)) 
  
  zzz = rho_firms
  print_set3  =  c(mean(zzz,na.rm=T),sd(zzz,na.rm=T),skewness(zzz,na.rm=T),kurtosis(zzz,na.rm=T),quantile(zzz  ,c(0.05,0.25,0.5,0.75,0.95) ,na.rm=T)
                   ,  mean(zzz<0,na.rm=T),mean(zzz==0,na.rm=T),mean(zzz>0,na.rm=T)) 
  
  zzz = ((rho_firms*(est$no_obs_deflated-1)+1)/(est$no_obs_deflated-4)) 
  print_set4  =  c(mean(zzz,na.rm=T),sd(zzz,na.rm=T),skewness(zzz,na.rm=T),kurtosis(zzz,na.rm=T),quantile(zzz  ,c(0.05,0.25,0.5,0.75,0.95) ,na.rm=T)
                   ,  mean(zzz<0,na.rm=T),mean(zzz==0,na.rm=T),mean(zzz>0,na.rm=T)) 

  sum(zzz*est$no_obs_deflated/sum(est$no_obs_deflated,na.rm=T),na.rm=T)
  # weighted average is also similar
  
  zzz = est$no_obs   
  print_set5 = c(mean(zzz,na.rm=T),sd(zzz,na.rm=T),skewness(zzz,na.rm=T),kurtosis(zzz,na.rm=T),quantile(zzz  ,c(0.05,0.25,0.5,0.75,0.95) ,na.rm=T)
                  ,NA, NA, NA) 
  
  print_set= matrix(rbind(print_set1, print_set2 , print_set3, print_set4 , print_set5 ) ,nrow=5)
  print(cbind(round(print_set[,1],4), round(print_set[,2],3),round(print_set[,3:4],2),
              round(print_set[,5:9],3), round(100*print_set[,10:12],1)    ))
  
  print(format(sum(est$no_obs_mu_sd1 ,na.rm=T), big.mark = "," ))
  
  sample.dataframe = cbind(  print_set[,1] ,   print_set[,2] ,print_set[,3:4],
                             print_set[,5:9], NA,  print_set[,c(10,11,12)]     )
  
  
  sample.dataframe = data.frame( sample.dataframe)
  row.names(sample.dataframe) = c("AR-coef(y)","bias-adjusted AR-coef(y)", "AR-coef(FE)","bias-adjusted AR-coef(FE)", "#obs")
  colnames(sample.dataframe) = c("Mean","S.D","Skew","Kurt","5%","25%","50%","75%","95%","AR coef","(-)","0","(+)")
  
  write.csv(x = sample.dataframe, file = "Table1B_firmlevel.csv" )
  
   
  # drop at 1% and 99% and drop firms with <20
  data_treat_history2 = rbind(data_treat_history, c( sum( tempidx), length(firm_TS_data$no_obs_deflated)    ))
  print(data_treat_history2 )
  print(sprintf("%d firm-quarter observations with %d firms and %d quarters"
                ,sum(sum(est$no_obs,na.rm=T)),sum(!is.na(est$no_obs)),dim(qtrsumstat$ye_stat)[2] ) ) 

  
  
}

 




