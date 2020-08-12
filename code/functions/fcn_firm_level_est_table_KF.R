fcn_firm_level_est_table_KF <- function( est) {  

 
  
  
  
  summary_set=matrix(rep(NA,8*6), nrow=6 )
    
  x=est$ar1KF$phi
  summary_set[1,1:2] = c(mean(x,na.rm=T) , median(x,na.rm=T))
  summary_set[2,1:2] = quantile(x, c(0.05, 0.95),na.rm=T)
  
  x=est$ar1KF$R2KF
  summary_set[1,5:6] = c(mean(x,na.rm=T) , median(x,na.rm=T))
  summary_set[2,5:6] = quantile(x, c(0.05, 0.95),na.rm=T)
     
  x=est$ar1KF$aic
  summary_set[1,7:8] = c(mean(x,na.rm=T) , median(x,na.rm=T))
  summary_set[2,7:8] = quantile(x, c(0.05, 0.95),na.rm=T)
  
  
  
  
  
  x=est$ma1KF$matheta
  summary_set[3,3:4] = c(mean(x,na.rm=T) , median(x,na.rm=T))
  summary_set[4,3:4] = quantile(x, c(0.05, 0.95),na.rm=T)
    
  x=est$ma1KF$R2KF
  summary_set[3,5:6] = c(mean(x,na.rm=T) , median(x,na.rm=T))
  summary_set[4,5:6] = quantile(x, c(0.05, 0.95),na.rm=T)
  
  x=est$ma1KF$aic
  summary_set[3,7:8] = c(mean(x,na.rm=T) , median(x,na.rm=T))
  summary_set[4,7:8] = quantile(x, c(0.05, 0.95),na.rm=T)
  
  
    
  tempidx = est$arma11KF$loglik > est$ar1KF$loglik
  
  x = est$arma11KF$phi*tempidx   + est$ar1KF$phi*(!tempidx)
  summary_set[5,1:2] = c(mean(x,na.rm=T) , median(x,na.rm=T))
  summary_set[6,1:2] = quantile(x, c(0.05, 0.95),na.rm=T)  
    
  
  x = est$arma11KF$matheta*tempidx   + est$ar1KF$matheta*(!tempidx)
  summary_set[5,3:4] = c(mean(x,na.rm=T) , median(x,na.rm=T))
  summary_set[6,3:4] = quantile(x, c(0.05, 0.95),na.rm=T)
  
  x = est$arma11KF$R2KF*tempidx   + est$ar1KF$R2KF*(!tempidx)
  summary_set[5,5:6] = c(mean(x,na.rm=T) , median(x,na.rm=T))
  summary_set[6,5:6] = quantile(x, c(0.05, 0.95),na.rm=T)
    
  x = est$arma11KF$aic*tempidx   + (2+est$ar1KF$aic)*(!tempidx)
  summary_set[5,7:8] = c(mean(x,na.rm=T) , median(x,na.rm=T))
  summary_set[6,7:8] = quantile(x, c(0.05, 0.95),na.rm=T)

  aicarma11 = est$arma11KF$aic*tempidx   + (2+est$ar1KF$aic)*(!tempidx)
  
  
  
  print(round(summary_set,3))
  
  summary_set_all=summary_set
  
  summary_set=matrix(rep(NA,4*1), nrow=2 )
  x=est$feR2
  summary_set[1,1:2] = c(mean(x,na.rm=T) , median(x,na.rm=T))
  summary_set[2,1:2] = quantile(x, c(0.05, 0.95),na.rm=T)
  print(round(summary_set,3))
  
  summary_set_all= rbind(summary_set_all , cbind( matrix(rep(NA,8),nrow=2) , summary_set, matrix(rep(NA,4),nrow=2)   )  )
  
  
  ww = as.numeric(table(apply(cbind(est$ar1KF$aic,est$ma1KF$aic,aicarma11  ),1 ,which.min)))
  ww = ww/sum(ww)*100
  #### ar=arma case carefeul
   
  
  
  
  summary_set_all=cbind(summary_set_all,c(ww[1] ,NA,ww[2],NA,ww[3],NA,NA,NA)  )
  
  
  sample.dataframe = data.frame(  summary_set_all)
  row.names(sample.dataframe) = c("AR(1)","","MA(1)"," ","ARMA(1,1)","  ","Analysts median","forecasts yhat")
  colnames(sample.dataframe) = c("phi","","theta","","Pseudo_R2","","AIC","","Lowest AIC(%)")
  
  write.csv(x = sample.dataframe, file = "Table2A.csv" )
  
  
  
            
            ##### TABLE2A in Juhani's pdf
  
  summary_set=matrix(rep(NA,17*5), ncol=5 )
  

  
  x=est$ar1KF$phi
  summary_set[1,1 ] = mean(x,na.rm=T)  
  summary_set[3:5,1] = quantile(x, c(0.05, 0.5, 0.95),na.rm=T)
  
  x=est$ar1KF$R2KF
  summary_set[13,1 ] = mean(x,na.rm=T)  
  summary_set[15:17,1] = quantile(x, c(0.05, 0.5, 0.95),na.rm=T)
 
  x=est$ma1KF$matheta
  summary_set[7,2 ] = mean(x,na.rm=T)  
  summary_set[9:11,2] = quantile(x, c(0.05, 0.5, 0.95),na.rm=T)
  
  x=est$ma1KF$R2KF
  summary_set[13,2] = mean(x,na.rm=T)  
  summary_set[15:17,2] = quantile(x, c(0.05, 0.5, 0.95),na.rm=T)
  
  tempidx = est$arma11KF$loglik > est$ar1KF$loglik
  
  x = est$arma11KF$phi*tempidx   + est$ar1KF$phi*(!tempidx)  
  summary_set[1,3 ] = mean(x,na.rm=T)  
  summary_set[3:5,3] = quantile(x, c(0.05, 0.5, 0.95),na.rm=T)
  
  x = est$arma11KF$matheta*tempidx   +0*(!tempidx)  
  summary_set[7,3 ] = mean(x,na.rm=T)  
  summary_set[9:11,3] = quantile(x, c(0.05, 0.5, 0.95),na.rm=T)
   
  x = est$arma11KF$R2KF*tempidx   + est$ar1KF$R2KF*(!tempidx)  
  summary_set[13,3 ] = mean(x,na.rm=T)  
  summary_set[15:17,3] = quantile(x, c(0.05, 0.5, 0.95),na.rm=T)
  
  
  x = est$arma11$SEma  
  summary_set[7,4 ] = mean(x,na.rm=T)  
  summary_set[9:11,4] = quantile(x, c(0.05, 0.5, 0.95),na.rm=T)
  
  
  x = est$feR2
  summary_set[13,5] = mean(x,na.rm=T)  
  summary_set[15:17,5] = quantile(x, c(0.05, 0.5, 0.95),na.rm=T)
 
  ww = as.numeric(table(apply(cbind(est$ar1KF$aic,est$ma1KF$aic,aicarma11  ),1 ,which.min)))
  ww = ww/sum(ww)*100
  #### ar=arma case carefeul
   
  summary_set =rbind(summary_set ,c(ww[1] , ww[2],ww[3],NA,NA)  )
  
  
  
  sample.dataframe = data.frame( summary_set )
  row.names(sample.dataframe) = c("Mean","Percentiles","'  5%","' 50%","' 95%", "",
                                  "Mean ","Percentiles ","'  5% ","' 50% ","' 95% ", " ",
                                  "Mean  ","Percentiles  ","'  5%  ","' 50%  ","' 95%  ", "chosen by AIC (%)"
                                  )
  colnames(sample.dataframe) = c("AR(1)","MA(1)","ARMA(1,1)","SE(theta) in ARMA(1,1)","Analysts forecasts")
  
  
  
  write.csv(x = sample.dataframe, file = "Table2A_KF.csv" )
  
  
  
  
          
            ##### TABLE2A-1 in Juhani's pdf
     #       include simulation result for AR(1) fitted by ARMA(1,1)
     #       include SE(matheta in ARMA) mean, ,median, quantile
            
            
  
  
  
  summary_set=matrix(rep(NA,4*5), nrow=5 )
  x=est$ar1$R2KF-est$ma1$R2KF
  summary_set[1,1:2] = c(mean(x,na.rm=T) , median(x,na.rm=T))
  summary_set[2,1:2] = quantile(x, c(0.05, 0.95),na.rm=T)
  
  tstat =mean(x,na.rm=T)/sd(x,na.rm=T)*sqrt(sum(!is.na(x)))
  pval = ( 1-pt(abs(tstat),sum(!is.na(x))-1) )/2
  summary_set[3,1] = mean(x>0,na.rm=T)
  summary_set[4,1] = tstat 
  summary_set[5,1] = pval
  
  x=est$ar1$aic - est$ma1$aic
  summary_set[1,3:4] = c(mean(x,na.rm=T) , median(x,na.rm=T))
  summary_set[2,3:4] = quantile(x, c(0.05, 0.95),na.rm=T)
 
  tstat =mean(x,na.rm=T)/sd(x,na.rm=T)*sqrt(sum(!is.na(x)))
  pval = ( 1-pt(abs(tstat),sum(!is.na(x))-1) )/2
  summary_set[3,3] = mean(x<0,na.rm=T)
  summary_set[4,3] = tstat 
  summary_set[5,3] = pval
  
  print(round(summary_set,3))
  
  
  summary_set_all=   cbind( matrix(rep(NA,20),nrow=5) , summary_set )  
  
  
  summary_set=matrix(rep(NA,4*5), nrow=5 )
  x=est$arma11$R2KF-est$ar1$R2KF
  summary_set[1,1:2] = c(mean(x,na.rm=T) , median(x,na.rm=T))
  summary_set[2,1:2] = quantile(x, c(0.05, 0.95),na.rm=T)
  
  tstat =mean(x,na.rm=T)/sd(x,na.rm=T)*sqrt(sum(!is.na(x)))
  pval = ( 1-pt(abs(tstat),sum(!is.na(x))-1) )/2
  summary_set[3,1] = mean(x>0,na.rm=T)
  summary_set[4,1] = tstat 
  summary_set[5,1] = pval
  
  x=est$arma11$aic - est$ar1$aic
  summary_set[1,3:4] = c(mean(x,na.rm=T) , median(x,na.rm=T))
  summary_set[2,3:4] = quantile(x, c(0.05, 0.95),na.rm=T)
  
  tstat =mean(x,na.rm=T)/sd(x,na.rm=T)*sqrt(sum(!is.na(x)))
  pval = ( 1-pt(abs(tstat),sum(!is.na(x))-1) )/2
  summary_set[3,3] = mean(x<0,na.rm=T)
  summary_set[4,3] = tstat 
  summary_set[5,3] = pval
  
  print(round(summary_set,3))
 
  
  
  summary_set_all= rbind(summary_set_all , cbind( matrix(rep(NA,20),nrow=5) , summary_set )  )
  
  

  

  
  sample.dataframe = data.frame(  summary_set_all)
  row.names(sample.dataframe) = c("AR(1)_MA(1)","  ","% ","t-stat ","p-val ","ARMA(1,1)_AR(1)","","%","t-stat","p-val")
  colnames(sample.dataframe) = c("phi","","theta","","Pseudo_R2","","AIC","")
  
  write.csv(x = sample.dataframe, file = "Table2B.csv" )
  
  
        ##### TABLE2B in Juhani's pdf
     #   Add t-stat for the proportion of positive (be careful about the same R2 in AR and ARMA)
          
  summary_set=matrix(rep(NA,11*6), nrow=11 )
  
  x = est$ar1KF$R2KF  - est$ma1KF$R2KF
  summary_set[1,1 ] = c( mean(x,na.rm=T)  )
  tstat =mean(x,na.rm=T)/sd(x,na.rm=T)*sqrt(sum(!is.na(x)))
  summary_set[2,1 ] = tstat
  summary_set[1,2:4] = quantile(x, c(0.05, 0.5, 0.95),na.rm=T)
  summary_set[1,6] = mean(x>0, na.rm=T)
  tstat =(mean(x>0,na.rm=T)-0.5)/sqrt(mean(x>0, na.rm=T)*(1-mean(x>0, na.rm=T))  )*sqrt(sum(!is.na(x)))
  summary_set[2,6] = tstat
  
  x = est$ar1KF$aic  - est$ma1KF$aic
  summary_set[4,1 ] = c( mean(x,na.rm=T)  )
  tstat =mean(x,na.rm=T)/sd(x,na.rm=T)*sqrt(sum(!is.na(x)))
  summary_set[5,1 ] = tstat
  summary_set[4,2:4] = quantile(x, c(0.05, 0.5, 0.95),na.rm=T)
  summary_set[4,6] = mean(x>0, na.rm=T)
  tstat =(mean(x>0,na.rm=T)-0.5)/sqrt(mean(x>0, na.rm=T)*(1-mean(x>0, na.rm=T))  )*sqrt(sum(!is.na(x)))
  summary_set[5,6] = tstat
  
  
  tempidx = est$arma11KF$loglik > est$ar1KF$loglik 
 
  x = est$arma11KF$R2KF*tempidx   + est$ar1KF$R2KF*(!tempidx)  - est$ar1KF$R2KF
  summary_set[7,1 ] = c( mean(x,na.rm=T)  )
  tstat =mean(x,na.rm=T)/sd(x,na.rm=T)*sqrt(sum(!is.na(x)))
  summary_set[8,1 ] = tstat
  summary_set[7,2:4] = quantile(x, c(0.05, 0.5, 0.95),na.rm=T)
  summary_set[7,6] = mean(x>0, na.rm=T)
  tstat =(mean(x>0,na.rm=T)-0.5)/sqrt(mean(x>0, na.rm=T)*(1-mean(x>0, na.rm=T))  )*sqrt(sum(!is.na(x)))
  summary_set[8,6] = tstat
  
  
  tempidx = est$arma11KF$loglik > est$ar1KF$loglik 
 
  x = est$arma11KF$aic*tempidx   + (est$ar1KF$aic+2)*(!tempidx)  - est$ar1KF$aic  
  summary_set[10,1 ] = c( mean(x,na.rm=T)  )
  tstat =mean(x,na.rm=T)/sd(x,na.rm=T)*sqrt(sum(!is.na(x)))
  summary_set[11,1 ] = tstat
  summary_set[10,2:4] = quantile(x, c(0.05, 0.5, 0.95),na.rm=T)
  summary_set[10,6] = mean(x>0, na.rm=T)
  tstat =(mean(x>0,na.rm=T)-0.5)/sqrt(mean(x>0, na.rm=T)*(1-mean(x>0, na.rm=T))  )*sqrt(sum(!is.na(x)))
  summary_set[11,6] = tstat
   
  
  sample.dataframe = data.frame(  summary_set )
  row.names(sample.dataframe) = c("$R^2$",""," ","AIC","  ","   ","$R^2$ ","    ","     ","AIC ","       ")
  colnames(sample.dataframe) = c("Mean","'  5%","' 50%","' 95%","","positive")
                                 
  write.csv(x = sample.dataframe, file = "Table2B_KF.csv" )
  
    
  
  
  



}


