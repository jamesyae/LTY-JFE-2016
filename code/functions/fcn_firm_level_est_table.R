fcn_firm_level_est_table <- function( est) {  

 
  
  
  
  summary_set=matrix(rep(NA,8*6), nrow=6 )
    
  x=est$ar1$phi
  summary_set[1,1:2] = c(mean(x,na.rm=T) , median(x,na.rm=T))
  summary_set[2,1:2] = quantile(x, c(0.05, 0.95),na.rm=T)
  
  x=est$ar1$R2KF
  summary_set[1,5:6] = c(mean(x,na.rm=T) , median(x,na.rm=T))
  summary_set[2,5:6] = quantile(x, c(0.05, 0.95),na.rm=T)
     
  x=est$ar1$aic
  summary_set[1,7:8] = c(mean(x,na.rm=T) , median(x,na.rm=T))
  summary_set[2,7:8] = quantile(x, c(0.05, 0.95),na.rm=T)
  
  
  
  
  
  x=est$ma1$matheta
  summary_set[3,3:4] = c(mean(x,na.rm=T) , median(x,na.rm=T))
  summary_set[4,3:4] = quantile(x, c(0.05, 0.95),na.rm=T)
    
  x=est$ma1$R2KF
  summary_set[3,5:6] = c(mean(x,na.rm=T) , median(x,na.rm=T))
  summary_set[4,5:6] = quantile(x, c(0.05, 0.95),na.rm=T)
  
  x=est$ma1$aic
  summary_set[3,7:8] = c(mean(x,na.rm=T) , median(x,na.rm=T))
  summary_set[4,7:8] = quantile(x, c(0.05, 0.95),na.rm=T)
  
  
    
  
  x=est$arma11$phi
  summary_set[5,1:2] = c(mean(x,na.rm=T) , median(x,na.rm=T))
  summary_set[6,1:2] = quantile(x, c(0.05, 0.95),na.rm=T)  
    
  
  x=est$arma11$matheta
  summary_set[5,3:4] = c(mean(x,na.rm=T) , median(x,na.rm=T))
  summary_set[6,3:4] = quantile(x, c(0.05, 0.95),na.rm=T)
  
  x=est$arma11$R2KF
  summary_set[5,5:6] = c(mean(x,na.rm=T) , median(x,na.rm=T))
  summary_set[6,5:6] = quantile(x, c(0.05, 0.95),na.rm=T)
    
  x=est$arma11$aic
  summary_set[5,7:8] = c(mean(x,na.rm=T) , median(x,na.rm=T))
  summary_set[6,7:8] = quantile(x, c(0.05, 0.95),na.rm=T)

  
  
  
  
  print(round(summary_set,3))
  
  summary_set_all=summary_set
  
  summary_set=matrix(rep(NA,4*1), nrow=2 )
  x=est$feR2
  summary_set[1,1:2] = c(mean(x,na.rm=T) , median(x,na.rm=T))
  summary_set[2,1:2] = quantile(x, c(0.05, 0.95),na.rm=T)
  print(round(summary_set,3))
  
  summary_set_all= rbind(summary_set_all , cbind( matrix(rep(NA,8),nrow=2) , summary_set, matrix(rep(NA,4),nrow=2)   )  )
  
  
  summary_set_all=cbind(summary_set_all,  )
  
  
  sample.dataframe = data.frame(  summary_set_all)
  row.names(sample.dataframe) = c("AR(1)","","MA(1)"," ","ARMA(1,1)","  ","Analysts median","forecasts yhat")
  colnames(sample.dataframe) = c("phi","","theta","","Pseudo_R2","","AIC","")
  
  write.csv(x = sample.dataframe, file = "Table2A.csv" )
  
  
  
  
  
  
  
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
  
  
  
  
  
}


