fcn_pooled_est_table <- function( est) {  

  
  summary_set=matrix(rep(NA,8*4), nrow=4 )
    
  x=est$ar1$phi
  summary_set[1,1:2] = c(mean(x,na.rm=T) , median(x,na.rm=T))
  summary_set[2,1:2] = quantile(x, c(0.05, 0.95),na.rm=T)
  
  
  x=est$ar1$sig.e
  summary_set[1,3:4] = c(mean(x,na.rm=T) , median(x,na.rm=T))*100
  summary_set[2,3:4] = quantile(x, c(0.05, 0.95),na.rm=T)*100
  
  
  x=est$ar1$R2KF
  summary_set[1,5:6] = c(mean(x,na.rm=T) , median(x,na.rm=T))
  summary_set[2,5:6] = quantile(x, c(0.05, 0.95),na.rm=T)
  
   
  x=est$ar1$loglik
  summary_set[1,7:8] = c(mean(x,na.rm=T) , median(x,na.rm=T))
  summary_set[2,7:8] = quantile(x, c(0.05, 0.95),na.rm=T)
  
  
  x=est$ma1$matheta
  summary_set[3,1:2] = c(mean(x,na.rm=T) , median(x,na.rm=T))
  summary_set[4,1:2] = quantile(x, c(0.05, 0.95),na.rm=T)
  
  
  x=est$ma1$sig.e
  summary_set[3,3:4] = c(mean(x,na.rm=T) , median(x,na.rm=T))*100
  summary_set[4,3:4] = quantile(x, c(0.05, 0.95),na.rm=T)*100
  
  
  x=est$ma1$R2KF
  summary_set[3,5:6] = c(mean(x,na.rm=T) , median(x,na.rm=T))
  summary_set[4,5:6] = quantile(x, c(0.05, 0.95),na.rm=T)
  
  x=est$ma1$loglik
  summary_set[3,7:8] = c(mean(x,na.rm=T) , median(x,na.rm=T))
  summary_set[4,7:8] = quantile(x, c(0.05, 0.95),na.rm=T)
  
  
  print(round(summary_set,3))
  
  
  
  
  summary_set=matrix(rep(NA,4*4), nrow=4 )
  x=est$ar1$R2KF-est$ma1$R2KF
  summary_set[1,1:2] = c(mean(x,na.rm=T) , median(x,na.rm=T))
  summary_set[2,1:2] = quantile(x, c(0.05, 0.95),na.rm=T)
  
  tstat =mean(x,na.rm=T)/sd(x,na.rm=T)*sqrt(sum(!is.na(x)))
  pval = ( 1-pt(tstat,sum(!is.na(x))-1) )/2
  summary_set[3,1] = tstat 
  summary_set[4,1] = pval
  
  x=est$ar1$loglik - est$ma1$loglik
  summary_set[1,3:4] = c(mean(x,na.rm=T) , median(x,na.rm=T))
  summary_set[2,3:4] = quantile(x, c(0.05, 0.95),na.rm=T)
 
  tstat =mean(x,na.rm=T)/sd(x,na.rm=T)*sqrt(sum(!is.na(x)))
  pval = ( 1-pt(tstat,sum(!is.na(x))-1) )/2
  summary_set[3,3] = tstat 
  summary_set[4,3] = pval
  
  print(round(summary_set,3))
  
  
  
  summary_set=matrix(rep(NA,8*2), nrow=2 )
  
  x=est$arma11$phi
  summary_set[1,1:2] = c(mean(x,na.rm=T) , median(x,na.rm=T))
  summary_set[2,1:2] = quantile(x, c(0.05, 0.95),na.rm=T)
  
  
  x=est$arma11$matheta
  summary_set[1,3:4] = c(mean(x,na.rm=T) , median(x,na.rm=T))
  summary_set[2,3:4] = quantile(x, c(0.05, 0.95),na.rm=T)
  
  
  x=est$arma11$R2KF
  summary_set[1,5:6] = c(mean(x,na.rm=T) , median(x,na.rm=T))
  summary_set[2,5:6] = quantile(x, c(0.05, 0.95),na.rm=T)
  
  
  x=est$arma11$loglik
  summary_set[1,7:8] = c(mean(x,na.rm=T) , median(x,na.rm=T))
  summary_set[2,7:8] = quantile(x, c(0.05, 0.95),na.rm=T)
  
  print(round(summary_set,3))
 
  summary_set=matrix(rep(NA,4*1), nrow=2 )
  x=est$feR2
  summary_set[1,1:2] = c(mean(x,na.rm=T) , median(x,na.rm=T))
  summary_set[2,1:2] = quantile(x, c(0.05, 0.95),na.rm=T)
  print(round(summary_set,3))
  
}


