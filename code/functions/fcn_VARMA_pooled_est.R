fcn_VARMA_pooled_est <- function( scaleVset, option) {  

  filename = fcn_filename_maker(scaleVset=scaleVset, option=option )
  load(file=filename)
  
  temps = NA
  ar1= list( aic=temps, loglkhd=temps,R2=temps,mu=temps,phi=temps,sig.e=temps, R2KF=temps, var.coef=temps ) 
  ma1=  list( aic=temps, loglkhd=temps,R2=temps,mu=temps,matheta=temps,sig.e=temps, R2KF=temps, var.coef=temps) 
  ar1_sd1= list( aic=temps, loglkhd=temps,R2=temps,mu=temps,phi=temps,sig.e=temps, R2KF=temps, var.coef=temps ) 
  ma1_sd1=  list( aic=temps, loglkhd=temps,R2=temps,mu=temps,matheta=temps,sig.e=temps, R2KF=temps, var.coef=temps) 
    
   
    
  ###################################
  ####### Pooled data (normalized) #######
  ###################################
  pdataset = list( y=firm_TS_data$ye_firm_deflated_mu_sd1, 
                   begin_idx =firm_TS_data$firm_begin_deflated, end_idx = firm_TS_data$firm_end_deflated)
  yyy1 = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
  pdataset = list( y=firm_TS_data$yf_firm_deflated_mu_sd1, 
                   begin_idx =firm_TS_data$firm_begin_deflated, end_idx = firm_TS_data$firm_end_deflated)
  yyy2 = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
  
  yt = matrix(rbind(yyy1,yyy2),nrow=2) 
  
  ### Multivariate regression OLS estimator with Hotelling t
  m1 = arima(yyy1, c(1,0,1))  
  m2 = summary(lm(yf1 ~ yf0 + ye0 ))
  
 
  
  ### Multivariate regression OLS estimator with Hotelling t  
  KFfit=fcn_KF_VARMA11_LTY_est( yt )
  KFfit$par["mue"]=0
  KFfit$par
  
  (KFfit$par)
  exp(KFfit$par)
  w = 1/exp(2*KFfit$par["logsigman"])/(1/exp(2*KFfit$par["logsigman"])+1/exp(2*KFfit$par["logsigmae"]))  
  estt.pis2 = 1/(1/exp(2*KFfit$par["logsigmae"])+1/exp(2*KFfit$par["logsigman"]));
  a = 1/exp(2*KFfit$par["logsigmaa"]) ;
  b = 1-KFfit$par["phihat"]^2 - estt.pis2/exp(2*KFfit$par["logsigmaa"])  ;
  c = -estt.pis2; 
  estt.pit1t2 = ( -b + sqrt(   b^2 - 4*a*c  )   ) /a / 2  ;
  Kx = estt.pit1t2 /(exp(2*KFfit$par["logsigmaa"]) + estt.pit1t2  )    ;
  
  
  
  
  KFfit2=fcn_KF_VARMA11_LTY_2signal_est(yt )
  
  (KFfit2$par)
  exp(KFfit2$par)
  w = 1/exp(2*KFfit2$par["logsigman"])/(1/exp(2*KFfit2$par["logsigman"])+1/exp(2*KFfit2$par["logsigmae"]))
  w
  
  estt.pis2 = 1/(1/exp(2*KFfit2$par["logsigmae"])+1/exp(2*KFfit2$par["logsigman"]));
  a = 1/exp(2*KFfit2$par["logsigmaa"]) ;
  b = 1-KFfit2$par["phihat"]^2 - estt.pis2/exp(2*KFfit2$par["logsigmaa"])  ;
  c = -estt.pis2; 
  estt.pit1t2 = ( -b + sqrt(   b^2 - 4*a*c  )   ) /a / 2  ;
  Kx = estt.pit1t2 /(exp(2*KFfit2$par["logsigmaa"]) + estt.pit1t2  )    ;
  Kx  
  
  
  
  
est = list(ar1 = ar1, ma1 = ma1 ,ar1_sd1 = ar1_sd1, ma1_sd1 = ma1_sd1   )
return(est)


}


