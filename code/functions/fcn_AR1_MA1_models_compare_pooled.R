fcn_AR1_MA1_models_compare_pooled <- function( scaleVset, option, fixedeffect) {  

  filename = fcn_filename_maker(scaleVset=scaleVset, option=option )
  load(file=filename) 
  est = firm_TS_data
  
  temps = NA
  ar1= list( aic=temps, loglkhd=temps,R2=temps,mu=temps,phi=temps,sig.e=temps, R2KF=temps, var.coef=temps ) 
  ma1=  list( aic=temps, loglkhd=temps,R2=temps,mu=temps,matheta=temps,sig.e=temps, R2KF=temps, var.coef=temps) 
  arma11= list( aic=temps, loglkhd=temps,R2=temps,mu=temps,matheta=temps,phi=temps,sig.e=temps, R2KF=temps, 
                var.coef=temps ,sigmaa=temps ,sigmae=temps) 
     
  
  #######################
  ####### Pooled #######
  #######################  

  yt= fcn_VARMA_data_make(scaleVset=scaleVset, option=option, fixedeffect= fixedeffect)
  yyy1 = yt[1,]
  
  # AR1
  m1=arima(yyy1, order=c(1,0,0), method = c("ML"),optim.control = list(maxit = 300000 ))
  
    ar1$aic =  m1$aic
    ar1$loglik =m1$loglik
    ar1$R2 = 1-m1$sigma2/var( yyy1 ,na.rm=T)
    ar1$mu = m1$coef[2] #arima produces mu, not intercept.
    ar1$phi =m1$coef[1]  
    ar1$sig.e = sqrt(m1$sigma2) 
    ar1$var.coef = m1$var.coef
    
    theta = list(A=matrix(m1$coef[2]), H= matrix(1), F=matrix(m1$coef[1]),  Q=matrix(m1$sigma2), R=matrix(0))
  
    m2 = fcn_KF_general(theta, Y=matrix(yyy1,ncol=1), xt=matrix(rep(1,length(yyy1)),ncol=1))
    feVar = var(yyy1 - as.vector(m2$ypred),na.rm=T)
    #ar1$loglik = -m2$negloglkhd
    ar1$R2KF = 1-feVar/var( yyy1 ,na.rm=T) #OOS-R2 given parameter
    
     
  # MA1 
  m1=arima(yyy1, order=c(0,0,1), method = c("ML"),optim.control = list(maxit = 300000 ))
    ma1$aic =  m1$aic
    ma1$loglik = m1$loglik
    ma1$R2 = 1-m1$sigma2/var( yyy1 ,na.rm=T)
    ma1$mu = m1$coef[2]  
    ma1$matheta = m1$coef[1]  
    ma1$sig.e = sqrt(m1$sigma2) 
    ma1$var.coef = m1$var.coef
    
    theta = list(A=matrix(m1$coef[2]), H= matrix(c(1,m1$coef[1]),2,1), F=matrix(c(0,1,0,0),2,2),  Q=matrix(c(m1$sigma2,0,0,0),2,2), R=matrix(0))
 
  m2 = fcn_KF_general(theta, Y=matrix(yyy1,ncol=1), xt=matrix(rep(1,length(yyy1)),ncol=1))
  feVar = var(yyy1 - as.vector(m2$ypred),na.rm=T)
  #ma1$loglik = -m2$negloglkhd
  ma1$R2KF = 1-feVar/var( yyy1 ,na.rm=T) #OOS-R2 given parameter
    
   
  # ARMA11
  m1=arima(yyy1, order=c(1,0,1), method = c("ML"),optim.control = list(maxit = 300000 ))
  
  arma11$aic =  m1$aic
  arma11$loglik =m1$loglik
  arma11$R2 = 1-m1$sigma2/var( yyy1 ,na.rm=T)
  arma11$mu = m1$coef[3] #arima produces mu, not intercept.
  arma11$phi = m1$coef[1]  
  arma11$matheta = m1$coef[2]    
  arma11$sig.e = sqrt(m1$sigma2) 
  arma11$var.coef = m1$var.coef
  
   
  arma11$sigmaa = as.numeric(sqrt(max(0,-m1$sigma2*m1$coef[2]/arma11$phi)))
  arma11$sigmae = as.numeric(sqrt(m1$sigma2 -(1+arma11$phi+arma11$phi^2)*arma11$sigmaa^2 ))
  
  theta = list(A=matrix(m1$coef[3]), H= matrix(1), F=matrix(m1$coef[1]),  Q=matrix(arma11$sigmae^2), R=matrix(arma11$sigmaa^2))
 
  m2 = fcn_KF_general(theta, Y=matrix(yyy1,ncol=1), xt=matrix(rep(1,length(yyy1)),ncol=1))
  feVar = var(yyy1 - as.vector(m2$ypred),na.rm=T)
  #arma11$loglik = -m2$negloglkhd
  arma11$R2KF = 1-feVar/var( yyy1 ,na.rm=T) #OOS-R2 given parameter
  
  
  yt= fcn_VARMA_data_make(scaleVset=scaleVset, option=option, fixedeffect=FALSE)
  yyy1 = yt[1,]
  yyy2 = yt[2,]
  feR2 = 1-var(yyy1-yyy2,na.rm=T)/var(yyy1,na.rm=T)
  
 
est = list(ar1 = ar1, ma1 = ma1 ,arma11 = arma11 ,feR2=feR2 )
return(est)


}


