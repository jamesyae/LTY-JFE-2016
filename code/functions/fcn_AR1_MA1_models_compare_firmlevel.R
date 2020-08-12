fcn_AR1_MA1_models_compare_firmlevel <- function( scaleVset, option ) {  

  filename = fcn_filename_maker(scaleVset=scaleVset, option=option )
  load( file = filename)
 
  firm_begin_deflated =  firm_TS_data$firm_begin_deflated
  firm_end_deflated =  firm_TS_data$firm_end_deflated
  
  ye_firm_deflated  =  firm_TS_data$ye_firm_deflated 
  yf_firm_deflated  =  firm_TS_data$yf_firm_deflated 
  
  
no_firms_deflated = length(firm_begin_deflated)
temps = rep(NA,no_firms_deflated )
ar1= list( aic=temps, loglkhd=temps,R2=temps,mu=temps,phi=temps,sig.e=temps, R2KF=temps , SE =temps) 
ma1=  list( aic=temps, loglkhd=temps,R2=temps,mu=temps,matheta=temps,sig.e=temps, R2KF=temps, SE =temps) 
arma11=  list( aic=temps, loglkhd=temps,R2=temps,mu=temps,phi=temps,matheta=temps,sig.e=temps, R2KF=temps, SEar =temps, SEma =temps) 
arma11raw=  list( aic=temps, loglkhd=temps,R2=temps,mu=temps,phi=temps,matheta=temps,sig.e=temps, R2KF=temps)
  feR2 = temps 
  ar1KF= list( aic=temps, loglkhd=temps,R2=temps,mu=temps,phi=temps,sig.e=temps, R2KF=temps , SE =temps) 
  arma11KF= list( aic=temps, loglkhd=temps,R2=temps,mu=temps,phi=temps,sig.e=temps, R2KF=temps , SE =temps) 
  ma1KF= list( aic=temps, loglkhd=temps,R2=temps,mu=temps,phi=temps,sig.e=temps, R2KF=temps , SE =temps) 
  
  
for (jj in seq(  1 ,no_firms_deflated))
{  
      if (!is.na(firm_begin_deflated[jj])) {
      firm_idx_temp = firm_begin_deflated[jj]:firm_end_deflated[jj] 
      yyy1  =  ye_firm_deflated[firm_idx_temp]    
      yyy2  =  yf_firm_deflated[firm_idx_temp]  
     
         if (   !(jj %in% c(0)  ) ) {          
          
          m1=arima(yyy1, order=c(1,0,0), method = c("ML"),optim.control = list(maxit = 300000 ))
          ar1$aic[jj] =  m1$aic
          ar1$loglik[jj] =m1$loglik
          ar1$R2[jj] = 1-m1$sigma2/var( yyy1 ,na.rm=T)
          ar1$mu[jj] = m1$coef[2] #arima produces mu, not intercept.
          ar1$phi[jj] =m1$coef[1]  
          ar1$sig.e[jj] = sqrt(m1$sigma2)    
          ar1$SE[jj] = sqrt(m1$var.coef[1,1])
          
          theta = list(A=matrix(m1$coef[2]), H= matrix(1), F=matrix(m1$coef[1]),  Q=matrix(m1$sigma2), R=matrix(0))
          feVar = var(yyy1 - as.vector(fcn_KF_general(theta, Y=matrix(yyy1,ncol=1), xt=matrix(rep(1,length(yyy1)),ncol=1))$ypred),na.rm=T)
          ar1$R2KF[jj] = 1-feVar/var( yyy1 ,na.rm=T) #OOS-R2 given parameter
          
          m1=arima(yyy1, order=c(0,0,1), method = c("ML"),optim.control = list(maxit = 300000 ))
          ma1$aic[jj] =  m1$aic
          ma1$loglik[jj] = m1$loglik
          ma1$R2[jj] = 1-m1$sigma2/var( yyy1 ,na.rm=T)
          ma1$mu[jj] = m1$coef[2]  
          ma1$matheta[jj] = m1$coef[1]  
          ma1$sig.e[jj] = sqrt(m1$sigma2)  
          ma1$SE[jj] = sqrt(m1$var.coef[1,1])

          theta = list(A=matrix(m1$coef[2]), H= matrix(c(1,m1$coef[1]),2,1), F=matrix(c(0,1,0,0),2,2),  Q=matrix(c(m1$sigma2,0,0,0),2,2), R=matrix(0))
          feVar = var(yyy1 - as.vector(fcn_KF_general(theta, Y=matrix(yyy1,ncol=1), xt=matrix(rep(1,length(yyy1)),ncol=1))$ypred),na.rm=T)
          ma1$R2KF[jj] = 1-feVar/var( yyy1 ,na.rm=T) #OOS-R2 given parameter
          
          m1=arima(yyy1, order=c(1,0,1), method = c("ML"),optim.control = list(maxit = 300000 ))
          arma11$aic[jj] =  m1$aic
          arma11$loglik[jj] =m1$loglik  
          arma11$R2[jj] = 1-m1$sigma2/var( yyy1 ,na.rm=T)
          arma11$mu[jj] = m1$coef[3] #arima produces mu, not intercept.
          arma11$phi[jj] =m1$coef[1]  
          arma11$matheta[jj] = m1$coef[2]
          arma11$sig.e[jj] = sqrt(m1$sigma2)     
          arma11$SEar[jj] = sqrt(m1$var.coef[1,1])
          arma11$SEma[jj] = sqrt(m1$var.coef[2,2])
          
          theta = list(A=matrix(m1$coef[3]), H= matrix(c(1,0),2,1), F=matrix(c(m1$coef[1],0,m1$coef[2],0),2,2),  Q=matrix(rep(1,4),2,2)*m1$sigma2, R=matrix(0))
          feVar = var(yyy1 - as.vector(fcn_KF_general(theta, Y=matrix(yyy1,ncol=1), xt=matrix(rep(1,length(yyy1)),ncol=1))$ypred),na.rm=T)
          arma11$R2KF[jj] = 1-feVar/var( yyy1 ,na.rm=T) #OOS-R2 given parameter

          
          arma11raw$aic[jj] =  arma11$aic[jj]
          arma11raw$loglik[jj] = arma11$loglik[jj]
          arma11raw$R2[jj] = arma11$R2[jj]
          arma11raw$mu[jj] = arma11$mu[jj] #arima produces mu, not intercept.
          arma11raw$phi[jj] = arma11$phi[jj] 
          arma11raw$matheta[jj] =  arma11$matheta[jj]
          arma11raw$sig.e[jj] =  arma11$sig.e[jj]     
          arma11raw$R2KF[jj] =  arma11$R2KF[jj] #OOS-R2 given parameter
          
           
          if  (arma11$R2KF[jj]<ar1$R2KF[jj])
          {
            arma11$aic[jj] =  ar1$aic[jj]+2
            arma11$loglik[jj] = ar1$loglik[jj]
            arma11$R2[jj] = ar1$R2[jj]
            arma11$mu[jj] = ar1$mu[jj]
            arma11$phi[jj] = ar1$phi[jj]
            arma11$matheta[jj] =0
            arma11$sig.e[jj] =  ar1$sig.e[jj]
            arma11$R2KF[jj] =  ar1$R2KF[jj] #OOS-R2 given parameter
            
          }
          
          
          m1=fcn_KF_AR1_para_est(yyy1)
          ar1KF$aic[jj] =  m1$aic
          ar1KF$loglik[jj] =m1$loglkhd
          ar1KF$R2[jj] =  m1$R2
          ar1KF$mu[jj] = m1$par[1]  
          ar1KF$phi[jj] =m1$par[2]  
          ar1KF$sig.e[jj] = sqrt(m1$par[3])    
          #ar1KF$SE[jj] = sqrt(m1$var.coef[2,2])     
          
          #theta = list(A=matrix(m1$par[1]), H= matrix(1), F=matrix(m1$par[2]),  Q=matrix(m1$par[3]), R=matrix(0))
          theta = list(A=matrix(m1$par[1]), H= matrix(c(1,0),2,1), F=matrix(c(m1$par[2],0,0,0),2,2),  Q=matrix(rep(1,4),2,2)*m1$par[4], R=matrix(0))          
          feVar = var(yyy1 - as.vector(fcn_KF_general(theta, Y=matrix(yyy1,ncol=1), xt=matrix(rep(1,length(yyy1)),ncol=1))$ypred),na.rm=T)
          ar1KF$R2KF[jj] = 1-feVar/var( yyy1 ,na.rm=T) #OOS-R2 given parameter
          
        
          m1=fcn_KF_MA1_para_est(yyy1)
          ma1KF$aic[jj] =  m1$aic
          ma1KF$loglik[jj] =m1$loglkhd
          ma1KF$R2[jj] =  m1$R2
          ma1KF$mu[jj] = m1$par[1]   
          ma1KF$matheta[jj] =m1$par[2]  
          ma1KF$sig.e[jj] = sqrt(m1$par[3])    
          #ar1KF$SE[jj] = sqrt(m1$var.coef[2,2])     
          
          theta = list(A=matrix(m1$par[1]), H= matrix(c(1,0),2,1), F=matrix(c(0,0,m1$par[2],0),2,2),  Q=matrix(rep(1,4),2,2)*m1$par[3], R=matrix(0))
          feVar = var(yyy1 - as.vector(fcn_KF_general(theta, Y=matrix(yyy1,ncol=1), xt=matrix(rep(1,length(yyy1)),ncol=1))$ypred),na.rm=T)
          ma1KF$R2KF[jj] = 1-feVar/var( yyy1 ,na.rm=T) #OOS-R2 given parameter
          
          
          m1=fcn_KF_ARMA11_para_est(yyy1)
          arma11KF$aic[jj] =  m1$aic
          arma11KF$loglik[jj] =m1$loglkhd
          arma11KF$R2[jj] =  m1$R2
          arma11KF$mu[jj] = m1$par[1]  
          arma11KF$phi[jj] =m1$par[2]  
          arma11KF$matheta[jj] =m1$par[3]  
          arma11KF$sig.e[jj] = sqrt(m1$par[4])    
          #ar1KF$SE[jj] = sqrt(m1$var.coef[2,2])     
    
          theta = list(A=matrix(m1$par[1]), H= matrix(c(1,0),2,1), F=matrix(c(m1$par[2],0,m1$par[3],0),2,2),  Q=matrix(rep(1,4),2,2)*m1$par[4], R=matrix(0))
          feVar = var(yyy1 - as.vector(fcn_KF_general(theta, Y=matrix(yyy1,ncol=1), xt=matrix(rep(1,length(yyy1)),ncol=1))$ypred),na.rm=T)
          arma11KF$R2KF[jj] = 1-feVar/var( yyy1 ,na.rm=T) #OOS-R2 given parameter
          
          
          feR2[jj] = 1-var(yyy1-yyy2,na.rm=T)/var(yyy1,na.rm=T)
          
         }       
      }
      
  if (jj %% round(no_firms_deflated/100) == 0) 
  {
    print(jj/no_firms_deflated)
  }
  
} 
 
est = list(ar1 = ar1, ma1 = ma1  ,arma11=arma11,arma11raw=arma11raw,ar1KF=ar1KF,ma1KF=ma1KF,arma11KF=arma11KF,feR2= feR2  )
  
return(est)


}


