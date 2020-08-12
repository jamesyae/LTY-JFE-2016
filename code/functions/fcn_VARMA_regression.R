fcn_VARMA_regression <- function(yt, init) {  
 
   
  ye1 = yt[1,2:(dim(yt)[2])]   
  ye0 = yt[1,1:(dim(yt)[2]-1)] 
  yf1 = yt[2,2:(dim(yt)[2])]   
  yf0 = yt[2,1:(dim(yt)[2]-1)]  
  
  m1 = arima(yt[1,], c(1,0,1))  
  m2 = summary(lm(yf1 ~ yf0 + ye0 ))
  
  
  
  theta.reg=list()
  theta.reg$phi = as.numeric(m1$coef[1])
  theta.reg$sigmaa = as.numeric(sqrt(max(0,-m1$sigma2*m1$coef[2]/theta.reg$phi)))
  theta.reg$sigmae = as.numeric(sqrt(m1$sigma2 -(1+theta.reg$phi+theta.reg$phi^2)*theta.reg$sigmaa^2 ))
  theta.reg$phihat = as.numeric(m2$coef[2,1]+m2$coef[3,1])
  theta.reg$Khat = as.numeric(m2$coef[3,1]/theta.reg$phihat)
 
    e1t = m1$residuals
  e2t = as.numeric(c(NA, yf1 - m2$coef[1,1] - m2$coef[2,1]*yf0 - m2$coef[3,1]*ye0))
  
  theta.reg$what = cov(e1t,e2t,use="complete")/theta.reg$sigmae^2

  theta.reg$sigman = sqrt(var(e2t,na.rm=T)/theta.reg$what^2-theta.reg$sigmae^2)
 
 
  
  theta.reg$logsigmaa = log(theta.reg$sigmaa)
  theta.reg$logsigmae = log(theta.reg$sigmae)
  theta.reg$logsigman = log(theta.reg$sigman)  
  
  
  theta.reg$ARMAsigma = sqrt(m1$sigma2)
  theta.reg$MA =  m1$coef[2] 
  
  
  if (init == TRUE){ 
    theta.reg$sigmaa = NULL
    theta.reg$sigmae = NULL
    theta.reg$sigman = NULL
    theta.reg$mue=0
    theta.reg$muehat=0
    theta.reg$ARMAsigma = NULL
    theta.reg$MA = NULL
  }
    
  
  return(theta.reg)
  
  
  
  
}