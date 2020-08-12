fcn_KF_AR1_para_est <- function(y) {  
  # Univariate KF
  
  
  arma21ss <- function( ct,ar1,   sigma) {
    Tt <- matrix(c(ar1, 0, 0, 0), ncol = 2)
    Zt <- matrix(c(1, 0), ncol = 2)
    ct <- matrix(ct)
    dt <- matrix(0, nrow = 2)
    GGt <- matrix(0)
    H <- matrix(c(1, 1), nrow = 2) * sigma
    HHt <- H %*% t(H)
    a0 <- c(0, 0)
    P0 <- matrix(solve( diag(rep(1,2^2)) - Tt%x%Tt )%*% matrix(HHt, 2^2,1),ncol=2)
    return(list(a0 = a0, P0 = P0, ct = ct, dt = dt, Zt = Zt, Tt = Tt, GGt = GGt, HHt = HHt))
  }
  
  KFobjective <- function(theta, yt) {  
    sp <- arma21ss(theta["ct"],theta["ar1"], theta["sigma"]) 
    ans <- fkf(a0 = sp$a0, P0 = sp$P0, dt = sp$dt, ct = sp$ct, Tt = sp$Tt,
               Zt = sp$Zt, HHt = sp$HHt, GGt = sp$GGt, yt =  matrix(yt,nrow=1))  
    return(-ans$logLik)
  }
  
  
  theta <- list( ct = mean(y,na.rm=T),  ar1 =  cor(y[1:(length(y)-1)],y[2:length(y)], use="complete.obs")
                 ,   sigma = sd(y,na.rm=T )*0.8)
  fit <- optim(theta, KFobjective, yt = y, control = list(maxit = 300000 ), hessian = FALSE)
  
  theta <- fit$par
  sp <- arma21ss(theta["ct"],theta["ar1"],  theta["sigma"]) 
  ans <- fkf(a0 = sp$a0, P0 = sp$P0, dt = sp$dt, ct = sp$ct, Tt = sp$Tt,
             Zt = sp$Zt, HHt = sp$HHt, GGt = sp$GGt, yt =  matrix(y ,nrow=1))  
  
  KFfit=c()
  KFfit$aic = 2*length(theta)-2*(-fit$value)
  KFfit$loglkhd = -fit$value
  KFfit$R2= 1-var(y-as.vector( ans$at[1:length(y)]  ),na.rm=T)/var(y,na.rm=T)
  KFfit$par= fit$par
  
  return(KFfit) 
}