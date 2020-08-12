fcn_KF_CLTY_est_ht <- function(yt) {  
  # Univariate KF
  
  # written by James S. Yae
  # Y: data (T x n)    
  # Staste: X(t+1) = d(t) + T(t)*X(t) + v(t+1)
  # Obs:      y(t) = c(t) + Z(t)*X(t) + w(t)
  # where
  # X(t), v(t+1) : (r x 1)  
  # y(t), w(t) : (n x 1) 
  # T(t) : (r x r) 
  # Z(t) : (n x r)   
  # c(t) : (n x 1)
  # E(v(t)*v(t)') = HH' : (r x r)
  # E(w(t)*w(t)') = GG' : (n x n)  
  
  fcn_logF_coef_m_T  <-  function(param1) {     
    
    zeroto52 = seq(0,52)
    
    
    a1y = param1$a1y
    a0y = param1$a0y
    sigy = param1$sigy
    Lambda_y = param1$Lambda_y
    Lambda_h = param1$Lambda_h
    Lambda_r = param1$Lambda_r
    eta = param1$eta
    xbar = param1$xbar
    sigx = param1$sigx
    sigF = param1$sigF  
    
    b3s = b3*xbar
    ws  = w*xbar^2
    as  = a*xbar^2
    cs  = c/xbar
    
    Dy  = a1y^zeroto52
    
    A1 = rep(NA, 53)
    Dx = A1
    dx = A1
    Dr = A1
    Gh = A1
    
    m=0 
    A1[m+1]   = 0
    Dx[m+1]  = -eta
    dx[m+1]  = 0
    Dr[m+1]  = 0 
    Gh[m+1]  = 0
    
    m=1
    A1[m+1]   = a0y + sigy^2*(- Lambda_y + 0.5) 
    Dx[m+1]  = -eta*b1
    dx[m+1]  = -eta*b2
    Dr[m+1]  = -eta*b3s 
    Gh[m+1]  = Lambda_h*eta + eta^2/2
    
    
    A_error = FALSE
    for (m in seq(3,53)) {    
      A1[m]   = A1[m-1] + Dy[m-1]*a0y + Dr[m-1]*c0 + Gh[m-1]*ws -
        Lambda_y*Dy[m-1]+ (sigy*Dy[m-1])^2/2 -Lambda_r*Dr[m-1]*sigr^2 +
        (sigr*Dr[m-1])^2/2 -  log(1-2*Gh[m-1]*as)/2
      Dx[m]  = Dx[m-1]*b1+dx[m-1] 
      dx[m]  = Dx[m-1]*b2
      Dr[m]  = Dx[m-1]*b3s+Dr[m-1]*c1
      Gh[m]  = Gh[m-1]*b + Gh[m-1]*as*(cs-(-Lambda_h+Dx[m-1])/2/Gh[m-1]/as)^2/(1-2*Gh[m-1]*as)+ 
        cs*(-Lambda_h+Dx[m-1]) -(-Lambda_h+Dx[m-1])^2/(4*as*Gh[m-1]) -Lambda_h^2/2
      A_error = A_error | (1-2*Gh[m-1]*as)<0
      #if ((1-2*Gh[m-1]*as)<0) { print((1-2*Gh[m-1]*as)) ;print( A_error) }
    }
    
    ## A: (TT x m) matrix
    ## m: the number of weeks between current date and the settlement date: 0,...,52  
    ## TT: the week index of futures settlement date: 1,...,52  
    A = cbind(rep(0,52), t(apply(    ( matrix(rep(1,52),ncol=1) %x% matrix(Dx[1:52],nrow=1))*(b0_set *xbar), 1, cumsum)) )+
      matrix(rep(1,52), ncol=1 ) %x% matrix(A1, nrow=1 ) 
    FCoef = list(A=A,Dy=Dy,Dx=Dx,dx=dx,Dr=Dr,Gh=Gh, A_error= A_error)
    
    return(FCoef)
  }
  
  KF_param_trans <- function(param1,FCoef) {
    
    theta = param1
    b3s = b3*theta$xbar
    ws  = w*theta$xbar^2
    as  = a*theta$xbar^2
    cs  = c/theta$xbar
    
    ct = matrix( rbind(t(matrix( FCoef$A[ m_week0*52 +  week_idx_set_FtT0 ],ncol=6))+
                         t(matrix(FCoef$Dr[m_week0+1],ncol=6)*rtetL )   , 0,0,0  ) ,nrow=9)
    Zt = array(rbind(
      array(rbind(t(matrix(FCoef$Dy[m_week0+1],ncol=6)),0,0,0),dim=c(9, TT)) ,
      array(rbind(t(matrix(FCoef$Dx[m_week0+1],ncol=6)),1/theta$xbar,1,0),dim=c(9, TT)) ,
      array(rbind(t(matrix(FCoef$dx[m_week0+1],ncol=6)),0,0,0),dim=c(9, TT))  ,
      array(rbind(t(matrix(FCoef$Gh[m_week0+1],ncol=6)),0,0,1/theta$xbar^2),dim=c(9, TT))          
    ) ,dim=c(9,4, TT))
    
    Tt = matrix( c(theta$a1y,0,0,0,0,b1,1,0,0,b2,0,0,0,0,0,b),4,4)   
    
    HHt = array(rbind(theta$sigy^2,0,0,0,0,theta$xbar^2*ht,0, -2*as*cs*ht*theta$xbar,0,0,0,theta$sigh^2), dim=c(4,4,TT))     
    GGt = diag(c(rep(theta$sigF,6),theta$sigx,theta$sigx,theta$sigh)^2)
    
    dt <- matrix( rbind(theta$a0y, theta$xbar*b0[week_idx_Xt] + rtetL*b3s, 0, as+(w+as*cs^2*ht)*param1$xbar^2 ) ,nrow=4)
    a0 <- rep(0,  dim(HHt)[1])
    P0 <- matrix(solve( diag(rep(1, dim(HHt)[1]^2)) - Tt%x%Tt )%*%(matrix(HHt, dim(HHt)[1]^2,1)),ncol=dim(HHt)[1])
    return(list(a0 = a0, P0 = P0, ct = ct, dt = dt, Zt = Zt, Tt = Tt, GGt = GGt, HHt = HHt))
  }
  
  
  
  KFobjective <- function(param0, yt) {  
    eta = exp(as.numeric(param0["logeta"]))
    xbar = exp(as.numeric(param0["logxbar"]))
    sigy = exp(as.numeric(param0["logsigy"]))
    sigx = exp(as.numeric(param0["logsigx"])) 
    sigF = exp(as.numeric(param0["logsigF"]))
    sigh = exp(as.numeric(param0["logsigh"]))
    a1y = as.numeric( param0["a1y"]  )
    param1 = list( a1y=a1y, a0y=as.numeric(param0["a0y"]), sigy=sigy, Lambda_y=as.numeric(param0["Lambda_y"]),
                   Lambda_h=as.numeric(param0["Lambda_h"]),Lambda_r=as.numeric(param0["Lambda_r"]),
                   eta=eta, xbar=xbar, sigx=sigx, sigF=sigF, sigh=sigh) 
    FCoef <- fcn_logF_coef_m_T(param1 = param1) 
    
    
    if (  FCoef$A_error  ) {neglkhd = Inf   } else {
      
      
      sp <- KF_param_trans(param1=param1, FCoef=FCoef) 
      if (sum(abs(eigen( sp$Tt )$values)>=1)>0   ) {neglkhd = Inf} else 
      {neglkhd = -(fkf(a0 = sp$a0, P0 = sp$P0, dt = sp$dt, ct = sp$ct, Tt = sp$Tt,
                       Zt = sp$Zt, HHt = sp$HHt, GGt = sp$GGt, yt = t(yt) ))$logLik }
    } 
    return(neglkhd) 
  }
  
  TT = dim(yt)[1]  
  
  ## Initial points  
  
  Lambda_y =  0
  Lambda_h =  0
  Lambda_r =  0
  logeta = log(1)
  logxbar = log(mean(yt[,8],na.rm=T)/mean(yt[,7],na.rm=T)  )
  logsigy = log(sd( yt[,8],na.rm=T)/5)  
  logsigx = log(sd( yt[,8],na.rm=T)/2) 
  logsigF = mean(log(apply( yt[,1:6],2,sd,na.rm=T)) )
  logsigh = log(0.5) 
  a1y  = 0.95
  a0y = 0
  
  
  param0 = list( a1y =a1y , a0y=a0y, logsigy=logsigy, Lambda_y=Lambda_y, Lambda_h=Lambda_h,
                 Lambda_r=Lambda_r, logeta=logeta, logxbar=logxbar, logsigx=logsigx,
                 logsigF=logsigF, logsigh=logsigh) 
  
  ## Fit KF
  KFfit <- optim( param0, KFobjective , yt = yt, control = list(maxit = 30000 ), hessian = TRUE)
  sqrt(diag(solve(KFfit$hessian[1:7,1:7])))
  KFfit$par
  t(param0)
  
  #optimHess(KFfit$par, KFobjective, yt = yt)
  
  #library(bbmle)
  # fit <- mle2(minuslogl=KFobjective ,  param0  = param0   , data = list(yt=yt), control = list(maxit = 30000 )   )
  #  summary(fit)
  #param00 = t(as.matrix(param0))
  # parnames( KFobjective ) <- names(param0 )
  
  
  return(KFfit) 
  
}

