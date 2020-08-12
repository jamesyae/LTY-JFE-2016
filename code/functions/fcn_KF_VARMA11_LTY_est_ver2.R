fcn_KF_VARMA11_LTY_est_ver2 <- function(yt) {  
# KF
 
  # written by James S. Yae
  # updated on 6/24/2014
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
  
Varma11_LTY_trans <- function(theta) {
    
    ct = matrix( c(0 , 0 ),2,1) 
    Zt = matrix(c(1,0,0,0,0,1),2,3) 
    Tt = matrix(c(as.numeric(theta["phi"]),0,as.numeric(theta["phihat"])*as.numeric(theta["Khat"]),
                  1,0,0,0,0,as.numeric(theta["phihat"])*(1-as.numeric(theta["Khat"]))),3,3)   
    
    q = matrix(c(1, -as.numeric(theta["phi"]), 0, 1 ,0 ,as.numeric(theta["what"]),0,0,as.numeric(theta["what"]) ),3,3)   
    HHt = q%*%diag(exp(2*c(as.numeric(c(theta["logsigmaa"], theta["logsigmae"], theta["logsigman"])))))%*%t(q)    
    GGt = matrix(c(0,0,0,0),2,2)
    
    dt <- matrix(  c(as.numeric(theta["mue"])*(1-as.numeric(theta["phi"])),0, as.numeric(theta["muehat"])*(1-as.numeric(theta["phihat"])) ), nrow = dim(HHt)[1])
    a0 <- c(solve(diag(3) -  Tt) %*% dt)
    P0 <- matrix(solve( diag(rep(1, dim(HHt)[1]^2)) - Tt%x%Tt )%*%(matrix(HHt, dim(HHt)[1]^2,1)),ncol=dim(HHt)[1])
    return(list(a0 = a0, P0 = P0, ct = ct, dt = dt, Zt = Zt, Tt = Tt, GGt = GGt, HHt = HHt))
  }
  
  


KFobjective <- function(theta, yt) {  
  sp <- Varma11_LTY_trans(theta) 
  if (sum(abs(eigen( sp$Tt )$values)>=1)>0  |  det(diag(3) -  sp$Tt)==0  ) neglkhd = Inf
  else 
    neglkhd = -(fkf(a0 = sp$a0, P0 = sp$P0, dt = sp$dt, ct = sp$ct, Tt = sp$Tt,
                    Zt = sp$Zt, HHt = sp$HHt, GGt = sp$GGt, yt = yt ))$logLik 
  # print(neglkhd)
  return(neglkhd)
}



 
## Initial point by MM with exact identification
theta = fcn_MM_VARMA11_LTY_est(yt=yt, init=TRUE)
#KFfit <- optim(theta, KFobjective , yt = yt, control = list(maxit = 3000 ), hessian = FALSE)

## pre-standardized data
KFfit <- optim(theta, KFobjective , yt = yt, control = list(maxit = 3000 ), hessian = FALSE)

 
return(KFfit) 

}
 
              
              