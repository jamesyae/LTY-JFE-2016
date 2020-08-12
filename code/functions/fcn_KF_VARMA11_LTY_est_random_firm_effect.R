fcn_KF_VARMA11_LTY_est_random_firm_effect <- function(yt) {  
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
    
    Tt = matrix(c(as.numeric(theta["phi"]),0,as.numeric(theta["phihat"])*as.numeric(theta["Khat"]),
                  1,0,0,0,0,as.numeric(theta["phihat"])*(1-as.numeric(theta["Khat"]))),3,3)   
    
    q = matrix(c(1, -as.numeric(theta["phi"]), 0, 1 ,0 ,as.numeric(theta["what"]),0,0,as.numeric(theta["what"]) ),3,3)   
    HHt = q%*%diag(exp(2*c(as.numeric(c(theta["logsigmaa"], theta["logsigmae"], theta["logsigman"])))))%*%t(q)    
    P0 <- matrix(solve( diag(rep(1, dim(HHt)[1]^2)) - Tt%x%Tt )%*%(matrix(HHt, dim(HHt)[1]^2,1)),ncol=dim(HHt)[1])
    
    
    Tt = cbind(rbind(Tt,rep(0,3),rep(0,3)), c(1-as.numeric(theta["phi"]),0,0,1,0), c(0,0,1-as.numeric(theta["phihat"]),0,1) )
    HHt = cbind( rbind(HHt,rep(0,3),rep(0,3)), rep(0,5),rep(0,5) )
    q = matrix( c( as.numeric(theta["musig11"]) , 0,  as.numeric(theta["musig12"])  ,  as.numeric(theta["musig22"]) ) ,2,2)
    covmu = (q %*% t(q) )
    P0 = cbind( rbind(P0,rep(0,3),rep(0,3)),  rbind(  matrix(rep(0,6), nrow=3) , covmu   ) )

      
    a0 <- c(0,0,0, as.numeric(theta["mue"]), as.numeric(theta["muehat"])    ) 
    Zt = matrix(c(1,0,0,0,0,1,0,0,0,0),2,5)     
    GGt = matrix(c(0,0,0,0),2,2)
    ct = matrix( rep(0,dim(GGt)[1]) , nrow = dim(GGt)[1])
    dt <- matrix( rep(0,dim(HHt)[1]) , nrow = dim(HHt)[1])

    
    return(list(a0 = a0, P0 = P0, ct = ct, dt = dt, Zt = Zt, Tt = Tt, GGt = GGt, HHt = HHt))
  }
  
  


KFobjective <- function(theta, yt) {  
  sp <- Varma11_LTY_trans(theta) 
  if (sum(abs(eigen( sp$Tt )$values)>=1)>2) neglkhd = Inf
  else 
    neglkhd = -(fkf(a0 = sp$a0, P0 = sp$P0, dt = sp$dt, ct = sp$ct, Tt = sp$Tt,
                    Zt = sp$Zt, HHt = sp$HHt, GGt = sp$GGt, yt = yt ))$logLik 
  # print(neglkhd)
  return(neglkhd)
}



 
## Initial point by MM with exact identification
theta = fcn_MM_VARMA11_LTY_est(yt=yt, init=TRUE)

theta$musig11 = sd(yt[1,]-yt[2,],na.rm=T)/5
theta$musig12 = 0.5*sd(yt[1,]-yt[2,],na.rm=T)/5
theta$musig22 = sd(yt[1,]-yt[2,],na.rm=T)/5

#KFfit <- optim(theta, KFobjective , yt = yt, control = list(maxit = 3000 ), hessian = FALSE)


## pre-standardized data
KFfit <- optim(theta, KFobjective , yt = yt, control = list(maxit = 3000 ), hessian = FALSE)

 
return(KFfit) 

}
 
              
              