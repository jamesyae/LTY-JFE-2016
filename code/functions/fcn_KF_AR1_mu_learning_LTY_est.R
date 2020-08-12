fcn_KF_AR1_mu_learning_LTY_est <- function(yt, KF_idx) {  
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
  # d(t) : (r x 1)
  # E(v(t)*v(t)') = HH' : (r x r)
  # E(w(t)*w(t)') = GG' : (n x n)  
  
Varma11_LTY_trans <- function(theta) {
  
  
  
 sige2 = exp(2*as.numeric(theta["logsigmae"]))
 sign2 =  exp(2*as.numeric(theta["logsigman"]))
 
 sigmu02 =exp(2*as.numeric(theta["logsigmamu0"]))
 sigmuhat02 =exp(2*as.numeric(theta["logsigmamuhat0"]))
 
 phihat = as.numeric(theta["phihat"])
 phi =  as.numeric(theta["phi"])
 what = as.numeric(theta["what"])
 pihats2 = exp(as.numeric(theta["logpihats2"]))
      
    #pis2 = 1/(  1/sige2 + 1/sign2  )
    Zt = matrix(c(0,0,1,0,0,1,1,0),2,4) 
    GGt = matrix(c(0,0,0,0),2,2)   
  
    # reset T counter for each firm
    sigmamu2t  = 1/(  1/sigmuhat02   +  (1-phihat)^2/pihats2*seq(1,maxT)  )
    sigmamu2t1 =  c(   sigmuhat02 , sigmamu2t[1:(maxT-1)]) 
    
    Tt = array(cbind(0, rbind(
            sigmamu2t/sigmamu2t1 , 0,  1-phi ,0 , 
             (phi-phihat)*(1-phihat)*sigmamu2t/pihats2 ,   phi  ,   phihat  ,0,
            0,0,0,0,
              (1- phihat )^2*sigmamu2t/pihats2  ,0,   phihat  ,  1) ), 
         dim=c(4,4,maxT+1)  ) 
    Tt  = Tt[ , , KF_idx+1]
 
 
 
   r = dim(Tt)[1] 
   n = dim(GGt)[1]
   
   a0 <-  c(theta["muhat0"], 0  , theta["muhat0"], theta["mu0"] ) 
 
   FF = matrix(c(  phi , 0,  phihat, 0  ),2,2) 
   q  = matrix(c(  1 , what,0, what  ),2,2)   
   HH = q%*%diag(c(sige2, sign2) )%*%t(q)   
 
 
   P00 <- matrix(solve( diag( rep(1, 2^2) ) - FF%x%FF )%*%(matrix(HH,  2^2, 1)), ncol=2 )
   P0 <- matrix(rep(0, r^2), ncol=r)
   P0[2:3,2:3] = P00
   P0[r,r] = sigmu02
 
      
    h1 =   (1- what )*(1- phihat )*sigmamu2t/pihats2*sige2
    h5 =    -what*(1- phihat )*sigmamu2t/pihats2* sign2  
    h2 = sqrt(sige2)
    h3 =  what * sqrt(sige2)
    h7 =  what * sqrt(sign2)
    
    HHt =  array(cbind(as.vector(P0), rbind(h1^2+h5^2, h1*h2, h1*h3+h5*h7,0,
                       h1*h2,h2^2,h2*h3,0,h1*h3+h5*h7,h2*h3,h3^2+h7^2,0,
                       0,0,0,0) ),dim=c(4,4,maxT+1)  )
    HHt  = HHt[ , , KF_idx+1]
    
    dt <-  matrix(0, nrow = r , ncol=TT )
    dt[ ,  KF_idx==0] = matrix(a0,r,1)

    ct = matrix(as.numeric(c(0 , -theta["b"] )),2,1) 
    
 
    

    return(list(a0 = a0, P0 = P0, ct = ct, dt = dt, Zt = Zt, Tt = Tt, GGt = GGt, HHt = HHt))
  }
  
 

KFobjective <- function(theta) {  
  sp <- Varma11_LTY_trans(theta) 
  if (  abs( as.numeric(theta["phi"])) >= 1  ||  abs( as.numeric(theta["phihat"])) >= 1  ) neglkhd = Inf
  else 
    neglkhd = -(fkf(a0 = sp$a0, P0 = sp$P0, dt = sp$dt, ct = sp$ct, Tt = sp$Tt,
                    Zt = sp$Zt, HHt = sp$HHt, GGt = sp$GGt, yt = yt ))$logLik 
  # print(neglkhd)
  return(neglkhd)
}
 
## Initial point by MM with exact identification

maxT = max(KF_idx)
TT = max(dim(yt))
theta = fcn_MM_AR1_LTY_est(yt=yt, init=TRUE) 
KFfit <- optim(theta, KFobjective ,  control = list(maxit = 3000 ), hessian = FALSE)
  
return(KFfit) 

}
 





              
              