fcn_KF_VARMA11_LTY_2signal_est <- function(yt) {  
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
  
  
  
Varma11_LTY_trans <- function(theta) {
  
    ct = matrix(as.numeric(c(theta["mue"] , theta["muehat"] )),2,1) 
    Zt = matrix(c(1,0,0,1,0,0,0,0),2,4) 
    Tt = matrix(c(as.numeric(theta["phi"]),as.numeric(theta["phihat"])*as.numeric(theta["Khat"]),0,0,
                  0,as.numeric(theta["phihat"])*(1-as.numeric(theta["Khat"])),0,0,
                  (-as.numeric(theta["phi"])),(-as.numeric(theta["phihat"])*as.numeric(theta["wahat"])),0,0,
                  0,(-as.numeric(theta["phihat"])*as.numeric(theta["wahat"])),0,0),4,4)   
    
    q = matrix(c(1, as.numeric(theta["wahat"]), 1 ,0,
                 1, as.numeric(theta["what"]), 0 ,0,                                           
                 0, as.numeric(theta["what"]), 0 ,0,   
                 0, as.numeric(theta["wahat"]), 0 ,1),4,4)                                           
    HHt = q%*%diag(exp(2*c(as.numeric(c(theta["logsigmaa"], theta["logsigmae"], theta["logsigman"], theta["logsigmana"])))))%*%t(q)    
    GGt = matrix(c(0,0,0,0  ),2,2)
  
    dt <- matrix(0, nrow = dim(HHt)[1])
    a0 <- rep(0,  dim(HHt)[1])
    P0 <- matrix(solve( diag(rep(1, dim(HHt)[1]^2)) - Tt%x%Tt )%*%(matrix(HHt, dim(HHt)[1]^2,1)),ncol=dim(HHt)[1])
    return(list(a0 = a0, P0 = P0, ct = ct, dt = dt, Zt = Zt, Tt = Tt, GGt = GGt, HHt = HHt))
  }

 
KFobjective <- function(theta, yt) {  
  sp <- Varma11_LTY_trans(theta) 
  if (sum(abs(eigen( sp$Tt )$values)>=1)>0) neglkhd = Inf
  else 
  neglkhd = -(fkf(a0 = sp$a0, P0 = sp$P0, dt = sp$dt, ct = sp$ct, Tt = sp$Tt,
             Zt = sp$Zt, HHt = sp$HHt, GGt = sp$GGt, yt = yt ))$logLik 
  #print(neglkhd)
  return(neglkhd)
}

 

ye1 = yt[1,2:(dim(yt)[2])]   
ye0 = yt[1,1:(dim(yt)[2]-1)] 
yf1 = yt[2,2:(dim(yt)[2])]   
yf0 = yt[2,1:(dim(yt)[2]-1)] 

covye1ye = mean(ye0*ye1, na.rm=T)-mean(ye1, na.rm=T)*mean(ye0, na.rm=T)    
covyf1yf = mean(yf0*yf1, na.rm=T)-mean(yf1, na.rm=T)*mean(yf0, na.rm=T)     
covye1yf = mean(yf0*ye1, na.rm=T)-mean(ye1, na.rm=T)*mean(yf0, na.rm=T)    
covyf1ye = mean(ye0*yf1, na.rm=T)-mean(yf1, na.rm=T)*mean(ye0, na.rm=T)     
covyeyf = mean(yt[1,]*yt[2,], na.rm=T)-mean(yt[1,], na.rm=T)*mean(yt[2,], na.rm=T)   
varye = mean(yt[1,]*yt[1,], na.rm=T)-mean(yt[1,], na.rm=T)*mean(yt[1,], na.rm=T)   
varyf = mean(yt[2,]*yt[2,], na.rm=T)-mean(yt[2,], na.rm=T)*mean(yt[2,], na.rm=T)   
 
mom.phi = min(1,max(-1,covye1yf/covyeyf)) 
mom.sigmaa2 = max(0,varye - covye1ye/mom.phi)
mom.sigmae2 = (1-mom.phi^2)*varye -(1-mom.phi^2)*mom.sigmaa2  
mom.phihat = sum(solve(  matrix(c(varyf,  covyeyf, covyeyf,  varye),2,2)  )%*%matrix(c(covyf1yf,covyf1ye),ncol=1)) 
mom.Khat =  (covyf1ye/mom.phihat-covyeyf  )/(varye - covyeyf)  
mom.what = 1/mom.sigmae2*(covyeyf*(1-mom.phi*mom.phihat*(1-mom.Khat))-
                            mom.phi*mom.phihat*mom.Khat*varye +mom.phi*mom.phihat*mom.Khat*mom.sigmaa2  )  
mom.sigman2 =   1/mom.what*(varyf*(1-mom.phihat^2*(1-mom.Khat)^2)
                            - mom.phihat^2*mom.Khat^2*varye
                            - mom.what^2*mom.sigmae2
                            - 2*mom.phihat^2*mom.Khat*(1-mom.Khat)*covyeyf)  

theta=list()
theta$mue = mean(yt[1,], na.rm=T)   
theta$muehat = mean(yt[2,], na.rm=T)   
theta$phi = mom.phi
theta$phihat = mom.phihat
theta$Khat = mom.Khat
theta$what = mom.what
theta$logsigmaa = log(sqrt(mom.sigmaa2))
theta$logsigmae = log(sqrt(mom.sigmae2))
theta$logsigman = log(sqrt(mom.sigman2))
theta$logsigmana = log(0.5)
theta$wahat = 0.2



sqrt(mom.sigman2)/sqrt(mom.sigmae2 ) 
sqrt(mom.sigmaa2)/sqrt(mom.sigmae2 ) 

w = 1/mom.sigman2/(1/mom.sigman2+1/mom.sigmae2)


estt.pis2 = 1/(1/mom.sigmae2+1/mom.sigman2);

a = 1/mom.sigmaa2 ;
b = 1-mom.phihat^2 - estt.pis2/mom.sigmaa2  ;
c = -estt.pis2; 
estt.pit1t2 = ( -b + sqrt(   b^2 - 4*a*c  )   ) /a / 2  ;
Kx = estt.pit1t2 /(mom.sigmaa2 + estt.pit1t2  )    ;

w
Kx 


bb=((1+0.05^2)*0.05^2+1)/0.5/0.05^2
(-bb+sqrt(bb^2-4))/2

bb=((1+0.13^2)*0.13^2+1)/0.5/0.13^2
(-bb+sqrt(bb^2-4))/2

bb=((1+0.5^2)*0.5^2+1)/0.5/0.5^2
(-bb+sqrt(bb^2-4))/2


G0 =  varye+varyf-2*covyeyf
G1 =  covye1ye + covyf1yf  - covye1yf  -covyf1ye
G1/G0

  
  
  
KFfit <- optim(theta, KFobjective, yt = yt, control = list(maxit = 3000 ), hessian = FALSE)
 
 
return(KFfit) 

}
 
              
              