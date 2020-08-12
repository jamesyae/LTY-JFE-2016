fcn_MM_AR1_LTY_est<- function(yt, init) {


## Method of moments with exact identification
  
  


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

mom.phi = min(1,max(-1,covye1ye/varye))
mom.sigmae2 = (1-mom.phi^2)*varye 
mom.phihat =min(1,max(-1,covyf1ye/varye)) 
mom.what =  (covyeyf - mom.phi*mom.phihat*varye)/mom.sigmae2
mom.sigman2 = (varyf -  mom.phihat^2*varye)/mom.what-mom.sigmae2   

theta=list() 
theta$b = mean(yt[1,], na.rm=T)-mean(yt[2,], na.rm=T)   
theta$phi = mom.phi
theta$phihat = mom.phihat
theta$what = mom.what 
theta$logsigmae = log(sqrt(mom.sigmae2))
theta$logsigman = log(sqrt(mom.sigman2))

theta$logsigmamu0 = log( sd(yt[1,] ,na.rm=T )/2  )
theta$mu0 = mean(yt[1,] ,na.rm=T)

theta$logsigmamuhat0 = log( sd(yt[1,] ,na.rm=T )/2  )
theta$muhat0 = mean(yt[1,] ,na.rm=T)
theta$logpihats2 = log(1/(  1/mom.sigmae2 + 1/mom.sigman2  ))


if (init==FALSE) {
 
  theta$sigmae = (sqrt(mom.sigmae2))
  theta$sigman = (sqrt(mom.sigman2))
   
  
  theta$w = 1/mom.sigman2/(1/mom.sigman2+1/mom.sigmae2)  
   
  
  G0 =  varye+varyf-2*covyeyf
  G1 =  covye1ye + covyf1yf  - covye1yf  -covyf1ye
  theta$FE_rho = G1/G0
}

return(theta)

}



