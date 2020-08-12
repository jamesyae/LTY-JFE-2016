fcn_MM_VARMA11_LTY_est<- function(yt, init) {


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
theta$logsigmaa = max(log(10^(-16)),log(sqrt(mom.sigmaa2)))
theta$logsigmae = log(sqrt(mom.sigmae2))
theta$logsigman = log(sqrt(mom.sigman2))

if (init==FALSE) {

theta$sigmaa = (sqrt(mom.sigmaa2))
theta$sigmae = (sqrt(mom.sigmae2))
theta$sigman = (sqrt(mom.sigman2))
 

theta$w = 1/mom.sigman2/(1/mom.sigman2+1/mom.sigmae2)

estt.pis2 = 1/(1/mom.sigmae2+1/mom.sigman2);
a = 1/mom.sigmaa2 ;
b = 1-mom.phihat^2 - estt.pis2/mom.sigmaa2  ;
c = -estt.pis2; 
estt.pit1t2 = ( -b + sqrt(   b^2 - 4*a*c  )   ) /a / 2  ;
theta$Kx = estt.pit1t2 /(mom.sigmaa2 + estt.pit1t2  )    ;
 

G0 =  varye+varyf-2*covyeyf
G1 =  covye1ye + covyf1yf  - covye1yf  -covyf1ye
theta$FE_rho = G1/G0
}

return(theta)

}



