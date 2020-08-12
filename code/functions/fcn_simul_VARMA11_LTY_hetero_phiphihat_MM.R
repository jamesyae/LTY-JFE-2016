fcn_simul_VARMA11_LTY_hetero_phiphihat_MM <- function(scaleVset, option ,   KFfit, fixedeffect, MM) {
  
  filename = fcn_filename_maker(scaleVset=scaleVset, option=option )
  load(file=filename)
  
  NN = length(firm_TS_data$firm_begin_deflated) # number of firms
  KK # number of iteration
  
  mom.phi= rep(NA,NN)
  mom.phihat = rep(NA,NN)
  cov_phiphihat = matrix( rep(NA,MM*4), nrow=4 )
  
  mom.mu= rep(NA,NN)
  mom.muhat = rep(NA,NN)
  cov_mumuhat = matrix( rep(NA,MM*4), nrow=4 )
  
  
  for (kk in seq(1,MM)) {     
    
        yt = fcn_VARMA11_LTY_random_data_firm_level(scaleVset=scaleVset, option=option , KFfit=KFfit )
        meanye = mean(yt[1,],na.rm=T)
        meanyf = mean(yt[2,],na.rm=T)
  
        for (jj in seq( 1, NN ))
        {  
          
          firm_idx_temp = firm_TS_data$firm_begin_deflated[jj]:firm_TS_data$firm_end_deflated[jj]
          Ty = length(firm_idx_temp)                    
          Ta = firm_TS_data$no_obs_deflated[jj]-1;
          
          ye_all = yt[1,firm_idx_temp]
          yf_all = yt[2,firm_idx_temp]  
          
          if (fixedeffect) {
            meanye = mean(ye_all,na.rm=T)
            meanyf = mean(yf_all,na.rm=T)
          }
          
          varye    = sum( (ye_all-meanye)^2 ,na.rm=T)/Ta;
          covyeyf  = sum( (ye_all-meanye)*(yf_all-meanyf),na.rm=T)/Ta;
          varyf    = sum( (yf_all-meanyf)^2 ,na.rm=T)/Ta;
          
          covye1ye = sum( (ye_all[2:Ty]-meanye)*(ye_all[1:(Ty-1)]-meanye),na.rm=T)/Ta;
          covyf1yf = sum( (yf_all[2:Ty]-meanyf)*(yf_all[1:(Ty-1)]-meanyf),na.rm=T)/Ta;
          covye1yf = sum( (ye_all[2:Ty]-meanye)*(yf_all[1:(Ty-1)]-meanyf),na.rm=T)/Ta;
          covyf1ye = sum( (yf_all[2:Ty]-meanyf)*(ye_all[1:(Ty-1)]-meanye),na.rm=T)/Ta;
          
          mom.phi[jj] = min(1,max(-1,covye1yf/covyeyf))
          mom.phihat[jj] = min(1,max(-1, sum(solve(  matrix(c(varyf,  covyeyf, covyeyf,  varye),2,2)  )%*%matrix(c(covyf1yf,covyf1ye),ncol=1)) ))
                       
          mom.mu[jj] =  meanye 
          mom.muhat[jj] =  meanyf 
          
        } 
        
     cov_mumuhat[,kk]  =  as.vector(cov(cbind(mom.mu, mom.muhat ))         )
    cov_phiphihat[,kk]  =  as.vector(cov(cbind(mom.phi, mom.phihat ))         )
        print(kk)
  }
  
  if (fixedeffect) temp_fixedeffect = "_fixedeffect"  else    temp_fixedeffect =  ""  
  save(cov_phiphihat,cov_mumuhat, file = sprintf("simul_cov_phiphihat%s.Rdata",temp_fixedeffect ) )
}
