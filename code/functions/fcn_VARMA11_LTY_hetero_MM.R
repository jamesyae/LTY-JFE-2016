fcn_VARMA11_LTY_hetero_MM <- function(scaleVset, option , fixedeffect) {
  
  filename = fcn_filename_maker(scaleVset=scaleVset, option=option )
  load(file=filename)
  
    
  NN = length(firm_TS_data$firm_begin_deflated) # number of firms
  
  
  mom.mu= rep(NA,NN)
  mom.muhat = rep(NA,NN) 
  
  
  mom.phi= rep(NA,NN)
  mom.phihat = rep(NA,NN) 
  
  
  yet =  firm_TS_data$ye_firm_deflated
  yft =  firm_TS_data$yf_firm_deflated
 
       
        for (jj in seq( 1, NN ))
        {  
          
          firm_idx_temp = firm_TS_data$firm_begin_deflated[jj]:firm_TS_data$firm_end_deflated[jj]
            
          ye_all = yet[ firm_idx_temp]
          yf_all = yft[ firm_idx_temp]  

          
          Ty = length(firm_idx_temp)                    
          Ta = firm_TS_data$no_obs_deflated[jj]-1;
          

   
            meanye = mean(ye_all,na.rm=T)
            meanyf = mean(yf_all,na.rm=T)
  

          
          mom.mu[jj] =  meanye 
          mom.muhat[jj] =  meanyf 
          
          varye    = sum( (ye_all-meanye)^2 ,na.rm=T)/Ta;
          covyeyf  = sum( (ye_all-meanye)*(yf_all-meanyf),na.rm=T)/Ta;
          varyf    = sum( (yf_all-meanyf)^2 ,na.rm=T)/Ta;
          
          covye1ye = sum( (ye_all[2:Ty]-meanye)*(ye_all[1:(Ty-1)]-meanye),na.rm=T)/Ta;
          covyf1yf = sum( (yf_all[2:Ty]-meanyf)*(yf_all[1:(Ty-1)]-meanyf),na.rm=T)/Ta;
          covye1yf = sum( (ye_all[2:Ty]-meanye)*(yf_all[1:(Ty-1)]-meanyf),na.rm=T)/Ta;
          covyf1ye = sum( (yf_all[2:Ty]-meanyf)*(ye_all[1:(Ty-1)]-meanye),na.rm=T)/Ta;
          
          mom.phi[jj] = min(1,max(-1,covye1yf/covyeyf))
          mom.phihat[jj] = min(1,max(-1, sum(solve(  matrix(c(varyf,  covyeyf, covyeyf,  varye),2,2)  )%*%matrix(c(covyf1yf,covyf1ye),ncol=1)) ))
          
        } 
        
    cov_mumuhat  =  (cov(cbind(mom.mu, mom.muhat ))         )
    cov_phiphihat =  (cov(cbind(mom.phi, mom.phihat ))         )
    cov_mumuhatphiphihat =  (cov(cbind(mom.mu, mom.muhat, mom.phi , mom.phihat  ))         )
 
  Est_cov = list(cov_mumuhat=cov_mumuhat, cov_phiphihat=cov_phiphihat, cov_mumuhatphiphihat=cov_mumuhatphiphihat)
  
  if (fixedeffect) temp_fixedeffect = "_fixedeffect"  else    temp_fixedeffect =  ""  
  save( Est_cov, file = sprintf("Est_cov_mumuhatphiphihat%s.Rdata",temp_fixedeffect ) )
}
