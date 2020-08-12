fcn_AR1_mu_learning_LTY_random_data<- function(scaleVset, option , KFfit ) {
  
  filename = fcn_filename_maker(scaleVset=scaleVset, option=option )
  load(file=filename)
   
  ytsim1set = rep(NA, length(firm_TS_data$ye_firm_deflated) )
  ytsim2set = ytsim1set 
  
  
  for (jj in seq( 1 , length(firm_TS_data$firm_begin_deflated)))
  {  
       
      firm_idx_temp = firm_TS_data$firm_begin_deflated[jj]:firm_TS_data$firm_end_deflated[jj]  
      
      Tsim = length(firm_idx_temp)  
 
      et_sim = rnorm(Tsim)*exp(KFfit$par["logsigmae"])  
      nt_sim = rnorm(Tsim)*exp(KFfit$par["logsigman"])
 
  
      
      mue = KFfit$par["mu0"] + exp(KFfit$par["logsigmamu0"])*rnorm(1) 
      muehat = KFfit$par["muhat0"]  
      sigmuhat2 = exp( 2*KFfit$par["logsigmamuhat0"] )
      
       
      
      ytsim1= rep(NA, Tsim )
      ytsim2= rep(NA, Tsim )
       
      ytsim1_old = mue +  rnorm(1)*exp(KFfit$par["logsigmae"])/sqrt(1-KFfit$par["phi"]^2)      
      for (ii  in seq(1, Tsim ))
      {    
        
        ytsim1[ii] = mue*(1-KFfit$par["phi"])   + KFfit$par["phi"]*ytsim1_old + et_sim[ii] 
        ytsim2[ii] = muehat*(1-KFfit$par["phihat"]) + KFfit$par["phihat"]*ytsim1_old + KFfit$par["what"]*(et_sim[ii] + nt_sim[ii])
       
        
        sigmuhat2_old = sigmuhat2
        sigmuhat2  = 1/(  1/sigmuhat2_old   +  (1-KFfit$par["phihat"])^2/exp(KFfit$par["logpihats2"])  )
        muehat =  sigmuhat2*( muehat/sigmuhat2_old 
                              + (ytsim1[ii]-KFfit$par["phihat"]*ytsim1_old - KFfit$par["what"]*(et_sim[ii] + nt_sim[ii])  )*(1-KFfit$par["phihat"])/exp(KFfit$par["logpihats2"])           )
        
        
        ytsim1_old =  ytsim1[ii]
        
      }
       
      
      naidx = is.na(firm_TS_data$ye_firm_deflated[firm_idx_temp])
      ytsim1[naidx] = NA
      
      naidx = is.na(firm_TS_data$yf_firm_deflated[firm_idx_temp])
      ytsim2[naidx] = NA
      
  
      ytsim1set[firm_idx_temp] = ytsim1 
      ytsim2set[firm_idx_temp] = ytsim2 
 
   }
    
  
  ### simul data for ACF(FE)
  pdataset = list( y=ytsim1set, 
                   begin_idx =firm_TS_data$firm_begin_deflated, end_idx = firm_TS_data$firm_end_deflated)
  ytsim1set = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
  pdataset = list( y=ytsim2set, 
                   begin_idx =firm_TS_data$firm_begin_deflated, end_idx = firm_TS_data$firm_end_deflated)
  ytsim2set = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )  
  yt_pooled = matrix(rbind(ytsim1set,ytsim2set),nrow=2) 
   
  
  ### simul data for 'AR(1) with mu learning' estimation
  pdataset = list( y1=ytsim1set  ,y2=ytsim2set,
                   begin_idx =firm_TS_data$firm_begin_deflated, end_idx = firm_TS_data$firm_end_deflated)
  temp_data = fcn_pooled_TS_make_mu_learning(pdataset  ) 
  
  
  temp_yt = list(yt=temp_data$yt,KF_idx= temp_data$KF_idx ,  yt_pooled=yt_pooled)
  
  
  
 return( temp_yt)
  
}
