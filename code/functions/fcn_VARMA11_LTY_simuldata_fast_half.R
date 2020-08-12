fcn_VARMA11_LTY_simuldata_fast_half <- function(naidx, simpar ) { 
  
  mue = simpar["mue"]      
  muehat = simpar["muehat"]         
  phi = simpar["phi"]     
  phihat = simpar["phihat"]        
  Khat = simpar["Khat"]      
  what = simpar["what"]   
  sigmaa = exp(simpar["logsigmaa"]   )
  sigmae = exp(simpar["logsigmae"]   )
  sigman = exp(simpar["logsigman"]   )
  
  
  Tsim = length(naidx)
  yesim= rep(NA, Tsim )
  yfsim= rep(NA, Tsim )
  
  yesim[1] = simpar["mue"] + rnorm(1)*sigmae
  yfsim[1] =simpar["muehat"] + yesim[1]-simpar["mue"]
  atsim_L1 = rnorm(1)*sigmaa
  for (ii  in seq(2, Tsim ))
  {            
    atsim = rnorm(1)*sigmaa
    etsim = rnorm(1)*sigmae
    ntsim = rnorm(1)*sigman
    
    yesim[ii] = mue*(1-phi) + phi*yesim[ii-1] + etsim + atsim - phi*atsim_L1        
    yfsim[ii] = muehat*(1-phihat) + phihat*(Khat*yesim[ii-1] + (1-Khat)*yfsim[ii-1] ) + what*(etsim + ntsim)
    atsim_L1 = atsim 
  }
  
  yesim[naidx] = NA      
  yfsim[naidx] = NA
  
  
  
  NN = length(firm_TS_data$firm_begin_deflated)
  
  sample_idx = sample.int(NN, size = round(NN/2), replace = TRUE)
  
  
  pdataset = list( y=yesim, 
                   begin_idx = firm_TS_data$firm_begin_deflated[sample_idx], 
                   end_idx   = firm_TS_data$firm_end_deflated[sample_idx])
  ytsim1set = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  ) 
  pdataset = list( y=yfsim , 
                   begin_idx = firm_TS_data$firm_begin_deflated[sample_idx], 
                   end_idx   = firm_TS_data$firm_end_deflated[sample_idx])
  ytsim2set = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  ) 
  
  
  yt_sim = matrix(rbind(ytsim1set,ytsim2set),nrow=2) 
  
  
  
   
  
  return(yt_sim)
  
}