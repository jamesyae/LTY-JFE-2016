fcn_VARMA11_LTY_random_data_firm_level_heterophi<- function(scaleVset, option , KFfit, cov_phi_est,phiset ,phihatset ) {
  filename = fcn_filename_maker(scaleVset=scaleVset, option=option )
  load(file=filename)
  
  JJ=length(firm_TS_data$firm_begin_deflated)

  
  
  
  ytsim1set = rep(NA, length(firm_TS_data$ye_firm_deflated ) )
  ytsim2set = ytsim1set 
  
  
  for (jj in seq( 1 ,   JJ))
  {  
    
    firm_idx_temp = firm_TS_data$firm_begin_deflated[jj]:firm_TS_data$firm_end_deflated[jj]  
    
    Tsim = length(firm_idx_temp) + 50
    
    if (runif(1)>0.5) {
      #set.seed( round(runif(1)*10^6)   )   
      et_sim = rnorm(Tsim)*exp(KFfit$par["logsigmae"])
      
      #set.seed( round(runif(1)*10^6)   )   
      at_sim = rnorm(Tsim)*exp(KFfit$par["logsigmaa"])
      
      #set.seed( round(runif(1)*10^6)   )   
      nt_sim = rnorm(Tsim)*exp(KFfit$par["logsigman"])
    } else {
      
      #set.seed( round(runif(1)*10^6)   )   
      nt_sim = rnorm(Tsim)*exp(KFfit$par["logsigman"]) 
      
      #set.seed( round(runif(1)*10^6)   )   
      at_sim = rnorm(Tsim)*exp(KFfit$par["logsigmaa"])
      
      et_sim = rnorm(Tsim)*exp(KFfit$par["logsigmae"])  
      
    }
    
    
    ytsim1= rep(NA, Tsim )
    ytsim2= rep(NA, Tsim )
    
    xtsim0 = 0
    ytsim1[1] = KFfit$par["mue"] + xtsim0+at_sim[1]
    ytsim2[1] =  KFfit$par["muehat"] 
    for (ii  in seq(2, Tsim ))
    {    
      xtsim1     = phiset[jj]*xtsim0 + et_sim[ii] 
      ytsim1[ii] = KFfit$par["mue"] + xtsim1 + at_sim[ii] 
      ytsim2[ii] = KFfit$par["muehat"] + phihatset[jj]*(KFfit$par["Khat"]*ytsim1[ii-1] +
                                                                (1-KFfit$par["Khat"])*ytsim2[ii-1] ) + KFfit$par["what"]*(et_sim[ii] + nt_sim[ii])
      xtsim0=xtsim1
    }
    
 

  
    ytsim1set[firm_idx_temp] = ytsim1[51:Tsim]
    ytsim2set[firm_idx_temp] = ytsim2[51:Tsim]
    
  }
  
  naidx = is.na(firm_TS_data$ye_firm_deflated )
  ytsim1set[naidx] = NA 
  ytsim2set[naidx] = NA
 
  yt = matrix(rbind(ytsim1set,ytsim2set),nrow=2) 
  
  return(yt)
  
}
