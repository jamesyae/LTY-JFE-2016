fcn_VARMA11_LTY_random_data_firm_level<- function(scaleVset, option , KFfit ) {
  
  filename = fcn_filename_maker(scaleVset=scaleVset, option=option )
  load(file=filename)
   
  
  Tsim = length(firm_TS_data$ye_firm_deflated) + 50
  ytsim1  = rep(NA, Tsim   )
  ytsim2 = ytsim1 
   
      
      if (runif(1)>0.5) { 
      et_sim = rnorm(Tsim)*exp(KFfit$par["logsigmae"])
      at_sim = rnorm(Tsim)*exp(KFfit$par["logsigmaa"])
      nt_sim = rnorm(Tsim)*exp(KFfit$par["logsigman"])
      } else {
        nt_sim = rnorm(Tsim)*exp(KFfit$par["logsigman"]) 
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
        xtsim1     = KFfit$par["phi"]*xtsim0 + et_sim[ii] 
        ytsim1[ii] = KFfit$par["mue"] + xtsim1 + at_sim[ii] 
        ytsim2[ii] = KFfit$par["muehat"] + KFfit$par["phihat"]*(KFfit$par["Khat"]*ytsim1[ii-1] +
                      (1-KFfit$par["Khat"])*ytsim2[ii-1] ) + KFfit$par["what"]*(et_sim[ii] + nt_sim[ii])
        xtsim0=xtsim1
      }
      
      ytsim1  = ytsim1[51:Tsim]
      ytsim2  = ytsim2[51:Tsim]
      
      naidx = is.na(firm_TS_data$ye_firm_deflated )
      ytsim1[naidx] = NA
      
      naidx = is.na(firm_TS_data$yf_firm_deflated )
      ytsim2[naidx] = NA
 
 
 
 
     
  yt = matrix(rbind(ytsim1 ,ytsim2 ),nrow=2) 
  
 return(yt)
  
}