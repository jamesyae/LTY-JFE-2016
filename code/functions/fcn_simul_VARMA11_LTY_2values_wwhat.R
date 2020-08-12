fcn_simul_VARMA11_LTY_2values_wwhat<- function(scaleVset, option , KFfit, MM,TT,wset,whatset,simulnumber   ) {
  
  filename = fcn_filename_maker(scaleVset=scaleVset, option=option )
  load(file=filename)
   
  erw    = c(rep(wset[1], TT/2), rep(wset[2], TT/2))
  erwhat =  c(rep(whatset[1], TT/2),rep(whatset[2], TT/2))
  
   
  
  theta_sim = matrix(rep(NA,10*MM),nrow=10)  
  
  for (iii in seq(1,MM)){
    
    ytsim1 = rep(NA, TT)
    ytsim2 = ytsim1   
    
           
        
        if (runif(1)>0.5) {
        #set.seed( round(runif(1)*10^6)   )   
        et_sim = rnorm(TT)*exp(KFfit$par["logsigmae"])
        at_sim = rnorm(TT)*exp(KFfit$par["logsigmaa"])
        nt_sim = rnorm(TT)*exp(KFfit$par["logsigman"])
        } else {
          
          nt_sim = rnorm(TT)*exp(KFfit$par["logsigman"]) 
          at_sim = rnorm(TT)*exp(KFfit$par["logsigmaa"])
          et_sim = rnorm(TT)*exp(KFfit$par["logsigmae"])  
       }
    
        
        xtsim0 = 0
        ytsim1[1] = KFfit$par["mue"] + xtsim0+at_sim[1]
        ytsim2[1] =  KFfit$par["muehat"] 
    
    
    
    
        for (ii  in seq(2, TT ))
        {           
          xtsim1     =  KFfit$par["phi"]*xtsim0 + et_sim[ii] 
          ytsim1[ii] = KFfit$par["mue"] + xtsim1 + at_sim[ii] 
          ytsim2[ii] = KFfit$par["muehat"] + KFfit$par["phihat"]*(KFfit$par["Khat"]*ytsim1[ii-1] +
                        (1-KFfit$par["Khat"])*ytsim2[ii-1] ) + erwhat[ii]*(et_sim[ii] + nt_sim[ii])
          xtsim0=xtsim1
        }
   
      
        
 
   yt = matrix(rbind(ytsim1,ytsim2),nrow=2) 
  

    KFfitsim = fcn_KF_VARMA11_LTY_est(yt, mueest=TRUE)
    
    theta_sim[1,iii] = KFfitsim$par["mue"]      
    theta_sim[2,iii] = KFfitsim$par["muehat"]         
    theta_sim[3,iii] = KFfitsim$par["phi"]     
    theta_sim[4,iii] = KFfitsim$par["phihat"]        
    theta_sim[5,iii] = KFfitsim$par["Khat"]      
    theta_sim[6,iii] = KFfitsim$par["what"]   
    theta_sim[7,iii] = KFfitsim$par["logsigmaa"]   
    theta_sim[8,iii] = KFfitsim$par["logsigmae"]   
    theta_sim[9,iii] = KFfitsim$par["logsigman"] 
    theta_sim[10,iii] = arima(ytsim1-ytsim2,c(1,0,0))$coef[1]
     
    filename = sprintf("theta_sim_VARMA_MLE_2values_wwhat_%d.Rdata", simulnumber ) 
    save(theta_sim, file = filename) 
    print(iii)  
  }
  
}


