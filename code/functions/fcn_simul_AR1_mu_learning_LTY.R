fcn_simul_AR1_mu_learning_LTY  <- function(scaleVset, option, KFfit, FE_rho_do , est_do  , MM, simulnumber){  
  
  
FE_rho_sim = rep(NA, MM)
theta_sim = matrix(rep(NA,11*MM),nrow=11)  

for (iii in seq(1,MM)){
  
  temp_yt = fcn_AR1_mu_learning_LTY_random_data( scaleVset=scaleVset, option=option, KFfit=KFfit )
   
  if (FE_rho_do) {
    FE_rho_sim[iii] =  arima( temp_yt$yt_pooled[1,]-temp_yt$yt_pooled[2,], c(1,0,0))$coef[1]  
    filename = sprintf("theta_sim_AR1_mu_learning_FE_rho%d.Rdata",  simulnumber) 
    save(FE_rho_sim, file = filename) 
  }
  
  
  if (est_do ) {
    KFfitsim = fcn_KF_AR1_mu_learning_LTY_est( temp_yt$yt, temp_yt$KF_idx)
    
    theta_sim[1,iii] = KFfitsim$par["mu0"]      
    theta_sim[2,iii] = KFfitsim$par["muhat0"]         
    theta_sim[3,iii] = KFfitsim$par["phi"]     
    theta_sim[4,iii] = KFfitsim$par["phihat"]        
    theta_sim[5,iii] = KFfitsim$par["b"]      
    theta_sim[6,iii] = KFfitsim$par["what"]   
    theta_sim[7,iii] = KFfitsim$par["logpihats2"]   
    theta_sim[8,iii] = KFfitsim$par["logsigmae"]   
    theta_sim[9,iii] = KFfitsim$par["logsigman"] 
    theta_sim[10,iii] = KFfitsim$par["logsigmamuhat0"]   
    theta_sim[11,iii] = KFfitsim$par["logsigmamu0"]   
     
    filename = sprintf("theta_sim_AR1_mu_learning_MLE%d.Rdata",  simulnumber) 
    save(theta_sim, file = filename) 
  }
   
  print(iii)  
}

}
 

