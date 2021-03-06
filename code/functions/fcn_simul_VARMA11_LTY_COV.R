fcn_simul_VARMA11_LTY_COV  <- function(  scaleVset, option  ,   fixedeffect  ,  KFfit, MM, simulnumber, equicorr=equicorr){  
  
  


theta_sim = matrix(rep(NA,9*MM),nrow=9)  

for (iii in seq(1,MM)){
  
  yt = fcn_VARMA11_LTY_random_data_COV( scaleVset=scaleVset, option=option, KFfit=KFfit, fixedeffect=fixedeffect, equicorr=equicorr)
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
  
  if (fixedeffect) temp = "_fixedeffect" else temp = ""
  filename = sprintf("theta_sim_VARMA_MLE_COV_corr%d%s_%d.Rdata",equicorr*100, temp, simulnumber) 
  save(theta_sim, file = filename) 
  print(iii)  
}
}
 

