fcn_VARMA_MLE_table  <- function(  fixedeffect) {  
   
  
  
  if (fixedeffect)  temp = "_fixedeffect" else temp = ""
  
    
  load(sprintf("KFfit_VARMA_MLE%s.Rdata", temp) )  
  MM_fit = KFfit$par # raw estimate from real data

   if (fixedeffect){
     KFfit$par["phi"]=KFfit$par["phi"]*1.07
     KFfit$par["phihat"]=KFfit$par["phihat"]*1.06
     KFfit$par["logsigmaa"] = KFfit$par["logsigmaa"] + log(0.9)
     KFfit$par["Khat"]=KFfit$par["Khat"]*1.2
   } 
 
  MM_theta2 = KFfit$par  # para values used in simulations 
  
theta_sim_all =c()
  
for (jjj in seq(1,4))  {
  load( sprintf("theta_sim_VARMA_MLE%s%d.Rdata",temp,jjj)) 
  theta_sim_all = cbind(theta_sim_all,theta_sim)
}
  
apply(theta_sim,1,median,na.rm=T)
apply(theta_sim,1,median,na.rm=T)

theta_sim = theta_sim_all 

 
 
Implist = fcn_implied_K_w_rho(theta_sim[3:9,]) 
estlist = rbind(theta_sim[3:6,], exp(theta_sim[7,]-theta_sim[8,]) , 
                exp(theta_sim[9,]-theta_sim[8,]),Implist$w,Implist$K,Implist$FE_rho   )
MM_esttheta_set = apply(estlist ,1,mean,na.rm=T)[ c(1,2,5,6,8,3,7,4,9)] 
MM_SEtheta= apply(estlist ,1, sd,na.rm=T) [ c(1,2,5,6,8,3,7,4,9)] 

# para values used in simulations  
MM_theta2 = as.numeric((MM_theta2[1:9]))
MM_truetheta =  c(MM_theta2[3:6], exp(MM_theta2[7]-MM_theta2[8]) , exp(MM_theta2[9]-MM_theta2[8]))
MM_truetheta_Imp = fcn_implied_K_w_rho(matrix(MM_theta2[3:9],ncol=1)) 
MM_truetheta_set = c(MM_truetheta, as.numeric(MM_truetheta_Imp))[ c(1,2,5,6,8,3,7,4,9)]

# raw estimate from real data
MM_fit_vec  = as.numeric((MM_fit[1:9]))
MM_fit_vec_Imp = fcn_implied_K_w_rho(matrix(MM_fit_vec[3:9]  ,ncol=1))
MM_fit_vec  = c(MM_fit_vec[3:6], exp(MM_fit_vec[7]-MM_fit_vec[8]) , exp(MM_fit_vec[9]-MM_fit_vec[8]))
MM_fit_vec_set = c(MM_fit_vec, as.numeric(MM_fit_vec_Imp))[ c(1,2,5,6,8,3,7,4,9)]

MM_bias = -(MM_truetheta_set-MM_esttheta_set) 
MM_fit_vec_bias_adj_set = MM_fit_vec_set - MM_bias  

MM_fit_vec_bias_adj = MM_fit_vec_bias_adj_set[c(1,2,6,8,3,4)]
save(MM_fit_vec_bias_adj  , file =   "theta_MM_fit_vec_bias_adj.Rdata") 

  
### table creat  
ps= matrix(rbind( MM_fit_vec_set,  MM_fit_vec_bias_adj_set  ),nrow=2) 
print('phi phihat sigmaa/sigmae sigman/sigmae K Khat w what rho')
print(matrix( sprintf('   %1$3.3f &  %2$3.3f &  %3$3.3f &  %4$3.3f &  %5$3.3f &  %6$3.3f &  %7$3.3f &  %8$3.3f &  %9$3.3f \\  '
                      ,ps[,1],ps[,2],ps[,3],ps[,4],ps[,5],ps[,6] ,ps[,7] ,ps[,8],ps[,9]    ), nrow=2 ))
  ps1=ps
ps= matrix(rbind( MM_SEtheta  ),nrow=1) 
print(  sprintf('(%1$1.4f) &  (%2$3.4f) &  (%3$3.4f) &  (%4$3.4f) &  (%5$3.4f) &  (%6$3.4f) &  (%7$3.4f) &  (%8$3.4f) &  (%9$3.4f) \\  '
                ,ps[,1],ps[,2],ps[,3],ps[,4],ps[,5],ps[,6] ,ps[,7] ,ps[,8],ps[,9]    ))
ps1=rbind(ps1,ps)
  
  ### table creat  
    
  sample.dataframe = data.frame( ps1 , row.names=NULL)
  row.names(sample.dataframe) = c("raw estimate", "bias-adjusted", "SE"  )
  colnames(sample.dataframe) = c("phi","phi_hat","sigma_a/sigma_e","sigma_n/sigma_e",
                                 "K","K_hat","w","w_hat","rho")
  
  
  write.csv(x = sample.dataframe, file = sprintf("Table4%s.csv",temp ))
  
  
  
}






