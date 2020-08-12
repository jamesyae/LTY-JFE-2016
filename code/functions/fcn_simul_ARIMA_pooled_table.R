fcn_simul_ARIMA_pooled_table  <- function(modelorder,theta_start, fixedeffect){  
  
  
if (fixedeffect) temp = "_fixedeffect" else temp = ""
load(sprintf("est_pooled_prc_cut1%s.Rdata",temp)) 
load( sprintf("theta_sim_AR%dMA%d%s.Rdata",modelorder[1],modelorder[3],temp) )

if (modelorder[1]*(1-modelorder[3])==1){
bias_adj_ar1_phi = est1$ar1$phi + (theta_start$phi - median(theta_sim[1,]))
SE_ar1_phi = sd(theta_sim[1,])
print( c(bias_adj_ar1_phi, SE_ar1_phi) )
}

if (modelorder[3]*(1-modelorder[1])==1){
bias_adj_ma1_theta = est1$ma1$matheta + (theta_start$ma  - median(theta_sim[2,]) )
SE_ma1_theta = sd(theta_sim[2,])
print( c(bias_adj_ma1_theta,SE_ma1_theta ))
}


if (modelorder[1]*modelorder[3]==1){

  bias_adj_arma11_phi = est1$arma11$phi + (theta_start$phi - median(theta_sim[1,]))
  SE_arma11_phi = sd(theta_sim[1,])
  bias_adj_arma11_ma = est1$arma11$matheta + (theta_start$ma - median(theta_sim[2,]))
  SE_arma11_ma = sd(theta_sim[2,])
  
  impsigmaa = as.numeric( sqrt(-1*theta_sim[2, ]*(theta_sim[2, ]<0)/theta_sim[1, ] )) 
  impsigmae = as.numeric(sqrt(1 -(1+theta_sim[1, ]+theta_sim[1, ]^2)*impsigmaa^2 ))
  bias_adj_sigmaa_sigmae  = est_sigmaa_sigmae + (theta_start$sigmaa_sigmae - median(impsigmaa/impsigmae))
   
  print( c(bias_adj_arma11_phi,SE_arma11_phi ))
  print( c(bias_adj_arma11_ma,SE_arma11_ma ))
  print( c(bias_adj_sigmaa_sigmae, sd(impsigmaa/impsigmae) ))
  
  impsigmaa = as.numeric( sqrt(-1*bias_adj_arma11_ma*(bias_adj_arma11_ma<0)/bias_adj_arma11_phi )) 
  impsigmae = as.numeric(sqrt(1 -(1+bias_adj_arma11_phi+bias_adj_arma11_phi^2)*impsigmaa^2 ))
  print(impsigmaa/impsigmae)
  
  
}

}



 