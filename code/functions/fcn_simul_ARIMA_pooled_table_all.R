fcn_simul_ARIMA_pooled_table_all  <- function(  theta_start_all,fixedeffect){  
  
modelorder=c(1,0,0)
theta_start = theta_start_all$AR1
if (fixedeffect) temp = "_fixedeffect" else temp = ""
load(sprintf("est_pooled_prc_cut1%s.Rdata",temp)) 
load( sprintf("theta_sim_AR%dMA%d%s.Rdata",modelorder[1],modelorder[3],temp) )

raw_ar1_phi =  est1$ar1$phi
bias_adj_ar1_phi = est1$ar1$phi + (theta_start$phi - median(theta_sim[1,]))
SE_ar1_phi = sd(theta_sim[1,])
print( c(bias_adj_ar1_phi, SE_ar1_phi) )
 

modelorder=c(0,0,1)
theta_start = theta_start_all$MA1
if (fixedeffect) temp = "_fixedeffect" else temp = ""
load(sprintf("est_pooled_prc_cut1%s.Rdata",temp)) 
load( sprintf("theta_sim_AR%dMA%d%s.Rdata",modelorder[1],modelorder[3],temp) )

raw_ma1_theta =  est1$ma1$matheta
bias_adj_ma1_theta = est1$ma1$matheta + (theta_start$ma  - median(theta_sim[2,]) )
SE_ma1_theta = sd(theta_sim[2,])
print( c(bias_adj_ma1_theta,SE_ma1_theta ))
 

modelorder=c(1,0,1)
theta_start = theta_start_all$AR1MA1
if (fixedeffect) temp = "_fixedeffect" else temp = ""
load(sprintf("est_pooled_prc_cut1%s.Rdata",temp)) 
load( sprintf("theta_sim_AR%dMA%d%s.Rdata",modelorder[1],modelorder[3],temp) )

raw_arma11_phi =  est1$arma11$phi
raw_arma11_matheta =  est1$arma11$matheta
  bias_adj_arma11_phi = est1$arma11$phi + (theta_start$phi - median(theta_sim[1,]))
  SE_arma11_phi = sd(theta_sim[1,])
  bias_adj_arma11_ma = est1$arma11$matheta + (theta_start$ma - median(theta_sim[2,]))
  SE_arma11_ma = sd(theta_sim[2,])
  

impsigmaa = as.numeric( sqrt(-1*raw_arma11_matheta*(raw_arma11_matheta<0)/raw_arma11_phi)) 
impsigmae = as.numeric(sqrt(1 +raw_arma11_matheta^2 -(1 +raw_arma11_phi^2)*impsigmaa^2 ))
raw_sigmaa_sigmae = (impsigmaa/impsigmae)

  impsigmaa = as.numeric( sqrt(-1*theta_sim[2, ]*(theta_sim[2, ]<0)/theta_sim[1, ] )) 
  impsigmae = as.numeric(sqrt(1+ theta_sim[2, ]^2 -(1+ theta_sim[1, ]^2)*impsigmaa^2 ))
  bias_adj_sigmaa_sigmae  = est_sigmaa_sigmae + (theta_start$sigmaa_sigmae - median(impsigmaa/impsigmae))
  SE_arma11_sigmaa_sigmae  = sd(impsigmaa/impsigmae)
   
  print( c(bias_adj_arma11_phi,SE_arma11_phi ))
  print( c(bias_adj_arma11_ma,SE_arma11_ma ))
  print( c(bias_adj_sigmaa_sigmae, sd(impsigmaa/impsigmae) ))
  
  impsigmaa = as.numeric( sqrt(-1*bias_adj_arma11_ma*(bias_adj_arma11_ma<0)/bias_adj_arma11_phi )) 
  impsigmae = as.numeric(sqrt(1+bias_adj_arma11_ma^2-(1+bias_adj_arma11_phi^2)*impsigmaa^2 ))
  print(impsigmaa/impsigmae)
  
summary_set_all = cbind(as.numeric(c(raw_ar1_phi,bias_adj_ar1_phi,SE_ar1_phi,NA,NA,NA,raw_arma11_phi, bias_adj_arma11_phi, SE_arma11_phi,NA)),
                      as.numeric( c(NA,NA,NA,  raw_ma1_theta,bias_adj_ma1_theta,SE_ma1_theta,NA,NA,NA,NA)), 
                     as.numeric( c(NA,NA,NA, NA,NA,NA, raw_arma11_matheta,bias_adj_arma11_ma,SE_arma11_ma ,NA)),
                        as.numeric( c(NA,NA,NA, NA,NA,NA, raw_sigmaa_sigmae,bias_adj_sigmaa_sigmae ,SE_arma11_sigmaa_sigmae ,NA)),
                     as.numeric( c(est1$ar1$R2KF, NA, NA,est1$ma1$R2KF, NA, NA,est1$arma11$R2KF, NA, NA,est1$feR2 )),
                     as.numeric(  c(est1$ar1$aic, NA, NA,est1$ma1$aic, NA, NA,est1$arma11$aic, NA, NA,NA )))
                        

sample.dataframe = data.frame(  summary_set_all)
row.names(sample.dataframe) = c("AR(1)",""," ","MA(1)","  ","       ","ARMA(1,1)","   ","    ","median forecast")
colnames(sample.dataframe) = c("Model","phi","theta","sigma_a/sigma_e","Pseudo_R2","AIC")

write.csv(x = sample.dataframe, file = sprintf("Table3%s.csv", temp ))



}



 