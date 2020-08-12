fcn_simul_ARIMA_pooled_bootstrap  <- function(modelorder, scaleVset, option  , fixedeffect ,   MM){  
  

theta_sim = matrix(rep(NA,2*MM),nrow=2)

for (iii in seq(1,MM)){   
  yyy1 = fcn_arima_LTY_random_data_BT( scaleVset=scaleVset, option=option  , fixedeffect=fixedeffect   )  
  m1 = arima(yyy1, order=modelorder, method = c("ML"),optim.control = list(maxit = 300000 ))
  theta_sim[1,iii] = m1$coef[1]*modelorder[1]
  theta_sim[2,iii] = m1$coef[2]*(modelorder[1]*modelorder[3]) +  m1$coef[1]*(1-modelorder[1])*modelorder[3]
  print(iii) 
}

if (fixedeffect) temp = "_fixedeffect" else temp = ""
filename = sprintf("theta_sim_BT_AR%dMA%d%s.Rdata",modelorder[1],modelorder[3],temp) 
save(theta_sim, file = filename)

}


 