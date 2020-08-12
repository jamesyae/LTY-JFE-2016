fcn_firm_level_simul_table_KF <- function( est) {  
  
  
  
  
##### TABLE2C

summary_set=matrix(rep(NA,8*9), ncol=9 )

tempidx = est$arma11KF$loglik > est$ar1KF$loglik

x=est$ar1KF$phi
summary_set[1:2,1 ] = c(mean(x,na.rm=T)  , median(x,na.rm=T)  ) 

x = est$arma11KF$phi*tempidx   + est$ar1KF$phi*(!tempidx)  
summary_set[4:5,1 ] = c(mean(x,na.rm=T)  , median(x,na.rm=T)  ) 

x = est$arma11KF$matheta*tempidx   +0*(!tempidx)  
summary_set[7:8,1 ] = c(mean(x,na.rm=T)  , median(x,na.rm=T)  ) 


load("firm_simul_ar48_sigasige0_KF.Rdata")
jj=3
tempidx = !firmsimul$reverse_idx

x= firmsimul$ar1temp
summary_set[1:2,jj ] = c(mean(x,na.rm=T)  , median(x,na.rm=T)  ) 

x = firmsimul$artemp*tempidx + firmsimul$ar1temp*(!tempidx)
summary_set[4:5,jj ] = c(mean(x,na.rm=T)  , median(x,na.rm=T)  ) 

x = firmsimul$matemp*tempidx + 0*(!tempidx)
summary_set[7:8,jj ] = c(mean(x,na.rm=T)  , median(x,na.rm=T)  ) 



load("firm_simul_ar48_sigasige5_KF.Rdata")
jj=jj+1
tempidx = !firmsimul$reverse_idx

x= firmsimul$ar1temp
summary_set[1:2,jj ] = c(mean(x,na.rm=T)  , median(x,na.rm=T)  ) 

x = firmsimul$artemp*tempidx + firmsimul$ar1temp*(!tempidx)
summary_set[4:5,jj ] = c(mean(x,na.rm=T)  , median(x,na.rm=T)  ) 

x = firmsimul$matemp*tempidx + 0*(!tempidx)
summary_set[7:8,jj ] = c(mean(x,na.rm=T)  , median(x,na.rm=T)  ) 



load("firm_simul_ar48_sigasige10_KF.Rdata")
jj=jj+1
tempidx = !firmsimul$reverse_idx

x= firmsimul$ar1temp
summary_set[1:2,jj ] = c(mean(x,na.rm=T)  , median(x,na.rm=T)  ) 

x = firmsimul$artemp*tempidx + firmsimul$ar1temp*(!tempidx)
summary_set[4:5,jj ] = c(mean(x,na.rm=T)  , median(x,na.rm=T)  ) 

x = firmsimul$matemp*tempidx + 0*(!tempidx)
summary_set[7:8,jj ] = c(mean(x,na.rm=T)  , median(x,na.rm=T)  ) 




load("firm_simul_ar48_sigasige15_KF.Rdata")
jj=jj+1
tempidx = !firmsimul$reverse_idx

x= firmsimul$ar1temp
summary_set[1:2,jj ] = c(mean(x,na.rm=T)  , median(x,na.rm=T)  ) 

x = firmsimul$artemp*tempidx + firmsimul$ar1temp*(!tempidx)
summary_set[4:5,jj ] = c(mean(x,na.rm=T)  , median(x,na.rm=T)  ) 

x = firmsimul$matemp*tempidx + 0*(!tempidx)
summary_set[7:8,jj ] = c(mean(x,na.rm=T)  , median(x,na.rm=T)  ) 


load("firm_simul_ar48_sigasige20_KF.Rdata")
jj=jj+1
tempidx = !firmsimul$reverse_idx

x= firmsimul$ar1temp
summary_set[1:2,jj ] = c(mean(x,na.rm=T)  , median(x,na.rm=T)  ) 

x = firmsimul$artemp*tempidx + firmsimul$ar1temp*(!tempidx)
summary_set[4:5,jj ] = c(mean(x,na.rm=T)  , median(x,na.rm=T)  ) 

x = firmsimul$matemp*tempidx + 0*(!tempidx)
summary_set[7:8,jj ] = c(mean(x,na.rm=T)  , median(x,na.rm=T)  ) 

load("firm_simul_ar48_sigasige25_KF.Rdata")
jj=jj+1
tempidx = !firmsimul$reverse_idx

x= firmsimul$ar1temp
summary_set[1:2,jj ] = c(mean(x,na.rm=T)  , median(x,na.rm=T)  ) 

x = firmsimul$artemp*tempidx + firmsimul$ar1temp*(!tempidx)
summary_set[4:5,jj ] = c(mean(x,na.rm=T)  , median(x,na.rm=T)  ) 

x = firmsimul$matemp*tempidx + 0*(!tempidx)
summary_set[7:8,jj ] = c(mean(x,na.rm=T)  , median(x,na.rm=T)  ) 

load("firm_simul_ar48_sigasige30_KF.Rdata")
jj=jj+1
tempidx = !firmsimul$reverse_idx

x= firmsimul$ar1temp
summary_set[1:2,jj ] = c(mean(x,na.rm=T)  , median(x,na.rm=T)  ) 

x = firmsimul$artemp*tempidx + firmsimul$ar1temp*(!tempidx)
summary_set[4:5,jj ] = c(mean(x,na.rm=T)  , median(x,na.rm=T)  ) 

x = firmsimul$matemp*tempidx + 0*(!tempidx)
summary_set[7:8,jj ] = c(mean(x,na.rm=T)  , median(x,na.rm=T)  ) 

arsimulset=c(NA,NA,0.48,0.48,0.48,0.48,0.48,0.48,0.48)
sigaesimulset=c(NA,NA,0,0.05,0.1,0.15,0.2,0.25,0.3 )
a=1;b=(1+arsimulset^2)/arsimulset + 1/sigaesimulset^2/arsimulset;c=1
masimulset = (-b+sqrt(b^2-4*a*c))/2/a
masimulset[3] = 0

summary_set = (rbind(arsimulset,sigaesimulset,masimulset,rep(NA,9),summary_set))

sample.dataframe = data.frame( summary_set , row.names=NULL)
row.names(sample.dataframe) = c("phi","sigma_a/sigma_e","theta","       ","phi in AR(1) Mean","Median", "",
                                "phi in  ARMA(1,1) Mean ","Median ", " ",
                                "theta in ARMA(1,1) Mean  ","Median  "
)
colnames(sample.dataframe) = c("Data","","Simul 1","Simul 2","Simul 3","Simul 4","Simul 5","Simul 6","Simul 7")



write.csv(x = sample.dataframe, file = "Table2C_KF.csv" )

}

