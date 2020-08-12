do_all = FALSE

filename = fcn_filename_maker(scaleVset=scaleVset, option=option )
load(file=filename)  

TS_range = range(firm_TS_data$year_qtr_idx, na.rm=T )  
TS_row = seq(TS_range[1], TS_range[2], by=0.25)  
N_row_TS = length( TS_row )  
N_row_TS_age = max(firm_TS_data$TS_index_firm_deflated,na.rm=T)  
est = firm_TS_data
yef_temp = est$ye_firm_deflated -est$yf_firm_deflated 
NA_idx_temp = is.na(  yef_temp ) 

##############################################################################
########################################################################
#######  I. variation mu-muhat (including variation in pessimism) #######
########################################################################
arima(arima.sim(list(order = c(1,0,0), ar = 0.5), n = 25000)[rep(c(rep(TRUE,4),rep(FALSE,21)),1000 )] ,c(1,0,0))

##############################################################################
## Autocorr(FE) benchmark. AR(1) fit. Pooled/Firmlevel_bias_adj
# Pooled autocorr(FE) = 0.258 
# Mean of firmlevel autocorr(FE) biase adjusted = 0.203
 

############ plot ##################################################################


x1=+0.05+rnorm(20)/15; 
x2=-0.05+rnorm(20)/15;  

m0=summary(lm(c(x1[2:20],x2[2:20])~c(x1[1:19],x2[1:19])))

par(mfrow=c(1,2))
symbols(c(-1,1)/20, c(-1,1)/20, circles=rep(1,2)/5, inches=FALSE, bg = c("gray",NA),
       fg="gray30",  xlim=c(-2,2)/6,ylim=c(-2,2)/6 ,
       main=expression("Varying E[FE"[t]*"|group i]"), xlab=expression("FE"[t]), ylab=expression("FE"[t+1]) )
lines( x1[1:19],x1[2:20], type="b", pch=1, lty=2 )
lines( x2[1:19],x2[2:20], type="b", pch=16 ) 
abline(h=1/20, lty=2)
abline(h=-1/20)
lines(c(-1,1), m0$coef[1] + m0$coef[2]*c(-1,1),  lwd=4, col="red" )
  

sdmusde = seq(0,0.2,by=0.001)
plot(sdmusde, sdmusde^2, type="l" ,
     xlab=expression(paste("SD[", mu[i], "] / SD[FE]") ),ylab="",main="Autocorr(FE)")



########## sample moment ##################################




pdataset = list( y=yef_temp , 
                 begin_idx =est$firm_begin_deflated, end_idx = est$firm_end_deflated)
zzz = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
m1 = arima( zzz    , order=c(1,0,0), method = c("ML"),optim.control = list(maxit = 300000 ))   
FErhopooled= m1$coef[1]
# Pooled autocorr(FE) = 0.258 
pdataset = list( y=yef_temp , 
                 begin_idx =est$firm_begin_deflated, end_idx = est$firm_end_deflated)
zzz = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
 arima( zzz    , order=c(1,0,1), method = c("ML"),optim.control = list(maxit = 300000 ))  


# use OLS (drop observation with adjacent missing values)
x = yef_temp 
y = x 
ylag1 = c(NA, x[1:(length(x)-1) ])
ylag1[est$firm_begin_deflated]=NA
lm(y~ylag1)$coef[2]  
0.2165866
0.2165866 - 0.2123966
0.2165866  -0.1872098
# use OLS (drop observation with adjacent missing values)
rho_firms_OLS =rep(NA, length(est$firm_begin_deflated ))    
rho_firms_OLS_adj =rep(NA, length(est$firm_begin_deflated ))    
for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
{  
  if (!is.na(est$firm_begin_deflated[jj])) { 
    firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj] 
    x = yef_temp[firm_idx_temp]
    y = x[2:length(x) ]
    ylag1 = x[1:(length(x)-1) ]
    m1=lm(y~ylag1)
    rho_firms_OLS[jj] = m1$coef[2]  
    rho_firms_OLS_adj[jj] = ((m1$coef[2] *(sum(!is.na(y+ylag1))-1)+1)/(sum(!is.na(y+ylag1))-4))     
  } 
}
mean( rho_firms_OLS_adj )
# 0.2123966
median( rho_firms_OLS_adj)
# 0.1872098
sd( rho_firms_OLS_adj )
# 0.4561528

# use ARIMA
rho_firms =rep(NA, length(est$firm_begin_deflated ))    
for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
{  
  if (!is.na(est$firm_begin_deflated[jj])) { 
    firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]  
    m1=arima( yef_temp[firm_idx_temp]  , order=c(1,0,0), method = c("ML"),optim.control = list(maxit = 300000 )) 
    rho_firms[jj] = m1$coef[1]  
  } 
}
FErhoadj = ((rho_firms*(est$no_obs_deflated-1)+1)/(est$no_obs_deflated-4)) 
avgFErhofirm = mean(FErhoadj)
median(FErhoadj)
sd(FErhoadj)
# Mean of firmlevel autocorr(FE) biase adjusted = 0.203


 
########## simulatoin ##################################
if (do_all)
{
  # Simulation of mean of rho(FE)
  NN = 1000 
  avgFErhofirm_sim =rep(NA,NN)
  for (ii in seq(  1 , NN ) ) { 
    rho_firms =rep(NA, length(est$firm_begin_deflated ))    
    for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
    {  
      if (!is.na(est$firm_begin_deflated[jj])) {
        
        firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj] 
        zzz = arima.sim(list(order = c(1,0,0), ar = 0.205   ), n = length(firm_idx_temp))
        zzz[ is.na(  yef_temp[firm_idx_temp]   )] = NA
        m1=arima(  zzz     , order=c(1,0,0), method = c("ML"),optim.control = list(maxit = 300000 ))       
        rho_firms[jj] = m1$coef[1] 
      } 
    }
    FErhoadj = ((rho_firms*(est$no_obs_deflated-1)+1)/(est$no_obs_deflated-4)) 
    avgFErhofirm_sim[ii] = mean(FErhoadj)
    print(ii)
  }
  
  
  # Simulation of pooled rho(FE)
  
  NN = 1000 
  SD_firmeffect_set = c(0, 0.15, 0.27)
  FErhopooled_sim = matrix(rep(NA,NN*3), nrow=3)
  for (jj in c(1:3) ) { 
    SD_firmeffect = SD_firmeffect_set[jj] 
    for (ii in seq(  1 , NN ) ) { 
      zzz = arima.sim(list(order = c(1,0,0), ar = 0.205   ), n = length(yef_temp )) 
      zzz[ is.na(  yef_temp    )] = NA
      firm_effect_sim = SD_firmeffect * rnorm( length(est$firm_begin_deflated) )
      firm_effect_sim = firm_effect_sim[est$firm_deflated_nn_idx]
      zzz = firm_effect_sim + zzz
      
      pdataset = list(y=zzz, begin_idx =est$firm_begin_deflated, end_idx = est$firm_end_deflated)
      zzz = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
      m1 = arima( zzz, order=c(1,0,0), method = c("ML"),optim.control = list(maxit = 300000 ))   
      FErhopooled_sim[jj,ii] = m1$coef[1]
      print(ii)
    } 
  }
  
  # save
  FErho_firmeffect_sim_result =list(FErhopooled=FErhopooled, avgFErhofirm=avgFErhofirm, avgFErhofirm_sim=avgFErhofirm_sim,
                                    SD_firmeffect_set=SD_firmeffect_set,FErhopooled_sim=FErhopooled_sim)
  save(FErho_firmeffect_sim_result   , file="FErho_firmeffect_sim_result.Rdata")        
}
0.27/sqrt(0.27^2 + 1/(1-0.205^2))






############ plot ##################################################################



par(mfrow=c(1,3))

## 1) calendar-time variation...(a) by mean...(b) by quantile
#(a) by mean
load( "mu_muhat_quaterly.Rdata") 
ssd = 2.58
xxx = mu_muhat_quaterly$yef_stat_qtr
avgFE = xxx[,1]/mean(xxx[,3])
CIlow = (xxx[,1]-xxx[,3]*ssd/sqrt(xxx[,4])   )/mean(xxx[,3])
CIhigh = (xxx[,1]+xxx[,3]*ssd/sqrt(xxx[,4])  )/mean(xxx[,3])
xaxis.year = as.numeric(levels(factor(firm_TS_data$year_qtr_idx)))
plot(NA,xlim=c(xaxis.year[1], xaxis.year[length(xaxis.year)]),  ylim=c(-0.45, 0.35),main="Avg(FE)", xlab="Year",ylab="")
polygon( c(xaxis.year, xaxis.year[length(xaxis.year):1]  ),  
         c(CIlow, CIhigh[length(xaxis.year):1]), border = NA, col = "gray")
lines(xaxis.year, avgFE, cex=0.5, lwd=1  ,type="l" )
lines(xaxis.year, avgFE, cex=0.5, lwd=1  ,type="b" )

## 2) age-time variation......(a) alone (b) cleandar-age at the same time
# (a) alone
load( "mu_muhat_quaterly_age.Rdata") 
ssd = 2.58
xxx = mu_muhat_quaterly_age$yef_stat_qtr
avgFE = xxx[,1]/mean(xxx[,3])
CIlow = (xxx[,1]-xxx[,3]*ssd/sqrt(xxx[,4])   )/mean(xxx[,3])
CIhigh = (xxx[,1]+xxx[,3]*ssd/sqrt(xxx[,4])  )/mean(xxx[,3])
xaxis.age = as.numeric(levels(factor(firm_TS_data$TS_index_firm_deflated)))
plot(NA,xlim=c(xaxis.age[1], xaxis.age[length(xaxis.year)]), ylim=c(-0.15, 0.5),main="Avg(FE)", xlab="Quarter since first forecast",ylab="")
polygon( c(xaxis.age, xaxis.age[length(xaxis.age):1]  ),  
         c(CIlow, CIhigh[length(xaxis.age):1]), border = NA, col = "gray")
lines(xaxis.age, avgFE, cex=0.5, lwd=1  ,type="l" )
lines(xaxis.age, avgFE, cex=0.5, lwd=1  ,type="b" )




load(  "FErho_firmeffect_sim_result.Rdata")


require(Hmisc) 
x = FErho_firmeffect_sim_result$FErhopooled_sim
xxx = apply(x, 1,mean)
CIlow = xxx - 1.96*apply(x, 1,sd)
CIhigh= xxx + 1.96*apply(x, 1,sd)

x = FErho_firmeffect_sim_result$avgFErhofirm_sim
xxx2 = rep(mean(x,na.rm=T),3)
CIlow2 = xxx2 - 1.96*sd(x,  na.rm=T) 
CIhigh2 =  xxx2 + 1.96*sd(x, na.rm=T) 

errbar( c(-0.005,FErho_firmeffect_sim_result$SD_firmeffect_set[2:3],0.005,
          FErho_firmeffect_sim_result$SD_firmeffect_set[2:3]), c(xxx,xxx2) , 
        c(CIlow,CIlow2), c(CIhigh,CIhigh2) ,
       cap=0.03,   type='p' , cex=1,  lty=c(rep(1,3),rep(2,3)), lwd=0.5 ,
       ylab="", xlab="SD(firm effect)",xlim=c(-0.27,0.3))
abline(h = FErho_firmeffect_sim_result$FErhopooled, col = "red", lwd = 1)
abline(h = FErho_firmeffect_sim_result$avgFErhofirm, col = "red", lwd =1, lty=2)
text(-0.1,0.26, "pooled 1st-order autocorr(FE)",col="red",cex=0.6)
text(-0.15,0.205, "average of firm-level",col="red",cex=0.6)
text(-0.15,0.2015 , "1st-order autocorr(FE)",col="red",cex=0.6)
title( main="Autocorr(FE)")




### mixed effect model estimation

library(nlme)
year_idx_numeric = floor(firm_TS_data$year_qtr_idx-0.2)
age_idx_numeric =  1+floor(firm_TS_data$TS_index_firm_deflated/4-0.2)
firm_idx_numeric = est$firm_deflated_nn_idx

year_dummy=  factor(year_idx_numeric)
age_dummy=  factor(age_idx_numeric)
firm_dummy=  factor(firm_idx_numeric)

FE_temp = c(firm_TS_data$ye_firm_deflated - firm_TS_data$yf_firm_deflated)
FE_temp_lag1 = c(NA,FE_temp[1:(length(FE_temp )-1)] )
FE_temp_lag1[est$firm_begin_deflated] = NA


m1 <- lme( FE_temp  ~ -1   + age_dummy + year_dummy ,random=   ~1 | firm_dummy   ,na.action=na.omit  )
#m1 <- lme( FE_temp  ~ -1   + age_dummy + year_dummy ,random=   ~1 | firm_dummy   ,na.action=na.omit ,method="ML" )
age_effect = m1$coef$fixed[1:28] 
year_effect = c( 0, m1$coef$fixed[29:56] )
firm_effect = m1$coef$random$firm_dummy
SD_err = m1$sigma 

m1 <- lmer( FE_temp  ~ -1   + age_dummy + year_dummy + FE_temp_lag1 + (1 | firm_dummy) + (-1 +FE_temp_lag1  | firm_dummy)   ,REML=FALSE )
0.1728704
# - firm
lmer( FE_temp  ~ -1   + age_dummy + year_dummy + FE_temp_lag1 +  (-1 +FE_temp_lag1  | firm_dummy)   ,REML=FALSE )
0.1977409-0.1728704
# - year
lmer( FE_temp  ~ -1   + age_dummy +  FE_temp_lag1 + (1 | firm_dummy) + (-1 +FE_temp_lag1  | firm_dummy)   ,REML=FALSE )
1.795e-01 - 0.1728704
#  - age
lmer( FE_temp  ~ -1   +  year_dummy + FE_temp_lag1 + (1 | firm_dummy) + (-1 +FE_temp_lag1  | firm_dummy)   ,REML=FALSE )
1.738e-01 - 0.1728704
# -all
lmer( FE_temp  ~  FE_temp_lag1  + (-1 +FE_temp_lag1  | firm_dummy)   ,REML=FALSE )
2.182e-01 - 0.1728704
0.1728704/2.182e-01 
0.203/0.258

# + firm
lmer( FE_temp  ~  FE_temp_lag1 + (1 | firm_dummy)  + (-1 +FE_temp_lag1  | firm_dummy)  , REML=FALSE)
2.182e-01   -   1.870e-01   
# + year
lmer( FE_temp  ~ year_dummy  + FE_temp_lag1+ (-1 +FE_temp_lag1  | firm_dummy)   , REML=FALSE)
2.182e-01  -   0.1981721        
# + age
lmer( FE_temp  ~  age_dummy  + FE_temp_lag1 + (-1 +FE_temp_lag1  | firm_dummy)  , REML=FALSE)
2.182e-01 -  2.123e-01  

#### FE-rho fixed
lmer( FE_temp  ~ -1   + age_dummy + year_dummy + FE_temp_lag1 + (1 | firm_dummy)    , REML=FALSE)
0.1744735 
# - firm
lm( FE_temp  ~ -1   + age_dummy + year_dummy + FE_temp_lag1     )
0.2040355 -0.1744735  
# - year
lmer( FE_temp  ~ -1   + age_dummy   + FE_temp_lag1 + (1 | firm_dummy)    , REML=FALSE)
1.773e-01 -0.1744735 
# - age
lmer( FE_temp  ~ -1 +    year_dummy + FE_temp_lag1 + (1 | firm_dummy)    , REML=FALSE)
1.752e-01-0.1744735 
# -all
lm( FE_temp  ~  FE_temp_lag1     )
2.166e-01-0.1744735 

# + firm
lmer( FE_temp  ~  FE_temp_lag1 + (1 | firm_dummy)    , REML=FALSE)
2.166e-01  -  1.820e-01  
# + year
lm( FE_temp  ~ year_dummy  + FE_temp_lag1  )
2.166e-01  -  2.045e-01  
# + age
lm( FE_temp  ~  age_dummy  + FE_temp_lag1  )
2.166e-01 - 2.125e-01  





0.001137687/SD_err
(0.001137687 )/sqrt(0.001137687^2 + SD_err^2)



# Simulation of pooled rho(FE)

fcn_AR1_coef_estimate_pooled<- function(FE_sim_pooled) {   
  pdataset = list(y=FE_sim_pooled, begin_idx =est$firm_begin_deflated, end_idx = est$firm_end_deflated)
  FE_sim_pooled = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
  m1 = arima( FE_sim_pooled, order=c(1,0,0), method = c("ML"),optim.control = list(maxit = 300000 ))  
  return(m1$coef[1])  
}

fcn_AR1_coef_estimate_avg_firm_biasadj<- function(FE_sim_pooled) {   
  rho_firms =rep(NA, length(est$firm_begin_deflated ))    
  for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
  {  
      firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]  
      m1=arima( FE_sim_pooled[firm_idx_temp]  , order=c(1,0,0), method = c("ML"),optim.control = list(maxit = 300000 )) 
      rho_firms[jj] = m1$coef[1]  
  }
  return(mean((rho_firms*(est$no_obs_deflated-1)+1)/(est$no_obs_deflated-4)))  
}


if (do_all) {
    
  NN = 1000  
  FErhopooled_fitted_sim.all = rep(NA,NN)
  FErhopooled_fitted_sim.age = rep(NA,NN)
  FErhopooled_fitted_sim.year = rep(NA,NN)
  FErhopooled_fitted_sim.firm = rep(NA,NN)
  
  FErhofirmbiasadj_fitted_sim.all = rep(NA,NN)
  FErhofirmbiasadj_fitted_sim.age = rep(NA,NN)
  FErhofirmbiasadj_fitted_sim.year = rep(NA,NN)
  FErhofirmbiasadj_fitted_sim.firm = rep(NA,NN)
  
  
  NA_idx_temp = is.na(  yef_temp    )
  year_idx_numeric_fix = year_idx_numeric - min(year_idx_numeric,na.rm=T) + 1 
  year_idx_numeric_fix[NA_idx_temp] = 1 
  zzz.age0  =  as.numeric(age_effect[age_idx_numeric])
  zzz.year0 =  as.numeric(year_effect[year_idx_numeric_fix ])

  for (ii in seq(  1 , NN ) ) { 
    
    ### data generation
    zzz =  SD_err * rnorm( length(yef_temp ))  
    zzz[NA_idx_temp ] = NA
    

    
    #zzz.firm =  as.numeric(firm_effect[firm_idx_numeric])      
    firm_effect_sim = 0.001137687* rnorm( length(est$firm_begin_deflated) )
    zzz.firm = firm_effect_sim[est$firm_deflated_nn_idx]
    
    
    zzz.all = zzz +  zzz.age0 + zzz.year0 + zzz.firm
    zzz.age  = zzz + zzz.age0
    zzz.year = zzz + zzz.year0
    zzz.firm = zzz + zzz.firm   
    
    ### pooled rho(FE)      
    FErhopooled_fitted_sim.all[ii]  = fcn_AR1_coef_estimate_pooled(zzz.all)
    FErhopooled_fitted_sim.age[ii]  = fcn_AR1_coef_estimate_pooled(zzz.age)
    FErhopooled_fitted_sim.year[ii] = fcn_AR1_coef_estimate_pooled(zzz.year)
    FErhopooled_fitted_sim.firm[ii] = fcn_AR1_coef_estimate_pooled(zzz.firm)
     
    
    # save
    if ( (ii %% 10) == 0 ) {
      FErhopooled_fitted_sim_result =list(FErhopooled_fitted_sim.all=FErhopooled_fitted_sim.all,
                                          FErhopooled_fitted_sim.age=FErhopooled_fitted_sim.age,
                                          FErhopooled_fitted_sim.year=FErhopooled_fitted_sim.year,
                                          FErhopooled_fitted_sim.firm=FErhopooled_fitted_sim.firm)
      save(FErhopooled_fitted_sim_result  , file="FErhopooled_fitted_sim_result.Rdata")
    }
    print(ii)
  } 
 
}



##############################################################################
### two interpretations of time-varying mean(FE)
### (1) time-varying pessimism (2) autocorr in FE(common earning component) sd ratio = 0.2:1
###  Interpretation doesn't matter.

load("FErhopooled_fitted_sim_result.Rdata")

mean(FErhopooled_fitted_sim_result$FErhopooled_fitted_sim.all)
sd(FErhopooled_fitted_sim_result$FErhopooled_fitted_sim.all)

mean(FErhopooled_fitted_sim_result$FErhopooled_fitted_sim.age)
sd(FErhopooled_fitted_sim_result$FErhopooled_fitted_sim.age)

mean(FErhopooled_fitted_sim_result$FErhopooled_fitted_sim.year)
sd(FErhopooled_fitted_sim_result$FErhopooled_fitted_sim.year)

mean(FErhopooled_fitted_sim_result$FErhopooled_fitted_sim.firm)
sd(FErhopooled_fitted_sim_result$FErhopooled_fitted_sim.firm)

## table

tempsum = mean(FErhopooled_fitted_sim_result$FErhopooled_fitted_sim.year)+
  mean(FErhopooled_fitted_sim_result$FErhopooled_fitted_sim.age)+
  mean(FErhopooled_fitted_sim_result$FErhopooled_fitted_sim.firm)

  mean(FErhopooled_fitted_sim_result$FErhopooled_fitted_sim.year)/tempsum 
  mean(FErhopooled_fitted_sim_result$FErhopooled_fitted_sim.age)/tempsum 
  mean(FErhopooled_fitted_sim_result$FErhopooled_fitted_sim.firm)/tempsum 


mean(FErhopooled_fitted_sim_result$FErhopooled_fitted_sim.year)/0.258 
mean(FErhopooled_fitted_sim_result$FErhopooled_fitted_sim.age)/0.258 
mean(FErhopooled_fitted_sim_result$FErhopooled_fitted_sim.firm)/0.258 
tempsum /0.258 
 
## 4) GENERAL: firm-wise time variation --> observationally equivalent to pos autocorr(FE) - tautology
## So still learning mu is not clearly ruled out (if on average avg(FE) neither increase nor decrease over firm-age)
## Simulations (fix mu and muhat, and change the bias b)











##############################################################################
########################################################################
#######  II. variation phi-phihat (including variation in pessimism) #######
########################################################################
##############################################################################

library(lme4)
year_idx_numeric = floor(firm_TS_data$year_qtr_idx-0.2)
age_idx_numeric =  1+floor(firm_TS_data$TS_index_firm_deflated/4-0.2)
firm_idx_numeric = est$firm_deflated_nn_idx

year_dummy=  factor(year_idx_numeric)
age_dummy=  factor(age_idx_numeric)
firm_dummy=  factor(firm_idx_numeric)

FE_temp = c(firm_TS_data$ye_firm_deflated-firm_TS_data$yf_firm_deflated)
pdataset = list(y=FE_temp, begin_idx =est$firm_begin_deflated, end_idx = est$firm_end_deflated)
FE_temp_pooled = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
FE_temp_pooled_lag1 = c(NA,FE_temp_pooled[1:(length(FE_temp_pooled )-1)] )

ye_temp =  firm_TS_data$ye_firm_deflated 
pdataset = list(y=ye_temp, begin_idx =est$firm_begin_deflated, end_idx = est$firm_end_deflated)
ye_temp_pooled = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
ye_temp_pooled_lag1 = c(NA,ye_temp_pooled[1:(length(ye_temp_pooled )-1)] )

yf_temp =  firm_TS_data$yf_firm_deflated 
pdataset = list(y=yf_temp, begin_idx =est$firm_begin_deflated, end_idx = est$firm_end_deflated)
yf_temp_pooled = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
yf_temp_pooled_lag1 = c(NA,yf_temp_pooled[1:(length(yf_temp_pooled )-1)] )

pdataset = list(y=year_idx_numeric, begin_idx =est$firm_begin_deflated, end_idx = est$firm_end_deflated)
year_dummy_pooled = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
year_dummy_pooled = factor(year_dummy_pooled)

pdataset = list(y=age_idx_numeric, begin_idx =est$firm_begin_deflated, end_idx = est$firm_end_deflated)
age_dummy_pooled = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
age_dummy_pooled = factor(age_dummy_pooled)

pdataset = list(y=firm_idx_numeric, begin_idx =est$firm_begin_deflated, end_idx = est$firm_end_deflated)
firm_dummy_pooled = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
firm_dummy_pooled = factor(firm_dummy_pooled)
 
 
 


if (do_all){
  m2.ye=lmer(ye_temp_pooled~ -1 + age_dummy_pooled + year_dummy_pooled  +   ye_temp_pooled_lag1:year_dummy_pooled +
            ye_temp_pooled_lag1:age_dummy_pooled + ye_temp_pooled_lag1 + (ye_temp_pooled_lag1|firm_dummy_pooled)    , REML = FALSE)
  
  m2.yf=lmer(yf_temp_pooled~ -1 + age_dummy_pooled + year_dummy_pooled  +   ye_temp_pooled_lag1:year_dummy_pooled +
               ye_temp_pooled_lag1:age_dummy_pooled + ye_temp_pooled_lag1 + (ye_temp_pooled_lag1|firm_dummy_pooled)    , REML = FALSE)
  
  m2.FE=lmer(FE_temp_pooled~ -1 + age_dummy_pooled + year_dummy_pooled  +   ye_temp_pooled_lag1:year_dummy_pooled +
           ye_temp_pooled_lag1:age_dummy_pooled + ye_temp_pooled_lag1 + (ye_temp_pooled_lag1|firm_dummy_pooled)    , REML = FALSE)
  m2fit = list(m2.FE=m2.FE, m2.ye=m2.ye, m2.yf=m2.yf)
  save(m2fit, file="MixedModel_phiphihat_Lmer.Rdata") 
  }

load("MixedModel_phiphihat_Lmer.Rdata") 
attr(str(VarCorr(m2fit$m2.FE)),"stddev")[1] 
sd_est_phiphihat = attr( VarCorr(m2fit$m2.FE)$firm_dummy_pooled ,"stddev")[2]
sd_est_phi = attr( VarCorr(m2fit$m2.ye)$firm_dummy_pooled ,"stddev")[2]
sd_est_phihat = attr( VarCorr(m2fit$m2.yf)$firm_dummy_pooled ,"stddev")[2]

cov_est_phiphihat = (sd_est_phi^2+sd_est_phihat^2-sd_est_phiphihat^2)/2

COV_phiphihat = matrix(c(sd_est_phi^2,cov_est_phiphihat,cov_est_phiphihat, sd_est_phihat^2  ) ,2,2)



phi_phihat_year = cbind(as.numeric( c(0,fixef(m2fit$m2.ye)[58:85])[year_idx_numeric_fix]   ),   
                        as.numeric( c(0,fixef(m2fit$m2.yf)[58:85])[year_idx_numeric_fix]   ) )

phi_phihat_age = cbind(as.numeric( c(0,fixef(m2fit$m2.ye)[86:112])[age_idx_numeric]   ),   
                       as.numeric( c(0,fixef(m2fit$m2.yf)[86:112])[age_idx_numeric ]   ) )   


# Simulation of SD(phi-phihat)



fcn_AR1_coef_estimate_pooled_heterophiphihat_general<- function(  KFfit, phi_phihat_age,phi_phihat_year, COV_phiphihat  ) { 
  
  FEsim = rep(NA, length(firm_TS_data$ye_firm_deflated) )  
   
  phi_phihat_age_adj = cbind(phi_phihat_age[,1]-mean(phi_phihat_age[!NA_idx_temp,1]),
                       phi_phihat_age[,2]-mean(phi_phihat_age[!NA_idx_temp,2]))
  phi_phihat_year_adj = cbind(phi_phihat_year[,1]-mean(phi_phihat_year[!NA_idx_temp,1]),
                             phi_phihat_year[,2]-mean(phi_phihat_year[!NA_idx_temp,2]))
   
  phi_fixedeffect = phi_phihat_age_adj[ ,1] + phi_phihat_year_adj[,1]  
  phihat_fixedeffect = phi_phihat_age_adj[,2]  + phi_phihat_year_adj[,2]  
  
  
  for (jj in seq( 1 , length(firm_TS_data$firm_begin_deflated)))
  {      
    firm_idx_temp = firm_TS_data$firm_begin_deflated[jj]:firm_TS_data$firm_end_deflated[jj]     
    Tsim = length(firm_idx_temp) + 50   
    ytsim1= rep(NA, Tsim )
    ytsim2= rep(NA, Tsim ) 
    
    xtsim0 = 0
    ytsim1[1] = xtsim0 + rnorm(1)
    ytsim2[1] = 0  
    
    if (prod(diag(COV_phiphihat)==0)==1) e_phi = c(0,0) else 
                                         e_phi = t(chol(COV_phiphihat))%*%matrix(rnorm(2),ncol=1)
        
    phi_i = KFfit$par["phi"] + c(rep(0,50), phi_fixedeffect[firm_idx_temp]) + e_phi[1]
    phihat_i = KFfit$par["phihat"] + c(rep(0,50),phihat_fixedeffect[firm_idx_temp]) + e_phi[2]
    
    for (ii  in seq(2, Tsim ))
    {          
      et_sim = rnorm(1)*exp(KFfit$par["logsigmae"])
      at_sim = rnorm(1)*exp(KFfit$par["logsigmaa"])
      nt_sim = rnorm(1)*exp(KFfit$par["logsigman"])
      
      xtsim1     = min(0.99, phi_i[ii]  )*xtsim0 + et_sim  
      ytsim1[ii] = xtsim1 + at_sim  
      ytsim2[ii] = phihat_i[ii]*(KFfit$par["Khat"]*ytsim1[ii-1] +
                               (1-KFfit$par["Khat"])*ytsim2[ii-1] ) + KFfit$par["what"]*(et_sim + nt_sim)
      xtsim0=xtsim1
    }
    FEsim[firm_idx_temp] = (ytsim1 - ytsim2 )[51:Tsim]    
  } 
  
  FEsim[NA_idx_temp] = NA  
  pdataset = list( y=FEsim,begin_idx =firm_TS_data$firm_begin_deflated, end_idx = firm_TS_data$firm_end_deflated)
  FEsim = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
  m1 = arima( FEsim , order=c(1,0,0), method = c("ML"),optim.control = list(maxit = 300000 ))  
  return(m1$coef[1])  
  
}



#####  Simulations #####

if (do_all)
{
  NN = 100 
  
  sigma_alpha_set = c(0,0.1,0.15,0.2,0.3)
  sigma_alpha_input = sigma_alpha_set[1]
  set.seed( 12345 ) 
  
  load("KFfit_VARMA_MLE.Rdata")
  KFfit$par["logsigmaa"] =log(sigma_alpha_input)  
  KFfit$par["logsigmae"]   = log(1)
  KFfit$par["logsigman"] = log(0.5)
  KFfit$par["mue"]    = 0    
  KFfit$par["muehat"] = 0         
  KFfit$par["phi"]  = 0.47   
  KFfit$par["phihat"]   = 0.47 
  Implist = fcn_implied_K_w_rho( matrix(as.numeric(KFfit$par)[3:9], ncol=1) )
  KFfit$par["Khat"] =    Implist$K
  if (KFfit$par["logsigmaa"]  ==  -Inf) KFfit$par["Khat"] = 1
  KFfit$par["what"] =    Implist$w 
  
  
  FErhopooled_fitted_sim_heterophiphihat = rep(NA, NN )
  
  # age effect only
  for (ii in seq(  1 , NN ) ) {  
    FErhopooled_fitted_sim_heterophiphihat[ii]  = 
    fcn_AR1_coef_estimate_pooled_heterophiphihat_general(  KFfit=KFfit, 
    phi_phihat_age=phi_phihat_age,phi_phihat_year=phi_phihat_year*0, COV_phiphihat=COV_phiphihat*0  )   
        
    if ( (ii %% 10) == 0 ) {
      FErhopooled_fitted_sim_heterophiphihat_result =list(KFfit=KFfit, COV_phiphihat=COV_phiphihat,
                                                          FErhopooled_fitted_sim_heterophiphihat=FErhopooled_fitted_sim_heterophiphihat)
      save(FErhopooled_fitted_sim_heterophiphihat_result, file=sprintf("FErhopooled_fitted_sim_heterophiphihat_result_age_%d.Rdata",sigma_alpha_input*100 ))
    }
    print(ii)
  } 
    
  # year effect only  
  for (ii in seq(  1 , NN ) ) {  
    FErhopooled_fitted_sim_heterophiphihat[ii]  = 
      fcn_AR1_coef_estimate_pooled_heterophiphihat_general(  KFfit=KFfit, 
      phi_phihat_age=phi_phihat_age*0,phi_phihat_year=phi_phihat_year, COV_phiphihat=COV_phiphihat*0  )   
    
    if ( (ii %% 10) == 0 ) {
      FErhopooled_fitted_sim_heterophiphihat_result =list(KFfit=KFfit, COV_phiphihat=COV_phiphihat,
                                                          FErhopooled_fitted_sim_heterophiphihat=FErhopooled_fitted_sim_heterophiphihat)
      save(FErhopooled_fitted_sim_heterophiphihat_result, file=sprintf("FErhopooled_fitted_sim_heterophiphihat_result_year_%d.Rdata",sigma_alpha_input*100 ))
    }
    print(ii)
  } 
  
  #   firm
  for (ii in seq(  1 , NN ) ) {  
    FErhopooled_fitted_sim_heterophiphihat[ii]  = 
      fcn_AR1_coef_estimate_pooled_heterophiphihat_general(  KFfit=KFfit, 
                                                             phi_phihat_age=phi_phihat_age*0,phi_phihat_year=phi_phihat_year*0, COV_phiphihat=COV_phiphihat  )   
    
    if ( (ii %% 10) == 0 ) {
      FErhopooled_fitted_sim_heterophiphihat_result =list(KFfit=KFfit, COV_phiphihat=COV_phiphihat,
                                                          FErhopooled_fitted_sim_heterophiphihat=FErhopooled_fitted_sim_heterophiphihat)
      save(FErhopooled_fitted_sim_heterophiphihat_result, file=sprintf("FErhopooled_fitted_sim_heterophiphihat_result_firm_%d.Rdata",sigma_alpha_input*100 ))
    }
    print(ii)
  } 
  
  
  
  # age+ year + firm
  for (ii in seq(  1 , NN ) ) {  
    FErhopooled_fitted_sim_heterophiphihat[ii]  = 
      fcn_AR1_coef_estimate_pooled_heterophiphihat_general(  KFfit=KFfit, 
                                                             phi_phihat_age=phi_phihat_age,phi_phihat_year=phi_phihat_year, COV_phiphihat=COV_phiphihat  )   
    
    if ( (ii %% 10) == 0 ) {
      FErhopooled_fitted_sim_heterophiphihat_result =list(KFfit=KFfit, COV_phiphihat=COV_phiphihat,
                                                          FErhopooled_fitted_sim_heterophiphihat=FErhopooled_fitted_sim_heterophiphihat)
      save(FErhopooled_fitted_sim_heterophiphihat_result, file=sprintf("FErhopooled_fitted_sim_heterophiphihat_result_all_%d.Rdata",sigma_alpha_input*100 ))
    }
    print(ii)
  } 

}
  
  
load(  "FErhopooled_fitted_sim_heterophiphihat_result_year_0.Rdata")
qq=FErhopooled_fitted_sim_heterophiphihat_result$FErhopooled_fitted_sim_heterophiphihat
mean(qq,na.rm=T) 
sd(qq,na.rm=T)
mean(qq,na.rm=T)/FErhopooled*100
  
load(  "FErhopooled_fitted_sim_heterophiphihat_result_age_0.Rdata")
qq=FErhopooled_fitted_sim_heterophiphihat_result$FErhopooled_fitted_sim_heterophiphihat
mean(qq,na.rm=T) 
sd(qq,na.rm=T)
mean(qq,na.rm=T)/FErhopooled*100

load(  "FErhopooled_fitted_sim_heterophiphihat_result_0.Rdata")
qq=FErhopooled_fitted_sim_heterophiphihat_result$FErhopooled_fitted_sim_heterophiphihat
mean(qq,na.rm=T) 
sd(qq,na.rm=T)
mean(qq,na.rm=T)/FErhopooled*100

load(  "FErhopooled_fitted_sim_heterophiphihat_result_firm_0.Rdata")
qq=FErhopooled_fitted_sim_heterophiphihat_result$FErhopooled_fitted_sim_heterophiphihat
mean(qq,na.rm=T) 
sd(qq,na.rm=T)
mean(qq,na.rm=T)/FErhopooled*100

load(  "FErhopooled_fitted_sim_heterophiphihat_result_all_0.Rdata")
qq=FErhopooled_fitted_sim_heterophiphihat_result$FErhopooled_fitted_sim_heterophiphihat
mean(qq,na.rm=T)
sd(qq,na.rm=T)
mean(qq,na.rm=T)/FErhopooled*100




 

# CS variance of (mu_i) is overestimated due to autocorr. Bias-adjusted var is 0.16, not 0.225
# But heterogeneity in FE(rho) exists, opposite bias generates.
x = 0.16*rnorm(10^4)[(c(t(matrix(rep(seq(1,10^4 ),20),ncol=20))))]/0.85 + arima.sim(list(order = c(1,0,0), ar = 0.15), n = 20*10^4)
fdy = factor(c(t(matrix(rep(seq(1,10^4 ),20),ncol=20))))
x1 = x
x0 = c(NA, x[1:(length(x)-1)])
x0[ c(1:10^4)*20-19] = NA

x. = 0.16*rnorm(10^4)[(c(t(matrix(rep(seq(1,10^4 ),20),ncol=20))))]/0.75 + arima.sim(list(order = c(1,0,0), ar = 0.25), n = 20*10^4)
fdy. = factor(c(t(matrix(rep(seq(1,10^4 ),20),ncol=20)))+10^4)
x1. = x.
x0. = c(NA, x.[1:(length(x.)-1)])
x0.[ c(1:10^4)*20-19] = NA

lmer(x1 ~  x0+ (x0|fdy)    , REML = FALSE)
lmer(x1 ~  x0+ (1|fdy)    , REML = FALSE)

x1 = c(x1,x1.) 
x0 = c(x0,x0.) 
fdy = c(fdy,fdy.)

lmer( x1 ~  x0 + (x0 | fdy)    , REML = FALSE)
lmer( x1 ~  x0 + ( 1 | fdy)    , REML = FALSE)
lmer( x1 ~  x0 + ( -1+x0 | fdy)    , REML = FALSE)
lmer( x1 ~   (1|fdy)    , REML = FALSE)

lme(x ~1, random=~1|fdy, method="ML")

arima(x1, c(1,0,0))

###
n.temp = 10^2
t.temp = 20
x_all= matrix(rep(NA, n.temp*t.temp),ncol=n.temp)
for (  ii in seq(1,n.temp) ) {
  
  x = rnorm(1)*0.2/0.75 + arima.sim(list(order = c(1,0,0), ar = min(0.999, 0.2+rnorm(1)*0.25) ), n = t.temp )
  x_all[,ii] = x
}
fdy = factor(c(t(matrix(rep(seq(1, n.temp ),t.temp),ncol=t.temp))))
x = c(x_all)
x1 = x
x0 = c(NA, x[1:(length(x)-1)])
x0[ c(1:n.temp)*t.temp-t.temp+1] = NA

lmer( x1 ~  x0 + ( 1| fdy)  + ( -1+x0 | fdy)    , REML = FALSE)

lmer( x1 ~  x0 + (x0 | fdy)    , REML = FALSE)
lmer( x1 ~  x0 + ( 1 | fdy)    , REML = FALSE)



lmer( x1 ~   (1|fdy)    , REML = FALSE)

 


# working: saved here as temp_20140623.Rdata

##############################################################################
########################################################################
#######  Learning?  #######
########################################################################
##############################################################################

# firmlevel FE_rho repeat: use ARIMA
rho_firms =rep(NA, length(est$firm_begin_deflated ))    
for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
{  
     firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]  
    m1=arima( yef_temp[firm_idx_temp]  , order=c(1,0,0), method = c("ML"),optim.control = list(maxit = 300000 )) 
    rho_firms[jj] = m1$coef[1] 
      
}
FErhoadj = ((rho_firms*(est$no_obs_deflated-1)+1)/(est$no_obs_deflated-4)) 
avgFErhofirm = mean(FErhoadj)
sd(FErhoadj)
# Mean of firmlevel autocorr(FE) biase adjusted = 0.203
sum(abs(NA_idx_temp - is.na(year_idx_numeric) ))


### construct year-quarter index without NA
yearqtr_idx_numeric = est$year_qtr_idx
for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
{  
     firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]      
    yearqtr_idx_numeric[firm_idx_temp] = seq( yearqtr_idx_numeric[est$firm_begin_deflated[jj]],yearqtr_idx_numeric[est$firm_end_deflated[jj]], by=0.25)
     
}
sum( is.na(yearqtr_idx_numeric)    )
sum(abs(yearqtr_idx_numeric-est$year_qtr_idx),na.rm=T)
range(yearqtr_idx_numeric)

### average of FE_rho for each quarter from firm-level FE_rho estiamte.
FErhoadj_firmqtr = FErhoadj[est$firm_deflated_nn_idx]
yearqtr_xaxis = seq(range(yearqtr_idx_numeric,na.rm=T)[1],range(yearqtr_idx_numeric,na.rm=T)[2],by=0.25)
N_row_yearqtr =length(table(yearqtr_idx_numeric))

if (do_all) {
  m1.learn = lmer(FE_temp_pooled~  FE_temp_pooled_lag1 + (FE_temp_pooled_lag1|firm_dummy_pooled)    , REML = FALSE)
  save(m1.learn, file="m1_learn.Rdata")
}

load("m1_learn.Rdata")
FErhoadj_mixed=fixef(m1.learn)[2]+(ranef(m1.learn)$firm_dummy_pooled$"FE_temp_pooled_lag1")
FErhoadj_mixed = ((FErhoadj_mixed*(est$no_obs_deflated-1)+1)/(est$no_obs_deflated-4)) 
FErhoadj_firmqtr_mixed = FErhoadj_mixed[est$firm_deflated_nn_idx]
sd(FErhoadj)
sd(FErhoadj_mixed)
sd( ((rho_firms_OLS*(est$no_obs_deflated-1)+1)/(est$no_obs_deflated-4))  )
sqrt(var(((rho_firms_OLS*(est$no_obs_deflated-1)+1)/(est$no_obs_deflated-4)))-var(FErhoadj_mixed))
plot(est$no_obs_deflated,FErhoadj_mixed)
mean(FErhoadj>0)
hist(FErhoadj,100)
hist(FErhoadj_mixed,100)
ttt=seq(20,100);rrr=0.2;plot(ttt, ( ((rrr*(ttt-1)+1)/(ttt-4))  )-rrr,type="l")
ttt=seq(20,100);rrr=0.1;plot(ttt, ( ((rrr*(ttt-1)+1)/(ttt-4))  )-rrr,type="l")
ttt=seq(20,100);rrr=0;plot(ttt, ( ((rrr*(ttt-1)+1)/(ttt-4))  )-rrr,type="l")
ttt=seq(20,100);rrr=-0.14;plot(ttt, ( ((rrr*(ttt-1)+1)/(ttt-4))  )-rrr,type="l")
ttt=seq(20,100);rrr=-0.2;plot(ttt, ( ((rrr*(ttt-1)+1)/(ttt-4))  )-rrr,type="l")
ttt=seq(10,100);rrr=0;plot(ttt, ( ((rrr*(ttt-1)+1)/(ttt-4))  )-rrr,type="l")


rhotemp0 = rep(NA,10^3)
for (iii in seq(1,10^3)) rhotemp0[iii] = arima( arima.sim(list(order = c(1,0,0), ar = -0.1  ), n = 20), c(1,0,0))$coef[1]
mean(rhotemp0,na.rm=T)



FErho_yearqtr_adj_firm_avg = rep(NA, N_row_yearqtr )
FErho_yearqtr_adj_firm_SE =  rep(NA, N_row_yearqtr )

FErho_yearqtr_adj_firm_avg_mixed = rep(NA, N_row_yearqtr )
FErho_yearqtr_adj_firm_SE_mixed =  rep(NA, N_row_yearqtr )

for (ii in seq( 1,N_row_yearqtr ) ) {    
  
  FErho_yearqtr_adj_firm = FErhoadj_firmqtr[ yearqtr_idx_numeric   == yearqtr_xaxis[ii]   ]  
  FErho_yearqtr_adj_firm_avg[ii] = mean(FErho_yearqtr_adj_firm ,na.rm=T)
  FErho_yearqtr_adj_firm_SE[ii] = sd(FErho_yearqtr_adj_firm,na.rm=T)/sqrt( sum(!is.na(FErho_yearqtr_adj_firm)) )
  
  FErho_yearqtr_adj_firm_mixed = FErhoadj_firmqtr_mixed[ yearqtr_idx_numeric   == yearqtr_xaxis[ii]   ]  
  FErho_yearqtr_adj_firm_avg_mixed[ii] = mean(FErho_yearqtr_adj_firm_mixed ,na.rm=T)
  FErho_yearqtr_adj_firm_SE_mixed[ii] = sd(FErho_yearqtr_adj_firm_mixed,na.rm=T)/sqrt( sum(!is.na(FErho_yearqtr_adj_firm_mixed)) )
    
  print(ii)
}

par(mfrow=c(1,2))
plot(yearqtr_xaxis, FErho_yearqtr_adj_firm_avg ,type="b")
plot(yearqtr_xaxis,FErho_yearqtr_adj_firm_SE,type="b")

par(mfrow=c(1,2))
plot(yearqtr_xaxis, FErho_yearqtr_adj_firm_avg_mixed ,type="b")
plot(yearqtr_xaxis,FErho_yearqtr_adj_firm_SE_mixed,type="b")
## two methods are similar.


# plot in the paper
par(mfrow=c(1,2))
# year effect on firm-level est
ssd = 1.96
xxx = FErho_yearqtr_adj_firm_avg 
CIlow =   xxx - FErho_yearqtr_adj_firm_SE*ssd   
CIhigh =  xxx + FErho_yearqtr_adj_firm_SE*ssd  
xaxis  = yearqtr_xaxis

T_temp = length(xxx)
xxx = xxx[1:(T_temp-1)]
CIlow =   CIlow[1:(T_temp-1)]
CIhigh =  CIhigh[1:(T_temp-1)]
xaxis  = xaxis[1:(T_temp-1)]

plot(NA,xlim=c(xaxis[1], xaxis[length(xaxis)]),  ylim=c(min(CIlow),max(CIhigh)) ,
     main="Year effect on Autocorr(FE)", xlab="Year",ylab="")
polygon( c(xaxis, xaxis[length(xaxis):1]  ),  
         c(CIlow, CIhigh[length(xaxis):1]), border = NA, col = "gray")
lines(xaxis,xxx, cex=0.5, lwd=1  ,type="l" )
points(xaxis,xxx, cex=0.5, lwd=1    )
abline(h=0, lty="dashed"  )


############ learning within firm (pilot) ##########
#1:20 - 21:40
#2:21 - 22:41
#3:22 - 23:42


### construct year-quarter index without NA
nonNA_index_firm = rep(NA, length(est$ye_firm_deflated) )
for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
{  
  firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]     
  nonNA_index_firm[ firm_idx_temp[!(NA_idx_temp[firm_idx_temp])] ] = seq(1, est$no_obs_deflated[jj]  )      
}
nonNA_index_firm[is.na(nonNA_index_firm)]=0


if (do_all) {
FErho_20_1 = rep(NA,length(est$firm_begin_deflated )  )
FErho_20_2 = rep(NA,length(est$firm_begin_deflated )  )
FErho_40_1 = rep(NA,length(est$firm_begin_deflated )  )
FErho_40_2 = rep(NA,length(est$firm_begin_deflated )  )
FErho_60_1 = rep(NA,length(est$firm_begin_deflated )  )
FErho_60_2 = rep(NA,length(est$firm_begin_deflated )  )
FErho_80_1 = rep(NA,length(est$firm_begin_deflated )  )
FErho_80_2 = rep(NA,length(est$firm_begin_deflated )  )


for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
{  
    firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]     
    nonNA_index_firm_temp = nonNA_index_firm[firm_idx_temp]
    xxx = yef_temp[firm_idx_temp]
    onetoT = est$TS_index_firm_deflated[firm_idx_temp]
    
    if (est$no_obs_deflated[jj] >= 40 ) {
      xxx1=xxx[ onetoT[nonNA_index_firm_temp==1]:onetoT[nonNA_index_firm_temp==20] ]
      xxx2=xxx[ onetoT[nonNA_index_firm_temp==21]:onetoT[nonNA_index_firm_temp==40] ]
      FErho_20_1[jj] = arima( xxx1 , order=c(1,0,0), method = c("ML"),optim.control = list(maxit = 300000 ))$coef[1]
      FErho_20_2[jj] = arima( xxx2 , order=c(1,0,0), method = c("ML"),optim.control = list(maxit = 300000 ))$coef[1]
      
    }
    
    if (est$no_obs_deflated[jj] >= 60 ) {
      xxx1[ onetoT[nonNA_index_firm_temp==1+20]:onetoT[nonNA_index_firm_temp==20+20] ]
      xxx2[ onetoT[nonNA_index_firm_temp==21+20]:onetoT[nonNA_index_firm_temp==40+20] ]
      FErho_40_1[jj] = arima( xxx1 , order=c(1,0,0), method = c("ML"),optim.control = list(maxit = 300000 ))$coef[1]
      FErho_40_2[jj] = arima( xxx2 , order=c(1,0,0), method = c("ML"),optim.control = list(maxit = 300000 ))$coef[1]
      
    }
    
    if (est$no_obs_deflated[jj] >= 80 ) {
      xxx1[ onetoT[nonNA_index_firm_temp==1+40]:onetoT[nonNA_index_firm_temp==20+40] ]
      xxx2[ onetoT[nonNA_index_firm_temp==21+40]:onetoT[nonNA_index_firm_temp==40+40] ]
      FErho_60_1[jj] = arima( xxx1 , order=c(1,0,0), method = c("ML"),optim.control = list(maxit = 300000 ))$coef[1]
      FErho_60_2[jj] = arima( xxx2 , order=c(1,0,0), method = c("ML"),optim.control = list(maxit = 300000 ))$coef[1]
      
    }
    
    if (est$no_obs_deflated[jj] >= 100 ) {
      xxx1[ onetoT[nonNA_index_firm_temp==1+60]:onetoT[nonNA_index_firm_temp==20+60] ]
      xxx2[ onetoT[nonNA_index_firm_temp==21+60]:onetoT[nonNA_index_firm_temp==40+60] ]
      FErho_80_1[jj] = arima( xxx1 , order=c(1,0,0), method = c("ML"),optim.control = list(maxit = 300000 ))$coef[1]
      FErho_80_2[jj] = arima( xxx2 , order=c(1,0,0), method = c("ML"),optim.control = list(maxit = 300000 ))$coef[1]
      
    }
    
    
  }
  
  
  FErho_20_1 = ((FErho_20_1*( 20 - 1)+1)/( 20 - 4))
  FErho_20_2 = ((FErho_20_2*( 20 - 1)+1)/( 20 - 4))
  FErho_40_1 = ((FErho_40_1*( 20 - 1)+1)/( 20 - 4))
  FErho_40_2 = ((FErho_40_2*( 20 - 1)+1)/( 20 - 4))
  FErho_60_1 = ((FErho_60_1*( 20 - 1)+1)/( 20 - 4))
  FErho_60_2 = ((FErho_60_2*( 20 - 1)+1)/( 20 - 4))
  FErho_80_1 = ((FErho_80_1*( 20 - 1)+1)/( 20 - 4))
  FErho_80_2 = ((FErho_80_2*( 20 - 1)+1)/( 20 - 4))
  
  dFErho_20 = FErho_20_2 - FErho_20_1
  dFErho_40 = FErho_40_2 - FErho_40_1
  dFErho_60 = FErho_60_2 - FErho_60_1
  dFErho_80 = FErho_80_2 - FErho_80_1
  
  dFErho_20 = abs(FErho_20_2) - abs(FErho_20_1)
  dFErho_40 = abs(FErho_40_2) - abs(FErho_40_1)
  dFErho_60 = abs(FErho_60_2) - abs(FErho_60_1)
  dFErho_80 = abs(FErho_80_2) - abs(FErho_80_1)
  
  mean(dFErho_20, na.rm=T)
  mean(dFErho_40, na.rm=T)
  mean(dFErho_60, na.rm=T)
  mean(dFErho_80, na.rm=T)
  
  median(dFErho_20, na.rm=T)
  median(dFErho_40, na.rm=T)
  median(dFErho_60, na.rm=T)
  median(dFErho_80, na.rm=T)
  
  sd(dFErho_20, na.rm=T)/sqrt(sum(!is.na(dFErho_20 )))
  sd(dFErho_40, na.rm=T)/sqrt(sum(!is.na(dFErho_40 )))
  sd(dFErho_60, na.rm=T)/sqrt(sum(!is.na(dFErho_60 )))
  sd(dFErho_80, na.rm=T)/sqrt(sum(!is.na(dFErho_80 )))
  
  (sum(!is.na(dFErho_20 )))
  (sum(!is.na(dFErho_40 )))
  (sum(!is.na(dFErho_60 )))
  (sum(!is.na(dFErho_80 )))
}

############ learning within firm (the real one) ##########
#1:20 - 21:40
#2:21 - 22:41
#3:22 - 23:42

 
## 30-quarter window is worse 



# working: saved here







# firmlevel FE_rho t=1:20 first 20 quarters within each firm: use ARIMA
rho_firms =rep(NA, length(est$firm_begin_deflated ))    
for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
{  
  firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]  
  m1=arima( yef_temp[firm_idx_temp]  , order=c(1,0,0), method = c("ML"),optim.control = list(maxit = 300000 )) 
  rho_firms[jj] = m1$coef[1] 
  
}
FErhoadj = ((rho_firms*(est$no_obs_deflated-1)+1)/(est$no_obs_deflated-4)) 
avgFErhofirm = mean(FErhoadj)
sd(FErhoadj)
# Mean of firmlevel autocorr(FE) biase adjusted = 0.203
sum(abs(NA_idx_temp - is.na(year_idx_numeric) ))

 

### average of FE_rho for each quarter from firm-level FE_rho estiamte.
FErhoadj_firmqtr = FErhoadj[est$firm_deflated_nn_idx]
 





############ learning within firm (the real one) 2 ##########
#1:20 - 21:40
#2:21 - 22:41
#3:22 - 23:42



max_no_obs_deflated = max(est$no_obs_deflated)
no_window_learn = max_no_obs_deflated-20+1

if (do_all) {
 
  FErho_learn_0 = matrix(rep(NA, no_window_learn*length(est$firm_begin_deflated )  ), ncol= no_window_learn) 
  FErhoadj_firmqtr_20 = rep(NA, length(est$ye_firm_deflated ))
  FErhoadj_firmqtr_10_30 = rep(NA, length(est$ye_firm_deflated ))
  FErhoadj_firmqtr_10_30_all = rep(NA, length(est$ye_firm_deflated ))
  FErho_learn_0_reg = matrix(rep(NA, no_window_learn*length(est$firm_begin_deflated )  ), ncol= no_window_learn) 
  
  for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
  {  
    firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]     
    nonNA_index_firm_temp = nonNA_index_firm[firm_idx_temp]
    xxx = yef_temp[firm_idx_temp]
    onetoT = est$TS_index_firm_deflated[firm_idx_temp]
    
    for (ii in c( 20:max_no_obs_deflated)) {
      if (est$no_obs_deflated[jj] >= ii ) {
        
        tryCatch({
          withinfirm_idx_temp = onetoT[nonNA_index_firm_temp==ii-19]:onetoT[nonNA_index_firm_temp==ii]
          xxx0 = xxx[ withinfirm_idx_temp ]          
          FErho_learn_0[jj,ii-19] = arima( xxx0 , order=c(1,0,0), method = c("ML"),optim.control = list(maxit = 300000 ))$coef[1]
          xxx0_0 = c(NA, xxx0[1:(length(xxx0)-1)])
          xxx0_1 = xxx0
          FErho_learn_0_reg[jj,ii-19] =  (lm(xxx0_1 ~ xxx0_0))$coef[2]          
          
           if (ii==20) FErhoadj_firmqtr_20[ firm_idx_temp[withinfirm_idx_temp] ] = FErho_learn_0[jj,ii-19] 
           if (ii==30) FErhoadj_firmqtr_10_30[ firm_idx_temp[withinfirm_idx_temp] ] = FErho_learn_0[jj,ii-19] 
          
          FErhoadj_firmqtr_20_all[ firm_idx_temp[withinfirm_idx_temp[1]] ] = FErho_learn_0[jj,ii-19] 
          FErhoadj_firmqtr_10_30_all[ firm_idx_temp[withinfirm_idx_temp[1]] ] = FErho_learn_0[jj,ii-19] 
          
          
        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
                             erroroccur = TRUE} )
      }
    }
    print(jj)
  }  
  
  FErho_learn_0 =  (FErho_learn_0 *( 20 - 1)+1) / ( 20 - 4)   
  FErhoadj_firmqtr_20 =  (FErhoadj_firmqtr_20  *( 20 - 1)+1) / ( 20 - 4)   
  FErhoadj_firmqtr_10_30 =  (FErhoadj_firmqtr_10_30  *( 20 - 1)+1) / ( 20 - 4)   
  FErhoadj_firmqtr_20_all =  (FErhoadj_firmqtr_20_all  *( 20 - 1)+1) / ( 20 - 4)   
  FErhoadj_firmqtr_10_30_all =  (FErhoadj_firmqtr_10_30_all  *( 20 - 1)+1) / ( 20 - 4)   
  FErho_learn_0_reg=  (FErho_learn_0_reg *( 20 - 1)+1) / ( 20 - 4)

  save(FErho_learn_0, file="FErho_learn_0.Rdata")     
  save(FErhoadj_firmqtr_20 , file="FErhoadj_firmqtr_20.Rdata")     
  save(FErhoadj_firmqtr_10_30 , file="FErhoadj_firmqtr_10_30.Rdata")     
  save(FErhoadj_firmqtr_20_all , file="FErhoadj_firmqtr_20_all.Rdata")     
  save(FErhoadj_firmqtr_10_30_all , file="FErhoadj_firmqtr_10_30_all.Rdata")     
  save(FErho_learn_0_reg, file="FErho_learn_0_reg.Rdata")     
}

 




FErho_yearqtr_adj_firm_avg = rep(NA, N_row_yearqtr )
FErho_yearqtr_adj_firm_SE =  rep(NA, N_row_yearqtr )


load("FErhoadj_firmqtr_10_30.Rdata") 
for (ii in seq( 1,N_row_yearqtr ) ) {    
  
  FErho_yearqtr_adj_firm = FErhoadj_firmqtr_10_30[ yearqtr_idx_numeric   == yearqtr_xaxis[ii]   ]  
  FErho_yearqtr_adj_firm_avg[ii] = mean(FErho_yearqtr_adj_firm ,na.rm=T)
  FErho_yearqtr_adj_firm_SE[ii] = sd(FErho_yearqtr_adj_firm,na.rm=T)/sqrt( sum(!is.na(FErho_yearqtr_adj_firm)) )
  
  print(ii)
}





load("FErhoadj_firmqtr_20_all.Rdata") 
for (ii in seq( 1,N_row_yearqtr ) ) {    
  
  FErho_yearqtr_adj_firm = FErhoadj_firmqtr_20_all[ yearqtr_idx_numeric   == yearqtr_xaxis[ii]   ]  
  FErho_yearqtr_adj_firm_avg[ii] = mean(FErho_yearqtr_adj_firm ,na.rm=T)
  FErho_yearqtr_adj_firm_SE[ii] = sd(FErho_yearqtr_adj_firm,na.rm=T)/sqrt( sum(!is.na(FErho_yearqtr_adj_firm)) )
  
  #print(ii)
  
  print( sum(!is.na(FErho_yearqtr_adj_firm)) )
}


FErho_yearqtr_adj_firm_avg = rep(NA, N_row_yearqtr )
FErho_yearqtr_adj_firm_SE =  rep(NA, N_row_yearqtr )

load("FErhoadj_firmqtr_20.Rdata") 
for (ii in seq( 1,N_row_yearqtr ) ) {    
  
  FErho_yearqtr_adj_firm = FErhoadj_firmqtr_20[ yearqtr_idx_numeric   == yearqtr_xaxis[ii]   ]  
  FErho_yearqtr_adj_firm_avg[ii] = mean(FErho_yearqtr_adj_firm ,na.rm=T)
  FErho_yearqtr_adj_firm_SE[ii] = sd(FErho_yearqtr_adj_firm,na.rm=T)/sqrt( sum(!is.na(FErho_yearqtr_adj_firm)) )
  
  #print(ii)
  
 # print( sum(!is.na(FErho_yearqtr_adj_firm)) )
}
save(FErho_yearqtr_adj_firm_avg,file="FErho_yearqtr_adj_firm_avg.Rdata")
save(FErho_yearqtr_adj_firm_SE,file="FErho_yearqtr_adj_firm_SE.Rdata")


############# pooled rho(FE) learning within a firm ##################

max_no_obs_deflated = max(est$no_obs_deflated)
no_window_learn = max_no_obs_deflated-20+1

if (do_all) {
  
  FErho_learn_pooled.ar = rep(NA, no_window_learn  ) 
  FErho_learn_pooled.reg = rep(NA, no_window_learn  ) 
  FErho_learn_lmer= rep(NA, no_window_learn  ) 
  
  for (ii in c( 20:max_no_obs_deflated   )) {    
    
    total_idx_temp = rep(FALSE, length(est$ye_firm_deflated))
    
    for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
    {  
      firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]     
      nonNA_index_firm_temp = nonNA_index_firm[firm_idx_temp]     
      onetoT = est$TS_index_firm_deflated[firm_idx_temp]
      
      if (est$no_obs_deflated[jj] >= ii ) {        
        total_idx_temp[firm_idx_temp[ onetoT[nonNA_index_firm_temp==ii-19]:onetoT[nonNA_index_firm_temp==ii] ]   ]=TRUE     
      }    
      
    }
    
    pdataset = list(y=total_idx_temp, begin_idx =est$firm_begin_deflated, end_idx = est$firm_end_deflated)
    total_idx_temp_pooled = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )    
    
    tryCatch({
      
      M0 = lmer(FE_temp_pooled ~ FE_temp_pooled_lag1 + (FE_temp_pooled_lag1 | firm_dummy_pooled )   ,subset = total_idx_temp_pooled ,  REML=FALSE)          
      FErho_learn_lmer[ii-19] = fixef(M0)[2]           
      FErho_learn_pooled.reg[ii-19] = lm(FE_temp_pooled ~ FE_temp_pooled_lag1 ,subset = total_idx_temp_pooled )$coef[2]       
      FErho_learn_pooled.ar[ii-19] = arima(FE_temp_pooled[total_idx_temp_pooled], c(1,0,0))$coef[1]
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
                         erroroccur = TRUE} )  
    
    print(ii)
  }
  
  save(FErho_learn_lmer, file="FErho_learn_lmer.Rdata") 
  save(FErho_learn_pooled.reg, file="FErho_learn_pooled.reg.Rdata") 
  save(FErho_learn_pooled.ar, file="FErho_learn_pooled.ar.Rdata") 
  
}
plot(FErho_learn_lmer,type="o")
plot(FErho_learn_pooled.reg,type="o")

plot(FErho_learn_pooled.ar,type="o")
load("FErho_learn_0.Rdata")   
lines( apply(FErho_learn_0,2,mean,na.rm=T),type="l")
plot(FErho_learn_pooled.ar-apply(FErho_learn_0,2,mean,na.rm=T),type="l")
plot(FErho_learn_pooled.reg-apply(FErho_learn_0_reg,2,mean,na.rm=T),type="l")



# Paired t-test
# http://en.wikipedia.org/wiki/Student's_t-test#Dependent_t-test_for_paired_samples
########### ###########  plot in the paper ########### ########### 
########### ###########  plot in the paper ########### ########### 
########### ###########  plot in the paper ########### ########### 


 

par(mfrow=c(1,2))
# year effect on firm-level est
ssd = 1.96
load("FErho_yearqtr_adj_firm_avg.Rdata")
load("FErho_yearqtr_adj_firm_SE.Rdata")
xxx = FErho_yearqtr_adj_firm_avg 
CIlow =   xxx - FErho_yearqtr_adj_firm_SE*ssd   
CIhigh =  xxx + FErho_yearqtr_adj_firm_SE*ssd  
xaxis  = yearqtr_xaxis

T_temp = length(xxx)
xxx = xxx[1:(T_temp-1)]
CIlow =   CIlow[1:(T_temp-1)]
CIhigh =  CIhigh[1:(T_temp-1)]
xaxis  = xaxis[1:(T_temp-1)]

plot(NA,xlim=c(xaxis[1], xaxis[length(xaxis)]),  ylim=c(0,max(CIhigh,na.rm=T)) ,
     main=expression(hat(E)[italic(t[c])]*'[ Autocorr'[italic(i)]*'('*italic('FE')[italic("i, 1:20")]*') ]'),
     xlab=expression('Year: calendar-time quarter '*italic(t[c])),ylab="")
polygon( c(xaxis, xaxis[length(xaxis):1]  ),  
         c(CIlow, CIhigh[length(xaxis):1]), border = NA, col = "gray")
lines(xaxis,xxx, cex=0.5, lwd=1  ,type="l" )
points(xaxis,xxx, cex=0.5, lwd=1    )
abline(h=0, lty=3  )
box()

legend("bottomleft", inset=0.02,
       c(expression(hat(E)[italic(t[c])]*' [ '*rho[italic(i)]*'(FE'[italic("i, 1:20")]*') ]'),
         expression('95% C.I. of '*hat(E)[italic(t[c])]*' [ '*rho[italic(i)]*'(FE'[italic("i, 1:20")]*') ]'),
         ""),
         lty=c(1,1,NA), lwd=c(1,7,NA), bty = "n",
       col=c("black","gray75",NA), pch=c(1,NA,NA), cex=0.75  )


# rho decreasing over age 
load("FErho_learn_0.Rdata")

#FErho_learn_0.1 = (matrix(rep((FErho_learn_0[,1 ])  ,93), ncol=93))
#FErho_learn_0.2 = FErho_learn_0[,1:93]

#FErho_learn_0.1 = FErho_learn_0.1 + FErho_learn_0.2*0
#FErho_learn_0.2 = FErho_learn_0.2 + FErho_learn_0.1*0

#dFErho_learn_mean =  apply( FErho_learn_0.1, 2, mean,na.rm=T) 
#dFErho_learn_SE =  apply(FErho_learn_0.2-FErho_learn_0.1,2, sd,na.rm=T) / sqrt( apply( !is.na(FErho_learn_0.2-FErho_learn_0.1),2, sum) )

x =  FErho_learn_0 
#x = FErho_learn_0_reg


FErho_learn_mean0.1 =  (matrix(rep((x[,1] )  ,93), ncol=93)) + x*0
FErho_learn_mean0.11 = (matrix(rep((x[,11])  ,93), ncol=93)) + x*0 
ssd = 1.96
xxx =  apply(x , 2, mean,na.rm=T)


dFErho_learn_SE0 =  apply(x,2, sd,na.rm=T) / sqrt( apply( !is.na(x ),2, sum) )
CIlow0 = xxx - dFErho_learn_SE0*ssd   
CIhigh0 = xxx + dFErho_learn_SE0*ssd  

dFErho_learn_SE0 =  apply(x-FErho_learn_mean0.11 ,2, sd,na.rm=T) / sqrt( apply( !is.na(x-FErho_learn_mean0.1  ),2, sum) )
dFErho_learn_SE0 =  apply(x-FErho_learn_mean0.1 ,2, sd,na.rm=T) / sqrt( apply( !is.na(x-FErho_learn_mean0.11  ),2, sum) )



CIlow = xxx - dFErho_learn_SE0*ssd   
CIhigh = xxx + dFErho_learn_SE0*ssd  
xaxis  = 1:93

T_temp = length(xxx)
xxx = xxx[1:(T_temp-1)]
CIlow =   CIlow[1:(T_temp-1)]
CIhigh =  CIhigh[1:(T_temp-1)]
CIlow0 =   CIlow0[1:(T_temp-1)]
CIhigh0 =  CIhigh0[1:(T_temp-1)]
xaxis  = xaxis[1:(T_temp-1)]


plot(NA,xlim=c(xaxis[1], xaxis[length(xaxis)]),  ylim=c(0, 0.2774057) ,
     main=expression(hat(E)[italic("t+1:t+20")]*'[ Autocorr'[italic(i)]*'(FE'[italic("i, t+1:t+20")]*') ]'),
     xlab=expression("Quarter "*italic("t")*" since the first observation"),ylab="")
polygon( c(xaxis, xaxis[length(xaxis):1]  ),  
         c(CIlow, CIhigh[length(xaxis):1]), border = NA, col = "gray75")
polygon( c(xaxis, xaxis[length(xaxis):1]  ),  
         c(CIlow0, CIhigh0[length(xaxis):1]), border = NA, col = "gray55")
lines(xaxis,xxx, cex=0.5, lwd=1  ,type="o" )
#points(xaxis,xxx, cex=0.5, lwd=1    )
#abline(h=xxx[1], lty="dashed"  )
#abline(h=0, lty="dashed"  )
box()

lines(xaxis[11:91], apply( FErho_learn_mean0.11  , 2, mean,na.rm=T)[11:91] , cex=0.5, lwd=1  ,type="l", lty="solid"  )
lines(xaxis[1:91], apply( FErho_learn_mean0.1 , 2, mean,na.rm=T)[1:91] , cex=0.5, lwd=1  ,type="l", lty="dashed"  )
abline(h=0, lty=3  )

legend("bottomleft", inset=0.02,
                c(expression(hat(E)[italic("t+1:t+20")]*' [ '*rho[italic(i)]*'(FE'[italic("i, t+1:t+20")]*') ]'),
                  expression(hat(E)[italic("t+1:t+20")]*' [ '*rho[italic(i)]*'(FE'[italic("i, 1:20")]*') ]'),
                  expression(hat(E)[italic("t+1:t+20")]*' [ '*rho[italic(i)]*'(FE'[italic("i, 11:30")]*') ]'),
                  expression('95% C.I. of '*hat(E)[italic("t+1:t+20")]*' [ '*rho[italic(i)]*'(FE'[italic("i, t+1:t+20")]*') ]'),
                  expression('95% no-rejection regieon of '*hat(E)[italic("t+1:t+20")]*' [ '*rho[italic(i)]*'(FE'[italic("i, 1:20")]*') ]'),
                  expression(paste("H"[o]*": "*E[italic("t+1:t+20")]*' [ '*rho[italic(i)]*'(FE'[italic("i, t+1:t+20")]*")",
                                   " - "*rho[italic(i)]*"(FE"[italic("i, 1:20")]*") ]=0"))),
       lty=c(1,2,1,1,1,0), lwd=c(1,1,1,7,7,0), bty = "n",
       col=c("black","black","black","gray55","gray75"), pch=c(1,NA,NA,NA,NA,NA), cex=0.75  )



###################### autocorr_learning.eps ###################### 

########### ###########  end of plot in the paper ########### ########### 
########### ###########  end of plot in the paper ########### ########### 
########### ###########  end of plot in the paper ########### ########### 





























# Paired t-test
# http://en.wikipedia.org/wiki/Student's_t-test#Dependent_t-test_for_paired_samples

par(mfrow=c(1,2))
# year effect on firm-level est
ssd = 1.96
xxx = FErho_yearqtr_adj_firm_avg 
CIlow =   xxx - FErho_yearqtr_adj_firm_SE*ssd   
CIhigh =  xxx + FErho_yearqtr_adj_firm_SE*ssd  
xaxis  = yearqtr_xaxis

T_temp = length(xxx)
xxx = xxx[1:(T_temp-1)]
CIlow =   CIlow[1:(T_temp-1)]
CIhigh =  CIhigh[1:(T_temp-1)]
xaxis  = xaxis[1:(T_temp-1)]

plot(NA,xlim=c(xaxis[1], xaxis[length(xaxis)]),  ylim=c(min(CIlow),max(CIhigh)) ,
     main=expression('Avg'[t]*'[Autocorr'*'(FE'[i]*')]'),
     xlab=expression("Quarter "*italic("t")),ylab="")
polygon( c(xaxis, xaxis[length(xaxis):1]  ),  
         c(CIlow, CIhigh[length(xaxis):1]), border = NA, col = "gray")
lines(xaxis,xxx, cex=0.5, lwd=1  ,type="l" )
points(xaxis,xxx, cex=0.5, lwd=1    )
abline(h=0, lty="dashed"  )
box()


# rho decreasing over age 
load("FErho_learn_0.Rdata")

FErho_learn_0.1 = (matrix(rep(FErho_learn_0[,11],93-10), ncol=93-10))
FErho_learn_0.2 = FErho_learn_0[,11:93]

FErho_learn_0.1 = FErho_learn_0.1 + FErho_learn_0.2*0
FErho_learn_0.2 = FErho_learn_0.2 + FErho_learn_0.1*0

dFErho_learn_mean =  apply(FErho_learn_0.2-FErho_learn_0.1, 2, mean,na.rm=T) 
dFErho_learn_SE =  apply(FErho_learn_0.2-FErho_learn_0.1,2, sd,na.rm=T) / sqrt( apply( !is.na(FErho_learn_0.2-FErho_learn_0.1),2, sum) )


ssd = 1.96
xxx =dFErho_learn_mean
CIlow =   xxx - dFErho_learn_SE*ssd   
CIhigh =  xxx + dFErho_learn_SE*ssd  
xaxis  = 11:93

T_temp = length(xxx)
xxx = xxx[1:(T_temp-2)]
CIlow =   CIlow[1:(T_temp-2)]
CIhigh =  CIhigh[1:(T_temp-2)]
xaxis  = xaxis[1:(T_temp-2)]

plot(NA,xlim=c(xaxis[1], xaxis[length(xaxis)]),  ylim=c(-0.14,0.03) ,
     main=expression('Autocorr'*'(FE'['t+1:t+20']*')'*'-'*'Autocorr'*'(FE'['11:30']*')'),
     xlab=expression("Quarter "*italic("t")),ylab="")
polygon( c(xaxis, xaxis[length(xaxis):1]  ),  
         c(CIlow, CIhigh[length(xaxis):1]), border = NA, col = "gray")
lines(xaxis,xxx, cex=0.5, lwd=1  ,type="l" )
points(xaxis,xxx, cex=0.5, lwd=1    )
abline(h=0, lty="dashed"  )
box()
##### still rho(FE) decreases even if focus on the matching pairs over time.





 






 

   



############ learning bias & phi within firm  ##########
#1:20 - 21:40
#2:21 - 22:41
#3:22 - 23:42


############ learning bias & phi within firm  ##########
#1:20 - 21:40
#2:21 - 22:41
#3:22 - 23:42

max_no_obs_deflated = max(est$no_obs_deflated)
no_window_learn = max_no_obs_deflated-20+1

if (do_all) {
  
  FEbias_learn_0 = matrix(rep(NA, no_window_learn*length(est$firm_begin_deflated )  ), ncol= no_window_learn) 
  FEbias_learn_0.lowball = matrix(rep(NA, no_window_learn*length(est$firm_begin_deflated )  ), ncol= no_window_learn) 
  
  phiphihat_learn_0 = matrix(rep(NA, no_window_learn*length(est$firm_begin_deflated )  ), ncol= no_window_learn) 
  phiphihat_learn_0.alt = matrix(rep(NA, no_window_learn*length(est$firm_begin_deflated )  ), ncol= no_window_learn) 
  
  phi_learn_0 = matrix(rep(NA, no_window_learn*length(est$firm_begin_deflated )  ), ncol= no_window_learn) 
  phihat_learn_0= phi_learn_0
  phi_learn_0.alt = phi_learn_0
  phihat_learn_0.alt= phi_learn_0
  
  
  for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
  {  
    firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]     
    nonNA_index_firm_temp = nonNA_index_firm[firm_idx_temp]
    xxx = yef_temp[firm_idx_temp]
    
    y = ye_temp[firm_idx_temp]
    yye = y
    yye.L1 = c(NA,y[1:(length(y)-1) ])
    
    x = yf_temp[firm_idx_temp]  
    yyf = x 
    yyf.L1 = c(NA,x[1:(length(x)-1) ])  
    
        #  phihat_firms[jj] = Reg2$coef[2]    
    onetoT = est$TS_index_firm_deflated[firm_idx_temp]
    
    for (ii in c( 20:est$no_obs_deflated[jj] )) {
      
      
      
      if (est$no_obs_deflated[jj] >= ii ) {
        
        tryCatch({
          
          xxx0=xxx[ onetoT[nonNA_index_firm_temp==ii-19]:onetoT[nonNA_index_firm_temp==ii] ]        
          FEbias_learn_0[jj,ii-19] = mean(xxx0,na.rm=T)          
          FEbias_learn_0.lowball[jj,ii-19] = (mean(xxx0<0,na.rm=T)+1-mean(xxx0>0,na.rm=T))/2
                    
          yye0=yye[ onetoT[nonNA_index_firm_temp==ii-19]:onetoT[nonNA_index_firm_temp==ii] ]               
          yye0.L1=yye.L1[ onetoT[nonNA_index_firm_temp==ii-19]:onetoT[nonNA_index_firm_temp==ii] ]          
          yyf0=yyf[ onetoT[nonNA_index_firm_temp==ii-19]:onetoT[nonNA_index_firm_temp==ii] ]          
          yyf0.L1=yyf.L1[ onetoT[nonNA_index_firm_temp==ii-19]:onetoT[nonNA_index_firm_temp==ii] ]          
          
          Reg1 = lm(yye0 ~    yye0.L1         )
          Reg2 = lm(yyf0 ~    yye0.L1 + yyf0.L1        )
          Reg3 = lm(yyf0 ~    yye0.L1         )
          
          phiphihat_learn_0[jj,ii-19] = Reg1$coef[2]- (Reg2$coef[2]+Reg2$coef[3])
          phiphihat_learn_0.alt[jj,ii-19] = Reg1$coef[2]- Reg3$coef[2]
          
          phi_learn_0[jj,ii-19] = Reg1$coef[2]  
          phihat_learn_0[jj,ii-19] =  (Reg2$coef[2]+Reg2$coef[3])
          phi_learn_0.alt[jj,ii-19] = Reg1$coef[2]
          phihat_learn_0.alt[jj,ii-19] = Reg3$coef[2]
          
    
          
          
        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
                             erroroccur = TRUE} )
      }
    }
    print(jj)
  }
  
  save(FEbias_learn_0, file="FEbias_learn_0.Rdata")
  save(FEbias_learn_0.lowball, file="FEbias_learn_0.lowball.Rdata")
  save(phiphihat_learn_0, file="phiphihat_learn_0.Rdata")
  save(phiphihat_learn_0.alt, file="phiphihat_learn_0.alt.Rdata")
  
  save(phi_learn_0, file="phi_learn_0.Rdata")
  save(phihat_learn_0, file="phihat_learn_0.Rdata")
  save(phi_learn_0.alt, file="phi_learn_0.alt.Rdata")
  save(phihat_learn_0.alt, file="phihat_learn_0.alt.Rdata")
  
}





max_no_obs_deflated = max(est$no_obs_deflated)
no_window_learn = max_no_obs_deflated-20+1

if (do_all) {
  
  phiphihat_learn_firmqtr_20  = rep(NA, length(est$ye_firm_deflated ))
  phiphihat_learn_firmqtr_10_30 = rep(NA, length(est$ye_firm_deflated ))
  
  phiphihat_learn_firmqtr_20_all = rep(NA, length(est$ye_firm_deflated ))
  
  
  FEbias_learn_firmqtr_20.lowball = rep(NA, length(est$ye_firm_deflated ))
  FEbias_learn_firmqtr_10_30.lowball= rep(NA, length(est$ye_firm_deflated ))
  FEbias_learn_firmqtr_20_all.lowball= rep(NA, length(est$ye_firm_deflated ))
  
  for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
  {  
    firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]     
    nonNA_index_firm_temp = nonNA_index_firm[firm_idx_temp]
 
    #  phihat_firms[jj] = Reg2$coef[2]    
    onetoT = est$TS_index_firm_deflated[firm_idx_temp]
    
    for (ii in c( 20:est$no_obs_deflated[jj] )) {
      
      
      
      if (est$no_obs_deflated[jj] >= ii ) {
        
        tryCatch({
          
           
          withinfirm_idx_temp = onetoT[nonNA_index_firm_temp==ii-19]:onetoT[nonNA_index_firm_temp==ii]
          
          
          if (ii==20) phiphihat_learn_firmqtr_20[ firm_idx_temp[withinfirm_idx_temp] ] = phiphihat_learn_0.alt[jj,ii-19] 
          if (ii==30) phiphihat_learn_firmqtr_10_30[ firm_idx_temp[withinfirm_idx_temp] ] =  phiphihat_learn_0.alt[jj,ii-19] 
          
          phiphihat_learn_firmqtr_20_all[ firm_idx_temp[withinfirm_idx_temp[1]] ] =  phiphihat_learn_0.alt[jj,ii-19] 
    
      
          
          if (ii==20) FEbias_learn_firmqtr_20.lowball[ firm_idx_temp[withinfirm_idx_temp] ] = FEbias_learn_0.lowball[jj,ii-19]
          if (ii==30) FEbias_learn_firmqtr_10_30.lowball[ firm_idx_temp[withinfirm_idx_temp] ] =  FEbias_learn_0.lowball[jj,ii-19] 
          
          FEbias_learn_firmqtr_20_all.lowball[ firm_idx_temp[withinfirm_idx_temp[1]] ] = FEbias_learn_0.lowball[jj,ii-19]

          
          
        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
                             erroroccur = TRUE} )
      } 
    }
    print(jj)
  }
  
  save(phiphihat_learn_firmqtr_20, file="phiphihat_learn_firmqtr_20.Rdata")
  save(phiphihat_learn_firmqtr_10_30, file="phiphihat_learn_firmqtr_10_30.Rdata")
    
  save(phiphihat_learn_firmqtr_20_all, file="phiphihat_learn_firmqtr_20_all.Rdata")
  
  save(FEbias_learn_firmqtr_20.lowball, file="FEbias_learn_firmqtr_20_lowball.Rdata")
  save(FEbias_learn_firmqtr_10_30.lowball, file="FEbias_learn_firmqtr_10_30_lowball.Rdata")
  save(FEbias_learn_firmqtr_20_all.lowball, file="FEbias_learn_firmqtr_20_all_lowball.Rdata")
  
}



phiphihat_learn_yearqtr_firm_avg = rep(NA, N_row_yearqtr )
phiphihat_learn_yearqtr_firm_var =  rep(NA, N_row_yearqtr )
phiphihat_learn_yearqtr_firm_SE =  rep(NA, N_row_yearqtr )
phiphihat_learn_yearqtr_firm_MSE =  rep(NA, N_row_yearqtr )

x = phiphihat_learn_firmqtr_20
for (ii in seq( 1,N_row_yearqtr ) ) {   
  phiphihat_learn_yearqtr_firm = x[ yearqtr_idx_numeric   == yearqtr_xaxis[ii]   ]  
  phiphihat_learn_yearqtr_firm_avg[ii] = mean(phiphihat_learn_yearqtr_firm ,na.rm=T)
  phiphihat_learn_yearqtr_firm_var[ii] = var(phiphihat_learn_yearqtr_firm ,na.rm=T)
  phiphihat_learn_yearqtr_firm_SE[ii] = sd(phiphihat_learn_yearqtr_firm,na.rm=T)/sqrt( sum(!is.na(phiphihat_learn_yearqtr_firm)) )  
  phiphihat_learn_yearqtr_firm_MSE[ii] = mean(phiphihat_learn_yearqtr_firm^2 ,na.rm=T)
  #print(ii)  
  print( sum(!is.na(phiphihat_learn_yearqtr_firm)) )

}
x =   phiphihat_learn_yearqtr_firm_avg
plot(x,type="o")
x = phiphihat_learn_yearqtr_firm_var
plot(x,type="o")


x = FEbias_learn_firmqtr_20.lowball
for (ii in seq( 1,N_row_yearqtr ) ) {   
  phiphihat_learn_yearqtr_firm = x[ yearqtr_idx_numeric   == yearqtr_xaxis[ii]   ]  
  phiphihat_learn_yearqtr_firm_avg[ii] = mean(phiphihat_learn_yearqtr_firm ,na.rm=T)
  phiphihat_learn_yearqtr_firm_var[ii] = var(phiphihat_learn_yearqtr_firm ,na.rm=T)
  phiphihat_learn_yearqtr_firm_SE[ii] = sd(phiphihat_learn_yearqtr_firm,na.rm=T)/sqrt( sum(!is.na(phiphihat_learn_yearqtr_firm)) )  
  phiphihat_learn_yearqtr_firm_MSE[ii] = mean(phiphihat_learn_yearqtr_firm^2 ,na.rm=T)  
  #print(ii)  
  print( sum(!is.na(phiphihat_learn_yearqtr_firm)) )
  
}

x = phiphihat_learn_yearqtr_firm_var
plot(x,type="o")
 
x =   phiphihat_learn_yearqtr_firm_avg
plot(x,type="o")


############ learning  phi within firm using lmer ##########
#1:20 - 21:40
#2:21 - 22:41
#3:22 - 23:42

max_no_obs_deflated = max(est$no_obs_deflated)
no_window_learn = max_no_obs_deflated-20+1

if (do_all) {
  
  phiphihat_learn_lmer0 = matrix(rep(NA, no_window_learn*length(est$firm_begin_deflated )  ), ncol= no_window_learn) 
  phiphihat_learn_lmer_var0 = rep(NA, no_window_learn  ) 
  phiphihat_learn_lmer_se0= rep(NA, no_window_learn  ) 
  
  for (ii in c( 20:max_no_obs_deflated   )) {    
    
    total_idx_temp = rep(FALSE, length(est$ye_firm_deflated))
    
    for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
    {  
      firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]     
      nonNA_index_firm_temp = nonNA_index_firm[firm_idx_temp]     
      onetoT = est$TS_index_firm_deflated[firm_idx_temp]
      
      if (est$no_obs_deflated[jj] >= ii ) {        
        total_idx_temp[firm_idx_temp[ onetoT[nonNA_index_firm_temp==ii-19]:onetoT[nonNA_index_firm_temp==ii] ]   ]=TRUE     
      }    
      
    }
    
    pdataset = list(y=total_idx_temp, begin_idx =est$firm_begin_deflated, end_idx = est$firm_end_deflated)
    total_idx_temp_pooled = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
    total_idx_temp_pooled[is.na(total_idx_temp_pooled)] = TRUE 
    
    tryCatch({
      
      M0.phiphihat = lmer(FE_temp_pooled ~ ye_temp_pooled_lag1 + (ye_temp_pooled_lag1 | firm_dummy_pooled )   ,subset = total_idx_temp_pooled ,  REML=FALSE)
      phiphihat_learn_lmer0[table(firm_dummy_pooled[total_idx_temp_pooled])>0,ii-19] = fixef(M0.phiphihat)[2]+ranef(M0.phiphihat)$firm_dummy_pooled$"ye_temp_pooled_lag1"
      phiphihat_learn_lmer_var0[ii-19] =  (attr( VarCorr(M0.phiphihat)$firm_dummy_pooled ,"stddev")[2])^2
      phiphihat_learn_lmer_se0[ii-19] = sqrt(vcov(M0.phiphihat)[2,2])
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
                         erroroccur = TRUE} )  
    
    print(ii)
  }
  
  save(phiphihat_learn_lmer0, file="phiphihat_learn_lmer0.Rdata") 
  save(phiphihat_learn_lmer_var0, file="phiphihat_learn_lmer_var0.Rdata") 
  save(phiphihat_learn_lmer_se0, file="phiphihat_learn_lmer_se0.Rdata") 
  
}

############ learning  phi over firms (calendar) using lmer ##########
#1:20 - 21:40
#2:21 - 22:41
#3:22 - 23:42

max_no_obs_deflated = max(est$no_obs_deflated)
no_window_learn = max_no_obs_deflated-20+1

if (do_all) {
   
  
  phiphihat_learn_firmqtr_lmer_var0 =  rep(NA,length(yearqtr_xaxis) ) 
    
    for (ii in c( 1:length(yearqtr_xaxis)   )) {    
      
      total_idx_temp = rep(FALSE, length(est$ye_firm_deflated)) 
      set_firm_idx_temp = est$firm_deflated_nn_idx[  yearqtr_idx_numeric   == yearqtr_xaxis[ii]    ]
      
      for (jj in  set_firm_idx_temp  )
      { 
                
        firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]          
        total_idx_temp[  firm_idx_temp[!is.na(phiphihat_learn_firmqtr_20[firm_idx_temp])]  ]=TRUE     
        
      }
      
      pdataset = list(y=total_idx_temp, begin_idx =est$firm_begin_deflated, end_idx = est$firm_end_deflated)
      total_idx_temp_pooled = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
      #total_idx_temp_pooled[is.na(total_idx_temp_pooled)] = TRUE 
      
      tryCatch({
        
        M0.phiphihat = lmer(FE_temp_pooled ~ ye_temp_pooled_lag1 + (ye_temp_pooled_lag1 | firm_dummy_pooled )   ,subset = total_idx_temp_pooled ,  REML=FALSE)
        phiphihat_learn_firmqtr_lmer_var0[ii] =  (attr( VarCorr(M0.phiphihat)$firm_dummy_pooled ,"stddev")[2])^2        
        
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
                           erroroccur = TRUE} )  
       
    print(ii)
  }
  

  save(phiphihat_learn_firmqtr_lmer_var0, file="phiphihat_learn_firmqtr_lmer_var0.Rdata") 

  
}

plot(phiphihat_learn_firmqtr_lmer_var0,type="o",ylim=c(0,0.02))
lines(phiphihat_learn_yearqtr_firm_var-0.015,type="l")






############ learning mu within firm using lmer (not quatntile 0) ##########
#1:20 - 21:40
#2:21 - 22:41
#3:22 - 23:42
max_no_obs_deflated = max(est$no_obs_deflated)
no_window_learn = max_no_obs_deflated-20+1

if (do_all) {

  FE_bias_learn_lmer_mu_var0.fixed = rep(NA, no_window_learn  ) 
  FE_bias_learn_lmer_mu_var0.random = rep(NA, no_window_learn  ) 
  FE_rho_learn_lmer.fixed = rep(NA, no_window_learn  ) 
  FE_rho_learn_lmer.random = rep(NA, no_window_learn  ) 
  FE_bias_learn_lmer_et_var0.fixed = rep(NA, no_window_learn  ) 
  FE_bias_learn_lmer_et_var0.random = rep(NA, no_window_learn  ) 
  FE_rho_learn_lmer_se.fixed = rep(NA, no_window_learn  ) 
  FE_rho_learn_lmer_se.random = rep(NA, no_window_learn  ) 
  FE_bias_learn_lmer_mu.random = rep(NA, no_window_learn  ) 
  FE_bias_learn_lmer_mu_se.random = rep(NA, no_window_learn  ) 
  
  FE_rho_learn_lmer0.random = matrix(rep(NA, no_window_learn*length(est$firm_begin_deflated )  ), ncol= no_window_learn) 
  FE_bias_learn_lmer0.random = matrix(rep(NA, no_window_learn*length(est$firm_begin_deflated )  ), ncol= no_window_learn) 
  
  ye_mean_lmer_et_var0.random = rep(NA, no_window_learn  ) 
  
  
  FE_bias_learn_lmer_mu_var_1.random = rep(NA, no_window_learn  ) 
  FE_bias_learn_lmer_et_var_1.random = rep(NA, no_window_learn  ) 
  FE_rho_learn_lmer_1.random = rep(NA, no_window_learn  ) 
  
  for (ii in c( 20:max_no_obs_deflated   )) {    
    
    total_idx_temp = rep(FALSE, length(est$ye_firm_deflated))
    total_idx_temp_1_temp = rep(FALSE, length(est$ye_firm_deflated))
    
    
    for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
    {  
      firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]     
      nonNA_index_firm_temp = nonNA_index_firm[firm_idx_temp]     
      onetoT = est$TS_index_firm_deflated[firm_idx_temp]
      
      if (est$no_obs_deflated[jj] >= ii ) {        
        total_idx_temp[firm_idx_temp[ onetoT[nonNA_index_firm_temp==ii-19]:onetoT[nonNA_index_firm_temp==ii] ]   ]=TRUE     
        
        if (ii>20) total_idx_temp_1_temp[  firm_idx_temp[ onetoT[nonNA_index_firm_temp==1]:onetoT[nonNA_index_firm_temp==20] ]    ] = TRUE                    
      }    
      
    }
    
    pdataset = list(y=total_idx_temp, begin_idx =est$firm_begin_deflated, end_idx = est$firm_end_deflated)
    total_idx_temp_pooled = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
    
    if (ii==20) total_idx_temp_1 = total_idx_temp
    
    tryCatch({      
 
      M0.quant0 = lmer(FE_temp_pooled ~  FE_temp_pooled_lag1 +(1| firm_dummy_pooled )   ,subset = total_idx_temp_pooled ,  REML=FALSE)

      FE_bias_learn_lmer_mu_var0.fixed[ii-19] = (attr( VarCorr(M0.quant0)$firm_dummy_pooled ,"stddev")[1])^2            
      FE_rho_learn_lmer.fixed[ii-19] = fixef(M0.quant0)[2]
      FE_bias_learn_lmer_et_var0.fixed[ii-19] = attr(VarCorr(M0.quant0), "sc")^2
      FE_rho_learn_lmer_se.fixed[ii-19] = sqrt(vcov(M0.quant0)[2,2])
      
      
      
      M0.quant0 = lmer(FE_temp_pooled ~  FE_temp_pooled_lag1 +(1| firm_dummy_pooled )+ (-1+FE_temp_pooled_lag1 | firm_dummy_pooled )   ,subset = total_idx_temp_pooled ,  REML=FALSE)
      
      FE_bias_learn_lmer_mu_var0.random[ii-19] = (attr( VarCorr(M0.quant0)$firm_dummy_pooled ,"stddev")[1])^2            
      FE_bias_learn_lmer_mu.random[ii-19] = fixef(M0.quant0)[1]
      FE_bias_learn_lmer_mu_se.random[ii-19] = sqrt(vcov(M0.quant0)[1,1])
      FE_rho_learn_lmer.random[ii-19] = fixef(M0.quant0)[2]
      FE_bias_learn_lmer_et_var0.random[ii-19] = attr(VarCorr(M0.quant0), "sc")^2
      FE_rho_learn_lmer_se.random[ii-19] = sqrt(vcov(M0.quant0)[2,2])
      FE_rho_learn_lmer0.random[table(firm_dummy_pooled[total_idx_temp_pooled])>0,ii-19] = fixef(M0.quant0)[2]+ranef(M0.quant0)$firm_dummy_pooled$"FE_temp_pooled_lag1"     
      FE_bias_learn_lmer0.random[table(firm_dummy_pooled[total_idx_temp_pooled])>0,ii-19] = fixef(M0.quant0)[1]+ranef(M0.quant0)$firm_dummy_pooled$"(Intercept)"      

      M0.quant0 = lmer(ye_temp_pooled ~  ye_temp_pooled_lag1 +(1| firm_dummy_pooled )+ (-1+FE_temp_pooled_lag1 | firm_dummy_pooled )   ,subset = total_idx_temp_pooled ,  REML=FALSE)
      ye_mean_lmer_et_var0.random[ii-19] = attr(VarCorr(M0.quant0), "sc")^2
 

      if (ii==20) total_idx_temp_1_temp =total_idx_temp
      pdataset = list(y=total_idx_temp_1_temp, begin_idx =est$firm_begin_deflated, end_idx = est$firm_end_deflated)
      total_idx_temp_pooled = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )      
      M0.quant0 = lmer(FE_temp_pooled ~  FE_temp_pooled_lag1 +(1| firm_dummy_pooled )+ (-1+FE_temp_pooled_lag1 | firm_dummy_pooled )   ,subset = total_idx_temp_pooled ,  REML=FALSE)
      FE_bias_learn_lmer_mu_var_1.random[ii-19] = (attr( VarCorr(M0.quant0)$firm_dummy_pooled ,"stddev")[1])^2            
      FE_bias_learn_lmer_et_var_1.random[ii-19] = attr(VarCorr(M0.quant0), "sc")^2
      FE_rho_learn_lmer_1.random[ii-19] = fixef(M0.quant0)[2]
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
                         erroroccur = TRUE} )  
    
    print(ii)
  }
  
  #save(FE_quant0_learn_lmer0, file="FE_quant0_learn_lmer0.Rdata") 
  #save(FE_quant0_learn_lmer_var0, file="FE_quant0_learn_lmer_var0.Rdata") 
  #save(FE_quant0_learn_lmer_se0, file="FE_quant0_learn_lmer_se0.Rdata") 
  
}

plot(sqrt(ye_mean_lmer_et_var0.random))

#intercept SD
x=sqrt(FE_bias_learn_lmer_mu_var0.fixed/FE_bias_learn_lmer_et_var0.fixed)
plot(x,type="o")
x=sqrt(FE_bias_learn_lmer_mu_var0.fixed/FE_bias_learn_lmer_et_var0.fixed*(1-FE_rho_learn_lmer.fixed^2))
plot(x,type="o")

# mu SD
x=sqrt(FE_bias_learn_lmer_mu_var0.fixed/FE_bias_learn_lmer_et_var0.fixed)/(1-FE_rho_learn_lmer.fixed)
plot(x,type="o")
x=sqrt(FE_bias_learn_lmer_mu_var0.fixed/FE_bias_learn_lmer_et_var0.fixed*(1-FE_rho_learn_lmer.fixed^2))/(1-FE_rho_learn_lmer.fixed)
plot(x,type="o")

x=FE_rho_learn_lmer.fixed
plot(x,type="o")
xx=FE_rho_learn_lmer_se.fixed
plot(x+2*xx,type="o",ylim=c(-0.01,0.3))
lines(x-2*xx,type="o")

abline(v=23)

x=FE_rho_learn_lmer_se.fixed
plot(x,type="o")


# mu SD
x=sqrt(FE_bias_learn_lmer_mu_var0.random/FE_bias_learn_lmer_et_var0.random*(1-FE_rho_learn_lmer.random^2))/(1-FE_rho_learn_lmer.random)
plot(x,type="o")
x=sqrt(FE_bias_learn_lmer_mu_var0.random)/(1-FE_rho_learn_lmer.random)/sqrt(FE_bias_learn_lmer_et_var0.random)
plot(x^2,type="o")
FE_bias_learn_lmer_1.random = (matrix(rep(( FE_bias_learn_lmer0.random[,1])  ,93), ncol=93)) + FE_bias_learn_lmer0.random*0
lines(  (x[1]*apply(FE_bias_learn_lmer_1.random, 2, sd,na.rm=T) /sd( FE_bias_learn_lmer_1.random[,1],na.rm=T))^2, lty="dashed")
lines( (x[1]/sqrt(FE_bias_learn_lmer_mu_var_1.random)*sqrt(FE_bias_learn_lmer_mu_var0.random)/sqrt(FE_bias_learn_lmer_et_var0.random)*sqrt(FE_bias_learn_lmer_et_var_1.random))^2   , lty="dashed")
abline(h=x[1]^2,lty=3)

# working

x=FE_rho_learn_lmer.random
plot(x,type="o", ylim=c(0,0.25) )
xx=FE_rho_learn_lmer_se.random
lines(x+2*xx,type="l",ylim=c(-0.01,0.3))
lines(x-2*xx,type="l")
abline(v=23)
FE_rho_learn_lmer_0_1.random = (matrix(rep(( FE_rho_learn_lmer0.random[,1])  ,93), ncol=93)) + FE_rho_learn_lmer0.random*0
lines(apply( FE_rho_learn_lmer_0_1.random, 2, mean,na.rm=T), lty="dashed")
lines( FE_rho_learn_lmer_1.random, lty="dashed")


x= FE_bias_learn_lmer_mu.random/(1-FE_rho_learn_lmer.random)/sqrt(FE_bias_learn_lmer_et_var0.random)
plot(x,type="o")

xx= FE_bias_learn_lmer_mu_se.random/(1-FE_rho_learn_lmer.random)
plot(x+2*xx,type="o")
lines(x-2*xx,type="o")



############ learning mu within firm using lmer ##########
#1:20 - 21:40
#2:21 - 22:41
#3:22 - 23:42
FE_temp_pooled_quant0 = FE_temp_pooled
FE_temp_pooled_quant0[FE_temp_pooled>0] = 0
FE_temp_pooled_quant0[FE_temp_pooled<0] = 1
FE_temp_pooled_quant0[FE_temp_pooled==0] = 1/2
max_no_obs_deflated = max(est$no_obs_deflated)
no_window_learn = max_no_obs_deflated-20+1

if (do_all) {
  
  FE_quant0_learn_lmer0 = matrix(rep(NA, no_window_learn*length(est$firm_begin_deflated )  ), ncol= no_window_learn) 
  FE_quant0_learn_lmer_var0 = rep(NA, no_window_learn  ) 
  FE_quant0_learn_lmer_se0= rep(NA, no_window_learn  ) 
  
  for (ii in c( 20:max_no_obs_deflated   )) {    
    
    total_idx_temp = rep(FALSE, length(est$ye_firm_deflated))
    
    for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
    {  
      firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]     
      nonNA_index_firm_temp = nonNA_index_firm[firm_idx_temp]     
      onetoT = est$TS_index_firm_deflated[firm_idx_temp]
      
      if (est$no_obs_deflated[jj] >= ii ) {        
        total_idx_temp[firm_idx_temp[ onetoT[nonNA_index_firm_temp==ii-19]:onetoT[nonNA_index_firm_temp==ii] ]   ]=TRUE     
      }    
      
    }
    
    pdataset = list(y=total_idx_temp, begin_idx =est$firm_begin_deflated, end_idx = est$firm_end_deflated)
    total_idx_temp_pooled = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
    total_idx_temp_pooled[is.na(total_idx_temp_pooled)] = TRUE 
    
    tryCatch({
      
      M0.quant0 = lmer(FE_temp_pooled_quant0 ~  (1 | firm_dummy_pooled )   ,subset = total_idx_temp_pooled ,  REML=FALSE)
      
      FE_quant0_learn_lmer0[table(firm_dummy_pooled[total_idx_temp_pooled])>0,ii-19] = fixef(M0.quant0)[1]+ranef(M0.quant0)$firm_dummy_pooled$"(Intercept)"
      FE_quant0_learn_lmer_var0[ii-19] =  (attr( VarCorr(M0.quant0)$firm_dummy_pooled ,"stddev")[1])^2      
      FE_quant0_learn_lmer_se0[ii-19] = sqrt(vcov(M0.quant0)[1,1])
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
                         erroroccur = TRUE} )  
    
    print(ii)
  }
  
  save(FE_quant0_learn_lmer0, file="FE_quant0_learn_lmer0.Rdata") 
  save(FE_quant0_learn_lmer_var0, file="FE_quant0_learn_lmer_var0.Rdata") 
  save(FE_quant0_learn_lmer_se0, file="FE_quant0_learn_lmer_se0.Rdata") 
  
}

############ learning bias & phi within firm (excluding exact zeros )  ##########
#1:20 - 21:40
#2:21 - 22:41
#3:22 - 23:42

max_no_obs_deflated = max(est$no_obs_deflated)
no_window_learn = max_no_obs_deflated-20+1

if (do_all) {
  
   FEbias_learn_0.lowball_adj = matrix(rep(NA, no_window_learn*length(est$firm_begin_deflated )  ), ncol= no_window_learn) 
  
    
  
  for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
  {  
    firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]     
    nonNA_index_firm_temp = nonNA_index_firm[firm_idx_temp]
    xxx = yef_temp[firm_idx_temp]
    
    y = ye_temp[firm_idx_temp]
    yye = y
    yye.L1 = c(NA,y[1:(length(y)-1) ])
    
    x = yf_temp[firm_idx_temp]  
    yyf = x 
    yyf.L1 = c(NA,x[1:(length(x)-1) ])  
    
    #  phihat_firms[jj] = Reg2$coef[2]    
    onetoT = est$TS_index_firm_deflated[firm_idx_temp]
    
    for (ii in c( 20:est$no_obs_deflated[jj] )) {
      
      
      
      if (est$no_obs_deflated[jj] >= ii ) {
        
        tryCatch({
          
          xxx0=xxx[ onetoT[nonNA_index_firm_temp==ii-19]:onetoT[nonNA_index_firm_temp==ii] ]                      
          FEbias_learn_0.lowball_adj[jj,ii-19] = sum(xxx0<0,na.rm=T)/sum(xxx0!=0,na.rm=T)
           
        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
                             erroroccur = TRUE} )
      }
    }
    print(jj)
  }
  
  save(FEbias_learn_0.lowball_adj, file="FEbias_learn_0.lowball_adj.Rdata")
    
}


############ learning mu within firm using lmer  (excluding exact zeros )   ##########
#1:20 - 21:40
#2:21 - 22:41
#3:22 - 23:42
FE_temp_pooled_quant0_adj = FE_temp_pooled
FE_temp_pooled_quant0_adj[FE_temp_pooled>0] = 0
FE_temp_pooled_quant0_adj[FE_temp_pooled<0] = 1
FE_temp_pooled_quant0_adj[FE_temp_pooled==0] = 1/2

max_no_obs_deflated = max(est$no_obs_deflated)
no_window_learn = max_no_obs_deflated-20+1

if (do_all) {
  
  FE_quant0_learn_lmer0_adj = matrix(rep(NA, no_window_learn*length(est$firm_begin_deflated )  ), ncol= no_window_learn) 
  FE_quant0_learn_lmer_var0_adj = rep(NA, no_window_learn  ) 
  FE_quant0_learn_lmer_se0_adj= rep(NA, no_window_learn  ) 
  
  for (ii in c( 20:max_no_obs_deflated   )) {    
    
    total_idx_temp = rep(FALSE, length(est$ye_firm_deflated))
    
    for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
    {  
      firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]     
      nonNA_index_firm_temp = nonNA_index_firm[firm_idx_temp]     
      onetoT = est$TS_index_firm_deflated[firm_idx_temp]
      
      if (est$no_obs_deflated[jj] >= ii ) {        
        total_idx_temp[firm_idx_temp[ onetoT[nonNA_index_firm_temp==ii-19]:onetoT[nonNA_index_firm_temp==ii] ]   ]=TRUE     
      }    
      
    }
    
    pdataset = list(y=total_idx_temp, begin_idx =est$firm_begin_deflated, end_idx = est$firm_end_deflated)
    total_idx_temp_pooled = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
    total_idx_temp_pooled[is.na(total_idx_temp_pooled)] = TRUE 
    total_idx_temp_pooled[FE_temp_pooled_quant0_adj==1/2] = FALSE 
    
    tryCatch({
      
      M0.quant0 = lmer(FE_temp_pooled_quant0_adj ~  (1 | firm_dummy_pooled )   ,subset = total_idx_temp_pooled ,  REML=FALSE)
      FE_quant0_learn_lmer0_adj[table(firm_dummy_pooled[total_idx_temp_pooled])>0,ii-19] = fixef(M0.quant0)[1]+ranef(M0.quant0)$firm_dummy_pooled$"(Intercept)"
      FE_quant0_learn_lmer_var0_adj[ii-19] =  (attr( VarCorr(M0.quant0)$firm_dummy_pooled ,"stddev")[1])^2
      FE_quant0_learn_lmer_se0_adj[ii-19] = sqrt(vcov(M0.quant0)[1,1])
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
                         erroroccur = TRUE} )  
    
    print(ii)
  }
  
  save(FE_quant0_learn_lmer0_adj, file="FE_quant0_learn_lmer0_adj.Rdata") 
  save(FE_quant0_learn_lmer_var0_adj, file="FE_quant0_learn_lmer_var0_adj.Rdata") 
  save(FE_quant0_learn_lmer_se0_adj, file="FE_quant0_learn_lmer_se0_adj.Rdata") 
  
}


########################### plot in the paper ##########################
########################### plot in the paper ##########################
########################### plot in the paper ##########################


load("FEbias_learn_0.Rdata")
load("FEbias_learn_0.lowball.Rdata")
load( "FE_quant0_learn_lmer0.Rdata") 
load( "FE_quant0_learn_lmer_var0.Rdata") 
load( "FE_quant0_learn_lmer_se0.Rdata") 

load("FEbias_learn_0.lowball_adj.Rdata")
load( "FE_quant0_learn_lmer_var0_adj.Rdata") 
#FEbias_learn_0.lowball = FEbias_learn_0.lowball_adj
#FE_quant0_learn_lmer_var0 = FE_quant0_learn_lmer_var0_adj

par(mfrow=c(1,2))


# mu learning: plot (1)
ssd = 1.96



x =  FEbias_learn_0.lowball
xxx =  (apply( x     , 2, mean,na.rm=T))  
x_SE =     sqrt(xxx*(1-xxx) ) / sqrt( apply( !is.na(x ),2, sum) )


FEbias_learn_0.1.lowball = (matrix(rep((FEbias_learn_0.lowball[,1])  ,93), ncol=93)) + x*0
FEbias_learn_0.11.lowball = (matrix(rep((FEbias_learn_0.lowball[,11])  ,93), ncol=93)) + x*0
xxx.1 =  (apply( FEbias_learn_0.1.lowball     , 2, mean,na.rm=T)) 
x_SE.1 =     sqrt(xxx.1*(1-xxx.1) ) / sqrt( apply( !is.na(FEbias_learn_0.1.lowball ),2, sum) )
x_SE.1 = sqrt(x_SE^2 + x_SE.1^2)
CIlow.1 = xxx -x_SE.1*ssd
CIhigh.1 = xxx +x_SE.1*ssd

nset=apply(!is.na(cbind( x )),2,sum)
CIlow = xxx -x_SE*ssd
CIhigh = xxx +x_SE*ssd

xaxis  = 1:93

T_temp = length(xxx)
xxx = xxx[1:(T_temp-1)]
CIlow =   CIlow[1:(T_temp-1)]
CIhigh =  CIhigh[1:(T_temp-1)]
xaxis  = xaxis[1:(T_temp-1)]
CIlow.1 =   CIlow.1[1:(T_temp-1)]
CIhigh.1 =  CIhigh.1[1:(T_temp-1)]

## plot in the paper
plot(NA,xlim=c(xaxis[1]-5, xaxis[length(xaxis)]), ylim=c(0, 0.55),
     main=expression(hat(E)[italic("t+1:t+20")]*"[ Pr("*italic(FE)<=bar(0)*")"[italic("i, t+1:t+20")]*" ]"),
     xlab=expression("Quarter "*italic("t")*" since the first observation"),ylab="")
polygon( c(xaxis, xaxis[length(xaxis):1]  ),  
         c(CIlow.1, CIhigh.1[length(xaxis):1]), border = NA, col = "gray75")
polygon( c(xaxis, xaxis[length(xaxis):1]  ),  
         c(CIlow, CIhigh[length(xaxis):1]), border = NA, col = "gray55")
lines(xaxis,xxx, cex=0.75, lwd=1  ,type="o" )
abline(h=1, lty="dashed"  )
box()
lines(xaxis[11:length(xaxis)], apply( FEbias_learn_0.11.lowball , 2, mean,na.rm=T)[11:92] , cex=0.5, lwd=1  ,type="l", lty="solid"  )
lines(xaxis, apply( FEbias_learn_0.1.lowball , 2, mean,na.rm=T)[1:92] , cex=0.5, lwd=1  ,type="l", lty="dashed"  )

legend("bottomleft", inset=0.02,
               c(expression(hat(E)[italic("t+1:t+20")]*"[ "*"Pr(FE"<=bar(0)*")"[italic("i, t+1:t+20")]*" ]"),
                 expression(hat(E)[italic("t+1:t+20")]*"[ "*"Pr(FE"<=bar(0)*")"[italic("i, 1:20")]*" ]"),
                 expression(hat(E)[italic("t+1:t+20")]*"[ "*"Pr(FE"<=bar(0)*")"[italic("i, 11:30")]*" ]"),
                 expression("95% C.I. of "*hat(E)[italic("t+1:t+20")]*"[ "*"Pr(FE"<=bar(0)*")"[italic("i, t+1:t+20")]*" ]"),
                 expression("95% no-rejection regieon of "*hat(E)[italic("t+1:t+20")]*"[ "*"Pr(FE"<=bar(0)*")"[italic("i, 1:20")]*" ]"),
                 expression(paste("H"[o]*": "*E[italic("t+1:t+20")]*"[ Pr(FE"<=bar(0)*")"[italic("i, t+1:t+20")]," - Pr(FE"<=bar(0)*")"[italic("i, 1:20")]*" ]=0"))),
           lty=c(1,2,1,1,1,0), lwd=c(1,1,1,7,7,0), bty = "n",
           col=c("black","black","black","gray55","gray75"), pch=c(1,NA,NA,NA,NA,NA), cex=0.7 )





# plot(2) - use simple firmwise lowball measure



# http://web.eecs.umich.edu/~fessler/papers/files/tr/stderr.pdf
gom.VARSD <- function(x){
  n = sum(!is.na(x))
  
  S2 = var(x,na.rm=T)  
  var.EST = S2
  var.SE = S2*sqrt(2/(n-1))  
  
  S = sd(x,na.rm=T)  
  Kn = sqrt((n-1)/2)*exp(  lgamma((n-1)/2)-lgamma(n/2)  )
  Vn = 2*((n-1)/2 -  exp(  2*lgamma(n/2)  - 2*lgamma((n-1)/2)  )  )
  sd.EST = Kn*S
  sd.SE = S*Kn*sqrt(Vn/(n-1))  
  
  #return(list(var.EST=var.EST  ,  var.SE=var.SE, sd.EST=sd.EST, sd.SE=sd.SE  ))
  return(sd.SE=sd.SE  )
}




ssd = 1.96
xxx = FE_quant0_learn_lmer_var0
x=  FEbias_learn_0.lowball
temp_scale = t(matrix(rep(1/apply( x , 2, sd,na.rm=T)*sqrt(xxx), dim(x)[1] ), ncol=dim(x)[1] ))

FEbias_learn_0.1.lowball = ((matrix(rep((x[,1])  ,93), ncol=93)) + x*0)
FEbias_learn_0.11.lowball = ((matrix(rep((x[,11])  ,93), ncol=93)) + x*0)

temp_Ref = FEbias_learn_0.1.lowball

FEbias_learn_0.11.lowball.mean = (1/apply( FEbias_learn_0.11.lowball , 2, var,na.rm=T)*apply( x , 2, var,na.rm=T)*xxx[1])
FEbias_learn_0.1.lowball.mean = (1/apply( FEbias_learn_0.1.lowball , 2, var,na.rm=T)*apply( x , 2, var,na.rm=T)*xxx[1])
 


nset=apply(!is.na(cbind(x )),2,sum)
CIlow = rep(NA,93)
CIhigh = rep(NA,93)
for (jj in seq(1,93)) {
  CIlow[jj] = qf((0.025), nset[jj]-1, nset[jj]-1)
  CIhigh[jj] = qf((0.975), nset[jj]-1, nset[jj]-1)
} 

nset=apply(!is.na(cbind( x )),2,sum)
CIlow.1 = rep(NA,93)
CIhigh.1 = rep(NA,93)
for (jj in seq(1,93)) {
  r = cor(x[,jj]  ,temp_Ref[,jj] , use="complete.obs")
  tL = qt((0.025), nset[jj]-2)  
  CIlow.1[jj] = 1/(1-2*tL*sqrt( (1-r^2)/(nset[jj]-2) ) )*xxx[1] 
  tH = qt((0.975), nset[jj]-2)
  CIhigh.1[jj] = 1/(1-2*tH*sqrt( (1-r^2)/(nset[jj]-2) ) )*xxx[1] 
} 


CIlow = (sqrt(xxx)-ssd*apply( x*temp_scale,2, gom.VARSD ))^2
CIhigh = (sqrt(xxx)+ssd*apply( x*temp_scale,2, gom.VARSD ))^2

#CIlow = (sqrt(xxx)-ssd*apply( x ,2, gom.VARSD ))^2
#CIhigh = (sqrt(xxx)+ssd*apply( x ,2, gom.VARSD ))^2

 
xaxis  = 1:93

T_temp = length(xxx)
xxx = xxx[1:(T_temp-2)]
CIlow =   CIlow[1:(T_temp-2)]
CIhigh =  CIhigh[1:(T_temp-2)]
xaxis  = xaxis[1:(T_temp-2)]
CIlow.1 =   CIlow.1[1:(T_temp-2)]
CIhigh.1 =  CIhigh.1[1:(T_temp-2)]

plot(NA,xlim=c(xaxis[1]-5, xaxis[length(xaxis)]), ylim=c(0, 0.03),  
     main=expression(hat("Var")[italic("t+1:t+20")]*"[ Pr("*italic(FE)<=bar(0)*")"[italic("i, t+1:t+20")]*" ]"),
     xlab=expression("Quarter "*italic("t")*" since the first observation"),ylab="")
polygon( c(xaxis, xaxis[length(xaxis):1]  ),  
         c(CIlow.1, CIhigh.1[length(xaxis):1]), border = NA, col = "gray75")
#polygon( c(xaxis, xaxis[length(xaxis):1]  ),  
#         c(CIlow, CIhigh[length(xaxis):1]), border = NA, col = "gray55")
lines(xaxis,xxx, cex=0.75, lwd=1  ,type="o" )
abline(h=xxx[1], lty=3)
box()


lines(xaxis[11:92], FEbias_learn_0.11.lowball.mean[11:92], cex=0.5, lwd=1  ,type="l", lty="solid"  )
lines(xaxis[1:92], FEbias_learn_0.1.lowball.mean[1:92]  , cex=0.5, lwd=1  ,type="l", lty="dashed"  )


legend("bottomleft", inset=0.02,
       c(expression(hat("Var")[italic("t+1:t+20")]*"[ "*"Pr(FE"<=bar(0)*")"[italic("i, t+1:t+20")]*" ]"),
         expression(paste(hat("Var")[italic("t+1:t+20")]*"[ "*"Pr(FE"<=bar(0)*")"[italic("i, t+1:t+20")]," ]  w.r.t. Pr(FE"<=bar(0)*")"[italic("i, 1:20")])),
         expression(paste(hat("Var")[italic("t+1:t+20")]*"[ "*"Pr(FE"<=bar(0)*")"[italic("i, t+1:t+20")]," ]  w.r.t. Pr(FE"<=bar(0)*")"[italic("i, 11:30")])),         
         expression("95% no-rejection regieon of "*hat("Var")[italic("t+1:t+20")]*"[ "*"Pr(FE"<=bar(0)*")"[italic("i, 1:20")]*" ]"),
         expression(paste("H"[o]*": "*Var[italic("t+1:t+20")]*"[ Pr(FE"<=bar(0)*")"[italic("i, t+1:t+20")]," ] = Var"[italic("t+1:t+20")]*"[Pr(FE"<=bar(0)*")"[italic("i, 1:20")]*" ]"))),
       lty=c(1,2,1,1,0), lwd=c(1,1,1,7,0), bty = "n",
       col=c("black","black","black","gray75"), pch=c(1,NA,NA,NA,NA), cex=0.6 )

# phi-phihat var reduction trhough firm dimension is flat, because phi fluctureates a lot.
# learning exists, yet implied autocorr(FE) is negligible.
# true SE should be smaller because of dependence of two samples
# Pitman's T-test, pitmam-morgan test. compare variance of two dependent groups.
## Morgan-Pitman test
#t=(F-1)/F*sqrt(n-2)/sqrt(1-r^2)/2 = (1-1/F)*sqrt(n-2)/sqrt(1-r^2)/2
#Comparing the Variances of Two Dependent Groups
#Author(s): Rand R. Wilcox
#Source: Journal of Educational Statistics, Vol. 15, No. 3 (Autumn, 1990), pp. 237-247
# MSE decreases, both bias and variance decrease
 

# plot(3) - use simple firmwise lowball measure



# http://web.eecs.umich.edu/~fessler/papers/files/tr/stderr.pdf
gom.VARSD <- function(x){
  n = sum(!is.na(x))
  
  S2 = var(x,na.rm=T)  
  var.EST = S2
  var.SE = S2*sqrt(2/(n-1))  
  
  S = sd(x,na.rm=T)  
  Kn = sqrt((n-1)/2)*exp(  lgamma((n-1)/2)-lgamma(n/2)  )
  Vn = 2*((n-1)/2 -  exp(  2*lgamma(n/2)  - 2*lgamma((n-1)/2)  )  )
  sd.EST = Kn*S
  sd.SE = S*Kn*sqrt(Vn/(n-1))  
  
  #return(list(var.EST=var.EST  ,  var.SE=var.SE, sd.EST=sd.EST, sd.SE=sd.SE  ))
  return(sd.SE=sd.SE  )
}




ssd = 1.96
xxx = FE_quant0_learn_lmer_var0
x=  FEbias_learn_0.lowball
temp_scale = t(matrix(rep(1/apply( x , 2, sd,na.rm=T)*sqrt(xxx), dim(x)[1] ), ncol=dim(x)[1] ))

FEbias_learn_0.1.lowball = ((matrix(rep((x[,1])  ,93), ncol=93)) + x*0)
FEbias_learn_0.11.lowball = ((matrix(rep((x[,11])  ,93), ncol=93)) + x*0)

temp_Ref = FEbias_learn_0.1.lowball

FEbias_learn_0.11.lowball.mean = (apply( FEbias_learn_0.11.lowball , 2, var,na.rm=T)/apply( x , 2, var,na.rm=T)*xxx)[1:91]
FEbias_learn_0.1.lowball.mean = (apply( FEbias_learn_0.1.lowball , 2, var,na.rm=T)/apply( x , 2, var,na.rm=T)*xxx)[1:91]

nset=apply(!is.na(cbind(x )),2,sum)
CIlow = rep(NA,93)
CIhigh = rep(NA,93)
for (jj in seq(1,93)) {
  CIlow[jj] = qf((0.025), nset[jj]-1, nset[jj]-1)
  CIhigh[jj] = qf((0.975), nset[jj]-1, nset[jj]-1)
} 

nset=apply(!is.na(cbind( x )),2,sum)
CIlow.1 = rep(NA,93)
CIhigh.1 = rep(NA,93)
for (jj in seq(1,93)) {
  r = cor(x[,jj]  ,temp_Ref[,jj] , use="complete.obs")
  tL = qt((0.025), nset[jj]-2)  
  CIlow.1[jj] = 1/(1-2*tL*sqrt( (1-r^2)/(nset[jj]-2) ) )*xxx[jj] 
  tH = qt((0.975), nset[jj]-2)
  CIhigh.1[jj] = 1/(1-2*tH*sqrt( (1-r^2)/(nset[jj]-2) ) )*xxx[jj] 
} 


CIlow = (sqrt(xxx)-ssd*apply( x*temp_scale,2, gom.VARSD ))^2
CIhigh = (sqrt(xxx)+ssd*apply( x*temp_scale,2, gom.VARSD ))^2

#CIlow = (sqrt(xxx)-ssd*apply( x ,2, gom.VARSD ))^2
#CIhigh = (sqrt(xxx)+ssd*apply( x ,2, gom.VARSD ))^2


xaxis  = 1:93

T_temp = length(xxx)
xxx = xxx[1:(T_temp-2)]
CIlow =   CIlow[1:(T_temp-2)]
CIhigh =  CIhigh[1:(T_temp-2)]
xaxis  = xaxis[1:(T_temp-2)]
CIlow.1 =   CIlow.1[1:(T_temp-2)]
CIhigh.1 =  CIhigh.1[1:(T_temp-2)]

plot(NA,xlim=c(xaxis[1], xaxis[length(xaxis)]), ylim=c(0, 0.025),  
     main=expression(hat("Var")[italic("t+1:t+20")]*"[ Pr("*italic(FE)<=bar(0)*")"[italic("i, t+1:t+20")]*" ]"),
     xlab=expression("Quarter "*italic("t")*" since the first observation"),ylab="")
polygon( c(xaxis, xaxis[length(xaxis):1]  ),  
         c(CIlow.1, CIhigh.1[length(xaxis):1]), border = NA, col = "gray75")
polygon( c(xaxis, xaxis[length(xaxis):1]  ),  
         c(CIlow, CIhigh[length(xaxis):1]), border = NA, col = "gray55")
lines(xaxis,xxx, cex=0.5, lwd=1  ,type="o" )
abline(h=1, lty="dashed"  )
box()


lines(xaxis[11:92], FEbias_learn_0.11.lowball.mean[11:92], cex=0.5, lwd=1  ,type="l", lty="solid"  )
lines(xaxis, FEbias_learn_0.1.lowball.mean  , cex=0.5, lwd=1  ,type="l", lty="dashed"  )


legend("bottomleft", inset=0.02,
       c(expression(hat("Var")[italic("t+1:t+20")]*"[ "*"Pr(FE"<=bar(0)*")"[italic("i, t+1:t+20")]*" ]"),
         expression(hat("Var")[italic("t+1:t+20")]*"[ "*"Pr(FE"<=bar(0)*")"[italic("i, 1:20")]*" ]"),
         expression(hat("Var")[italic("t+1:t+20")]*"[ "*"Pr(FE"<=bar(0)*")"[italic("i, 11:30")]*" ]"),
         expression("95% C.I. of "*hat("Var")[italic("t+1:t+20")]*"[ "*"Pr(FE"<=bar(0)*")"[italic("i, t+1:t+20")]*" ]"),
         expression("95% no-rejection regieon of "*hat("Var")[italic("t+1:t+20")]*"[ "*"Pr(FE"<=bar(0)*")"[italic("i, 1:20")]*" ]"),
         expression(paste("H"[o]*": "*Var[italic("t+1:t+20")]*"[ Pr(FE"<=bar(0)*")"[italic("i, t+1:t+20")]," ] = Var"[italic("t+1:t+20")]*"[Pr(FE"<=bar(0)*")"[italic("i, 1:20")]*" ]"))),
       lty=c(1,2,1,1,1,0), lwd=c(1,1,1,7,7,0), bty = "n",
       col=c("black","black","black","gray55","gray75"), pch=c(1,NA,NA,NA,NA,NA), cex=0.6 )

# phi-phihat var reduction trhough firm dimension is flat, because phi fluctureates a lot.
# learning exists, yet implied autocorr(FE) is negligible.
# true SE should be smaller because of dependence of two samples
# Pitman's T-test, pitmam-morgan test. compare variance of two dependent groups.
## Morgan-Pitman test
#t=(F-1)/F*sqrt(n-2)/sqrt(1-r^2)/2 = (1-1/F)*sqrt(n-2)/sqrt(1-r^2)/2
#Comparing the Variances of Two Dependent Groups
#Author(s): Rand R. Wilcox
#Source: Journal of Educational Statistics, Vol. 15, No. 3 (Autumn, 1990), pp. 237-247
# MSE decreases, both bias and variance decrease

# plot(3) - use random effect mode est coefficient for reference and SE



# http://web.eecs.umich.edu/~fessler/papers/files/tr/stderr.pdf
gom.VARSD <- function(x){
  n = sum(!is.na(x))
  
  S2 = var(x,na.rm=T)  
  var.EST = S2
  var.SE = S2*sqrt(2/(n-1))  
  
  S = sd(x,na.rm=T)  
  Kn = sqrt((n-1)/2)*exp(  lgamma((n-1)/2)-lgamma(n/2)  )
  Vn = 2*((n-1)/2 -  exp(  2*lgamma(n/2)  - 2*lgamma((n-1)/2)  )  )
  sd.EST = Kn*S
  sd.SE = S*Kn*sqrt(Vn/(n-1))  
  
  #return(list(var.EST=var.EST  ,  var.SE=var.SE, sd.EST=sd.EST, sd.SE=sd.SE  ))
  return(sd.SE=sd.SE  )
}




ssd = 1.96
x=FE_quant0_learn_lmer0

FEbias_learn_0.1.lowball = ((matrix(rep((x[,1])  ,93), ncol=93)) + x*0)
FEbias_learn_0.11.lowball = ((matrix(rep((x[,11])  ,93), ncol=93)) + x*0)

temp_Ref = FEbias_learn_0.1.lowball
xxx = FE_quant0_learn_lmer_var0

FEbias_learn_0.11.lowball.mean = (apply( FEbias_learn_0.11.lowball , 2, var,na.rm=T)/apply( x , 2, var,na.rm=T)*xxx)[1:91]
FEbias_learn_0.1.lowball.mean = (apply( FEbias_learn_0.1.lowball , 2, var,na.rm=T)/apply( x , 2, var,na.rm=T)*xxx)[1:91]

temp_scale = t(matrix(rep(1/apply( x , 2, sd,na.rm=T)*sqrt(xxx), dim(x)[1] ), ncol=dim(x)[1] ))


nset=apply(!is.na(cbind(x )),2,sum)
CIlow = rep(NA,93)
CIhigh = rep(NA,93)
for (jj in seq(1,93)) {
  CIlow[jj] = qf((0.025), nset[jj]-1, nset[jj]-1)
  CIhigh[jj] = qf((0.975), nset[jj]-1, nset[jj]-1)
} 

nset=apply(!is.na(cbind( x )),2,sum)
CIlow.1 = rep(NA,93)
CIhigh.1 = rep(NA,93)
for (jj in seq(1,93)) {
  r = cor(x[,jj]  ,temp_Ref[,jj] , use="complete.obs")
  tL = qt((0.025), nset[jj]-2)  
  CIlow.1[jj] = 1/(1-2*tL*sqrt( (1-r^2)/(nset[jj]-2) ) )*xxx[jj] 
  tH = qt((0.975), nset[jj]-2)
  CIhigh.1[jj] = 1/(1-2*tH*sqrt( (1-r^2)/(nset[jj]-2) ) )*xxx[jj] 
} 


CIlow = (sqrt(xxx)-ssd*apply( x*temp_scale,2, gom.VARSD ))^2
CIhigh = (sqrt(xxx)+ssd*apply( x*temp_scale,2, gom.VARSD ))^2

#CIlow = (sqrt(xxx)-ssd*apply( x ,2, gom.VARSD ))^2
#CIhigh = (sqrt(xxx)+ssd*apply( x ,2, gom.VARSD ))^2


xaxis  = 1:93

T_temp = length(xxx)
xxx = xxx[1:(T_temp-2)]
CIlow =   CIlow[1:(T_temp-2)]
CIhigh =  CIhigh[1:(T_temp-2)]
xaxis  = xaxis[1:(T_temp-2)]
CIlow.1 =   CIlow.1[1:(T_temp-2)]
CIhigh.1 =  CIhigh.1[1:(T_temp-2)]

plot(NA,xlim=c(xaxis[1], xaxis[length(xaxis)]), ylim=c(0, 0.025),  
     main=expression(hat("Var")[italic("t+1:t+20")]*"[ Pr("*italic(FE)<=bar(0)*")"[italic("i, t+1:t+20")]*" ]"),
     xlab=expression("Quarter "*italic("t")*" since the first observation"),ylab="")
polygon( c(xaxis, xaxis[length(xaxis):1]  ),  
         c(CIlow.1, CIhigh.1[length(xaxis):1]), border = NA, col = "gray75")
polygon( c(xaxis, xaxis[length(xaxis):1]  ),  
         c(CIlow, CIhigh[length(xaxis):1]), border = NA, col = "gray55")
lines(xaxis,xxx, cex=0.5, lwd=1  ,type="o" )
abline(h=1, lty="dashed"  )
box()


lines(xaxis[11:92], FEbias_learn_0.11.lowball.mean[11:92], cex=0.5, lwd=1  ,type="l", lty="solid"  )
lines(xaxis, FEbias_learn_0.1.lowball.mean  , cex=0.5, lwd=1  ,type="l", lty="dashed"  )


legend("bottomleft", inset=0.02,
       c(expression(hat("Var")[italic("t+1:t+20")]*"[ "*"Pr(FE"<=bar(0)*")"[italic("i, t+1:t+20")]*" ]"),
         expression(hat("Var")[italic("t+1:t+20")]*"[ "*"Pr(FE"<=bar(0)*")"[italic("i, 1:20")]*" ]"),
         expression(hat("Var")[italic("t+1:t+20")]*"[ "*"Pr(FE"<=bar(0)*")"[italic("i, 11:30")]*" ]"),
         expression("95% C.I. of "*hat("Var")[italic("t+1:t+20")]*"[ "*"Pr(FE"<=bar(0)*")"[italic("i, t+1:t+20")]*" ]"),
         expression("95% no-rejection regieon of "*hat("Var")[italic("t+1:t+20")]*"[ "*"Pr(FE"<=bar(0)*")"[italic("i, 1:20")]*" ]"),
         expression(paste("H"[o]*": "*Var[italic("t+1:t+20")]*"[ Pr(FE"<=bar(0)*")"[italic("i, t+1:t+20")]," ] = Var"[italic("t+1:t+20")]*"[Pr(FE"<=bar(0)*")"[italic("i, 1:20")]*" ]"))),
       lty=c(1,2,1,1,1,0), lwd=c(1,1,1,7,7,0), bty = "n",
       col=c("black","black","black","gray55","gray75"), pch=c(1,NA,NA,NA,NA,NA), cex=0.6 )

###################### learning_mean.eps ################

########################### end of plot in the paper ##########################
########################### end of plot in the paper ##########################
########################### end of plot in the paper ##########################




########################### plot in the paper ##########################
########################### plot in the paper ##########################
########################### plot in the paper ##########################




load("phiphihat_learn_0.alt.Rdata")
load("phiphihat_learn_0.Rdata")

par(mfrow=c(1,2))


# phi-phihat learning: plot (1)
ssd = 1.96



x =   (phiphihat_learn_0.alt)
xxx =  (apply( x     , 2, mean,na.rm=T))  
x_SE =    apply( x,2, sd,na.rm=T)/ sqrt( apply( !is.na(x ),2, sum) )


phiphihat_learn_0.1 = (matrix(rep((x[,1])  ,93), ncol=93)) + x*0
phiphihat_learn_0.11 = (matrix(rep((x[,11])  ,93), ncol=93)) + x*0
xxx.1 =  (apply( phiphihat_learn_0.1     , 2, mean,na.rm=T)) 
x_SE.1 =    apply( x-phiphihat_learn_0.1 ,2, sd,na.rm=T)/ sqrt( apply( !is.na(phiphihat_learn_0.1 ),2, sum) )
CIlow.1 = xxx -x_SE.1*ssd
CIhigh.1 = xxx +x_SE.1*ssd

nset=apply(!is.na(cbind( x )),2,sum)
CIlow = xxx -x_SE*ssd
CIhigh = xxx +x_SE*ssd

xaxis  = 1:93

T_temp = length(xxx)
xxx = xxx[1:(T_temp-2)]
CIlow =   CIlow[1:(T_temp-2)]
CIhigh =  CIhigh[1:(T_temp-2)]
xaxis  = xaxis[1:(T_temp-2)]
CIlow.1 =   CIlow.1[1:(T_temp-2)]
CIhigh.1 =  CIhigh.1[1:(T_temp-2)]

## plot in the paper
plot(NA,xlim=c(xaxis[1]-5, xaxis[length(xaxis)]), ylim=c(-0.04, 0.035),
     main=expression(hat(E)[italic("t+1:t+20")]*"[ ("*phi-hat(phi)*")"[italic("i, t+1:t+20")]*" ]"),
     xlab=expression("Quarter "*italic("t")*" since the first observation"),ylab="")
polygon( c(xaxis, xaxis[length(xaxis):1]  ),  
         c(CIlow.1, CIhigh.1[length(xaxis):1]), border = NA, col = "gray75")
polygon( c(xaxis, xaxis[length(xaxis):1]  ),  
         c(CIlow, CIhigh[length(xaxis):1]), border = NA, col = "gray55")
lines(xaxis,xxx, cex=0.75, lwd=1  ,type="o" )
abline(h=0, lty=3)
box()
lines(xaxis[11:91], apply( phiphihat_learn_0.11 , 2, mean,na.rm=T)[11:91] , cex=0.5, lwd=1  ,type="l", lty="solid"  )
lines(xaxis[1:91], apply( phiphihat_learn_0.1 , 2, mean,na.rm=T)[1:91] , cex=0.5, lwd=1  ,type="l", lty="dashed"  )

legend("bottomleft", inset=0.02,
       c(expression(hat(E)[italic("t+1:t+20")]*"[ ("*phi-hat(phi)*")"[italic("i, t+1:t+20")]*" ]"),
         expression(hat(E)[italic("t+1:t+20")]*"[ ("*phi-hat(phi)*")"[italic("i, 1:20")]*" ]"),
         expression(hat(E)[italic("t+1:t+20")]*"[ ("*phi-hat(phi)*")"[italic("i, 11:30")]*" ]"),
         expression("95% C.I. of "*hat(E)[italic("t+1:t+20")]*"[ ("*phi-hat(phi)*")"[italic("i, t+1:t+20")]*" ]"),
         expression("95% no-rejection regieon of "*hat(E)[italic("t+1:t+20")]*"[ ("*phi-hat(phi)*")"[italic("i, 1:20")]*" ]"),
         expression(paste("H"[o]*": "*E[italic("t+1:t+20")]*"[ ("*phi-hat(phi)*")"[italic("i, t+1:t+20")]," - ("*phi-hat(phi)*")"[italic("i, 1:20")]*" ]=0"))),
       lty=c(1,2,1,1,1,0), lwd=c(1,1,1,7,7,0), bty = "n",
       col=c("black","black","black","gray55","gray75"), pch=c(1,NA,NA,NA,NA,NA), cex=0.6  )


# plot(2) - use fixed effect coefficients.
 
 
 
 


load("phiphihat_learn_0.alt.Rdata")
load("phiphihat_learn_lmer_var0.Rdata")


# http://web.eecs.umich.edu/~fessler/papers/files/tr/stderr.pdf
gom.VARSD <- function(x){
  n = sum(!is.na(x))
  
  S2 = var(x,na.rm=T)  
  var.EST = S2
  var.SE = S2*sqrt(2/(n-1))  
  
  S = sd(x,na.rm=T)  
  Kn = sqrt((n-1)/2)*exp(  lgamma((n-1)/2)-lgamma(n/2)  )
  Vn = 2*((n-1)/2 -  exp(  2*lgamma(n/2)  - 2*lgamma((n-1)/2)  )  )
  sd.EST = Kn*S
  sd.SE = S*Kn*sqrt(Vn/(n-1))  
  
  #return(list(var.EST=var.EST  ,  var.SE=var.SE, sd.EST=sd.EST, sd.SE=sd.SE  ))
  return(sd.SE=sd.SE  )
}




ssd = 1.96
x= phiphihat_learn_0.alt
xxx = phiphihat_learn_lmer_var0
temp_scale = t(matrix(rep(1/apply( x , 2, sd,na.rm=T)*sqrt(xxx), dim(x)[1] ), ncol=dim(x)[1] ))


phiphihat_learn_0.1 = ((matrix(rep((x[,1])  ,93), ncol=93)) + x*0)
phiphihat_learn_0.11  = ((matrix(rep((x[,11])  ,93), ncol=93)) + x*0)

temp_Ref = phiphihat_learn_0.1 
temp_Ref2 = phiphihat_learn_0.11 

phiphihat_learn_0.11.mean =(1/apply( phiphihat_learn_0.11 , 2, var,na.rm=T)*apply( x , 2, var,na.rm=T)*xxx[1])
phiphihat_learn_0.1.mean = (1/apply( phiphihat_learn_0.1  , 2, var,na.rm=T)*apply( x , 2, var,na.rm=T)*xxx[1])


 



nset=apply(!is.na(cbind(x )),2,sum)
CIlow = rep(NA,93)
CIhigh = rep(NA,93)
for (jj in seq(1,93)) {
  CIlow[jj] = qf((0.025), nset[jj]-1, nset[jj]-1)
  CIhigh[jj] = qf((0.975), nset[jj]-1, nset[jj]-1)
} 

nset=apply(!is.na(cbind( x )),2,sum)
CIlow.1 = rep(NA,93)
CIhigh.1 = rep(NA,93)
for (jj in seq(1,93)) {
  r = cor(x[,jj]  ,temp_Ref[,jj] , use="complete.obs")
  tL = qt((0.025), nset[jj]-2)  
  CIlow.1[jj] = 1/(1-2*tL*sqrt( (1-r^2)/(nset[jj]-2) ) )*xxx[1]   
  tH = qt((0.975), nset[jj]-2)
  CIhigh.1[jj] = 1/(1-2*tH*sqrt( (1-r^2)/(nset[jj]-2) ) )*xxx[1] 
} 

nset=apply(!is.na(cbind( x )),2,sum)
CIlow = rep(NA,93)
CIhigh = rep(NA,93)
for (jj in seq(1,93)) {
  r = cor(x[,jj]  ,temp_Ref2[,jj] , use="complete.obs")
  tL = qt((0.025), nset[jj]-2)  
  CIlow[jj] = 1/(1-2*tL*sqrt( (1-r^2)/(nset[jj]-2) ) )*xxx[11]   
  tH = qt((0.975), nset[jj]-2)
  CIhigh[jj] = 1/(1-2*tH*sqrt( (1-r^2)/(nset[jj]-2) ) )*xxx[11] 
} 



xaxis  = 1:93

T_temp = length(xxx)
xxx = xxx[1:(T_temp-2)]
CIlow =   CIlow[1:(T_temp-2)]
CIhigh =  CIhigh[1:(T_temp-2)]
xaxis  = xaxis[1:(T_temp-2)]
CIlow.1 =   CIlow.1[1:(T_temp-2)]
CIhigh.1 =  CIhigh.1[1:(T_temp-2)]

plot(NA,xlim=c(xaxis[1]-5, xaxis[length(xaxis)]), ylim=c(0, 0.016),  
     main=expression(hat("Var")[italic("t+1:t+20")]*"[ ("*phi-hat(phi)*")"[italic("i, t+1:t+20")]*" ]"),
     xlab=expression("Quarter "*italic("t")*" since the first observation"),ylab="")
polygon( c(xaxis, xaxis[length(xaxis):1]  ),  
         c(CIlow.1, CIhigh.1[length(xaxis):1]), border = NA, col = "gray75")
#polygon( c(xaxis, xaxis[length(xaxis):1]  ),  
#         c(CIlow, CIhigh[length(xaxis):1]), border = NA, col = "gray55")
lines(xaxis,xxx, cex=0.75, lwd=1  ,type="o" )
abline(h=xxx[1], lty=3 )
box()


lines(xaxis[11:91], phiphihat_learn_0.11.mean[11:91], cex=0.5, lwd=1  ,type="l", lty="solid"  )
lines(xaxis[1:91], phiphihat_learn_0.1.mean[1:91]  , cex=0.5, lwd=1  ,type="l", lty="dashed"  )

legend("bottomleft", inset=0.02,
       c(expression(hat("Var")[italic("t+1:t+20")]*"[ ("*phi-hat(phi)*")"[italic("i, t+1:t+20")]*" ] as a random effect"),
         expression(hat("Var")[italic("t+1:t+20")]*"[ ("*phi-hat(phi)*")"[italic("i, t+1:t+20")]*" ]  w.r.t. ("*phi-hat(phi)*")"[italic("i, 1:20")]),
         expression(hat("Var")[italic("t+1:t+20")]*"[ ("*phi-hat(phi)*")"[italic("i, t+1:t+20")]*" ]  w.r.t. ("*phi-hat(phi)*")"[italic("i, 11:30")]),         
         expression("95% no-rejection regieon of "*hat("Var")[italic("t+1:t+20")]*"[ ("*phi-hat(phi)*")"[italic("i, 1:20")]*" ]"),
         expression(paste("H"[o]*": "*"Var"[italic("t+1:t+20")]*"[ ("*phi-hat(phi)*")"[italic("i, t+1:t+20")],"] = Var"[italic("t+1:t+20")]*"[ ("*phi-hat(phi)*")"[italic("i, 1:20")]*" ]"))),
       lty=c(1,2,1,1,0), lwd=c(1,1,1,7,0), bty = "n",
       col=c("black","black","black","gray75"), pch=c(1,NA,NA,NA,NA), cex=0.6 )








# plot(3) - use fixed effect coefficients.


load("phiphihat_learn_0.alt.Rdata")
load("phiphihat_learn_lmer_var0.Rdata")


# http://web.eecs.umich.edu/~fessler/papers/files/tr/stderr.pdf
gom.VARSD <- function(x){
  n = sum(!is.na(x))
  
  S2 = var(x,na.rm=T)  
  var.EST = S2
  var.SE = S2*sqrt(2/(n-1))  
  
  S = sd(x,na.rm=T)  
  Kn = sqrt((n-1)/2)*exp(  lgamma((n-1)/2)-lgamma(n/2)  )
  Vn = 2*((n-1)/2 -  exp(  2*lgamma(n/2)  - 2*lgamma((n-1)/2)  )  )
  sd.EST = Kn*S
  sd.SE = S*Kn*sqrt(Vn/(n-1))  
  
  #return(list(var.EST=var.EST  ,  var.SE=var.SE, sd.EST=sd.EST, sd.SE=sd.SE  ))
  return(sd.SE=sd.SE  )
}




ssd = 1.96
x= phiphihat_learn_0.alt
xxx = phiphihat_learn_lmer_var0
temp_scale = t(matrix(rep(1/apply( x , 2, sd,na.rm=T)*sqrt(xxx), dim(x)[1] ), ncol=dim(x)[1] ))


phiphihat_learn_0.1 = ((matrix(rep((x[,1])  ,93), ncol=93)) + x*0)
phiphihat_learn_0.11  = ((matrix(rep((x[,11])  ,93), ncol=93)) + x*0)

temp_Ref = phiphihat_learn_0.1 

phiphihat_learn_0.11.mean = (apply( phiphihat_learn_0.11 , 2, var,na.rm=T)/apply( x , 2, var,na.rm=T)*xxx)[1:91]
phiphihat_learn_0.1.mean = (apply( phiphihat_learn_0.1  , 2, var,na.rm=T)/apply( x , 2, var,na.rm=T)*xxx)[1:91]






nset=apply(!is.na(cbind(x )),2,sum)
CIlow = rep(NA,93)
CIhigh = rep(NA,93)
for (jj in seq(1,93)) {
  CIlow[jj] = qf((0.025), nset[jj]-1, nset[jj]-1)
  CIhigh[jj] = qf((0.975), nset[jj]-1, nset[jj]-1)
} 

nset=apply(!is.na(cbind( x )),2,sum)
CIlow.1 = rep(NA,93)
CIhigh.1 = rep(NA,93)
for (jj in seq(1,93)) {
  r = cor(x[,jj]  ,temp_Ref[,jj] , use="complete.obs")
  tL = qt((0.025), nset[jj]-2)  
  CIlow.1[jj] = 1/(1-2*tL*sqrt( (1-r^2)/(nset[jj]-2) ) )*xxx[jj] 
  tH = qt((0.975), nset[jj]-2)
  CIhigh.1[jj] = 1/(1-2*tH*sqrt( (1-r^2)/(nset[jj]-2) ) )*xxx[jj] 
} 


CIlow = (sqrt(xxx)-ssd*apply( x*temp_scale,2, gom.VARSD ))^2
CIhigh = (sqrt(xxx)+ssd*apply( x*temp_scale,2, gom.VARSD ))^2

#CIlow = (sqrt(xxx)-ssd*apply( x ,2, gom.VARSD ))^2
#CIhigh = (sqrt(xxx)+ssd*apply( x ,2, gom.VARSD ))^2


xaxis  = 1:93

T_temp = length(xxx)
xxx = xxx[1:(T_temp-4)]
CIlow =   CIlow[1:(T_temp-4)]
CIhigh =  CIhigh[1:(T_temp-4)]
xaxis  = xaxis[1:(T_temp-4)]
CIlow.1 =   CIlow.1[1:(T_temp-4)]
CIhigh.1 =  CIhigh.1[1:(T_temp-4)]

plot(NA,xlim=c(xaxis[1], xaxis[length(xaxis)]), ylim=c(0, 0.016),  
     main=expression(hat("Var")[italic("t+1:t+20")]*"[ ("*phi-hat(phi)*")"[italic("i, t+1:t+20")]*" ]"),
     xlab=expression("Quarter "*italic("t")*" since the first observation"),ylab="")
polygon( c(xaxis, xaxis[length(xaxis):1]  ),  
         c(CIlow.1, CIhigh.1[length(xaxis):1]), border = NA, col = "gray75")
polygon( c(xaxis, xaxis[length(xaxis):1]  ),  
         c(CIlow, CIhigh[length(xaxis):1]), border = NA, col = "gray55")
lines(xaxis,xxx, cex=0.5, lwd=1  ,type="o" )
#points(xaxis,xxx, cex=1.5, lwd=1    )
abline(h=1, lty="dashed"  )
box()


lines(xaxis[11:89], phiphihat_learn_0.11.mean[11:89], cex=0.5, lwd=1  ,type="l", lty="solid"  )
lines(xaxis[1:89], phiphihat_learn_0.1.mean[1:89]  , cex=0.5, lwd=1  ,type="l", lty="dashed"  )

legend("bottomleft", inset=0.02,
       c(expression(hat("Var")[italic("t+1:t+20")]*"[ ("*phi-hat(phi)*")"[italic("i, t+1:t+20")]*" ]"),
         expression(hat("Var")[italic("t+1:t+20")]*"[ ("*phi-hat(phi)*")"[italic("i, 1:20")]*" ]"),
         expression(hat("Var")[italic("t+1:t+20")]*"[ ("*phi-hat(phi)*")"[italic("i, 11:30")]*" ]"),
         expression("95% C.I. of "*hat("Var")[italic("t+1:t+20")]*"[ ("*phi-hat(phi)*")"[italic("i, t+1:t+20")]*" ]"),
         expression("95% no-rejection regieon of "*hat("Var")[italic("t+1:t+20")]*"[ ("*phi-hat(phi)*")"[italic("i, 1:20")]*" ]"),
         expression(paste("H"[o]*": "*"Var"[italic("t+1:t+20")]*"[ ("*phi-hat(phi)*")"[italic("i, t+1:t+20")],"] = Var[ ("*phi-hat(phi)*")"[italic("i, 1:20")]*" ]"))),
       lty=c(1,2,1,1,1,0), lwd=c(1,1,1,7,7,0), bty = "n",
       col=c("black","black","black","gray55","gray75"), pch=c(1,NA,NA,NA,NA,NA), cex=0.7 )


# phi-phihat var reduction trhough firm dimension is flat, because phi fluctureates a lot.
# learning exists, yet implied autocorr(FE) is negligible.
# true SE should be smaller because of dependence of two samples
# Pitman's T-test, pitmam-morgan test. compare variance of two dependent groups.
## Morgan-Pitman test
#t=(F-1)/F*sqrt(n-2)/sqrt(1-r^2)/2 = (1-1/F)*sqrt(n-2)/sqrt(1-r^2)/2
#Comparing the Variances of Two Dependent Groups
#Author(s): Rand R. Wilcox
#Source: Journal of Educational Statistics, Vol. 15, No. 3 (Autumn, 1990), pp. 237-247
# MSE decreases, both bias and variance decrease


# plot(3) -use random effect coeffeicients


load("phiphihat_learn_lmer0.Rdata") 
load("phiphihat_learn_lmer_var0.Rdata")
load("phiphihat_learn_lmer_se0.Rdata")

# http://web.eecs.umich.edu/~fessler/papers/files/tr/stderr.pdf
gom.VARSD <- function(x){
  n = sum(!is.na(x))
  
  S2 = var(x,na.rm=T)  
  var.EST = S2
  var.SE = S2*sqrt(2/(n-1))  
  
  S = sd(x,na.rm=T)  
  Kn = sqrt((n-1)/2)*exp(  lgamma((n-1)/2)-lgamma(n/2)  )
  Vn = 2*((n-1)/2 -  exp(  2*lgamma(n/2)  - 2*lgamma((n-1)/2)  )  )
  sd.EST = Kn*S
  sd.SE = S*Kn*sqrt(Vn/(n-1))  
  
  #return(list(var.EST=var.EST  ,  var.SE=var.SE, sd.EST=sd.EST, sd.SE=sd.SE  ))
  return(sd.SE=sd.SE  )
}




ssd = 1.96
x=phiphihat_learn_lmer0

xxx = phiphihat_learn_lmer_var0

temp_scale = t(matrix(rep(1/apply( x , 2, sd,na.rm=T)*sqrt(xxx), dim(x)[1] ), ncol=dim(x)[1] ))

x = apply(x, 2, mean, na.rm=T ) +  (x-apply(x, 2, mean, na.rm=T ) )*temp_scale
phiphihat_learn_0.1 = ((matrix(rep((x[,1])  ,93), ncol=93)) + x*0)
phiphihat_learn_0.11  = ((matrix(rep((x[,11])  ,93), ncol=93)) + x*0)

temp_Ref = phiphihat_learn_0.1 


phiphihat_learn_0.11.mean = (apply( phiphihat_learn_0.11 , 2, var,na.rm=T)/apply( x , 2, var,na.rm=T)*xxx)[1:91]
phiphihat_learn_0.1.mean = (apply( phiphihat_learn_0.1  , 2, var,na.rm=T)/apply( x , 2, var,na.rm=T)*xxx)[1:91]






nset=apply(!is.na(cbind(x )),2,sum)
CIlow = rep(NA,93)
CIhigh = rep(NA,93)
for (jj in seq(1,93)) {
  CIlow[jj] = qf((0.025), nset[jj]-1, nset[jj]-1)
  CIhigh[jj] = qf((0.975), nset[jj]-1, nset[jj]-1)
} 

nset=apply(!is.na(cbind( x )),2,sum)
CIlow.1 = rep(NA,93)
CIhigh.1 = rep(NA,93)
for (jj in seq(1,93)) {
  r = cor(x[,jj]  ,temp_Ref[,jj] , use="complete.obs")
  tL = qt((0.025), nset[jj]-2)  
  CIlow.1[jj] = 1/(1-2*tL*sqrt( (1-r^2)/(nset[jj]-2) ) )*xxx[jj] 
  tH = qt((0.975), nset[jj]-2)
  CIhigh.1[jj] = 1/(1-2*tH*sqrt( (1-r^2)/(nset[jj]-2) ) )*xxx[jj] 
} 


#CIlow = (sqrt(xxx)-ssd*apply( x*temp_scale,2, gom.VARSD ))^2
#CIhigh = (sqrt(xxx)+ssd*apply( x*temp_scale,2, gom.VARSD ))^2

CIlow = (sqrt(xxx)-ssd*apply( x ,2, gom.VARSD ))^2
CIhigh = (sqrt(xxx)+ssd*apply( x ,2, gom.VARSD ))^2


xaxis  = 1:93

T_temp = length(xxx)
xxx = xxx[1:(T_temp-2)]
CIlow =   CIlow[1:(T_temp-2)]
CIhigh =  CIhigh[1:(T_temp-2)]
xaxis  = xaxis[1:(T_temp-2)]
CIlow.1 =   CIlow.1[1:(T_temp-2)]
CIhigh.1 =  CIhigh.1[1:(T_temp-2)]

plot(NA,xlim=c(xaxis[1], xaxis[length(xaxis)]), ylim=c(0, 0.013),  
     main=expression(hat("Var")[italic("t+1:t+20")]*"[ Pr("*italic(FE)<=bar(0)*")"[italic("i, t+1:t+20")]*" ]"),
     xlab=expression("Quarter "*italic("t")*" since the first observation"),ylab="")
polygon( c(xaxis, xaxis[length(xaxis):1]  ),  
         c(CIlow.1, CIhigh.1[length(xaxis):1]), border = NA, col = "gray75")
polygon( c(xaxis, xaxis[length(xaxis):1]  ),  
         c(CIlow, CIhigh[length(xaxis):1]), border = NA, col = "gray55")
lines(xaxis,xxx, cex=0.5, lwd=1  ,type="l" )
points(xaxis,xxx, cex=0.5, lwd=1    )
abline(h=1, lty="dashed"  )
box()


lines(xaxis[11:91], phiphihat_learn_0.11.mean[11:91], cex=0.5, lwd=1  ,type="l", lty="solid"  )
lines(xaxis, phiphihat_learn_0.1.mean  , cex=0.5, lwd=1  ,type="l", lty="dashed"  )


legend("bottomleft", inset=0.02,
       c(expression(hat("Var")[italic("t+1:t+20")]*"[ "*"Pr(FE"<=bar(0)*")"[italic("i, t+1:t+20")]*" ]"),
         expression(hat("Var")[italic("t+1:t+20")]*"[ "*"Pr(FE"<=bar(0)*")"[italic("i, 1:20")]*" ]"),
         expression(hat("Var")[italic("t+1:t+20")]*"[ "*"Pr(FE"<=bar(0)*")"[italic("i, 11:30")]*" ]"),
         expression("95% C.I. of "*hat("Var")[italic("t+1:t+20")]*"[ "*"Pr(FE"<=bar(0)*")"[italic("i, t+1:t+20")]*" ]"),
         expression("95% no-rejection regieon of "*hat("Var")[italic("t+1:t+20")]*"[ "*"Pr(FE"<=bar(0)*")"[italic("i, 1:20")]*" ]"),
         expression(paste("H"[o]*": "*Var[italic("t+1:t+20")]*"[ Pr(FE"<=bar(0)*")"[italic("i, t+1:t+20")]," ] = Var"[italic("t+1:t+20")]*"[Pr(FE"<=bar(0)*")"[italic("i, 1:20")]*" ]"))),
       lty=c(1,2,1,1,1,0), lwd=c(1,1,1,7,7,0), bty = "n",
       col=c("black","black","black","gray55","gray75"), pch=c(1,NA,NA,NA,NA,NA), cex=0.6 )


########################## learning_phiphihat.eps ################












########################### end of plot in the paper ##########################
########################### end of plot in the paper ##########################
########################### end of plot in the paper ##########################


######## learning phi
### without age dummy

# Figure (1) phi  & phihat. They are time-varying state variables
Reg1 = lm(ye_temp_pooled~   -1+ year_dummy_pooled  + ye_temp_pooled_lag1+  year_dummy_pooled:ye_temp_pooled_lag1    )
Reg2 = lm(yf_temp_pooled~   -1 + year_dummy_pooled  + ye_temp_pooled_lag1+  year_dummy_pooled:ye_temp_pooled_lag1 +yf_temp_pooled_lag1+  year_dummy_pooled:yf_temp_pooled_lag1    )
# Reg1 + Reg2 = Reg3
Reg3 = lm(FE_temp_pooled~   -1 + year_dummy_pooled  + ye_temp_pooled_lag1+  year_dummy_pooled:ye_temp_pooled_lag1 +yf_temp_pooled_lag1+  year_dummy_pooled:yf_temp_pooled_lag1    )


phi_year_TS =  c(0,Reg1$coef[31:58])+Reg1$coef[30]
phihat_year_TS = c(c(0,Reg2$coef[32:59])+Reg2$coef[30]) + c(c(0,Reg2$coef[60:87])+Reg2$coef[31])
mean(phi_year_TS)
mean(phihat_year_TS)
 

###################### plot in the paper ################
###################### plot in the paper ################
###################### plot in the paper ################
par(mfrow=c(1,2))  
plot(1985:2013,phi_year_TS, 
     type="l",lwd=6, col="gray60",ylim=c(0.15,0.6),xlab="Year",
     ylab="",
     main=expression("Autoregressive coef. ("*phi*", "*hat(phi)*")"[t]))
lines(1985:2013,  phihat_year_TS , type="o")
legend("bottomright", inset = 0.02, c(expression(hat(phi)),expression(phi)), lty=c(1,1), 
       lwd=c(1,6),col=c("black","gray60"), pch=c(1,NA),bty = "n")




### phi-phihat
ssd = 1.96 
phi_year_TS =  c(0,Reg1$coef[31:58])+Reg1$coef[30]
phiphihat_year_TS = c(c(0,Reg3$coef[ (31:58)+1] )+Reg3$coef[29+1]) + c(c(0,Reg3$coef[(59:86)+1])+Reg3$coef[30+1])
#ts.plot(cbind(phi_year_TS-phihat_year_TS,phiphihat_year_TS) )

CIhigh = rep(NA,29)
CIlow =  rep(NA,29)
phiphihat_year_TS =  rep(NA,29)
betacov =  vcov(Reg3)[57:114-27,57:114-27]
betavec =  matrix(Reg3$coef[57:114-27],ncol=1)
for (ii in c(1:29)) {
  
  a1 = rep(0,28)
  a2 = rep(0,28)
  if (ii>1){
    a1[ii-1]=1
    a2[ii-1]=1
  }
  A = matrix(c(1, 1, a1,a2),nrow=1)
  SE=sqrt(A%*%betacov%*%t(A))
  CIhigh[ii] = A%*%betavec +ssd*SE
  CIlow[ii] = A%*%betavec-ssd*SE
  phiphihat_year_TS[ii] = A%*%betavec
}

#### phi learn plot ########### plot in the paper
xxx = phiphihat_year_TS
xaxis.year = 1985:2013

T_temp = length(xxx)
xxx = xxx[1:(T_temp-1)]
CIlow =   CIlow[1:(T_temp-1)]
CIhigh =  CIhigh[1:(T_temp-1)]
xaxis.year  = xaxis.year[1:(T_temp-1)]

plot(NA,xlim=c(xaxis.year[1], xaxis.year[length(xaxis.year)]) ,ylim=c(-0.1,0.1),
     xlab="Year", ylab="",main=expression("Autoregressive coef. ("*phi*"-"*hat(phi)*")"[t]))
polygon( c(xaxis.year, xaxis.year[length(xaxis.year):1]  ),  
         c(CIlow, CIhigh[length(xaxis.year):1]), border = NA, col = "gray")
lines(xaxis.year,xxx, cex=0.5, lwd=1  ,type="l" )
points(xaxis.year,xxx, cex=1, lwd=1    )
abline(h=0, lty="dashed")

legend("bottomright", inset = 0.02, c(expression("("*phi*"-"*hat(phi)*")"[t]),
                                      expression("95% C.I. of ("*phi-hat(phi)*")"[t])),
       lty=c(1,1), 
       lwd=c(1,6),col=c("black","gray60"), pch=c(1,NA),bty = "n")
 

###################### end of plot in the paper ################
###################### end of plot in the paper ################
###################### end of plot in the paper ################
###################### end of plot in the paper ################







############ learning mu within firm using lmer ##########
#1:20 - 21:40
#2:21 - 22:41
#3:22 - 23:42
require('corpcor')

FE_temp_quant0_adj = FE_temp
FE_temp_quant0_adj[FE_temp>0] = 0
FE_temp_quant0_adj[FE_temp<0] = 1
FE_temp_quant0_adj[FE_temp==0] = 1/2
FE_temp_quant0_adj = FE_temp


max_no_obs_deflated = max(est$no_obs_deflated)

if (do_all) {
  
  
  avg_corr_temp_shrink=rep(NA, max_no_obs_deflated  ) 
  avg_corr_temp_pearson=rep(NA, max_no_obs_deflated  ) 
  avg_corr_temp_spearman=rep(NA, max_no_obs_deflated  ) 
  
  for (ii in c( 20:max_no_obs_deflated   )) {       
  
    temp_T = sum( est$no_obs_deflated  >= ii, na.rm=T )    
    
    data_matrix_temp = matrix( rep(NA, temp_T*20  ), nrow=20  )
    #data_matrix_temp = matrix( rep(NA, temp_T*ii  ), nrow=ii  )
    
    
    kk=1
    for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
    {  
      if (est$no_obs_deflated[jj] >= ii ) {   
        
        firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]     
        nonNA_index_firm_temp = nonNA_index_firm[firm_idx_temp]     
        use_index_firm_temp = rep(TRUE, length(firm_idx_temp))
        use_index_firm_temp[nonNA_index_firm_temp==0 | nonNA_index_firm_temp>ii | nonNA_index_firm_temp<ii-19   ]=FALSE
        #use_index_firm_temp[nonNA_index_firm_temp==0 | nonNA_index_firm_temp>ii    ]=FALSE
              
        data_matrix_temp[,kk] = c( FE_temp_quant0_adj[firm_idx_temp] )[ use_index_firm_temp ]          
        kk=kk+1
      }      
      
    }
    kk=kk-1
    
    avg_corr_temp_shrink[ii] = ( sum(cor.shrink(data_matrix_temp)-diag(kk),na.rm=T)/( kk^2-kk)   )
    avg_corr_temp_pearson[ii] = ( sum(cor(data_matrix_temp, method="pearson")-diag(kk),na.rm=T)/( kk^2-kk)   )
    avg_corr_temp_spearman[ii] = ( sum(cor(data_matrix_temp, method= "spearman")-diag(kk),na.rm=T)/( kk^2-kk)   )
    
   # print(ii)
  }
  
  plot( 1:93,   avg_corr_temp[20:112], type="o")
  save(avg_corr_temp_shrink,file="avg_corr_temp_shrink_20.Rdata")
  save(avg_corr_temp_pearson,file="avg_corr_temp_pearson_20.Rdata")
  save(avg_corr_temp_spearman,file="avg_corr_temp_spearman_20.Rdata")
  
  #save(avg_corr_temp_shrink,file="avg_corr_temp_shrink.Rdata")
  #save(avg_corr_temp_pearson,file="avg_corr_temp_pearson.Rdata")
  #save(avg_corr_temp_spearman,file="avg_corr_temp_spearman.Rdata")
  
  
}

load("avg_corr_temp.Rdata")
plot( 1:93,   avg_corr_temp_shrink[20:112], type="o")
plot( 1:93,   avg_corr_temp_pearson[20:112], type="o")
plot( 1:93,   avg_corr_temp_spearman[20:112], type="o")
 
kk=1000
( sum( cor(  matrix(rnorm(20*1000), nrow=20), method= "spearman") -diag(kk),na.rm=T)/( kk^2-kk)   )
### long-series corr is higher b/c two variables shares some persistent common variable,
### then mean moves, so variation in mu-muhat effect, just like autocorrelation case
### we have the same effect even in two different variables ye and yf
### 20quarters of data show not that big corr. just ignore.




rhotemp0 = rep(NA,10^3)
for (iii in seq(1,10^3))
mean(rhotemp0,na.rm=T)

## variation effect on FE(rho) is small
arima(  seq(1,10^6)/10^6*0.67 + arima.sim(list(order = c(1,0,0), ar = 0.15  ), n = 10^6), c(1,0,0))$coef[1]
arima(  seq(1,10^6)/10^6*0.5 + arima.sim(list(order = c(1,0,0), ar = 0.15  ), n = 10^6), c(1,0,0))$coef[1]
arima(  rep(seq(1,10^2)/10^2*0.5) + arima.sim(list(order = c(1,0,0), ar = 0.15  ), n = 10^6), c(1,0,0))$coef[1]

## individual mu learning effect on FE(rho) is small
qnorm(0.4)
qnorm(0.4-apply(FEbias_learn_0.lowball,2,sd,na.rm=T)[1]*qnorm(0.75))
qnorm(0.4+apply(FEbias_learn_0.lowball,2,sd,na.rm=T)[1]*qnorm(0.75))

qnorm(0.4) - qnorm(0.4-apply(FEbias_learn_0.lowball,2,sd,na.rm=T)[1]*qnorm(0.75))

arima(  seq(1,10^6)/10^6*0.32+  arima.sim(list(order = c(1,0,0), ar = 0.15  ), n = 10^6), c(1,0,0))$coef[1]

############# working here ###################
# phi-phihat var reduction trhough firm dimension is flat, because phi fluctureates a lot.
# learning exists, yet implied autocorr(FE) is negligible.
# true SE should be smaller because of dependence of two samples
# Pitman's T-test, pitmam-morgan test. compare variance of two dependent groups.
## Morgan-Pitman test
#t=(F-1)/F*sqrt(n-2)/sqrt(1-r^2)/2 = (1-1/F)*sqrt(n-2)/sqrt(1-r^2)/2
#Comparing the Variances of Two Dependent Groups
#Author(s): Rand R. Wilcox
#Source: Journal of Educational Statistics, Vol. 15, No. 3 (Autumn, 1990), pp. 237-247
# MSE decreases, both bias and variance decrease

fm1Fun <- update(M0.phiphihat,devFunOnly=TRUE)
>  library(numDeriv)
>  fm1_thpar <- getME(M0.phiphihat,"theta")
>  h <- hessian(fm1Fun, fm1_thpar)
>  g <- grad(fm1Fun, fm1_thpar)
> library(MASS)
>   sqrt(diag(ginv(h)))


# firmlevel phi: use ARIMA
phi_firms =rep(NA, length(est$firm_begin_deflated ))    
for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
{  
  firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]  
  m1=arima( yef_temp[firm_idx_temp]  , order=c(1,0,0), method = c("ML"),optim.control = list(maxit = 300000 )) 
  rho_firms[jj] = m1$coef[1] 
  
}
FErhoadj = ((rho_firms*(est$no_obs_deflated-1)+1)/(est$no_obs_deflated-4)) 
avgFErhofirm = mean(FErhoadj)
sd(FErhoadj)
# Mean of firmlevel autocorr(FE) biase adjusted = 0.203
sum(abs(NA_idx_temp - is.na(year_idx_numeric) ))


### construct year-quarter index without NA
yearqtr_idx_numeric = est$year_qtr_idx
for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
{  
  firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]      
  yearqtr_idx_numeric[firm_idx_temp] = seq( yearqtr_idx_numeric[est$firm_begin_deflated[jj]],yearqtr_idx_numeric[est$firm_end_deflated[jj]], by=0.25)
  
}
sum( is.na(yearqtr_idx_numeric)    )
sum(abs(yearqtr_idx_numeric-est$year_qtr_idx),na.rm=T)
range(yearqtr_idx_numeric)

### average of FE_rho for each quarter from firm-level FE_rho estiamte.
FErhoadj_firmqtr = FErhoadj[est$firm_deflated_nn_idx]
yearqtr_xaxis = seq(range(yearqtr_idx_numeric,na.rm=T)[1],range(yearqtr_idx_numeric,na.rm=T)[2],by=0.25)
N_row_yearqtr =length(table(yearqtr_idx_numeric))

if (do_all) {
  m1.learn = lmer(FE_temp_pooled~  FE_temp_pooled_lag1 + (FE_temp_pooled_lag1|firm_dummy_pooled)    , REML = FALSE)
  save(m1.learn, file="m1_learn.Rdata")
}

load("m1_learn.Rdata")
FErhoadj_mixed=fixef(m1.learn)[2]+(ranef(m1.learn)$firm_dummy_pooled$"FE_temp_pooled_lag1")
FErhoadj_mixed = ((FErhoadj_mixed*(est$no_obs_deflated-1)+1)/(est$no_obs_deflated-4)) 
FErhoadj_firmqtr_mixed = FErhoadj_mixed[est$firm_deflated_nn_idx]
sd(FErhoadj)
sd(FErhoadj_mixed)
sd( ((rho_firms_OLS*(est$no_obs_deflated-1)+1)/(est$no_obs_deflated-4))  )
sqrt(var(((rho_firms_OLS*(est$no_obs_deflated-1)+1)/(est$no_obs_deflated-4)))-var(FErhoadj_mixed))
plot(est$no_obs_deflated,FErhoadj_mixed)
mean(FErhoadj>0)
hist(FErhoadj,100)
hist(FErhoadj_mixed,100)
ttt=seq(20,100);rrr=0.2;plot(ttt, ( ((rrr*(ttt-1)+1)/(ttt-4))  )-rrr,type="l")
ttt=seq(20,100);rrr=0.1;plot(ttt, ( ((rrr*(ttt-1)+1)/(ttt-4))  )-rrr,type="l")
ttt=seq(20,100);rrr=0;plot(ttt, ( ((rrr*(ttt-1)+1)/(ttt-4))  )-rrr,type="l")
ttt=seq(20,100);rrr=-0.14;plot(ttt, ( ((rrr*(ttt-1)+1)/(ttt-4))  )-rrr,type="l")
ttt=seq(20,100);rrr=-0.2;plot(ttt, ( ((rrr*(ttt-1)+1)/(ttt-4))  )-rrr,type="l")
ttt=seq(10,100);rrr=0;plot(ttt, ( ((rrr*(ttt-1)+1)/(ttt-4))  )-rrr,type="l")


rhotemp0 = rep(NA,10^3)
for (iii in seq(1,10^3)) rhotemp0[iii] = arima( arima.sim(list(order = c(1,0,0), ar = -0.1  ), n = 20), c(1,0,0))$coef[1]
mean(rhotemp0,na.rm=T)



FErho_yearqtr_adj_firm_avg = rep(NA, N_row_yearqtr )
FErho_yearqtr_adj_firm_SE =  rep(NA, N_row_yearqtr )

FErho_yearqtr_adj_firm_avg_mixed = rep(NA, N_row_yearqtr )
FErho_yearqtr_adj_firm_SE_mixed =  rep(NA, N_row_yearqtr )

for (ii in seq( 1,N_row_yearqtr ) ) {    
  
  FErho_yearqtr_adj_firm = FErhoadj_firmqtr[ yearqtr_idx_numeric   == yearqtr_xaxis[ii]   ]  
  FErho_yearqtr_adj_firm_avg[ii] = mean(FErho_yearqtr_adj_firm ,na.rm=T)
  FErho_yearqtr_adj_firm_SE[ii] = sd(FErho_yearqtr_adj_firm,na.rm=T)/sqrt( sum(!is.na(FErho_yearqtr_adj_firm)) )
  
  FErho_yearqtr_adj_firm_mixed = FErhoadj_firmqtr_mixed[ yearqtr_idx_numeric   == yearqtr_xaxis[ii]   ]  
  FErho_yearqtr_adj_firm_avg_mixed[ii] = mean(FErho_yearqtr_adj_firm_mixed ,na.rm=T)
  FErho_yearqtr_adj_firm_SE_mixed[ii] = sd(FErho_yearqtr_adj_firm_mixed,na.rm=T)/sqrt( sum(!is.na(FErho_yearqtr_adj_firm_mixed)) )
  
  print(ii)
}

par(mfrow=c(1,2))
plot(yearqtr_xaxis, FErho_yearqtr_adj_firm_avg ,type="b")
plot(yearqtr_xaxis,FErho_yearqtr_adj_firm_SE,type="b")

par(mfrow=c(1,2))
plot(yearqtr_xaxis, FErho_yearqtr_adj_firm_avg_mixed ,type="b")
plot(yearqtr_xaxis,FErho_yearqtr_adj_firm_SE_mixed,type="b")
## two methods are similar.


## phi change within firm over age

load("phi_learn_0.Rdata")
load("phihat_learn_0.Rdata")
load("phi_learn_0.alt.Rdata")
load("phihat_learn_0.alt.Rdata")
par(mfrow=c(1,2))


# mu learning: plot (1)
ssd = 1.96



x =    phi_learn_0
#x =    phi_learn_0 - phihat_learn_0
#x =    phi_learn_0.alt - phihat_learn_0.alt


xxx =  (apply( x     , 2, mean,na.rm=T))  
x_SE =    apply( x,2, sd,na.rm=T)/ sqrt( apply( !is.na(x ),2, sum) )


phiphihat_learn_0.1 = (matrix(rep((x[,1])  ,93), ncol=93)) + x*0
phiphihat_learn_0.11 = (matrix(rep((x[,11])  ,93), ncol=93)) + x*0
xxx.1 =  (apply( phiphihat_learn_0.1     , 2, mean,na.rm=T)) 
x_SE.1 =    apply( x-phiphihat_learn_0.1 ,2, sd,na.rm=T)/ sqrt( apply( !is.na(phiphihat_learn_0.1 ),2, sum) )
CIlow.1 = xxx -x_SE.1*ssd
CIhigh.1 = xxx +x_SE.1*ssd

nset=apply(!is.na(cbind( x )),2,sum)
CIlow = xxx -x_SE*ssd
CIhigh = xxx +x_SE*ssd

xaxis  = 1:93

T_temp = length(xxx)
xxx = xxx[1:(T_temp-2)]
CIlow =   CIlow[1:(T_temp-2)]
CIhigh =  CIhigh[1:(T_temp-2)]
xaxis  = xaxis[1:(T_temp-2)]
CIlow.1 =   CIlow.1[1:(T_temp-2)]
CIhigh.1 =  CIhigh.1[1:(T_temp-2)]

## plot in the paper
plot(NA,xlim=c(xaxis[1], xaxis[length(xaxis)]), ylim=c(-0.1+0.4, 0.1+0.4),
     main=expression(hat(E)[italic("t+1:t+20")]*"[ ("*phi-hat(phi)*")"[italic("i, t+1:t+20")]*" ]"),
     xlab=expression("Quarter "*italic("t")*" since the first observation"),ylab="")
polygon( c(xaxis, xaxis[length(xaxis):1]  ),  
         c(CIlow.1, CIhigh.1[length(xaxis):1]), border = NA, col = "gray75")
polygon( c(xaxis, xaxis[length(xaxis):1]  ),  
         c(CIlow, CIhigh[length(xaxis):1]), border = NA, col = "gray55")
lines(xaxis,xxx, cex=0.5, lwd=1  ,type="o" )
abline(h=1, lty="dashed"  )
box()
lines(xaxis[11:91], apply( phiphihat_learn_0.11 , 2, mean,na.rm=T)[11:91] , cex=0.5, lwd=1  ,type="l", lty="solid"  )
lines(xaxis[1:91], apply( phiphihat_learn_0.1 , 2, mean,na.rm=T)[1:91] , cex=0.5, lwd=1  ,type="l", lty="dashed"  )

legend("bottomleft", inset=0.02,
       c(expression(hat(E)[italic("t+1:t+20")]*"[ ("*phi-hat(phi)*")"[italic("i, t+1:t+20")]*" ]"),
         expression(hat(E)[italic("t+1:t+20")]*"[ ("*phi-hat(phi)*")"[italic("i, 1:20")]*" ]"),
         expression(hat(E)[italic("t+1:t+20")]*"[ ("*phi-hat(phi)*")"[italic("i, 11:30")]*" ]"),
         expression("95% C.I. of "*hat(E)[italic("t+1:t+20")]*"[ ("*phi-hat(phi)*")"[italic("i, t+1:t+20")]*" ]"),
         expression("95% no-rejection regieon of "*hat(E)[italic("t+1:t+20")]*"[ ("*phi-hat(phi)*")"[italic("i, 1:20")]*" ]"),
         expression(paste("H"[o]*": "*E[italic("t+1:t+20")]*"[ ("*phi-hat(phi)*")"[italic("i, t+1:t+20")]," - ("*phi-hat(phi)*")"[italic("i, 1:20")]*" ]=0"))),
       lty=c(1,2,1,1,1,0), lwd=c(1,1,1,7,7,0), bty = "n",
       col=c("black","black","black","gray55","gray75"), pch=c(1,NA,NA,NA,NA,NA), cex=0.7 )











load("phiphihat_learn_0.alt.Rdata")
load("phiphihat_learn_0.Rdata")

par(mfrow=c(1,2))
# phi learning: plot (1)
ssd = 1.96
x =  (phiphihat_learn_0.alt)
temp_Ref  = matrix(rep(  (x[, 11-10*0]),93),ncol=93)
temp_Ref = temp_Ref +x*0 
x = x+ temp_Ref *0
x_SE =  apply(  x - temp_Ref ,2, sd,na.rm=T) / sqrt( apply( !is.na(x- temp_Ref),2, sum) )
xxx =  (apply( x-temp_Ref   , 2, mean,na.rm=T)) + mean(  temp_Ref[,11]    ,na.rm=T)

nset=apply(!is.na(cbind( x )),2,sum)
CIlow = xxx -x_SE*ssd
CIhigh = xxx +x_SE*ssd

xaxis  = 1:93

T_temp = length(xxx)
xxx = xxx[1:(T_temp-1)]
CIlow =   CIlow[1:(T_temp-1)]
CIhigh =  CIhigh[1:(T_temp-1)]
xaxis  = xaxis[1:(T_temp-1)]


## plot in the paper
plot(NA,xlim=c(xaxis[1], xaxis[length(xaxis)]), ylim=c(-0.3, 0.5)/10,
     main=expression(frac("Var[("*phi[i]*"-"*hat(phi)[i]*')'["t+1:t+20"]*"]","Var[("*phi[i]*"-"*hat(phi)[i]*')'["1:20"]*"]")),
     xlab=expression("Quarter "*italic("t")),ylab="")
polygon( c(xaxis, xaxis[length(xaxis):1]  ),  
         c(CIlow, CIhigh[length(xaxis):1]), border = NA, col = "gray")
lines(xaxis,xxx, cex=0.5, lwd=1  ,type="l" )
points(xaxis,xxx, cex=0.5, lwd=1    )
abline(h=0, lty="dashed"  )
box()


 
load("phiphihat_learn_lmer0.Rdata") 
load("phiphihat_learn_lmer_var0.Rdata")
load("phiphihat_learn_lmer_se0.Rdata")
# phi learning: plot (1)
ssd = 1.96
x =  phiphihat_learn_lmer0
#temp_Ref  = matrix(rep(  (x[, 11-10 ]),93),ncol=93)
#temp_Ref = temp_Ref +x*0 
#x = x+ temp_Ref *0
x_SE =  phiphihat_learn_lmer_se0
xxx =  (apply( x    , 2, mean,na.rm=T))  

nset=apply(!is.na(cbind( x )),2,sum)
CIlow = xxx -x_SE*ssd
CIhigh = xxx +x_SE*ssd

xaxis  = 1:93

T_temp = length(xxx)
xxx = xxx[1:(T_temp-1)]
CIlow =   CIlow[1:(T_temp-1)]
CIhigh =  CIhigh[1:(T_temp-1)]
xaxis  = xaxis[1:(T_temp-1)]


## plot in the paper
plot(NA,xlim=c(xaxis[1], xaxis[length(xaxis)]), ylim=c(-0.03, 0.05) ,
     main=expression(frac("Var[("*phi[i]*"-"*hat(phi)[i]*')'["t+1:t+20"]*"]","Var[("*phi[i]*"-"*hat(phi)[i]*')'["1:20"]*"]")),
     xlab=expression("Quarter "*italic("t")),ylab="")
polygon( c(xaxis, xaxis[length(xaxis):1]  ),  
         c(CIlow, CIhigh[length(xaxis):1]), border = NA, col = "gray")
lines(xaxis,xxx, cex=0.5, lwd=1  ,type="l" )
points(xaxis,xxx, cex=0.5, lwd=1    )
abline(h=0, lty="dashed"  )
box()
 

plot(1:93, sqrt(phiphihat_learn_lmer_var0),type="b")
plot(1:93, apply(phiphihat_learn_lmer0,2,sd,na.rm=T),type="b")






# phi learning: plot (2)
ssd = 1.96
x=phiphihat_learn_0.alt
temp_Ref  = matrix(rep( x[, 11-10 ],93),ncol=93)
temp_Ref = temp_Ref +x*0 
x = x+ temp_Ref*0

apply(x, temp_Ref, 2, cor, use="complete.obs")

xxx = (apply( x , 2, var,na.rm=T))/(apply(  temp_Ref, 2, var,na.rm=T))
#xxx = (apply( x , 2, var,na.rm=T))
#xxx = (apply( x^2 , 2, mean,na.rm=T))/(apply(  temp_Ref^2, 2, mean,na.rm=T))

nset=apply(!is.na(cbind( x )),2,sum)
CIlow = rep(NA,93)
CIhigh = rep(NA,93)
for (jj in seq(1,93)) {
  CIlow[jj] = qf((0.025), nset[jj]-1, nset[jj]-1)
  CIhigh[jj] = qf((0.975), nset[jj]-1, nset[jj]-1)
} 

nset=apply(!is.na(cbind( x )),2,sum)
CIlow = rep(NA,93)
CIhigh = rep(NA,93)
for (jj in seq(1,93)) {
  r = cor(x[,jj]  ,temp_Ref[,jj] , use="complete.obs")  
  tL = qt((0.025), nset[jj]-2)  
  CIlow[jj] = 1/(1-2*tL*sqrt( (1-r^2)/(nset[jj]-2) ) )
  tH = qt((0.975), nset[jj]-2)
  CIhigh[jj] = 1/(1-2*tH*sqrt( (1-r^2)/(nset[jj]-2) ) )
} 

xaxis  = 1:93

T_temp = length(xxx)
xxx = xxx[1:(T_temp-1)]
CIlow =   CIlow[1:(T_temp-1)]
CIhigh =  CIhigh[1:(T_temp-1)]
xaxis  = xaxis[1:(T_temp-1)]


## plot in the paper
plot(NA,xlim=c(xaxis[1], xaxis[length(xaxis)]), ylim=c(0.4, 1.2),
     main=expression(frac("Var[("*phi[i]*"-"*hat(phi)[i]*')'["t+1:t+20"]*"]","Var[("*phi[i]*"-"*hat(phi)[i]*')'["1:20"]*"]")),
     xlab=expression("Quarter "*italic("t")),ylab="")
polygon( c(xaxis, xaxis[length(xaxis):1]  ),  
         c(CIlow, CIhigh[length(xaxis):1]), border = NA, col = "gray")
lines(xaxis,xxx, cex=0.5, lwd=1  ,type="l" )
points(xaxis,xxx, cex=0.5, lwd=1    )
abline(h=1, lty="dashed"  )
box()






































# phi learning: plot (2)
ssd = 1.96
x=phiphihat_learn_0.alt
temp_Ref  = matrix(rep( x[, 11-10 ],93),ncol=93)
temp_Ref = temp_Ref +x*0 
x = x+ temp_Ref*0

apply(x, temp_Ref, 2, cor, use="complete.obs")
      
xxx = (apply( x , 2, var,na.rm=T))/(apply(  temp_Ref, 2, var,na.rm=T))
#xxx = (apply( x , 2, var,na.rm=T))
#xxx = (apply( x^2 , 2, mean,na.rm=T))/(apply(  temp_Ref^2, 2, mean,na.rm=T))

nset=apply(!is.na(cbind( x )),2,sum)
CIlow = rep(NA,93)
CIhigh = rep(NA,93)
for (jj in seq(1,93)) {
  CIlow[jj] = qf((0.025), nset[jj]-1, nset[jj]-1)
  CIhigh[jj] = qf((0.975), nset[jj]-1, nset[jj]-1)
} 

nset=apply(!is.na(cbind( x )),2,sum)
CIlow = rep(NA,93)
CIhigh = rep(NA,93)
for (jj in seq(1,93)) {
  r = cor(x[,jj]  ,temp_Ref[,jj] , use="complete.obs")  
  tL = qt((0.025), nset[jj]-2)  
  CIlow[jj] = 1/(1-2*tL*sqrt( (1-r^2)/(nset[jj]-2) ) )
  tH = qt((0.975), nset[jj]-2)
  CIhigh[jj] = 1/(1-2*tH*sqrt( (1-r^2)/(nset[jj]-2) ) )
} 

xaxis  = 1:93

T_temp = length(xxx)
xxx = xxx[1:(T_temp-1)]
CIlow =   CIlow[1:(T_temp-1)]
CIhigh =  CIhigh[1:(T_temp-1)]
xaxis  = xaxis[1:(T_temp-1)]

 
## plot in the paper
plot(NA,xlim=c(xaxis[1], xaxis[length(xaxis)]), ylim=c(0.4, 1.2),
     main=expression(frac("Var[("*phi[i]*"-"*hat(phi)[i]*')'["t+1:t+20"]*"]","Var[("*phi[i]*"-"*hat(phi)[i]*')'["1:20"]*"]")),
     xlab=expression("Quarter "*italic("t")),ylab="")
polygon( c(xaxis, xaxis[length(xaxis):1]  ),  
         c(CIlow, CIhigh[length(xaxis):1]), border = NA, col = "gray")
lines(xaxis,xxx, cex=0.5, lwd=1  ,type="l" )
points(xaxis,xxx, cex=0.5, lwd=1    )
abline(h=1, lty="dashed"  )
box()


















# http://en.wikipedia.org/wiki/F-test_of_equality_of_variances
var(x, na.rm=T)/var(y, na.rm=T)
qf(0.025, xn, yn)
qf(0.975, xn, yn)

nset=apply(!is.na(FEbias_learn_2.lowball.normal),2,sum)
CIlow = rep(NA,73)
CIhigh = rep(NA,73)
for (jj in seq(1,73)) {
  CIlow[jj] = qf((0.025), nset[jj]-1, nset[jj]-1)
  CIhigh[jj] = qf((0.975), nset[jj]-1, nset[jj]-1)
}
ts.plot(cbind(CIlow,  CIhigh)    ,ylim=c(0.7,1.3))
lines( )

(apply( FEbias_learn_2.lowball.normal , 2, var,na.rm=T))/(apply(  temp_Ref.normal, 2, var,na.rm=T))





load("FEbias_learn_0.Rdata")
load("FEbias_learn_0.lowball.Rdata")

par(mfrow=c(1,2))


# mu learning: plot (1)
ssd = 1.96



x =  FEbias_learn_0.lowball
xxx =  (apply( x     , 2, mean,na.rm=T))  
x_SE =     sqrt(xxx*(1-xxx) ) / sqrt( apply( !is.na(x ),2, sum) )


FEbias_learn_0.1.lowball = (matrix(rep((FEbias_learn_0.lowball[,1])  ,93), ncol=93)) + x*0
FEbias_learn_0.11.lowball = (matrix(rep((FEbias_learn_0.lowball[,11])  ,93), ncol=93)) + x*0
xxx.1 =  (apply( FEbias_learn_0.1.lowball     , 2, mean,na.rm=T)) 
x_SE.1 =     sqrt(xxx.1*(1-xxx.1) ) / sqrt( apply( !is.na(FEbias_learn_0.1.lowball ),2, sum) )
x_SE.1 = sqrt(x_SE^2 + x_SE.1^2)
CIlow.1 = xxx -x_SE.1*ssd
CIhigh.1 = xxx +x_SE.1*ssd

nset=apply(!is.na(cbind( x )),2,sum)
CIlow = xxx -x_SE*ssd
CIhigh = xxx +x_SE*ssd

xaxis  = 1:93

T_temp = length(xxx)
xxx = xxx[1:(T_temp-1)]
CIlow =   CIlow[1:(T_temp-1)]
CIhigh =  CIhigh[1:(T_temp-1)]
xaxis  = xaxis[1:(T_temp-1)]
CIlow.1 =   CIlow.1[1:(T_temp-1)]
CIhigh.1 =  CIhigh.1[1:(T_temp-1)]

## plot in the paper
plot(NA,xlim=c(xaxis[1], xaxis[length(xaxis)]), ylim=c(0, 0.55),
     main=expression("Avg[ "*italic("Lowball")["i, t+1:t+20"]*" ]"),
     xlab=expression("Quarter "*italic("t")),ylab="")
polygon( c(xaxis, xaxis[length(xaxis):1]  ),  
         c(CIlow.1, CIhigh.1[length(xaxis):1]), border = NA, col = "gray75")
polygon( c(xaxis, xaxis[length(xaxis):1]  ),  
         c(CIlow, CIhigh[length(xaxis):1]), border = NA, col = "gray55")

lines(xaxis,xxx, cex=0.5, lwd=1  ,type="l" )
points(xaxis,xxx, cex=0.5, lwd=1    )
abline(h=1, lty="dashed"  )
box()
lines(xaxis, apply( FEbias_learn_0.11.lowball , 2, mean,na.rm=T)[1:92] , cex=0.5, lwd=1  ,type="l", lty="solid"  )
lines(xaxis, apply( FEbias_learn_0.1.lowball , 2, mean,na.rm=T)[1:92] , cex=0.5, lwd=1  ,type="l", lty="dashed"  )

legend(0, 0.2, c(expression("Avg[ "*italic("Lowball")["i, t+1:t+20"]*" ]"),
                 expression("Avg[ "*italic("Lowball")["i, 1:20"]*" ]"),
                 expression("Avg[ "*italic("Lowball")["i, 11:30"]*" ]"), 
                 expression("95% C.I. of Avg[ "*italic("Lowball")["i, t+1:t+20"]*" ]"),
                 expression("95% no-rejection regieon of Avg[ "*italic("Lowball")["i, 1:20"]*" ]"),
                 expression("with H"[o]*":Avg[ "*italic("Lowball")["i, t+1:t+20"]*" ]=Avg[ "*italic("Lowball")["i, 1:20"]*" ]")),
       lty=c(1,2,1,1,1,0), lwd=c(1,1,1,7,7,0), bty = "n",
       col=c("black","black","black","gray55","gray75"), pch=c(1,NA,NA,NA,NA,NA), cex=0.8 )



# plot(2)


# http://web.eecs.umich.edu/~fessler/papers/files/tr/stderr.pdf
gom.VARSD <- function(x){
  n = sum(!is.na(x))
  
  S2 = var(x,na.rm=T)  
  var.EST = S2
  var.SE = S2*sqrt(2/(n-1))  
  
  S = sd(x,na.rm=T)  
  Kn = sqrt((n-1)/2)*exp(  lgamma((n-1)/2)-lgamma(n/2)  )
  Vn = 2*((n-1)/2 -  exp(  2*lgamma(n/2)  - 2*lgamma((n-1)/2)  )  )
  sd.EST = Kn*S
  sd.SE = S*Kn*sqrt(Vn/(n-1))  
  
  #return(list(var.EST=var.EST  ,  var.SE=var.SE, sd.EST=sd.EST, sd.SE=sd.SE  ))
  return(sd.SE=sd.SE  )
}




ssd = 1.96
x=FEbias_learn_0.lowball

FEbias_learn_0.1.lowball = (matrix(rep((FEbias_learn_0.lowball[,1])  ,93), ncol=93)) + x*0
FEbias_learn_0.11.lowball = (matrix(rep((FEbias_learn_0.lowball[,11])  ,93), ncol=93)) + x*0
temp_Ref = FEbias_learn_0.1.lowball


xxx = (apply( x , 2, var,na.rm=T))#/(apply( temp_Ref , 2, var,na.rm=T))




nset=apply(!is.na(cbind(x )),2,sum)
CIlow = rep(NA,93)
CIhigh = rep(NA,93)
for (jj in seq(1,93)) {
  CIlow[jj] = qf((0.025), nset[jj]-1, nset[jj]-1)
  CIhigh[jj] = qf((0.975), nset[jj]-1, nset[jj]-1)
} 

nset=apply(!is.na(cbind( x )),2,sum)
CIlow = rep(NA,93)
CIhigh = rep(NA,93)
for (jj in seq(1,93)) {
  r = cor(x[,jj]  ,temp_Ref[,jj] , use="complete.obs")
  tL = qt((0.025), nset[jj]-2)  
  CIlow[jj] = 1/(1-2*tL*sqrt( (1-r^2)/(nset[jj]-2) ) )*xxx[jj] 
  tH = qt((0.975), nset[jj]-2)
  CIhigh[jj] = 1/(1-2*tH*sqrt( (1-r^2)/(nset[jj]-2) ) )*xxx[jj] 
} 


CIlow.1 = (sqrt(xxx)-ssd*apply( x,2, gom.VARSD ))^2
CIhigh.1 = (sqrt(xxx)+ssd*apply( x,2, gom.VARSD ))^2




xaxis  = 1:93

T_temp = length(xxx)
xxx = xxx[1:(T_temp-2)]
CIlow =   CIlow[1:(T_temp-2)]
CIhigh =  CIhigh[1:(T_temp-2)]
xaxis  = xaxis[1:(T_temp-2)]
CIlow.1 =   CIlow.1[1:(T_temp-2)]
CIhigh.1 =  CIhigh.1[1:(T_temp-2)]

plot(NA,xlim=c(xaxis[1], xaxis[length(xaxis)]), ylim=c(0, 0.035),  
     main=expression(hat("Var")*"["*"Lowball"["i, t+1:t+20"]*"]+sampling var"),
     xlab=expression("Quarter "*italic("t")),ylab="")
polygon( c(xaxis, xaxis[length(xaxis):1]  ),  
         c(CIlow, CIhigh[length(xaxis):1]), border = NA, col = "gray75")
polygon( c(xaxis, xaxis[length(xaxis):1]  ),  
         c(CIlow.1, CIhigh.1[length(xaxis):1]), border = NA, col = "gray55")
lines(xaxis,xxx, cex=0.5, lwd=1  ,type="l" )
points(xaxis,xxx, cex=0.5, lwd=1    )
abline(h=1, lty="dashed"  )
box()
lines(xaxis, apply( FEbias_learn_0.11.lowball , 2, var,na.rm=T)[1:91] , cex=0.5, lwd=1  ,type="l", lty="solid"  )
lines(xaxis, apply( FEbias_learn_0.1.lowball , 2, var,na.rm=T)[1:91] , cex=0.5, lwd=1  ,type="l", lty="dashed"  )
legend(0, 0.01, c(expression("Avg[ "*italic("Lowball")["i, t+1:t+20"]*" ]"),
                  expression("Avg[ "*italic("Lowball")["i, 1:20"]*" ]" ),
                  expression("Avg[ "*italic("Lowball")["i, 11:30"]*" ]" )),
       lty=c(1,2,1), lwd=c(1,1,1), bty = "n",
       col=c("black","black","black"), pch=c(1,NA,NA)   )

legend(0, 0.013, c(expression("Var["*"Lowball"["i, t+1:t+20"]*"]"),
                   expression("Var["*"Lowball"["i, 1:20"]*"]"),
                   expression("Var["*"Lowball"["i, 11:30"]*"]"), 
                   expression("95% C.I. of Var["*"Lowball"["i, t+1:t+20"]*"]"),
                   expression("95% no-rejection regieon of Avg[ "*italic("Lowball")["i, 1:20"]*" ]"),
                   expression("with H"[o]*":Avg[ "*italic("Lowball")["i, t+1:t+20"]*" ]=Avg[ "*italic("Lowball")["i, 1:20"]*" ]")),
       lty=c(1,2,1,1,1,0), lwd=c(1,1,1,7,7,0), bty = "n",
       col=c("black","black","black","gray55","gray75"), pch=c(1,NA,NA,NA,NA,NA), cex=0.8 )

























max_no_obs_deflated = max(est$no_obs_deflated)
no_window_learn = max_no_obs_deflated-40+1

if (do_all) {
  FErho_learn_1 = matrix(rep(NA, no_window_learn*length(est$firm_begin_deflated )  ), ncol= no_window_learn) 
  FErho_learn_2 = matrix(rep(NA, no_window_learn*length(est$firm_begin_deflated )  ), ncol= no_window_learn) 
  
  
  for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
  {  
    firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]     
    nonNA_index_firm_temp = nonNA_index_firm[firm_idx_temp]
    xxx = yef_temp[firm_idx_temp]
    onetoT = est$TS_index_firm_deflated[firm_idx_temp]
    
    for (ii in c( 40:max_no_obs_deflated)) {
      if (est$no_obs_deflated[jj] >= ii ) {
        
        tryCatch({
          xxx1=xxx[ onetoT[nonNA_index_firm_temp==ii-39]:onetoT[nonNA_index_firm_temp==ii-20] ]
          xxx2=xxx[ onetoT[nonNA_index_firm_temp==ii-19]:onetoT[nonNA_index_firm_temp==ii] ]        
          FErho_learn_1[jj,ii-39] = arima( xxx1 , order=c(1,0,0), method = c("ML"),optim.control = list(maxit = 300000 ))$coef[1]
          FErho_learn_2[jj,ii-39] = arima( xxx2 , order=c(1,0,0), method = c("ML"),optim.control = list(maxit = 300000 ))$coef[1]    
        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
                             erroroccur = TRUE} )
      }
    }
    print(jj)
  }
  
  
  FErho_learn_1 =  (FErho_learn_1 *( 20 - 1)+1) / ( 20 - 4)
  FErho_learn_2 =  (FErho_learn_2 *( 20 - 1)+1) / ( 20 - 4)
  
  save(FErho_learn_1, file="FErho_learn_1.Rdata")
  save(FErho_learn_2, file="FErho_learn_2.Rdata")
  
  dFErho_abs_learn = abs(FErho_learn_2) - abs(FErho_learn_1)
  save(dFErho_abs_learn, file="dFErho_abs_learn.Rdata")
  
  dFErho_learn = (FErho_learn_2) - (FErho_learn_1)
  save(dFErho_learn, file="dFErho_learn.Rdata")
  
}



load("dFErho_abs_learn.Rdata")
dFErho_learn_mean =  apply(dFErho_abs_learn,2, mean,na.rm=T) 
dFErho_learn_SE = apply(dFErho_abs_learn,2, sd,na.rm=T) / sqrt( apply( !is.na(dFErho_abs_learn),2, sum) )

load("FErho_learn_1.Rdata")
load("FErho_learn_2.Rdata")

dFErho_learn = FErho_learn_2 - matrix(rep(FErho_learn_1[,1],73), ncol=73)
dFErho_learn_mean =  apply(dFErho_learn,2, mean,na.rm=T) 
dFErho_learn_SE = apply(dFErho_learn,2, sd,na.rm=T) / sqrt( apply( !is.na(dFErho_learn),2, sum) )

load("FErho_learn_1.Rdata")
load("FErho_learn_2.Rdata")

dFErho_learn = abs(FErho_learn_2) - abs(matrix(rep(FErho_learn_1[,11],73), ncol=73))
dFErho_learn = cbind(abs(FErho_learn_1[,11:20]) - abs(matrix(rep(FErho_learn_1[,11],10), ncol=10)) , dFErho_learn)

dFErho_learn =  (FErho_learn_2) - (matrix(rep(FErho_learn_1[,1],73), ncol=73))
dFErho_learn = cbind( (FErho_learn_1[,1:10]) -  (matrix(rep(FErho_learn_1[,1],10), ncol=10)) , dFErho_learn)



dFErho_learn =  (FErho_learn_2) - (matrix(rep(FErho_learn_1[,11],73), ncol=73))
dFErho_learn = cbind( (FErho_learn_1[,11:20]) -  (matrix(rep(FErho_learn_1[,11],10), ncol=10)) , dFErho_learn)

dFErho_learn_mean =  apply(dFErho_learn,2, mean,na.rm=T) 
dFErho_learn_SE = apply(dFErho_learn,2, sd,na.rm=T) / sqrt( apply( !is.na(dFErho_learn),2, sum) )


########### plot in the paper
par(mfrow=c(1,2))
# year effect on firm-level est
ssd = 1.96
xxx = FErho_yearqtr_adj_firm_avg 
CIlow =   xxx - FErho_yearqtr_adj_firm_SE*ssd   
CIhigh =  xxx + FErho_yearqtr_adj_firm_SE*ssd  
xaxis  = yearqtr_xaxis

T_temp = length(xxx)
xxx = xxx[1:(T_temp-1)]
CIlow =   CIlow[1:(T_temp-1)]
CIhigh =  CIhigh[1:(T_temp-1)]
xaxis  = xaxis[1:(T_temp-1)]

plot(NA,xlim=c(xaxis[1], xaxis[length(xaxis)]),  ylim=c(min(CIlow),max(CIhigh)) ,
     main="Year effect on Autocorr(FE)", xlab="Year",ylab="")
polygon( c(xaxis, xaxis[length(xaxis):1]  ),  
         c(CIlow, CIhigh[length(xaxis):1]), border = NA, col = "gray")
lines(xaxis,xxx, cex=0.5, lwd=1  ,type="l" )
points(xaxis,xxx, cex=0.5, lwd=1    )
abline(h=0, lty="dashed"  )



# year effect on firm-level est
ssd = 1.96
xxx =dFErho_learn_mean
CIlow =   xxx - dFErho_learn_SE*ssd   
CIhigh =  xxx + dFErho_learn_SE*ssd  
xaxis  = 10:92

T_temp = length(xxx)
xxx = xxx[1:(T_temp-2)]
CIlow =   CIlow[1:(T_temp-2)]
CIhigh =  CIhigh[1:(T_temp-2)]
xaxis  = xaxis[1:(T_temp-2)]

plot(NA,xlim=c(xaxis[1], xaxis[length(xaxis)]),  ylim=c(-0.14,0.03) ,
     main="Autocorr(FE(t+1:t+20))-Autocorr(FE(11:30))", xlab="quarter t",ylab="")
polygon( c(xaxis, xaxis[length(xaxis):1]  ),  
         c(CIlow, CIhigh[length(xaxis):1]), border = NA, col = "gray")
lines(xaxis,xxx, cex=0.5, lwd=1  ,type="l" )
points(xaxis,xxx, cex=0.5, lwd=1    )
abline(h=0, lty="dashed"  )











### construct year-quarter index without NA
yearqtr_idx_numeric = est$year_qtr_idx
for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
{  
  firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]      
  yearqtr_idx_numeric[firm_idx_temp] = seq( yearqtr_idx_numeric[est$firm_begin_deflated[jj]],yearqtr_idx_numeric[est$firm_end_deflated[jj]], by=0.25)
  
}
sum( is.na(yearqtr_idx_numeric)    )
sum(abs(yearqtr_idx_numeric-est$year_qtr_idx),na.rm=T)
range(yearqtr_idx_numeric)




# firmlevel phi, phihat repeat:
phi_firms =rep(NA, length(est$firm_begin_deflated ))    
phihat_firms =rep(NA, length(est$firm_begin_deflated ))    
for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
{  
  firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]  
  
  y = ye_temp[firm_idx_temp]
  yye = y[2:length(y) ]
  yye.L1 = y[1:(length(y)-1) ]
  
  x = yf_temp[firm_idx_temp]  
  yyf = x[2:length(x) ]
  yyf.L1 = x[1:(length(x)-1) ]
  
  Reg1 = lm(yye ~    yye.L1         )
  Reg2 = lm(yyf ~    yye.L1 + yyf.L1        )
  #   Reg2 = lm(yyf ~    yye.L1         )
  
  phi_firms[jj] =   Reg1$coef[2]
  phihat_firms[jj] = Reg2$coef[2]+Reg2$coef[3]
  #  phihat_firms[jj] = Reg2$coef[2]
  
}



### average of FE_rho for each quarter from firm-level FE_rho estiamte.
phiphihat_firmqtr =  (phi_firms-phihat_firms*0)[est$firm_deflated_nn_idx]
yearqtr_xaxis = seq(range(yearqtr_idx_numeric,na.rm=T)[1],range(yearqtr_idx_numeric,na.rm=T)[2],by=0.25)
N_row_yearqtr =length(table(yearqtr_idx_numeric))

phiphihat_yearqtr_firm_SD =  rep(NA, N_row_yearqtr )
phiphihat_yearqtr_firm_RMSE =  rep(NA, N_row_yearqtr )
phiphihat_yearqtr_firm_mean =  rep(NA, N_row_yearqtr )
for (ii in seq( 1,N_row_yearqtr ) ) {     
  phiphihat_yearqtr_firm_SD[ii] = sd( phiphihat_firmqtr[ yearqtr_idx_numeric   == yearqtr_xaxis[ii]   ] ,na.rm=T)
  phiphihat_yearqtr_firm_RMSE[ii] = sqrt(mean( phiphihat_firmqtr[ yearqtr_idx_numeric   == yearqtr_xaxis[ii]   ]^2 ,na.rm=T))
  phiphihat_yearqtr_firm_mean[ii] =  (mean( phiphihat_firmqtr[ yearqtr_idx_numeric   == yearqtr_xaxis[ii]   ] ,na.rm=T))
  print(ii)
}

par(mfrow=c(1,2))
plot(yearqtr_xaxis,  phiphihat_yearqtr_firm_SD  ,type="b")
plot(yearqtr_xaxis,  phiphihat_yearqtr_firm_RMSE  ,type="b")
## it does not change much
plot(yearqtr_xaxis,  phiphihat_yearqtr_firm_mean  ,type="b")








max_no_obs_deflated = max(est$no_obs_deflated)
no_window_learn = max_no_obs_deflated-40+1

if (do_all) {
  
  FEbias_learn_1 = matrix(rep(NA, no_window_learn*length(est$firm_begin_deflated )  ), ncol= no_window_learn) 
  FEbias_learn_2 = matrix(rep(NA, no_window_learn*length(est$firm_begin_deflated )  ), ncol= no_window_learn) 
  
  FEbias_learn_1.lowball = matrix(rep(NA, no_window_learn*length(est$firm_begin_deflated )  ), ncol= no_window_learn) 
  FEbias_learn_2.lowball = matrix(rep(NA, no_window_learn*length(est$firm_begin_deflated )  ), ncol= no_window_learn) 
  
  
  #phiphihat_learn_1 = matrix(rep(NA, no_window_learn*length(est$firm_begin_deflated )  ), ncol= no_window_learn) 
  #phiphihat_learn_1.alt = matrix(rep(NA, no_window_learn*length(est$firm_begin_deflated )  ), ncol= no_window_learn) 
  #phiphihat_learn_2 = matrix(rep(NA, no_window_learn*length(est$firm_begin_deflated )  ), ncol= no_window_learn) 
  #phiphihat_learn_2.alt = matrix(rep(NA, no_window_learn*length(est$firm_begin_deflated )  ), ncol= no_window_learn) 
  
  
  for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
  {  
    firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]     
    nonNA_index_firm_temp = nonNA_index_firm[firm_idx_temp]
    xxx = yef_temp[firm_idx_temp]

    y = ye_temp[firm_idx_temp]
    yye = y
    yye.L1 = c(NA,y[1:(length(y)-1) ])
    
    x = yf_temp[firm_idx_temp]  
    yyf = x 
    yyf.L1 = c(NA,x[1:(length(x)-1) ])

    

    #  phihat_firms[jj] = Reg2$coef[2]
    
    
    onetoT = est$TS_index_firm_deflated[firm_idx_temp]
    
    for (ii in c( 40:max_no_obs_deflated)) {
      
      if (est$no_obs_deflated[jj] >= ii ) {
        
        tryCatch({
          xxx1=xxx[ onetoT[nonNA_index_firm_temp==ii-39]:onetoT[nonNA_index_firm_temp==ii-20] ]
          xxx2=xxx[ onetoT[nonNA_index_firm_temp==ii-19]:onetoT[nonNA_index_firm_temp==ii] ]        
          FEbias_learn_1[jj,ii-39] = mean(xxx1,na.rm=T)
          FEbias_learn_2[jj,ii-39] = mean(xxx2,na.rm=T)
          
          FEbias_learn_1.lowball[jj,ii-39] = (mean(xxx1<0,na.rm=T)+1-mean(xxx1>0,na.rm=T))/2
          FEbias_learn_2.lowball[jj,ii-39] = (mean(xxx2<0,na.rm=T)+1-mean(xxx2>0,na.rm=T))/2
          
          #yye1=yye[ onetoT[nonNA_index_firm_temp==ii-39]:onetoT[nonNA_index_firm_temp==ii-20] ]
          #yye2=yye[ onetoT[nonNA_index_firm_temp==ii-19]:onetoT[nonNA_index_firm_temp==ii] ]    
          
          #yye1.L1=yye.L1[ onetoT[nonNA_index_firm_temp==ii-39]:onetoT[nonNA_index_firm_temp==ii-20] ]
          #yye2.L1=yye.L1[ onetoT[nonNA_index_firm_temp==ii-19]:onetoT[nonNA_index_firm_temp==ii] ]    

          #yyf1=yyf[ onetoT[nonNA_index_firm_temp==ii-39]:onetoT[nonNA_index_firm_temp==ii-20] ]
          #yyf2=yyf[ onetoT[nonNA_index_firm_temp==ii-19]:onetoT[nonNA_index_firm_temp==ii] ]    
          
          #yyf1.L1=yyf.L1[ onetoT[nonNA_index_firm_temp==ii-39]:onetoT[nonNA_index_firm_temp==ii-20] ]
          #yyf2.L1=yyf.L1[ onetoT[nonNA_index_firm_temp==ii-19]:onetoT[nonNA_index_firm_temp==ii] ]    
          
          #Reg1 = lm(yye1 ~    yye1.L1         )
          #Reg2 = lm(yyf1 ~    yye1.L1 + yyf1.L1        )
          #Reg3 = lm(yyf1 ~    yye1.L1         )
                 
          #phiphihat_learn_1[jj,ii-39] = Reg1$coef[2]- (Reg2$coef[2]+Reg2$coef[3])
          #phiphihat_learn_1.alt[jj,ii-39] = Reg1$coef[2]- Reg3$coef[2]
          
          #Reg1 = lm(yye2 ~    yye2.L1         )
          #Reg2 = lm(yyf2 ~    yye2.L1 + yyf2.L1        )
          #Reg3 = lm(yyf2 ~    yye2.L1         )
          
          #phiphihat_learn_2[jj,ii-39] = Reg1$coef[2]- (Reg2$coef[2]+Reg2$coef[3])
          #phiphihat_learn_2.alt[jj,ii-39] = Reg1$coef[2]- Reg3$coef[2]
            
            
            
        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
                             erroroccur = TRUE} )
      }
    }
    print(jj)
  }
  
  
 
  
}

temp_Ref  = matrix(rep( FEbias_learn_1.lowball[,11],73),ncol=73)
temp_Ref = temp_Ref +FEbias_learn_2.lowball*0 
ts.plot((apply( FEbias_learn_2.lowball , 2, var,na.rm=T))/(apply(  temp_Ref, 2, var,na.rm=T)) ,ylim=c(0.7,1.3))
ts.plot((apply( FEbias_learn_2.lowball , 2, mean,na.rm=T))/(apply(  temp_Ref, 2, mean,na.rm=T)))

ts.plot((apply( FEbias_learn_2.lowball , 2, var,na.rm=T))/(apply( (FEbias_learn_1.lowball +FEbias_learn_2.lowball*0) , 2, var,na.rm=T)) ,ylim=c(0.7,1.3))

ts.plot((apply( FEbias_learn_2.lowball , 2, mean,na.rm=T))/(apply( (FEbias_learn_1.lowball +FEbias_learn_2.lowball*0) , 2, mean,na.rm=T)) ,ylim=c(0.7,1.3))



temp_Ref  = matrix(rep( FEbias_learn_1[,11],73),ncol=73)
temp_Ref = temp_Ref +FEbias_learn_2*0 
ts.plot(  (apply( FEbias_learn_2 , 2, var,na.rm=T)) / ( apply( (FEbias_learn_1+FEbias_learn_2*0), 2, var, na.rm=T) )   )
ts.plot(  (apply( FEbias_learn_2 , 2, var,na.rm=T)) / ( apply( temp_Ref, 2, var, na.rm=T) )  ,ylim=c(0,2) )


FEbias_learn_2.lowball.normal = FEbias_learn_2.lowball
FEbias_learn_1.lowball.normal = FEbias_learn_1.lowball
FEbias_learn_2.lowball.normal[FEbias_learn_2.lowball.normal==0] = 0.01
FEbias_learn_2.lowball.normal[FEbias_learn_2.lowball.normal==1] = 0.99
FEbias_learn_1.lowball.normal[FEbias_learn_1.lowball.normal==0] = 0.01
FEbias_learn_1.lowball.normal[FEbias_learn_1.lowball.normal==1] = 0.99
FEbias_learn_1.lowball.normal = qnorm(FEbias_learn_1.lowball.normal)
FEbias_learn_2.lowball.normal = qnorm(FEbias_learn_2.lowball.normal)


temp_Ref.normal  = matrix(rep( FEbias_learn_1.lowball.normal[,11],73),ncol=73)
temp_Ref.normal = temp_Ref.normal +FEbias_learn_2.lowball.normal*0 
ts.plot((apply( FEbias_learn_2.lowball.normal , 2, var,na.rm=T))/(apply(  temp_Ref.normal, 2, var,na.rm=T)) )
ts.plot((apply( FEbias_learn_2.lowball.normal , 2, var,na.rm=T))/(apply( (FEbias_learn_1.lowball.normal +FEbias_learn_2.lowball.normal*0) , 2, var,na.rm=T)) )



# year effect on firm-level est
ssd = 1.96
temp_Ref  = matrix(rep( FEbias_learn_1.lowball[, 11-10],73),ncol=73)
temp_Ref = temp_Ref +FEbias_learn_2.lowball*0 

xxx = (apply( FEbias_learn_2.lowball , 2, var,na.rm=T))/(apply(  temp_Ref, 2, var,na.rm=T))

temp_Ref_temp  = matrix(rep( FEbias_learn_1.lowball[,11-10],10),ncol=10)
temp_Ref_temp = temp_Ref_temp +FEbias_learn_1.lowball[,11:20-10]*0 

xxx = c(  (apply( FEbias_learn_1.lowball[,11:20-10] , 2, var,na.rm=T))/(apply(  temp_Ref_temp, 2, var,na.rm=T)) ,xxx)
xxx = sqrt(xxx)
nset=apply(!is.na(cbind(FEbias_learn_1.lowball[,11:20-10],FEbias_learn_2.lowball)),2,sum)
CIlow = rep(NA,83)
CIhigh = rep(NA,83)
for (jj in seq(1,83)) {
  CIlow[jj] = qf(sqrt(0.025), nset[jj]-1, nset[jj]-1)
  CIhigh[jj] = qf(sqrt(0.975), nset[jj]-1, nset[jj]-1)
} 
xaxis  = 10:92

 

plot(NA,xlim=c(xaxis[1], xaxis[length(xaxis)]), ylim=c(0.8, 1.3),
     main="Autocorr(FE(t+1:t+20))-Autocorr(FE(11:30))", xlab="quarter t",ylab="")
polygon( c(xaxis, xaxis[length(xaxis):1]  ),  
         c(CIlow, CIhigh[length(xaxis):1]), border = NA, col = "gray")
lines(xaxis,xxx, cex=0.5, lwd=1  ,type="l" )
points(xaxis,xxx, cex=0.5, lwd=1    )
abline(h=0, lty="dashed"  )



rhosimultemp = rep(NA, 10^4)
for (jj in  seq(1, length(rhosimultemp))) rhosimultemp[jj] = arima( seq(1,50)/100-1.27+rnorm(50),c(1,0,0), method="ML")$coef[1]
mean(((rhosimultemp*( 50 -1)+1)/( 50-4)))



 













# Figure (1)
Reg1 = lm(ye_temp_pooled~   -1+ year_dummy_pooled  + ye_temp_pooled_lag1+  year_dummy_pooled:ye_temp_pooled_lag1    )
Reg2 = lm(yf_temp_pooled~   -1 + year_dummy_pooled  + ye_temp_pooled_lag1+  year_dummy_pooled:ye_temp_pooled_lag1 )

phi_year_TS =  c(0,Reg1$coef[31:58])+Reg1$coef[30]
phihat_year_TS = c(0,Reg2$coef[31:58])+Reg2$coef[30]
mean(phi_year_TS)
mean(phihat_year_TS)
par(mfrow=c(1,2))  
plot(1985:2013,phi_year_TS, 
     type="l",lwd=6, col="gray60",ylim=c(0.15,0.6),xlab="Year",main="Without Random Firm Effect",
     ylab=expression(paste("Autoregressive coef. (",phi,", ",hat(phi),")")))
lines(1985:2013,  phihat_year_TS , type="l")


ts.plot((apply( phiphihat_learn_2^2 , 2, mean,na.rm=T)) -(apply( (phiphihat_learn_1+phiphihat_learn_2*0)^2 , 2, mean,na.rm=T)), ylim=c(-0.03,0.0300))
ts.plot((apply( phiphihat_learn_2.alt^2 , 2, mean,na.rm=T)) -(apply( (phiphihat_learn_1.alt+phiphihat_learn_2*0)^2 , 2, mean,na.rm=T)), ylim=c(-0.01,0.01))


phiphihat_learn_1.01 = matrix(rep( phiphihat_learn_1[,2],73),ncol=73)
phiphihat_learn_1.01 = phiphihat_learn_1.01 +phiphihat_learn_2*0
ts.plot( (apply( phiphihat_learn_2^2  , 2, mean,na.rm=T)) - (apply( phiphihat_learn_1.01^2 , 2, mean,na.rm=T)), ylim=c(-0.03,0.01))


ts.plot((apply( phiphihat_learn_2  , 2, sd,na.rm=T)) -(apply( phiphihat_learn_1 , 2, sd,na.rm=T)), ylim=c(-0.05,0.1))
ts.plot((apply( phiphihat_learn_2.alt  , 2, sd,na.rm=T)) -(apply( phiphihat_learn_1.alt , 2, sd,na.rm=T)), ylim=c(-0.05,0.1))

ts.plot(sqrt(apply( phiphihat_learn_2^2 , 2, mean,na.rm=T)) -sqrt(mean( phiphihat_learn_1[,1]^2 ,  na.rm=T)), ylim=c(-0.1,0.1))
ts.plot(sqrt(apply( phiphihat_learn_2.alt^2 , 2, mean,na.rm=T)) -sqrt(mean( phiphihat_learn_1.alt[,1]^2 ,  na.rm=T)), ylim=c(-0.1,0.1))

ts.plot((apply( phiphihat_learn_2  , 2, sd,na.rm=T)) -(apply( phiphihat_learn_1 , 2, sd,na.rm=T)), ylim=c(-0.05,0.1))
ts.plot((apply( phiphihat_learn_2.alt  , 2, sd,na.rm=T)) -(apply( phiphihat_learn_1.alt , 2, sd,na.rm=T)), ylim=c(-0.05,0.1))
 

phiphihat_learn_1 - phiphihat_learn_2 + phiphihat_learn_2
phiphihat_learn_1 - phiphihat_learn_2 + phiphihat_learn_2

 
dFErho_learn =  (FErho_learn_2) - (matrix(rep(FErho_learn_1[,11],73), ncol=73))
dFErho_learn = cbind( (FErho_learn_1[,11:20]) -  (matrix(rep(FErho_learn_1[,11],10), ncol=10)) , dFErho_learn)

dFErho_learn_mean =  apply(dFErho_learn,2, mean,na.rm=T) 
dFErho_learn_SE = apply(dFErho_learn,2, sd,na.rm=T) / sqrt( apply( !is.na(dFErho_learn),2, sum) )


# plot in the paper
par(mfrow=c(1,2))
# year effect on firm-level est
ssd = 1.96
xxx = FErho_yearqtr_adj_firm_avg 
CIlow =   xxx - FErho_yearqtr_adj_firm_SE*ssd   
CIhigh =  xxx + FErho_yearqtr_adj_firm_SE*ssd  
xaxis  = yearqtr_xaxis

T_temp = length(xxx)
xxx = xxx[1:(T_temp-1)]
CIlow =   CIlow[1:(T_temp-1)]
CIhigh =  CIhigh[1:(T_temp-1)]
xaxis  = xaxis[1:(T_temp-1)]

plot(NA,xlim=c(xaxis[1], xaxis[length(xaxis)]),  ylim=c(min(CIlow),max(CIhigh)) ,
     main="Year effect on Autocorr(FE)", xlab="Year",ylab="")
polygon( c(xaxis, xaxis[length(xaxis):1]  ),  
         c(CIlow, CIhigh[length(xaxis):1]), border = NA, col = "gray")
lines(xaxis,xxx, cex=0.5, lwd=1  ,type="l" )
points(xaxis,xxx, cex=0.5, lwd=1    )
abline(h=0, lty="dashed"  )














# Copmute AR(1) estimate for 2-subperiods at firm-level and take the average of bias-adjusted rho. do it cumulatively with different minimum data length
# compare 1:20 to 21:40,  21:40 to 41:60, 41:60 to 61:80, like this....
x=0.25*2;arima(  FE_temp_pooled[ as.numeric(age_dummy_pooled)<20*x & as.numeric(age_dummy_pooled)> 10*x & no_firmobs_pooled>20*x], c(1,0,0)   )$coef[1]-arima(  FE_temp_pooled[ as.numeric(age_dummy_pooled)<10*x   & no_firmobs_pooled>20*x ], c(1,0,0)   )$coef[1]  












# firmlevel FE_rho (para / age) : use OLS
rho_firms =rep(NA, length(est$firm_begin_deflated ))    
for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
{  
  firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj] 
  x = yef_temp[firm_idx_temp]
  y = x[2:length(x) ]
  ylag1 = x[1:(length(x)-1) ]
  m1=lm(y~ylag1)
  rho_firms[jj] = m1$coef[2]  
}
FErhoadj = ((rho_firms*(est$no_obs_deflated-1)+1)/(est$no_obs_deflated-4)) 
avgFErhofirm = mean(FErhoadj)
sd(FErhoadj)
# Mean of firmlevel autocorr(FE) biase adjusted = 0.203
sum(abs(NA_idx_temp - is.na(year_idx_numeric) ))













NA_idx_temp.lag1 = c(FALSE, NA_idx_temp[1:(length(NA_idx_temp)-1)   ])
NA_idx_temp.lag_1 = c(NA_idx_temp[2:(length(NA_idx_temp))   ], FALSE)
yearqtr_idx_numeric[NA_idx_temp] = (yearqtr_idx_numeric[NA_idx_temp.lag1]+yearqtr_idx_numeric[NA_idx_temp.lag_1])/2


FErhoadj_firmqtr = FErhoadj[est$firm_deflated_nn_idx]
FErho_year_adj_firm_avg = rep(NA, 2013-1985+1)
FErho_year_adj_firm_SE =  rep(NA, 2013-1985+1)
for (ii in c( 1985:2013 ) ) {    
     
  FErho_year_adj_firm = FErhoadj_firmqtr[ abs(year_idx_numeric - ii)<2.1 ]
  
  FErho_year_adj_firm_avg[ii-1984] = mean(FErho_year_adj_firm ,na.rm=T)
  FErho_year_adj_firm_SE[ii-1984] = sd(FErho_year_adj_firm,na.rm=T)/sqrt( sum(!is.na(FErho_year_adj_firm)) )
   
  print(ii)
}

plot(1985:2013,FErho_year_adj_firm_avg,type="b")
plot(1985:2013,FErho_year_adj_firm_SE,type="b")

# Copmute AR(1) estimate for 5-year rolling window 
# at firm-level and take the average of bias-adjusted rho.
# use ARIMA

FErho_year_adj_firm_avg = rep(NA, 2013-1985+1)
FErho_year_adj_firm_SE =  rep(NA, 2013-1985+1)

FErho_year_pooled = rep(NA, 2013-1985+1)
FErho_year_pooled_SE = rep(NA, 2013-1985+1)

for (ii in c( 1985:2013 ) ) {    
  rho_firms_temp = rep(NA, length( est$firm_begin_deflated ) )      
  no_year_temp = rep(NA, length( est$firm_begin_deflated ) )    
  
  for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
  {  
    firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]  
    xxx = c(yef_temp[firm_idx_temp])[  abs(year_idx_numeric[firm_idx_temp] - ii)<2.1 ]
    if (sum(!is.na(xxx))>=20 ){
      m1=arima( xxx  , order=c(1,0,0), method = c("ML"),optim.control = list(maxit = 300000 )) 
      rho_firms_temp[jj] = m1$coef[1]   
      no_year_temp[jj] =  sum(!is.na(xxx))
    }
  }  
  FErho_year_adj_firm = ((rho_firms_temp*( no_year_temp -1)+1)/( no_year_temp -4))
  FErho_year_adj_firm_avg[ii-1984]  = mean(   FErho_year_adj_firm,na.rm=T)
  FErho_year_adj_firm_SE[ii-1984] = sd(FErho_year_adj_firm,na.rm=T)/sqrt( sum(!is.na(  FErho_year_adj_firm ) ))  
  
     
  xxx =  yef_temp
  xxx[ abs(year_idx_numeric - ii)>2.1 ] = NA
  pdataset = list( y=xxx, begin_idx =firm_TS_data$firm_begin_deflated, end_idx = firm_TS_data$firm_end_deflated)
  FEsim = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
  
  m1=arima( xxx  , order=c(1,0,0), method = c("ML"),optim.control = list(maxit = 300000 )) 
  FErho_year_pooled[ii-1984] = m1$coef[1]   
  FErho_year_pooled_SE[ii-1984] =  sqrt(m1$var.coef[1])  
    
  print(ii)
}

plot(FErho_year_pooled,type="b")
plot(FErho_year_pooled_SE,type="b")
plot(FErho_year_adj_firm_avg,type="b")
plot(FErho_year_adj_firm_SE,type="b")




FErho_year_pooled = rep(NA, 2013-1985+1)
FErho_year_pooled_SE = rep(NA, 2013-1985+1)

for (ii in c( 1985:2013 ) ) {    
  rho_firms_temp = rep(NA, length( est$firm_begin_deflated ) )      
  no_year_temp = rep(NA, length( est$firm_begin_deflated ) )    
  
     
  
  xxx =  ye_temp
  xxx[ abs(year_idx_numeric - ii)>2.1 ] = NA
  pdataset = list( y=xxx, begin_idx =firm_TS_data$firm_begin_deflated, end_idx = firm_TS_data$firm_end_deflated)
  FEsim = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
  
  m1=arima( xxx  , order=c(1,0,0), method = c("ML"),optim.control = list(maxit = 300000 )) 
  FErho_year_pooled[ii-1984] = m1$coef[1]   
  FErho_year_pooled_SE[ii-1984] =  sqrt(m1$var.coef[1])  
  
  print(ii)
}

plot(FErho_year_pooled,type="b")
plot(FErho_year_pooled_SE,type="b")

# plot in the paper
par(mfrow=c(1,2))
# year effect
ssd = 1.96
xxx = mean_rho.year 
CIlow =  xxx -SE_year*ssd   
CIhigh =  xxx+SE_year*ssd  
xaxis.year = 1985:2013
plot(NA,xlim=c(xaxis.year[1], xaxis.year[length(xaxis.year)]),  ylim=c(-0.2,0.2),main="Year effect on Autocorr(FE)", xlab="Year",ylab="")
polygon( c(xaxis.year, xaxis.year[length(xaxis.year):1]  ),  
         c(CIlow, CIhigh[length(xaxis.year):1]), border = NA, col = "gray")
lines(xaxis.year,xxx, cex=0.5, lwd=1  ,type="l" )
points(xaxis.year,xxx, cex=0.5, lwd=1    )
abline(h=0, lty="dashed"  )


# Mean of firmlevel autocorr(FE) biase adjusted = 0.203

plot(summary(lm(FErhoadj ~ -1+ factor( as.numeric(year_idx_numeric[est$firm_end_deflated])+as.numeric(year_idx_numeric[est$firm_begin_deflated]))))$coef[,1])









m3.FE.1 = lmer(FE_temp_pooled~ -1 + age_dummy_pooled + year_dummy_pooled  + FE_temp_pooled_lag1:year_dummy_pooled +
     FE_temp_pooled_lag1:age_dummy_pooled + FE_temp_pooled_lag1 + (FE_temp_pooled_lag1|firm_dummy_pooled)    , REML = FALSE)
m3.FE.2 = lmer(FE_temp_pooled~ -1 +  year_dummy_pooled  + FE_temp_pooled_lag1:year_dummy_pooled + FE_temp_pooled_lag1:age_dummy_pooled + FE_temp_pooled_lag1 + (FE_temp_pooled_lag1|firm_dummy_pooled)    , REML = FALSE)
m3.FE=list(m3.FE.1=m3.FE.1, m3.FE.2=m3.FE.2)
save(m3.FE, file="FErho_learn_result_all.Rdata")


############## Plot of learning ############
load("FErho_learn_result_all.Rdata")
# year
#plot(1985:2013, c(0,fixef(m3.FE$m3.FE.1)[58:85] ),type="b")
#mean_year = fixef(m3.FE$m3.FE.1)[58:84]-mean(fixef(m3.FE$m3.FE.1)[58:84])
#SE_year = sqrt(diag(matrix((vcov(m3.FE$m3.FE.1)[58:84,58:84]),ncol=27)))
#plot(1986:2012, c( fixef(m3.FE$m3.FE.1)[58:84]+ SE_year*2 ),type="b")
#plot(1986:2012, c( fixef(m3.FE$m3.FE.1)[58:84]- SE_year*2 ),type="b")
#ts.plot(c(0,fixef(m3.FE$m3.FE.1)[c(86:112)] )) 


 


# year & # age
Reg.rho.age =  lm(FE_temp_pooled~ -1 +  year_dummy_pooled +  age_dummy_pooled +   FE_temp_pooled_lag1 + FE_temp_pooled_lag1:year_dummy_pooled + FE_temp_pooled_lag1:age_dummy_pooled  )
mean_rho.year = c(0,Reg.rho.age$coef[c(58:85)])-mean( c(0,Reg.rho.age$coef[c(58:85)]) )
mean_rho.age = c(0,Reg.rho.age$coef[c(86:112)])-mean( c(0,Reg.rho.age$coef[c(86:112)]) )
#SE_age = sqrt(diag(matrix((vcov(m3.FE$m3.FE.1)[c(57,86:112),c(57,86:112)]),ncol=27)))
SE_rho.age = rep(NA,28)
SE_rho.year = rep(NA,28)                                                
betacov.year =   vcov(Reg.rho.age)[c(57,58:85),c(57,58:85)]  
betacov.age =   vcov(Reg.rho.age)[c(57,86:112),c(57,86:112)]  
for (ii in c(1:28)) { 
  a1 = rep(0,27)   
  if (ii>1){
    a1[ii-1]=1 
  }
  A = matrix(c(1,   a1 ),nrow=1)
  SE_age[ii] = sqrt(A%*%betacov.age%*%t(A)) 
}
                                                      
for (ii in c(1:29)) { 
  a1 = rep(0,28)   
  if (ii>1){
    a1[ii-1]=1 
  }
  A = matrix(c(1,   a1 ),nrow=1)
  SE_year[ii] = sqrt(A%*%betacov.year%*%t(A)) 
}

                                                        
                                                      
# plot in the paper
par(mfrow=c(1,2))
# year effect
ssd = 1.96
xxx = mean_rho.year 
CIlow =  xxx -SE_year*ssd   
CIhigh =  xxx+SE_year*ssd  
xaxis.year = 1985:2013
plot(NA,xlim=c(xaxis.year[1], xaxis.year[length(xaxis.year)]),  ylim=c(-0.2,0.2),main="Year effect on Autocorr(FE)", xlab="Year",ylab="")
polygon( c(xaxis.year, xaxis.year[length(xaxis.year):1]  ),  
         c(CIlow, CIhigh[length(xaxis.year):1]), border = NA, col = "gray")
lines(xaxis.year,xxx, cex=0.5, lwd=1  ,type="l" )
points(xaxis.year,xxx, cex=0.5, lwd=1    )
abline(h=0, lty="dashed"  )

# age effect
ssd = 1.96
xxx = mean_rho.age 
CIlow =  xxx -SE_age*ssd   
CIhigh =  xxx+SE_age*ssd                                                        
xaxis.age = 1:length(xxx)
                                                      
plot(NA,xlim=c(xaxis.age[1], xaxis.age[length(xaxis.age)]),  ylim=c(-0.2,0.2),main="Age effect on Autocorr(FE)", xlab="Age",ylab="")
polygon( c(xaxis.age, xaxis.age[length(xaxis.age):1]  ),  
         c(CIlow, CIhigh[length(xaxis.age):1]), border = NA, col = "gray")
lines(xaxis.age,xxx, cex=0.5, lwd=1  ,type="l" )
points(xaxis.age,xxx, cex=0.5, lwd=1    )
abline(h=0, lty="dashed"  )












############################################
######### learn phi ##############


# SD(phi-phihat )>0, some firm phi>phihat, some firm opposite.
# So, analysts learn phi and have smaller |phi-phihat|?
# now rho(FE) doesn't matter. we just want to know whether analysts learn.



# |phi-phhat| decreases over time in a firm? measure SD(phi-phihat) in 2 subperiods                                                   
# go more detailed with 

# |phi-phhat| decreases over time overall?  


temp_TS_index = firm_TS_data$TS_index_firm_deflated
temp_TS_index[NA_idx_temp] = NA 
firsthalf_dummy =rep(NA, length(est$ye_firm_deflated ))  
secondhalf_dummy=rep(NA, length(est$ye_firm_deflated ))  
no_firmobs =rep(NA, length(est$ye_firm_deflated ))  

for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
{  
  if (!is.na(est$firm_begin_deflated[jj])) { 
    firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj] 
    firsthalf_dummy[firm_idx_temp] = temp_TS_index[firm_idx_temp] <= median(temp_TS_index[firm_idx_temp] ,na.rm=T ) 
    secondhalf_dummy[firm_idx_temp] = temp_TS_index[firm_idx_temp] >= median(temp_TS_index[firm_idx_temp] ,na.rm=T ) 
    no_firmobs[firm_idx_temp] = est$no_obs_deflated[jj]
  } 
}
#firsthalf_dummy[is.na(firsthalf_dummy)]=FALSE
#secondhalf_dummy[is.na(secondhalf_dummy)]=FALSE

pdataset = list(y=firsthalf_dummy, begin_idx =est$firm_begin_deflated, end_idx = est$firm_end_deflated)
firsthalf_dummy_pooled = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
firsthalf_dummy_pooled = factor(firsthalf_dummy_pooled)

pdataset = list(y=secondhalf_dummy, begin_idx =est$firm_begin_deflated, end_idx = est$firm_end_deflated)
secondhalf_dummy_pooled = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
secondhalf_dummy_pooled = factor(secondhalf_dummy_pooled)


pdataset = list(y=no_firmobs , begin_idx =est$firm_begin_deflated, end_idx = est$firm_end_deflated)
no_firmobs_pooled = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )




### with age dummy
Reg1 = lm(ye_temp_pooled~   -1+ year_dummy_pooled  + age_dummy_pooled + ye_temp_pooled_lag1+  year_dummy_pooled:ye_temp_pooled_lag1   )
Reg2 = lm(yf_temp_pooled~   -1 + year_dummy_pooled  + age_dummy_pooled + ye_temp_pooled_lag1+  year_dummy_pooled:ye_temp_pooled_lag1 +yf_temp_pooled_lag1+  year_dummy_pooled:yf_temp_pooled_lag1    )
# Reg1 + Reg2 = Reg3
Reg3 = lm(FE_temp_pooled~   -1 + year_dummy_pooled  + age_dummy_pooled + ye_temp_pooled_lag1+  year_dummy_pooled:ye_temp_pooled_lag1 +yf_temp_pooled_lag1+  year_dummy_pooled:yf_temp_pooled_lag1    )

ssd = 1.96
phi_year_TS =  c(0,Reg1$coef[(31:58)+27])+Reg1$coef[30+27]
phiphihat_year_TS = c(c(0,Reg3$coef[ (31:58)+28] )+Reg3$coef[29+28]) + c(c(0,Reg3$coef[(59:86)+28])+Reg3$coef[30+28])
mean(phi_year_TS)
mean(phiphihat_year_TS)
plot(phiphihat_year_TS)

CIhigh.phi = rep(NA,29)
CIlow.phi =  rep(NA,29)
phi_year_TS =  rep(NA,29)
SE_phi =  rep(NA,29)
betacov.phi =  vcov(Reg1)[57:85,57:85]
betavec.phi =  matrix(Reg1$coef[57:85],ncol=1)
for (ii in c(1:29)) {
  
  a1 = rep(0,28) 
  if (ii>1){
    a1[ii-1]=1 
  }
  A = matrix(c(1,  a1 ),nrow=1)
  SE=sqrt(A%*%betacov.phi%*%t(A))
  SE_phi[ii] = SE
  #SE= summary(Reg1)$coef[ii+56,2]
  CIhigh.phi[ii] = A%*%betavec.phi +ssd*SE
  CIlow.phi[ii] = A%*%betavec.phi-ssd*SE
  phi_year_TS[ii] = A%*%betavec.phi
}


CIhigh = rep(NA,29)
CIlow =  rep(NA,29)
phiphihat_year_TS =  rep(NA,29)
betacov =  vcov(Reg3)[57:114,57:114]
betavec =  matrix(Reg3$coef[57:114],ncol=1)
for (ii in c(1:29)) {
  
  a1 = rep(0,28)
  a2 = rep(0,28)
  if (ii>1){
    a1[ii-1]=1
    a2[ii-1]=1
  }
  A = matrix(c(1, 1, a1,a2),nrow=1)
  SE=sqrt(A%*%betacov%*%t(A))
  CIhigh[ii] = A%*%betavec +ssd*SE
  CIlow[ii] = A%*%betavec-ssd*SE
  phiphihat_year_TS[ii] = A%*%betavec
}

#### phi learn plot
par(mfrow=c(1,2))

xxx = phi_year_TS
xaxis.year = 1985:2013
plot(NA,xlim=c(xaxis.year[1], xaxis.year[length(xaxis.year)]) ,ylim=c(0.1,0.7))
polygon( c(xaxis.year, xaxis.year[length(xaxis.year):1]  ),  
         c(CIlow.phi, CIhigh.phi[length(xaxis.year):1]), border = NA, col = "gray")
lines(xaxis.year,xxx, cex=0.5, lwd=1  ,type="l" )
points(xaxis.year,xxx, cex=0.5, lwd=1    ) 


xxx = phiphihat_year_TS
xaxis.year = 1985:2013
plot(NA,xlim=c(xaxis.year[1], xaxis.year[length(xaxis.year)]) ,ylim=c(-0.1,0.1))
polygon( c(xaxis.year, xaxis.year[length(xaxis.year):1]  ),  
         c(CIlow, CIhigh[length(xaxis.year):1]), border = NA, col = "gray")
lines(xaxis.year,xxx, cex=0.5, lwd=1  ,type="l" )
points(xaxis.year,xxx, cex=0.5, lwd=1    )
abline(h=0, lty="dashed")


  
summary(lm(FE_temp_pooled~ -1 +  year_dummy_pooled  + FE_temp_pooled_lag1:year_dummy_pooled + FE_temp_pooled_lag1:age_dummy_pooled + FE_temp_pooled_lag1 ))$coef[,2]
summary(lm(ye_temp_pooled~ -1 +  year_dummy_pooled  + ye_temp_pooled_lag1:year_dummy_pooled + ye_temp_pooled_lag1:age_dummy_pooled + ye_temp_pooled_lag1 ))$coef[,2]
vcov(lm(ye_temp_pooled~ -1 +  year_dummy_pooled  + ye_temp_pooled_lag1:year_dummy_pooled + ye_temp_pooled_lag1:age_dummy_pooled + ye_temp_pooled_lag1 ))[1:2,1:2]

                                                      

summary(lm(FE_temp_pooled~ -1 +  year_dummy_pooled +  firsthalf_dummy_pooled +   FE_temp_pooled_lag1 + FE_temp_pooled_lag1:year_dummy_pooled + FE_temp_pooled_lag1:firsthalf_dummy_pooled +  + FE_temp_pooled_lag1:firsthalf_dummy_pooled:no_firmobs_pooled  ))
summary(lm(FE_temp_pooled~ -1 +  year_dummy_pooled +  firsthalf_dummy_pooled +   FE_temp_pooled_lag1 + FE_temp_pooled_lag1:year_dummy_pooled + FE_temp_pooled_lag1:firsthalf_dummy_pooled  ))
summary(lm(FE_temp_pooled~ -1 +  year_dummy_pooled +  secondhalf_dummy_pooled +   FE_temp_pooled_lag1 + FE_temp_pooled_lag1:year_dummy_pooled + FE_temp_pooled_lag1:secondhalf_dummy_pooled  ))


for (oo in c(20,30,40,50,60,70,80,90,100,110,120)) { 
  print(summary(lm(FE_temp_pooled~  -1+firsthalf_dummy_pooled+  FE_temp_pooled_lag1  + FE_temp_pooled_lag1:firsthalf_dummy_pooled, subset= (no_firmobs_pooled>oo & no_firmobs_pooled<oo+10) )  ))}
# ACF(FE) decreases over time in a firm?
                                                      
                                                      
for (oo in c( 20,30,40,50,60,70,80,90,100,110,120)) { 
  print( c(arima(  yef_temp[ (no_firmobs_pooled >  oo & no_firmobs_pooled < oo + 10) & firsthalf_dummy_pooled==TRUE ], c(1,0,0))$coef[1],arima(  yef_temp[ (no_firmobs_pooled >  oo & no_firmobs_pooled < oo + 10) & firsthalf_dummy_pooled==FALSE ], c(1,0,0))$coef[1])  )}
# ACF(FE) decreases over time in a firm?
                                              
                                                      
for (oo in c(20,30,40,50,60,70,80,90,100,110,120)) { 
  print(summary(lm(FE_temp_pooled~    ye_temp_pooled_lag1  + ye_temp_pooled_lag1:firsthalf_dummy_pooled, subset= (no_firmobs_pooled>oo & no_firmobs_pooled<oo+10) )  ))}

 

                    

m2.FE=lmer(FE_temp_pooled~ -1 + age_dummy_pooled + year_dummy_pooled  +   ye_temp_pooled_lag1:year_dummy_pooled +
ye_temp_pooled_lag1:age_dummy_pooled + ye_temp_pooled_lag1 + (ye_temp_pooled_lag1|firm_dummy_pooled)    , REML = FALSE)
                                                      

































### without any dummies

Reg1 = lm(ye_temp_pooled~    ye_temp_pooled_lag1+  year_dummy_pooled:ye_temp_pooled_lag1   )
Reg2 = lm(yf_temp_pooled~      ye_temp_pooled_lag1+  year_dummy_pooled:ye_temp_pooled_lag1 +yf_temp_pooled_lag1+  year_dummy_pooled:yf_temp_pooled_lag1    )

phi_year_TS =  c(0,Reg1$coef[(31:58)-28])+Reg1$coef[30-28]
phihat_year_TS = c(c(0,Reg2$coef[ (31:58)-27] )+Reg2$coef[29-27]) + c(c(0,Reg2$coef[(59:86)-27])+Reg2$coef[30-27])
mean(phi_year_TS)
mean(phihat_year_TS)
ts.plot(cbind(phi_year_TS,phihat_year_TS))

### without age dummy
Reg1 = lm(ye_temp_pooled~   -1+ year_dummy_pooled  + ye_temp_pooled_lag1+  year_dummy_pooled:ye_temp_pooled_lag1    )
Reg2 = lm(yf_temp_pooled~   -1 + year_dummy_pooled  + ye_temp_pooled_lag1+  year_dummy_pooled:ye_temp_pooled_lag1 +yf_temp_pooled_lag1+  year_dummy_pooled:yf_temp_pooled_lag1    )

phi_year_TS =  c(0,Reg1$coef[31:58])+Reg1$coef[30]
phihat_year_TS = c(c(0,Reg2$coef[32:59])+Reg2$coef[30]) + c(c(0,Reg2$coef[60:87])+Reg2$coef[31])
mean(phi_year_TS)
mean(phihat_year_TS)

ts.plot(cbind(phi_year_TS,phihat_year_TS))










load("MixedModel_phiphihat_Lmer.Rdata") 

par(mfrow=c(1,2))  
plot(1985:2013,phi_year_TS, 
     type="l",lwd=6, col="gray60",ylim=c(0.15,0.6),xlab="Year",main="Without Random Firm Effect",
     ylab=expression(paste("Autoregressive coef. (",phi,", ",hat(phi),")")))
lines(1985:2013,  phihat_year_TS , type="l")
plot(1985:2013,c(0,fixef(m2fit$m2.ye)[58:85])+fixef(m2fit$m2.ye)[57], 
     type="l",lwd=6, col="gray60",ylim=c(0.15,0.6),xlab="Year",main="With Random Firm Effect",
     ylab="")
lines(1985:2013,  c(0,fixef(m2fit$m2.yf)[58:85])+fixef(m2fit$m2.yf)[57]+0.017 , type="l")


#postscript("whatever.eps")
# setEPS()
# postscript("whatever.eps")
# plot(rnorm(100), main="Hey Some Data")
# dev.off()

# year
plot(fixef(m3.FE$m3.FE.2)[31:58] )
SE_year = sqrt(diag(matrix((vcov(m3.FE$m3.FE.2)[31:58,31:58]),ncol=28)))
# age
plot(fixef(m3.FE$m3.FE.2)[59:95] )



 




 



library(nlme)
lme(FE_temp_pooled~ -1 + age_dummy_pooled + year_dummy_pooled +
    FE_temp_pooled_lag1, random=  ~ FE_temp_pooled_lag1|firm_dummy_pooled, na.action=na.omit    )


lmer(FE_temp_pooled~ -1 +  year_dummy_pooled    + FE_temp_pooled_lag1 + (FE_temp_pooled_lag1|firm_dummy_pooled)    , REML = FALSE)


plot(lm(FE_temp_pooled~   year_dummy_pooled    + FE_temp_pooled_lag1+  year_dummy_pooled:FE_temp_pooled_lag1 + age_dummy_pooled:FE_temp_pooled_lag1   )$coef)


m4 = lmer(FE_temp_pooled~ -1 + year_dummy_pooled  + age_dummy_pooled + ye_temp_pooled_lag1+  year_dummy_pooled:ye_temp_pooled_lag1 +yf_temp_pooled_lag1+  year_dummy_pooled:yf_temp_pooled_lag1 + (FE_temp_pooled_lag1|firm_dummy_pooled)    , REML = FALSE)

Reg3 = lm(FE_temp_pooled~   -1 + year_dummy_pooled  + age_dummy_pooled + ye_temp_pooled_lag1+  year_dummy_pooled:ye_temp_pooled_lag1 +yf_temp_pooled_lag1+  year_dummy_pooled:yf_temp_pooled_lag1    )
























################## simul: firm effect on phi phihat

 

fcn_AR1_coef_estimate_pooled_heterophiphihat2<- function(  KFfit,  COV_phiphihat  ) { 
  
  FEsim = rep(NA, length(firm_TS_data$ye_firm_deflated) )  
  
  for (jj in seq( 1 , length(firm_TS_data$firm_begin_deflated)))
  {      
    firm_idx_temp = firm_TS_data$firm_begin_deflated[jj]:firm_TS_data$firm_end_deflated[jj]     
    Tsim = length(firm_idx_temp) + 50   
    ytsim1= rep(NA, Tsim )
    ytsim2= rep(NA, Tsim )
    
    xtsim0 = 0
    ytsim1[1] = xtsim0 + rnorm(1)
    ytsim2[1] = 0 
    
    e_phi = t(chol(COV_phiphihat))%*%matrix(rnorm(2),ncol=1)
    
    phi_i = KFfit$par["phi"] + e_phi[1]
    phihat_i = KFfit$par["phihat"] + e_phi[2]
    
    for (ii  in seq(2, Tsim ))
    {          
      et_sim = rnorm(1)*exp(KFfit$par["logsigmae"])
      at_sim = rnorm(1)*exp(KFfit$par["logsigmaa"])
      nt_sim = rnorm(1)*exp(KFfit$par["logsigman"])
      
      xtsim1     = min(0.99, phi_i  )*xtsim0 + et_sim  
      ytsim1[ii] = xtsim1 + at_sim  
      ytsim2[ii] = phihat_i*(KFfit$par["Khat"]*ytsim1[ii-1] +
                                          (1-KFfit$par["Khat"])*ytsim2[ii-1] ) + KFfit$par["what"]*(et_sim + nt_sim)
      xtsim0=xtsim1
    }
    FEsim[firm_idx_temp] = (ytsim1 - ytsim2 )[51:Tsim]    
  } 
  
  FEsim[NA_idx_temp] = NA  
  pdataset = list( y=FEsim,begin_idx =firm_TS_data$firm_begin_deflated, end_idx = firm_TS_data$firm_end_deflated)
  FEsim = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
  m1 = arima( FEsim , order=c(1,0,0), method = c("ML"),optim.control = list(maxit = 300000 ))  
  return(m1$coef[1])  
  
}



#####  #####
NN = 1000

sigma_alpha_set = c(0,0.1,0.15,0.2,0.3)
sigma_alpha_input = sigma_alpha_set[1]
set.seed( 12345 ) 

load("KFfit_VARMA_MLE.Rdata")
KFfit$par["logsigmaa"] =log(sigma_alpha_input)  
KFfit$par["logsigmae"]   = log(1)
KFfit$par["logsigman"] = log(0.5)
KFfit$par["mue"]    = 0    
KFfit$par["muehat"] = 0         
KFfit$par["phi"]  = 0.47   
KFfit$par["phihat"]   = 0.47 
Implist = fcn_implied_K_w_rho( matrix(as.numeric(KFfit$par)[3:9], ncol=1) )
KFfit$par["Khat"] =    Implist$K
if (KFfit$par["logsigmaa"]  ==  -Inf) KFfit$par["Khat"] = 1
KFfit$par["what"] =    Implist$w 

NA_idx_temp = is.na(  yef_temp ) 
FErhopooled_fitted_sim_heterophiphihat = rep(NA, NN )

for (ii in seq(  1 , NN ) ) {  
  FErhopooled_fitted_sim_heterophiphihat[ii]  = fcn_AR1_coef_estimate_pooled_heterophiphihat2( KFfit, COV_phiphihat )
  if ( (ii %% 10) == 0 ) {
    FErhopooled_fitted_sim_heterophiphihat_result =list(KFfit=KFfit, COV_phiphihat=COV_phiphihat,
                                                        FErhopooled_fitted_sim_heterophiphihat=FErhopooled_fitted_sim_heterophiphihat)
    save(FErhopooled_fitted_sim_heterophiphihat_result, file=sprintf("FErhopooled_fitted_sim_heterophiphihat_result_%d.Rdata",sigma_alpha_input*100 ))
  }
  print(ii)
} 
























## 1) calendar-time variation...(a) simple rolling window (phi,phihat/different lags) (b) quarterly (more missing values)


## 2) age-time variation......(a) simple rolling window (phi,phihat/different lags) (b) quarterly(more missing vlue)


## 3) C-S variation....(a) by var(phi-phihat)...(b) by estimation of random effect model



### mixed effect model estimation
# 
library(nlme)
year_idx_numeric = floor(firm_TS_data$year_qtr_idx-0.2)
age_idx_numeric =  1+floor(firm_TS_data$TS_index_firm_deflated/4-0.2)
firm_idx_numeric = est$firm_deflated_nn_idx

year_dummy=  factor(year_idx_numeric)
age_dummy=  factor(age_idx_numeric)
firm_dummy=  factor(firm_idx_numeric)

FE_temp = c(firm_TS_data$ye_firm_deflated-firm_TS_data$yf_firm_deflated)
pdataset = list(y=FE_temp, begin_idx =est$firm_begin_deflated, end_idx = est$firm_end_deflated)
FE_temp_pooled = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )

ye_temp =  firm_TS_data$ye_firm_deflated 
pdataset = list(y=ye_temp, begin_idx =est$firm_begin_deflated, end_idx = est$firm_end_deflated)
ye_temp_pooled = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
ye_temp_pooled_lag1 = c(NA,ye_temp_pooled[1:(length(ye_temp_pooled )-1)] )

yf_temp =  firm_TS_data$yf_firm_deflated 
pdataset = list(y=yf_temp, begin_idx =est$firm_begin_deflated, end_idx = est$firm_end_deflated)
yf_temp_pooled = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )



pdataset = list(y=year_idx_numeric, begin_idx =est$firm_begin_deflated, end_idx = est$firm_end_deflated)
year_dummy_pooled = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
year_dummy_pooled = factor(year_dummy_pooled)

pdataset = list(y=age_idx_numeric, begin_idx =est$firm_begin_deflated, end_idx = est$firm_end_deflated)
age_dummy_pooled = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
age_dummy_pooled = factor(age_dummy_pooled)

pdataset = list(y=firm_idx_numeric, begin_idx =est$firm_begin_deflated, end_idx = est$firm_end_deflated)
firm_dummy_pooled = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
firm_dummy_pooled = factor(firm_dummy_pooled)
 
lme( FE_temp_pooled  ~ ye_temp_pooled_lag1 ,random=  ~ ye_temp_pooled_lag1-1 |  year_dummy_pooled     ,na.action=na.omit  )
lme( FE_temp_pooled  ~ ye_temp_pooled_lag1 ,random=  ~ ye_temp_pooled_lag1-1 |  age_dummy_pooled     ,na.action=na.omit  )
lme( FE_temp_pooled  ~ ye_temp_pooled_lag1 ,random=  ~ ye_temp_pooled_lag1-1 |  firm_dummy_pooled     ,na.action=na.omit  )

lme( FE_temp_pooled  ~ ye_temp_pooled_lag1 ,random=  list( firm_dummy_pooled=~1,  firm_dummy_pooled = ~ ye_temp_pooled_lag1-1)        ,na.action=na.omit  )
m1

m1=lme( FE_temp_pooled  ~ year_dummy_pooled + age_dummy_pooled + ye_temp_pooled_lag1 ,random=  list( firm_dummy_pooled=~1,  firm_dummy_pooled = ~ ye_temp_pooled_lag1-1)        ,na.action=na.omit  )
m1

0.09318239

m2=lme( FE_temp_pooled  ~ year_dummy_pooled:ye_temp_pooled_lag1 + ye_temp_pooled_lag1 ,random=  list( firm_dummy_pooled=~1,  firm_dummy_pooled = ~ ye_temp_pooled_lag1-1)        ,na.action=na.omit  )
sd(m2$coef$fixed[3:length(m2$coef$fixed)])

m2= lmer(FE_temp_pooled~ ye_temp_pooled_lag1 + (ye_temp_pooled_lag1 |firm_dummy_pooled)  + (1|firm_dummy_pooled) )


lmer(FE_temp_pooled~ year_dummy_pooled + age_dummy_pooled + ye_temp_pooled_lag1     + (1|firm_dummy_pooled) + (-1+ye_temp_pooled_lag1  |year_dummy_pooled) , REML  FALSE)
lmer(FE_temp_pooled~ year_dummy_pooled + age_dummy_pooled + ye_temp_pooled_lag1  +  ye_temp_pooled_lag1:age_dummy_pooled  + (1|firm_dummy_pooled)  , REML  FALSE)
lmer(FE_temp_pooled~ year_dummy_pooled + age_dummy_pooled + ye_temp_pooled_lag1 + (ye_temp_pooled_lag1 |firm_dummy_pooled)   , REML  FALSE)



m2=lmer(FE_temp_pooled~ -1 + age_dummy_pooled + year_dummy_pooled  +   ye_temp_pooled_lag1:year_dummy_pooled +
       ye_temp_pooled_lag1:age_dummy_pooled + ye_temp_pooled_lag1 + (ye_temp_pooled_lag1|firm_dummy_pooled)    , REML = FALSE)
age_effect = m2$coef$fixed[1:28] 
year_effect = c( 0, m2$coef$fixed[29:56] )
firm_effect = m2$coef$random$firm_dummy
SD_err = m2$sigma 










fcn_AR1_coef_estimate_pooled_heterophiphihat<- function(  KFfit, SD_phiphihat  ) { 
  
  FEsim = rep(NA, length(firm_TS_data$ye_firm_deflated) )
    
  for (jj in seq( 1 , length(firm_TS_data$firm_begin_deflated)))
  {      
    firm_idx_temp = firm_TS_data$firm_begin_deflated[jj]:firm_TS_data$firm_end_deflated[jj]     
    Tsim = length(firm_idx_temp) + 50   
    ytsim1= rep(NA, Tsim )
    ytsim2= rep(NA, Tsim )
    
    xtsim0 = 0
    ytsim1[1] = xtsim0 + rnorm(1)
    ytsim2[1] = 0 
    
    erphi = rnorm(1)*SD_phiphihat
    
    for (ii  in seq(2, Tsim ))
    {          
      et_sim = rnorm(1)*exp(KFfit$par["logsigmae"])
      at_sim = rnorm(1)*exp(KFfit$par["logsigmaa"])
      nt_sim = rnorm(1)*exp(KFfit$par["logsigman"])
      
      xtsim1     = min(0.99, KFfit$par["phi"] + erphi )*xtsim0 + et_sim  
      ytsim1[ii] = xtsim1 + at_sim  
      ytsim2[ii] = KFfit$par["phihat"]*(KFfit$par["Khat"]*ytsim1[ii-1] +
                 (1-KFfit$par["Khat"])*ytsim2[ii-1] ) + KFfit$par["what"]*(et_sim + nt_sim)
      xtsim0=xtsim1
    }
    FEsim[firm_idx_temp] = (ytsim1 - ytsim2 )[51:Tsim]    
  } 
  
  FEsim[NA_idx_temp] = NA  
  pdataset = list( y=FEsim,begin_idx =firm_TS_data$firm_begin_deflated, end_idx = firm_TS_data$firm_end_deflated)
  FEsim = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
  m1 = arima( FEsim , order=c(1,0,0), method = c("ML"),optim.control = list(maxit = 300000 ))  
  return(m1$coef[1])  
   
}



#####  #####
NN = 100


load("KFfit_VARMA_MLE.Rdata")
KFfit$par["mue"]    = 0    
KFfit$par["muehat"] = 0         
KFfit$par["phi"]  = 0.5    
KFfit$par["phihat"]   = 0.5     
KFfit$par["logsigmaa"] =log(0.01)  
KFfit$par["logsigmae"]   = log(1)
KFfit$par["logsigman"] = log(0.5)
Implist = fcn_implied_K_w_rho( matrix(as.numeric(KFfit$par)[3:9], ncol=1) )
KFfit$par["Khat"] =    Implist$K
KFfit$par["what"] =    Implist$w 

NA_idx_temp = is.na(  yef_temp )
SD_phiphihat=0.09318239
FErhopooled_fitted_sim_heterophiphihat = rep(NA, NN )

for (ii in seq(  1 , NN ) ) {  
  FErhopooled_fitted_sim_heterophiphihat[ii]  = fcn_AR1_coef_estimate_pooled_heterophiphihat( KFfit, SD_phiphihat )
  if ( (ii %% 10) == 0 ) {
    FErhopooled_fitted_sim_heterophiphihat_result =list(KFfit=KFfit, SD_phiphihat=SD_phiphihat,
                                      FErhopooled_fitted_sim_heterophiphihat=FErhopooled_fitted_sim_heterophiphihat)
    save(FErhopooled_fitted_sim_heterophiphihat_result, file="FErhopooled_fitted_sim_heterophiphihat_result_01.Rdata")
  }
  print(ii)
} 

#####  #####
NN = 100
load("KFfit_VARMA_MLE.Rdata")
KFfit$par["mue"]    = 0    
KFfit$par["muehat"] = 0         
KFfit$par["phi"]  = 0.5    
KFfit$par["phihat"]   = 0.5     
KFfit$par["logsigmaa"] =log(0.10)  
KFfit$par["logsigmae"]   = log(1)
KFfit$par["logsigman"] = log(0.5)
Implist = fcn_implied_K_w_rho( matrix(as.numeric(KFfit$par)[3:9], ncol=1) )
KFfit$par["Khat"] =    Implist$K
KFfit$par["what"] =    Implist$w 

NA_idx_temp = is.na(  yef_temp )
SD_phiphihat=0.09318239
FErhopooled_fitted_sim_heterophiphihat = rep(NA, NN )

for (ii in seq(  1 , NN ) ) {  
  FErhopooled_fitted_sim_heterophiphihat[ii]  = fcn_AR1_coef_estimate_pooled_heterophiphihat( KFfit, SD_phiphihat )
  if ( (ii %% 10) == 0 ) {
    FErhopooled_fitted_sim_heterophiphihat_result =list(KFfit=KFfit,SD_phiphihat=SD_phiphihat,
                                                        FErhopooled_fitted_sim_heterophiphihat=FErhopooled_fitted_sim_heterophiphihat)
    save(FErhopooled_fitted_sim_heterophiphihat_result, file="FErhopooled_fitted_sim_heterophiphihat_result_10.Rdata")
  }
  print(ii)
} 

#####  #####
NN = 100
load("KFfit_VARMA_MLE.Rdata")
KFfit$par["mue"]    = 0    
KFfit$par["muehat"] = 0         
KFfit$par["phi"]  = 0.5    
KFfit$par["phihat"]   = 0.5     
KFfit$par["logsigmaa"] =log(0.20)  
KFfit$par["logsigmae"]   = log(1)
KFfit$par["logsigman"] = log(0.5)
Implist = fcn_implied_K_w_rho( matrix(as.numeric(KFfit$par)[3:9], ncol=1) )
KFfit$par["Khat"] =    Implist$K
KFfit$par["what"] =    Implist$w 

NA_idx_temp = is.na(  yef_temp )
SD_phiphihat=0.09318239
FErhopooled_fitted_sim_heterophiphihat = rep(NA, NN )

for (ii in seq(  1 , NN ) ) {  
  FErhopooled_fitted_sim_heterophiphihat[ii]  = fcn_AR1_coef_estimate_pooled_heterophiphihat( KFfit, SD_phiphihat )
  if ( (ii %% 10) == 0 ) {
    FErhopooled_fitted_sim_heterophiphihat_result =list(KFfit=KFfit,SD_phiphihat=SD_phiphihat,
                                                        FErhopooled_fitted_sim_heterophiphihat=FErhopooled_fitted_sim_heterophiphihat)
    save(FErhopooled_fitted_sim_heterophiphihat_result, file="FErhopooled_fitted_sim_heterophiphihat_result_20.Rdata")
  }
  print(ii)
} 








 


m1 = lm( FE_temp_pooled  ~   ye_temp_pooled_lag1       )


m1 = lme( FE_temp  ~ -1 + age_dummy + year_dummy + ye_temp_pooled_lag1 ,random=  ~1 | firm_dummy   ,na.action=na.omit  )
age_effect = m1$coef$fixed[1:28] 
year_effect = c( 0, m1$coef$fixed[29:56] )
firm_effect = m1$coef$random$firm_dummy
SD_err = m1$sigma 


## 4) GENERAL: firm-wise time variation
# KF estimation of AR(1) model


##################################################################################
#######  III. Any learning in the static sense (learning fixed parameters)? #######
##################################################################################

## 1) autocorr(FE) decreases over age-time


## 2) autocorr(FE) decreases over calendar-time







## 3) CS variance of biases should decreases over age in any cases of b, 
#in terms of actual or quantile. Divide the whole period of each firm into two.
#(random effect model)









##################################################################



    ### average of firm-level rho(FE)    
    tryCatch({
      FErhofirmbiasadj_fitted_sim.all[ii]  = fcn_AR1_coef_estimate_avg_firm_biasadj(zzz.all)
      FErhofirmbiasadj_fitted_sim.age[ii]  = fcn_AR1_coef_estimate_avg_firm_biasadj(zzz.age)
      FErhofirmbiasadj_fitted_sim.year[ii] = fcn_AR1_coef_estimate_avg_firm_biasadj(zzz.year)
      FErhofirmbiasadj_fitted_sim.firm[ii] = fcn_AR1_coef_estimate_avg_firm_biasadj(zzz.firm)       
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
                         erroroccur = TRUE} )
          










FErhoadj[FErhoadj>1] = 1
FErhoadj[FErhoadj< -1] = -1
mean(FErhoadj)
median(FErhoadj)
hist(FErhoadj,30,main="Bias-adjusted 1st order autocorr(FE)",xlab="")
abline(v = FErhopooled, col = "blue", lwd = 2 )
abline(v = mean(FErhoadj), col = "red", lwd = 2)



#(a) by mean
FEbias = list()




require(Hmisc)
errbar(xaxis.year, avgFE , CIlow, CIhigh ,
       cap=0.005,   type='p' , cex=1,  lty=3, lwd=0.3 , 
       ylab="Avg(FE)", xlab="Year")


ts.plot( cbind(CIlow, x  ,  CIhigh ))
ts.plot( x/sqrt(mean(xxx[,3]^2)))
ts.plot( xxx[,1]/xxx[,3])
FEbias$cal.bymean = c(xxx[,1]/xxx[,3])

ggplot(data = df1, aes(x = hp, y = wt)) +
  scale_x_continuous("gross horsepower") +
  scale_y_continuous("weight (lb/1000)", limits = c(1,6)) +
  my.theme






axis(2)

group <- factor(sample(1:10,100,T))
y <- (1:10)[group] + rnorm(100)
grmean <- tapply(y,group,mean)
lims <- tapply(y,list(group),FUN = sd)*2
errbar( 1:10, grmean, grmean + lims, grmean - lims ,   type='b' )
 
axis(2)




#(b) by quantile
ssd = 2.58
xxx = mu_muhat_quaterly$yef_stat_qtr
x = xxx[,5]
CIlow = x-xxx[,6]*ssd/sqrt(xxx[,4])
CIhigh = x+xxx[,6]*ssd/sqrt(xxx[,4])
ts.plot( 1-cbind(CIlow, x  ,  CIhigh ))
ts.plot( qnorm(1-x))
FEbias$cal.byquant =  qnorm(1-x)
  


 



##############################################################################
## 2) age-time variation......(a) alone (b) cleandar-age at the same time


# (a) alone
load( "mu_muhat_quaterly_age.Rdata") 

ssd = 2.58
xxx = mu_muhat_quaterly_age$yef_stat_qtr
x = xxx[,1]
CIlow = x-xxx[,3]*ssd/sqrt(xxx[,4])
CIhigh = x+xxx[,3]*ssd/sqrt(xxx[,4])
ts.plot( cbind(CIlow, x  ,  CIhigh ))
ts.plot( x/sqrt(mean(xxx[,3]^2)))
ts.plot( xxx[,1]/xxx[,3])
FEbias$age.alonebymean = c(xxx[,1]/xxx[,3])
  
x = xxx[,5]
CIlow = x-xxx[,6]*ssd/sqrt(xxx[,4])
CIhigh = x+xxx[,6]*ssd/sqrt(xxx[,4])
ts.plot( 1-cbind(CIlow, x  ,  CIhigh ))
ts.plot( qnorm(1-x))
FEbias$age.alonebyquant = qnorm(1-x)






ts.plot( cbind(ye_stat_qtr_age[,2]-ye_stat_qtr_age[,3]*2.58/sqrt(ye_stat_qtr_age[,4]) , ye_stat_qtr_age[,2]+ye_stat_qtr_age[,3]*2.58/sqrt(ye_stat_qtr_age[,4])  ))
ts.plot( cbind(yf_stat_qtr_age[,2]-yf_stat_qtr_age[,3]*2.58/sqrt(yf_stat_qtr_age[,4]) , yf_stat_qtr_age[,2]+yf_stat_qtr_age[,3]*2.58/sqrt(yf_stat_qtr_age[,4])  ))
ts.plot( cbind(yef_stat_qtr_age[,1]-yef_stat_qtr_age[,3]*2.58/sqrt(yef_stat_qtr_age[,4]) , yef_stat_qtr_age[,1]+yef_stat_qtr_age[,3]*2.58/sqrt(yef_stat_qtr_age[,4])  ))
ts.plot( cbind(yef_stat_qtr_age[,2]-yef_stat_qtr_age[,3]*2.58/sqrt(yef_stat_qtr_age[,4]) , yef_stat_qtr_age[,2]+yef_stat_qtr_age[,3]*2.58/sqrt(yef_stat_qtr_age[,4])  ))
ts.plot( cbind(yef_stat_qtr_age[,5]-yef_stat_qtr_age[,6]*2.58/sqrt(yef_stat_qtr_age[,4]) , yef_stat_qtr_age[,5]+yef_stat_qtr_age[,6]*2.58/sqrt(yef_stat_qtr_age[,4])  ))    



plot(TS_row, rho_MA_reg_calendar_4$FE_rho_MA_qtr, type='b' )
plot(TS_row, rho_MA_reg_calendar_4$SE_FE_rho_MA_qtr, type='b' )
plot(TS_row, rho_MA_reg_calendar_4$phi_MA_qtr, type='b' )
plot(TS_row, rho_MA_reg_calendar_4$SE_phi_MA_qtr, type='b' )
plot(TS_row, rho_MA_reg_calendar_4$phihat_MA_qtr, type='b' )
plot(TS_row, rho_MA_reg_calendar_4$SE_phihat_MA_qtr, type='b' )
plot(TS_row, rho_MA_reg_calendar_4$phi_MA_qtr-rho_MA_reg_calendar_4$phihat_MA_qtr, type='b' )



plot(TS_row, rho_MA_reg_age_4$FE_rho_MA_qtr, type='b' )
plot(TS_row, rho_MA_reg_age_4$SE_FE_rho_MA_qtr, type='b' )
plot(TS_row, rho_MA_reg_age_4$phi_MA_qtr, type='b' )
plot(TS_row, rho_MA_reg_age_4$SE_phi_MA_qtr, type='b' )
plot(TS_row, rho_MA_reg_age_4$phihat_MA_qtr, type='b' )
plot(TS_row, rho_MA_reg_age_4$SE_phihat_MA_qtr, type='b' )



a=randn(1000000,1) + [0:(0.5/999999):0.5];corr(a(1:999999),a(2:1000000) )


#### matching cov for turncated normal
invTrcNormCov <- function(x) { 
  xx=matrix(c(x[1],x[2],0,x[3]),ncol=2)
  y = (mtmvnorm(c(true.phi0, est.phi0), xx%*%t(xx), lower=c(-1,-1),  upper=c(1,1) )$tvar-COV_phiphihat)[c(1,2,4)]
  return(y)
}

fcnval = mtmvnorm(c(true.phi0, est.phi0), COV_phiphihat, lower=c(-1,-1),  upper=c(1,1) )$tvar
xstart = numeric(3)
xstart[1] = -fcnval[1,1] + 2*COV_phiphihat[1,1]
xstart[2] = -fcnval[2,2] + 2*COV_phiphihat[2,2]
xstart[3] = sqrt(xstart[1]*xstart[2])*COV_phiphihat[1,2]/sqrt(COV_phiphihat[1,1]*COV_phiphihat[2,2])
xstart = t(chol( matrix(c(xstart[1],xstart[3],xstart[3],xstart[2]),ncol=2) ))[c(1,2,4)]


# a solution is c(1,1)
sols = nleqslv(xstart, invTrcNormCOv, control=list(btol=.001))
xx=matrix(c(sols$x[1],sols$x[2],0,sols$x[3]),ncol=2)
mtmvnorm(c(true.phi0, est.phi0), xx%*%t(xx), lower=c(-1,-1),  upper=c(1,1) )$tvar
COV_phiphihat



#### matching mean cov for turncated normal
invTrcNorm <- function(x) { 
  xx  = matrix(c(x[1],x[2],0,x[3]),ncol=2)  
  fcnval = mtmvnorm(x[c(4,5)], xx%*%t(xx), lower=c(-1,-1),  upper=c(1,1) )
  y = c( (fcnval$tvar-COV_phiphihat)[c(1,2,4)], (fcnval$tmean - c(true.phi0, est.phi0)) )
  print(y)
  return(y)
}

invTrcNorm.start <- function() { 
  fcnval = mtmvnorm(c(true.phi0, est.phi0), COV_phiphihat, lower=c(-1,-1),  upper=c(1,1) )$tvar
  xstart = numeric(3)
  xstart[1] = -fcnval[1,1] + 2*COV_phiphihat[1,1]
  xstart[2] = -fcnval[2,2] + 2*COV_phiphihat[2,2]
  xstart[3] = sqrt(xstart[1]*xstart[2])*COV_phiphihat[1,2]/sqrt(COV_phiphihat[1,1]*COV_phiphihat[2,2])
  xstart = c(t(chol( matrix(c(xstart[1],xstart[3],xstart[3],xstart[2]),ncol=2) ))[c(1,2,4)],c(true.phi0, est.phi0))
  
  return(xstart)
}

# a solution is c(1,1)
sols = nleqslv( invTrcNorm.start() , invTrcNorm, control=list(btol=.001))

# diagnoristics
xx=matrix(c(sols$x[1],sols$x[2],0,sols$x[3]),ncol=2)
mtmvnorm(sols$x[4:5], xx%*%t(xx), lower=c(-1,-1),  upper=c(1,1) )
COV_phiphihat
c(true.phi0, est.phi0)


