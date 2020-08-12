

# working: saved here as temp_20140623.Rdata

##############################################################################
########################################################################
#######  Learning?  #######
########################################################################
##############################################################################
require(lme4)
require(nlme)

load("temp_20140623.RData")
do_all = FALSE


######################################
######################################
#######  household chores  #######
######################################
######################################

filename = fcn_filename_maker(scaleVset=scaleVset, option=option )
load(file=filename)  

TS_range = range(firm_TS_data$year_qtr_idx, na.rm=T )  
TS_row = seq(TS_range[1], TS_range[2], by=0.25)  
N_row_TS = length( TS_row )  
N_row_TS_age = max(firm_TS_data$TS_index_firm_deflated,na.rm=T)  
est = firm_TS_data
yef_temp = est$ye_firm_deflated - est$yf_firm_deflated 
NA_idx_temp = is.na(  yef_temp ) 

yearqtr_idx_numeric = est$year_qtr_idx
yearqtr_xaxis = seq(range(yearqtr_idx_numeric,na.rm=T)[1],range(yearqtr_idx_numeric,na.rm=T)[2],by=0.25)
N_row_yearqtr =length(table(yearqtr_idx_numeric))


  
### construct year-quarter index without NA  & firm-wise TS index without NA
yearqtr_idx_numeric = est$year_qtr_idx
nonNA_index_firm = rep(NA, length(est$ye_firm_deflated) )
for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
{  
  firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]     
  nonNA_index_firm[ firm_idx_temp[!(NA_idx_temp[firm_idx_temp])] ] = seq(1, est$no_obs_deflated[jj]  )      
  yearqtr_idx_numeric[firm_idx_temp] = seq( yearqtr_idx_numeric[est$firm_begin_deflated[jj]],yearqtr_idx_numeric[est$firm_end_deflated[jj]], by=0.25)
}
nonNA_index_firm[is.na(nonNA_index_firm)]=0
sum( is.na(yearqtr_idx_numeric)    )
sum(abs(yearqtr_idx_numeric-est$year_qtr_idx),na.rm=T)
range(yearqtr_idx_numeric)



### construct variables ###
year_idx_numeric = floor(firm_TS_data$year_qtr_idx-0.2)
age_idx_numeric =  1+floor(firm_TS_data$TS_index_firm_deflated/4-0.2)
firm_idx_numeric = est$firm_deflated_nn_idx

year_dummy=  factor(year_idx_numeric)
age_dummy=  factor(age_idx_numeric)
firm_dummy=  factor(firm_idx_numeric)
  

FE_temp = c(firm_TS_data$ye_firm_deflated-firm_TS_data$yf_firm_deflated)
FE_temp_lag1 = c(NA,FE_temp[1:(length(FE_temp )-1)] )
FE_temp_lag1[est$firm_begin_deflated] = NA
pdataset = list(y=FE_temp, begin_idx =est$firm_begin_deflated, end_idx = est$firm_end_deflated)
FE_temp_pooled = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
FE_temp_pooled_lag1 = c(NA,FE_temp_pooled[1:(length(FE_temp_pooled )-1)] )

ye_temp =  firm_TS_data$ye_firm_deflated 
ye_temp_lag1 = c(NA,ye_temp[1:(length(ye_temp )-1)] )
ye_temp_lag1[est$firm_begin_deflated] = NA
pdataset = list(y=ye_temp, begin_idx =est$firm_begin_deflated, end_idx = est$firm_end_deflated)
ye_temp_pooled = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
ye_temp_pooled_lag1 = c(NA,ye_temp_pooled[1:(length(ye_temp_pooled )-1)] )

yf_temp =  firm_TS_data$yf_firm_deflated 
yf_temp_lag1 = c(NA,yf_temp[1:(length(yf_temp )-1)] )
yf_temp_lag1[est$firm_begin_deflated] = NA
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

 

##################################################
##################################################
#######  decaying autocorrelation of FE   #######
##################################################
##################################################
#1:20 - 21:40
#2:21 - 22:41
#3:22 - 23:42



max_no_obs_deflated = max(est$no_obs_deflated)
no_window_learn = max_no_obs_deflated-20+1

if (do_all) {
  
  FErho_learn_0 = matrix(rep(NA, no_window_learn*length(est$firm_begin_deflated )  ), ncol= no_window_learn) 
  FErhoadj_firmqtr_20 = rep(NA, length(est$ye_firm_deflated ))
  FErhoadj_firmqtr_10_30 = rep(NA, length(est$ye_firm_deflated ))
  FErhoadj_firmqtr_20_all = rep(NA, length(est$ye_firm_deflated ))
  FErhoadj_firmqtr_10_30_all = rep(NA, length(est$ye_firm_deflated ))
  FErho_learn_0_reg = matrix(rep(NA, no_window_learn*length(est$firm_begin_deflated )  ), ncol= no_window_learn) 
  FErhoadj_firmqtr_20_reg = rep(NA, length(est$ye_firm_deflated ))
  FErhoadj_firmqtr_10_30_reg = rep(NA, length(est$ye_firm_deflated ))
  N_FErho_learn_0_reg = matrix(rep(NA, no_window_learn*length(est$firm_begin_deflated )  ), ncol= no_window_learn) 
  
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
          
          N_FErho_learn_0_reg[jj,ii-19] = (sum(!is.na(xxx0_1+xxx0_0)))
          
          if (ii==20) FErhoadj_firmqtr_20[ firm_idx_temp[withinfirm_idx_temp] ] = FErho_learn_0[jj,ii-19] 
          if (ii==30) FErhoadj_firmqtr_10_30[ firm_idx_temp[withinfirm_idx_temp] ] = FErho_learn_0[jj,ii-19] 
          
          FErhoadj_firmqtr_20_all[ firm_idx_temp[withinfirm_idx_temp[1]] ] = FErho_learn_0[jj,ii-19] 
          FErhoadj_firmqtr_10_30_all[ firm_idx_temp[withinfirm_idx_temp[1]] ] = FErho_learn_0[jj,ii-19] 
        
        
          if (ii==20) FErhoadj_firmqtr_20_reg[ firm_idx_temp[withinfirm_idx_temp] ] = FErho_learn_0_reg[jj,ii-19] 
          if (ii==30) FErhoadj_firmqtr_10_30_reg[ firm_idx_temp[withinfirm_idx_temp] ] = FErho_learn_0_reg[jj,ii-19] 
           
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
    
  save(FErho_learn_0, file="FErho_learn_0.Rdata")     
  save(FErhoadj_firmqtr_20 , file="FErhoadj_firmqtr_20.Rdata")     
  save(FErhoadj_firmqtr_10_30 , file="FErhoadj_firmqtr_10_30.Rdata")     
  save(FErhoadj_firmqtr_20_all , file="FErhoadj_firmqtr_20_all.Rdata")     
  save(FErhoadj_firmqtr_10_30_all , file="FErhoadj_firmqtr_10_30_all.Rdata")   
  
  
  
  FErho_learn_0_reg=  (FErho_learn_0_reg *( 20 - 1)+1) / ( 20 - 4)
  FErhoadj_firmqtr_20_reg =  (FErhoadj_firmqtr_20_reg  *( 20 - 1)+1) / ( 20 - 4)   
  FErhoadj_firmqtr_10_30_reg =  (FErhoadj_firmqtr_10_30_reg  *( 20 - 1)+1) / ( 20 - 4)  
  
  save(FErho_learn_0_reg, file="FErho_learn_0_reg.Rdata")     
  save(FErhoadj_firmqtr_20_reg , file="FErhoadj_firmqtr_20_reg.Rdata")     
  save(FErhoadj_firmqtr_10_30_reg , file="FErhoadj_firmqtr_10_30_reg.Rdata")     
    
  
}




 







#######  1. FE_rho learning calendar time from raw estimate  #######
load("FErho_learn_0_reg.Rdata") 
 
if (do_all) {
   
  FErhoadj_firmqtr_all = matrix(rep(NA, length(yearqtr_xaxis)*length(est$firm_begin_deflated )  ), ncol= length(yearqtr_xaxis)) 
   
  for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
  {  
    firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]     
    nonNA_index_firm_temp = nonNA_index_firm[firm_idx_temp]      
    onetoT = est$TS_index_firm_deflated[firm_idx_temp]
    
    for (ii in c( 20:max_no_obs_deflated)) {
      
      if (est$no_obs_deflated[jj] >= ii ) {
        
        withinfirm_idx_temp = onetoT[nonNA_index_firm_temp==ii-19]:onetoT[nonNA_index_firm_temp==ii]                          
        yearqtr_idx_temp = (yearqtr_idx_numeric[firm_idx_temp[withinfirm_idx_temp]]-yearqtr_xaxis[1])*4+1
        FErhoadj_firmqtr_all[jj, yearqtr_idx_temp[1] ] = FErho_learn_0_reg[jj,ii-19]
         
      }
    }
    
    
  
     
    if (jj %% 100 ==0) print(jj/length(est$firm_begin_deflated ))
  }
    
  save(FErhoadj_firmqtr_all , file="FErhoadj_firmqtr_all.Rdata")   
  
}

load("FErhoadj_firmqtr_all.Rdata")   
rr=1;plot(yearqtr_xaxis[rr:111], FErho_yearqtr_adj_firm_avg[rr:111],type="o",ylim=c(0.1,0.25))
rr=1;plot(yearqtr_xaxis[rr:111], apply(FErhoadj_firmqtr_all[ !is.na(FErhoadj_firmqtr_all[,rr])  , ],2,mean,na.rm=T)[rr:111], lty=1,lwd=rr/10, col="gray10",ylim=c(0.1,0.25),type="l")
rr=11;lines(yearqtr_xaxis[rr:111], apply(FErhoadj_firmqtr_all[ !is.na(FErhoadj_firmqtr_all[,rr])  , ],2,mean,na.rm=T)[rr:111], lty=1,lwd=rr/10, col="gray30")
rr=21;lines(yearqtr_xaxis[rr:111], apply(FErhoadj_firmqtr_all[ !is.na(FErhoadj_firmqtr_all[,rr])  , ],2,mean,na.rm=T)[rr:111], lty=1,lwd=rr/10, col="gray50")
rr=31;lines(yearqtr_xaxis[rr:111], apply(FErhoadj_firmqtr_all[ !is.na(FErhoadj_firmqtr_all[,rr])  , ],2,mean,na.rm=T)[rr:111], lty=1,lwd=rr/10, col="gray60")
rr=41;lines(yearqtr_xaxis[rr:111], apply(FErhoadj_firmqtr_all[ !is.na(FErhoadj_firmqtr_all[,rr])  , ],2,mean,na.rm=T)[rr:111], lty=1,lwd=rr/10, col="gray70")
rr=51;lines(yearqtr_xaxis[rr:111], apply(FErhoadj_firmqtr_all[ !is.na(FErhoadj_firmqtr_all[,rr])  , ],2,mean,na.rm=T)[rr:111], lty=1,lwd=rr/10, col="gray80")
rr=61;lines(yearqtr_xaxis[rr:111], apply(FErhoadj_firmqtr_all[ !is.na(FErhoadj_firmqtr_all[,rr])  , ],2,mean,na.rm=T)[rr:111], lty=1,lwd=rr/10, col="gray90")





FErhoadj_firmqtr_all_firm_idx = matrix(rep(NA, length(yearqtr_xaxis)*length(est$firm_begin_deflated )  ), ncol= length(yearqtr_xaxis)) 

for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
{  
  firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]     
  nonNA_index_firm_temp = nonNA_index_firm[firm_idx_temp]      
  onetoT = est$TS_index_firm_deflated[firm_idx_temp]
  
  for (ii in c( 20:max_no_obs_deflated)) {
    
    if (est$no_obs_deflated[jj] >= ii ) {
      
      withinfirm_idx_temp = onetoT[nonNA_index_firm_temp==ii-19]:onetoT[nonNA_index_firm_temp==ii]                          
      yearqtr_idx_temp = (yearqtr_idx_numeric[firm_idx_temp[withinfirm_idx_temp]]-yearqtr_xaxis[1])*4+1
      FErhoadj_firmqtr_all_firm_idx[jj, yearqtr_idx_temp[1] ] = ii-19
      
    }
  }
   
  if (jj %% 100 ==0) print(jj/length(est$firm_begin_deflated ))
}

xx = FErhoadj_firmqtr_all
xx[c(FErhoadj_firmqtr_all_firm_idx!=1)] = NA
ts.plot(  apply( !is.na( matrix(xx, ncol=112) )  ,2, sum,na.rm=T)  )   

xx = FErhoadj_firmqtr_all
xx[c(FErhoadj_firmqtr_all_firm_idx!=11)] = NA
lines( apply( matrix(xx, ncol=112),2,mean,na.rm=T) ,lwd=2 )   


xx = FErhoadj_firmqtr_all
xx[c(FErhoadj_firmqtr_all_firm_idx!=21)] = NA
lines( apply( matrix(xx, ncol=112),2,mean,na.rm=T) ,lwd=4 )   




############ learning mu within firm using lmer (not quatntile 0) ##########
#1:20 - 21:40
#2:21 - 22:41
#3:22 - 23:42
max_no_obs_deflated = max(est$no_obs_deflated)
no_window_learn = max_no_obs_deflated-20+1

if (do_all) {
  
  FE_rho_learn_lmer.fixed = rep(NA, no_window_learn  ) 
  FE_rho_learn_lmer.random = rep(NA, no_window_learn  ) 
  FE_rho_learn_lmer_se.fixed = rep(NA, no_window_learn  ) 
  FE_rho_learn_lmer_se.random = rep(NA, no_window_learn  )     
  
  ye_mean_lmer_et_var0.random = rep(NA, no_window_learn  ) 
  ye_mean_lmer_et_var0.fixed = rep(NA, no_window_learn  ) 
   
  FE_rho_learn_lmer_1.random = rep(NA, no_window_learn  ) 
  FE_rho_learn_lmer_1.fixed = rep(NA, no_window_learn  ) 
  
  FE_rho_learn_pooled.reg = rep(NA, no_window_learn  ) 
  FE_rho_learn_pooled_SE.reg = rep(NA, no_window_learn  ) 
  
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
      
      M0.quant0 = lmer(FE_temp_pooled ~  FE_temp_pooled_lag1 +(1| firm_dummy_pooled )   ,subset = total_idx_temp_pooled ,  REML=TRUE)    
      
      FE_rho_learn_lmer.fixed[ii-19] = fixef(M0.quant0)[2]
      FE_rho_learn_lmer_se.fixed[ii-19] = sqrt(vcov(M0.quant0)[2,2])

      
      M0.quant0 = lmer(FE_temp_pooled ~  FE_temp_pooled_lag1 +(1| firm_dummy_pooled )+ (-1+FE_temp_pooled_lag1 | firm_dummy_pooled )   ,subset = total_idx_temp_pooled ,  REML=TRUE)      
      FE_rho_learn_lmer.random[ii-19] = fixef(M0.quant0)[2]
      FE_rho_learn_lmer_se.random[ii-19] = sqrt(vcov(M0.quant0)[2,2])

      
      M0.quant0 = lmer(ye_temp_pooled ~  ye_temp_pooled_lag1 +(1| firm_dummy_pooled )+ (-1+ye_temp_pooled_lag1 | firm_dummy_pooled )   ,subset = total_idx_temp_pooled ,  REML=TRUE)
      ye_mean_lmer_et_var0.random[ii-19] = attr(VarCorr(M0.quant0), "sc")^2
      

      M0.quant0 = lmer(ye_temp_pooled ~  ye_temp_pooled_lag1 +(1| firm_dummy_pooled )   ,subset = total_idx_temp_pooled ,  REML=TRUE)
      ye_mean_lmer_et_var0.fixed[ii-19] = attr(VarCorr(M0.quant0), "sc")^2
      
      
      
      M0 = lm(FE_temp_pooled ~  FE_temp_pooled_lag1    ,subset = total_idx_temp_pooled)          
      FE_rho_learn_pooled.reg[ii-19] = summary(M0)$coef[2,1]
      FE_rho_learn_pooled_SE.reg[ii-19] = summary(M0)$coef[2,2]
      
      
      if (ii==20) total_idx_temp_1_temp =total_idx_temp
      pdataset = list(y=total_idx_temp_1_temp, begin_idx =est$firm_begin_deflated, end_idx = est$firm_end_deflated)
      total_idx_temp_pooled = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )      
      
      M0.quant0 = lmer(FE_temp_pooled ~  FE_temp_pooled_lag1 +(1| firm_dummy_pooled )+ (-1+FE_temp_pooled_lag1 | firm_dummy_pooled )   ,subset = total_idx_temp_pooled ,  REML=TRUE)
      FE_rho_learn_lmer_1.random[ii-19] = fixef(M0.quant0)[2]
      
      M0.quant0 = lmer(FE_temp_pooled ~  FE_temp_pooled_lag1 +(1| firm_dummy_pooled )    ,subset = total_idx_temp_pooled ,  REML=TRUE)
      FE_rho_learn_lmer_1.fixed[ii-19] = fixef(M0.quant0)[2]
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
                         erroroccur = TRUE} )  
    
    print(ii)
  }
  
  FE_rho_learn_lmer_fixed = list(FE_rho = FE_rho_learn_lmer.fixed,
                                FE_rho_se = FE_rho_learn_lmer_se.fixed,
                                FE_rho_1 = FE_rho_learn_lmer_1.fixed,
                                var_et =  ye_mean_lmer_et_var0.fixed)
                                
  
  FE_rho_learn_lmer_random = list(FE_rho = FE_rho_learn_lmer.random,
                                 FE_rho_se = FE_rho_learn_lmer_se.random,
                                 FE_rho_1 = FE_rho_learn_lmer_1.random,
                                 var_et =  ye_mean_lmer_et_var0.random)
  
  save(FE_rho_learn_lmer_fixed ,file="FE_rho_learn_lmer_fixed.Rdata)")
  save(FE_rho_learn_lmer_random ,file="FE_rho_learn_lmer_random.Rdata)")
  
  save( FE_rho_learn_pooled.reg , file="FE_rho_learn_pooled_reg.Rdata")     
  save( FE_rho_learn_pooled_SE.reg , file="FE_rho_learn_pooled_SE_reg.Rdata")     
  
}

load("FE_rho_learn_lmer_random.Rdata)")
plot(FE_rho_learn_lmer_random$FE_rho,type="o")
load("FE_rho_learn_lmer_fixed.Rdata)")
plot(FE_rho_learn_lmer_fixed$FE_rho,type="o")

########### ###########  plot in the paper ########### ########### 
########### ###########  plot in the paper ########### ########### 
########### ###########  plot in the paper ########### ########### 

###################### autocorr_learning.eps ###################### 

# Paired t-test
# http://en.wikipedia.org/wiki/Student's_t-test#Dependent_t-test_for_paired_samples
load("FErhoadj_firmqtr_20_reg.Rdata") 
FErhoadj_firmqtr_20_trunc = FErhoadj_firmqtr_20_reg
FErhoadj_firmqtr_20_trunc[FErhoadj_firmqtr_20_trunc>1]=0.9999
FErhoadj_firmqtr_20_trunc[FErhoadj_firmqtr_20_trunc< (-1)]=-0.9999

FErho_yearqtr_adj_firm_avg = rep(NA, N_row_yearqtr )
FErho_yearqtr_adj_firm_SE =  rep(NA, N_row_yearqtr )

for (ii in seq( 1,N_row_yearqtr ) ) {    
  
  FErho_yearqtr_adj_firm = FErhoadj_firmqtr_20_trunc[ yearqtr_idx_numeric   == yearqtr_xaxis[ii]   ]  
  FErho_yearqtr_adj_firm_avg[ii] = mean(FErho_yearqtr_adj_firm ,na.rm=T)
  FErho_yearqtr_adj_firm_SE[ii] = sd(FErho_yearqtr_adj_firm,na.rm=T)/sqrt( sum(!is.na(FErho_yearqtr_adj_firm)) )
  
}


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


 

# plot2:  rho decreasing over age 
load("FErho_learn_0_reg.Rdata")


FErho_learn_0_reg_trunc = FErho_learn_0_reg
FErho_learn_0_reg_trunc[FErho_learn_0_reg_trunc>1]=0.9999
FErho_learn_0_reg_trunc[FErho_learn_0_reg_trunc< (-1)]=-0.9999

#x =  FErho_learn_0 
x = FErho_learn_0_reg_trunc

load("FErhoadj_firmqtr_all.Rdata")
x = FErhoadj_firmqtr_all


FErho_learn_mean0.1 =  (matrix(rep((x[,1] )  ,dim(x)[2]), ncol=dim(x)[2])) + x*0
FErho_learn_mean0.11 = (matrix(rep((x[,31])  ,dim(x)[2]), ncol=dim(x)[2])) + x*0

ssd = 1.96
xxx =  apply(x , 2, mean,na.rm=T)
#xxx =  apply(x , 2, median,na.rm=T)

dFErho_learn_SE0 =  apply(x,2, sd,na.rm=T) / sqrt( apply( !is.na(x ),2, sum) )
CIlow0 = xxx - dFErho_learn_SE0*ssd   
CIhigh0 = xxx + dFErho_learn_SE0*ssd  

#dFErho_learn_SE0 =  apply(x-FErho_learn_mean0.11 ,2, sd,na.rm=T) / sqrt( apply( !is.na(x-FErho_learn_mean0.11  ),2, sum) )
dFErho_learn_SE0 =  apply(x-FErho_learn_mean0.1 ,2, sd,na.rm=T) / sqrt( apply( !is.na(x-FErho_learn_mean0.1  ),2, sum) )

CIlow = xxx - dFErho_learn_SE0*ssd   
CIhigh = xxx + dFErho_learn_SE0*ssd  
xaxis  = 1:93
xaxis  =  yearqtr_xaxis

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

lines(xaxis[11:91], apply( FErho_learn_mean0.11  , 2, mean,na.rm=T)[11:91] , cex=0.5, lwd=1  ,type="l", lty=3  )
lines(xaxis[1:91], apply( FErho_learn_mean0.1 , 2, mean,na.rm=T)[1:91] , cex=0.5, lwd=1  ,type="l", lty="dashed"  )

#lines(xaxis[11:91], apply( FErho_learn_mean0.11  , 2, median,na.rm=T)[11:91] , cex=0.5, lwd=1  ,type="l", lty="solid"  )
#lines(xaxis[1:91], apply( FErho_learn_mean0.1 , 2, median,na.rm=T)[1:91] , cex=0.5, lwd=1  ,type="l", lty="dashed"  )

abline(h=0, lty=3  )


legend("bottomleft", inset=0.02,
       c(expression(hat(E)[italic("t+1:t+20")]*' [ '*rho[italic(i)]*'(FE'[italic("i, t+1:t+20")]*') ]'),
         expression(hat(E)[italic("t+1:t+20")]*' [ '*rho[italic(i)]*'(FE'[italic("i, 1:20")]*') ]'),
         expression(hat(E)[italic("t+1:t+20")]*' [ '*rho[italic(i)]*'(FE'[italic("i, 11:30")]*') ]'),
         expression('95% C.I. of '*hat(E)[italic("t+1:t+20")]*' [ '*rho[italic(i)]*'(FE'[italic("i, t+1:t+20")]*') ]'),
         expression('95% no-rejection regieon of '*hat(E)[italic("t+1:t+20")]*' [ '*rho[italic(i)]*'(FE'[italic("i, 1:20")]*') ]'),
         expression(paste("H"[o]*": "*E[italic("t+1:t+20")]*' [ '*rho[italic(i)]*'(FE'[italic("i, t+1:t+20")]*")",
                          " - "*rho[italic(i)]*"(FE"[italic("i, 1:20")]*") ]=0"))),
       lty=c(1,2,3,1,1,0), lwd=c(1,1,1,7,7,0), bty = "n",
       col=c("black","black","black","gray55","gray75"), pch=c(1,NA,NA,NA,NA,NA), cex=0.75  )





########### ###########  end of plot in the paper ########### ########### 
########### ###########  end of plot in the paper ########### ########### 
########### ###########  end of plot in the paper ########### ########### 














#########################################################
#########################################################
#######  learning phi within firm using lmer   #######
#########################################################
#########################################################
 
#1:20 - 21:40
#2:21 - 22:41
#3:22 - 23:42

max_no_obs_deflated = max(est$no_obs_deflated)
no_window_learn = max_no_obs_deflated-20+1

if (do_all) {
  
  phi_learn_lmer00 = rep(NA, no_window_learn  ) 
  phi_learn_lmer_var00 = rep(NA, no_window_learn  )  
  phihat_learn_lmer00 = rep(NA, no_window_learn  ) 
  phihat_learn_lmer_var00 = rep(NA, no_window_learn  )  
  phiphihat_learn_lmer00 = rep(NA, no_window_learn  ) 
  phiphihat_learn_lmer_var00 = rep(NA, no_window_learn  )  
  phiphihat_learn_lmer_var00_20 = rep(NA, no_window_learn  )  
  phiphihat_learn_lmer_var00_10_30 = rep(NA, no_window_learn  )  
  
  
  for (ii in c( 20:max_no_obs_deflated   )) {    
    
    total_idx_temp = rep(FALSE, length(est$ye_firm_deflated))
    total_idx_temp_20 = rep(FALSE, length(est$ye_firm_deflated))
    total_idx_temp_10_30 = rep(FALSE, length(est$ye_firm_deflated))
    
    for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
    {  
      firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]     
      nonNA_index_firm_temp = nonNA_index_firm[firm_idx_temp]     
      onetoT = est$TS_index_firm_deflated[firm_idx_temp]
      
      if (est$no_obs_deflated[jj] >= ii ) {        
        total_idx_temp[firm_idx_temp[ onetoT[nonNA_index_firm_temp==ii-19]:onetoT[nonNA_index_firm_temp==ii] ]   ]=TRUE     
        total_idx_temp_20[firm_idx_temp[ onetoT[nonNA_index_firm_temp==1]:onetoT[nonNA_index_firm_temp==20] ]   ]=TRUE     
        if (est$no_obs_deflated[jj] >= 30 )
           total_idx_temp_10_30[firm_idx_temp[ onetoT[nonNA_index_firm_temp==11]:onetoT[nonNA_index_firm_temp==30] ]   ]=TRUE             
      }    
      
    }
    
    
    tryCatch({
      
      
      M0.phiphihat = lmer(ye_temp ~ ye_temp_lag1+ (1| firm_dummy ) + (-1 + ye_temp_lag1 | firm_dummy )   ,subset = total_idx_temp,  REML=TRUE)
      phi_learn_lmer00[ii-19] = fixef(M0.phiphihat)[2]
      phi_learn_lmer_var00[ii-19] =  (attr( VarCorr(M0.phiphihat)$firm_dummy.1 ,"stddev")[1])^2
      
      M0.phiphihat = lmer(yf_temp ~ ye_temp_lag1+ (1| firm_dummy ) + (-1 + ye_temp_lag1 | firm_dummy )   ,subset = total_idx_temp,  REML=TRUE)
      phihat_learn_lmer00[ii-19] = fixef(M0.phiphihat)[2]
      phihat_learn_lmer_var00[ii-19] =  (attr( VarCorr(M0.phiphihat)$firm_dummy.1 ,"stddev")[1])^2
      
      M0.phiphihat = lmer(FE_temp ~ ye_temp_lag1+ (1| firm_dummy ) + (-1 + ye_temp_lag1 | firm_dummy )   ,subset = total_idx_temp,  REML=TRUE)
      phiphihat_learn_lmer00[ii-19] = fixef(M0.phiphihat)[2]
      phiphihat_learn_lmer_var00[ii-19] =  (attr( VarCorr(M0.phiphihat)$firm_dummy.1 ,"stddev")[1])^2
            
      
      M0.phiphihat = lmer(FE_temp ~ ye_temp_lag1+ (1| firm_dummy ) + (-1 + ye_temp_lag1 | firm_dummy )   ,subset = total_idx_temp_20,  REML=TRUE)      
      phiphihat_learn_lmer_var00_20[ii-19] =  (attr( VarCorr(M0.phiphihat)$firm_dummy.1 ,"stddev")[1])^2
      
      M0.phiphihat = lmer(FE_temp ~ ye_temp_lag1+ (1| firm_dummy ) + (-1 + ye_temp_lag1 | firm_dummy )   ,subset = total_idx_temp_10_30,  REML=TRUE)
      phiphihat_learn_lmer_var00_10_30[ii-19] =  (attr( VarCorr(M0.phiphihat)$firm_dummy.1 ,"stddev")[1])^2
      
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
                         erroroccur = TRUE} )  
    
    print(ii)
  }
  
  phi_learn = list(phiest = phi_learn_lmer00, phivar = phi_learn_lmer_var00,
                phihatest = phihat_learn_lmer00, phihatvar = phihat_learn_lmer_var00,
                phiphihatest = phiphihat_learn_lmer00, phiphihatvar = phiphihat_learn_lmer_var00,
                phiphihatvar_20 = phiphihat_learn_lmer_var00_20, phiphihatvar_10_30 = phiphihat_learn_lmer_var00_10_30  )
  
  save(phi_learn, file="phi_learn_lmer00.Rdata") 
  
}

 
load("phi_learn_lmer00.Rdata") 

ts.plot( phi_learn$phiphihatvar ,ylim=c(0,0.015),type="o")
lines(phi_learn$phiphihatvar[1]* phi_learn$phiphihatvar/phi_learn$phiphihatvar_20,lty=3 )
lines(phi_learn$phiphihatvar[1]* phi_learn$phiphihatvar/phi_learn$phiphihatvar_10_30 )




###############################################################
###############################################################
#######  learning  phi within firm from raw estimate  #######
###############################################################
############################################################### 
#1:20 - 21:40
#2:21 - 22:41
#3:22 - 23:42

max_no_obs_deflated = max(est$no_obs_deflated)
no_window_learn = max_no_obs_deflated-20+1

if (do_all) {
  
  phi_learn_0.alt = matrix(rep(NA, no_window_learn*length(est$firm_begin_deflated )  ), ncol= no_window_learn) 
  phihat_learn_0.alt= phi_learn_0.alt 
  phiphihat_learn_0.alt= phi_learn_0.alt 
  N_phi_learn_0.alt= phi_learn_0.alt 
  
  for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
  {  
    firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]     
    nonNA_index_firm_temp = nonNA_index_firm[firm_idx_temp]
        
    y = ye_temp[firm_idx_temp]
    yye = y
    yye.L1 = c(NA,y[1:(length(y)-1) ])
    
    x = yf_temp[firm_idx_temp]  
    yyf = x 
    yyf.L1 = c(NA,x[1:(length(x)-1) ])  
        
    onetoT = est$TS_index_firm_deflated[firm_idx_temp]
    
    for (ii in c( 20:est$no_obs_deflated[jj] )) {
          
      if (est$no_obs_deflated[jj] >= ii ) {
        
        tryCatch({
          
          tempidx = rep(FALSE, length(firm_idx_temp) )
          tempidx[ onetoT[nonNA_index_firm_temp==ii-19]:onetoT[nonNA_index_firm_temp==ii] ]=TRUE
                    
          Reg1 = lm(yye ~    yye.L1 , subset=tempidx        )          
          Reg3 = lm(yyf ~    yye.L1  , subset=tempidx        )          
          
          phiphihat_learn_0.alt[jj,ii-19] = Reg1$coef[2]- Reg3$coef[2]          
          phi_learn_0.alt[jj,ii-19] = Reg1$coef[2]
          phihat_learn_0.alt[jj,ii-19] = Reg3$coef[2]              
          N_phi_learn_0.alt[jj,ii-19] = sum(!is.na(yye + yye.L1))        
          
        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
                             erroroccur = TRUE} )
      }
    }
    if (jj %% 100 ==0) print(jj/length(est$firm_begin_deflated ))
  }
 
  
  
  phi_learn_firm = list(phiest = phi_learn_0.alt ,
                   phihatest  =  phihat_learn_0.alt,
                   phiphihatest = phiphihat_learn_0.alt)
  
  save(phi_learn_firm, file="phi_learn_firm00.Rdata") 
  
}


 

########################### plot in the paper ##########################
########################### plot in the paper ##########################
########################### plot in the paper ##########################
###########################  learning_phiphihat.eps ##########################
 
load("phi_learn_lmer00.Rdata") 
load("phi_learn_firm00.Rdata") 
 

ssd = 1.96
x= phi_learn_firm$phiphihatest
xxx = phi_learn$phiphihatvar
temp_scale = t(matrix(rep(1/apply( x , 2, sd,na.rm=T)*sqrt(xxx), dim(x)[1] ), ncol=dim(x)[1] ))

phiphihat_learn_0.1 = ((matrix(rep((x[,1])  ,93), ncol=93)) + x*0)
phiphihat_learn_0.11  = ((matrix(rep((x[,11])  ,93), ncol=93)) + x*0)

temp_Ref = phiphihat_learn_0.1 
temp_Ref2 = phiphihat_learn_0.11 

phiphihat_learn_0.11.mean =(1/apply( phiphihat_learn_0.11 , 2, var,na.rm=T)*apply( x , 2, var,na.rm=T)*xxx[1])
phiphihat_learn_0.1.mean = (1/apply( phiphihat_learn_0.1  , 2, var,na.rm=T)*apply( x , 2, var,na.rm=T)*xxx[1])
phiphihat_learn_0_lmer.1.mean = xxx[1]*xxx/phi_learn$phiphihatvar_20 


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


xaxis  = 0:92
T_temp = length(xxx)
xxx = xxx[1:(T_temp-2)]
xaxis  = xaxis[1:(T_temp-2)]
CIlow.1 =   CIlow.1[1:(T_temp-2)]
CIhigh.1 =  CIhigh.1[1:(T_temp-2)]

plot(NA,xlim=c(xaxis[1]-5, xaxis[length(xaxis)]), ylim=c(0, 0.0188),  
     main=expression(hat(sigma)[phi*",t"]^2*"="*hat("Var")*"[ ("*phi-hat(phi)*")"[italic("i, t+1:t+20")]*" ]"),
     xlab=expression("Quarter "*italic("t")*" since the first observation"),ylab="",
      cex.axis=0.8, tck= -0.02)
polygon( c(xaxis, xaxis[length(xaxis):1]  ),  
         c(CIlow.1, CIhigh.1[length(xaxis):1]), border = NA, col = "gray75")
#polygon( c(xaxis, xaxis[length(xaxis):1]  ),  
#         c(CIlow, CIhigh[length(xaxis):1]), border = NA, col = "gray55")
#lines(xaxis[1:91], phiphihat_learn_0.1.mean[1:91]  , cex=0.5, lwd=3  ,type="l", lty=1 ,col="gray75"   )
lines(xaxis[1:91], phiphihat_learn_0.1.mean[1:91]  , cex=0.5, lwd=1  ,type="l", lty=3  )
lines(xaxis[1:91], phiphihat_learn_0_lmer.1.mean[1:91], cex=0.5, lwd=1  ,type="l", lty=2)
lines(xaxis,xxx, cex=0.75, lwd=1  ,type="o" )
abline(h=xxx[1], lty=3 )
axis(4, at=c(0,xxx[1]), labels=c("0%","100%"), las=1, cex.axis=0.6, tck= -0.01, hadj=0.435)
#mtext("(%)",side=4,line=0.5,adj=1)
abline(h=0, lty=3  )
box()
 
legend("topleft", inset=0.02,
       c(expression(hat(sigma)[phi*",t"]^2*" from a mixed model"),
         expression(sigma[phi*",t"]^2*"/"*sigma[phi*",o"]^2*" est. from firm-matching mixed models"),
         expression("F="*S[phi*",t"]^2*"/"*S[phi*",o"]^2*" statistic for Pitman-Morgan test"),
         expression("95% no-rejection regieon of F="*S[phi*",t"]^2*"/"*S[phi*",o"]^2*" for H"[o]*": "*sigma[phi*",t"]^2*"="*sigma[phi*",o"]^2)),
         #expression("for Pitman-Morgan test with H"[o]*": "*sigma[phi*",t"]^2*"="*sigma[phi*",o"]^2 )),
       lty=c(1,2,3,1,0), lwd=c(1,1,1,7,0), bty = "n",
       col=c("black","black","black","gray75"), pch=c(1,NA,NA,NA,NA), cex=0.58,y.intersp=1.5 )


########################### end of plot in the paper ##########################
########################### end of plot in the paper ##########################
########################### end of plot in the paper ##########################



#########################################################
#########################################################
#######  learning mu within firm using lmer   #######
#########################################################
#########################################################

#1:20 - 21:40
#2:21 - 22:41
#3:22 - 23:42

max_no_obs_deflated = max(est$no_obs_deflated)
no_window_learn = max_no_obs_deflated-20+1

if (do_all) {
  
  REML_idx = TRUE
  
  mumuhat_learn_lmer_var00 = matrix(rep(NA, no_window_learn*6  ),nrow=6)    
  mumuhat_learn_lmer_var00_20 = mumuhat_learn_lmer_var00
  mumuhat_learn_lmer_var00_10_30 = mumuhat_learn_lmer_var00
  
  
  for (ii in c( 20:max_no_obs_deflated   )) {    
    
    total_idx_temp = rep(FALSE, length(est$ye_firm_deflated))
    total_idx_temp_20 = rep(FALSE, length(est$ye_firm_deflated))
    total_idx_temp_10_30 = rep(FALSE, length(est$ye_firm_deflated))
    
    for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
    {  
      firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]     
      nonNA_index_firm_temp = nonNA_index_firm[firm_idx_temp]     
      onetoT = est$TS_index_firm_deflated[firm_idx_temp]
      
      if (est$no_obs_deflated[jj] >= ii ) {        
        total_idx_temp[firm_idx_temp[ onetoT[nonNA_index_firm_temp==ii-19]:onetoT[nonNA_index_firm_temp==ii] ]   ]=TRUE     
        total_idx_temp_20[firm_idx_temp[ onetoT[nonNA_index_firm_temp==1]:onetoT[nonNA_index_firm_temp==20] ]   ]=TRUE     
        if (est$no_obs_deflated[jj] >= 30 )
          total_idx_temp_10_30[firm_idx_temp[ onetoT[nonNA_index_firm_temp==11]:onetoT[nonNA_index_firm_temp==30] ]   ]=TRUE             
      }    
      
    }
    
    
    tryCatch({
       
                
      M0.mumuhat = lmer(FE_temp ~ FE_temp_lag1+ ( FE_temp_lag1 | firm_dummy )   ,subset = total_idx_temp,  REML=REML_idx)      
      mumuhat_learn_lmer_var00[,ii-19] = c(attr( VarCorr(M0.mumuhat)$firm_dummy ,"stddev"),
                                           attr( VarCorr(M0.mumuhat)$firm_dummy ,"correlation")[1,2],
                                           attr(VarCorr(M0.mumuhat), "sc"),
                                           fixef(M0.mumuhat))
      
      M0.mumuhat = lmer(FE_temp ~ FE_temp_lag1+ ( FE_temp_lag1 | firm_dummy )   ,subset = total_idx_temp_20,  REML=REML_idx)      
      mumuhat_learn_lmer_var00_20[,ii-19] =  c(attr( VarCorr(M0.mumuhat)$firm_dummy ,"stddev"),
                                              attr( VarCorr(M0.mumuhat)$firm_dummy ,"correlation")[1,2],
                                              attr(VarCorr(M0.mumuhat), "sc"),
                                              fixef(M0.mumuhat))      
      
      M0.mumuhat = lmer(FE_temp ~ FE_temp_lag1+ ( FE_temp_lag1 | firm_dummy )   ,subset = total_idx_temp_10_30,  REML=REML_idx)
      mumuhat_learn_lmer_var00_10_30[,ii-19] = c(attr( VarCorr(M0.mumuhat)$firm_dummy ,"stddev"),
                                                attr( VarCorr(M0.mumuhat)$firm_dummy ,"correlation")[1,2],
                                                attr(VarCorr(M0.mumuhat), "sc"),
                                                fixef(M0.mumuhat))
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
                         erroroccur = TRUE} )  
    
    print(ii)
  }
  
  mu_learn = list( mumuhatvar = mumuhat_learn_lmer_var00,
                   mumuhatvar_20 = mumuhat_learn_lmer_var00_20, 
                   mumuhatvar_10_30 = mumuhat_learn_lmer_var00_10_30  )
  
  if (REML_idx) save(mu_learn, file="mu_learn_lmer00.Rdata") else
                save(mu_learn, file="mu_learn_lmer00_ML.Rdata") 
  
}

### transform the distributions of intercepts to the those of unconditional means.
# X = mu, Y = 1-phi
# (X,Y) = (mu, 1-phi) ~ BVN( , ) 
# a = XY = mu*(1-phi), b = 1-Y = phi
# Find E[a], E[b], and Cov(a,b)


  
require(nleqslv)
require(tmvtnorm)

#### matching mean cov for turncated normal
fcn_Mixed_moment_convert <- function(r) { 
  # r = (E(MU), E(PHI), Var(MU), Var(PHI), Cov(MU,PHI))
  # x = MU, y = 1-PHI
  # mu = E(X), Sigma = Cov(x,y)
  mu = c(r[1], 1-r[2])
  Sigma = matrix( c( r[3], -r[5], -r[5], r[4]), 2, 2 )
       
  # Isserlis' theorem
  # Isserlis, L. (1918). "On a formula for the product-moment coefficient of any order of a normal frequency distribution in any number of variables". Biometrika 12: 134-139.
  # (x,y) ~ BVN(mu, Sigma)
  Ex = mu[1]
  Ey = mu[2]
  
  Exy = Sigma[1,2]+Ex*Ey
  Exx = Sigma[1,1]+Ex^2
  Eyy = Sigma[2,2]+Ey^2
  Exxy = 2*Ex*Exy + Ey*Sigma[1,1]-Ex^2*Ey
  Exyy = 2*Ey*Exy + Ex*Sigma[2,2]-Ey^2*Ex
  Exxyy = Sigma[1,1]*Sigma[2,2] + 2*Sigma[1,2]^2 +2*Ey*Exxy - 
    Ey^2*Exx - Ex^2*Eyy + 2*Ey*Ex^2*Ey - Ex^2*Ey^2 + 
    2*Ex*Exyy - 4*Ex*Ey*Exy + 2*Ex*Ey^2*Ex  
  
  z = list(Ex=Ex, Ey=Ey, Exy=Exy, Exx=Exx, Eyy=Eyy, Exxy=Exxy, Exyy=Exyy, Exxyy=Exxyy)
  Ea = z$Exy
  Eb = 1-z$Ey
  Vara = z$Exxyy - (z$Exy)^2
  Varb = z$Eyy - (z$Ey)^2
  Covab = -z$Exyy - z$Exy*z$Ey
  
  E = c(Ea,Eb,Vara,Varb,Covab  ) - c(moments_from_mixed)
  
  return(E)
}



r = sols$x

load("mu_learn_lmer00.Rdata")
#load("mu_learn_lmer00_ML.Rdata") 


mest = mu_learn$mumuhatvar
mest = mu_learn$mumuhatvar_20

moments_from_mixed_set = rbind(mest[5:6,], mest[1,]^2, mest[2,]^2, mest[3,]*mest[2,]*mest[1,]  )
moments_from_mixed_set[is.na(moments_from_mixed_set)] = 0

trans_mmt = matrix(rep(NA,5*dim(moments_from_mixed_set)[2]  ), nrow=5)

for ( ii in seq(1, dim(moments_from_mixed_set)[2]  )  )
{
  
  x = moments_from_mixed_set[,ii]
  moments_from_mixed = x
  # r = (E(MU), E(PHI), Var(MU), Var(PHI), Cov(MU,PHI))
  r = c(x[1]/(1-x[2]),x[2],(x[2]+x[3]),x[4],0) 
  trans_mmt[,ii] = nleqslv( r  , fcn_Mixed_moment_convert, control=list(btol=.001))$x 
  
}


plot(moments_from_mixed_set[3,], type="o")
plot(moments_from_mixed_set[4,], type="o")
plot(trans_mmt[1,]  , type="o")
plot(trans_mmt[2,]  , type="o")
plot( (trans_mmt[3,]/mest[4,]^2)[1:92]  , type="o")
plot(trans_mmt[3,]  , type="o")
plot(trans_mmt[4,] , type="o")
plot( (mest[4,]^2)[1:92]  , type="o")


plot(mest[2,])
plot(mest[4,])




#### use the estimated rho(FE) to pre-subtract 
load("FErho_learn_0_reg.Rdata")
FE_rho_learn_est = apply(fcn_1_1_winsor(FErho_learn_0_reg),2,mean,na.rm=T)
############ learning mu within firm using lmer (not quatntile 0) ##########
#1:20 - 21:40
#2:21 - 22:41
#3:22 - 23:42
max_no_obs_deflated = max(est$no_obs_deflated)
no_window_learn = max_no_obs_deflated-20+1

if (do_all) { 
  
  REML_idx = TRUE
  
  FE_bias_learn_lmer_mu_var0.random = rep(NA, no_window_learn  )
  FE_rho_learn_lmer.random = rep(NA, no_window_learn  )   
  FE_bias_learn_lmer_et_var0.random = rep(NA, no_window_learn  )  
    
  FE_bias_learn_lmer_mu_var0.fixed = rep(NA, no_window_learn  ) 
  FE_rho_learn_lmer.fixed = rep(NA, no_window_learn  ) 
  FE_bias_learn_lmer_et_var0.fixed = rep(NA, no_window_learn  ) 
   
  
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
      
      M0.quant0 = lmer(c(FE_temp_pooled - FE_rho_learn_est[ii-19]*FE_temp_pooled_lag1)  ~ (1| firm_dummy_pooled )   ,subset = total_idx_temp_pooled ,  REML=REML_idx)      
      FE_bias_learn_lmer_mu_var0.fixed[ii-19] = (attr( VarCorr(M0.quant0)$firm_dummy_pooled ,"stddev")[1])^2            
      FE_rho_learn_lmer.fixed[ii-19] = fixef(M0.quant0)[1]
      FE_bias_learn_lmer_et_var0.fixed[ii-19] = attr(VarCorr(M0.quant0), "sc")^2
      
 
      
      if (ii==20) total_idx_temp_1_temp =total_idx_temp
      pdataset = list(y=total_idx_temp_1_temp, begin_idx =est$firm_begin_deflated, end_idx = est$firm_end_deflated)
      total_idx_temp_pooled = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )      
      
      M0.quant0 = lmer(c(FE_temp_pooled - FE_rho_learn_est[ii-19]*FE_temp_pooled_lag1)  ~(1| firm_dummy_pooled )   ,subset = total_idx_temp_pooled ,  REML=REML_idx)
      FE_bias_learn_lmer_mu_var_1.fixed[ii-19] = (attr( VarCorr(M0.quant0)$firm_dummy_pooled ,"stddev")[1])^2            
      FE_bias_learn_lmer_et_var_1.fixed[ii-19] = attr(VarCorr(M0.quant0), "sc")^2
      FE_rho_learn_lmer_1.fixed[ii-19] = fixef(M0.quant0)[1]
      
 
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
                         erroroccur = TRUE} )  
    
    print(ii)
  }
  
  
  mu_learn = list(  
    FE_bias_learn_lmer_mu_var0.fixed = FE_bias_learn_lmer_mu_var0.fixed,
    FE_rho_learn_lmer.fixed = FE_rho_learn_lmer.fixed,
    FE_bias_learn_lmer_et_var0.fixed = FE_bias_learn_lmer_et_var0.fixed,
    FE_bias_learn_lmer_mu_var_1.fixed = FE_bias_learn_lmer_mu_var_1.fixed,
    FE_rho_learn_lmer_1.fixed = FE_rho_learn_lmer_1.fixed,
    FE_bias_learn_lmer_et_var_1.fixed = FE_bias_learn_lmer_et_var_1.fixed)
      
  
  if (REML_idx) save(mu_learn, file="mu_learn_lmer00_premean.Rdata") else
    save(mu_learn, file="mu_learn_lmer00_premean_ML.Rdata") 
  
  
}









###############################################################
###############################################################
#######  learning  bias  within firm from raw estimate  #######
###############################################################
############################################################### 
#### use the estimated rho(FE) to pre-subtract for firmwise
load("FErho_learn_0_reg.Rdata")
FE_rho_learn_est = apply(fcn_1_1_winsor(FErho_learn_0_reg),2,mean,na.rm=T)
############ learning mu within firm using lmer (not quatntile 0) ##########
#1:20 - 21:40
#2:21 - 22:41
#3:22 - 23:42

max_no_obs_deflated = max(est$no_obs_deflated)
no_window_learn = max_no_obs_deflated-20+1

if (do_all) {
  
  mumuhat_learn_0 = matrix(rep(NA, no_window_learn*length(est$firm_begin_deflated )  ), ncol= no_window_learn) 
     
  for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
  {  
    firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]     
    nonNA_index_firm_temp = nonNA_index_firm[firm_idx_temp]
    
    y = FE_temp[firm_idx_temp]

    
    onetoT = est$TS_index_firm_deflated[firm_idx_temp]
    
    for (ii in c( 20:est$no_obs_deflated[jj] )) {
      
      if (est$no_obs_deflated[jj] >= ii ) {
        
        tryCatch({
          
          tempidx = rep(FALSE, length(firm_idx_temp) )
          tempidx[ onetoT[nonNA_index_firm_temp==ii-19]:onetoT[nonNA_index_firm_temp==ii] ]=TRUE
         
          yye = y[tempidx]
          yye.L1 = c(NA,yye[1:(length(yye)-1) ])          
          mumuhat_learn_0[jj,ii-19] = mean(yye-FE_rho_learn_est[ii-19]*yye.L1 , na.rm=T)/(1-FE_rho_learn_est[ii-19])        
                  
        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
                             erroroccur = TRUE} )
      }
    }
    if (jj %% 100 ==0) print(jj/length(est$firm_begin_deflated ))
  }
  
   
  save(mumuhat_learn_0, file="mu_learn_firm00.Rdata") 
  
}



 



 


### de-meand (year effect) series construction
year_axis =seq(range(year_idx_numeric ,na.rm=T)[1],range(year_idx_numeric ,na.rm=T)[2],by=0.25)
ye_year_mean = rep(NA, length(year_axis) )
FE_year_mean  =  rep(NA, length(year_axis) )

for (ii in  seq(1,length(year_axis)) ){
  temp_YQ_idx =  year_idx_numeric  == year_axis[ii]
  temp_YQ_idx[is.na(temp_YQ_idx)] = FALSE 
  
  ye_year_mean[ii]  =  mean(ye_temp[ temp_YQ_idx]  ,na.rm=T)     
  FE_year_mean[ii]  =  mean(FE_temp[ temp_YQ_idx]  ,na.rm=T)  
} 
  
FE_temp.year = FE_temp - FE_year_mean[ round( (year_idx_numeric-year_axis[1]+0.25)*4 )  ]
FE_temp_lag1.year = c(NA,FE_temp.year[1:(length(FE_temp.year )-1)] )
FE_temp_lag1.year[est$firm_begin_deflated] = NA
pdataset = list(y=FE_temp.year, begin_idx =est$firm_begin_deflated, end_idx = est$firm_end_deflated)
FE_temp_pooled.year = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
FE_temp_pooled_lag1.year = c(NA,FE_temp_pooled.year[1:(length(FE_temp_pooled.year )-1)] )

ye_temp.year = ye_temp - ye_year_mean[ round( (year_idx_numeric-year_axis[1]+0.25)*4 )  ]
ye_temp_lag1.year = c(NA,ye_temp.year[1:(length(ye_temp.year )-1)] )
ye_temp_lag1.year[est$firm_begin_deflated] = NA
pdataset = list(y=ye_temp.year, begin_idx =est$firm_begin_deflated, end_idx = est$firm_end_deflated)
ye_temp_pooled.year = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
ye_temp_pooled_lag1.year = c(NA,ye_temp_pooled.year[1:(length(ye_temp_pooled.year )-1)] )


############ learning mu within firm using lmer (no corr between random effects) ##########
#1:20 - 21:40
#2:21 - 22:41
#3:22 - 23:42
max_no_obs_deflated = max(est$no_obs_deflated)
no_window_learn = max_no_obs_deflated-20+1

if (do_all) { 
  
  
  REML_idx = TRUE

  #FE_temp_pooled.save = FE_temp_pooled 
  #FE_temp_pooled_lag1.save = FE_temp_pooled_lag1  
  #ye_temp_pooled.save = ye_temp_pooled 
  #ye_temp_pooled_lag1.save = ye_temp_pooled_lag1 
  
  #FE_temp_pooled = FE_temp_pooled.year
  #FE_temp_pooled_lag1 = FE_temp_pooled_lag1.year   
  #ye_temp_pooled = ye_temp_pooled.year
  #ye_temp_pooled_lag1 = ye_temp_pooled_lag1.year 
  
  
  
  FE_bias_learn_lmer_mu_var0.random = rep(NA, no_window_learn  )
  FE_rho_learn_lmer.random = rep(NA, no_window_learn  )   
  FE_bias_learn_lmer_et_var0.random = rep(NA, no_window_learn  )  
  ye_mean_lmer_et_var0.random = rep(NA, no_window_learn  ) 
  
  FE_bias_learn_lmer_mu_var_1.random = rep(NA, no_window_learn  ) 
  FE_rho_learn_lmer_1.random = rep(NA, no_window_learn  )   
  FE_bias_learn_lmer_et_var_1.random = rep(NA, no_window_learn  ) 
  ye_mean_lmer_et_var_1.random = rep(NA, no_window_learn  ) 
  
  
  FE_bias_learn_lmer_mu_var0.fixed = rep(NA, no_window_learn  ) 
  FE_rho_learn_lmer.fixed = rep(NA, no_window_learn  ) 
  FE_bias_learn_lmer_et_var0.fixed = rep(NA, no_window_learn  ) 
  ye_mean_lmer_et_var0.fixed = rep(NA, no_window_learn  ) 
    
  FE_bias_learn_lmer_mu_var_1.fixed = rep(NA, no_window_learn  ) 
  FE_rho_learn_lmer_1.fixed = rep(NA, no_window_learn  )   
  FE_bias_learn_lmer_et_var_1.fixed = rep(NA, no_window_learn  ) 
  ye_mean_lmer_et_var_1.fixed = rep(NA, no_window_learn  ) 
    
  
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
      
   
      M0.quant0 = lmer(FE_temp_pooled ~  FE_temp_pooled_lag1 +(1| firm_dummy_pooled )   ,subset = total_idx_temp_pooled ,  REML=REML_idx)      
      FE_bias_learn_lmer_mu_var0.fixed[ii-19] = (attr( VarCorr(M0.quant0)$firm_dummy_pooled ,"stddev")[1])^2            
      FE_rho_learn_lmer.fixed[ii-19] = fixef(M0.quant0)[2]
      FE_bias_learn_lmer_et_var0.fixed[ii-19] = attr(VarCorr(M0.quant0), "sc")^2
      
      M0.quant0 = lmer(ye_temp_pooled ~  ye_temp_pooled_lag1 +(1| firm_dummy_pooled )   ,subset = total_idx_temp_pooled ,  REML=REML_idx)
      ye_mean_lmer_et_var0.fixed[ii-19] = attr(VarCorr(M0.quant0), "sc")^2
      
      
      
      M0.quant0 = lmer(FE_temp_pooled ~  FE_temp_pooled_lag1 +(1| firm_dummy_pooled )+ (-1+FE_temp_pooled_lag1 | firm_dummy_pooled )   ,subset = total_idx_temp_pooled ,  REML=REML_idx)      
      FE_bias_learn_lmer_mu_var0.random[ii-19] = (attr( VarCorr(M0.quant0)$firm_dummy_pooled ,"stddev")[1])^2            
      FE_rho_learn_lmer.random[ii-19] = fixef(M0.quant0)[2]
      FE_bias_learn_lmer_et_var0.random[ii-19] = attr(VarCorr(M0.quant0), "sc")^2
      
      M0.quant0 = lmer(ye_temp_pooled ~  ye_temp_pooled_lag1 +(1| firm_dummy_pooled )+ (-1+ye_temp_pooled_lag1 | firm_dummy_pooled )   ,subset = total_idx_temp_pooled ,  REML=REML_idx)
      ye_mean_lmer_et_var0.random[ii-19] = attr(VarCorr(M0.quant0), "sc")^2
      
      
      
      
      
      if (ii==20) total_idx_temp_1_temp =total_idx_temp
      pdataset = list(y=total_idx_temp_1_temp, begin_idx =est$firm_begin_deflated, end_idx = est$firm_end_deflated)
      total_idx_temp_pooled = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )      
    
      M0.quant0 = lmer(FE_temp_pooled ~  FE_temp_pooled_lag1 +(1| firm_dummy_pooled )   ,subset = total_idx_temp_pooled ,  REML=REML_idx)
      FE_bias_learn_lmer_mu_var_1.fixed[ii-19] = (attr( VarCorr(M0.quant0)$firm_dummy_pooled ,"stddev")[1])^2            
      FE_bias_learn_lmer_et_var_1.fixed[ii-19] = attr(VarCorr(M0.quant0), "sc")^2
      FE_rho_learn_lmer_1.fixed[ii-19] = fixef(M0.quant0)[2]
      
      M0.quant0 = lmer(ye_temp_pooled ~  ye_temp_pooled_lag1 +(1| firm_dummy_pooled )    ,subset = total_idx_temp_pooled ,  REML=REML_idx)
      ye_mean_lmer_et_var_1.fixed[ii-19] = attr(VarCorr(M0.quant0), "sc")^2
      
      
      M0.quant0 = lmer(FE_temp_pooled ~  FE_temp_pooled_lag1 +(1| firm_dummy_pooled )+ (-1+FE_temp_pooled_lag1 | firm_dummy_pooled )   ,subset = total_idx_temp_pooled ,  REML=REML_idx)
      FE_bias_learn_lmer_mu_var_1.random[ii-19] = (attr( VarCorr(M0.quant0)$firm_dummy_pooled ,"stddev")[1])^2            
      FE_bias_learn_lmer_et_var_1.random[ii-19] = attr(VarCorr(M0.quant0), "sc")^2
      FE_rho_learn_lmer_1.random[ii-19] = fixef(M0.quant0)[2]
      
      M0.quant0 = lmer(ye_temp_pooled ~  ye_temp_pooled_lag1 +(1| firm_dummy_pooled )+ (-1+ye_temp_pooled_lag1 | firm_dummy_pooled )   ,subset = total_idx_temp_pooled ,  REML=REML_idx)
      ye_mean_lmer_et_var_1.random[ii-19] = attr(VarCorr(M0.quant0), "sc")^2
      
      
       
      
      
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
                         erroroccur = TRUE} )  
    
    print(ii)
  }
  
  
  mu_learn = list(  
  FE_bias_learn_lmer_mu_var0.random = FE_bias_learn_lmer_mu_var0.random,
  FE_rho_learn_lmer.random = FE_rho_learn_lmer.random,
  FE_bias_learn_lmer_et_var0.random = FE_bias_learn_lmer_et_var0.random,
  ye_mean_lmer_et_var0.random = ye_mean_lmer_et_var0.random,
  FE_bias_learn_lmer_mu_var_1.random = FE_bias_learn_lmer_mu_var_1.random, 
  FE_rho_learn_lmer_1.random = FE_rho_learn_lmer_1.random, 
  FE_bias_learn_lmer_et_var_1.random = FE_bias_learn_lmer_et_var_1.random,
  ye_mean_lmer_et_var_1.random = ye_mean_lmer_et_var_1.random,  
  FE_bias_learn_lmer_mu_var0.fixed = FE_bias_learn_lmer_mu_var0.fixed,
  FE_rho_learn_lmer.fixed = FE_rho_learn_lmer.fixed,
  FE_bias_learn_lmer_et_var0.fixed = FE_bias_learn_lmer_et_var0.fixed,
  ye_mean_lmer_et_var0.fixed = ye_mean_lmer_et_var0.fixed,  
  FE_bias_learn_lmer_mu_var_1.fixed = FE_bias_learn_lmer_mu_var_1.fixed,
  FE_rho_learn_lmer_1.fixed = FE_rho_learn_lmer_1.fixed,
  FE_bias_learn_lmer_et_var_1.fixed = FE_bias_learn_lmer_et_var_1.fixed,
  ye_mean_lmer_et_var_1.fixed = ye_mean_lmer_et_var_1.fixed)
   
  
  if (REML_idx) save(mu_learn, file="mu_learn_lmer00_simple.Rdata") else
    save(mu_learn, file="mu_learn_lmer00_simple_ML.Rdata") 
  
  #if (REML_idx) save(mu_learn, file="mu_learn_lmer00_simple_year.Rdata") else
  #              save(mu_learn, file="mu_learn_lmer00_simple_ML_year.Rdata") 
   
  #FE_temp_pooled = FE_temp_pooled.save 
  #FE_temp_pooled_lag1  = FE_temp_pooled_lag1.save  
  #ye_temp_pooled  = ye_temp_pooled.save 
  #ye_temp_pooled_lag1  = ye_temp_pooled_lag1.save 
  
}




load("mu_learn_lmer00_simple_year.Rdata" )
xxx= mu_learn$FE_bias_learn_lmer_mu_var0.fixed/(1-mu_learn$FE_rho_learn_lmer.fixed)^2
ts.plot(xxx)
ts.plot(xxx/mu_learn$FE_bias_learn_lmer_et_var0.fixed)
ts.plot((xxx/mu_learn$ye_mean_lmer_et_var0.fixed)[1:92] )
xxx1 = mu_learn$FE_bias_learn_lmer_mu_var_1.fixed/(1-mu_learn$FE_rho_learn_lmer_1.fixed)^2
ts.plot((xxx/xxx1)[1:80])
ts.plot(((xxx/mu_learn$FE_bias_learn_lmer_et_var0.fixed) / (xxx1/mu_learn$FE_bias_learn_lmer_et_var_1.fixed )  )[1:80])
ts.plot(((xxx/mu_learn$ye_mean_lmer_et_var0.fixed) / (xxx1/mu_learn$ye_mean_lmer_et_var_1.fixed )  )[1:80])




load("mu_learn_lmer00_simple.Rdata" ) 
ts.plot(mu_learn$FE_bias_learn_lmer_mu_var0.random/(1-mu_learn$FE_rho_learn_lmer.random)^2)
ts.plot(mu_learn$FE_bias_learn_lmer_mu_var0.random/(1-mu_learn$FE_rho_learn_lmer.random)^2/mu_learn$FE_bias_learn_lmer_et_var0.random)
ts.plot((mu_learn$FE_bias_learn_lmer_mu_var0.random/(1-mu_learn$FE_rho_learn_lmer.random)^2/mu_learn$ye_mean_lmer_et_var0.random)[1:92] )
ts.plot((mu_learn$FE_bias_learn_lmer_mu_var0.random/(1-mu_learn$FE_rho_learn_lmer.random)^2/(mu_learn$FE_bias_learn_lmer_mu_var_1.random/(1-mu_learn$FE_rho_learn_lmer_1.random)^2))[1:80])




load("mu_learn_lmer00_simple.Rdata" )
xxx= mu_learn$FE_bias_learn_lmer_mu_var0.fixed/(1-mu_learn$FE_rho_learn_lmer.fixed)^2
plot(xxx, type="o")
xxx.yeet = xxx/mu_learn$ye_mean_lmer_et_var0.fixed*mu_learn$ye_mean_lmer_et_var0.fixed[1]
xxx.FEet = xxx/mu_learn$FE_bias_learn_lmer_et_var0.fixed*mu_learn$FE_bias_learn_lmer_et_var0.fixed[1]
lines(xxx.yeet, lty=2)
lines(xxx.FEet, lty=3)
### scalcing by var(et), var(FE) do not matter


xxx1 = mu_learn$FE_bias_learn_lmer_mu_var_1.fixed/(1-mu_learn$FE_rho_learn_lmer_1.fixed)^2
ts.plot((xxx/xxx1)[1:80])
ts.plot(((xxx/mu_learn$FE_bias_learn_lmer_et_var0.fixed) / (xxx1/mu_learn$FE_bias_learn_lmer_et_var_1.fixed )  )[1:80])
ts.plot(((xxx/mu_learn$ye_mean_lmer_et_var0.fixed) / (xxx1/mu_learn$ye_mean_lmer_et_var_1.fixed )  )[1:80])
 

########################### plot in the paper ##########################
########################### plot in the paper ##########################
########################### plot in the paper ##########################
###########################  learning_mumuhat.eps ##########################
par(mfrow=c(1,2))

load("mu_learn_lmer00_simple.Rdata" )
load("mu_learn_firm00.Rdata") 


ssd = 1.96
x= mumuhat_learn_0
  
  
xxx = mu_learn$FE_bias_learn_lmer_mu_var0.fixed/(1-mu_learn$FE_rho_learn_lmer.fixed)^2*10^6
xxx1 = mu_learn$FE_bias_learn_lmer_mu_var_1.fixed/(1-mu_learn$FE_rho_learn_lmer_1.fixed)^2

mumuhat_learn_0.1 = ((matrix(rep((x[,1])  ,93), ncol=93)) + x*0)
mumuhat_learn_0.11  = ((matrix(rep((x[,11])  ,93), ncol=93)) + x*0)

temp_Ref = mumuhat_learn_0.1 
temp_Ref2 = mumuhat_learn_0.11 

mumuhat_learn_0.11.mean =(1/apply( mumuhat_learn_0.11 , 2, var,na.rm=T)*apply( x , 2, var,na.rm=T)*xxx[1])
mumuhat_learn_0.1.mean = (1/apply( mumuhat_learn_0.1  , 2, var,na.rm=T)*apply( x , 2, var,na.rm=T)*xxx[1])
mumuhat_learn_0_lmer.1.mean = xxx/xxx1*xxx1[1]


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


xaxis  = 0:92

 

plot(NA,xlim=c(xaxis[1]-5, xaxis[length(xaxis)]), ylim=c(0, 3.3 ),  
     main=expression(hat(sigma)[mu*",t"]^2*"="*hat("Var")*"[ ("*mu-hat(mu)*" + L)"[italic("i, t+1:t+20")]*" ]   ("*x10^6*")"),
     xlab=expression("Quarter "*italic("t")*" since the first observation"),ylab="",
     cex.axis=0.8, tck= -0.02)
polygon( c(xaxis, xaxis[length(xaxis):1]  ),  
         c(CIlow.1, CIhigh.1[length(xaxis):1]), border = NA, col = "gray75")
#polygon( c(xaxis, xaxis[length(xaxis):1]  ),  
#         c(CIlow, CIhigh[length(xaxis):1]), border = NA, col = "gray55")
#lines(xaxis[1:91], phiphihat_learn_0.1.mean[1:91]  , cex=0.5, lwd=3  ,type="l", lty=1 ,col="gray75"   )
lines(xaxis[1:83], mumuhat_learn_0_lmer.1.mean[1:83], cex=0.5, lwd=1  ,type="l", lty=2)
lines(xaxis[1:83], mumuhat_learn_0.1.mean[1:83]  , cex=0.5, lwd=1  ,type="l", lty=3  )
lines(xaxis[1:83],xxx[1:83], cex=0.75, lwd=1  ,type="o" )

abline(h=xxx[1], lty=3 )
axis(4, at=c(0,xxx[1]), labels=c("0%","100%"), las=1, cex.axis=0.6, tck= -0.01, hadj=0.435)
#mtext("(%)",side=4,line=0.5,adj=1)
abline( h=0, lty=3 )
box()

legend("topleft", inset=0.02,
       c(expression(hat(sigma)[mu*",t"]^2*" from a mixed model"),
         expression(sigma[mu*",t"]^2*"/"*sigma[mu*",o"]^2*" est. from firm-matching mixed models"),
         expression("F="*S[mu*",t"]^2*"/"*S[mu*",o"]^2*" statistic for Pitman-Morgan test"),
         expression("95% no-rejection regieon of F="*S[mu*",t"]^2*"/"*S[mu*",o"]^2*" for H"[o]*": "*sigma[mu*",t"]^2*"="*sigma[mu*",o"]^2)),
       #expression("for Pitman-Morgan test with H"[o]*": "*sigma[phi*",t"]^2*"="*sigma[phi*",o"]^2 )),
       lty=c(1,2,3,1,0), lwd=c(1,1,1,7,0), bty = "n",
       col=c("black","black","black","gray75"), pch=c(1,NA,NA,NA,NA), cex=0.58,y.intersp=1.5)


########################### end of plot in the paper ##########################
########################### end of plot in the paper ##########################
########################### end of plot in the paper ##########################

# robustcheck: scaled by sd(FE) or sd(et)...... pre demeaned year effect....use avg(rho_FE)
# to subtract FE.














#######################################################################
#######################################################################
#######  model implied autocorr in FE due to learning of phi  #########
#######################################################################
####################################################################### 

require(nleqslv)
require(tmvtnorm)

#### matching mean cov for turncated normal
invTrcNorm <- function(x) { 
  xx  = matrix(c(x[1],x[2],0,x[3]),ncol=2)  
  fcnval = mtmvnorm(x[c(4,5)], xx%*%t(xx), lower=c(-1,-1),  upper=c(1,1) )
  y = c( (fcnval$tvar-COV_phiphihat)[c(1,2,4)], (fcnval$tmean - c(true.phi0, est.phi0)) )
  
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

NN=10^6

## set parameters for calculations
true.phi0 = 0.5
est.phi0 = 0.5

true.sigma.a = 0 # almost no effect on FE_rho.
true.sigma.e = 1
true.sigma.n = 0.5

load("phi_learn_lmer00.Rdata") 
var_est_phi       = (phi_learn$phivar)
var_est_phihat    = (phi_learn$phihatvar)
var_est_phiphihat = (phi_learn$phiphihatvar)
cov_est_phiphihat = (var_est_phi+var_est_phihat-var_est_phiphihat)/2
COV_phiphihat_set = matrix(rbind(var_est_phi,cov_est_phiphihat,cov_est_phiphihat, var_est_phihat),nrow=4)

phi_learn_imp_rho_FE = rep(NA, length(var_est_phi ) )

if (do_all){ 
  for (ii in seq(1, (length(var_est_phi )-1)   )) {
    COV_phiphihat = matrix( COV_phiphihat_set[,ii], 2,2)
    
    sols = nleqslv( invTrcNorm.start() , invTrcNorm, control=list(btol=.001))
    t_chol_COV_phiphihat_adj = matrix(c(sols$x[1],sols$x[2],0,sols$x[3]),ncol=2)
    true_est_phi0_adj  = sols$x[4:5]  
    
    if (prod(diag(COV_phiphihat)==0)==1) e_phi = 0*matrix(rnorm(2*NN),nrow=2) else 
      e_phi = t_chol_COV_phiphihat_adj%*%matrix(rnorm(2*NN),nrow=2)
    
    true.phi = true_est_phi0_adj[1] +   e_phi[1,]
    est.phi = true_est_phi0_adj[2] +  e_phi[2,]
    
    statidx = abs(true.phi)<1 & abs(est.phi)<1 
    true.phi = true.phi[statidx] 
    est.phi = est.phi[statidx] 
    
    ## computation
    true.w = 1/true.sigma.n^2/(1/true.sigma.n^2+1/true.sigma.e^2)
    true.pis2 = 1/(1/true.sigma.e^2+1/true.sigma.n^2)
    est.w  = true.w
    
    ### b/c pospos_varE = 1/(1/pos_varE + 1/(true.sigma.a^2 + pospos_varE*true.phi^2)  );
    a = 1
    b = (1-true.phi^2)*true.sigma.a^2 - true.pis2
    c = -true.pis2*true.sigma.a^2
    
    ### a x^2 + bx + c = 0
    true.pit1t2 = ( -b + sqrt(   b^2 - 4*a*c  )   ) /a / 2  ;
    true.Kx = true.pit1t2/(true.sigma.a^2 + true.pit1t2  )     ;
    est.Kx = true.Kx
    
    varY = ( (1+true.phi^2)*true.sigma.a^2 + true.sigma.e^2 - 2*true.phi^2*true.sigma.a^2  )/(1-true.phi^2)
    covYYh = (true.phi*est.phi*est.Kx*(varY - true.sigma.a^2) + est.w*true.sigma.e^2  )/(1-true.phi*est.phi*(1-est.Kx)); 
    varYh = ( est.phi^2*est.Kx^2*varY + est.w^2*(true.sigma.e^2+true.sigma.n^2) + 2*est.phi^2*(1-est.Kx)*est.Kx*covYYh  )/(1-est.phi^2*(1-est.Kx)^2); 
    
    covYY1 = true.phi*( varY -  true.sigma.a^2);
    covYhYh1 = est.phi*( (1-est.Kx)*varYh + est.Kx*covYYh );
    covY1Yh = true.phi*covYYh;
    covYh1Y = est.phi*( (1-est.Kx)*covYYh + est.Kx*varY );
    
    ACF0 = varY + varYh - 2*covYYh;
    ACF1 = covYY1 + covYhYh1 - covY1Yh - covYh1Y;
    rho1 = ACF1/ACF0 
    
    phi_learn_imp_rho_FE[ii] = mean(rho1)
    
    
    print(ii)
  }
  save(phi_learn_imp_rho_FE, file="phi_learn_imp_rho_FE.Rdata")
}
load("phi_learn_imp_rho_FE.Rdata")
FErho_learn_01=FErho_learn_0
FErho_learn_01[FErho_learn_0>1]=1
FErho_learn_01[FErho_learn_0< (-1)]=-1


plot( apply(FErho_learn_01,2,mean,na.rm=T)[1:92], type="o",ylim=c(0,0.2))
lines( apply(FErho_learn_01,2,mean,na.rm=T) - phi_learn_imp_rho_FE , lty=2)
lines( apply(FErho_learn_01,2,mean,na.rm=T) - phi_learn_imp_rho_FE - apply(phi_learn_firm$phiphihatest,2,mean,na.rm=T)  , lty=3)



plot( apply(phi_learn_firm$phiphihatest,2,mean,na.rm=T)+phi_learn_imp_rho_FE+apply(FErho_learn_01,2,mean,na.rm=T)[1]-phi_learn_imp_rho_FE[1] ,type="o",ylim=c(0.1,0.2))
lines( apply(FErho_learn_01,2,mean,na.rm=T), lty=2)
lines( apply(phi_learn_firm$phiphihatest,2,mean,na.rm=T), lty=2)
lines(phi_learn$phiphihatest+0.15,lty=3)

plot(phi_learn_imp_rho_FE+apply(FErho_learn_0,2,mean,na.rm=T)[1]-phi_learn_imp_rho_FE[1] ,type="o",ylim=c(0.1,0.2))
lines( apply(FErho_learn_0,2,mean,na.rm=T), lty=2)
lines(phi_learn$phiphihatest,lty=3)
lines( apply(FErho_learn_0_reg,2,mean,na.rm=T)-0.125, lty=3)
lines( apply(FErho_learn_0,2,median,na.rm=T)-0.125 , lty=2)
lines( apply(FErho_learn_0_reg,2,median,na.rm=T)-0.125, lty=3)

plot( KFfit_K_w_rho_all[,3] ,type="o")
lines(FErho_learn_pooled.ar,lty=3)

plot(phi_learn$phiphihatest , type="o")
lines(apply(phiphihat_learn_0.alt,2,mean,na.rm=T), lty=3)
#### phi-phihat est (in ar(1)) decresese.
##### two possible causes: phi-phihat actually decreases, or Khat decreases
##### test...using 








#### Funtions for step 2: update the phi-phihat estimate to match (adjust bias)


fcn_phiphihat_ar_bias.1 <-function(calibpar,NN)  {
  
  source('fcn_implied_rho_phiphihat.r')
  phiphihat_ar_bias = rep(NA, (length(calibpar$phi0)-1 ) )
  
  for (ii in seq(1,length(calibpar$phi0)) ) {
    true.phi0 = calibpar$phi0[ii]
    true.phihat0 = calibpar$phihat0[ii]
    true.Khat = calibpar$Khat[ii]
    
    phiphihat_all=rep(NA, NN)
    
    for (jj in seq( 1 ,  NN ))
    {    
      Tsim = 20 + 50  
      
      et_sim = rnorm(Tsim)*true.sigma.e
      at_sim = rnorm(Tsim)*true.sigma.a
      nt_sim = rnorm(Tsim)*true.sigma.n
      
      ytsim1= rep(NA, Tsim )
      ytsim2= rep(NA, Tsim )
      
      ytsim1[1] = et_sim[1] + at_sim[1]
      ytsim2[1] = true.what*(et_sim[1] + nt_sim[1])
      
      for (iii  in seq(2,Tsim  ) )  {        
        ytsim1[iii] = true.phi0*ytsim1[iii-1] + et_sim[iii] + at_sim[iii] - true.phi0*at_sim[iii-1]     
        ytsim2[iii] = true.phihat0*true.Khat*ytsim1[iii-1] + (1-true.Khat)*true.phihat0*ytsim2[iii-1]  + true.what*(et_sim[iii] + nt_sim[iii])    
      }
      
      ytsim1  = ytsim1[51:Tsim]  
      ytsim2  = ytsim2[51:Tsim]
      
      phiphihat_all[jj] = lm( c(ytsim1-ytsim2)[2:20] ~ ytsim1[1:19] )$coef[2]  
    }   
    
    Imp = fcn_implied_rho_phiphihat( c(true.phi0, true.phihat0, true.Khat) )      
    phiphihat_ar_bias[ii] = mean(phiphihat_all) - Imp$phiphihat_ar_est
    #phiphihat_ar_bias[ii] = (true.phi0-true.phihat0) - Imp$phiphihat_ar_est
    print(ii)
  }
  return(phiphihat_ar_bias)
}



fcn_phiphihat_ar_bias <-function(calibpar,NN)  {  
  source('fcn_implied_rho_phiphihat.r')
  phiphihat_ar_bias = rep(NA, (length(calibpar$phi0) ) )
  
  for (ii in seq(1,length(calibpar$phi0)) ) {
    true.phi0 = calibpar$phi0[ii]
    true.phihat0 = calibpar$phihat0[ii]
    true.Khat = calibpar$Khat[ii] 
    
    
    phiphihat_all=rep(NA, NN)
    
    Tsim = 20*NN + 50  
    
    et_sim = rnorm(1)*true.sigma.e
    at_sim1 = rnorm(1)*true.sigma.a
    nt_sim = rnorm(1)*true.sigma.n
    
    ytsim1= rep(NA, Tsim )
    ytsim2= rep(NA, Tsim )
    
    ytsim1[1] = et_sim[1] + at_sim1[1]
    ytsim2[1] = true.what*(et_sim[1] + nt_sim[1])
    
    at_sim0= at_sim1
    
    for (iii  in seq(2,Tsim  ) )  {        
      et_sim = rnorm(1)*true.sigma.e
      at_sim1 = rnorm(1)*true.sigma.a
      nt_sim = rnorm(1)*true.sigma.n
      ytsim1[iii] = true.phi0*ytsim1[iii-1] + et_sim + at_sim1 - true.phi0*at_sim0     
      ytsim2[iii] = true.phihat0*true.Khat*ytsim1[iii-1] + (1-true.Khat)*true.phihat0*ytsim2[iii-1]  + true.what*(et_sim + nt_sim)    
      at_sim0= at_sim1
    }
    
    y  = matrix(ytsim1[51:Tsim]-ytsim2[51:Tsim] , nrow=20) 
    x  = matrix(ytsim1[50:(Tsim-1)], nrow=20) 
    
    phiphihat_all = (apply(x*y,2,sum)-apply(x,2,sum)*apply(y,2,sum)/20) / (apply(x^2,2,sum)-(apply(x,2,sum))^2/20)
    
    #Imp = fcn_implied_rho_phiphihat( c(true.phi0, true.phihat0, true.Khat) )         
    #phiphihat_ar_bias[ii] = mean(phiphihat_all) - Imp$phiphihat_ar_est
    #phiphihat_ar_bias[ii] = (true.phi0-true.phihat0) - Imp$phiphihat_ar_est
    phiphihat_ar_bias[ii] = mean(phiphihat_all) - (true.phi0-true.phihat0) 
    print(ii)
  }
  return(phiphihat_ar_bias)
}




#### Functions for step 3: update "rho(fe) due to var(phi-phihat)" estimate with better-calibrated parameters.


source("fcn_varphiphihat_imp_rho_FE.r")



#### Functions for step 1: Calibration by MM with exact identification.



fcn_1_1_winsor <- function(x) {
  x[x>1] = 0.9999
  x[x<  (-1)] = -0.9999  
  return(x)  
}



fcn_calib_phi_phihat_Khat <- function() {
  
  source('fcn_implied_rho_phiphihat.r')  
  
  fcn_inv_phihat  <- function(x) {     
    Imp = fcn_implied_rho_phiphihat(x)  
    y = c(Imp$FE_rho+phi_learn_imp_rho_FE_t        - FE_rho_learn_est_t, 
          Imp$phi_ar_est                           - phiest_t, 
          x[1]-x[2]+phiphihat_ar_bias_t            - phiphihatest_t)           
    
    return(y)
  }
  
  phiest_ar.imp = rep(NA, length(phiest_ar))
  phihatest_ar.imp = phiest_ar.imp
  Khat.imp = phihatest_ar.imp
  
  for (ii in seq(1 , (length(phiest_ar)-1) )   ) {
    # moments to match
    phiest_t = phiest_ar[ii]
    phiphihatest_t = phiphihatest_ar[ii] 
    FE_rho_learn_est_t = FE_rho_learn_est[ii] 
    
    # implied moments to match
    phi_learn_imp_rho_FE_t = phi_learn_imp_rho_FE[ii] 
    phiphihat_ar_bias_t = phiphihat_ar_bias[ii] 
    
    #nonlinear equation solver for ( phi, phihat, Khat)
    theta.start = c(phiest_t, phiest_t-phiphihatest_t, 0.5) # c( phi, phihat, Khat)
    sols = nleqslv( theta.start , fcn_inv_phihat,  control=list(btol=.001))
    
    phiest_ar.imp[ii]  = sols$x[1]
    phihatest_ar.imp[ii] = sols$x[2]
    Khat.imp[ii] = sols$x[3]    
  }     
  
  return(list(phi0=phiest_ar.imp,phihat0=phihatest_ar.imp,Khat=Khat.imp))
  
}

####################################################################
####################################################################
####################################################################
############### rho(FE) decomposition by calibration ###############
####################################################################
####################################################################
####################################################################

#### Libraries ####
require(nleqslv)
require(tmvtnorm)

#### Global variables. #### 
true.sigma.a = 0     # almost no effect on FE_rho.
true.sigma.e = 1
true.sigma.n = 0.5
true.w = 1/true.sigma.n^2/(1/true.sigma.n^2+1/true.sigma.e^2)
true.what = true.w

#### Global variables: momnets to match. ####
# 1) unbiased phi estiamtes (t)
load("phi_learn_firm00.Rdata") 
ar_phi_est_avg_adj = (phi_learn_firm$phiest *( 20  - 1)+1) / ( 20 - 4)
phiest_ar= apply( fcn_1_1_winsor(ar_phi_est_avg_adj),2,mean,na.rm=T)
# 2) biased phi-phihat estiamtes (t)
phiphihatest_ar = phi_learn_firm$phiphihatest  #+ 0.018 #*( 20  - 1)/( 20 - 4)
phiphihatest_ar = apply(fcn_1_1_winsor(phiphihatest_ar),2,mean,na.rm=T)
# 3) unbiased rho(FE) estimates (t)
load("FErho_learn_0_reg.Rdata")
FE_rho_learn_est = apply(fcn_1_1_winsor(FErho_learn_0_reg),2,mean,na.rm=T)
#load("phi_learn_0.Rdata") 
#phiest_ar = (phi_learn$phiest *( 20  - 1)+1) / ( 20 - 4)
#phiphihatest_ar = phi_learn$phiphihatest 


#### Local variables: Initial values ####
calibpar= list()
calibpar$phi0 = phiest_ar
calibpar$phihat0 = phiest_ar-phiphihatest_ar
calibpar$Khat0 = phiest_ar*0+0.7

#### Locally global variables: Initial values ####
# phi_learn_imp_rho_FE 
# phiphihat_ar_bias 

## assume phi=phihat=0.5
load("phi_learn_imp_rho_FE.Rdata")
phiphihat_ar_bias = phiphihatest_ar
phiest_ar_store = phiest_ar
phiest_ar = phiest_ar*0+0.5
for (kk in c(1:5)) {  
  calibpar = fcn_calib_phi_phihat_Khat()
  phi_imp_rho_FE =  fcn_varphiphihat_imp_rho_FE(calibpar,NN=10^6)
  phi_learn_imp_rho_FE =  phi_imp_rho_FE[,1] - phi_imp_rho_FE[,2]
  
  save(phi_imp_rho_FE,file="phi_imp_rho_FE_phi05.Rdata")
}
phiest_ar = phiest_ar_store
plot(calibpar$phi0,calibpar$phihat0)


## assume phi_t=phihat_t
load("phi_learn_imp_rho_FE.Rdata")
phiphihat_ar_bias = phiphihatest_ar
for (kk in c(1:5)) {  
  calibpar = fcn_calib_phi_phihat_Khat()
  phi_imp_rho_FE =  fcn_varphiphihat_imp_rho_FE(calibpar,NN=10^6)
  phi_learn_imp_rho_FE =  phi_imp_rho_FE[,1] - phi_imp_rho_FE[,2]
  
  save(phi_imp_rho_FE,file="phi_imp_rho_FE_phiphihat0.Rdata")
}
plot(calibpar$phi0-calibpar$phihat0)



## assume bias and true moments of phi-phihat cancels out
phiphihat_ar_bias = phiphihatest_ar*0
load("phi_learn_imp_rho_FE.Rdata")
for (kk in c(1:5)) {
  calibpar = fcn_calib_phi_phihat_Khat()
  phi_imp_rho_FE =  fcn_varphiphihat_imp_rho_FE(calibpar,NN=10^6)
  phi_learn_imp_rho_FE =  phi_imp_rho_FE[,1] - phi_imp_rho_FE[,2]
  
  save(phi_imp_rho_FE,file="phi_imp_rho_FE.Rdata")
}




## full
phiphihat_ar_bias = phiphihatest_ar*0
load("phi_imp_rho_FE.Rdata")
phi_learn_imp_rho_FE =  phi_imp_rho_FE[,1] - phi_imp_rho_FE[,2]

for (kk in c(1:5)) {
  calibpar = fcn_calib_phi_phihat_Khat()
  
  phiphihat_ar_bias =   fcn_phiphihat_ar_bias(calibpar,NN=10^6)    
  calibpar$phihat0 = calibpar$phihat0 + phiphihat_ar_bias
  
  phi_imp_rho_FE =  fcn_varphiphihat_imp_rho_FE(calibpar,NN=10^6)
  phi_learn_imp_rho_FE =  phi_imp_rho_FE[,1] - phi_imp_rho_FE[,2] 
  
  save(phi_imp_rho_FE,file="phi_imp_rho_FE_full.Rdata")
  save( phiphihat_ar_bias,file=" phiphihat_ar_bias_full.Rdata")
}


#### step 0: Initiate the phi-phihat estimate to match (adjust bias)
#            Initiate "rho(fe) due to var(phi-phihat)" estimate 

#### step 1: Calibration by MM with exact identification.
#### moment matching
#### step 2: update the phi-phihat estimate to match (adjust bias)

#### step 3: update "rho(fe) due to var(phi-phihat)" estimate with better-calibrated parameters.

#### Repeat step 1-3 until it converges.



#plot 

load("phi_imp_rho_FE_phi05.Rdata")

phi_learn_imp_rho_FE =  phi_imp_rho_FE[,1] - phi_imp_rho_FE[,2]
plot( FE_rho_learn_est[1:92], type="o", ylim=c(0,0.2))
lines( FE_rho_learn_est - phi_learn_imp_rho_FE , lty=2)
lines( FE_rho_learn_est- phi_learn_imp_rho_FE -(phi_imp_rho_FE[,2] - phi_imp_rho_FE[,3]), lty=3)
lines(phi_imp_rho_FE[,1], lwd=3, col="gray")

plot(  (phi_imp_rho_FE[,1] - phi_imp_rho_FE[,2])/((phi_imp_rho_FE[,1] - phi_imp_rho_FE[,2])[1]), ylim=c(0,1.2), type="l")
polygon( c( 1:92, 92:1  ),  
         c( rep(0,92),  ((phi_imp_rho_FE[,3])/((phi_imp_rho_FE[,3])[1]))[92:1]), border = NA, col = "gray75")
lines(  (phi_imp_rho_FE[,1] - phi_imp_rho_FE[,2])/((phi_imp_rho_FE[,1] - phi_imp_rho_FE[,2])[1]), ylim=c(0,1.2), type="l")
lines( (phi_imp_rho_FE[,2] - phi_imp_rho_FE[,3])/((phi_imp_rho_FE[,2] - phi_imp_rho_FE[,3])[1]), lty=2)
lines( (phi_imp_rho_FE[,3])/((phi_imp_rho_FE[,3])[1]), lty=3)

plot(  (phi_imp_rho_FE[,1] - phi_imp_rho_FE[,3])/((phi_imp_rho_FE[,1] - phi_imp_rho_FE[,3])[1]), ylim=c(0,1.2), type="l")
polygon( c( 1:92, 92:1  ),  
         c( rep(0,92),  ((phi_imp_rho_FE[,3])/((phi_imp_rho_FE[,3])[1]))[92:1]), border = NA, col = "gray75")
lines(  (phi_imp_rho_FE[,1] - phi_imp_rho_FE[,3])/((phi_imp_rho_FE[,1] - phi_imp_rho_FE[,3])[1]), ylim=c(0,1.2), type="l")



load("phi_imp_rho_FE.Rdata")

 







plot(phiest_ar-phiest_ar.imp  ,type="o")

par(mfrow=c(1,2))
plot(phiest_ar.imp - phihatest_ar.imp ,type="o")
plot(Khat.imp ,type="o")

plot( FE_rho_learn_est  ,type="o", ylim=c(0.058,0.32))
lines(FE_rho_learn_est- phi_learn_imp_rho_FE,type="l",lty=3)
lines( FE_rho_learn_est- phi_learn_imp_rho_FE-(phiest_ar.imp - phihatest_ar.imp ) )
load("FE_rho_learn_pooled_reg.Rdata")
lines( FE_rho_learn_pooled.reg )  





########################### plot in the paper ##########################
########################### plot in the paper ##########################
########################### plot in the paper ##########################
###########################  rho_dynamic_decomposition.eps ##########################
par(mfrow=c(1,2))

load("phi_imp_rho_FE_phi05.Rdata")
load("FE_rho_learn_pooled_reg.Rdata")
load("FErho_learn_0_reg.Rdata")
FE_rho_learn_est = apply(fcn_1_1_winsor(FErho_learn_0_reg),2,mean,na.rm=T)
  
xaxis  = 0:92
Tmaxplot = 91

plot(NA,xlim=c(xaxis[1]-5, xaxis[length(xaxis)]), ylim=c(0, 0.33 ),  
     main=expression('Autocorr(FE'[italic("t+1:t+20")]*')'),
     xlab=expression("Quarter "*italic("t")*" since the first observation"),ylab="",
     cex.axis=0.8, tck= -0.02)
polygon( c( 0:91, 91:0 ),  
         c( rep(0,92),  ((phi_imp_rho_FE[,3]))[92:1]), border = NA, col = "gray75")
lines(xaxis[1:Tmaxplot], FE_rho_learn_est[1:Tmaxplot], cex=0.75, lwd=1  ,type="o" )
lines(xaxis[1:Tmaxplot], FE_rho_learn_pooled.reg[1:Tmaxplot], cex=0.5, lwd=1  ,type="l", lty=2)
#lines(xaxis[1:Tmaxplot], phi_imp_rho_FE[1:Tmaxplot,2]  , cex=0.5, lwd=1  ,type="l", lty=1  )
abline( h=0, lty=3 )
box()

legend("topleft", inset=0.02,
       c(expression("from ("*hat(K)<K*")  +  Var("*phi[i]-hat(phi)[i]*")>0   +  Var("*mu[i]-hat(mu)[i]-L[i]*")>0"  ),
         expression("from ("*hat(K)<K*")  +  Var("*phi[i]-hat(phi)[i]*")>0"  ),
         expression("from ("*hat(K)<K*")")),
         lty=c(2,1,1), lwd=c(1,1,7), bty = "n",
         col=c("black","black","gray75"), pch=c(NA,1,NA), cex=0.58,y.intersp=1.5)
       



plot(NA,xlim=c(xaxis[1]-5, xaxis[length(xaxis)]), ylim=c(0, 1.4),  
     main=expression('Autocorr(FE'[italic("t+1:t+20")]*')'),
     xlab=expression("Quarter "*italic("t")*" since the first observation"),ylab="",
     cex.axis=0.8, tck= -0.02, yaxt='n'  )
polygon( c( 0:91, 91:0 ),  
         c( rep(0,92),  ((phi_imp_rho_FE[,3]))[92:1]/((phi_imp_rho_FE[,3])[1])), border = NA, col = "gray75")
#lines(xaxis[1:Tmaxplot], phi_imp_rho_FE[1:Tmaxplot,3]/(phi_imp_rho_FE[1,3])   , cex=0.5, lwd=1  ,type="l", lty=1  )
lines(xaxis[1:Tmaxplot], (phi_imp_rho_FE[1:Tmaxplot,1]-phi_imp_rho_FE[1:Tmaxplot,2])/(phi_imp_rho_FE[1,1]-phi_imp_rho_FE[1,2])  , cex=0.5, lwd=1  ,type="l", lty=1  )

axis(2, at=seq(0,1,by=0.2), labels=c("0%","20%","40%","60%","80%","100%"), las=1, cex.axis=0.6, tck= -0.01, hadj=0.435)
#mtext("(%)",side=4,line=0.5,adj=1)
abline( h=1, lty=3 )
abline( h=0, lty=3 )
box()
legend("topleft", inset=0.02,
       c(expression("from Var("*phi[i]-hat(phi)[i]*")>0"  ),
         expression("from ("*hat(K)<K*")")),
       lty=c(1,1), lwd=c(1,7), bty = "n",
       col=c("black","gray75"), pch=c(NA,NA), cex=0.58,y.intersp=1.5)






par(mfrow=c(1,2))

load("phi_imp_rho_FE.Rdata")
load("FE_rho_learn_pooled_reg.Rdata")
load("FErho_learn_0_reg.Rdata")
FE_rho_learn_est = apply(fcn_1_1_winsor(FErho_learn_0_reg),2,mean,na.rm=T)

xaxis  = 0:92
Tmaxplot = 91

plot(NA,xlim=c(xaxis[1]-5, xaxis[length(xaxis)]), ylim=c(0, 0.33 ),  
     main=expression('Autocorr(FE'[italic("t+1:t+20")]*')'),
     xlab=expression("Quarter "*italic("t")*" since the first observation"),ylab="",
     cex.axis=0.8, tck= -0.02)
polygon( c( 0:91, 91:0 ),  
         c( rep(0,92),  ((phi_imp_rho_FE[,3]))[92:1]), border = NA, col = "gray75")
lines(xaxis[1:Tmaxplot], FE_rho_learn_est[1:Tmaxplot], cex=0.75, lwd=1  ,type="o" )
lines(xaxis[1:Tmaxplot], FE_rho_learn_pooled.reg[1:Tmaxplot], cex=0.5, lwd=1  ,type="l", lty=2)
lines(xaxis[1:Tmaxplot], phi_imp_rho_FE[1:Tmaxplot,2]  , cex=0.5, lwd=1  ,type="l", lty=1  )
#lines(xaxis[1:Tmaxplot], phi_imp_rho_FE[1:Tmaxplot,3]  , cex=0.5, lwd=1  ,type="l", lty=3  )
abline( h=0, lty=3 )
box()

legend("topleft", inset=0.02,
       c(expression("from ("*hat(K)<K*")  +  E("*phi-hat(phi)*")>0   +  Var("*phi[i]-hat(phi)[i]*")>0   +  Var("*mu[i]-hat(mu)[i]-L[i]*")>0"  ),
         expression("from ("*hat(K)<K*")  +  E("*phi-hat(phi)*")>0   +  Var("*phi[i]-hat(phi)[i]*")>0"  ),
         expression("from ("*hat(K)<K*")  +  E("*phi-hat(phi)*")>0   +  Var("*phi[i]-hat(phi)[i]*")>0"  ),
         expression("from ("*hat(K)<K*")")),
       lty=c(2,1,1,1), lwd=c(1,1,1,7), bty = "n",
       col=c("black","black","black","gray75"), pch=c(NA,1,NA,NA), cex=0.58,y.intersp=1.5)




plot(NA,xlim=c(xaxis[1]-5, xaxis[length(xaxis)]), ylim=c(0, 1.4),  
     main=expression('Autocorr(FE'[italic("t+1:t+20")]*')'),
     xlab=expression("Quarter "*italic("t")*" since the first observation"),ylab="",
     cex.axis=0.8, tck= -0.02, yaxt='n'  )
polygon( c( 0:91, 91:0 ),  
         c( rep(0,92),  ((phi_imp_rho_FE[,3]))[92:1]/((phi_imp_rho_FE[,3])[1])), border = NA, col = "gray75")
#lines(xaxis[1:Tmaxplot], phi_imp_rho_FE[1:Tmaxplot,3]/(phi_imp_rho_FE[1,3])   , cex=0.5, lwd=1  ,type="l", lty=1  )
lines(xaxis[1:Tmaxplot], (phi_imp_rho_FE[1:Tmaxplot,1]-phi_imp_rho_FE[1:Tmaxplot,2])/(phi_imp_rho_FE[1,1]-phi_imp_rho_FE[1,2])  , cex=0.5, lwd=1  ,type="l", lty=1  )
lines(xaxis[1:Tmaxplot], (phi_imp_rho_FE[1:Tmaxplot,2]-phi_imp_rho_FE[1:Tmaxplot,3])/(phi_imp_rho_FE[1,2]-phi_imp_rho_FE[1,3])  , cex=0.5, lwd=1  ,type="l", lty=2  )

axis(2, at=seq(0,1,by=0.2), labels=c("0%","20%","40%","60%","80%","100%"), las=1, cex.axis=0.6, tck= -0.01, hadj=0.435)
#mtext("(%)",side=4,line=0.5,adj=1)
abline( h=1, lty=3 )
abline( h=0, lty=3 )
box()
legend("topleft", inset=0.02,
       c(expression("from Var("*phi[i]-hat(phi)[i]*")>0"  ),
         expression("from E("*phi[i]-hat(phi)[i]*")>0"  ),
         expression("from ("*hat(K)<K*")")),
       lty=c(1,2,1), lwd=c(1,1,7), bty = "n",
       col=c("black","black","gray75"), pch=c(NA,NA,NA), cex=0.58,y.intersp=1.5)




########################### end of plot in the paper ##########################
########################### end of plot in the paper ##########################
########################### end of plot in the paper ##########################
 


# Pooled-regression estimate
# Avg. of firm-level estimates
# implied effect of var(phi-phihat)
  

#####################################################################################
#####################################################################################
#####################################################################################

# working

#####################################################################################
#####################################################################################
#####################################################################################


 

#######  1. learning phi calendar time from raw estimate  #######
 
load("phi_learn_firm00.Rdata") 
load("FErho_learn_0_reg.Rdata")  
 

if (do_all) {
  
  phi_firmqtr_20_reg =  rep(NA, length(est$ye_firm_deflated ))
  phihat_firmqtr_20_reg = phi_firmqtr_20_reg 
  phiphihat_firmqtr_20_reg = phi_firmqtr_20_reg 
  
  phi_learn_0.alt = matrix(rep(NA, length(yearqtr_xaxis)*length(est$firm_begin_deflated )  ), ncol= length(yearqtr_xaxis)) 
  phihat_learn_0.alt  =  phi_learn_0.alt
  phiphihat_learn_0.alt  =  phi_learn_0.alt
  FErhoadj_firmqtr_20_reg_cal =  phi_learn_0.alt
   
  for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
  {  
    firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]     
    nonNA_index_firm_temp = nonNA_index_firm[firm_idx_temp]      
    onetoT = est$TS_index_firm_deflated[firm_idx_temp]
          
    for (ii in c( 20)) {
      
      if (est$no_obs_deflated[jj] >= ii ) {
        
          withinfirm_idx_temp = onetoT[nonNA_index_firm_temp==ii-19]:onetoT[nonNA_index_firm_temp==ii]                
          phi_firmqtr_20_reg[ firm_idx_temp[withinfirm_idx_temp] ] = phi_learn_firm$phiest[jj,ii-19]
          phihat_firmqtr_20_reg[ firm_idx_temp[withinfirm_idx_temp] ] = phi_learn_firm$phihatest[jj,ii-19]
          phiphihat_firmqtr_20_reg[ firm_idx_temp[withinfirm_idx_temp] ] = phi_learn_firm$phiphihatest[jj,ii-19]
            
          yearqtr_idx_temp = (yearqtr_idx_numeric[firm_idx_temp[withinfirm_idx_temp]]-yearqtr_xaxis[1])*4+1
            
          #rint(yearqtr_idx_temp[1])
          
          phi_learn_0.alt[jj, yearqtr_idx_temp ] = phi_learn_firm$phiest[jj,ii-19]
          phihat_learn_0.alt[jj, yearqtr_idx_temp ] =  phi_learn_firm$phihatest[jj,ii-19]
          phiphihat_learn_0.alt[jj, yearqtr_idx_temp ] = phi_learn_firm$phiphihatest[jj,ii-19]
          FErhoadj_firmqtr_20_reg_cal[jj, yearqtr_idx_temp ] =  FErho_learn_0_reg[jj,ii-19]
      }
    }
    if (jj %% 100 ==0) print(jj/length(est$firm_begin_deflated ))
  }
  
  save(phi_firmqtr_20_reg, file="phi_firmqtr_20_reg.Rdata") 
  save(phihat_firmqtr_20_reg, file="phihat_firmqtr_20_reg.Rdata") 
  save(phiphihat_firmqtr_20_reg, file="phiphihat_firmqtr_20_reg.Rdata") 
  
  phi_learn_firm = list(phiest = phi_learn_0.alt ,
                        phihatest  =  phihat_learn_0.alt,
                        phiphihatest = phiphihat_learn_0.alt)
  
  save(phi_learn_firm, file="phi_learn_firm00_cal.Rdata")   
  save(FErhoadj_firmqtr_20_reg_cal,file="FErhoadj_firmqtr_20_reg_cal.Rdata") 
  
}

load("phi_firmqtr_20_reg.Rdata")
plot(  apply(fcn_1_1_winsor(phi_firmqtr_20_reg),2,mean,na.rm=T), type="o")

#x_firmqtr_20_trunc = phiphihat_firmqtr_20_reg
#x_firmqtr_20_trunc = phiphihat_firmqtr_20_reg


x_firmqtr_20_trunc  = fcn_1_1_winsor(phi_firmqtr_20_reg) 
x_firmqtr_20_trunc  = fcn_1_1_winsor(phihat_firmqtr_20_reg) 
x_firmqtr_20_trunc  = fcn_1_1_winsor(phiphihat_firmqtr_20_reg) 
x_yearqtr_adj_firm_avg = rep(NA, N_row_yearqtr )
x_yearqtr_adj_firm_SE =  rep(NA, N_row_yearqtr )
phiphihat_yearqtr_adj_firm_SS=  rep(NA, N_row_yearqtr )
for (ii in seq( 1,N_row_yearqtr ) ) {    
  
  x_yearqtr_adj_firm = x_firmqtr_20_trunc[ yearqtr_idx_numeric   == yearqtr_xaxis[ii]   ]  
  x_yearqtr_adj_firm_avg[ii] = mean(x_yearqtr_adj_firm ,na.rm=T)
  x_yearqtr_adj_firm_SE[ii] = sd(x_yearqtr_adj_firm,na.rm=T)/sqrt( sum(!is.na(x_yearqtr_adj_firm)) )
  phiphihat_yearqtr_adj_firm_SS[ii] = var(x_yearqtr_adj_firm,na.rm=T)
}

plot(x_yearqtr_adj_firm_avg)
ts.plot(phiphihat_yearqtr_adj_firm_SS)



save(phiphihat_yearqtr_adj_firm_SS, file="phiphihat_yearqtr_adj_firm_SS")
 

plot( (x_yearqtr_adj_firm_avg *( 20 - 1)+1) / ( 20 - 4)   ,lty=3 )

 
 




#######  2. learning phi calendar time using lmer  #######


load("phiphihat_firmqtr_20_reg.Rdata")
  

if (do_all) {  
  
  phi_learn_lmer00 = rep(NA, length(yearqtr_xaxis) )
  phi_learn_lmer_var00 = rep(NA, length(yearqtr_xaxis) ) 
  phihat_learn_lmer00 = rep(NA, length(yearqtr_xaxis) )
  phihat_learn_lmer_var00 = rep(NA,length(yearqtr_xaxis) ) 
  phiphihat_learn_lmer00 = rep(NA, length(yearqtr_xaxis) )
  phiphihat_learn_lmer_var00 = rep(NA, length(yearqtr_xaxis) ) 
  FE_rho_learn_pooled.reg = rep(NA, length(yearqtr_xaxis) ) 
  
  for (ii in c( 1:length(yearqtr_xaxis)   )) {    
    
    total_idx_temp = rep(FALSE, length(est$ye_firm_deflated)) 
    set_firm_idx_temp = est$firm_deflated_nn_idx[  yearqtr_idx_numeric   == yearqtr_xaxis[ii]  &  nonNA_index_firm<21 &  nonNA_index_firm>0   ]
    
    for (jj in  set_firm_idx_temp  )
    { 
      
      firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]          
      total_idx_temp[  firm_idx_temp[!is.na(phiphihat_firmqtr_20_reg[firm_idx_temp])]  ]=TRUE     
      
    }
    
     
    tryCatch({      
      
      M0.phiphihat = lmer(ye_temp ~ ye_temp_lag1+ (1| firm_dummy ) + (-1 + ye_temp_lag1 | firm_dummy )   ,subset = total_idx_temp,  REML=TRUE)
      phi_learn_lmer00[ii] = fixef(M0.phiphihat)[2]
      phi_learn_lmer_var00[ii] =  (attr( VarCorr(M0.phiphihat)$firm_dummy.1 ,"stddev")[1])^2
      
      M0.phiphihat = lmer(yf_temp ~ ye_temp_lag1+ (1| firm_dummy ) + (-1 + ye_temp_lag1 | firm_dummy )   ,subset = total_idx_temp,  REML=TRUE)
      phihat_learn_lmer00[ii] = fixef(M0.phiphihat)[2]
      phihat_learn_lmer_var00[ii] =  (attr( VarCorr(M0.phiphihat)$firm_dummy.1 ,"stddev")[1])^2
      
      M0.phiphihat = lmer(FE_temp ~ ye_temp_lag1+ (1| firm_dummy ) + (-1 + ye_temp_lag1 | firm_dummy )   ,subset = total_idx_temp,  REML=TRUE)
      phiphihat_learn_lmer00[ii] = fixef(M0.phiphihat)[2]
      phiphihat_learn_lmer_var00[ii] =  (attr( VarCorr(M0.phiphihat)$firm_dummy.1 ,"stddev")[1])^2
   
      
      M0 = lm(FE_temp  ~  FE_temp_lag1    ,subset = total_idx_temp )          
      FE_rho_learn_pooled.reg[ii ] = summary(M0)$coef[2,1]
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
                         erroroccur = TRUE} )  
    
    print(ii)
  }
  
  phi_learn = list(phiest = phi_learn_lmer00, phivar = phi_learn_lmer_var00,
                   phihatest = phihat_learn_lmer00, phihatvar = phihat_learn_lmer_var00,
                   phiphihatest = phiphihat_learn_lmer00, phiphihatvar = phiphihat_learn_lmer_var00 )
  
  save(phi_learn, file="phi_learn_lmer00_cal.Rdata") 
  
  save(FE_rho_learn_pooled.reg, file="FE_rho_learn_pooled_reg_cal.Rdata" )

  
  
}




load("phi_learn_lmer00_cal.Rdata") 
#load("phi_learn_lmer00.Rdata") 
ts.plot( phi_learn$phiphihatvar ,type="o")
ts.plot( phi_learn$phivar ,type="o")


ts.plot( phi_learn$phiphihatest ,type="o")
ts.plot( phi_learn$phiest ,type="o")


#######  3. calibrate info quality and phi  ####### 


load("phi_learn_lmer00_cal.Rdata") 

fcn_inv_sigphinp <- function(x) {       
  sigphihat2 =x[1]
  signp2 = x[2]
  whatp =  sigphihat2/(signp2  + sigphihat2  )
  y = c(whatp^2*(sigphi2 + signp2 )           - phihatvar_t, 
       (1-whatp)^2*sigphi2 + whatp^2*signp2   - phiphihatvar_t)           
  
  return(y)
}

hatvar_phi = rep(NA,length(yearqtr_xaxis))
var_n_phi = rep(NA,length(yearqtr_xaxis))

for (ii in c( 1:length(yearqtr_xaxis)   )) { 
  sigphi2 = phi_learn$phivar[ii]
  phihatvar_t = phi_learn$phihatvar[ii]
  phiphihatvar_t = phi_learn$phiphihatvar[ii]
  #nonlinear equation solver for ( varhat_phi, var_n_phi  )
  theta.start = c( sigphi2 ,  sigphi2/4 ) # c( phi, phihat, Khat)
  sols = nleqslv( theta.start , fcn_inv_sigphinp,  control=list(btol=.001))
  hatvar_phi[ii] = sols$x[1]
  var_n_phi[ii] = sols$x[2] 
  print(ii)
}

# var_t(phi_i-phihat_i) & info quality measure
plot(yearqtr_xaxis[1:111],phi_learn$phiphihatvar[1:111] ,type="l", ylim = c(0,0.02))
lines(yearqtr_xaxis[1:111],var_n_phi[1:111], lty=3)

# var_t(phi_i) & hat(var)(phi_i) in prior of phi_i
plot(yearqtr_xaxis[1:111],phi_learn$phivar[1:111], type="l", ylim = c(0,0.12))
lines(yearqtr_xaxis[1:111],hatvar_phi[1:111],lty=3)


# info quality measure
plot(yearqtr_xaxis[1:111],var_n_phi[1:111], type="o")
plot(yearqtr_xaxis[1:111],var_n_phi[1:111]/phi_learn$phivar[1:111], type="o")



#######  4. learning mu calendar time using lmer  ####### 


load("phiphihat_firmqtr_20_reg.Rdata")


if (do_all) {  
  
  mu_learn_lmer00 = rep(NA, length(yearqtr_xaxis) )
  mu_learn_lmer_var00 = rep(NA, length(yearqtr_xaxis) ) 
  muhat_learn_lmer00 = rep(NA, length(yearqtr_xaxis) )
  muhat_learn_lmer_var00 = rep(NA,length(yearqtr_xaxis) ) 
  mumuhat_learn_lmer00 = rep(NA, length(yearqtr_xaxis) )
  mumuhat_learn_lmer_var00 = rep(NA, length(yearqtr_xaxis) ) 
  
  for (ii in c( 1:length(yearqtr_xaxis)   )) {    
    
    total_idx_temp = rep(FALSE, length(est$ye_firm_deflated)) 
    set_firm_idx_temp = est$firm_deflated_nn_idx[  yearqtr_idx_numeric   == yearqtr_xaxis[ii]  &  nonNA_index_firm<21 &  nonNA_index_firm>0   ]
    
    for (jj in  set_firm_idx_temp  )
    { 
      
      firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]          
      total_idx_temp[  firm_idx_temp[!is.na(phiphihat_firmqtr_20_reg[firm_idx_temp])]  ]=TRUE     
      
    }
    
    
    tryCatch({      
      
      M0.mumuhat = lmer(ye_temp ~ ye_temp_lag1+ (1| firm_dummy )    ,subset = total_idx_temp,  REML=TRUE)
      mu_learn_lmer00[ii] = fixef(M0.mumuhat)[1]/(1- fixef(M0.mumuhat)[2])
      mu_learn_lmer_var00[ii] =  (attr( VarCorr(M0.mumuhat)$firm_dummy ,"stddev")[1])^2/(1- fixef(M0.mumuhat)[2])^2
      
      M0.mumuhat = lmer(yf_temp ~ ye_temp_lag1+ (1| firm_dummy )    ,subset = total_idx_temp,  REML=TRUE)
      muhat_learn_lmer00[ii] = fixef(M0.mumuhat)[1]/(1- fixef(M0.mumuhat)[2])
      muhat_learn_lmer_var00[ii] =  (attr( VarCorr(M0.mumuhat)$firm_dummy ,"stddev")[1])^2/(1- fixef(M0.mumuhat)[2])^2
      
      M0.mumuhat = lmer(FE_temp ~ ye_temp_lag1+ (1| firm_dummy )    ,subset = total_idx_temp,  REML=TRUE)
      mumuhat_learn_lmer00[ii] = fixef(M0.mumuhat)[1]/(1- fixef(M0.mumuhat)[2])
      mumuhat_learn_lmer_var00[ii] =  (attr( VarCorr(M0.mumuhat)$firm_dummy ,"stddev")[1])^2/(1- fixef(M0.mumuhat)[2])^2
      
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
                         erroroccur = TRUE} )  
    
    print(ii)
  }
  
  mu_learn = list(muest = mu_learn_lmer00, muvar = mu_learn_lmer_var00,
                   muhatest = muhat_learn_lmer00, muhatvar = muhat_learn_lmer_var00,
                   mumuhatest = mumuhat_learn_lmer00, mumuhatvar = mumuhat_learn_lmer_var00 )
  
  save(mu_learn, file="mu_learn_lmer00_cal.Rdata") 
  
  
  
  
  
}

load("mu_learn_lmer00_cal.Rdata") 
#load("mu_learn_lmer00.Rdata") 
ts.plot( mu_learn$mumuhatvar ,type="o")
ts.plot( mu_learn$muhatvar ,type="o")
lines( mu_learn$muvar,lty=3)
## CS-variation in target quantiles


ts.plot( mu_learn$mumuhatest ,type="o")
ts.plot( mu_learn$muhatest ,type="o")
lines( mu_learn$muest,lty=3)
## time-variation in target quantiles



#######  5. calibrate info quality and mu ###############
 

load("mu_learn_lmer00_cal.Rdata") 

fcn_inv_sigmunp <- function(x) {       
  sigmuhat2 =x[1]
  signp2 = x[2]
  whatp =  sigmuhat2/(signp2  + sigmuhat2  )
  y = c(whatp^2*(sigmu2 + signp2 )           - muhatvar_t, 
        (1-whatp)^2*sigmu2 + whatp^2*signp2   - mumuhatvar_t)           
  
  return(y)
}

hatvar_mu = rep(NA,length(yearqtr_xaxis))
var_n_mu = rep(NA,length(yearqtr_xaxis))

for (ii in c( 1:length(yearqtr_xaxis)   )) { 
  sigmu2 = mu_learn$muvar[ii]
  muhatvar_t = mu_learn$muhatvar[ii]
  mumuhatvar_t = mu_learn$mumuhatvar[ii]
  #nonlinear equation solver for ( varhat_mu, var_n_mu  )
  theta.start = c( sigmu2 ,  sigmu2/4 ) # c( mu, muhat, Khat)
  tryCatch({      
    
    sols = nleqslv( theta.start , fcn_inv_sigmunp,  control=list(btol=.001))
    
    hatvar_mu[ii] = sols$x[1]
    var_n_mu[ii] = sols$x[2] 
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
                       erroroccur = TRUE} )  
  

  print(ii)
}

# var_t(mu_i-muhat_i) & info quality measure
plot(yearqtr_xaxis[1:111],mu_learn$mumuhatvar[1:111] ,type="l" )
plot(yearqtr_xaxis[1:111],var_n_mu[1:111], lty=3)

# var_t(mu_i) & hat(var)(mu_i) in prior of mu_i
plot(yearqtr_xaxis[1:111],mu_learn$muvar[1:111], type="l" )
lines(yearqtr_xaxis[1:111],hatvar_mu[1:111],lty=3)

  
   

#######  6. decompose ###############




#######################################################################
#######################################################################
#######  model implied autocorr in FE due to learning of phi  #########
#######################################################################
####################################################################### 

require(nleqslv)
require(tmvtnorm)

#### matching mean cov for turncated normal
invTrcNorm <- function(x) { 
  xx  = matrix(c(x[1],x[2],0,x[3]),ncol=2)  
  fcnval = mtmvnorm(x[c(4,5)], xx%*%t(xx), lower=c(-1,-1),  upper=c(1,1) )
  y = c( (fcnval$tvar-COV_phiphihat)[c(1,2,4)], (fcnval$tmean - c(true.phi0, est.phi0)) )
  
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

NN=10^6

## set parameters for calculations
true.phi0 = 0.5
est.phi0 = 0.5

true.sigma.a = 0 # almost no effect on FE_rho.
true.sigma.e = 1
true.sigma.n = 0.5

load("phi_learn_lmer00_cal.Rdata") 
var_est_phi       = (phi_learn$phivar)
var_est_phihat    = (phi_learn$phihatvar)
var_est_phiphihat = (phi_learn$phiphihatvar)
cov_est_phiphihat = (var_est_phi+var_est_phihat-var_est_phiphihat)/2
COV_phiphihat_set = matrix(rbind(var_est_phi,cov_est_phiphihat,cov_est_phiphihat, var_est_phihat),nrow=4)

phi_learn_imp_rho_FE = rep(NA, length(var_est_phi ) )

if (do_all){ 
  for (ii in seq(1, (length(var_est_phi )-1)   )) {
    COV_phiphihat = matrix( COV_phiphihat_set[,ii], 2,2)
    
    sols = nleqslv( invTrcNorm.start() , invTrcNorm, control=list(btol=.001))
    t_chol_COV_phiphihat_adj = matrix(c(sols$x[1],sols$x[2],0,sols$x[3]),ncol=2)
    true_est_phi0_adj  = sols$x[4:5]  
    
    if (prod(diag(COV_phiphihat)==0)==1) e_phi = 0*matrix(rnorm(2*NN),nrow=2) else 
      e_phi = t_chol_COV_phiphihat_adj%*%matrix(rnorm(2*NN),nrow=2)
    
    true.phi = true_est_phi0_adj[1] +   e_phi[1,]
    est.phi = true_est_phi0_adj[2] +  e_phi[2,]
    
    statidx = abs(true.phi)<1 & abs(est.phi)<1 
    true.phi = true.phi[statidx] 
    est.phi = est.phi[statidx] 
    
    ## computation
    true.w = 1/true.sigma.n^2/(1/true.sigma.n^2+1/true.sigma.e^2)
    true.pis2 = 1/(1/true.sigma.e^2+1/true.sigma.n^2)
    est.w  = true.w
    
    ### b/c pospos_varE = 1/(1/pos_varE + 1/(true.sigma.a^2 + pospos_varE*true.phi^2)  );
    a = 1
    b = (1-true.phi^2)*true.sigma.a^2 - true.pis2
    c = -true.pis2*true.sigma.a^2
    
    ### a x^2 + bx + c = 0
    true.pit1t2 = ( -b + sqrt(   b^2 - 4*a*c  )   ) /a / 2  ;
    true.Kx = true.pit1t2/(true.sigma.a^2 + true.pit1t2  )     ;
    est.Kx = true.Kx
    
    varY = ( (1+true.phi^2)*true.sigma.a^2 + true.sigma.e^2 - 2*true.phi^2*true.sigma.a^2  )/(1-true.phi^2)
    covYYh = (true.phi*est.phi*est.Kx*(varY - true.sigma.a^2) + est.w*true.sigma.e^2  )/(1-true.phi*est.phi*(1-est.Kx)); 
    varYh = ( est.phi^2*est.Kx^2*varY + est.w^2*(true.sigma.e^2+true.sigma.n^2) + 2*est.phi^2*(1-est.Kx)*est.Kx*covYYh  )/(1-est.phi^2*(1-est.Kx)^2); 
    
    covYY1 = true.phi*( varY -  true.sigma.a^2);
    covYhYh1 = est.phi*( (1-est.Kx)*varYh + est.Kx*covYYh );
    covY1Yh = true.phi*covYYh;
    covYh1Y = est.phi*( (1-est.Kx)*covYYh + est.Kx*varY );
    
    ACF0 = varY + varYh - 2*covYYh;
    ACF1 = covYY1 + covYhYh1 - covY1Yh - covYh1Y;
    rho1 = ACF1/ACF0 
    
    phi_learn_imp_rho_FE[ii] = mean(rho1)
    
    
    print(ii)
  }
  save(phi_learn_imp_rho_FE, file="phi_learn_imp_rho_FE_cal.Rdata")
}
load("phi_learn_imp_rho_FE_cal.Rdata")
plot(yearqtr_xaxis[1:111],phi_learn_imp_rho_FE[1:111], type="o")

 
#### phi-phihat est (in ar(1)) decresese.
##### two possible causes: phi-phihat actually decreases, or Khat decreases
##### test...using 






#### Functions for step 3: update "rho(fe) due to var(phi-phihat)" estimate with better-calibrated parameters.


source("fcn_varphiphihat_imp_rho_FE_cal.r")



#### Functions for step 1: Calibration by MM with exact identification.



fcn_1_1_winsor <- function(x) {
  x[x>1] = 0.9999
  x[x<  (-1)] = -0.9999  
  return(x)  
}



fcn_calib_phi_phihat_Khat <- function() {
  
  source('fcn_implied_rho_phiphihat.r')  
  
  fcn_inv_phihat  <- function(x) {     
    Imp = fcn_implied_rho_phiphihat(x)  
    y = c(Imp$FE_rho+phi_learn_imp_rho_FE_t        - FE_rho_learn_est_t, 
          Imp$phi_ar_est                           - phiest_t, 
          x[1]-x[2]+phiphihat_ar_bias_t            - phiphihatest_t)           
    
    return(y)
  }
  
  phiest_ar.imp = rep(NA, length(phiest_ar))
  phihatest_ar.imp = phiest_ar.imp
  Khat.imp = phihatest_ar.imp
  
  for (ii in seq(1 , (length(phiest_ar)-1) )   ) {
    # moments to match
    phiest_t = phiest_ar[ii]
    phiphihatest_t = phiphihatest_ar[ii] 
    FE_rho_learn_est_t = FE_rho_learn_est[ii] 
    
    # implied moments to match
    phi_learn_imp_rho_FE_t = phi_learn_imp_rho_FE[ii] 
    phiphihat_ar_bias_t = phiphihat_ar_bias[ii] 
    
    #nonlinear equation solver for ( phi, phihat, Khat)
    theta.start = c(phiest_t, phiest_t-phiphihatest_t, 0.5) # c( phi, phihat, Khat)
    sols = nleqslv( theta.start , fcn_inv_phihat,  control=list(btol=.001))
    
    phiest_ar.imp[ii]  = sols$x[1]
    phihatest_ar.imp[ii] = sols$x[2]
    Khat.imp[ii] = sols$x[3]    
  }     
  
  return(list(phi0=phiest_ar.imp,phihat0=phihatest_ar.imp,Khat=Khat.imp))
  
}

####################################################################
####################################################################
####################################################################
############### rho(FE) decomposition by calibration ###############
####################################################################
####################################################################
####################################################################

#### Libraries ####
require(nleqslv)
require(tmvtnorm)

#### Global variables. #### 
true.sigma.a = 0     # almost no effect on FE_rho.
true.sigma.e = 1
true.sigma.n = 0.5
true.w = 1/true.sigma.n^2/(1/true.sigma.n^2+1/true.sigma.e^2)
true.what = true.w

#### Global variables: momnets to match. ####
# 1) unbiased phi estiamtes (t)
load("phi_learn_firm00_cal.Rdata") 
ar_phi_est_avg_adj = (phi_learn_firm$phiest *( 20  - 1)+1) / ( 20 - 4)
phiest_ar= apply( fcn_1_1_winsor(ar_phi_est_avg_adj),2,mean,na.rm=T)
# 2) biased phi-phihat estiamtes (t)
phiphihatest_ar = phi_learn_firm$phiphihatest  #+ 0.018 #*( 20  - 1)/( 20 - 4)
phiphihatest_ar = apply(fcn_1_1_winsor(phiphihatest_ar),2,mean,na.rm=T)
# 3) unbiased rho(FE) estimates (t)
load("FErhoadj_firmqtr_20_reg_cal.Rdata") 
FE_rho_learn_est = apply(fcn_1_1_winsor(FErhoadj_firmqtr_20_reg_cal),2,mean,na.rm=T)
#load("phi_learn_0.Rdata") 
#phiest_ar = (phi_learn$phiest *( 20  - 1)+1) / ( 20 - 4)
#phiphihatest_ar = phi_learn$phiphihatest 


#### Local variables: Initial values ####
calibpar= list()
calibpar$phi0 = phiest_ar
calibpar$phihat0 = phiest_ar-phiphihatest_ar
calibpar$Khat0 = phiest_ar*0+0.7

#### Locally global variables: Initial values ####
# phi_learn_imp_rho_FE 
# phiphihat_ar_bias 

## assume phi=phihat=0.5
load("phi_learn_imp_rho_FE_cal.Rdata")
phiphihat_ar_bias = phiphihatest_ar
phiest_ar_store = phiest_ar
phiest_ar = phiest_ar*0+0.5
for (kk in c(1:5)) {  
  calibpar = fcn_calib_phi_phihat_Khat()
  phi_imp_rho_FE =  fcn_varphiphihat_imp_rho_FE_cal(calibpar,NN=10^6)
  phi_learn_imp_rho_FE =  phi_imp_rho_FE[,1] - phi_imp_rho_FE[,2]
  
  save(phi_imp_rho_FE,file="phi_imp_rho_FE_phi05_cal.Rdata")
}
phiest_ar = phiest_ar_store
plot(calibpar$phi0,calibpar$phihat0)


## assume phi_t=phihat_t
load("phi_learn_imp_rho_FE_cal.Rdata")
phiphihat_ar_bias = phiphihatest_ar
for (kk in c(1:5)) {  
  calibpar = fcn_calib_phi_phihat_Khat()
  phi_imp_rho_FE =  fcn_varphiphihat_imp_rho_FE_cal(calibpar,NN=10^6)
  phi_learn_imp_rho_FE =  phi_imp_rho_FE[,1] - phi_imp_rho_FE[,2]
  
  save(phi_imp_rho_FE,file="phi_imp_rho_FE_phiphihat0_cal.Rdata")
}
plot(calibpar$phi0-calibpar$phihat0)



## assume bias and true moments of phi-phihat cancels out
phiphihat_ar_bias = phiphihatest_ar*0
load("phi_learn_imp_rho_FE_cal.Rdata")
for (kk in c(1:5)) {
  calibpar = fcn_calib_phi_phihat_Khat()
  phi_imp_rho_FE =  fcn_varphiphihat_imp_rho_FE_cal(calibpar,NN=10^6)
  phi_learn_imp_rho_FE =  phi_imp_rho_FE[,1] - phi_imp_rho_FE[,2]
  
  save(phi_imp_rho_FE,file="phi_imp_rho_FE_cal.Rdata")
}


 

#### step 0: Initiate the phi-phihat estimate to match (adjust bias)
#            Initiate "rho(fe) due to var(phi-phihat)" estimate 

#### step 1: Calibration by MM with exact identification.
#### moment matching
#### step 2: update the phi-phihat estimate to match (adjust bias)

#### step 3: update "rho(fe) due to var(phi-phihat)" estimate with better-calibrated parameters.

#### Repeat step 1-3 until it converges.

 


########################### plot in the paper ##########################
########################### plot in the paper ##########################
########################### plot in the paper ##########################
###########################  rho_dynamic_decomposition_cal.eps ##########################
par(mfrow=c(1,2))

load("FErhoadj_firmqtr_20_reg_cal.Rdata") 
FE_rho_learn_est = apply(fcn_1_1_winsor(FErhoadj_firmqtr_20_reg_cal),2,mean,na.rm=T)
load("phi_imp_rho_FE_phi05_cal.Rdata")
load("FE_rho_learn_pooled_reg_cal.Rdata")
 

xaxis  =  yearqtr_xaxis
Tmaxplot = 111

plot(NA,xlim=c(xaxis[1]-0.5, xaxis[length(xaxis)]), ylim=c(0, 0.33 ),  
     main=expression('Autocorr(FE'[italic("1:20")]*')'),
     xlab=expression("Quarter "*italic("t")*" since the first observation"),ylab="",
     cex.axis=0.8, tck= -0.02)
polygon( c( xaxis[1:Tmaxplot], xaxis[ seq(Tmaxplot,1,by=-1) ] ),  
         c( rep(0,Tmaxplot),  ((phi_imp_rho_FE[,3]))[Tmaxplot:1]), border = NA, col = "gray75")
lines(xaxis[1:Tmaxplot], FE_rho_learn_est[1:Tmaxplot], cex=0.75, lwd=1  ,type="o" )
lines(xaxis[1:Tmaxplot], FE_rho_learn_pooled.reg[1:Tmaxplot], cex=0.5, lwd=1  ,type="l", lty=2)
#lines(xaxis[1:Tmaxplot], phi_imp_rho_FE[1:Tmaxplot,2]  , cex=0.5, lwd=1  ,type="l", lty=1  )
abline( h=0, lty=3 )
box()

legend("topleft", inset=0.02,
       c(expression("from ("*hat(K)<K*")  +  Var("*phi[i]-hat(phi)[i]*")>0   +  Var("*mu[i]-hat(mu)[i]-L[i]*")>0"  ),
         expression("from ("*hat(K)<K*")  +  Var("*phi[i]-hat(phi)[i]*")>0"  ),
         expression("from ("*hat(K)<K*")")),
       lty=c(2,1,1), lwd=c(1,1,7), bty = "n",
       col=c("black","black","gray75"), pch=c(NA,1,NA), cex=0.58,y.intersp=1.5)




plot(NA,xlim=c(xaxis[1]-0.5, xaxis[length(xaxis)]), ylim=c(0, 1.4),  
     main=expression('Autocorr(FE'[italic("1:20")]*')'),
     xlab=expression("Quarter "*italic("t")*" since the first observation"),ylab="",
     cex.axis=0.8, tck= -0.02, yaxt='n'  )
polygon(  c( xaxis[1:Tmaxplot], xaxis[ seq(Tmaxplot,1,by=-1) ] ), 
         c( rep(0,Tmaxplot),  ((phi_imp_rho_FE[,3]))[Tmaxplot:1]/((phi_imp_rho_FE[,3])[1])), border = NA, col = "gray75")
#lines(xaxis[1:Tmaxplot], phi_imp_rho_FE[1:Tmaxplot,3]/(phi_imp_rho_FE[1,3])   , cex=0.5, lwd=1  ,type="l", lty=1  )
lines(xaxis[1:Tmaxplot], (phi_imp_rho_FE[1:Tmaxplot,1]-phi_imp_rho_FE[1:Tmaxplot,2])/(phi_imp_rho_FE[1,1]-phi_imp_rho_FE[1,2])  , cex=0.5, lwd=1  ,type="l", lty=1  )

axis(2, at=seq(0,1,by=0.2), labels=c("0%","20%","40%","60%","80%","100%"), las=1, cex.axis=0.6, tck= -0.01, hadj=0.435)
#mtext("(%)",side=4,line=0.5,adj=1)
abline( h=1, lty=3 )
abline( h=0, lty=3 )
box()
legend("topleft", inset=0.02,
       c(expression("from Var("*phi[i]-hat(phi)[i]*")>0"  ),
         expression("from ("*hat(K)<K*")")),
       lty=c(1,1), lwd=c(1,7), bty = "n",
       col=c("black","gray75"), pch=c(NA,NA), cex=0.58,y.intersp=1.5)






par(mfrow=c(1,2))

load("phi_imp_rho_FE_cal.Rdata")
load("FE_rho_learn_pooled_reg_cal.Rdata")
load("FErhoadj_firmqtr_20_reg_cal.Rdata") 
FE_rho_learn_est = apply(fcn_1_1_winsor(FErhoadj_firmqtr_20_reg_cal),2,mean,na.rm=T)

xaxis  =  yearqtr_xaxis
Tmaxplot = 111

plot(NA,xlim=c(xaxis[1]-0.5, xaxis[length(xaxis)]), ylim=c(0, 0.33 ),  
     main=expression('Autocorr(FE'[italic("1:20")]*')'),
     xlab=expression("Quarter "*italic("t")*" since the first observation"),ylab="",
     cex.axis=0.8, tck= -0.02)
polygon( c( xaxis[1:Tmaxplot], xaxis[ seq(Tmaxplot,1,by=-1) ] ),  
         c( rep(0,Tmaxplot),  ((phi_imp_rho_FE[,3]))[Tmaxplot:1]), border = NA, col = "gray75")
lines(xaxis[1:Tmaxplot], FE_rho_learn_est[1:Tmaxplot], cex=0.75, lwd=1  ,type="o" )
lines(xaxis[1:Tmaxplot], FE_rho_learn_pooled.reg[1:Tmaxplot], cex=0.5, lwd=1  ,type="l", lty=2)
lines(xaxis[1:Tmaxplot], phi_imp_rho_FE[1:Tmaxplot,2]  , cex=0.5, lwd=1  ,type="l", lty=1  )
#lines(xaxis[1:Tmaxplot], phi_imp_rho_FE[1:Tmaxplot,3]  , cex=0.5, lwd=1  ,type="l", lty=3  )
abline( h=0, lty=3 )
box()

legend("topleft", inset=0.02,
       c(expression("from ("*hat(K)<K*")  +  E("*phi-hat(phi)*")>0   +  Var("*phi[i]-hat(phi)[i]*")>0   +  Var("*mu[i]-hat(mu)[i]-L[i]*")>0"  ),
         expression("from ("*hat(K)<K*")  +  E("*phi-hat(phi)*")>0   +  Var("*phi[i]-hat(phi)[i]*")>0"  ),
         expression("from ("*hat(K)<K*")  +  E("*phi-hat(phi)*")>0   +  Var("*phi[i]-hat(phi)[i]*")>0"  ),
         expression("from ("*hat(K)<K*")")),
       lty=c(2,1,1,1), lwd=c(1,1,1,7), bty = "n",
       col=c("black","black","black","gray75"), pch=c(NA,1,NA,NA), cex=0.58,y.intersp=1.5)




plot(NA,xlim=c(xaxis[1]-0.5, xaxis[length(xaxis)]), ylim=c(0, 1.4),  
     main=expression('Autocorr(FE'[italic("1:20")]*')'),
     xlab=expression("Quarter "*italic("t")*" since the first observation"),ylab="",
     cex.axis=0.8, tck= -0.02, yaxt='n'  )
polygon(  c( xaxis[1:Tmaxplot], xaxis[ seq(Tmaxplot,1,by=-1) ] ), 
          c( rep(0,Tmaxplot),  ((phi_imp_rho_FE[,3]))[Tmaxplot:1]/((phi_imp_rho_FE[,3])[1])), border = NA, col = "gray75")
#lines(xaxis[1:Tmaxplot], phi_imp_rho_FE[1:Tmaxplot,3]/(phi_imp_rho_FE[1,3])   , cex=0.5, lwd=1  ,type="l", lty=1  )
lines(xaxis[1:Tmaxplot], (phi_imp_rho_FE[1:Tmaxplot,1]-phi_imp_rho_FE[1:Tmaxplot,2])/(phi_imp_rho_FE[1,1]-phi_imp_rho_FE[1,2])  , cex=0.5, lwd=1  ,type="l", lty=1  )
lines(xaxis[1:Tmaxplot], (phi_imp_rho_FE[1:Tmaxplot,2]-phi_imp_rho_FE[1:Tmaxplot,3])/(phi_imp_rho_FE[1,2]-phi_imp_rho_FE[1,3])  , cex=0.5, lwd=1  ,type="l", lty=2  )

axis(2, at=seq(0,1,by=0.2), labels=c("0%","20%","40%","60%","80%","100%"), las=1, cex.axis=0.6, tck= -0.01, hadj=0.435)
#mtext("(%)",side=4,line=0.5,adj=1)
abline( h=1, lty=3 )
abline( h=0, lty=3 )
box()
legend("topleft", inset=0.02,
       c(expression("from Var("*phi[i]-hat(phi)[i]*")>0"  ),
         expression("from E("*phi[i]-hat(phi)[i]*")>0"  ),
         expression("from ("*hat(K)<K*")")),
       lty=c(1,2,1), lwd=c(1,1,7), bty = "n",
       col=c("black","black","gray75"), pch=c(NA,NA,NA), cex=0.58,y.intersp=1.5)




########################### end of plot in the paper ##########################
########################### end of plot in the paper ##########################
########################### end of plot in the paper ##########################

# working














































 













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





























 


source("fcn_KF_VARMA11_LTY_est_ver2.r")
source("fcn_KF_VARMA11_LTY_est_random_firm_effect.r")
source("fcn_MM_VARMA11_LTY_est.r")


require(FKF)

fixedeffect=FALSE
yt= fcn_VARMA_data_make(scaleVset=scaleVset, option=option, fixedeffect=fixedeffect)
KFfit = fcn_KF_VARMA11_LTY_est(yt, mueest=TRUE)
KFfit$par 

fixedeffect=TRUE
yt= fcn_VARMA_data_make(scaleVset=scaleVset, option=option, fixedeffect=fixedeffect)
KFfit_fe = fcn_KF_VARMA11_LTY_est(yt, mueest=TRUE)
KFfit_fe$par 

KFfit = fcn_KF_VARMA11_LTY_est_random_firm_effect(yt)

pdataset = list( y=firm_TS_data$ye_firm_deflated  ,   begin_idx =firm_TS_data$firm_begin_deflated, end_idx = firm_TS_data$firm_end_deflated)
yyy1 = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=100  )
pdataset = list( y=firm_TS_data$yf_firm_deflated  ,   begin_idx =firm_TS_data$firm_begin_deflated, end_idx = firm_TS_data$firm_end_deflated)
yyy2 = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=100  )
yt = matrix(rbind(yyy1,yyy2),nrow=2) 




############# pooled rho(FE) learning within a firm ##################
source("fcn_MM_VARMA11_LTY_est.r")
max_no_obs_deflated = max(est$no_obs_deflated)
no_window_learn = max_no_obs_deflated-20+1

if (do_all) {

  fixedeffect=FALSE
  yt= fcn_VARMA_data_make(scaleVset=scaleVset, option=option, fixedeffect=fixedeffect)
   
  KFfit_learn_all=c()
  
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
      
      KFfit_learn_all = rbind(KFfit_learn_all,fcn_KF_VARMA11_LTY_est(yt[,total_idx_temp_pooled],mueest=TRUE)$par)
  
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
                         erroroccur = TRUE} )  
    
    print(ii)
  }
  
  save(KFfit_learn_all, file="KFfit_learn_all.Rdata")  
  
}

KFfit_K_w_rho_all=c()
for (ii in c( 20:max_no_obs_deflated   )) {  
  
  tryCatch({
    KFfit_K_w_rho_all = rbind(KFfit_K_w_rho_all,fcn_implied_K_w_rho( matrix(KFfit_learn_all[ii-19,3:9] ,ncol=1) ))

  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
                       erroroccur = TRUE} )  
}
KFfit_K_w_rho_all = matrix( as.numeric(KFfit_K_w_rho_all),ncol=3)

plot(KFfit_learn_all[,"Khat"],type="o" ,ylim=c(-0.2,1))
lines( KFfit_K_w_rho_all[,2] )

plot(KFfit_learn_all[,"what"],type="o" ,ylim=c(0.7,1))
plot( KFfit_K_w_rho_all[,1] -KFfit_learn_all[,"what"],type="o")

plot( KFfit_K_w_rho_all[,3] ,type="o")

plot(KFfit_learn_all[,"phi"],type="o"  ,ylim=c(0.35,0.57))
plot(exp(KFfit_learn_all[,"logsigmaa"]-KFfit_learn_all[,"logsigmae"]),type="o"  )
plot(KFfit_learn_all[,"phi"]-KFfit_learn_all[,"phihat"],type="o"  )

plot(ARMA_learn_ar-KFfit_learn_all[,"phihat"],type="o"  )























############# pooled rho(FE) learning within a firm ##################

max_no_obs_deflated = max(est$no_obs_deflated)
no_window_learn = max_no_obs_deflated-20+1

if (do_all) {
   
  #ARMA_learn_ar=rep(NA,max_no_obs_deflated-19)
  #ARMA_learn_ma=rep(NA,max_no_obs_deflated-19)
  ARMA_learn_KF=matrix(rep(NA, (max_no_obs_deflated-19)*4)  ,ncol=4)
  
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
      
      #m1=arima(ye_temp_pooled[ total_idx_temp_pooled] , c(1,0,1))   
            #ARMA_learn_ar[ii-19] =  m1$coef[1]
      #ARMA_learn_ma[ii-19] =  m1$coef[2]     
      ARMA_learn_KF[ii-19,] = fcn_KF_ARMA11_para_est(ye_temp_pooled[ total_idx_temp_pooled])$par
      
      #m1=lmer(ye_temp_pooled~  ye_temp_pooled_lag1 + (ye_temp_pooled_lag1|firm_dummy_pooled)  ,subset= total_idx_temp_pooled  , REML = T)
      #print(attr( VarCorr(m1)$firm_dummy_pooled ,"stddev")[1])
      
      #m1=lmer(ye_temp_pooled~  ye_temp_pooled_lag1 + (1|firm_dummy_pooled)  ,subset= total_idx_temp_pooled  , REML = T)
      #print(attr( VarCorr(m1)$firm_dummy_pooled ,"stddev")[1])
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
                         erroroccur = TRUE} )  
    
    print(ii)
  }
   
  
}




arima(yf_temp_pooled , c(1,0,1))     


plot(ARMA_learn_ar,type="o")
lines(ARMA_learn_KF[,2], type="l", lty=3)
plot(ARMA_learn_ma,type="o")
lines(ARMA_learn_KF[,3], type="l", lty=3)
plot(ARMA_learn_KF[,2]+ARMA_learn_KF[,3], type="l", lty=3)



xtemp=rep(NA,10)
for (jjj in seq(1:10)) {
  #xtemp[jjj]  = arima( 0.2/sqrt(1-0.85^2)*arima.sim(list(order = c(1,0,0), ar = 0*0.85), n = length(total_idx_temp) )   + (arima.sim(list(order = c(1,0,0), ar = 0.45), n = length(total_idx_temp) ) + 0*rnorm( 3778)[est$firm_deflated_nn_idx])[ total_idx_temp] , c(1,0,1))$coef[2] 
  xtemp[jjj]  = arima(  arima.sim(list(order = c(1,0,0), ar = 0.45), n = length(total_idx_temp_pooled) )[ total_idx_temp_pooled]  , c(1,0,1))$coef[2] 
  xtemp[jjj]  = arima(  arima.sim(list(order = c(1,0,1), ar = 0.45, ma = 0.05), n = length(total_idx_temp_pooled) )  , c(1,0,0))$coef[1] 

  print(xtemp[jjj] )
}
hist((xtemp))
mean(xtemp)
median(xtemp)




m1=lmer(ye_temp_pooled~  ye_temp_pooled_lag1 + (ye_temp_pooled_lag1|firm_dummy_pooled)    , REML = FALSE)
attr( VarCorr(m1)$firm_dummy_pooled ,"stddev")[1]
m1=lmer(ye_temp_pooled~  ye_temp_pooled_lag1 + (ye_temp_pooled_lag1|firm_dummy_pooled)    , REML = TRUE)
attr( VarCorr(m1)$firm_dummy_pooled ,"stddev")[1]

m1=lmer(ye_temp_pooled~  ye_temp_pooled_lag1 + (1|firm_dummy_pooled) + (-1+ye_temp_pooled_lag1|firm_dummy_pooled)    , REML = FALSE)
attr( VarCorr(m1)$firm_dummy_pooled ,"stddev")[1]
m1=lmer(ye_temp_pooled~  ye_temp_pooled_lag1 + (1|firm_dummy_pooled) + (-1+ye_temp_pooled_lag1|firm_dummy_pooled)    , REML = TRUE)
attr( VarCorr(m1)$firm_dummy_pooled ,"stddev")[1]

m1=lmer(ye_temp_pooled~  ye_temp_pooled_lag1 + (1|firm_dummy_pooled)    , REML = FALSE)
attr( VarCorr(m1)$firm_dummy_pooled ,"stddev")[1]
m1=lmer(ye_temp_pooled~  ye_temp_pooled_lag1 + (1|firm_dummy_pooled)     , REML = TRUE)
attr( VarCorr(m1)$firm_dummy_pooled ,"stddev")[1]





m1=lmer(yf_temp_pooled~  yf_temp_pooled_lag1 + (yf_temp_pooled_lag1|firm_dummy_pooled)    , REML = FALSE)
attr( VarCorr(m1)$firm_dummy_pooled ,"stddev")[1]
m1=lmer(yf_temp_pooled~  yf_temp_pooled_lag1 + (yf_temp_pooled_lag1|firm_dummy_pooled)    , REML = TRUE)
attr( VarCorr(m1)$firm_dummy_pooled ,"stddev")[1]

m1=lmer(yf_temp_pooled~  yf_temp_pooled_lag1 + (1|firm_dummy_pooled) + (-1+yf_temp_pooled_lag1|firm_dummy_pooled)    , REML = FALSE)
attr( VarCorr(m1)$firm_dummy_pooled ,"stddev")[1]
m1=lmer(yf_temp_pooled~  yf_temp_pooled_lag1 + (1|firm_dummy_pooled) + (-1+yf_temp_pooled_lag1|firm_dummy_pooled)    , REML = TRUE)
attr( VarCorr(m1)$firm_dummy_pooled ,"stddev")[1]

m1=lmer(yf_temp_pooled~  yf_temp_pooled_lag1 + (1|firm_dummy_pooled)    , REML = FALSE)
attr( VarCorr(m1)$firm_dummy_pooled ,"stddev")[1]
m1=lmer(yf_temp_pooled~  yf_temp_pooled_lag1 + (1|firm_dummy_pooled)     , REML = TRUE)
attr( VarCorr(m1)$firm_dummy_pooled ,"stddev")[1]




m1=lmer(yf_temp_pooled~  ye_temp_pooled_lag1+ yf_temp_pooled_lag1 + (yf_temp_pooled_lag1|firm_dummy_pooled)    , REML = FALSE)
attr( VarCorr(m1)$firm_dummy_pooled ,"stddev")[1]
m1=lmer(yf_temp_pooled~  ye_temp_pooled_lag1+ yf_temp_pooled_lag1 + (yf_temp_pooled_lag1|firm_dummy_pooled)    , REML = TRUE)
attr( VarCorr(m1)$firm_dummy_pooled ,"stddev")[1]

m1=lmer(yf_temp_pooled~  ye_temp_pooled_lag1+ yf_temp_pooled_lag1 + (1|firm_dummy_pooled) + (-1+yf_temp_pooled_lag1|firm_dummy_pooled)   + (-1+ye_temp_pooled_lag1|firm_dummy_pooled)  , REML = FALSE)
attr( VarCorr(m1)$firm_dummy_pooled ,"stddev")[1]
m1=lmer(yf_temp_pooled~  ye_temp_pooled_lag1+ yf_temp_pooled_lag1 + (1|firm_dummy_pooled) + (-1+yf_temp_pooled_lag1|firm_dummy_pooled)  + (-1+ye_temp_pooled_lag1|firm_dummy_pooled)  , REML = TRUE)
attr( VarCorr(m1)$firm_dummy_pooled ,"stddev")[1]

m1=lmer(yf_temp_pooled~  ye_temp_pooled_lag1+ yf_temp_pooled_lag1 + (1|firm_dummy_pooled)    , REML = FALSE)
attr( VarCorr(m1)$firm_dummy_pooled ,"stddev")[1]
m1=lmer(yf_temp_pooled~  ye_temp_pooled_lag1+ yf_temp_pooled_lag1 + (1|firm_dummy_pooled)     , REML = TRUE)
attr( VarCorr(m1)$firm_dummy_pooled ,"stddev")[1]


m1=lmer(FE_temp_pooled~  FE_temp_pooled_lag1+  (1|firm_dummy_pooled)     , REML = TRUE)
attr( VarCorr(m1)$firm_dummy_pooled ,"stddev")[1]


m1=lmer(FE_temp_pooled~   (1|firm_dummy_pooled)     , REML = TRUE)
attr( VarCorr(m1)$firm_dummy_pooled ,"stddev")[1]


m1=lmer(ye_temp_pooled~   (1|firm_dummy_pooled)     , REML = TRUE)
attr( VarCorr(m1)$firm_dummy_pooled ,"stddev")[1]


m1=lmer(yf_temp_pooled~   (1|firm_dummy_pooled)     , REML = TRUE)
attr( VarCorr(m1)$firm_dummy_pooled ,"stddev")[1]

lmer(FE_temp_pooled~  ye_temp_pooled_lag1+ yf_temp_pooled_lag1 + (1|firm_dummy_pooled) + (-1 + ye_temp_pooled_lag1|firm_dummy_pooled) + (-1 +yf_temp_pooled_lag1|firm_dummy_pooled)    , REML = TRUE)


lmer( rt(length( firm_dummy_pooled),2) ~   (1|firm_dummy_pooled)     , REML = TRUE)
lmer( (rbinom(length( firm_dummy_pooled), 1,0.5)*2-1)*rexp(length( firm_dummy_pooled),2) ~   (1|firm_dummy_pooled)     , REML = TRUE)
lmer( (rt(length( firm_dummy_pooled),1.2)+(rbinom(length( firm_dummy_pooled), 1,0.5)*2-1)*rexp(length( firm_dummy_pooled),1)) ~   (1|firm_dummy_pooled)     , REML = TRUE)



summary(lm(ye_temp~FE_temp_lag1) )




############# pooled rho(FE) learning within a firm ##################

max_no_obs_deflated = max(est$no_obs_deflated)
no_window_learn = max_no_obs_deflated-20+1

if (do_all) {
  
  ARMA_length_ar=rep(NA,max_no_obs_deflated-28)
  ARMA_length_ma=rep(NA,max_no_obs_deflated-28)
  
  for (ii in c( 30:(max_no_obs_deflated+1)   )) {    
    
    
    
    total_idx_temp= (est$no_obs_deflated > (ii-11) & est$no_obs_deflated < ii    )[est$firm_deflated_nn_idx]
    pdataset = list(y=total_idx_temp, begin_idx =est$firm_begin_deflated, end_idx = est$firm_end_deflated)
    total_idx_temp_pooled = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )    
    
    tryCatch({
      
      m1=arima(ye_temp_pooled[ total_idx_temp_pooled] , c(1,0,1))     
      ARMA_length_ar[ii-19] =  m1$coef[1]
      ARMA_length_ma[ii-19] =  m1$coef[2]     
      
      m1=lmer(ye_temp_pooled~  ye_temp_pooled_lag1 + (ye_temp_pooled_lag1|firm_dummy_pooled)  ,subset= total_idx_temp_pooled  , REML = T)
      print(attr( VarCorr(m1)$firm_dummy_pooled ,"stddev")[1])
      
      m1=lmer(ye_temp_pooled~  ye_temp_pooled_lag1 + (1|firm_dummy_pooled)  ,subset= total_idx_temp_pooled  , REML = T)
      print(attr( VarCorr(m1)$firm_dummy_pooled ,"stddev")[1])
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
                         erroroccur = TRUE} )  
    
    print(ii)
  }
  
  
}



plot(ARMA_length_ar,type="o")
plot(ARMA_length_ma,type="o")



max_no_obs_deflated = max(est$no_obs_deflated)
no_window_learn = max_no_obs_deflated-20+1

if (do_all) {
  
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
          xxx0_0 = c(NA, xxx0[1:(length(xxx0)-1)])
          xxx0_1 = xxx0
          FErho_learn_0_reg[jj,ii-19] =  (lm(xxx0_1 ~ xxx0_0))$coef[2]          
           
          
        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
                             erroroccur = TRUE} )
      }
    }
    print(jj)
  }  
  
  FErho_learn_0_reg=  (FErho_learn_0_reg *( 20 - 1)+1) / ( 20 - 4)
 

}

yt_e0=yt
yt_e0[, FE_temp_pooled==0 ] = NA
m1_e0 = fcn_KF_VARMA11_LTY_est(yt_e0 ,mueest=TRUE)
m1_e0$par
fcn_implied_K_w_rho( matrix(as.numeric(m1_e0$par)[3:9], ncol=1) )
# sd(alpha)/sd(e) =  0.07068563






























source("fcn_KF_VARMA11_LTY_est_ver2.r")
source("fcn_KF_VARMA11_LTY_est_random_firm_effect.r")
source("fcn_MM_VARMA11_LTY_est.r")


require(FKF)

fixedeffect=FALSE
yt= fcn_VARMA_data_make(scaleVset=scaleVset, option=option, fixedeffect=fixedeffect)
 
fixedeffect=TRUE
yt_fe= fcn_VARMA_data_make(scaleVset=scaleVset, option=option, fixedeffect=fixedeffect)
 
 




 


############# pooled rho(FE) learning within a firm ##################
source("fcn_MM_VARMA11_LTY_est.r")
max_no_obs_deflated = max(est$no_obs_deflated)
no_window_learn = max_no_obs_deflated-20+1

if (do_all) {
  
  fixedeffect=FALSE
  yt= fcn_VARMA_data_make(scaleVset=scaleVset, option=option, fixedeffect=fixedeffect)
  fixedeffect=TRUE
  yt_fe= fcn_VARMA_data_make(scaleVset=scaleVset, option=option, fixedeffect=fixedeffect)
  
  #KFfit_learn_all=c()
  KFfit_learn_all_fe=c()
  KFfit_learn_all_fe_20=c()
  
  for (ii in c( 20:max_no_obs_deflated   )) {    
    
    total_idx_temp = rep(FALSE, length(est$ye_firm_deflated))
    total_idx_temp_20 = rep(FALSE, length(est$ye_firm_deflated))    
    
    for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
    {  
      firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]     
      nonNA_index_firm_temp = nonNA_index_firm[firm_idx_temp]     
      onetoT = est$TS_index_firm_deflated[firm_idx_temp]
      
      if (est$no_obs_deflated[jj] >= ii ) {        
        #total_idx_temp[firm_idx_temp[ onetoT[nonNA_index_firm_temp==ii-19]:onetoT[nonNA_index_firm_temp==ii] ]   ]=TRUE     
        total_idx_temp_20[firm_idx_temp[ onetoT[nonNA_index_firm_temp==1]:onetoT[nonNA_index_firm_temp==20] ]   ]=TRUE     
      }    
      
    }
    
    pdataset = list(y=total_idx_temp, begin_idx =est$firm_begin_deflated, end_idx = est$firm_end_deflated)
    total_idx_temp_pooled = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )    

    pdataset = list(y=total_idx_temp_20, begin_idx =est$firm_begin_deflated, end_idx = est$firm_end_deflated)
    total_idx_temp_pooled_20 = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )    
    
    
    tryCatch({
      
      #KFfit_learn_all = rbind(KFfit_learn_all,fcn_KF_VARMA11_LTY_est(yt[,total_idx_temp_pooled],mueest=TRUE)$par)
      #KFfit_learn_all_fe = rbind(KFfit_learn_all_fe,fcn_KF_VARMA11_LTY_est(yt_fe[,total_idx_temp_pooled],mueest=TRUE)$par)
      KFfit_learn_all_fe_20 = rbind(KFfit_learn_all_fe_20,fcn_KF_VARMA11_LTY_est(yt_fe[,total_idx_temp_pooled_20],mueest=TRUE)$par)
      
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
                         erroroccur = TRUE} )  
    
    print(ii)
  }
  
  #save(KFfit_learn_all, file="KFfit_learn_all.Rdata")  
  #save(KFfit_learn_all_fe, file="KFfit_learn_all_fe.Rdata")  
  save(KFfit_learn_all_fe_20, file="KFfit_learn_all_fe_20.Rdata")  
}

load("KFfit_learn_all.Rdata") 
#KFfit_K_w_rho_all=c()
#KFfit_K_w_rho_all_fe=c()
KFfit_K_w_rho_all_fe_20=c()
for (ii in c( 20:max_no_obs_deflated   )) {  
  
  tryCatch({
   # KFfit_K_w_rho_all = rbind(KFfit_K_w_rho_all,fcn_implied_K_w_rho( matrix(KFfit_learn_all[ii-19,3:9] ,ncol=1) ))
   # KFfit_K_w_rho_all_fe = rbind(KFfit_K_w_rho_all_fe,fcn_implied_K_w_rho( matrix(KFfit_learn_all_fe[ii-19,3:9] ,ncol=1) ))
    KFfit_K_w_rho_all_fe_20 = rbind(KFfit_K_w_rho_all_fe_20,fcn_implied_K_w_rho( matrix(KFfit_learn_all_fe[ii-19,3:9] ,ncol=1) ))

    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
                       erroroccur = TRUE} )  
}
#KFfit_K_w_rho_all = matrix( as.numeric(KFfit_K_w_rho_all),ncol=3)
#KFfit_K_w_rho_all_fe = matrix( as.numeric(KFfit_K_w_rho_all_fe),ncol=3)
KFfit_K_w_rho_all_fe_20 = matrix( as.numeric(KFfit_K_w_rho_all_fe_20),ncol=3)

#plot( KFfit_K_w_rho_all[,3] ,type="o")
#lines( KFfit_K_w_rho_all_fe[,3] ,type="l")
plot( KFfit_K_w_rho_all_fe_20[,3] ,type="o")

 

plot(KFfit_learn_all[,"Khat"],type="o" ,ylim=c(-0.2,1))
lines( KFfit_K_w_rho_all[,2] )

plot(KFfit_learn_all[,"what"],type="o" ,ylim=c(0.7,1))
plot( KFfit_K_w_rho_all[,1] -KFfit_learn_all[,"what"],type="o")

plot( KFfit_K_w_rho_all[,3] ,type="o")

plot(KFfit_learn_all[,"phi"],type="o"  ,ylim=c(0.35,0.57))
plot(exp(KFfit_learn_all[,"logsigmaa"]-KFfit_learn_all[,"logsigmae"]),type="o"  )
plot(KFfit_learn_all[,"phi"]-KFfit_learn_all[,"phihat"],type="o"  )

plot(ARMA_learn_ar-KFfit_learn_all[,"phihat"],type="o"  )







############# pooled rho(FE) learning within a firm ##################
source("fcn_MM_VARMA11_LTY_est.r")
max_no_obs_deflated = max(est$no_obs_deflated)
no_window_learn = max_no_obs_deflated-20+1

if (do_all) {
  
  fixedeffect=FALSE
  yt= fcn_VARMA_data_make(scaleVset=scaleVset, option=option, fixedeffect=fixedeffect)
  fixedeffect=TRUE
  yt_fe= fcn_VARMA_data_make(scaleVset=scaleVset, option=option, fixedeffect=fixedeffect)
  
  KFfit_learn_all=c()
  KFfit_learn_all_fe=c()
  KFfit_learn_all_fe_20=c()
  
  for (ii in c( 20:max_no_obs_deflated   )) {    
    
    total_idx_temp = rep(FALSE, length(est$ye_firm_deflated))
    total_idx_temp_20 = rep(FALSE, length(est$ye_firm_deflated))    
    
    for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
    {  
      firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]     
      nonNA_index_firm_temp = nonNA_index_firm[firm_idx_temp]     
      onetoT = est$TS_index_firm_deflated[firm_idx_temp]
      
      if (est$no_obs_deflated[jj] >= ii ) {        
        total_idx_temp[firm_idx_temp[ onetoT[nonNA_index_firm_temp==ii-19]:onetoT[nonNA_index_firm_temp==ii] ]   ]=TRUE     
        total_idx_temp_20[firm_idx_temp[ onetoT[nonNA_index_firm_temp==1]:onetoT[nonNA_index_firm_temp==20] ]   ]=TRUE     
      }    
      
    }
    
    pdataset = list(y=total_idx_temp, begin_idx =est$firm_begin_deflated, end_idx = est$firm_end_deflated)
    total_idx_temp_pooled = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )    
    
    pdataset = list(y=total_idx_temp_20, begin_idx =est$firm_begin_deflated, end_idx = est$firm_end_deflated)
    total_idx_temp_pooled_20 = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )    
    
    
    tryCatch({
      
      KFfit_learn_all = rbind(KFfit_learn_all,fcn_KF_VARMA11_LTY_est_restricted(yt[,total_idx_temp_pooled],mueest=TRUE)$par)
      KFfit_learn_all_fe = rbind(KFfit_learn_all_fe,fcn_KF_VARMA11_LTY_est_restricted(yt_fe[,total_idx_temp_pooled],mueest=TRUE)$par)
      KFfit_learn_all_fe_20 = rbind(KFfit_learn_all_fe_20,fcn_KF_VARMA11_LTY_est_restricted(yt_fe[,total_idx_temp_pooled_20],mueest=TRUE)$par)
      
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
                         erroroccur = TRUE} )  
    
    print(ii)
  }
  
  save(KFfit_learn_all, file="KFfit_learn_all_restricted.Rdata")  
  save(KFfit_learn_all_fe, file="KFfit_learn_all_fe_restricted.Rdata")  
  save(KFfit_learn_all_fe_20, file="KFfit_learn_all_fe_20_restricted.Rdata")  
}


### see phi-phihat change



load("KFfit_learn_all_restricted.Rdata") 
#KFfit_K_w_rho_all=c()
 KFfit_K_w_rho_all_fe=c()
KFfit_K_w_rho_all_fe_20=c()

load("KFfit_learn_all_fe.Rdata") 

load("KFfit_learn_all_fe_20.Rdata") 

for (ii in c( 20:max_no_obs_deflated   )) {  
  
  tryCatch({
    # KFfit_K_w_rho_all = rbind(KFfit_K_w_rho_all,fcn_implied_K_w_rho( matrix(KFfit_learn_all[ii-19,3:9] ,ncol=1) ))
      KFfit_K_w_rho_all_fe = rbind(KFfit_K_w_rho_all_fe,fcn_implied_K_w_rho( matrix(KFfit_learn_all_fe[ii-19,3:9] ,ncol=1) ))
    KFfit_K_w_rho_all_fe_20 = rbind(KFfit_K_w_rho_all_fe_20,fcn_implied_K_w_rho( matrix(KFfit_learn_all_fe_20[ii-19,3:9] ,ncol=1) ))
    
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
                       erroroccur = TRUE} )  
}
#KFfit_K_w_rho_all = matrix( as.numeric(KFfit_K_w_rho_all),ncol=3)
KFfit_K_w_rho_all_fe = matrix( as.numeric(KFfit_K_w_rho_all_fe),ncol=3)
KFfit_K_w_rho_all_fe_20 = matrix( as.numeric(KFfit_K_w_rho_all_fe_20),ncol=3)

#plot( KFfit_K_w_rho_all[,3] ,type="o")
#lines( KFfit_K_w_rho_all_fe[,3] ,type="l")
plot( KFfit_K_w_rho_all_fe_20[,3] ,type="o")
plot(KFfit_learn_all_fe[,3] - KFfit_learn_all_fe[,4] ,type="o")
plot(-KFfit_learn_all_fe[,5] + KFfit_K_w_rho_all_fe[,2] ,type="o")
plot(KFfit_learn_all_fe[,5] / KFfit_K_w_rho_all_fe[,2] ,type="o",ylim=c(0,1))
plot(KFfit_learn_all_fe[,6]- KFfit_K_w_rho_all_fe[,1] ,type="o")
plot( KFfit_K_w_rho_all_fe[,3]-KFfit_K_w_rho_all_fe_20[,3] ,type="o")

plot( KFfit_K_w_rho_all_fe[,3],type="o")
      lines(KFfit_K_w_rho_all_fe_20[,3] ,type="l",lty=3)




#####################################################################################
#####################################################################################
#####################################################################################
##### check whether using ar(1) to measure var(phi-phihat) is accuate enough.
#####################################################################################
#####################################################################################
#####################################################################################

est.phi = 0.48+rnorm(10^6)*0.1
est.phi = est.phi[abs(true.phi)<1]
true.phi = 0.48

## set parameters for calculations
true.phi = 0.48+rnorm(10^6)*0.1
true.phi =true.phi[abs(true.phi)<1]
est.phi = 0.48


true.phi = 0.48 #+ rnorm(10^6)*0.1
est.phi = 0.48

true.sigma.a = 0  # almost no effect on FE_rho.
true.sigma.e = 1
true.sigma.n = 0.5

## computation
true.w = 1/true.sigma.n^2/(1/true.sigma.n^2+1/true.sigma.e^2)
true.pis2 = 1/(1/true.sigma.e^2+1/true.sigma.n^2)
est.w  = true.w

### b/c pospos_varE = 1/(1/pos_varE + 1/(true.sigma.a^2 + pospos_varE*true.phi^2)  );
a = 1
b = (1-true.phi^2)*true.sigma.a^2 - true.pis2
c = -true.pis2*true.sigma.a^2

### a x^2 + bx + c = 0
true.pit1t2 = ( -b + sqrt(   b^2 - 4*a*c  )   ) /a / 2  ;
true.Kx = true.pit1t2/(true.sigma.a^2 + true.pit1t2  )     ;
est.Kx = true.Kx*0.6  #+ rnorm(10^6)*0.3

varY = ( (1+true.phi^2)*true.sigma.a^2 + true.sigma.e^2 - 2*true.phi^2*true.sigma.a^2  )/(1-true.phi^2)
covYYh = (true.phi*est.phi*est.Kx*(varY - true.sigma.a^2) + est.w*true.sigma.e^2  )/(1-true.phi*est.phi*(1-est.Kx)); 
varYh = ( est.phi^2*est.Kx^2*varY + est.w^2*(true.sigma.e^2+true.sigma.n^2) + 2*est.phi^2*(1-est.Kx)*est.Kx*covYYh  )/(1-est.phi^2*(1-est.Kx)^2); 

covYY1 = true.phi*( varY -  true.sigma.a^2);
covYhYh1 = est.phi*( (1-est.Kx)*varYh + est.Kx*covYYh );
covY1Yh = true.phi*covYYh;
covYh1Y = est.phi*( (1-est.Kx)*covYYh + est.Kx*varY );

# true regression coef yef(t) ~ y(t-1)
mean((covYY1-covYh1Y)/varY)
median((covYY1-covYh1Y)/varY)


ACF0 = varY + varYh - 2*covYYh;
ACF1 = covYY1 + covYhYh1 - covY1Yh - covYh1Y;
rho1 = ACF1/ACF0    
rho1 

covYY1/varY
covYh1Y/varY
(covYY1-covYh1Y)/varY
var((covYY1-covYh1Y)/varY,na.rm=T)

## therefore phi-phihat variance est usiing ar(1) is quite good.
## effect of var(Kx) is negligible.
true.phi*(  (1-est.Kx)*(  - covYY1 + varY)    -true.sigma.a^2   )
true.phi*( (1-est.Kx)*varY -  true.sigma.a^2 - (1-est.Kx)*covYYh  )
est.phi*( (1-est.Kx)*covYYh + est.Kx*varY )




#####################################################################################
#####################################################################################
### effect of NA in rho estimation. &  bias adjustment
#####################################################################################
#####################################################################################

arx=rep(NA,10^4)
regx=arx
for (ii in c(1:length(arx)))
{
  x=arima.sim(list(order = c(1,0,0), ar = 0.2), n = 30)
  #x[c(11:20)]=NA
  x = x[c(1:20)]
  x[7]=NA
   x[10]=NA
   x[15]=NA
  #arx[ii] = arima(x,c(1,0,0), method="ML")$coef[1]
  regx[ii] = lm( x[2:23]  ~ x[1:22]  )$coef[2 ]
}
mean(arx)
mean(regx)
range(arx)
range(regx)

regxx= (regx *( 20 - 1)+1) / ( 20 - 4)   
regxx= (regx *( 19 - 1)+1) / ( 19 - 4)   
regxx= (regx *( 18 - 1)+1) / ( 18 - 4)   

regxx[regxx>1] = 0.9999
regxx[regxx< (-1)] = -0.9999
mean(regxx)


arxx= (arx *( 20 - 1)+1) / ( 20 - 4)   
arxx= (arx *( 18 - 1)+1) / ( 18 - 4)   

arxx[arxx>1] =0.9999
arxx[arxx< (-1)] =-0.9999
mean(arxx)


 





max_no_obs_deflated = max(est$no_obs_deflated)
no_window_learn = max_no_obs_deflated-20+1

if (do_all) {
  
  phi_learning_ar =  matrix(rep(NA, no_window_learn*length(est$firm_begin_deflated )  ), ncol= no_window_learn) 
  
  for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
  {  
    firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]     
    nonNA_index_firm_temp = nonNA_index_firm[firm_idx_temp]
    xxx = ye_temp[firm_idx_temp]    
    onetoT = est$TS_index_firm_deflated[firm_idx_temp]
    
    for (ii in c( 20:max_no_obs_deflated)) {
      if (est$no_obs_deflated[jj] >= ii ) {
        tryCatch({
          xxx0=xxx[ onetoT[nonNA_index_firm_temp==ii-19]:onetoT[nonNA_index_firm_temp==ii] ]        
          phi_learning_ar[jj,ii-19] = arima( xxx0 , order=c(1,0,0), method = c("ML"),optim.control = list(maxit = 300000 ))$coef[1]     
          
        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
                             erroroccur = TRUE} )  
        
        
      }
    }
    print(jj)
  }  
  
  save(phi_learning_ar,file="phi_learning_ar.Rdata")
   
  
}
plot( apply( (phi_learning_ar*( 20  - 1)+1) / ( 20 - 4), 2,mean,na.rm=T  ), type="o")













### phi-phihat is fcn of phi, phihat, Khat, and also there is bias due to finite sample.
### The effect of Khat on the true moment and finite sample bias are almost cancelled out.
### The former is slightly bigger.

phi =0.5 
phihat=0.48
Khat = 1*0.7 
what = 0.8 

phiphihat_all=rep(NA,10^3)
rho_all=rep(NA,10^3)

for (jj in seq( 1 ,  length(phiphihat_all)  ))
{    
  
  Tsim = 20 + 50  
    
    et_sim = rnorm(Tsim)*1
    at_sim = rnorm(Tsim)*0.1
    nt_sim = rnorm(Tsim)*0.5
  
  ytsim1= rep(NA, Tsim )
  ytsim2= rep(NA, Tsim )
  
  
  ytsim1[1] = at_sim[1]
  ytsim2[1] = 0 
  
  for (ii  in seq(2,Tsim  ) )
  {    
    
    ytsim1[ii] = phi*ytsim1[ii-1] + et_sim[ii] + at_sim[ii] - phi*at_sim[ii-1]     
    ytsim2[ii] = phihat*Khat*ytsim1[ii-1] + (1-Khat)*phihat*ytsim2[ii-1]  + what*(et_sim[ii] + nt_sim[ii])
    
  }
  
  ytsim1  = ytsim1[51:Tsim]  #+  arima.sim(list(order = c(1,0,0), ar = 0.3), n = 20)/2
  ytsim2  = ytsim2[51:Tsim]
  
  phiphihat_all[jj] = lm( c(ytsim1-ytsim2)[2:20] ~ ytsim1[1:19] )$coef[2]
  rho_all[jj] = lm( c(ytsim1-ytsim2)[2:20] ~ (ytsim1-ytsim2)[1:19] )$coef[2]
  
}

mean(phiphihat_all)
median(phiphihat_all)

mean(rho_all)
median(rho_all)
mean( (rho_all *( 20 - 1)+1) / ( 20 - 4) )




