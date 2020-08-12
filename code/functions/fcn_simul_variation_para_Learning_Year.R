rm(list=ls(all=TRUE))

setwd('C:/Users/syae/Dropbox/Research/Juhani/Model/ARnoise/version1')
setwd('C:/Users/James/Dropbox/Research/Juhani/Model/ARnoise/version1')
setwd('C:/Users/syae/Dropbox_new2/Dropbox/Research/Juhani/Model/ARnoise/version1')

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

yearqtr_idx_one = (yearqtr_idx_numeric-yearqtr_xaxis[1])*4+1

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



fcn_1_1_winsor <- function(x) {
  x[x>1] = 0.9999
  x[x<  (-1)] = -0.9999  
  return(x)  
}


### construct variables ###

TS_index_firm_all = est$TS_index_firm_deflated
nonNA_index_firm
beginyearqtr_idx_numeric = c(yearqtr_idx_numeric[est$firm_begin_deflated])[firm_idx_numeric]

beginyearqtr_dummy=  factor(beginyearqtr_idx_numeric)

beginyear_idx_numeric = c(year_idx_numeric[est$firm_begin_deflated])[firm_idx_numeric]
beginyear_dummy=  factor(beginyear_idx_numeric)


trueyearqtr_idx_numeric = est$firmage.firm


trueage_idx_numeric = floor(est$firmage.firm)
trueage_dummy=  factor(trueage_idx_numeric )



table(trueage_dummy[est$firm_begin_deflated])


table(c(yearqtr_idx_numeric[est$firm_begin_deflated]))
table(trueage_dummy[est$firm_begin_deflated])
table((trueage_dummy[est$firm_begin_deflated])[(c(yearqtr_idx_numeric[est$firm_begin_deflated])==1985.5)])
# among firms starting from 1985.5, choose firms younger than 5 years.
sum(table((trueage_dummy[est$firm_begin_deflated])[(c(yearqtr_idx_numeric[est$firm_begin_deflated])==1985.5)]))


firm_at_1985_5_exclude_index = (trueage_idx_numeric[est$firm_begin_deflated]>=5)  &   (c(yearqtr_idx_numeric[est$firm_begin_deflated])==1985.5)  


table( c(trueage_dummy[est$firm_begin_deflated])[firm_at_1985_5_exclude_index]  )   
table( c(trueage_dummy[est$firm_begin_deflated])[!firm_at_1985_5_exclude_index]  )   

hist(est$no_obs_deflated[firm_at_1985_5_exclude_index ])
hist(est$no_obs_deflated[!firm_at_1985_5_exclude_index ])
 










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
    
    if (jj %in%  !firm_at_1985_5_exclude_index) {
    
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

max_no_obs_deflated = max(est$no_obs_deflated)
no_window_learn = max_no_obs_deflated-20+1

if (do_all) {
  
  FErhoadj_firmqtr_all = matrix(rep(NA, length(yearqtr_xaxis)*length(est$firm_begin_deflated )  ), ncol= length(yearqtr_xaxis)) 
  FErhoadj_firmqtr_all0 =   FErhoadj_firmqtr_all 
  FErhoadj_firmqtr_all10 =   FErhoadj_firmqtr_all 
  FErhoadj_firmqtr_all20 =   FErhoadj_firmqtr_all 
  FErhoadj_firmqtr_all30 =   FErhoadj_firmqtr_all 
  FErhoadj_firmqtr_all40 =   FErhoadj_firmqtr_all 
  FErhoadj_firmqtr_all50 =   FErhoadj_firmqtr_all 
  FErhoadj_firmqtr_all60 =   FErhoadj_firmqtr_all 
  FErhoadj_firmqtr_all70 =   FErhoadj_firmqtr_all 
  
  FErhoadj_firmqtr_0 =   FErhoadj_firmqtr_all 
  
  
  TS_index_firm_all.panel=   FErhoadj_firmqtr_all 
  nonNA_index_firm.panel=   FErhoadj_firmqtr_all 
  beginyearqtr_idx_numeric.panel=   FErhoadj_firmqtr_all 
  trueage_idx_numeric.panel=   FErhoadj_firmqtr_all 
  
  
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
        if (ii==20) FErhoadj_firmqtr_0[jj, yearqtr_idx_temp[1] ] = FErho_learn_0_reg[jj,ii-19]
        if (ii==20) FErhoadj_firmqtr_all0[jj, yearqtr_idx_temp  ] = FErho_learn_0_reg[jj,ii-19]
        if (ii==30) FErhoadj_firmqtr_all10[jj, yearqtr_idx_temp  ] = FErho_learn_0_reg[jj,ii-19]
        if (ii==40) FErhoadj_firmqtr_all20[jj, yearqtr_idx_temp  ] = FErho_learn_0_reg[jj,ii-19]
        if (ii==50) FErhoadj_firmqtr_all30[jj, yearqtr_idx_temp  ] = FErho_learn_0_reg[jj,ii-19]
        if (ii==60) FErhoadj_firmqtr_all40[jj, yearqtr_idx_temp  ] = FErho_learn_0_reg[jj,ii-19]
        if (ii==70) FErhoadj_firmqtr_all50[jj, yearqtr_idx_temp  ] = FErho_learn_0_reg[jj,ii-19]
        
        tempidx = firm_idx_temp[withinfirm_idx_temp[1]]
        
        TS_index_firm_all.panel[jj, yearqtr_idx_temp[1] ] = TS_index_firm_all[tempidx] 
        nonNA_index_firm.panel[jj, yearqtr_idx_temp[1] ] = nonNA_index_firm[tempidx]
        beginyearqtr_idx_numeric.panel[jj, yearqtr_idx_temp[1] ] = beginyearqtr_idx_numeric[tempidx]
        trueage_idx_numeric.panel[jj, yearqtr_idx_temp[1] ] =  trueage_idx_numeric[tempidx]
        
        
      }
    }
    
    
    
    
    if (jj %% 100 ==0) print(jj/length(est$firm_begin_deflated ))
  }
  
  save(FErhoadj_firmqtr_all , file="FErhoadj_firmqtr_all.Rdata")   
  save(FErhoadj_firmqtr_all0 , file="FErhoadj_firmqtr_all0.Rdata")   
  save(FErhoadj_firmqtr_all10 , file="FErhoadj_firmqtr_all10.Rdata")   
  save(FErhoadj_firmqtr_all20 , file="FErhoadj_firmqtr_all20.Rdata")   
  save(FErhoadj_firmqtr_all30 , file="FErhoadj_firmqtr_all30.Rdata")   
  save(FErhoadj_firmqtr_all40 , file="FErhoadj_firmqtr_all40.Rdata")   
  save(FErhoadj_firmqtr_all50 , file="FErhoadj_firmqtr_all50.Rdata")   
  save(FErhoadj_firmqtr_all60 , file="FErhoadj_firmqtr_all60.Rdata")   
  save(FErhoadj_firmqtr_all70 , file="FErhoadj_firmqtr_all70.Rdata")   
  
  save(FErhoadj_firmqtr_0 , file="FErhoadj_firmqtr_0.Rdata")   
  
}

load("FErhoadj_firmqtr_all.Rdata")   
rr=1;plot(yearqtr_xaxis[rr:111], FErho_yearqtr_adj_firm_avg[rr:111],type="o",ylim=c(0.1,0.25))
rr=1;  plot(yearqtr_xaxis[rr:111], apply(FErhoadj_firmqtr_all[ !is.na(FErhoadj_firmqtr_all[,rr])  , ],2,mean,na.rm=T)[rr:111], lty=1,lwd=rr/10, col="gray10",ylim=c(0.1,0.25),type="l")
rr=11;lines(yearqtr_xaxis[rr:111], apply(FErhoadj_firmqtr_all[ !is.na(FErhoadj_firmqtr_all[,rr])  , ],2,mean,na.rm=T)[rr:111], lty=1,lwd=rr/10, col="gray30")
rr=21;lines(yearqtr_xaxis[rr:111], apply(FErhoadj_firmqtr_all[ !is.na(FErhoadj_firmqtr_all[,rr])  , ],2,mean,na.rm=T)[rr:111], lty=1,lwd=rr/10, col="gray50")
rr=31;lines(yearqtr_xaxis[rr:111], apply(FErhoadj_firmqtr_all[ !is.na(FErhoadj_firmqtr_all[,rr])  , ],2,mean,na.rm=T)[rr:111], lty=1,lwd=rr/10, col="gray60")
rr=41;lines(yearqtr_xaxis[rr:111], apply(FErhoadj_firmqtr_all[ !is.na(FErhoadj_firmqtr_all[,rr])  , ],2,mean,na.rm=T)[rr:111], lty=1,lwd=rr/10, col="gray70")
rr=51;lines(yearqtr_xaxis[rr:111], apply(FErhoadj_firmqtr_all[ !is.na(FErhoadj_firmqtr_all[,rr])  , ],2,mean,na.rm=T)[rr:111], lty=1,lwd=rr/10, col="gray80")
rr=61;lines(yearqtr_xaxis[rr:111], apply(FErhoadj_firmqtr_all[ !is.na(FErhoadj_firmqtr_all[,rr])  , ],2,mean,na.rm=T)[rr:111], lty=1,lwd=rr/10, col="gray90")

load("FErhoadj_firmqtr_all0.Rdata")   
rr=1;plot(yearqtr_xaxis[rr:111], FErho_yearqtr_adj_firm_avg[rr:111],type="o",ylim=c(0.1,0.25))

idxtemp =  !firm_at_1985_5_exclude_index 
rr=1;plot(yearqtr_xaxis[rr:111],  apply(fcn_1_1_winsor(FErhoadj_firmqtr_0)[idxtemp,],2,mean,na.rm=T)[rr:111], lty=1,lwd=rr/10, col="gray10",type="l")
rr=1;plot(yearqtr_xaxis[rr:111],  apply(fcn_1_1_winsor(FErhoadj_firmqtr_all0)[idxtemp,],2,mean,na.rm=T)[rr:111], lty=1,lwd=rr/10, col="gray10",type="l")

idxtemp =  !firm_at_1985_5_exclude_index*0
rr=1;   plot(yearqtr_xaxis[rr:111], apply(fcn_1_1_winsor(FErhoadj_firmqtr_all)[idxtemp,],2,mean,na.rm=T)[rr:111], lty=1,lwd=rr/10, col="gray10",type="l")
rr=1; plot(yearqtr_xaxis[rr:111], apply(fcn_1_1_winsor(FErhoadj_firmqtr_all)[!is.na(FErhoadj_firmqtr_all0[,rr]) & idxtemp,],2,mean,na.rm=T)[rr:111], lty=3,lwd=rr/10, col="gray30")
rr=11; lines(yearqtr_xaxis[rr:111], apply(fcn_1_1_winsor(FErhoadj_firmqtr_all)[!is.na(FErhoadj_firmqtr_all0[,rr]) & idxtemp,],2,mean,na.rm=T)[rr:111], lty=3,lwd=rr/10, col="gray30")
rr=21; lines(yearqtr_xaxis[rr:111], apply(fcn_1_1_winsor(FErhoadj_firmqtr_all)[!is.na(FErhoadj_firmqtr_all0[,rr]) & idxtemp,],2,mean,na.rm=T)[rr:111], lty=3,lwd=rr/10, col="gray30")
rr=31; lines(yearqtr_xaxis[rr:111], apply(fcn_1_1_winsor(FErhoadj_firmqtr_all)[!is.na(FErhoadj_firmqtr_all0[,rr]) & idxtemp,],2,mean,na.rm=T)[rr:111], lty=3,lwd=rr/10, col="gray30")
rr=41; lines(yearqtr_xaxis[rr:111], apply(fcn_1_1_winsor(FErhoadj_firmqtr_all)[!is.na(FErhoadj_firmqtr_all0[,rr]) & idxtemp,],2,mean,na.rm=T)[rr:111], lty=3,lwd=rr/10, col="gray30")


idxtemp =  !firm_at_1985_5_exclude_index
rr=1;   plot(yearqtr_xaxis[rr:111], apply(TS_index_firm_all.panel[idxtemp,],2,mean,na.rm=T)[rr:111], lty=1,lwd=rr/10, col="gray10",type="l")
rr=1;   plot(yearqtr_xaxis[rr:111], apply(nonNA_index_firm.panel[idxtemp,],2,mean,na.rm=T)[rr:111], lty=1,lwd=rr/10, col="gray10",type="l")
rr=1;   plot(yearqtr_xaxis[rr:111], apply(beginyearqtr_idx_numeric.panel[idxtemp,],2,mean,na.rm=T)[rr:111], lty=1,lwd=rr/10, col="gray10",type="l")
rr=1;   plot(yearqtr_xaxis[rr:111], apply(trueage_idx_numeric.panel[idxtemp,],2,mean,na.rm=T)[rr:111], lty=1,lwd=rr/10, col="gray10",type="l")

TS_index_firm_all.panel 
nonNA_index_firm.panel fix 0 to cumul
beginyearqtr_idx_numeric.panel 
trueage_idx_numeric.panel 

yearqtr_dummy_panel = factor(  t(matrix( rep(yearqtr_xaxis, dim(TS_index_firm_all.panel )[1]), ncol = dim(TS_index_firm_all.panel )[1])  )   ) 
summary(lm( c(fcn_1_1_winsor(FErhoadj_firmqtr_all)) ~  yearqtr_dummy_panel+c(nonNA_index_firm.panel) +  c(TS_index_firm_all.panel)+c(beginyearqtr_idx_numeric.panel)+c(trueage_idx_numeric.panel)       ))
summary(lm( c(fcn_1_1_winsor(FErhoadj_firmqtr_all)) ~  yearqtr_dummy_panel+  c(nonNA_index_firm.panel)        ))
summary(lm( c(fcn_1_1_winsor(FErhoadj_firmqtr_all)) ~  yearqtr_dummy_panel +  c(TS_index_firm_all.panel)     ))
summary(lm( c(fcn_1_1_winsor(FErhoadj_firmqtr_all)) ~  yearqtr_dummy_panel+  c( beginyearqtr_idx_numeric.panel )       ))
summary(lm( c(fcn_1_1_winsor(FErhoadj_firmqtr_all)) ~  yearqtr_dummy_panel+c(trueage_idx_numeric.panel)       ))

summary(lm( c(fcn_1_1_winsor(FErhoadj_firmqtr_all)) ~  c(trueage_idx_numeric.panel)       ))



idxtemp =  !firm_at_1985_5_exclude_index*0
rr=1;plot(yearqtr_xaxis[rr:111],  apply(FErhoadj_firmqtr_all[ !is.na(FErhoadj_firmqtr_all0[,rr]) & idxtemp , ],2,mean,na.rm=T)[rr:111], lty=1,lwd=rr/10, col="gray10",ylim=c(0.1,0.25),type="l")
rr=11;lines(yearqtr_xaxis[rr:111], apply(FErhoadj_firmqtr_all[ !is.na(FErhoadj_firmqtr_all0[,rr])  &  idxtemp , ],2,mean,na.rm=T)[rr:111], lty=1,lwd=rr/10, col="gray30")
rr=21;lines(yearqtr_xaxis[rr:111], apply(FErhoadj_firmqtr_all[ !is.na(FErhoadj_firmqtr_all0[,rr])  &  idxtemp, ],2,mean,na.rm=T)[rr:111], lty=1,lwd=rr/10, col="gray50")
rr=31;lines(yearqtr_xaxis[rr:111], apply(FErhoadj_firmqtr_all[ !is.na(FErhoadj_firmqtr_all0[,rr]) &  idxtemp , ],2,mean,na.rm=T)[rr:111], lty=1,lwd=rr/10, col="gray60")
rr=41;lines(yearqtr_xaxis[rr:111], apply(FErhoadj_firmqtr_all[ !is.na(FErhoadj_firmqtr_all0[,rr]) &  idxtemp , ],2,mean,na.rm=T)[rr:111], lty=1,lwd=rr/10, col="gray70")
rr=51;lines(yearqtr_xaxis[rr:111], apply(FErhoadj_firmqtr_all[ !is.na(FErhoadj_firmqtr_all0[,rr]) &  idxtemp , ],2,mean,na.rm=T)[rr:111], lty=1,lwd=rr/10, col="gray80")
rr=61;lines(yearqtr_xaxis[rr:111], apply(FErhoadj_firmqtr_all[ !is.na(FErhoadj_firmqtr_all0[,rr]) &  idxtemp , ],2,mean,na.rm=T)[rr:111], lty=1,lwd=rr/10, col="gray90")





FErhoadj_firmqtr_all0_ref =  apply(fcn_1_1_winsor(FErhoadj_firmqtr_all0[!firm_at_1985_5_exclude_index,]) , 2, mean,na.rm=T)
FErhoadj_firmqtr_all_firm_idx = matrix(rep(NA, length(yearqtr_xaxis)*length(est$firm_begin_deflated )  ), ncol= length(yearqtr_xaxis)) 
FErho_learn_0_reg_ref =    matrix(rep(NA, no_window_learn*length(est$firm_begin_deflated )  ), ncol= no_window_learn) 

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
      
      FErho_learn_0_reg_ref[jj,ii-19] = FErhoadj_firmqtr_all0_ref[
     (((yearqtr_idx_numeric[firm_idx_temp])[nonNA_index_firm_temp==ii-10]-yearqtr_xaxis[1])*4+1)]/2+
       FErhoadj_firmqtr_all0_ref[(((yearqtr_idx_numeric[firm_idx_temp])[nonNA_index_firm_temp==ii-9]-yearqtr_xaxis[1])*4+1)]/2  
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








> sum(est$no_obs_deflated[firm_at_1985_5_exclude_index])
[1] 51574
> sum(est$no_obs_deflated)
[1] 186895
 

########### ###########  plot in the paper ########### ########### 
########### ###########  plot in the paper ########### ########### 
########### ###########  plot in the paper ########### ########### 

###################### autocorr_learning.eps ###################### 
par(mfrow=c(1,2))
# Paired t-test
# http://en.wikipedia.org/wiki/Student's_t-test#Dependent_t-test_for_paired_samples
load("FErhoadj_firmqtr_20_reg.Rdata") 
load("FErhoadj_firmqtr_10_30_reg.Rdata")
FErhoadj_firmqtr_20_trunc = FErhoadj_firmqtr_20_reg
 FErhoadj_firmqtr_20_trunc = FErhoadj_firmqtr_10_30_reg
#FErhoadj_firmqtr_20_trunc = FErhoadj_firmqtr_20_reg[!firm_at_1985_5_exclude_index,]
#FErhoadj_firmqtr_20_trunc = FErhoadj_firmqtr_20_reg[firm_at_1985_5_exclude_index,]

FErho_yearqtr_adj_firm_avg = rep(NA, N_row_yearqtr )
FErho_yearqtr_adj_firm_SE =  rep(NA, N_row_yearqtr )

for (ii in seq( 1,N_row_yearqtr ) ) {    
  
  FErho_yearqtr_adj_firm = FErhoadj_firmqtr_20_trunc[ yearqtr_idx_numeric   == yearqtr_xaxis[ii]   ]  
  FErho_yearqtr_adj_firm_avg[ii] = mean(FErho_yearqtr_adj_firm ,na.rm=T)
  FErho_yearqtr_adj_firm_SE[ii] = sd(FErho_yearqtr_adj_firm,na.rm=T)/sqrt( sum(!is.na(FErho_yearqtr_adj_firm)) )
  
}

xxx = FErho_yearqtr_adj_firm_avg 



FErhoadj_firmqtr_20_trunc =FErhoadj_firmqtr_all0
FErhoadj_firmqtr_20_trunc =FErhoadj_firmqtr_all0[!firm_at_1985_5_exclude_index,]
#FErhoadj_firmqtr_20_trunc = FErhoadj_firmqtr_all0[firm_at_1985_5_exclude_index,]

FErhoadj_firmqtr_20_trunc[FErhoadj_firmqtr_20_trunc>1]=0.9999
FErhoadj_firmqtr_20_trunc[FErhoadj_firmqtr_20_trunc< (-1)]=-0.9999

x  = FErhoadj_firmqtr_20_trunc

xxx =  apply(x , 2, mean,na.rm=T)
xxx = FErho_yearqtr_adj_firm_avg
plot( apply( !is.na(x) , 2, sum,na.rm=T))

FErho_yearqtr_adj_firm_SE = apply(  x  , 2, sd,na.rm=T)/sqrt(  apply( !is.na(x) , 2, sum,na.rm=T)        )
  
# year effect on firm-level est
ssd = 1.96

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





#lines(xaxis, apply(fcn_1_1_winsor(FErhoadj_firmqtr_all10),2,mean,na.rm=T)[1:(T_temp-1)] )
lines(xaxis, apply(fcn_1_1_winsor(FErhoadj_firmqtr_all20),2,mean,na.rm=T)[1:(T_temp-1)] )
lines(xaxis, apply(fcn_1_1_winsor(FErhoadj_firmqtr_all30),2,mean,na.rm=T)[1:(T_temp-1)] )
lines(xaxis, apply(fcn_1_1_winsor(FErhoadj_firmqtr_all40),2,mean,na.rm=T)[1:(T_temp-1)] )
 
rr=1;lines(xaxis, apply(fcn_1_1_winsor(FErhoadj_firmqtr_all)[ !is.na(FErhoadj_firmqtr_all0[,rr]) &  firm_at_1985_5_exclude_index , ],2,mean,na.rm=T)[rr:111])
rr=1;lines(xaxis, apply(fcn_1_1_winsor(FErhoadj_firmqtr_all)[ !is.na(FErhoadj_firmqtr_all0[,rr]) &  !firm_at_1985_5_exclude_index , ],2,mean,na.rm=T)[rr:111])









# plot2:  rho decreasing over age 
load("FErho_learn_0_reg.Rdata")
 


FErho_learn_0_reg_trunc = FErho_learn_0_reg

FErho_learn_0_reg_trunc = FErho_learn_0_reg[!firm_at_1985_5_exclude_index,]
#FErho_learn_0_reg_trunc = FErho_learn_0_reg[firm_at_1985_5_exclude_index,]


FErho_learn_0_reg_trunc[FErho_learn_0_reg_trunc>1]=0.9999
FErho_learn_0_reg_trunc[FErho_learn_0_reg_trunc< (-1)]=-0.9999

#x =  FErho_learn_0 
x = FErho_learn_0_reg_trunc

#load("FErhoadj_firmqtr_all.Rdata")
#x = FErhoadj_firmqtr_all


FErho_learn_mean0.1 =  (matrix(rep((x[,1] )  ,dim(x)[2]), ncol=dim(x)[2])) + x*0
FErho_learn_mean0.11 = (matrix(rep((x[,11])  ,dim(x)[2]), ncol=dim(x)[2])) + x*0

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
#xaxis  =  yearqtr_xaxis

T_temp = length(xxx)-1
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

x_time_ref = apply(fcn_1_1_winsor(FErho_learn_0_reg_ref[!firm_at_1985_5_exclude_index,]) , 2, mean,na.rm=T)
lines(xaxis[1:91], x_time_ref[1:91] , cex=0.5, lwd=3  ,type="l", lty=1  )



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



















m0.FErho=lmer(FE_temp~ -1 + trueage_dummy + year_dummy   +   FE_temp_lag1:year_dummy +
                FE_temp_lag1:trueage_dummy + FE_temp_lag1 + (1 | firm_dummy )+ (-1 + FE_temp_lag1|firm_dummy)    , REML = TRUE)
save(m0.FErho, file="MixedModel_FErho_Lmer.Rdata") 


m0.FErho=lmer(FE_temp~ -1 + age_dummy + year_dummy   +   FE_temp_lag1:year_dummy +
                FE_temp_lag1:age_dummy + FE_temp_lag1 + (1 | firm_dummy )+ (-1 + FE_temp_lag1|firm_dummy)    , REML = TRUE)
save(m0.FErho, file="MixedModel_FErho_Lmer.Rdata") 



m0.FErho=lmer(FE_temp~ -1 + age_dummy + year_dummy   +      FE_temp_lag1:beginyear_dummy  +
                FE_temp_lag1:age_dummy + FE_temp_lag1 + (1 | firm_dummy )+ (-1 + FE_temp_lag1|firm_dummy)    , REML = TRUE)
save(m0.FErho, file="MixedModel_FErho_Lmer.Rdata") 

load("MixedModel_FErho_Lmer.Rdata") 


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


