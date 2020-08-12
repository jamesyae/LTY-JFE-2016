rm(list=ls(all=TRUE))
separ_firms = TRUE
trueage_proxy = FALSE



if (separ_firms == FALSE  &   trueage_proxy == TRUE)  save_file_path = "firmunited_trueage"
if (separ_firms == FALSE  &   trueage_proxy == FALSE)  save_file_path = "firmunited_nonNAidx"
if (separ_firms == TRUE  &   trueage_proxy == FALSE)  save_file_path = "firmsepar_nonNAidx"
if (separ_firms == TRUE  &   trueage_proxy == TRUE)  save_file_path = "firmsepar_trueage"





require(lme4)
require(nlme)
require(sandwich)
require(quantreg)


setwd('C:/Users/syae/Dropbox/Research/Juhani/Model/ARnoise/version1')
setwd('C:/Users/James/Dropbox/Research/Juhani/Model/ARnoise/version1')
setwd('C:/Users/syae/Dropbox_new2/Dropbox/Research/Juhani/Model/ARnoise/version1')
load("temp_20140623.RData")
do_all = TRUE

setwd(sprintf("C:/Users/syae/Dropbox/Research/Juhani/Model/ARnoise/version1/%s",save_file_path))
setwd(sprintf("C:/Users/James/Dropbox/Research/Juhani/Model/ARnoise/version1/%s",save_file_path))
setwd(sprintf("C:/Users/syae/Dropbox_new2/Dropbox/Research/Juhani/Model/ARnoise/version1/%s",save_file_path))
#source("fcn_firmlevel_TS_make_general.r")
#fcn_firmlevel_TS_make_general( scaleVset=scaleVset, option=option,  maxlength=20, separ_firms=separ_firms ) 



##############################################################################
########################################################################
#######  Learning?  #######
########################################################################
##############################################################################




filename = fcn_filename_maker(scaleVset=scaleVset, option=option )
load(file=filename)  
est = firm_TS_data



fcn_1_1_winsor <- function(x) {
  x[x>1] = 0.9999
  x[x<  (-1)] = -0.9999  
  return(x)  
}


######################################
######################################
#######  household chores  #######
######################################
######################################

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
yearqtr_idx_one = (yearqtr_idx_numeric-yearqtr_xaxis[1])*4+1

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




### construct variables ###
TS_index_firm_all = est$TS_index_firm_deflated

beginyearqtr_idx_numeric = c(yearqtr_idx_numeric[est$firm_begin_deflated])[firm_idx_numeric]
beginyearqtr_dummy=  factor(beginyearqtr_idx_numeric)

beginyear_idx_numeric = c(year_idx_numeric[est$firm_begin_deflated])[firm_idx_numeric]
beginyear_dummy=  factor(beginyear_idx_numeric)

trueyearqtr_idx_numeric = est$firmage.firm
trueage_idx_numeric = floor(est$firmage.firm)
trueage_dummy=  factor(trueage_idx_numeric )

yearqtr_dummy.panel = factor(  t(matrix( rep(yearqtr_xaxis, length(est$no_obs_deflated)), ncol =  length(est$no_obs_deflated) )  )   ) 
year_dummy.panel = factor(  t(matrix( rep(  floor(yearqtr_xaxis -0.2)  , length(est$no_obs_deflated)), ncol =  length(est$no_obs_deflated) )  )   ) 

firm_at_1985_5_exclude_index = (trueage_idx_numeric[est$firm_begin_deflated]>=5)  &   (c(yearqtr_idx_numeric[est$firm_begin_deflated])==1985.5)  

firm_truly_newborn = seq(1, length(est$firm_begin_deflated))
if( trueage_proxy) firm_truly_newborn = which(trueage_idx_numeric[est$firm_begin_deflated]<=10/4)


table(trueage_dummy[est$firm_begin_deflated])
table(c(yearqtr_idx_numeric[est$firm_begin_deflated]))
table(trueage_dummy[est$firm_begin_deflated])
table((trueage_dummy[est$firm_begin_deflated])[(c(yearqtr_idx_numeric[est$firm_begin_deflated])==1985.5)])
# among firms starting from 1985.5, choose firms younger than 5 years.
sum(table((trueage_dummy[est$firm_begin_deflated])[(c(yearqtr_idx_numeric[est$firm_begin_deflated])==1985.5)]))
table( c(trueage_dummy[est$firm_begin_deflated])[firm_at_1985_5_exclude_index]  )   
table( c(trueage_dummy[est$firm_begin_deflated])[!firm_at_1985_5_exclude_index]  )   









##################################################
##################################################
#######  decaying autocorrelation of FE   #######
##################################################
##################################################
#1:20 - 21:40
#2:21 - 22:41
#3:22 - 23:42





if (do_all) {
  
  max_no_obs_deflated = max(est$no_obs_deflated)
  window_size = 10
  no_window_learn = max_no_obs_deflated-(2*window_size-1)+1
  
  FErho_learn_0 = matrix(rep(NA, no_window_learn*length(est$firm_begin_deflated )  ), ncol= no_window_learn)
  FErho_learn_0_reg = matrix(rep(NA, no_window_learn*length(est$firm_begin_deflated )  ), ncol= no_window_learn) 
  FErhoadj_firmqtr_all = matrix(rep(NA, length(yearqtr_xaxis)*length(est$firm_begin_deflated )  ), ncol= length(yearqtr_xaxis)) 
  
  
  for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
  {  
    firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]     
    nonNA_index_firm_temp = nonNA_index_firm[firm_idx_temp]
    xxx = yef_temp[firm_idx_temp]    
    onetoT = est$TS_index_firm_deflated[firm_idx_temp]
    
    for (ii in c( (2*window_size-1):max_no_obs_deflated  )) {
      
      
      
      if (est$no_obs_deflated[jj] >= ii ) {
        
        tryCatch({
          
          withinfirm_idx_temp = onetoT[nonNA_index_firm_temp==ii-(2*window_size-2)]:onetoT[nonNA_index_firm_temp==ii]
          xxx0 = xxx[ withinfirm_idx_temp ]          
          FErho_learn_0[jj,ii-(2*window_size-2)] = arima( xxx0 , order=c(1,0,0), method = c("ML"),optim.control = list(maxit = 300000 ))$coef[1]
          xxx0_0 = c(NA, xxx0[1:(length(xxx0)-1)])
          xxx0_1 = xxx0
          FErho_learn_0_reg[jj,ii-(2*window_size-2)] =  (lm(xxx0_1 ~ xxx0_0))$coef[2]    
          
          yearqtr_idx_one_temp = yearqtr_idx_one[ firm_idx_temp[ onetoT[nonNA_index_firm_temp==ii-(window_size-1)] ]   ]
          FErhoadj_firmqtr_all[jj,  yearqtr_idx_one_temp ] = FErho_learn_0_reg[jj,ii-(2*window_size-2)]
          
          
        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
                             erroroccur = TRUE} ) 
      }
      
    }
    print(jj)
  }  
  
  FErho_learn_0 =  (FErho_learn_0 *( (2*window_size-1) - 1)+1) / ( (2*window_size-1) - 4)   
  FErho_learn_0_reg =  (FErho_learn_0_reg *( (2*window_size-1) - 1)+1) / ( (2*window_size-1) - 4)  
  FErhoadj_firmqtr_all =  (FErhoadj_firmqtr_all *( (2*window_size-1) - 1)+1) / ( (2*window_size-1) - 4) 
  
  save(FErho_learn_0, file="FErho_learn_0_center.Rdata")  
  save(FErho_learn_0_reg, file="FErho_learn_0_center_reg.Rdata")       
  save(FErhoadj_firmqtr_all,   file="FErhoadj_firmqtr_all_center.Rdata")
}




if (do_all) { 
  
  load("FErho_learn_0_center_reg.Rdata")
  load("FErhoadj_firmqtr_all_center.Rdata")
  
  max_no_obs_deflated = max(est$no_obs_deflated)
  window_size = 10
  no_window_learn = max_no_obs_deflated-(2*window_size-1)+1 
  
  
  TS_index_firm_all.panel = matrix(rep(NA, length(yearqtr_xaxis)*length(est$firm_begin_deflated )  ), ncol= length(yearqtr_xaxis)) 
  nonNA_index_firm.panel=    TS_index_firm_all.panel
  trueyearqtr_idx_numeric.panel =    TS_index_firm_all.panel
  FErhoadj_firmqtr_20_reg=    TS_index_firm_all.panel
  FErhoadj_firmqtr_30_reg=    TS_index_firm_all.panel
  FErhoadj_firmqtr_40_reg=    TS_index_firm_all.panel
  FErhoadj_firmqtr_50_reg=    TS_index_firm_all.panel  
  
  
  for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
  {  
    firstidx = TRUE
    firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]     
    nonNA_index_firm_temp = nonNA_index_firm[firm_idx_temp] 
    onetoT = est$TS_index_firm_deflated[firm_idx_temp]
    
    for (ii in c( (2*window_size-1):max_no_obs_deflated)) {
      if (est$no_obs_deflated[jj] >= ii ) {
        
        #tryCatch({
        
        tempidx =   firm_idx_temp[ onetoT[nonNA_index_firm_temp==ii-window_size+1] ]  
        yearqtr_idx_one_temp = yearqtr_idx_one[ tempidx  ] 
        
        TS_index_firm_all.panel[jj, yearqtr_idx_one_temp ] = TS_index_firm_all[tempidx] 
        nonNA_index_firm.panel[jj, yearqtr_idx_one_temp] = nonNA_index_firm[tempidx]
        trueyearqtr_idx_numeric.panel[jj, yearqtr_idx_one_temp ] =  trueyearqtr_idx_numeric[tempidx]
        
        withinfirm_idx_temp = onetoT[nonNA_index_firm_temp==ii-(2*window_size-2)]:onetoT[nonNA_index_firm_temp==ii]
        yearqtr_idx_temp = yearqtr_idx_one[firm_idx_temp[withinfirm_idx_temp]] 
        
        
        if (ii==2*window_size-1 &  jj  %in% firm_truly_newborn ) FErhoadj_firmqtr_20_reg[jj,  yearqtr_idx_temp ]  = FErho_learn_0_reg[jj,ii-(2*window_size-2)] 
        if (ii==3*window_size-1&  jj  %in% firm_truly_newborn) FErhoadj_firmqtr_30_reg[jj,  yearqtr_idx_temp ]  = FErho_learn_0_reg[jj,ii-(3*window_size-2)]
        if (ii==4*window_size-1&  jj  %in% firm_truly_newborn) FErhoadj_firmqtr_40_reg[jj,  yearqtr_idx_temp ]  = FErho_learn_0_reg[jj,ii-(4*window_size-2)]
        if (ii==5*window_size-1&  jj  %in% firm_truly_newborn) FErhoadj_firmqtr_50_reg[jj,  yearqtr_idx_temp ]  = FErho_learn_0_reg[jj,ii-(5*window_size-2)]
        
        
      }
    }
    if (jj %% 100 ==0) print(jj/length(est$firm_begin_deflated ))
  }  
  
  age_proxy = list(TS_index_firm_all.panel=TS_index_firm_all.panel, nonNA_index_firm.panel=nonNA_index_firm.panel,
                   trueyearqtr_idx_numeric.panel=trueyearqtr_idx_numeric.panel )
  save(age_proxy, file="age_proxy.Rdata")  
  
  save(FErhoadj_firmqtr_20_reg, file="FErhoadj_firmqtr_20_center_reg.Rdata")
  save(FErhoadj_firmqtr_30_reg, file="FErhoadj_firmqtr_30_center_reg.Rdata")
  save(FErhoadj_firmqtr_40_reg, file="FErhoadj_firmqtr_40_center_reg.Rdata")
  save(FErhoadj_firmqtr_50_reg, file="FErhoadj_firmqtr_50_center_reg.Rdata")
  
  
}








#####  age regression table

load( "age_proxy.Rdata")  
load("FErhoadj_firmqtr_all_center.Rdata")

#arima(c(t(fcn_1_1_winsor(FErhoadj_firmqtr_all))),c(3,0,0))
acf( c(t(fcn_1_1_winsor(FErhoadj_firmqtr_all))) ,na.action = na.pass )
pacf( c(t(fcn_1_1_winsor(FErhoadj_firmqtr_all))) ,na.action = na.pass )

rq( c(fcn_1_1_winsor(FErhoadj_firmqtr_all)) ~  year_dummy.panel +  c(age_proxy$nonNA_index_firm.panel)        ) 
rq( c(fcn_1_1_winsor(FErhoadj_firmqtr_all)) ~  year_dummy.panel +  c(age_proxy$TS_index_firm_all.panel)     ) 
rq( c(fcn_1_1_winsor(FErhoadj_firmqtr_all)) ~  year_dummy.panel + c(age_proxy$trueyearqtr_idx_numeric.panel*4)       ) 

summary(lm( c(fcn_1_1_winsor(FErhoadj_firmqtr_all)) ~  year_dummy.panel +  c(age_proxy$nonNA_index_firm.panel)        ))
summary(lm( c(fcn_1_1_winsor(FErhoadj_firmqtr_all)) ~  year_dummy.panel +  c(age_proxy$TS_index_firm_all.panel)     )) 
summary(lm( c(fcn_1_1_winsor(FErhoadj_firmqtr_all)) ~  year_dummy.panel + c(age_proxy$trueyearqtr_idx_numeric.panel*4)       ))
summary(lm( c(t(fcn_1_1_winsor(FErhoadj_firmqtr_all))) ~  factor(t( matrix(year_dummy.panel,ncol=dim(FErhoadj_firmqtr_all)[2] ) )) + c(t(age_proxy$trueyearqtr_idx_numeric.panel*4))       ))






mm0  =(lm( c(t(fcn_1_1_winsor(FErhoadj_firmqtr_all))) ~ -1+ factor(t( matrix(year_dummy.panel,ncol=dim(FErhoadj_firmqtr_all)[2] ) )) + c(t(age_proxy$trueyearqtr_idx_numeric.panel*4))     ,subset = idxtemp  ))
mm0  =(lm( c(t(fcn_1_1_winsor(FErhoadj_firmqtr_all))) ~ -1+ factor(t( matrix(year_dummy.panel,ncol=dim(FErhoadj_firmqtr_all)[2] ) )) + c(t(age_proxy$trueyearqtr_idx_numeric.panel*4))       ))
summary(mm0)
sqrt(diag(NeweyWest(mm0 )))

mm0  =(lm( c(t(fcn_1_1_winsor(FErhoadj_firmqtr_all))) ~ -1+ factor(t( matrix(year_dummy.panel,ncol=dim(FErhoadj_firmqtr_all)[2] ) )) + c(t(age_proxy$nonNA_index_firm.panel))       ))
summary(mm0)
sqrt(diag(NeweyWest(mm0 )))

## The Newey & West (1987) estimator requires specification
## of the lag and suppression of prewhitening
sqrt(diag(NeweyWest(mm0 , lag =30, prewhite = FALSE)))

## Newey & West (1994) compute this type of estimator
sqrt(diag(NeweyWest(mm0 )))

summary(lm( c(fcn_1_1_winsor(FErhoadj_firmqtr_all)) ~  yearqtr_dummy.panel +  c(age_proxy$nonNA_index_firm.panel)        ))
summary(lm( c(fcn_1_1_winsor(FErhoadj_firmqtr_all)) ~  yearqtr_dummy.panel +  c(age_proxy$TS_index_firm_all.panel)     )) 
summary(lm( c(fcn_1_1_winsor(FErhoadj_firmqtr_all)) ~  yearqtr_dummy.panel + c(age_proxy$trueyearqtr_idx_numeric.panel*4)       ))
summary(lm( c(fcn_1_1_winsor(FErhoadj_firmqtr_all)) ~  yearqtr_dummy.panel+  c(age_proxy$TS_index_firm_all.panel) +  c(age_proxy$trueyearqtr_idx_numeric.panel*4)      ))
summary(lm( c(fcn_1_1_winsor(FErhoadj_firmqtr_all)) ~  yearqtr_dummy.panel+   c(age_proxy$nonNA_index_firm.panel)  +  c(age_proxy$trueyearqtr_idx_numeric.panel*4)      ))

idxtemp =   !firm_at_1985_5_exclude_index

summary(lm( c(fcn_1_1_winsor(FErhoadj_firmqtr_all[idxtemp,])) ~ factor(matrix(yearqtr_dummy.panel, nrow=length(est$no_obs_deflated) )[idxtemp,] ) +  c(age_proxy$nonNA_index_firm.panel[idxtemp,])        ))
summary(lm( c(fcn_1_1_winsor(FErhoadj_firmqtr_all[idxtemp,])) ~  factor(matrix(yearqtr_dummy.panel, nrow=length(est$no_obs_deflated) )[idxtemp,] )+  c(age_proxy$TS_index_firm_all.panel[idxtemp,])     )) 
summary(lm( c(fcn_1_1_winsor(FErhoadj_firmqtr_all[idxtemp,])) ~  factor(matrix(yearqtr_dummy.panel, nrow=length(est$no_obs_deflated) )[idxtemp,] ) + c(age_proxy$trueyearqtr_idx_numeric.panel[idxtemp,])       ))


load("FErhoadj_firmqtr_all_center.Rdata")

plot(yearqtr_xaxis , apply(fcn_1_1_winsor(FErhoadj_firmqtr_all[idxtemp,]) ,2,mean,na.rm=T) , lty=1,  col="gray10",type="l")
plot(yearqtr_xaxis , apply(fcn_1_1_winsor(FErhoadj_firmqtr_all) ,2,mean,na.rm=T) , lty=1,  col="gray10",type="l")


m0=(rq(FE_temp  ~ -1+ year_dummy  + FE_temp_lag1:year_dummy  + FE_temp_lag1 + FE_temp_lag1:trueage_idx_numeric    ))
summry(m0)
plot(  m0$coef[30] + m0$coef[31:58], type="o")


year_idx_numeric = floor(firm_TS_data$year_qtr_idx-0.2)
age_idx_numeric =  1+floor(firm_TS_data$TS_index_firm_deflated/4-0.2)
firm_idx_numeric = est$firm_deflated_nn_idx


M0 = lmer(FE_temp ~ ye_temp_lag1+ (1| firm_dummy ) + factor(year_idx_numeric)  + FE_temp_lag1:year_dummy  + FE_temp_lag1:trueage_idx_numeric   ,  REML=TRUE)      
plot( fixef(M0 )[31:60] +  fixef(M0 )[2], type="o")

m0=(lmer(FE_temp  ~ -1+ year_dummy  + FE_temp_lag1:year_dummy  + FE_temp_lag1 + FE_temp_lag1:trueage_idx_numeric    ))
summry(m0)
plot(  m0$coef[30] + m0$coef[31:58], type="o")


summary(rq(FE_temp  ~ -1+ year_dummy  + FE_temp_lag1:year_dummy  + FE_temp_lag1 + FE_temp_lag1:nonNA_index_firm    ))

plot(yearqtr_xaxis, apply(age_proxy$trueyearqtr_idx_numeric.panel, 2, mean,na.rm=T))
plot(yearqtr_xaxis, apply(age_proxy$trueyearqtr_idx_numeric.panel, 2, median,na.rm=T))
plot(yearqtr_xaxis, apply( age_proxy$trueyearqtr_idx_numeric.panel<6, 2, sum,na.rm=T))
plot(yearqtr_xaxis, apply( age_proxy$trueyearqtr_idx_numeric.panel<6, 2, mean,na.rm=T))



load("FErhoadj_firmqtr_all_center.Rdata")
load( "age_proxy.Rdata")  
##### estimated rho(FE) at age zero when it is born at time t.
xa =  factor(t( matrix(yearqtr_dummy.panel,ncol=dim(FErhoadj_firmqtr_all)[2] ) ))
xb = c(t(age_proxy$trueyearqtr_idx_numeric.panel*4)) 
m0  =(lm( c(t(fcn_1_1_winsor(FErhoadj_firmqtr_all))) ~ -1+ xa + xb      ))
se.temp0 = sqrt(diag(NeweyWest(m0 )))
se.temp0[93]
se.temp = se.temp0[1:92]
plot(yearqtr_xaxis[10:101],m0$coef[1:92],type="o" ,ylim=c(0.14,0.26)); 
lines(yearqtr_xaxis[10:101],m0$coef[1:92]+2*se.temp, lty=3)
lines(yearqtr_xaxis[10:101],m0$coef[1:92]-2*se.temp, lty=3)
########## 


xa =  factor(t( matrix(yearqtr_dummy.panel,ncol=dim(FErhoadj_firmqtr_all)[2] ) ))
xb = c(t(age_proxy$trueyearqtr_idx_numeric.panel*4)) 

m1  =(lm( c(t(fcn_1_1_winsor(FErhoadj_firmqtr_all))) ~ -1+ xa + xb +  c(xb^2)     ))
summary(m1)

xa =  factor(t( matrix(yea_dummy.panel,ncol=dim(FErhoadj_firmqtr_all)[2] ) ))
xb = floor(c(t(age_proxy$trueyearqtr_idx_numeric.panel)) )

m1  =(lm( c(t(fcn_1_1_winsor(FErhoadj_firmqtr_all))) ~ -1+ xa + xb +  c(xb^2)     ))
summary(m1)
### learning degree is decreasing.

m1  =(lm( c(t(fcn_1_1_winsor(FErhoadj_firmqtr_all))) ~ -1+ xa + xb + xa:xb     ))
summary(m1)
se.temp = sqrt(diag(NeweyWest(m0 )))[1:92]
plot(yearqtr_xaxis[10:101],m1$coef[1:92],type="o" ); 
lines(yearqtr_xaxis[10:101],m1$coef[1:92]+2*se.temp, lty=3)
lines(yearqtr_xaxis[10:101],m1$coef[1:92]-2*se.temp, lty=3)
plot(yearqtr_xaxis[11:101],m1$coef[93] + m1$coef[94:184],type="o" ); 



load("FErhoadj_firmqtr_all_center.Rdata")
load( "age_proxy.Rdata") 
idxtemp =  !firm_at_1985_5_exclude_index*0
xa =  factor(t( matrix(yearqtr_dummy.panel,ncol=dim(FErhoadj_firmqtr_all)[2] )[idxtemp,] ))
xb = c(t(age_proxy$nonNA_index_firm.panel[idxtemp,]))  
m2  =(lm( c(t(fcn_1_1_winsor(FErhoadj_firmqtr_all[idxtemp,]))) ~ -1+ xa + xb + xa:xb  ))
summary(m2)
plot(yearqtr_xaxis[10:101],m2$coef[1:92],type="o" ); 
plot(yearqtr_xaxis[11:101],m2$coef[93] + m2$coef[94:184],type="o" ,ylim=c(-0.02,0.02)); 
abline(h=0,lty=3)
### diff(old new) is decreasing.








###############################################################  
###############################################################  
#####################   calendar graph  #####################  
###############################################################  
###############################################################  


idxtemp =  !firm_at_1985_5_exclude_index*0 
load("FErhoadj_firmqtr_all_center.Rdata")
load("age_proxy.Rdata")
old_age_q70   = apply(age_proxy$trueyearqtr_idx_numeric.panel ,2, quantile,0.5 ,na.rm=T)
old_age_q70   = t(matrix(rep(old_age_q70, dim(age_proxy$trueyearqtr_idx_numeric.panel)[1]), ncol=dim(age_proxy$trueyearqtr_idx_numeric.panel)[1]))
young_age_q30 = apply(age_proxy$trueyearqtr_idx_numeric.panel ,2, quantile,0.5,na.rm=T)
young_age_q30 = t(matrix(rep(young_age_q30, dim(age_proxy$trueyearqtr_idx_numeric.panel)[1]), ncol=dim(age_proxy$trueyearqtr_idx_numeric.panel)[1]))

old_firm_idx = age_proxy$trueyearqtr_idx_numeric.panel >=  old_age_q70
young_firm_idx = age_proxy$trueyearqtr_idx_numeric.panel <=  young_age_q30

FErhoadj_firmqtr_all.tempold = FErhoadj_firmqtr_all
FErhoadj_firmqtr_all.tempold[!old_firm_idx] = NA
FErhoadj_firmqtr_all.tempold = matrix(FErhoadj_firmqtr_all.tempold ,ncol=length(yearqtr_xaxis)) 

FErhoadj_firmqtr_all.tempyoung = FErhoadj_firmqtr_all
FErhoadj_firmqtr_all.tempyoung[!young_firm_idx] = NA
FErhoadj_firmqtr_all.tempyoung = matrix(FErhoadj_firmqtr_all.tempyoung ,ncol=length(yearqtr_xaxis))  

apply(!is.na(FErhoadj_firmqtr_all.tempold),2,sum,na.mr=T)

plot(yearqtr_xaxis,apply(fcn_1_1_winsor(FErhoadj_firmqtr_all[idxtemp,]) ,2,mean, na.rm=T), type="o",ylim=c( 0.1,0.25))
lines(yearqtr_xaxis,apply(fcn_1_1_winsor(  FErhoadj_firmqtr_all.tempold[idxtemp,]) ,2, mean,na.rm=T), lty=2)
lines(yearqtr_xaxis,apply(fcn_1_1_winsor( FErhoadj_firmqtr_all.tempyoung[idxtemp,])  ,2, mean,na.rm=T), lty=3)
# we do not decompose this b/c average age is different at each quarter
lines(yearqtr_xaxis[11:102],m0$coef[1:92],lty=1); 
# implied rho(FE) of zero-quarter-old firm is similar to the young half.



###############################################################  
###############################################################  
#####################  age-quartiles  bar plots #####################  
###############################################################  
###############################################################  


load("FErhoadj_firmqtr_all_center.Rdata")
load("age_proxy.Rdata")
idxtemp =   !firm_at_1985_5_exclude_index*0   
source("fcn_quantile_group_mean.r")
no_group = 4
qgroup_means_all2 = matrix(rep(NA, length(yearqtr_xaxis)*no_group ), nrow=no_group)
qgroup_means_all1 = matrix(rep(NA, length(yearqtr_xaxis)*no_group ), nrow=no_group)
for (jj in seq(  1 , length(yearqtr_xaxis) ) ){    
  
  x =fcn_1_1_winsor(FErhoadj_firmqtr_all[idxtemp,jj])-
    mean(fcn_1_1_winsor(FErhoadj_firmqtr_all[idxtemp,jj]),na.rm=T)
  
  crt = age_proxy$trueyearqtr_idx_numeric.panel[idxtemp,jj]
  
  #crt = age_proxy$TS_index_firm_all.panel[idxtemp,jj]
  qgroup_means_all1[,jj] = fcn_quantile_group_mean(x= x,
                                                   crt=crt, no_group=no_group)     
  
  crt = age_proxy$nonNA_index_firm.panel[idxtemp,jj]
  qgroup_means_all2[,jj] = fcn_quantile_group_mean(x= x,
                                                   crt=crt, no_group=no_group) 
  
} 
#barplot(apply(qgroup_means_all1,1,mean,na.rm=T),ylim=c(-0.02,0.02))
#barplot(apply(qgroup_means_all2,1,mean,na.rm=T),ylim=c(-0.02,0.02))

x1 <- apply(qgroup_means_all1,1,mean,na.rm=T)
x2 <- apply(qgroup_means_all2,1,mean,na.rm=T)
A <- gl(no_group,1,no_group,labels=c("1st","2nd","3rd","4th"  ))
data1 <- cbind(x1,x2)
rownames(data1) <- levels(A)
barplot(t(data1),beside=T,ylim=c(-0.025,0.025),legend.text=c("Age","Obs order"),        
        col=c("grey50","grey80"),ylab="",xlab="Age Quartile",args.legend = list( bty = "n", cex=0.75))
abline(h=0, lty=3  )
title( main=expression(hat(E)[italic("t+1:t+20")]*'[ Autocorr'[italic(i)]*'(FE'[italic("i, t+1:t+20")]*') ]'))
box()



##################################################
##################################################
##################################################
##################################################
##################################################
##################################################
############       II.  phi- phihat     ##########
##################################################
##################################################
##################################################
##################################################
##################################################
##################################################





#########################################################
#########################################################
#######  learning phi "within firm" using lmer   #######
#########################################################
#########################################################

#1:20 - 21:40
#2:21 - 22:41
#3:22 - 23:42


if (do_all) {
  
  max_no_obs_deflated = max(est$no_obs_deflated)
  window_size = 10 
  no_window_learn = max_no_obs_deflated - window_size*2 + 2
  
  phi_learn_lmer00 = rep(NA, no_window_learn  ) 
  phi_learn_lmer_var00 = rep(NA, no_window_learn  )  
  phihat_learn_lmer00 = rep(NA, no_window_learn  ) 
  phihat_learn_lmer_var00 = rep(NA, no_window_learn  )  
  phiphihat_learn_lmer00 = rep(NA, no_window_learn  ) 
  phiphihat_learn_lmer_var00 = rep(NA, no_window_learn  )  
  phiphihat_learn_lmer_var00_20 = rep(NA, no_window_learn  )  
  phiphihat_learn_lmer_var00_10_30 = rep(NA, no_window_learn  )  
  
  
  for (ii in c( (2*window_size-1):max_no_obs_deflated   )) {    
    
    total_idx_temp = rep(FALSE, length(est$ye_firm_deflated))
    total_idx_temp_20 = rep(FALSE, length(est$ye_firm_deflated))
    total_idx_temp_10_30 = rep(FALSE, length(est$ye_firm_deflated))
    
    for (jj in seq(  1 , length(est$firm_begin_deflated ) ) )
    {  
      firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]     
      nonNA_index_firm_temp = nonNA_index_firm[firm_idx_temp]     
      onetoT = est$TS_index_firm_deflated[firm_idx_temp]
      
      if (est$no_obs_deflated[jj] >= ii ) {        
        total_idx_temp[firm_idx_temp[ onetoT[nonNA_index_firm_temp==ii-(2*window_size-2)]:onetoT[nonNA_index_firm_temp==ii] ]   ]=TRUE     
        total_idx_temp_20[firm_idx_temp[ onetoT[nonNA_index_firm_temp==1]:onetoT[nonNA_index_firm_temp==(2*window_size-1)] ]   ]=TRUE     
        if (est$no_obs_deflated[jj] >= 30 )
          total_idx_temp_10_30[firm_idx_temp[ onetoT[nonNA_index_firm_temp==window_size]:onetoT[nonNA_index_firm_temp==(3*window_size-2)] ]   ]=TRUE             
      }    
      
    }
    
    
    tryCatch({
      
      
      M0.phiphihat = lmer(ye_temp ~ ye_temp_lag1+ (1| firm_dummy ) + (-1 + ye_temp_lag1 | firm_dummy )   ,subset = total_idx_temp,  REML=TRUE)
      phi_learn_lmer00[ii-(2*window_size-2)] = fixef(M0.phiphihat)[2]
      phi_learn_lmer_var00[ii-(2*window_size-2)] =  (attr( VarCorr(M0.phiphihat)$firm_dummy.1 ,"stddev")[1])^2
      
      M0.phiphihat = lmer(yf_temp ~ ye_temp_lag1+ (1| firm_dummy ) + (-1 + ye_temp_lag1 | firm_dummy )   ,subset = total_idx_temp,  REML=TRUE)
      phihat_learn_lmer00[ii-(2*window_size-2)] = fixef(M0.phiphihat)[2]
      phihat_learn_lmer_var00[ii-(2*window_size-2)] =  (attr( VarCorr(M0.phiphihat)$firm_dummy.1 ,"stddev")[1])^2
      
      M0.phiphihat = lmer(FE_temp ~ ye_temp_lag1+ (1| firm_dummy ) + (-1 + ye_temp_lag1 | firm_dummy )   ,subset = total_idx_temp,  REML=TRUE)
      phiphihat_learn_lmer00[ii-(2*window_size-2)] = fixef(M0.phiphihat)[2]
      phiphihat_learn_lmer_var00[ii-(2*window_size-2)] =  (attr( VarCorr(M0.phiphihat)$firm_dummy.1 ,"stddev")[1])^2
      
      
      M0.phiphihat = lmer(FE_temp ~ ye_temp_lag1+ (1| firm_dummy ) + (-1 + ye_temp_lag1 | firm_dummy )   ,subset = total_idx_temp_20,  REML=TRUE)      
      phiphihat_learn_lmer_var00_20[ii-(2*window_size-2)] =  (attr( VarCorr(M0.phiphihat)$firm_dummy.1 ,"stddev")[1])^2
      
      M0.phiphihat = lmer(FE_temp ~ ye_temp_lag1+ (1| firm_dummy ) + (-1 + ye_temp_lag1 | firm_dummy )   ,subset = total_idx_temp_10_30,  REML=TRUE)
      phiphihat_learn_lmer_var00_10_30[ii-(2*window_size-2)] =  (attr( VarCorr(M0.phiphihat)$firm_dummy.1 ,"stddev")[1])^2
      
      
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




###############################################################
###############################################################
#######  learning  phi "within firm" from raw estimate  #######
###############################################################
############################################################### 
#1:20 - 21:40
#2:21 - 22:41
#3:22 - 23:42


if (do_all) {
  
  max_no_obs_deflated = max(est$no_obs_deflated)
  window_size = 10 
  no_window_learn = max_no_obs_deflated - window_size*2 + 2
  
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
    
    for (ii in c( (2*window_size-1):est$no_obs_deflated[jj] )) {
      
      if (est$no_obs_deflated[jj] >= ii ) {
        
        tryCatch({
          
          tempidx = rep(FALSE, length(firm_idx_temp) )
          tempidx[ onetoT[nonNA_index_firm_temp==ii-(2*window_size-2)]:onetoT[nonNA_index_firm_temp==ii] ]=TRUE
          
          Reg1 = lm(yye ~    yye.L1 , subset=tempidx        )          
          Reg3 = lm(yyf ~    yye.L1  , subset=tempidx        )          
          
          phiphihat_learn_0.alt[jj,ii-(2*window_size-2)] = Reg1$coef[2]- Reg3$coef[2]          
          phi_learn_0.alt[jj,ii-(2*window_size-2)] = Reg1$coef[2]
          phihat_learn_0.alt[jj,ii-(2*window_size-2)] = Reg3$coef[2]              
          N_phi_learn_0.alt[jj,ii-(2*window_size-2)] = sum(!is.na(yye + yye.L1))        
          
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
par(mfrow=c(1,2))

ssd = 1.96
x= fcn_1_1_winsor(phi_learn_firm$phiphihatest)
xxx = phi_learn$phiphihatvar
temp_scale = t(matrix(rep(1/apply( x , 2, sd,na.rm=T)*sqrt(xxx), dim(x)[1] ), ncol=dim(x)[1] ))

phiphihat_learn_0.1 = ((matrix(rep((x[,1])  ,94), ncol=94)) + x*0)
phiphihat_learn_0.11  = ((matrix(rep((x[,11])  ,94), ncol=94)) + x*0)

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


xaxis  = 0:93
T_temp = length(xxx)-3
xxx = xxx[1:(T_temp)]
xaxis  = xaxis[1:(T_temp)]
CIlow.1 =   CIlow.1[1:(T_temp)]
CIhigh.1 =  CIhigh.1[1:(T_temp)]

plot(NA,xlim=c(xaxis[1]-5, xaxis[length(xaxis)]), ylim=c(0, 0.0188),  
     main=expression(hat(sigma)[phi*",t"]^2*"="*hat("Var")*"[ ("*phi-hat(phi)*")"[italic("i, t+1:t+20")]*" ]"),
     xlab=expression("Quarter "*italic("t")*" since the first observation"),ylab="",
     cex.axis=0.8, tck= -0.02)
polygon( c(xaxis[1:(T_temp)], xaxis[T_temp:1]  ),  
         c(CIlow.1[1:(T_temp)], CIhigh.1[T_temp:1]), border = NA, col = "gray75")
#polygon( c(xaxis, xaxis[length(xaxis):1]  ),  
#         c(CIlow, CIhigh[length(xaxis):1]), border = NA, col = "gray55")
#lines(xaxis[1:91], phiphihat_learn_0.1.mean[1:91]  , cex=0.5, lwd=3  ,type="l", lty=1 ,col="gray75"   )
lines(xaxis[1:(T_temp)], phiphihat_learn_0.1.mean[1:(T_temp)]  , cex=0.5, lwd=1  ,type="l", lty=3  )
lines(xaxis[1:(T_temp)], phiphihat_learn_0_lmer.1.mean[1:(T_temp)], cex=0.5, lwd=1  ,type="l", lty=2)
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
########################### end of plot in the paper #######################



#######  1. MAtching learning phi calendar time from raw estimate  #######


if (do_all) {
  
  load("phi_learn_firm00.Rdata")    
  window_size = 10 
  
  phi_firmqtr_20_reg =  rep(NA, length(est$ye_firm_deflated ))
  phihat_firmqtr_20_reg = phi_firmqtr_20_reg 
  phiphihat_firmqtr_20_reg = phi_firmqtr_20_reg 
  
  phi_learn_0.alt = matrix(rep(NA, length(yearqtr_xaxis)*length(est$firm_begin_deflated )  ), ncol= length(yearqtr_xaxis)) 
  phihat_learn_0.alt  =  phi_learn_0.alt
  phiphihat_learn_0.alt  =  phi_learn_0.alt
  
  for (jj in  firm_truly_newborn   ) 
  {  
    firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]     
    nonNA_index_firm_temp = nonNA_index_firm[firm_idx_temp]      
    onetoT = est$TS_index_firm_deflated[firm_idx_temp]
    
    for (ii in c( 2*window_size-1)) {
      
      if (est$no_obs_deflated[jj] >= ii ) {
        
        withinfirm_idx_temp = onetoT[nonNA_index_firm_temp==ii-(2*window_size-2)]:onetoT[nonNA_index_firm_temp==ii]                
        phi_firmqtr_20_reg[ firm_idx_temp[withinfirm_idx_temp] ] = phi_learn_firm$phiest[jj,ii-(2*window_size-2)]
        phihat_firmqtr_20_reg[ firm_idx_temp[withinfirm_idx_temp] ] = phi_learn_firm$phihatest[jj,ii-(2*window_size-2)]
        phiphihat_firmqtr_20_reg[ firm_idx_temp[withinfirm_idx_temp] ] = phi_learn_firm$phiphihatest[jj,ii-(2*window_size-2)]
        
        yearqtr_idx_temp = yearqtr_idx_one[firm_idx_temp[withinfirm_idx_temp]]
        
        phi_learn_0.alt[jj, yearqtr_idx_temp ] = phi_learn_firm$phiest[jj,ii-(2*window_size-2)]
        phihat_learn_0.alt[jj, yearqtr_idx_temp ] =  phi_learn_firm$phihatest[jj,ii-(2*window_size-2)]
        phiphihat_learn_0.alt[jj, yearqtr_idx_temp ] = phi_learn_firm$phiphihatest[jj,ii-(2*window_size-2)]
      }
    }
    if (jj %% 100 ==0) print(jj/length(est$firm_begin_deflated ))    
  }
  
  
  
  phi_learn_firm = list(phiest = phi_learn_0.alt ,
                        phihatest  =  phihat_learn_0.alt,
                        phiphihatest = phiphihat_learn_0.alt)
  
  
  save(phi_learn_firm, file="phi_learn_firm00_cal.Rdata")    
  
  save(phi_firmqtr_20_reg, file="phi_firmqtr_20_reg.Rdata") 
  save(phihat_firmqtr_20_reg, file="phihat_firmqtr_20_reg.Rdata") 
  save(phiphihat_firmqtr_20_reg, file="phiphihat_firmqtr_20_reg.Rdata") 
  
}


#######  2. learning phi calendar time using lmer  #######


if (do_all) {  
  
  
  
  load("phiphihat_firmqtr_20_reg.Rdata") 
  window_size = 10 
  phi_learn_lmer00 = rep(NA, length(yearqtr_xaxis) )
  phi_learn_lmer_var00 = rep(NA, length(yearqtr_xaxis) ) 
  phihat_learn_lmer00 = rep(NA, length(yearqtr_xaxis) )
  phihat_learn_lmer_var00 = rep(NA,length(yearqtr_xaxis) ) 
  phiphihat_learn_lmer00 = rep(NA, length(yearqtr_xaxis) )
  phiphihat_learn_lmer_var00 = rep(NA, length(yearqtr_xaxis) ) 
  FE_rho_learn_pooled.reg = rep(NA, length(yearqtr_xaxis) ) 
  
  
  
  
  
  for (ii in c( 1:length(yearqtr_xaxis)   )) {    
    
    total_idx_temp = rep(FALSE, length(est$ye_firm_deflated)) 
    set_firm_idx_temp = est$firm_deflated_nn_idx[     yearqtr_idx_numeric==yearqtr_xaxis[ii]  &  nonNA_index_firm<(2*window_size)  &   nonNA_index_firm>0   ]
    
    for (jj in  intersect(set_firm_idx_temp, firm_truly_newborn ) )
    { 
      firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj] 
      
      total_idx_temp[  firm_idx_temp[ !is.na(phiphihat_firmqtr_20_reg[firm_idx_temp]) ]  ]=TRUE 
      
      
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
  
  save(phi_learn, file= "phi_learn_lmer00_cal.Rdata"  )
  save(FE_rho_learn_pooled.reg, file = "FE_rho_learn_pooled_reg_cal.Rdata"  )  
  
  
  
}



#######  measure var(phi-phihat) in the first plot  using lmer  #######
load("FErhoadj_firmqtr_all_center.Rdata")
load("age_proxy.Rdata")
old_age_q70   = apply(age_proxy$trueyearqtr_idx_numeric.panel ,2, quantile,0.5 ,na.rm=T)
old_age_q70   = t(matrix(rep(old_age_q70, dim(age_proxy$trueyearqtr_idx_numeric.panel)[1]), ncol=dim(age_proxy$trueyearqtr_idx_numeric.panel)[1]))
young_age_q30 = apply(age_proxy$trueyearqtr_idx_numeric.panel ,2, quantile,0.5,na.rm=T)
young_age_q30 = t(matrix(rep(young_age_q30, dim(age_proxy$trueyearqtr_idx_numeric.panel)[1]), ncol=dim(age_proxy$trueyearqtr_idx_numeric.panel)[1]))

old_firm_idx = age_proxy$trueyearqtr_idx_numeric.panel >=  old_age_q70
young_firm_idx = age_proxy$trueyearqtr_idx_numeric.panel <=   young_age_q30

old_firm_idx[is.na(old_firm_idx)] = FALSE
young_firm_idx[is.na(young_firm_idx)] = FALSE



#######  2-1. learning phi calendar time using lmer for tthe first plot  #######


if (do_all) {  
  
  # load("FErhoadj_firmqtr_all_fill_center.Rdata")
  load("FErhoadj_firmqtr_all_center.Rdata")
  
  for (age_type in  c("all","old","young")   ) {
    
    window_size = 10 
    phi_learn_lmer00 = rep(NA, length(yearqtr_xaxis) )
    phi_learn_lmer_var00 = rep(NA, length(yearqtr_xaxis) ) 
    phihat_learn_lmer00 = rep(NA, length(yearqtr_xaxis) )
    phihat_learn_lmer_var00 = rep(NA,length(yearqtr_xaxis) ) 
    phiphihat_learn_lmer00 = rep(NA, length(yearqtr_xaxis) )
    phiphihat_learn_lmer_var00 = rep(NA, length(yearqtr_xaxis) ) 
    FE_rho_learn_pooled.reg = rep(NA, length(yearqtr_xaxis) ) 
    
    
    if (age_type=="all") {
      age_group_idx = old_firm_idx
      age_group_idx[,] = TRUE
    }
    
    if (age_type=="old") age_group_idx = old_firm_idx 
    if (age_type=="young") age_group_idx = young_firm_idx
    
    
    for (ii in c( 1:length(yearqtr_xaxis)   )) {    
      
      total_idx_temp = rep(FALSE, length(est$ye_firm_deflated))  
      set_firm_idx_temp = which(   !is.na(FErhoadj_firmqtr_all[,ii] )  & age_group_idx[ , ii ]    )
      
      if  (length(set_firm_idx_temp)>0) {
        
        for (jj in  set_firm_idx_temp  )
        { 
          firm_idx_temp = est$firm_begin_deflated[jj]:est$firm_end_deflated[jj]    
          nonNA_index_firm_temp = nonNA_index_firm[firm_idx_temp]  
          kk = which(yearqtr_idx_one[firm_idx_temp] == ii)
          nonNA_index_firm_temp_left = nonNA_index_firm_temp[1:(kk-1)]
          nonNA_index_firm_temp_right = nonNA_index_firm_temp[(kk+1):length(nonNA_index_firm_temp)]
          
          no_nonNA_left = sum( nonNA_index_firm_temp_left>0,na.rm=T)      
          no_nonNA_right = sum(nonNA_index_firm_temp_right>0,na.rm=T)
          max_no_nonNA_left = max(  nonNA_index_firm_temp_left)
          min_no_nonNA_left = min( nonNA_index_firm_temp_right[ nonNA_index_firm_temp_right >0] )
          
          withinfirm_idx_temp  = seq(which(nonNA_index_firm_temp==(max_no_nonNA_left-window_size+2)),which(nonNA_index_firm_temp==(min_no_nonNA_left +window_size-2 )))         
          total_idx_temp[  firm_idx_temp[withinfirm_idx_temp]  ]=TRUE                     
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
        
      }
      print(ii)
    }
    
    phi_learn = list(phiest = phi_learn_lmer00, phivar = phi_learn_lmer_var00,
                     phihatest = phihat_learn_lmer00, phihatvar = phihat_learn_lmer_var00,
                     phiphihatest = phiphihat_learn_lmer00, phiphihatvar = phiphihat_learn_lmer_var00 )
    
    save(phi_learn, file= sprintf("phi_learn_lmer00_cal_%s.Rdata", age_type )   )
    save(FE_rho_learn_pooled.reg, file = sprintf("FE_rho_learn_pooled_reg_cal_%s.Rdata", age_type )  )  
    #   
    # save(phi_learn, file= sprintf("phi_learn_lmer00_cal_fill_%s.Rdata", age_type )   )
    # save(FE_rho_learn_pooled.reg, file = sprintf("FE_rho_learn_pooled_reg_cal_fill_%s.Rdata", age_type )  )  
    
  }
  
  
}



load("phi_learn_lmer00_cal_all.Rdata")
plot(yearqtr_xaxis,phi_learn$phiphihatvar, type="o")
load("phi_learn_lmer00_cal_old.Rdata")
lines(yearqtr_xaxis,phi_learn$phiphihatvar, lty=2)
a=phi_learn$phiphihatvar
load("phi_learn_lmer00_cal_young.Rdata")
plot(yearqtr_xaxis,phi_learn$phiphihatvar-a, type="o")


load("phi_learn_lmer00_cal_old.Rdata") 
b=phi_learn$phiphihatest
load("phi_learn_lmer00_cal_young.Rdata")
plot(phi_learn$phiphihatvar+phi_learn$phiphihatest^2-(b^2+a), type="o")






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


# info quality measure
plot(yearqtr_xaxis[1:111],var_n_phi[1:111], type="o")
plot(yearqtr_xaxis[1:111],var_n_phi[1:111]/phi_learn$phivar[1:111], type="o")

######################################
######################################
####### var_t(phi_i-phihat_i) & info quality measure
######################################
######################################
######################################
plot(yearqtr_xaxis[1:111],phi_learn$phiphihatvar[1:111] ,type="l", ylim = c(0,0.02))
lines(yearqtr_xaxis[1:111],var_n_phi[1:111], lty=3)

# var_t(phi_i) & hat(var)(phi_i) in prior of phi_i
plot(yearqtr_xaxis[1:111],phi_learn$phivar[1:111], type="l", ylim = c(0,0.12))
lines(yearqtr_xaxis[1:111],hatvar_phi[1:111],lty=3)





#######################################################################
#######################################################################
#######  model implied autocorr in FE(1:20) due to learning of phi prior over calendar  #########
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
  for (ii in seq(1, sum(!is.na(var_est_phi) ))   ) {
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
  
  for (ii in seq(1 , (length(phiest_ar)) )   ) {
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
load("FErhoadj_firmqtr_20_center_reg.Rdata") 
FE_rho_learn_est = apply(fcn_1_1_winsor(FErhoadj_firmqtr_20_reg),2,mean,na.rm=T)
#load("phi_learn_0.Rdata") 
#phiest_ar = (phi_learn$phiest *( 20  - 1)+1) / ( 20 - 4)
#phiphihatest_ar = phi_learn$phiphihatest 

Ttemp = sum(  !is.na( phiest_ar+phiphihatest_ar+FE_rho_learn_est ) )

phiest_ar = phiest_ar[1:Ttemp]
phiphihatest_ar = phiphihatest_ar[1:Ttemp]
FE_rho_learn_est = FE_rho_learn_est[1:Ttemp]


#### Local variables: Initial values ####
calibpar= list()
calibpar$phi0 = phiest_ar
calibpar$phihat0 = phiest_ar-phiphihatest_ar
calibpar$Khat0 = phiest_ar*0+0.7

#### Locally global variables: Initial values ####
# phi_learn_imp_rho_FE 
# phiphihat_ar_bias 

## assume phi=phihat=0.5

if (do_all){ 
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
}

## assume phi_t=phihat_t
if (do_all){ 
  load("phi_learn_imp_rho_FE_cal.Rdata")
  phiphihat_ar_bias = phiphihatest_ar
  for (kk in c(1:5)) {  
    calibpar = fcn_calib_phi_phihat_Khat()
    phi_imp_rho_FE =  fcn_varphiphihat_imp_rho_FE_cal(calibpar,NN=10^6)
    phi_learn_imp_rho_FE =  phi_imp_rho_FE[,1] - phi_imp_rho_FE[,2]
    
    save(phi_imp_rho_FE,file="phi_imp_rho_FE_phiphihat0_cal.Rdata")
  }
  plot(calibpar$phi0-calibpar$phihat0)
}


## assume bias and true moments of phi-phihat cancels out
if (do_all){ 
  phiphihat_ar_bias = phiphihatest_ar*0
  load("phi_learn_imp_rho_FE_cal.Rdata")
  for (kk in c(1:5)) {
    calibpar = fcn_calib_phi_phihat_Khat()
    phi_imp_rho_FE =  fcn_varphiphihat_imp_rho_FE_cal(calibpar,NN=10^6)
    phi_learn_imp_rho_FE =  phi_imp_rho_FE[,1] - phi_imp_rho_FE[,2]
    
    save(phi_imp_rho_FE,file="phi_imp_rho_FE_cal.Rdata")
  }
  
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

load("FErhoadj_firmqtr_20_center_reg.Rdata") 
FE_rho_learn_est = apply(fcn_1_1_winsor(FErhoadj_firmqtr_20_reg),2,mean,na.rm=T)
load("phi_imp_rho_FE_phi05_cal.Rdata")
load("FE_rho_learn_pooled_reg_cal.Rdata")


xaxis  =  yearqtr_xaxis
Tmaxplot = 109

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




plot(NA,xlim=c(xaxis[1]-0.5, xaxis[length(xaxis)]), ylim=c(0, 2),  
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
load("FErhoadj_firmqtr_20_center_reg.Rdata") 
FE_rho_learn_est = apply(fcn_1_1_winsor(FErhoadj_firmqtr_20_reg),2,mean,na.rm=T)

xaxis  =  yearqtr_xaxis
Tmaxplot = 109

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




plot(NA,xlim=c(xaxis[1]-0.5, xaxis[length(xaxis)]), ylim=c(0, 2),  
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
