##########################################################
# Description	: Main code for 
#               "Reading the Tea Leaves: Model Uncertainty, Robust Forecasts, 
#                and Serial-Correlation in Analysts'  Forecast Errors" 
#                Journal of Financial Economics, Volume 122, Issue 1, 2016, pp.42-64 
# Input files	: IBES Analysts' Earnigns Forecasts data
# Author		  : James Yae
##########################################################


##########################################################
##########################################################
##########################################################
#################     Data Load    #######################
##########################################################
##########################################################
########################################################## 

rm(list=ls(all=TRUE))
do_all = TRUE # If FALSE, skip the tasks that are previously done and saved

# IBES data loading
FE=read.csv("ibes_crsp_compustat.txt",head=TRUE,sep=",") 
head(FE)
summary(FE) 
N_row = dim(FE)[1]
 

# ticker: ticker symbol
# fpedats: end of the fiscal quarter
# cusip 
# companyname 
# report_date: earnings announcement date
# actual_eps: actual EPS
# avg_estimate: avg of EPS estimates
# no_of_estimates: number of EPS estimates
# sd_estimate: standard devition of EPS estimates
# median_estimate: median of EPS estimates
# min_estimate: smallest EPS estimate
# max_estimate: largest EPS estimate
# no_of_primary: number of estimates flagged as 'primary' (ignore)
# no_of_dilu~d: number of estimates flagged as 'diluted' (ignore)
# yyyymm: report_date in yyyy-mm format
# permno: CRSP permno
# exchcd: exchcd==1: NYSE, 2 or 3: Amex and NASDAQ
# prc: share price from CRSP/Compustat. 
# marketvalue: market value of equity in millions of dollars
# shrout2:  shares outstanding (in millions) from CRSP/Compustat.  
# dateindex: an index for month (1, ., )
# at: total assets from Compustat
# sales: sales from Compustat
# be: book value of equity from Compustat, defined using French's formula
# volatility: volatility: annualized historical stock volatility
# size_cat: size category using NYSE breakpoints: 1 = below 20th percentile; 2 = below 50th percentile; 3 = larger than 50th percentile
# ffindustry: 1, ., 49 Fama-French industries
# firmage: firm age in years, computed using the difference between today's date and the firm's first CRSP appearance
# price: share price from IBES
# shout: shares outstanding (in millions) from IBES


# Data treatment step 1

#################################################################
#################################################################
################# I. Identify Earnings Models #################
#################################################################
#################################################################

########### 1) Find the best scaling variables for earnings #################
source('fcn_data_treatment_step1_IBES.r')
source('fcn_data_treatment_step2_IBES.r')

if (do_all) {
  fcn_data_treatment_step1_IBES(dataset=FE)
}
load("new_variables_IBES_common.Rdata")

if (do_all) {
  optionlist=list(aggregateforecast="median", scalemax=Inf, scalemin=5, yearmin=1984 , yearmax=Inf ,FEmPESmax=10) 
  #optionlist=list(aggregateforecast="median", scalemax=Inf, scalemin=0, yearmin= (-Inf) , yearmax=Inf ,FEmPESmax=Inf) 
  fcn_data_treatment_step2_IBES(dataset=FE, scaling_variable= prc, filename="prc" , option=optionlist   )
  
  
  #optionlist=list(aggregateforecast="median", scalemax=Inf, scalemin=0, yearmin=1984 , yearmax=Inf ,FEmPESmax=10) 
  optionlist=list(aggregateforecast="median", scalemax=Inf, scalemin=0, yearmin= (-Inf) , yearmax=Inf ,FEmPESmax=Inf) 
  fcn_data_treatment_step2_IBES(dataset=FE, scaling_variable= at/shout, filename="at" ,option=optionlist )
  fcn_data_treatment_step2_IBES(dataset=FE, scaling_variable= (prc*0.5 + 0.5*at/shout), filename="prc5at5" ,option=optionlist )
  fcn_data_treatment_step2_IBES(dataset=FE, scaling_variable= (sales/shout) , filename="sales_pos" ,option=optionlist )
  fcn_data_treatment_step2_IBES(dataset=FE, scaling_variable= (at+ (marketvalue-be)*0.1)/shout , filename="campbell",option=optionlist )
  fcn_data_treatment_step2_IBES(dataset=FE, scaling_variable= 0*prc+1 , filename="none",option=optionlist )
}


scaleVset = c("prc")
thisfilename = sprintf("new_variables_IBES_%s.Rdata", scaleVset )
load(thisfilename)

#In the beginning#
print(data_treat_history )


########### 2) Compare TS models for Earnings #################

load("new_variables_IBES_common.Rdata")
library(moments)
library(quantreg)
source('fcn_winsorization.r')
source('fcn_truncate_idx.r')
source('fcn_IQR_truncate_idx.r')
source('fcn_IQR_truncate_idx2.r')
source('fcn_filename_maker.r')
source('fcn_firmlevel_TS_make.r')
source('fcn_firmlevel_TS_make_last_trunc.r')

source('fcn_firmlevel_TS_make_pooled_trunc.r')
source('fcn_firmlevel_TS_make.r')
source('fcn_firmlevel_TS_make_general.r')

option = list( ye_trunc=TRUE,yf_trunc=TRUE,yef_trunc=TRUE, truncate="quantile", truncate_crt=0.01 )
scaleVset = c("prc")

#non_converge_set = c(272,627,2277,2549,2959 ) ## nn 
non_converge_set = c(1340,2791,10243,11509,13331) ##jj



if (do_all) {
  #fcn_firmlevel_TS_make_pooled_trunc(scaleVset, option=option )
  fcn_firmlevel_TS_make(scaleVset, option=option, maxlength=20 ) 
  #fcn_firmlevel_TS_make_general(scaleVset, option=option, maxlength=20 ,separ_firms=FALSE) 
}
#optionlist = list( ye_trunc=TRUE,yf_trunc=TRUE,yef_trunc=TRUE, truncate="IQR2", truncate_crt=0.01 )

filename = fcn_filename_maker(scaleVset=scaleVset, option=option )
load(file=filename)
tempidx = !is.na(firm_TS_data$ye_firm_deflated) 



### A) firm-level AR/MA estimation ###
require(FKF)
source('fcn_KF_general.r')
source('fcn_AR1_MA1_models_compare_firmlevel.r') 
source('fcn_firm_level_est_table.r') 
source('fcn_firm_level_est_table_KF.r') 
source('fcn_firm_level_simul_table_KF.r')
source('fcn_summary_stat.r')
source('fcn_summary_stat_quarterly.r')
source('fcn_summary_stat_quarterly_table.r')
source('fcn_simul_ARIMA_firmlevel.r')
source('fcn_simul_ARIMA_firmlevel_KF.r')
source('fcn_KF_AR1_para_est.r')
source('fcn_KF_MA1_para_est.r')
source('fcn_KF_ARMA11_para_est.r')


if (do_all) {
  est0 = fcn_AR1_MA1_models_compare_firmlevel( scaleVset, option=option)
  save(est0, file = "est_firm_level_prc_cut1.Rdata")
}


load( "est_firm_level_prc_cut1.Rdata")
fcn_firm_level_est_table(est0)


load( "est_firm_level_prc_cut1.Rdata")
fcn_firm_level_est_table_KF(est0)



#### firm level simulation & calibration
set.seed( round(runif(1)*2485)   ) 
arcoef_set = 0.48 
sigmaa2_sigmae2_set = 0.1^2
KK= 30
savefilename =sprintf("firm_simul_ar%d_sigasige%d.Rdata",arcoef_set*100,sqrt(sigmaa2_sigmae2_set)*100 ) 

if (do_all) {
  fcn_simul_ARIMA_firmlevel(scaleVset=scaleVset, option=option ,
                            savefilename =savefilename, 
                            arcoef_set = arcoef_set ,  
                            sigmaa2_sigmae2_set = sigmaa2_sigmae2_set, KK=KK) 
}

load(savefilename) 


#### firm level simulation & calibration
set.seed( round(runif(1)*2485)   ) 
arcoef_set = 0.48 
sigmaa2_sigmae2_set = 0.1^2
KK= 30
savefilename =sprintf("firm_simul_ar%d_sigasige%d_KF.Rdata",arcoef_set*100,sqrt(sigmaa2_sigmae2_set)*100 ) 

if (do_all) {
  fcn_simul_ARIMA_firmlevel(scaleVset=scaleVset, option=option ,
                            savefilename =savefilename, 
                            arcoef_set = arcoef_set ,  
                            sigmaa2_sigmae2_set = sigmaa2_sigmae2_set, KK=KK) 
}

load(savefilename) 


load( "est_firm_level_prc_cut1.Rdata")
fcn_firm_level_simul_table_KF(est0)



### B) Pooled estimation ###

source('fcn_KF_general.r')
source('fcn_pooled_TS_make.r')
source('fcn_AR1_MA1_models_compare_pooled.r')
source('fcn_arima_LTY_random_data.r')
source('fcn_VARMA_data_make.r')
source('fcn_pooled_est_table.r') 
source('fcn_simul_ARIMA_pooled.r')
source('fcn_simul_ARIMA_pooled_table.r')
source('fcn_simul_ARIMA_pooled_table_all.r')


### summary statistics ###
if (do_all) {
  qtrsumstat = fcn_summary_stat_quarterly(scaleVset, option=option )
  save(qtrsumstat , file= "quarterly_sumstat_prc_cut1.Rdata")
}

load( "quarterly_sumstat_prc_cut1.Rdata")
fcn_summary_stat_quarterly_table(qtrsumstat , scaleVset, option=option) 







if (do_all) {
  est1 = fcn_AR1_MA1_models_compare_pooled( scaleVset, option=option, fixedeffect=FALSE)
  save(est1, file = "est_pooled_prc_cut1.Rdata")
  est1 = fcn_AR1_MA1_models_compare_pooled( scaleVset, option=option, fixedeffect=TRUE)
  save(est1, file = "est_pooled_prc_cut1_fixedeffect.Rdata")
  
}

load("est_pooled_prc_cut1.Rdata")
fcn_pooled_est_table(est1)

impsigmaa = as.numeric(sqrt(max(0,-1*est1$arma11$matheta/est1$arma11$phi)))
impsigmae = as.numeric(sqrt(1+est1$arma11$matheta^2 -(1+est1$arma11$phi^2)*impsigmaa ^2 ))
est_sigmaa_sigmae = impsigmaa/impsigmae
est_sigmaa_sigmae


load("est_pooled_prc_cut1_fixedeffect.Rdata")
fcn_pooled_est_table(est1 )

impsigmaa = as.numeric(sqrt(max(0,-1*est1$arma11$matheta/est1$arma11$phi)))
impsigmae = as.numeric(sqrt(1+est1$arma11$matheta^2 -(1+ est1$arma11$phi^2)*impsigmaa ^2 ))
est_sigmaa_sigmae = impsigmaa/impsigmae
est_sigmaa_sigmae


### B) Pooled estimation - simulation ###
theta_start_all=list()
MM=1000
set.seed( round(runif(1)*10^6)   )
######## AR1 ########
modelorder=c(1,0,0)
theta_start = list()
theta_start$phi = 0.47
theta_start$ma = 0   

fixedeffect=FALSE
if (do_all) fcn_simul_ARIMA_pooled(modelorder=modelorder, scaleVset=scaleVset, option=option,
                                   fixedeffect=fixedeffect, theta_start=theta_start, MM=MM) 
fcn_simul_ARIMA_pooled_table(modelorder=modelorder, theta_start=theta_start, fixedeffect=fixedeffect)

fixedeffect=TRUE
if (do_all) fcn_simul_ARIMA_pooled(modelorder=modelorder, scaleVset=scaleVset, option=option,
                                   fixedeffect=fixedeffect, theta_start=theta_start, MM=MM) 
fcn_simul_ARIMA_pooled_table(modelorder=modelorder, theta_start=theta_start, fixedeffect=fixedeffect)

theta_start_all$AR1 = theta_start



######## MA1 ######## 
modelorder=c(0,0,1) 
theta_start = list()
theta_start$phi = 0
theta_start$ma = 0.44  

fixedeffect=FALSE
if (do_all) fcn_simul_ARIMA_pooled(modelorder=modelorder, scaleVset=scaleVset, option=option,
                                   fixedeffect=fixedeffect, theta_start=theta_start, MM=MM) 
fcn_simul_ARIMA_pooled_table(modelorder=modelorder, theta_start=theta_start, fixedeffect=fixedeffect)

fixedeffect=TRUE
if (do_all) fcn_simul_ARIMA_pooled(modelorder=modelorder, scaleVset=scaleVset, option=option,
                                   fixedeffect=fixedeffect, theta_start=theta_start, MM=MM) 
fcn_simul_ARIMA_pooled_table(modelorder=modelorder, theta_start=theta_start, fixedeffect=fixedeffect)

theta_start_all$MA1 = theta_start


######## ARAM11  ########
modelorder=c(1,0,1) 
theta_start = list()
theta_start$phi = 0.51 
theta_start$ma = -0.036
impsigmaa = as.numeric(sqrt(max(0,-1*theta_start$ma/theta_start$phi )))
impsigmae = as.numeric(sqrt(1+theta_start$ma^2 -(1+theta_start$phi^2)*impsigmaa^2 ))
theta_start$sigmaa_sigmae = (impsigmaa/impsigmae)

fixedeffect=FALSE
if (do_all) fcn_simul_ARIMA_pooled(modelorder=modelorder, scaleVset=scaleVset, option=option,
                                   fixedeffect=fixedeffect, theta_start=theta_start, MM=MM) 
fcn_simul_ARIMA_pooled_table(modelorder=modelorder, theta_start=theta_start, fixedeffect=fixedeffect)

fixedeffect=TRUE
if (do_all) fcn_simul_ARIMA_pooled(modelorder=modelorder, scaleVset=scaleVset, option=option,
                                   fixedeffect=fixedeffect, theta_start=theta_start, MM=MM) 
fcn_simul_ARIMA_pooled_table(modelorder=modelorder, theta_start=theta_start, fixedeffect=fixedeffect)

theta_start_all$AR1MA1 = theta_start


#####
fcn_simul_ARIMA_pooled_table_all(theta_start_all=theta_start_all, fixedeffect=FALSE)
fcn_simul_ARIMA_pooled_table_all(theta_start_all=theta_start_all, fixedeffect=TRUE)









### B) Pooled estimation - simulation with bootstrap ###
source('fcn_arima_LTY_random_data_BT.r') 
source('fcn_simul_ARIMA_pooled_bootstrap.r')

theta_start_all_BT=list()
MM=1000
set.seed( round(runif(1)*10^6)   )
######## AR1 ########
modelorder=c(1,0,0)
theta_start = list()
theta_start$phi = 0.47
theta_start$ma = 0   

fixedeffect=FALSE
if (do_all) fcn_simul_ARIMA_pooled_bootstrap(modelorder=modelorder, scaleVset=scaleVset, option=option,
                                             fixedeffect=fixedeffect,   MM=MM) 

fixedeffect=TRUE
if (do_all) fcn_simul_ARIMA_pooled_bootstrap(modelorder=modelorder, scaleVset=scaleVset, option=option,
                                             fixedeffect=fixedeffect,   MM=MM) 

theta_start_all_BT$AR1 = theta_start



######## MA1 ######## 
modelorder=c(0,0,1) 
theta_start = list()
theta_start$phi = 0
theta_start$ma = 0.44  

fixedeffect=FALSE
if (do_all) fcn_simul_ARIMA_pooled_bootstrap(modelorder=modelorder, scaleVset=scaleVset, option=option,
                                             fixedeffect=fixedeffect,  MM=MM) 

fixedeffect=TRUE
if (do_all) fcn_simul_ARIMA_pooled_bootstrap(modelorder=modelorder, scaleVset=scaleVset, option=option,
                                             fixedeffect=fixedeffect,   MM=MM) 

theta_start_all_BT$MA1 = theta_start


######## ARAM11  ########
modelorder=c(1,0,1) 
theta_start = list()
theta_start$phi = 0.51 
theta_start$ma = -0.036
impsigmaa = as.numeric(sqrt(max(0,-1*theta_start$ma/theta_start$phi )))
impsigmae = as.numeric(sqrt(1+theta_start$ma^2 -(1+theta_start$phi^2)*impsigmaa^2 ))
theta_start$sigmaa_sigmae = (impsigmaa/impsigmae)

fixedeffect=FALSE
if (do_all) fcn_simul_ARIMA_pooled_bootstrap(modelorder=modelorder, scaleVset=scaleVset, option=option,
                                             fixedeffect=fixedeffect,  MM=MM) 

fixedeffect=TRUE
if (do_all) fcn_simul_ARIMA_pooled_bootstrap(modelorder=modelorder, scaleVset=scaleVset, option=option,
                                             fixedeffect=fixedeffect,   MM=MM) 

theta_start_all_BT$AR1MA1 = theta_start


#####



### B) Pooled estimation - simulation with equicorr ###
require('corpcor')
require('spcov')
source('fcn_est_covM.r') 
source('fcn_arima_LTY_random_data_COV.r') 
source('fcn_simul_ARIMA_pooled_COV.r')

theta_start_all_COV=list()
MM=1000
set.seed( round(runif(1)*10^6)   )
######## AR1 ########
modelorder=c(1,0,0)
theta_start = list()
theta_start$phi = 0.47
theta_start$ma = 0   
equicorr = 0.1

fixedeffect=FALSE
if (do_all) fcn_simul_ARIMA_pooled_COV(modelorder=modelorder, scaleVset=scaleVset, option=option,
                                       fixedeffect=fixedeffect, theta_start=theta_start,  equicorr =equicorr,  MM=MM) 


fixedeffect=TRUE
if (do_all) fcn_simul_ARIMA_pooled_COV(modelorder=modelorder, scaleVset=scaleVset, option=option,
                                       fixedeffect=fixedeffect, theta_start=theta_start,  equicorr =equicorr,  MM=MM) 

theta_start_all_COV$AR1 = theta_start



######## MA1 ######## 
modelorder=c(0,0,1) 
theta_start = list()
theta_start$phi = 0
theta_start$ma = 0.44  

fixedeffect=FALSE
if (do_all) fcn_simul_ARIMA_pooled_COV(modelorder=modelorder, scaleVset=scaleVset, option=option,
                                       fixedeffect=fixedeffect, theta_start=theta_start,  equicorr =equicorr,  MM=MM) 

fixedeffect=TRUE
if (do_all) fcn_simul_ARIMA_pooled_COV(modelorder=modelorder, scaleVset=scaleVset, option=option,
                                       fixedeffect=fixedeffect, theta_start=theta_start,  equicorr =equicorr,  MM=MM) 

theta_start_all_COV$MA1 = theta_start


######## ARAM11  ########
modelorder=c(1,0,1) 
theta_start = list()
theta_start$phi = 0.51 
theta_start$ma = -0.036
impsigmaa = as.numeric(sqrt(max(0,-1*theta_start$ma/theta_start$phi )))
impsigmae = as.numeric(sqrt(1+theta_start$ma^2 -(1+theta_start$phi^2)*impsigmaa^2 ))
theta_start$sigmaa_sigmae = (impsigmaa/impsigmae)

fixedeffect=FALSE
if (do_all) fcn_simul_ARIMA_pooled_COV(modelorder=modelorder, scaleVset=scaleVset, option=option,
                                       fixedeffect=fixedeffect, theta_start=theta_start,  equicorr =equicorr,  MM=MM) 

fixedeffect=TRUE
if (do_all) fcn_simul_ARIMA_pooled_COV(modelorder=modelorder, scaleVset=scaleVset, option=option,
                                       fixedeffect=fixedeffect, theta_start=theta_start,  equicorr =equicorr,  MM=MM) 

theta_start_all_COV$AR1MA1 = theta_start



#################################################################
#################################################################
################# I. VARMA Estimation #################
#################################################################
#################################################################
require('FKF')
source('fcn_pooled_TS_make.r')
source('fcn_VARMA_pooled_est.r')
source('fcn_KF_VARMA11_LTY_est.r')
source('fcn_KF_VARMA11_LTY_2signal_est.r')
source('fcn_MM_VARMA11_LTY_est.r') 
source('fcn_VARMA11_LTY_random_data.r')
source('fcn_VARMA11_LTY_random_data_MM.r') 
source('fcn_VARMA_regression.r')
source('fcn_implied_K_w_rho.r')
source('fcn_simul_VARMA11_LTY.r')
source('fcn_VARMA_MLE_table.r')

#############################################################
#############################################################
###################   VARMA MLE  ######################### 
#############################################################
#############################################################

if (do_all) {
  
  ## Transform firm-level data to VARMA pooled data
  fixedeffect=FALSE
  yt= fcn_VARMA_data_make(scaleVset=scaleVset, option=option, fixedeffect=fixedeffect)
  KFfit = fcn_KF_VARMA11_LTY_est( yt=yt , mueest=TRUE)
  save(KFfit, file = "KFfit_VARMA_MLE.Rdata")
  
  fixedeffect=TRUE
  yt= fcn_VARMA_data_make(scaleVset=scaleVset, option=option, fixedeffect=fixedeffect)
  KFfit = fcn_KF_VARMA11_LTY_est( yt=yt , mueest=TRUE)
  save(KFfit, file = "KFfit_VARMA_MLE_fixedeffect.Rdata")
  
}



load("KFfit_VARMA_MLE.Rdata")
c(as.numeric(KFfit$par)[3:6],exp(as.numeric(KFfit$par)[7]-as.numeric(KFfit$par)[8]),
  exp(as.numeric(KFfit$par)[9]-as.numeric(KFfit$par)[8]))
Implist = fcn_implied_K_w_rho( matrix(as.numeric(KFfit$par)[3:9], ncol=1) ) 
Implist

load("KFfit_VARMA_MLE_fixedeffect.Rdata")
c(as.numeric(KFfit$par)[3:6],exp(as.numeric(KFfit$par)[7]-as.numeric(KFfit$par)[8]),
  exp(as.numeric(KFfit$par)[9]-as.numeric(KFfit$par)[8]))
Implist = fcn_implied_K_w_rho( matrix(as.numeric(KFfit$par)[3:9], ncol=1) ) 
Implist





#############################################################
## Simulation for VARMA MLE results ###   
#############################################################
MM=50
set.seed( round(runif(1)*10^2)*25   )   


fixedeffect=FALSE

if (fixedeffect) temp = "_fixedeffect" else temp = ""
load(sprintf("KFfit_VARMA_MLE%s.Rdata", temp) )  

simulnumber = 2
if (do_all) fcn_simul_VARMA11_LTY(scaleVset=scaleVset, option=option,fixedeffect=fixedeffect,
                                  KFfit=KFfit, MM=MM, simulnumber=simulnumber)



fixedeffect=TRUE

if (fixedeffect) temp = "_fixedeffect" else temp = ""
load(sprintf("KFfit_VARMA_MLE%s.Rdata", temp) )  
KFfit$par["phi"]=KFfit$par["phi"]*1.07
KFfit$par["phihat"]=KFfit$par["phihat"]*1.06
KFfit$par["logsigmaa"] = KFfit$par["logsigmaa"] + log(0.9)
KFfit$par["Khat"]=KFfit$par["Khat"]*1.2

simulnumber = 1
if (do_all) fcn_simul_VARMA11_LTY(scaleVset=scaleVset, option=option,fixedeffect=fixedeffect,
                                  KFfit=KFfit, MM=MM, simulnumber=simulnumber)



#n=3000;x=matrix(rnorm(n^2),ncol=n);x=x%*%t(x);a=chol(x);chol2inv(a)%*%x



#############################################################
###################   end of simulatoin  ######################### 
############################################################# 


#theta_sim_VARMA_MLE_fixedeffect1
# SE(k-Khat)


fcn_VARMA_MLE_table(  fixedeffect=FALSE)   
fcn_VARMA_MLE_table(  fixedeffect=TRUE)   








#############################################################
## Simulation for VARMA MLE results- Bootstrapping ###   
#############################################################


source('fcn_VARMA11_LTY_random_data_BT.r') 
source('fcn_simul_VARMA11_LTY_BT.r')
source('fcn_VARMA_MLE_table_general.r')


MM=50
set.seed( round(runif(1)*10^2)*25   )   


fixedeffect=FALSE

if (fixedeffect) temp = "_fixedeffect" else temp = ""
load(sprintf("KFfit_VARMA_MLE%s.Rdata", temp) )  

simulnumber = 1
if (do_all) fcn_simul_VARMA11_LTY_BT(scaleVset=scaleVset, option=option,fixedeffect=fixedeffect,
                                     MM=MM, simulnumber=simulnumber)



fixedeffect=TRUE

if (fixedeffect) temp = "_fixedeffect" else temp = ""
load(sprintf("KFfit_VARMA_MLE%s.Rdata", temp) )  
KFfit$par["phi"]=KFfit$par["phi"]*1.07
KFfit$par["phihat"]=KFfit$par["phihat"]*1.06
KFfit$par["logsigmaa"] = KFfit$par["logsigmaa"] + log(0.9)
KFfit$par["Khat"]=KFfit$par["Khat"]*1.2

simulnumber = 1
if (do_all) fcn_simul_VARMA11_LTY_BT(scaleVset=scaleVset, option=option,fixedeffect=fixedeffect,
                                     MM=MM, simulnumber=simulnumber)



#n=3000;x=matrix(rnorm(n^2),ncol=n);x=x%*%t(x);a=chol(x);chol2inv(a)%*%x

fcn_VARMA_MLE_table_general(filename="BT", fixedeffect=TRUE,simulnumber=1)
fcn_VARMA_MLE_table_general(filename="BT", fixedeffect=FALSE,simulnumber=1)

#############################################################
###################   end of simulatoin  ######################### 
############################################################# 







#############################################################
## Simulation for VARMA MLE results- Equicorr ###   
#############################################################

source('fcn_VARMA11_LTY_random_data_COV.r') 
source('fcn_simul_VARMA11_LTY_COV.r')



MM=50
set.seed( round(runif(1)*10^2)*25   )   
equicorr = 0.03

fixedeffect=FALSE

if (fixedeffect) temp = "_fixedeffect" else temp = ""
load(sprintf("KFfit_VARMA_MLE%s.Rdata", temp) )  

simulnumber = 1
if (do_all) fcn_simul_VARMA11_LTY_COV(scaleVset=scaleVset, option=option,fixedeffect=fixedeffect,
                                      KFfit=KFfit, MM=MM, simulnumber=simulnumber, equicorr=equicorr)



fixedeffect=TRUE

if (fixedeffect) temp = "_fixedeffect" else temp = ""
load(sprintf("KFfit_VARMA_MLE%s.Rdata", temp) )  
KFfit$par["phi"]=KFfit$par["phi"]*1.07
KFfit$par["phihat"]=KFfit$par["phihat"]*1.06
KFfit$par["logsigmaa"] = KFfit$par["logsigmaa"] + log(0.9)
KFfit$par["Khat"]=KFfit$par["Khat"]*1.2

simulnumber = 1
if (do_all) fcn_simul_VARMA11_LTY_COV(scaleVset=scaleVset, option=option,fixedeffect=fixedeffect,
                                      KFfit=KFfit, MM=MM, simulnumber=simulnumber, equicorr=equicorr)



#n=3000;x=matrix(rnorm(n^2),ncol=n);x=x%*%t(x);a=chol(x);chol2inv(a)%*%x


fcn_VARMA_MLE_table_general(filename=sprintf("COV_corr%d",equicorr*100), fixedeffect=TRUE,simulnumber=1)
fcn_VARMA_MLE_table_general(filename=sprintf("COV_corr%d",equicorr*100), fixedeffect=FALSE,simulnumber=1)


#############################################################
###################   end of simulatoin  ######################### 
############################################################# 






#############################################################
## Simulation for VARMA MLE results- Heterogeneity in phi, phihat ###   
#############################################################

source('fcn_VARMA11_LTY_random_data_hetePHI.r') 
source('fcn_simul_VARMA11_LTY_hetePHI.r')

MM=50
set.seed( round(runif(1)*10^2)*25   )   
sd_phi=0.1

fixedeffect=FALSE

if (fixedeffect) temp = "_fixedeffect" else temp = ""
load(sprintf("KFfit_VARMA_MLE%s.Rdata", temp) )  

simulnumber = 1
if (do_all) fcn_simul_VARMA11_LTY_hetePHI(scaleVset=scaleVset, option=option,fixedeffect=fixedeffect,
                                          KFfit=KFfit, MM=MM, simulnumber=simulnumber,sd_phi=sd_phi)



fixedeffect=TRUE

if (fixedeffect) temp = "_fixedeffect" else temp = ""
load(sprintf("KFfit_VARMA_MLE%s.Rdata", temp) )  
KFfit$par["phi"]=KFfit$par["phi"]*1.07
KFfit$par["phihat"]=KFfit$par["phihat"]*1.06
KFfit$par["logsigmaa"] = KFfit$par["logsigmaa"] + log(0.9)
KFfit$par["Khat"]=KFfit$par["Khat"]*1.2

simulnumber = 1
if (do_all) fcn_simul_VARMA11_LTY_hetePHI(scaleVset=scaleVset, option=option,fixedeffect=fixedeffect,
                                          KFfit=KFfit, MM=MM, simulnumber=simulnumber,sd_phi=sd_phi)



#n=3000;x=matrix(rnorm(n^2),ncol=n);x=x%*%t(x);a=chol(x);chol2inv(a)%*%x


fcn_VARMA_MLE_table_general(filename=sprintf("hetePHI_sdphi%d",sd_phi*100), fixedeffect=TRUE,simulnumber=1)
fcn_VARMA_MLE_table_general(filename=sprintf("hetePHI_sdphi%d",sd_phi*100), fixedeffect=FALSE,simulnumber=1)


#############################################################
###################   end of simulatoin  ######################### 
############################################################# 







source('fcn_VARMA11_LTY_random_data_firm_level.r')
source('fcn_VARMA11_LTY_random_data_firm_level_heterophi.r')
source('fcn_simul_VARMA11_LTY_hetero_phiphihat_MM.r')
source('fcn_simul_VARMA11_LTY_hetero_mumuhat_MM.r')
source('fcn_VARMA11_LTY_hetero_MM.r')
set.seed( round(runif(1)*10^2)*25   )  
MM=300

## No fixedeffect - meaningless for mu muhat hetero
fixedeffect=FALSE
par_temp = c(mue = 0, muehat =0, phi = 0.482, phihat = 0.482, Khat = 0.257, what = 0.83,
             logsigmaa = -5.998989, logsigmae =-4.625102, logsigman = -5.395864)
KFfit=list(par=par_temp)
if (do_all) fcn_simul_VARMA11_LTY_hetero_phiphihat_MM(scaleVset=scaleVset, option=option, KFfit=KFfit, fixedeffect=fixedeffect,MM=MM) 
if (fixedeffect) temp_fixedeffect = "_fixedeffect"  else    temp_fixedeffect =  ""  
load( sprintf("simul_cov_phiphihat%s.Rdata",temp_fixedeffect ) )
cov_phi_est = matrix(apply(cov_phiphihat,1,mean) ,ncol=2)



## fixedeffect phi phihat heterogeneity
fixedeffect=TRUE
par_temp = c(mue = 0, muehat =0, phi = 0.48 , phihat = 0.47, Khat = 0.45, what = 0.8,
             logsigmaa = -6.378139, logsigmae = -4.614165, logsigman = -5.304773)
KFfit=list(par=par_temp) 
if (do_all) fcn_simul_VARMA11_LTY_hetero_phiphihat_MM(scaleVset=scaleVset, option=option, KFfit=KFfit, fixedeffect=fixedeffect,MM=MM) 

##  heterogeneity in data
fixedeffect=TRUE
if (do_all) fcn_VARMA11_LTY_hetero_MM(scaleVset=scaleVset, option=option , fixedeffect=fixedeffect)

## fixedeffect mu muhat heterogeneity
fixedeffect=TRUE
if (fixedeffect) temp_fixedeffect = "_fixedeffect"  else    temp_fixedeffect =  ""  
load( sprintf("simul_cov_phiphihat%s.Rdata",temp_fixedeffect ) )
if (fixedeffect) temp_fixedeffect = "_fixedeffect"  else    temp_fixedeffect =  ""  
load(   file = sprintf("Est_cov_mumuhatphiphihat%s.Rdata",temp_fixedeffect ) )
cov_phi_est =  Est_cov$cov_phiphihat  - matrix(apply(cov_phiphihat,1,mean) ,ncol=2) 
cov_mu_est =  Est_cov$cov_mumuhat  - matrix(apply(cov_mumuhat,1,mean) ,ncol=2) 
if (do_all) fcn_simul_VARMA11_LTY_hetero_mumuhat_MM(scaleVset=scaleVset, option=option, KFfit=KFfit, cov_phi_est=cov_phi_est,MM=MM) 


if (fixedeffect) temp_fixedeffect = "_fixedeffect"  else    temp_fixedeffect =  ""  
load( sprintf("simul_cov_mumuhat%s.Rdata",temp_fixedeffect ) )
cov_mu_est =  Est_cov$cov_mumuhat  - matrix(apply(cov_mumuhat,1,mean) ,ncol=2) 
print(cov_mu_est)
cov_phi_est 
matrix(apply(cov_phiphihat,1,mean) ,ncol=2) 

## Fat-tail distribution and heterogeneityin other parameters will amplify the heterogeneity of interest.















#############################################################
#############################################################
###################  AR(1) mu lerniang via MLE  ######################### 
#############################################################
#############################################################


source('fcn_KF_AR1_mu_learning_LTY_est.r')
source('fcn_MM_AR1_LTY_est.r')
source('fcn_pooled_TS_make_mu_learning.r')

if (do_all) {
  
  ## Transform firm-level data to VARMA pooled data 
  filename = fcn_filename_maker(scaleVset=scaleVset, option=option )
  load(file=filename)
  pdataset = list( y1=firm_TS_data$ye_firm_deflated  ,y2=firm_TS_data$yf_firm_deflated ,
                   begin_idx =firm_TS_data$firm_begin_deflated, end_idx = firm_TS_data$firm_end_deflated)
  temp_data = fcn_pooled_TS_make_mu_learning(pdataset  ) 
  
  KFfit = fcn_KF_AR1_mu_learning_LTY_est( yt=temp_data$yt, KF_idx=temp_data$KF_idx  )
  save(KFfit, file = "KFfit_AR1_mu_learning_MLE.Rdata")
  
}

source('fcn_AR1_mu_learning_LTY_random_data.r')
source('fcn_simul_AR1_mu_learning_LTY.r')
load("KFfit_AR1_mu_learning_MLE.Rdata")
MM = 50
simulnumber=1
set.seed( round(runif(1)*10^3)*simulnumber   )  


if (do_all) {
  fcn_simul_AR1_mu_learning_LTY(scaleVset=scaleVset, option=option, KFfit=KFfit, 
                                FE_rho_do=TRUE , est_do=TRUE, MM=MM, simulnumber=simulnumber)
}  




filename = sprintf("theta_sim_AR1_mu_learning_FE_rho%d.Rdata",  simulnumber) 
load(file = filename) 
hist(FE_rho_sim)


