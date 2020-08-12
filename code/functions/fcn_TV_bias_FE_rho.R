fcn_TV_bias_FE_rho<- function(scaleVset, option    ) {
  
  filename = fcn_filename_maker(scaleVset=scaleVset, option=option )
  load(file=filename)  
   
  TS_range = range(firm_TS_data$year_qtr_idx, na.rm=T )  
  TS_row = seq(TS_range[1], TS_range[2], by=0.25)  
  N_row_TS = length( TS_row )  
  N_row_TS_age = max(firm_TS_data$TS_index_firm_deflated,na.rm=T)  
  
  ye_stat_qtr = matrix(rep(NA, N_row_TS*6), ncol=6)
  yf_stat_qtr = ye_stat_qtr
  yef_stat_qtr = ye_stat_qtr  
  
  ye_stat_qtr_age = matrix(rep(NA, N_row_TS_age*6), ncol=6)
  yf_stat_qtr_age = ye_stat_qtr
  yef_stat_qtr_age = ye_stat_qtr
  
  fcn_stat_qtr<- function(zzz) {
    stat_qtr = c()
    stat_qtr$mean_qtr = mean(zzz,na.rm=T)
    stat_qtr$median_qtr = median(zzz,na.rm=T)
    stat_qtr$sd_qtr = sd(zzz,na.rm=T)
    stat_qtr$N_qtr = sum(!is.na(zzz),na.rm=T)  
    stat_qtr$lowball_qtr = (mean(zzz<0,na.rm=T) + (1-mean(zzz>0,na.rm=T)))/2
    stat_qtr$lowball_SD_qtr = sqrt(   stat_qtr$lowball_qtr*(1-  stat_qtr$lowball_qtr) )   

    return(as.numeric(stat_qtr ))
  }
  
  
  ### calendar quarterly  for  bias & lowball 
  for (ii in seq(1,N_row_TS )){
    temp_YQ_idx = firm_TS_data$year_qtr_idx == TS_row[ii]
    temp_YQ_idx[is.na(temp_YQ_idx)] = FALSE 
         
    zzz = firm_TS_data$ye_firm_deflated[ temp_YQ_idx]    
    ye_stat_qtr[ ii,] =  fcn_stat_qtr(zzz) 
      
    zzz = firm_TS_data$yf_firm_deflated[ temp_YQ_idx]  
    yf_stat_qtr[ ii,] = fcn_stat_qtr(zzz)
    
    zzz = firm_TS_data$ye_firm_deflated[ temp_YQ_idx]-firm_TS_data$yf_firm_deflated[ temp_YQ_idx]  
    yef_stat_qtr[ ii,] = fcn_stat_qtr(zzz)   
  
  } 
  
   mu_muhat_quaterly=list(ye_stat_qtr=ye_stat_qtr,yf_stat_qtr=yf_stat_qtr,yef_stat_qtr=yef_stat_qtr)
  save(mu_muhat_quaterly , file="mu_muhat_quaterly.Rdata")
  
  ### over-time quarterly  for  bias & lowball  
  

  for (ii in seq(1, N_row_TS_age )){
    temp_YQ_idx = firm_TS_data$TS_index_firm_deflated == ii
    temp_YQ_idx[is.na(temp_YQ_idx)] = FALSE 
    
    # bias & lowball 
    zzz = firm_TS_data$ye_firm_deflated[ temp_YQ_idx]    
    ye_stat_qtr_age[ ii,] =  fcn_stat_qtr(zzz) 
    
    zzz = firm_TS_data$yf_firm_deflated[ temp_YQ_idx]  
    yf_stat_qtr_age[ ii,] = fcn_stat_qtr(zzz)
    
    zzz = firm_TS_data$ye_firm_deflated[ temp_YQ_idx]-firm_TS_data$yf_firm_deflated[ temp_YQ_idx]  
    yef_stat_qtr_age[ ii,] = fcn_stat_qtr(zzz)
    
    
  } 
   
  mu_muhat_quaterly_age=list(ye_stat_qtr_age=ye_stat_qtr_age,yf_stat_qtr_age=yf_stat_qtr_age,yef_stat_qtr_age=yef_stat_qtr_age)
  save(mu_muhat_quaterly_age , file="mu_muhat_quaterly_age.Rdata")
  
  
  
  
   
  
  
  
  #########################
  #########################
  ######## rho(FE) ######
  #########################
  #########################
  #########################
  
  no_obs_deflated_firmqtr =rep(NA, length(firm_TS_data$ye_firm_deflated))
  for (jj in seq( 1 , length(firm_TS_data$firm_begin_deflated)))
  {  
    firm_idx_temp = firm_TS_data$firm_begin_deflated[jj]:firm_TS_data$firm_end_deflated[jj]
    no_obs_deflated_firmqtr[firm_idx_temp] = firm_TS_data$no_obs_deflated[jj]
  }   
  
  
   
  
  ### calendar quarterly reg_ for  rho(FE)
  fcn_FE_rho_MA_reg_calendar<- function(MA_lag ) {
    FE_rho_MA_qtr =  rep(NA, N_row_TS  )  
    phi_MA_qtr =  rep(NA, N_row_TS  )  
    phihat_MA_qtr =  rep(NA, N_row_TS  )  
    SE_FE_rho_MA_qtr =  rep(NA, N_row_TS  )  
    SE_phi_MA_qtr =  rep(NA, N_row_TS  )  
    SE_phihat_MA_qtr =  rep(NA, N_row_TS  )  
    
    for (ii in seq(1,N_row_TS-MA_lag+1 )){
      temp_YQ_idx =  firm_TS_data$year_qtr_idx  >= TS_row[ii]    &  firm_TS_data$year_qtr_idx  <=  TS_row[ii+MA_lag-1]
      temp_YQ_idx[is.na(temp_YQ_idx)] = FALSE 
            
      zzz = firm_TS_data$ye_firm_deflated 
      zzz[ !temp_YQ_idx] = NA
      pdataset = list( y=zzz, begin_idx =firm_TS_data$firm_begin_deflated, end_idx = firm_TS_data$firm_end_deflated)
      zzz = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
      m0 = summary(lm(zzz[2:length(zzz)] ~ zzz[1:(length(zzz)-1)] ))
      phi_MA_qtr[ii+MA_lag-1 ] =  m0$coef[2] 
      SE_phi_MA_qtr[ii+MA_lag-1 ]  = m0$coef[4] 
      
      zzz2 = firm_TS_data$yf_firm_deflated 
      zzz2[ !temp_YQ_idx] = NA
      pdataset = list( y=zzz2,begin_idx =firm_TS_data$firm_begin_deflated, end_idx = firm_TS_data$firm_end_deflated)
      zzz2 = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )      
      m0 = summary(lm(zzz2[2:length(zzz)] ~ zzz[1:(length(zzz)-1)] ))
      phihat_MA_qtr[ii+MA_lag-1 ] =  m0$coef[2] 
      SE_phihat_MA_qtr[ii+MA_lag-1 ]  = m0$coef[4]
      
      zzz3= zzz-zzz2 
      m0 = summary(lm(zzz3[2:length(zzz)] ~ zzz3[1:(length(zzz)-1)] ))
      FE_rho_MA_qtr[ii+MA_lag-1 ] =  m0$coef[2] 
      SE_FE_rho_MA_qtr[ii+MA_lag-1 ]  = m0$coef[4]
      
      print(ii) 
    } 
    return(list( FE_rho_MA_qtr=FE_rho_MA_qtr,SE_FE_rho_MA_qtr=SE_FE_rho_MA_qtr, 
                 phi_MA_qtr=phi_MA_qtr,SE_phi_MA_qtr=SE_phi_MA_qtr,
                 phihat_MA_qtr=phihat_MA_qtr,SE_phihat_MA_qtr=SE_phihat_MA_qtr))
  }
  
  
  rho_MA_reg_calendar_4 = fcn_FE_rho_MA_reg_calendar(MA_lag=4 )
  rho_MA_reg_calendar_8 = fcn_FE_rho_MA_reg_calendar(MA_lag=8 )
  rho_MA_reg_calendar_12 = fcn_FE_rho_MA_reg_calendar(MA_lag=12 )
  
   
  save(rho_MA_reg_calendar_4 , file="rho_MA_reg_calendar_4.Rdata")
  save(rho_MA_reg_calendar_8 , file="rho_MA_reg_calendar_8.Rdata")
  save(rho_MA_reg_calendar_12 , file="rho_MA_reg_calendar_12.Rdata")
  
  
  
  
  ### age learning over time quarterly reg_ for  rho(FE)
  fcn_FE_rho_MA_reg_age<- function(MA_lag ) {
    FE_rho_MA_qtr =  rep(NA, N_row_TS  )  
    phi_MA_qtr =  rep(NA, N_row_TS  )  
    phihat_MA_qtr =  rep(NA, N_row_TS  )  
    SE_FE_rho_MA_qtr =  rep(NA, N_row_TS  )  
    SE_phi_MA_qtr =  rep(NA, N_row_TS  )  
    SE_phihat_MA_qtr =  rep(NA, N_row_TS  )  
    
    
    for (ii in seq(1, N_row_TS_age-MA_lag+1 )){
      temp_YQ_idx = firm_TS_data$TS_index_firm_deflated>=ii  & 
        firm_TS_data$TS_index_firm_deflated< ii+MA_lag &
        no_obs_deflated_firmqtr >=  min_obs_firms    
      
      
      zzz = firm_TS_data$ye_firm_deflated 
      zzz[ !temp_YQ_idx] = NA
      pdataset = list( y=zzz, begin_idx =firm_TS_data$firm_begin_deflated, end_idx = firm_TS_data$firm_end_deflated)
      zzz = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
      m0 = summary(lm(zzz[2:length(zzz)] ~ zzz[1:(length(zzz)-1)] ))
      phi_MA_qtr[ii+MA_lag-1 ] =  m0$coef[2] 
      SE_phi_MA_qtr[ii+MA_lag-1 ]  = m0$coef[4] 
      
      zzz2 = firm_TS_data$yf_firm_deflated 
      zzz2[ !temp_YQ_idx] = NA
      pdataset = list( y=zzz2,begin_idx =firm_TS_data$firm_begin_deflated, end_idx = firm_TS_data$firm_end_deflated)
      zzz2 = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )      
      m0 = summary(lm(zzz2[2:length(zzz)] ~ zzz[1:(length(zzz)-1)] ))
      phihat_MA_qtr[ii+MA_lag-1 ] =  m0$coef[2] 
      SE_phihat_MA_qtr[ii+MA_lag-1 ]  = m0$coef[4]
      
      zzz= zzz-zzz2 
      m0 = summary(lm(zzz[2:length(zzz)] ~ zzz[1:(length(zzz)-1)] ))
      FE_rho_MA_qtr[ii+MA_lag-1 ] =  m0$coef[2] 
      SE_FE_rho_MA_qtr[ii+MA_lag-1 ]  = m0$coef[4]
      
      print(ii) 
    } 
    return(list( FE_rho_MA_qtr=FE_rho_MA_qtr,SE_FE_rho_MA_qtr=SE_FE_rho_MA_qtr, 
                 phi_MA_qtr=phi_MA_qtr,SE_phi_MA_qtr=SE_phi_MA_qtr,
                 phihat_MA_qtr=phihat_MA_qtr,SE_phihat_MA_qtr=SE_phihat_MA_qtr))
  }
  
  
  rho_MA_reg_age_4 = fcn_FE_rho_MA_reg_calendar(MA_lag=4 )
  rho_MA_reg_age_8 = fcn_FE_rho_MA_reg_calendar(MA_lag=8 )
  rho_MA_reg_age_12 = fcn_FE_rho_MA_reg_calendar(MA_lag=12 )
  
  
  save(rho_MA_reg_age_4 , file="rho_MA_reg_age_4.Rdata")
  save(rho_MA_reg_age_8 , file="rho_MA_reg_age_8.Rdata")
  save(rho_MA_reg_age_12 , file="rho_MA_reg_age_12.Rdata")
  
}

###########################################
###########################################
###########################################
###########################################
###########################################
###########################################
###########################################
############### still working ###################
###########################################
###########################################
###########################################
###########################################
###########################################
###########################################
###########################################
###########################################
###########################################



### year,age at the same time

year_effect = as.factor(floor(firm_TS_data$year_qtr_idx-0.2))
age_effect = as.factor(1+floor(firm_TS_data$TS_index_firm_deflated/4-0.2))

m1 = summary(lm(c(firm_TS_data$ye_firm_deflated -firm_TS_data$yf_firm_deflated) ~-1+  year_effect   + age_effect ))
ts.plot(cbind(m1$coef[1:29,1]-2.58*m1$coef[1:29,2],m1$coef[1:29,1]+2.58*m1$coef[1:29,2]))
ts.plot(cbind(m1$coef[30:56,1]-2.58*m1$coef[30:56,2],m1$coef[30:56,1]+2.58*m1$coef[30:56,2]))


yef_temp = c(firm_TS_data$ye_firm_deflated -firm_TS_data$yf_firm_deflated)

m1 = summary(lm(  yef_temp[2:length( yef_temp)]~-1+  year_effect[2:length( yef_temp)]   + year_effect[2:length( yef_temp)]:firm_TS_data$ye_firm_deflated[1:(length( yef_temp)-1)] ))
m1

### time-varying rho(FE)  as moving average of 20 lags..no fixed effect
### 1) with raw FE, 2) with residuals left from year 
### 1) calendar quarter  2) learning over-time
### 1) all firms,   2) firms with T>=60

  
  ### calendar quarterly  for  rho(FE)
  fcn_FE_rho_MA_calendar<- function(MA_lag ) {
    FE_rho_MA_qtr =  rep(NA, N_row_TS  )  
    phi_MA_qtr =  rep(NA, N_row_TS  )  
    SE_FE_rho_MA_qtr =  rep(NA, N_row_TS  )  
    SE_phi_MA_qtr =  rep(NA, N_row_TS  )  
    
    for (ii in seq(1,N_row_TS-MA_lag+1 )){
      temp_YQ_idx =  firm_TS_data$year_qtr_idx  >= TS_row[ii]    &  firm_TS_data$year_qtr_idx  <=  TS_row[ii+MA_lag-1]
      temp_YQ_idx[is.na(temp_YQ_idx)] = FALSE 
      
      zzz = firm_TS_data$ye_firm_deflated - firm_TS_data$yf_firm_deflated 
      zzz[ !temp_YQ_idx] = NA
      pdataset = list( y=zzz,begin_idx =firm_TS_data$firm_begin_deflated, end_idx = firm_TS_data$firm_end_deflated)
      zzz = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
      SS = sum(!is.na(zzz)) 
      m0 = arima(zzz,c(1,0,0),  method = c("ML"),optim.control = list(maxit = 300000 ))                  
      FE_rho_MA_qtr[ii+MA_lag-1 ] = ((m0$coef[1]*(SS-1)+1)/(SS-4)) 
      SE_FE_rho_MA_qtr[ii+MA_lag-1 ]  = sqrt(m0$var.coef[1])
      
      zzz = firm_TS_data$ye_firm_deflated 
      zzz[ !temp_YQ_idx] = NA
      pdataset = list( y=zzz, begin_idx =firm_TS_data$firm_begin_deflated, end_idx = firm_TS_data$firm_end_deflated)
      zzz = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
      SS = sum(!is.na(zzz)) 
      m0 = arima(zzz,c(1,0,0),  method = c("ML"),optim.control = list(maxit = 300000 ))                  
      phi_MA_qtr[ii+MA_lag-1 ] = ((m0$coef[1]*(SS-1)+1)/(SS-4)) 
      SE_phi_MA_qtr[ii+MA_lag-1 ]  = sqrt(m0$var.coef[1])
      
      print(ii) 
    } 
    return(list( FE_rho_MA_qtr=FE_rho_MA_qtr,SE_FE_rho_MA_qtr=SE_FE_rho_MA_qtr, 
                 phi_MA_qtr=phi_MA_qtr,SE_phi_MA_qtr=SE_phi_MA_qtr))
  }
  
  
  rho_MA_calendar_4 = fcn_FE_rho_MA_calendar(MA_lag=4 )
  plot(TS_row, rho_MA_calendar_4$FE_rho_MA_qtr, type='b' )
  plot(TS_row, rho_MA_calendar_4$SE_FE_rho_MA_qtr, type='b' )
  plot(TS_row, rho_MA_calendar_4$phi_MA_qtr, type='b' )
  plot(TS_row, rho_MA_calendar_4$SE_phi_MA_qtr, type='b' )
  
  
  FE_rho_MA_calendar_20 = fcn_FE_rho_MA_calendar(MA_lag=20 )  
  plot(TS_row, FE_rho_MA_calendar_20, type='b' )
  
  ###   learning over quarter
  fcn_FE_rho_MA<- function(MA_lag ,  min_obs_firms) {
    FE_rho_MA_age =  rep(NA, N_row_TS_age  ) 
    SE_FE_rho_MA_age =  rep(NA, N_row_TS_age  ) 
    phi_MA_age =  rep(NA, N_row_TS_age  ) 
    SE_phi_MA_age =  rep(NA, N_row_TS_age  ) 
    
    
    for (ii in seq(1, N_row_TS_age-MA_lag+1 )){
      temp_YQ_idx = firm_TS_data$TS_index_firm_deflated>=ii  & 
                    firm_TS_data$TS_index_firm_deflated< ii+MA_lag &
                    no_obs_deflated_firmqtr >=  min_obs_firms      
      
      temp_YQ_idx[is.na(temp_YQ_idx)] = FALSE  
      
      zzz = firm_TS_data$ye_firm_deflated - firm_TS_data$yf_firm_deflated 
      zzz[ !temp_YQ_idx] = NA
      pdataset = list( y=zzz, 
                       begin_idx =firm_TS_data$firm_begin_deflated, end_idx = firm_TS_data$firm_end_deflated)
      zzz = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
      SS = sum(!is.na(zzz))  
      m0 = arima(zzz,c(1,0,0),  method = c("ML"),optim.control = list(maxit = 300000 ))                  
      FE_rho_MA_age[ii+MA_lag-1 ] = ((m0$coef[1]*(SS-1)+1)/(SS-4)) 
      SE_FE_rho_MA_age[ii+MA_lag-1 ]  = sqrt(m0$var.coef[1])
      
      zzz = firm_TS_data$ye_firm_deflated   
      zzz[ !temp_YQ_idx] = NA
      pdataset = list( y=zzz, 
                       begin_idx =firm_TS_data$firm_begin_deflated, end_idx = firm_TS_data$firm_end_deflated)
      zzz = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
      SS = sum(!is.na(zzz))  
      m0 = arima(zzz,c(1,0,0),  method = c("ML"),optim.control = list(maxit = 300000 ))                  
      phi_MA_age[ii+MA_lag-1 ] = ((m0$coef[1]*(SS-1)+1)/(SS-4)) 
      SE_phi_MA_age[ii+MA_lag-1 ]  = sqrt(m0$var.coef[1])
      
      print(ii) 
    } 
    return(list( FE_rho_MA_age=FE_rho_MA_age,SE_FE_rho_MA_age=SE_FE_rho_MA_age, 
                 phi_MA_age=phi_MA_age,SE_phi_MA_age=SE_phi_MA_age))
  }
    
  rho_MA_4 = fcn_FE_rho_MA(MA_lag=4 ,  min_obs_firms= 0)
  plot(1:N_row_TS_age, rho_MA_4$FE_rho_MA_age, type='b' )
  plot(1:N_row_TS_age, rho_MA_4$SE_FE_rho_MA_age, type='b' )
  plot(1:N_row_TS_age, rho_MA_4$phi_MA_age, type='b' )
  plot(1:N_row_TS_age, rho_MA_4$SE_phi_MA_age, type='b' )
  
  
  rho_MA_4_min60 = fcn_FE_rho_MA(MA_lag=4 ,  min_obs_firms= 60)
  plot(1:60, rho_MA_4_min60$FE_rho_MA_age[1:60], type='b' )
  plot(1:60, rho_MA_4_min60$SE_FE_rho_MA_age[1:60], type='b' )
  plot(1:60, rho_MA_4_min60$phi_MA_age[1:60], type='b' )
  plot(1:60, rho_MA_4_min60$SE_phi_MA_age[1:60], type='b' )
  
  
  
  
  #########################
  #########################
  ######## VARMA with 4-quarters rolling windows  ---> estimation method is not stable #####
  ######### check the time-variation in phi-phihat ################
  #########################
  #########################
  
  ###### Calendar #########
  MA_lag=4
  VARMA_theta_TV_calendar =  matrix(rep(NA, N_row_TS_age*9  ) , ncol=9)
   
  for (ii in seq(1,N_row_TS-MA_lag+1 )){
    temp_YQ_idx =  firm_TS_data$year_qtr_idx  >= TS_row[ii]    &  firm_TS_data$year_qtr_idx  <=  TS_row[ii+MA_lag-1]    
    temp_YQ_idx[is.na(temp_YQ_idx)] = FALSE  
    zzz = firm_TS_data$ye_firm_deflated   
    zzz[ !temp_YQ_idx] = NA
    pdataset = list( y=zzz, 
                     begin_idx =firm_TS_data$firm_begin_deflated, end_idx = firm_TS_data$firm_end_deflated)
    yy11 = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
 
    
    zzz = firm_TS_data$yf_firm_deflated 
    zzz[ !temp_YQ_idx] = NA
    pdataset = list( y=zzz, 
                     begin_idx =firm_TS_data$firm_begin_deflated, end_idx = firm_TS_data$firm_end_deflated)
    yy22 = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
    
    yt = rbind(yy11,yy22)
    
    fixedeffect=FALSE 
    KFfit = fcn_KF_VARMA11_LTY_est( yt=yt , mueest=TRUE)
                     
    VARMA_theta_TV_calendar[ii+MA_lag-1, ] = KFfit$par 
    print(ii) 
    save(VARMA_theta_TV_calendar, file = sprintf("VARMA_theta_TV_calendar_MAlag%d.Rdata",MA_lag) )
  } 
  
  
  ###### learn over time #########
  MA_lag=4 
  min_obs_firms = 60
  VARMA_theta_TV_age = matrix(rep(NA, N_row_TS_age*9  ) , ncol=9)
  
  for (ii in seq(1, N_row_TS_age-MA_lag+1 )){
    temp_YQ_idx = firm_TS_data$TS_index_firm_deflated>=ii  & 
      firm_TS_data$TS_index_firm_deflated< ii+MA_lag &
      no_obs_deflated_firmqtr >=  min_obs_firms  
    temp_YQ_idx[is.na(temp_YQ_idx)] = FALSE  
    zzz = firm_TS_data$ye_firm_deflated   
    zzz[ !temp_YQ_idx] = NA
    pdataset = list( y=zzz, 
                     begin_idx =firm_TS_data$firm_begin_deflated, end_idx = firm_TS_data$firm_end_deflated)
    yy11 = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
    
    
    zzz = firm_TS_data$yf_firm_deflated 
    zzz[ !temp_YQ_idx] = NA
    pdataset = list( y=zzz, 
                     begin_idx =firm_TS_data$firm_begin_deflated, end_idx = firm_TS_data$firm_end_deflated)
    yy22 = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
    
    yt = rbind(yy11,yy22)
    
    fixedeffect=FALSE 
    KFfit = fcn_KF_VARMA11_LTY_est( yt=yt , mueest=TRUE)
    
    VARMA_theta_TV_age[ii+MA_lag-1, ] = KFfit$par 
    print(ii) 
    save(VARMA_theta_TV_age, file = sprintf("VARMA_theta_TV_age_MAlag%d_min%d.Rdata",MA_lag, min_obs_firms) )
  } 
  
  
   
  
  
  
  FE_rho_MA_20 = fcn_FE_rho_MA(MA_lag=20,  min_obs_firms= 0)
  plot(1:N_row_TS_age, FE_rho_MA_20 , type='b' ) 
  
  FE_rho_MA_20_min40 = fcn_FE_rho_MA(MA_lag=20 ,  min_obs_firms=40)
  FE_rho_MA_20_min60 = fcn_FE_rho_MA(MA_lag=20 ,  min_obs_firms=60)
  FE_rho_MA_20_min80 = fcn_FE_rho_MA(MA_lag=20 ,  min_obs_firms=80)
  
  plot(1:40, FE_rho_MA_20_min40[1:40] , type='b' ) 
  plot(1:60, FE_rho_MA_20_min60[1:60] , type='b' ) 
  plot(1:80, FE_rho_MA_20_min80[1:80] , type='b' ) 
   
  


   
  
  