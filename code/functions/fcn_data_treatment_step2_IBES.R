fcn_data_treatment_step2_IBES <- function(dataset, scaling_variable,filename, option) {  
  
N_row = dim(dataset)[1] 
attach(dataset)  
  
################ Lag-variable generator #################
fcn_Lagged_variables <- function(x, firm_begin, firm_end, TS_index, primary_idx, no_maxlag)
{ 
  xL = matrix(rep(NA,no_maxlag*length(x)),ncol=no_maxlag)
  for (kk in seq( 1,no_maxlag))
  {
    for (jj in seq( 1, length(firm_begin)))
    {   
      firm_idx_temp = firm_begin[jj]:firm_end[jj]  
      primary_idx_temp = primary_idx[firm_idx_temp]
      Lagidx = match(TS_index[firm_idx_temp][primary_idx_temp]-kk,  TS_index[firm_idx_temp][primary_idx_temp])  
      xL[firm_idx_temp[primary_idx_temp],kk] = x[firm_idx_temp][primary_idx_temp][Lagidx] 
    }
  }
  return(xL)  
} 

### year quarter - restriction
tempidx = !(fpe_year< option$yearmin)  &  !(fpe_year> option$yearmax)
data_treat_history = c( sum( tempidx), length(table( ticker_fixed[tempidx  ] )))

### eps forecats existence
tempidx = tempidx & !is.na(FE$actual_eps + FE$median_estimate  )
data_treat_history = rbind(data_treat_history,c( sum( tempidx), length(table( ticker_fixed[tempidx  ] ))))

scaling_variable[scaling_variable == Inf ] = NA
scaling_variable[scaling_variable == 0 ] = NA
scaling_variable[fpe_year< option$yearmin ] = NA
scaling_variable[fpe_year> option$yearmax ] = NA 

################ Compute dEarning, Forecasts #################
# Compute 1-quarter lagged Earning
lag_scaling_variable = fcn_Lagged_variables( scaling_variable, firm_begin, firm_end, TS_index, primary_idx, no_maxlag=1)
actual_eps_scaled = actual_eps/lag_scaling_variable
avg_estimate_scaled = avg_estimate/lag_scaling_variable
median_estimate_scaled = median_estimate/lag_scaling_variable


actual_epsL_scaled = fcn_Lagged_variables(actual_eps_scaled, firm_begin, firm_end, TS_index, primary_idx, no_maxlag=6)
ye =  actual_eps_scaled - actual_epsL_scaled[,4] # ye: seasonal earning difference

if (option$aggregateforecast=="mean") {
  yf  =  avg_estimate_scaled - actual_epsL_scaled[,4] # yf: forecast for seasonal earning difference
  yef = actual_eps_scaled - avg_estimate_scaled 
}

if (option$aggregateforecast=="median") {
  yf  =  median_estimate_scaled - actual_epsL_scaled[,4] # yf: forecast for seasonal earning difference
  yef = actual_eps_scaled - median_estimate_scaled
}
 
### ye yf define
tempidx = tempidx & !is.na(ye+yf+yef )
data_treat_history = rbind(data_treat_history,c( sum( tempidx), length(table( ticker_fixed[tempidx  ] ))))



### remove  potentially problematic data 
actual_epsLtemp = fcn_Lagged_variables(actual_eps, firm_begin, firm_end, TS_index, primary_idx, no_maxlag=6)
if (option$aggregateforecast=="mean") {
  scaling_variable[ abs(actual_eps-avg_estimate - actual_epsLtemp[,1]) > option$FEmPESmax ] = NA
}
if (option$aggregateforecast=="median") {
  scaling_variable[ abs(actual_eps-median_estimate - actual_epsLtemp[,1])  > option$FEmPESmax] = NA
}
scaling_variable[scaling_variable > option$scalemax ] = NA
scaling_variable[scaling_variable < option$scalemin ] = NA


# Compute 1-quarter lagged Earning - repeat
lag_scaling_variable = fcn_Lagged_variables( scaling_variable, firm_begin, firm_end, TS_index, primary_idx, no_maxlag=1)
actual_eps_scaled = actual_eps/lag_scaling_variable
avg_estimate_scaled = avg_estimate/lag_scaling_variable
median_estimate_scaled = median_estimate/lag_scaling_variable
DISP = FE$sd_estimate/lag_scaling_variable
#DISP = FE$sd_estimate/fcn_Lagged_variables(FE$price, firm_begin, firm_end, TS_index, primary_idx, no_maxlag=1)

actual_epsL_scaled = fcn_Lagged_variables(actual_eps_scaled, firm_begin, firm_end, TS_index, primary_idx, no_maxlag=6)
ye =  actual_eps_scaled - actual_epsL_scaled[,4] # ye: seasonal earning difference

if (option$aggregateforecast=="mean") {
  yf  =  avg_estimate_scaled - actual_epsL_scaled[,4] # yf: forecast for seasonal earning difference
  yef = actual_eps_scaled - avg_estimate_scaled 
}

if (option$aggregateforecast=="median") {
  yf  =  median_estimate_scaled - actual_epsL_scaled[,4] # yf: forecast for seasonal earning difference
  yef = actual_eps_scaled - median_estimate_scaled
}

### remove  potentially problematic data 
tempidx = tempidx & !is.na(ye+yf+yef )
data_treat_history = rbind(data_treat_history, c( sum( tempidx), length(table( ticker_fixed[tempidx  ] ))))

# Compute 1-quarter lagged dEarning  
yeL = fcn_Lagged_variables(ye, firm_begin, firm_end, TS_index, primary_idx, no_maxlag=6)

# Compute 1-quarter lagged forecast errors 
yefL = fcn_Lagged_variables(yef, firm_begin, firm_end, TS_index, primary_idx, no_maxlag=6)

# Compute 1-quarter lagged forecast  
yfL = fcn_Lagged_variables(yf, firm_begin, firm_end, TS_index, primary_idx, no_maxlag=6)

thisfilename = sprintf("new_variables_IBES_%s.Rdata", filename)
save(ye,yf,yef,yeL,yfL,yefL,DISP,data_treat_history,file = thisfilename)



detach(dataset)  
}




