fcn_autocorr_compare <- function(scaleVset, option) {  
  
  filename = fcn_filename_maker(scaleVset=scaleVset, option=option )
  load(file=filename) 
  est = firm_TS_data
   
  
pdataset = list( y=firm_TS_data$ye_firm_deflated - firm_TS_data$yf_firm_deflated  , 
                 begin_idx =firm_TS_data$firm_begin_deflated, end_idx = firm_TS_data$firm_end_deflated)
zzz = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )

arima(zzz , c(1,0,0), 
      method = c("ML"),optim.control = list(maxit = 300000 ))$coef[1]

# obs with missing lagged obs have higher standard devation due to truncation 
#  with the presence of heterogeneity in sd across firms and time. 
# ARIMA do not drop such obs, so autocorr est is different and possibly big
# If we ignore such obs, autocorr is small just like pearson corr
#  arima(zzz[2:length(zzz)][!is.na(zzz[2:length(zzz)]+zzz[1:(length(zzz)-1)] )] , c(1,0,0), 
#      method = c("ML"),optim.control = list(maxit = 300000 ))$coef[1]  
# sum( !is.na( zzz[2:length(zzz)] ))
# sum( !is.na( (zzz[2:length(zzz)])[!is.na(zzz[2:length(zzz)]+zzz[1:(length(zzz)-1)] )] ))
# difference in obs used is 11% . this 11% of data have high sd and not randomly selected 
  
# due to outliers, pearson and spearman corr coef are different. Also OLS is different.

summary(lm(zzz[2:length(zzz)]~zzz[1:(length(zzz)-1)]))$coef[2]  
cor(zzz[2:length(zzz)], zzz[1:(length(zzz)-1)], use="complete" ) 
cor(zzz[2:length(zzz)], zzz[1:(length(zzz)-1)], use="complete" ,method="spearman" ) 
  

  acf(zzz , lag.max=1, na.action=na.pass, plot=FALSE)$acf[2]
# acf use full sample mean sd coef in correlation computation instead of mean, sd 
  # of each lags. So obs with missing lagged obs is included
# mean are not very different but SD is bigger when such obs are included
  # so the autocorr is smaller.
#sd(zzz[2:length(zzz)][!is.na(zzz[2:length(zzz)]+zzz[1:(length(zzz)-1)] )],na.rm=T)
#sd(zzz[2:length(zzz)],na.rm=T)

  yyy = arima.sim(list(order = c(1,0,0), ar = 0.5), n = 100000); 
  arima(yyy, c(1,0,0))  
  yyy[abs(yyy)>quantile(yyy,0.98)]=NA
  arima(yyy, c(1,0,0))
  summary(lm( yyy[2: length(yyy)]  ~ yyy[1: (length(yyy)-1)] ))
  cor(yyy[2:length(yyy)], yyy[1:(length(yyy)-1)], use="complete" ) 
  cor(yyy[2:length(yyy)], yyy[1:(length(yyy)-1)], use="complete" ,method="spearman" ) 
  
  
}
