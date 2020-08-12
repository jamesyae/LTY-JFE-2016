fcn_arima_LTY_random_data_COV<- function(scaleVset, option , fixedeffect, theta, equicorr) {
  
  filename = fcn_filename_maker(scaleVset=scaleVset, option=option )
  load(file=filename)
   
  ytsim1set = rep(NA, length(firm_TS_data$ye_firm_deflated) ) 
  
  ett =   ytsim1set
  TS_range = range(firm_TS_data$year_qtr_idx, na.rm=T )  
  TS_row = seq(TS_range[1], TS_range[2], by=0.25)  
  N_row_TS = length( TS_row )

  
  for (ii in seq(1,N_row_TS )){
    temp_YQ_idx = firm_TS_data$year_qtr_idx == TS_row[ii]
    temp_YQ_idx[is.na(temp_YQ_idx)] = FALSE 
    ett[ temp_YQ_idx] =  rnorm(1)*equicorr  + rnorm( sum(temp_YQ_idx))*sqrt(1-equicorr)  
  }
   
 
  
  
  if (theta$phi==0) {
    ytsim1long = arima.sim(list(order = c(0,0,1),   ma=theta$ma),innov = ett, n = length(ytsim1set))
  }
  else {
    ytsim1long = arima.sim(list(order = c(1,0,1), ar = theta$phi, ma=theta$ma),innov = ett, n = length(ytsim1set))  
  }
    
    
  for (jj in seq( 1 , length(firm_TS_data$firm_begin_deflated)))
  {  
      if (!is.na(firm_TS_data$firm_begin_deflated[jj]))  {
      firm_idx_temp = firm_TS_data$firm_begin_deflated[jj]:firm_TS_data$firm_end_deflated[jj]        
       
      ytsim1  = ytsim1long[firm_idx_temp]       
      naidx = is.na(firm_TS_data$ye_firm_deflated[firm_idx_temp])
      ytsim1[naidx] = NA 
      
      if (fixedeffect) {
        ytsim1 = (ytsim1-mean(ytsim1,na.rm=T) )
      }
      ytsim1set[firm_idx_temp] = ytsim1  
      }
 
   }
    
    
  pdataset = list( y=ytsim1set, 
                   begin_idx =firm_TS_data$firm_begin_deflated, end_idx = firm_TS_data$firm_end_deflated)
  ytsim1set = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
    
 return(ytsim1set)  
}

#aa=rnorm(10000);
#m1=arima(arima.sim(list(order = c(1,0,1), ar = 0.5, ma=-0.02),  innov = aa, n = 10000) ,order=c(1,0,1),
#         method = c("ML"),optim.control = list(maxit = 300000 ))  ;
#plot(aa,m1$residuals)


