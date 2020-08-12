fcn_arima_LTY_random_data<- function(scaleVset, option , fixedeffect, theta) {
  
  filename = fcn_filename_maker(scaleVset=scaleVset, option=option )
  load(file=filename)
   
  ytsim1set = rep(NA, length(firm_TS_data$ye_firm_deflated) ) 
  
  if (theta$phi==0) {
    ytsim1long = arima.sim(list(order = c(0,0,1),   ma=theta$ma), n = length(ytsim1set))
  }
  else {
    ytsim1long = arima.sim(list(order = c(1,0,1), ar = theta$phi, ma=theta$ma), n = length(ytsim1set))  
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