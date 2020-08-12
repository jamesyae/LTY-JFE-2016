fcn_VARMA11_LTY_random_data_MM<- function(scaleVset, option , theta) {
  
  filename = fcn_filename_maker(scaleVset=scaleVset, option=option )
  load(file=filename)
   
  ytsim1set = rep(NA, length(firm_TS_data$ye_firm_deflated) )
  ytsim2set = ytsim1set 
  
  
  for (jj in seq( 1 , length(firm_TS_data$firm_begin_deflated)))
  {  
       
      firm_idx_temp = firm_TS_data$firm_begin_deflated[jj]:firm_TS_data$firm_end_deflated[jj]  
      
      Tsim = length(firm_idx_temp) + 50
      
      
      if (runif(1)>0.5) {
        #set.seed( round(runif(1)*10^6)   )   
        et_sim = rnorm(Tsim)*exp(theta$logsigmae)
        
        #set.seed( round(runif(1)*10^6)   )   
        at_sim = rnorm(Tsim)*exp(theta$logsigmaa )
        
        #set.seed( round(runif(1)*10^6)   )   
        nt_sim = rnorm(Tsim)*exp(theta$logsigman )
      }
      
      else {
        #set.seed( round(runif(1)*10^6)   )   
        at_sim = rnorm(Tsim)*exp(theta$logsigmaa )
        
        #set.seed( round(runif(1)*10^6)   )   
        et_sim = rnorm(Tsim)*exp(theta$logsigmae)
        
        #set.seed( round(runif(1)*10^6)   )   
        nt_sim = rnorm(Tsim)*exp(theta$logsigman )
      }
      
 
      
  
      
      ytsim1= rep(NA, Tsim )
      ytsim2= rep(NA, Tsim )
      
      xtsim0 = 0
      ytsim1[1] = theta$mue  + xtsim0+at_sim[1]
      ytsim2[1] =  theta$muehat  
      for (ii  in seq(2, Tsim ))
      {    
        xtsim1     = theta$phi*xtsim0 + et_sim[ii] 
        ytsim1[ii] = theta$mue  + xtsim1 + at_sim[ii] 
        ytsim2[ii] = theta$muehat  + theta$phihat*(theta$Khat*ytsim1[ii-1] +
                      (1-theta$Khat )*ytsim2[ii-1] ) + theta$what*(et_sim[ii] + nt_sim[ii])
        xtsim0=xtsim1
      }
      
      ytsim1  = ytsim1[51:Tsim]
      ytsim2  = ytsim2[51:Tsim]
      
      naidx = is.na(firm_TS_data$ye_firm_deflated[firm_idx_temp])
      ytsim1[naidx] = NA
      
      naidx = is.na(firm_TS_data$yf_firm_deflated[firm_idx_temp])
      ytsim2[naidx] = NA
      
      
      ytsim1 = (ytsim1-mean(ytsim1,na.rm=T) )/sd(ytsim1,na.rm=T)
      ytsim2 = (ytsim2-mean(ytsim2,na.rm=T) )/sd(ytsim1,na.rm=T) 
      
      ytsim1set[firm_idx_temp] = ytsim1 
      ytsim2set[firm_idx_temp] = ytsim2 
 
   }
    
  
  
  pdataset = list( y=ytsim1set, 
                   begin_idx =firm_TS_data$firm_begin_deflated, end_idx = firm_TS_data$firm_end_deflated)
  ytsim1set = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
  pdataset = list( y=ytsim2set, 
                   begin_idx =firm_TS_data$firm_begin_deflated, end_idx = firm_TS_data$firm_end_deflated)
  ytsim2set = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
  
  yt = matrix(rbind(ytsim1set,ytsim2set),nrow=2) 
  
 return(yt)
  
}