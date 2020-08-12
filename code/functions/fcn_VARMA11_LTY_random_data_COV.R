fcn_VARMA11_LTY_random_data_COV<- function(scaleVset, option , KFfit, fixedeffect, equicorr=equicorr) {
  
  filename = fcn_filename_maker(scaleVset=scaleVset, option=option )
  load(file=filename)
   
  ytsim1set = rep(NA, length(firm_TS_data$ye_firm_deflated) )
  ytsim2set = ytsim1set 
  
  ett =   ytsim1set
  TS_range = range(firm_TS_data$year_qtr_idx, na.rm=T )  
  TS_row = seq(TS_range[1], TS_range[2], by=0.25)  
  N_row_TS = length( TS_row )
  
  
  for (ii in seq(1,N_row_TS )){
    temp_YQ_idx = firm_TS_data$year_qtr_idx == TS_row[ii]
    temp_YQ_idx[is.na(temp_YQ_idx)] = FALSE 
    ett[ temp_YQ_idx] =  rnorm(1)*equicorr  + rnorm( sum(temp_YQ_idx))*sqrt(1-equicorr)  
  }
  
  
  
  for (jj in seq( 1 , length(firm_TS_data$firm_begin_deflated)))
  {  
       
      firm_idx_temp = firm_TS_data$firm_begin_deflated[jj]:firm_TS_data$firm_end_deflated[jj]  
      
      Tsim = length(firm_idx_temp) + 50
      nonaidx = !is.na(firm_TS_data$ye_firm_deflated[firm_idx_temp])
      ett2 = rnorm(Tsim-50 ) 
      ett2[nonaidx] =  ett[ firm_idx_temp[nonaidx]  ]
      
      if (runif(1)>0.5) {
      #set.seed( round(runif(1)*10^6)   )   
      et_sim = c(rnorm(50),ett2 )*exp(KFfit$par["logsigmae"])
      
      #set.seed( round(runif(1)*10^6)   )   
      at_sim = rnorm(Tsim)*exp(KFfit$par["logsigmaa"])
      
      #set.seed( round(runif(1)*10^6)   )   
      nt_sim = rnorm(Tsim)*exp(KFfit$par["logsigman"])
      } else {
        
        #set.seed( round(runif(1)*10^6)   )   
        nt_sim = rnorm(Tsim)*exp(KFfit$par["logsigman"]) 
        
        #set.seed( round(runif(1)*10^6)   )   
        at_sim = rnorm(Tsim)*exp(KFfit$par["logsigmaa"])
        
        et_sim = c(rnorm(50),ett2 )*exp(KFfit$par["logsigmae"])  
        
      }
  
      
      ytsim1= rep(NA, Tsim )
      ytsim2= rep(NA, Tsim )
      
      xtsim0 = 0
      ytsim1[1] = KFfit$par["mue"] + xtsim0+at_sim[1]
      ytsim2[1] =  KFfit$par["muehat"] 
      for (ii  in seq(2, Tsim ))
      {    
        xtsim1     = KFfit$par["phi"]*xtsim0 + et_sim[ii] 
        ytsim1[ii] = KFfit$par["mue"] + xtsim1 + at_sim[ii] 
        ytsim2[ii] = KFfit$par["muehat"] + KFfit$par["phihat"]*(KFfit$par["Khat"]*ytsim1[ii-1] +
                      (1-KFfit$par["Khat"])*ytsim2[ii-1] ) + KFfit$par["what"]*(et_sim[ii] + nt_sim[ii])
        xtsim0=xtsim1
      }
      
      ytsim1  = ytsim1[51:Tsim]
      ytsim2  = ytsim2[51:Tsim]
      
      naidx = is.na(firm_TS_data$ye_firm_deflated[firm_idx_temp])
      ytsim1[naidx] = NA
      
      naidx = is.na(firm_TS_data$yf_firm_deflated[firm_idx_temp])
      ytsim2[naidx] = NA
      
      if (fixedeffect) {
        ytsim1 = (ytsim1-mean(ytsim1,na.rm=T) ) 
        ytsim2 = (ytsim2-mean(ytsim2,na.rm=T) ) 
      }
      
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