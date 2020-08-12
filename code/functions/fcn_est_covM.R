fcn_est_covM <- function(modelorder , scaleVset ,  option, fixedeffect  , theta_hat ) {
  
    
  filename = fcn_filename_maker(scaleVset=scaleVset, option=option )
  load(file=filename)
  
  
  TS_range = range(firm_TS_data$year_qtr_idx, na.rm=T )  
  TS_row = seq(TS_range[1], TS_range[2], by=0.25)
  
  N_row_TS = length( TS_row)
  N_col_CS = length(firm_TS_data$no_obs_deflated)
  
  Panel_data_set = matrix(rep(NA, N_row_TS*N_col_CS), ncol =N_col_CS )
  res_Panel_data_set = matrix(rep(NA, N_row_TS*N_col_CS), ncol =N_col_CS )
  
  for (ii in seq(1,N_row_TS )){
    temp_YQ_idx = firm_TS_data$year_qtr_idx == TS_row[ii]
    temp_YQ_idx[is.na(temp_YQ_idx)] = FALSE
    temp_firm_idx =   firm_TS_data$firm_deflated_nn_idx[temp_YQ_idx]
    Panel_data_set[ii,temp_firm_idx] = firm_TS_data$ye_firm_deflated[temp_YQ_idx]  
  }
  
   
  for (ii in seq(1, N_col_CS )){ 
    res_Panel_data_set[ ,ii]  
    m1=arima( Panel_data_set[ ,ii], order=c(1,0,1),  
           fixed=list(ar=theta_hat$phi, ma=theta_hat$ma, intercept=NA),
          method = c("ML"),optim.control = list(maxit = 300000 ))       
    res_Panel_data_set[ ,ii]  = m1$residuals
  }
  
  
  kk=1
  temp_data_corr =  rep(NA, N_col_CS^2 )
  for (ii in seq(1, N_col_CS-1 )){ 
    for (jj in seq(ii+1, N_col_CS )){     
      temp_data = cbind(res_Panel_data_set[ ,ii],res_Panel_data_set[ ,jj]  )
      temp_data = temp_data[  !is.na(apply(temp_data, 1, sum ))    ,]
      if ( length(dim(temp_data)[1])>0  ) {
        if (dim(temp_data)[1]  >= 5) {
          temp_data_corr[kk] = cor( temp_data  )[2]
        }
      }
      kk=kk+1
      
      if (kk %% round(N_col_CS*(N_col_CS-1)/2/100) == 0)  print(kk/(N_col_CS*(N_col_CS-1)/2))
    }
  }
    
  apply( res_Panel_data_set, 2 ,var,na.rm=T)
  covM = temp_data_corr     
  
  return(covM)   
}



 






