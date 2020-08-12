fcn_firmlevel_TS_make_quarterly <- function( scaleVset, option) {  
  

    thisfilename = sprintf("new_variables_IBES_%s.Rdata", scaleVset)
    load(thisfilename)
   
    
    ye_scaled = ye 
    yf_scaled = yf 
 
  
  
  year_qtr_idx = fpe_year+qtr_index/4
  year_qtr_idx[is.na(ye)] = NA
  
  year_qtr_set = as.numeric(names(table(year_qtr_idx )))
 
    
  no_obs_qtr= rep(NA,length(year_qtr_set))
  onetoend = seq(1,length(ye))
  
  jj=1
  for (ii in year_qtr_set) {
        
    tempidx = onetoend[year_qtr_idx == ii ]
    
    yyy1 = ye_scaled[tempidx]
    yyy2 = yf_scaled[tempidx]
    yyy3 = yyy1-yyy2
        
    trunc_idx = rep(TRUE, length(yyy1))
    if (option$ye_trunc|option$yf_trunc|option$yef_trunc) {
      data_trunc = matrix(cbind( yyy1,yyy2,yyy3), ncol=3)[, c(option$ye_trunc,option$yf_trunc,option$yef_trunc) ]
      if (option$truncate=="quantile") trunc_idx <- (fcn_truncate_idx(  data_trunc , option$truncate_crt ))    
      if (option$truncate=="IQR2")     trunc_idx <- (fcn_IQR_truncate_idx2(  data_trunc , option$truncate_crt))                                              
    }
    
    ye_scaled[tempidx[!trunc_idx]] = NA
    yf_scaled[tempidx[!trunc_idx]] = NA
     
    no_obs_qtr[jj]  = sum(trunc_idx)
    print(ii)
    jj=jj+1
  }
  
    firm_TS_data = list(no_obs=no_obs,
                        ye_scaled=ye_scaled,
                        yf_scaled=yf_scaled,
                        year_qtr_set = year_qtr_set
                        no_obs_qtr=no_obs_qtr
                        )
    
  
  filename = fcn_filename_maker(scaleVset=scaleVset, option=option ) 
  save(firm_TS_data, file = filename)
    
    plot(ye_scaled)
    plot(yf_scaled)
    plot(ye_scaled-yf_scaled)
    print( no_obs_qtr)
  
}
 


