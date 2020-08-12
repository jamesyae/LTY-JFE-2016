fcn_firmlevel_TS_make_pooled_trunc <- function( scaleVset, option) {  
## Just truncate pooled level  

    thisfilename = sprintf("new_variables_IBES_%s.Rdata", scaleVset)
    load(thisfilename)
   
    
    ye_scaled = ye 
    yf_scaled = yf 
 
    yyy1 = ye_scaled 
    yyy2 = yf_scaled 
    yyy3 = yyy1-yyy2
   
    trunc_idx = rep(TRUE, length(yyy1))
    if (option$ye_trunc|option$yf_trunc|option$yef_trunc) {
      data_trunc = matrix(cbind( yyy1,yyy2,yyy3), ncol=3)[, c(option$ye_trunc,option$yf_trunc,option$yef_trunc) ]
      if (option$truncate=="quantile") trunc_idx <- (fcn_truncate_idx(  data_trunc , option$truncate_crt ))    
      if (option$truncate=="IQR2")     trunc_idx <- (fcn_IQR_truncate_idx2(  data_trunc , option$truncate_crt))                                              
    }
    
    ye_scaled[!trunc_idx] = NA
    yf_scaled[!trunc_idx] = NA
    
    
    
    
    
    
    ye_yf_scaled = list(ye=ye_scaled,
                        yf=yf_scaled)
    
  
  filename = fcn_filename_maker(scaleVset=scaleVset, option=option ) 
  save( ye_yf_scaled, file = filename)
    
#    plot(ye_scaled)
#    plot(yf_scaled)
#    plot(ye_scaled-yf_scaled)
  
}
 


