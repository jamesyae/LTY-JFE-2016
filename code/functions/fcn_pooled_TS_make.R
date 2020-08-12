fcn_pooled_TS_make <- function(pdataset, initial_NA, inbetween_NA) {  
  
  pdataset$y
  pdataset$begin_idx
  pdataset$end_idx
  
  y_pooled = rep(NA, max(0, initial_NA - inbetween_NA) )
  for (ii in c(1:length(pdataset$begin_idx)))  
    if (!is.na(pdataset$begin_idx[ii]))
    y_pooled = c(y_pooled, rep(NA, inbetween_NA), pdataset$y[ pdataset$begin_idx[ii]:pdataset$end_idx[ii]  ] )
    
  return(y_pooled)
}


 