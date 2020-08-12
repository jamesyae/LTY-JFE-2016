
################  IQR- Truncation function #################

fcn_IQR_truncate_idx <- function(data_set, trunc_crt) {    
  ### trunc_crt is single tail prob....so for 1% truncation in total, type in 0.5%.
  if (is.null(dim(data_set))) { 
    trunc_pnt = IQR(data_set[,jj], na.rm=TRUE)/2/qnorm(0.75)*qnorm(1-trunc_crt)
    temp_range = median(data_set[,jj] , na.rm=TRUE)+c(-trunc_pnt,trunc_pnt)
    idx_temp = data_set>temp_range[1]  &  data_set<temp_range[2]  
  } else {
    idx_temp = rep(TRUE, dim(data_set)[1])
    for (jj in seq(1,dim(data_set)[2])) {
      trunc_pnt = IQR(data_set[,jj], na.rm=TRUE)/2/qnorm(0.75)*qnorm(1-trunc_crt)
      temp_range = median(data_set[,jj] , na.rm=TRUE)+c(-trunc_pnt,trunc_pnt)
      idx_temp = idx_temp & data_set[,jj]>temp_range[1]  &  data_set[,jj]<temp_range[2]
    }
  } 
  idx_temp[is.na(idx_temp)]=FALSE
  return(idx_temp)  
}