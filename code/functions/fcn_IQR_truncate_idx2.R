################  IQR- Truncation function 2 #################

fcn_IQR_truncate_idx2 <- function(data_set, trunc_crt) {    
  ### trunc_crt is single tail prob....so for 1% truncation in total, type in 0.5%.
  if (is.null(dim(data_set))) { 
    trunc_pnt = (IQR(data_set[,jj], na.rm=TRUE)/2)*(1/qnorm(0.75)*qnorm(1-trunc_crt)-1)
    temp_range = c(quantile(data_set[,jj],.25, na.rm=TRUE)-trunc_pnt, quantile(data_set[,jj],.75, na.rm=TRUE)+trunc_pnt)    
    idx_temp = data_set>temp_range[1]  &  data_set<temp_range[2]  
  } else {
    idx_temp = rep(TRUE, dim(data_set)[1])
    for (jj in seq(1,dim(data_set)[2])) {
      trunc_pnt = (IQR(data_set[,jj], na.rm=TRUE)/2)*(1/qnorm(0.75)*qnorm(1-trunc_crt)-1)      
      temp_range = c(quantile(data_set[,jj],.25, na.rm=TRUE)-trunc_pnt, quantile(data_set[,jj],.75, na.rm=TRUE)+trunc_pnt)
      idx_temp = idx_temp & data_set[,jj]>temp_range[1]  &  data_set[,jj]<temp_range[2]
    }
  } 
  idx_temp[is.na(idx_temp)]=FALSE
  return(idx_temp)  
}
