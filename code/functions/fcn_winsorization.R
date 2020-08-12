################  Winsorization function #################

fcn_winsorization <- function(data_set, win_crt) {   
  ### win_crt is single tail prob....so for 1% winsorazation in total, type in 0.5%.
  if (is.null(dim(data_set))) { 
    temp_range = quantile(data_set, c(win_crt, 1-win_crt ), na.rm=TRUE)
    
    data_set[data_set<temp_range[1]] = temp_range[1]
    data_set[data_set>temp_range[2]] = temp_range[2] 
  } else {
    for (jj in seq(1,dim(data_set)[2])) {
      temp_range = quantile(data_set[,jj], c(win_crt, 1-win_crt ), na.rm=TRUE) 
      data_set[data_set[,jj]<temp_range[1],jj] = temp_range[1]
      data_set[data_set[,jj]>temp_range[2],jj] = temp_range[2]       
    }
  } 
  
  return(data_set)  
}
