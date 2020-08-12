## compute the average of quintile, deciles.......
# x: variable to compute the quantile group mean
# crt: criteria variable for quantile grouping 
# no_group: number of quantile groups


fcn_quantile_group_mean <- function(x, crt, no_group) { 
  
    
  qgroup_means = rep(NA, no_group)
  break_points = quantile(crt, seq(0, 1, by=1/no_group),na.rm=T)
  break_points[no_group+1]= Inf
  

  for (ii in c(1:no_group)) {
    
    quantile_group_idx = break_points[ii] <= crt   &  crt  <break_points[ii+1]
    qgroup_means[ii] = mean(x[quantile_group_idx], na.rm=T)
    
  }

  return(qgroup_means)

}