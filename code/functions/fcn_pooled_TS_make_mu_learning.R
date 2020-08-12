fcn_pooled_TS_make_mu_learning <- function(pdataset ) {  
  
  ii=1
  ye_pooled = pdataset$y1[ pdataset$begin_idx[ii]:pdataset$end_idx[ii]  ]
  yf_pooled = pdataset$y2[ pdataset$begin_idx[ii]:pdataset$end_idx[ii]  ]
  KF_idx = seq(1, c(pdataset$end_idx[ii]-pdataset$begin_idx[ii] +1))
  
  
  
  
  for (ii in c(2:length(pdataset$begin_idx)))  
    if (!is.na(pdataset$begin_idx[ii])) {
      ye_pooled = c(ye_pooled, rep(NA, 1) ,  pdataset$y1[ pdataset$begin_idx[ii]:pdataset$end_idx[ii]  ]  )
      yf_pooled = c(yf_pooled, rep(NA, 1) ,  pdataset$y2[ pdataset$begin_idx[ii]:pdataset$end_idx[ii]  ]  )
      KF_idx = c(  KF_idx,   seq(0, c(pdataset$end_idx[ii] - pdataset$begin_idx[ii]+1))) 
    }    
   
  yt = matrix(rbind(ye_pooled,yf_pooled),nrow=2) 
  
  temp_data = list(yt=yt, KF_idx=KF_idx)
  
  return(temp_data)
}




