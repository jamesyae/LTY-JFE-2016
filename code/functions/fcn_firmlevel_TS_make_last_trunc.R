fcn_firmlevel_TS_make_last_trunc <- function( scaleVset, option) {  
   
  
  filename = fcn_filename_maker(scaleVset=scaleVset, option=option )
  load(file=filename)
  
  est = firm_TS_data
   
  
  fcn_multivar_trunc <- function(yyy1,yyy2,  option) {  
    if (option$ye_trunc|option$yf_trunc|option$yef_trunc) {  
      data_trunc = matrix(cbind( yyy1,yyy2,yyy1-yyy2), ncol=3)[, c(option$ye_trunc,option$yf_trunc,option$yef_trunc) ]
      if (option$truncate=="quantile") trunc_idx <- (fcn_truncate_idx(  data_trunc , option$truncate_crt ))    
      if (option$truncate=="IQR2")     trunc_idx <- (fcn_IQR_truncate_idx2(  data_trunc , option$truncate_crt))     
      return( seq(1,length(yyy1))[!trunc_idx] )  
    }
  }
  
  truncNAidx = fcn_multivar_trunc(yyy1=est$ye_scaled, yyy2=est$yf_scaled,  option)   
  est$ye_scaled[truncNAidx] = NA
  est$yf_scaled[truncNAidx] = NA
  
  tempidx = abs(est$ye_scaled + 0.0006614201)<0.00000001
  est$ye_scaled[tempidx ] = NA
  est$yf_scaled[tempidx ] = NA
   
  truncNAidx = fcn_multivar_trunc(yyy1=est$ye_firm_deflated_mu_sd1, yyy2=est$yf_firm_deflated_mu_sd1,   option)   
  est$ye_firm_deflated_mu_sd1[truncNAidx] = NA
  est$yf_firm_deflated_mu_sd1[truncNAidx] = NA
   
  
  
  ## storage
  ye_scaled = rep(NA, length(est$ye_scaled))
  yf_scaled = rep(NA, length(est$yf_scaled))
  no_obs_deflated = rep(NA, length(est$no_obs))
  
  tempset = matrix(rep(NA, length(est$no_obs)*3), ncol=3)
  sd_ye = tempset 
  sd_yf = tempset
  sd_yef = tempset
  
  skew_ye = tempset
  kurt_ye = tempset
  
  skew_yf = tempset
  kurt_yf = tempset
  
  skew_yef = tempset
  kurt_yef = tempset
  
   
  # Drop data with length less 30
  for (jj in seq( 1 ,no_firms))
  {      
    if (    no_eps[jj]>=30    )  {
      
      firm_idx_temp = firm_begin[jj]:firm_end[jj] 
      firm_idx_temp = firm_idx_temp[primary_idx[firm_idx_temp]]   
      
      trunc_idx = !is.na( est$ye_scaled[firm_idx_temp] )
        
        if (sum( trunc_idx)>=30) {     
              
          # store
          yyy1 = est$ye_scaled[firm_idx_temp[trunc_idx]] 
          yyy2 = est$yf_scaled[firm_idx_temp[trunc_idx]]   
            
          ye_scaled[firm_idx_temp[trunc_idx]] =  yyy1
          yf_scaled[firm_idx_temp[trunc_idx]] =  yyy2   
          no_obs_deflated[jj] = sum(trunc_idx) 
          
          nonNAidx = !is.na( yyy1)          
          yyy1 =  yyy1[nonNAidx]
          yyy2 =  yyy2[nonNAidx]           
          lyyy = sum(nonNAidx)
          
          for (kk in c(1:3)) {
            if (kk==1) tempidx = rep(TRUE, lyyy)
            if (kk==2) tempidx = c( rep(TRUE, floor(lyyy/2)), rep(FALSE, ceiling(lyyy/2))   )
            if (kk==3) tempidx = c( rep(FALSE, floor(lyyy/2)), rep(TRUE, ceiling(lyyy/2))   )
            
            sd_ye[jj,kk] = sd(yyy1[tempidx])
            sd_yf[jj,kk] = sd(yyy2[tempidx])
            sd_yef[jj,kk] = sd(yyy1[tempidx]-yyy2[tempidx])
            
            skew_ye[jj,kk] = skewness(yyy1[tempidx])
            kurt_ye[jj,kk] = kurtosis(yyy1[tempidx])
                 
            skew_yf[jj,kk] = skewness(yyy2[tempidx])
            kurt_yf[jj,kk] = kurtosis(yyy2[tempidx])
            
            skew_yef[jj,kk] = skewness((yyy1-yyy2)[tempidx])
            kurt_yef[jj,kk] = kurtosis((yyy1-yyy2)[tempidx])            
            
            }
        }
    }
  }
  
  trunc_result  = sprintf('Originally %s firm-quarter observations & %s firm observations. After pooled truncation and >30 rule, %s firm-quarter observations & %s firm observations',
          format(sum(!is.na(firm_TS_data$ye_scaled))-1, big.mark = "," ),
          format(sum(!is.na( firm_TS_data$no_obs )), big.mark = "," ),
          format(sum(!is.na(ye_scaled)), big.mark = "," ),
          format(sum(!is.na( no_obs_deflated)), big.mark = "," ) )
  
  
          
  
  
  ## storage
  ye_firm_deflated_mu_sd1 =  est$ye_firm_deflated_mu_sd1 
  yf_firm_deflated_mu_sd1 =  est$yf_firm_deflated_mu_sd1 
  no_obs_mu_sd1 = rep(NA, length( est$firm_begin_deflated))
  firm_begin_deflated = est$firm_begin_deflated
  firm_end_deflated = est$firm_end_deflated
  
  
  tempset = matrix(rep(NA, length(est$firm_begin_deflated)*3), ncol=3)
  sd_ye_mu_sd1 = tempset 
  sd_yf_mu_sd1 = tempset
  sd_yef_mu_sd1 = tempset
  
  skew_ye_mu_sd1 = tempset
  kurt_ye_mu_sd1 = tempset
  
  skew_yf_mu_sd1 = tempset
  kurt_yf_mu_sd1 = tempset
  
  skew_yef_mu_sd1 = tempset
  kurt_yef_mu_sd1 = tempset
  
          
        
  # Drop data with length less 30
  for (nn in seq( 1 , length(est$firm_begin_deflated) ))
  {      
  
      firm_idx_temp = est$firm_begin_deflated[nn]:est$firm_end_deflated[nn]  
      
      trunc_idx = !is.na(  est$ye_firm_deflated_mu_sd1[firm_idx_temp]  )
      
        
      if (sum( trunc_idx)<30) { 
        firm_begin_deflated[nn] = NA
        firm_end_deflated[nn] = NA
        no_obs_mu_sd1[nn] = NA
        ye_firm_deflated_mu_sd1[firm_idx_temp] = NA
        yf_firm_deflated_mu_sd1[firm_idx_temp] = NA
      } else
      {     
        no_obs_mu_sd1[nn] = sum( trunc_idx)
        
        yyy1 = est$ye_firm_deflated_mu_sd1[firm_idx_temp]-mean(est$ye_firm_deflated_mu_sd1[firm_idx_temp] ,na.rm=T)   
        yyy2 = est$yf_firm_deflated_mu_sd1[firm_idx_temp]-mean(est$yf_firm_deflated_mu_sd1[firm_idx_temp] ,na.rm=T)     
        
        ye_firm_deflated_mu_sd1[firm_idx_temp] =  yyy1  
        yf_firm_deflated_mu_sd1[firm_idx_temp] =  yyy2 
        
        
        nonNAidx = !is.na( yyy1)          
        yyy1 =  yyy1[nonNAidx]
        yyy2 =  yyy2[nonNAidx]           
        lyyy = sum(nonNAidx)
        
        for (kk in c(1:3)) {
          if (kk==1) tempidx = rep(TRUE, lyyy)
          if (kk==2) tempidx = c( rep(TRUE, floor(lyyy/2)), rep(FALSE, ceiling(lyyy/2))   )
          if (kk==3) tempidx = c( rep(FALSE, floor(lyyy/2)), rep(TRUE, ceiling(lyyy/2))   )
          
          sd_ye_mu_sd1[nn,kk] = sd(yyy1[tempidx])
          sd_yf_mu_sd1[nn,kk] = sd(yyy2[tempidx])
          sd_yef_mu_sd1[nn,kk] = sd(yyy1[tempidx]-yyy2[tempidx])
          
          skew_ye_mu_sd1[nn,kk] = skewness(yyy1[tempidx])
          kurt_ye_mu_sd1[nn,kk] = kurtosis(yyy1[tempidx])
          
          skew_yf_mu_sd1[nn,kk] = skewness(yyy2[tempidx])
          kurt_yf_mu_sd1[nn,kk] = kurtosis(yyy2[tempidx])
          
          skew_yef_mu_sd1[nn,kk] = skewness((yyy1-yyy2)[tempidx])
          kurt_yef_mu_sd1[nn,kk] = kurtosis((yyy1-yyy2)[tempidx])  
        }
      }
    }

  
  trunc_result_mu_sd1 =  sprintf('Originally %s firm-quarter observations & %s firm observations. After pooled truncation and >30 rule, %s firm-quarter observations & %s firm observations',
          format(sum(!is.na(firm_TS_data$ye_firm_deflated_mu_sd1)), big.mark = "," ),
          format(sum(  length(firm_TS_data$firm_begin_deflated) ) , big.mark = "," ),
          format(sum(!is.na( ye_firm_deflated_mu_sd1)), big.mark = "," ),
          format(sum(!is.na( no_obs_mu_sd1)), big.mark = "," ) )
  
  
  ye_firm_deflated_sd1 = est$ye_firm_deflated_sd1[!is.na(ye_firm_deflated_mu_sd1)] 
  yf_firm_deflated_sd1 = est$yf_firm_deflated_sd1[!is.na(ye_firm_deflated_mu_sd1)] 
  
  ## storage
  firm_TS_data = list()
  firm_TS_data = list(ye_scaled = ye_scaled,
                      yf_scaled = yf_scaled,
                      no_obs_deflated = no_obs_deflated,  
                      sd_ye = sd_ye,
                      sd_yf = sd_yf,
                      sd_yef = sd_yef,  
                      skew_ye = skew_ye,
                      kurt_ye = kurt_ye,  
                      skew_yf = skew_yf,
                      kurt_yf = kurt_yf,  
                      skew_yef = skew_yef,
                      kurt_yef =  kurt_yef,  
                      ye_firm_deflated_sd1 = ye_firm_deflated_sd1 ,
                      yf_firm_deflated_sd1 =  yf_firm_deflated_sd1 ,
                      ye_firm_deflated_mu_sd1 = ye_firm_deflated_mu_sd1 ,
                      yf_firm_deflated_mu_sd1 =  yf_firm_deflated_mu_sd1 ,
                      no_obs_mu_sd1  = no_obs_mu_sd1,
                      firm_begin_deflated = firm_begin_deflated,
                      firm_end_deflated =  firm_end_deflated,   
                      sd_ye_mu_sd1 = sd_ye_mu_sd1 ,
                      sd_yf_mu_sd1 = sd_yf_mu_sd1,
                      sd_yef_mu_sd1 = sd_yef_mu_sd1  ,
                      skew_ye_mu_sd1 = skew_ye_mu_sd1 ,
                      kurt_ye_mu_sd1 = kurt_ye_mu_sd1  ,
                      skew_yf_mu_sd1 = skew_yf_mu_sd1,
                      kurt_yf_mu_sd1 = kurt_yf_mu_sd1 , 
                      skew_yef_mu_sd1 =  skew_yef_mu_sd1,
                      kurt_yef_mu_sd1 = kurt_yef_mu_sd1,
                      trunc_result = trunc_result,
                      trunc_result_mu_sd1 = trunc_result_mu_sd1 )
    
  
  filename = fcn_filename_maker(scaleVset=scaleVset, option=option ) 
  save(firm_TS_data, file = filename)
 
  
}


 


