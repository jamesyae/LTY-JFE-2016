fcn_firmlevel_TS_make_general <- function( scaleVset, option,  maxlength, separ_firms ) {  
  
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
  
  
  
  no_scaleV = length(scaleVset)
  for (mm in c(1:no_scaleV)) {
    thisfilename = sprintf("new_variables_IBES_%s.Rdata", scaleVset[mm])
    load(thisfilename)
    #N_row= length(ye)    
    filename = fcn_filename_maker(scaleVset=scaleVset, option=option ) 
    load(  file = filename)
    ye = ye_yf_scaled$ye
    yf = ye_yf_scaled$yf    
    if (mm==1){
      ye_scaled_set = matrix(rep(NA,N_row*no_scaleV),nrow=N_row)
      yf_scaled_set = matrix(rep(NA,N_row*no_scaleV),nrow=N_row)
    }
    
    ye_scaled_set[,mm] = ye 
    yf_scaled_set[,mm] = yf 
  }  
  temps = rep(NA, no_firms )
  ar1 =  list( aic=temps, loglkhd=temps,R2=temps,mu=temps,phi=temps,sig.e=temps, R2KF=temps ) 
  ma1 =  list( aic=temps, loglkhd=temps,R2=temps,mu=temps,matheta=temps,sig.e=temps, R2KF=temps)   
  ar1_sd1 =  list( aic=temps, loglkhd=temps,R2=temps,mu=temps,phi=temps,sig.e=temps, R2KF=temps ) 
  ma1_sd1 =  list( aic=temps, loglkhd=temps,R2=temps,mu=temps,matheta=temps,sig.e=temps, R2KF=temps)   
  
  
  # ye and yf anctually used in estimation
  ye_scaled = rep(NA, N_row)
  yf_scaled = rep(NA, N_row)
  
  trunc_idx_used  =    matrix(rep(FALSE,N_row),ncol=1)  
  
  temps = rep(NA, no_firms) 
  
  ar1= list( aic=temps, loglkhd=temps,R2=temps,mu=temps,phi=temps,sig.e=temps) 
  ma1=  list( aic=temps, loglkhd=temps,R2=temps,mu=temps,matheta=temps,sig.e=temps) 
  
  
  truncate_rate = rep(NA, no_firms)
  scaling_idx = rep(FALSE, no_firms)
  
  sd_ye=    matrix(rep(NA,no_firms*3),ncol=3) 
  sd_yf=    matrix(rep(NA,no_firms*3),ncol=3) 
  sd_yef=    matrix(rep(NA,no_firms*3),ncol=3) 
  skew_ye=    matrix(rep(NA,no_firms*3),ncol=3) 
  kurt_ye=    matrix(rep(NA,no_firms*3),ncol=3) 
  mad_ye=    matrix(rep(NA,no_firms*3),ncol=3)
  
  skew_yf=    matrix(rep(NA,no_firms*3),ncol=3)
  skew_yef=    matrix(rep(NA,no_firms*3),ncol=3)
  kurt_yf=    matrix(rep(NA,no_firms*3),ncol=3)
  kurt_yef=    matrix(rep(NA,no_firms*3),ncol=3)
  
  
  no_obs= rep(NA, no_firms)
  no_obs_deflated= rep(NA, no_firms)
  firm_match_idx= rep(NA, no_firms)
  
  
  firm_begin_deflated = rep(NA, no_firms) 
  firm_end_deflated = rep(NA, no_firms) 
  ye_firm_deflated = rep(NA, N_row)
  yf_firm_deflated = rep(NA, N_row)
  ye_firm_deflated_mu = rep(NA, N_row)
  yf_firm_deflated_mu = rep(NA, N_row)  
  TS_index_firm_deflated = rep(NA, N_row)  
  
  
  year_qtr_idx = rep(NA, N_row)
  firm_deflated_nn_idx = rep(NA, N_row)
  
  sd_estimate.firm= rep(NA, N_row)
  marketvalue.firm= rep(NA, N_row)
  at.firm= rep(NA, N_row)
  sales.firm= rep(NA, N_row)
  be.firm= rep(NA, N_row)
  size_cat.firm= rep(NA, N_row)
  ffindustry.firm= rep(NA, N_row)
  firmage.firm= rep(NA, N_row) 
  volatility.firm= rep(NA, N_row) 
  nocov.firm = rep(NA, N_row) 
  
  if (1>2) {
    # when firmage exist, permno exists.
    sum(    (  is.na(FE$permno) > is.na(FE$firmage) )  )
    sum(    (  is.na(FE$cusip) > is.na(FE$firmage) )  )
    sum( !is.na(FE$median_estimate) & is.na(FE$cusip) )
      
    tempcheck0=  rep(NA, no_firms)  
    for (jj in seq( 1 ,no_firms))
    {      
      
      if (    no_eps[jj]>=3        )  {
        firm_idx_temp = firm_begin[jj]:firm_end[jj] 
          
        report_date_index = as.numeric(substr(strptime( FE$report_date[ firm_idx_temp] , "%d %b %Y")  ,1,4) )+
          as.numeric(substr(strptime( FE$report_date[ firm_idx_temp] , "%d %b %Y")  ,6,7))/12 
        x = report_date_index-FE$firmage[ firm_idx_temp]
              
        tempcheck0[jj] =   sum(tapply(x, FE$permno[ firm_idx_temp ], sd, na.rm=T) ,na.rm=T)
      }
    } 
    max(tempcheck0,na.rm=T)
    #firm age follows permno
  }
  

  
  
  
## (I) fill the missing permno 
## 1) NA before the first permno, NA after the last permno.
## 2) match missing permno in-between based on cusip and companyname.
   
  permno.NA_fill = FE$permno
  for (jj in seq( 1  ,no_firms))
  {      
    firm_idx_temp = firm_begin[jj]:firm_end[jj]  
 
    
    if (   sum( !is.na(ye_scaled_set[firm_idx_temp])) >= maxlength        )  {
       
      x = FE$permno[firm_idx_temp]
      
      # if all permno are missing, assign one unified permno for all.
      if ( sum(!is.na(x))==0  )  permno.NA_fill[firm_idx_temp]=  90000 
      
    
      # if there is only one permno
      napermnoset = which( is.na(permno.NA_fill[firm_idx_temp] ) )
      savetemp = (permno.NA_fill[firm_idx_temp] )[ which( !is.na(permno.NA_fill[firm_idx_temp] ) ) ]
      if (sum(table(permno.NA_fill[firm_idx_temp])>0) == 1 )  permno.NA_fill[firm_idx_temp[ napermnoset ] ] = savetemp[1]
      
      
      #NA before the first permno, 
      val1 = (x[ !is.na(x ) ])[1]    
      if (sum(!is.na(x))>0)  permno.NA_fill[ firm_idx_temp[1:(which(!is.na(x ))[1])  ] ]= val1
      
      #NA after the last permno.
      val1 = (x[ !is.na(x) ])[  sum(!is.na(x))    ]
      if  (sum(!is.na(x))>0)  permno.NA_fill[ firm_idx_temp[ (which(!is.na(x ))[ sum(!is.na(x)) ]):length(firm_idx_temp)  ] ]= val1
       
  
      # match cusip
      napermnoset = which( is.na(permno.NA_fill[firm_idx_temp] ) )      
      if (  length(napermnoset )>0  &  sum(!is.na(napermnoset))>0  & sum(!is.na(permno.NA_fill[firm_idx_temp]))>0 ) {  
        y = FE$cusip[firm_idx_temp[ napermnoset  ] ]
        napermnoset =  napermnoset[y!=""]
        y = FE$cusip[firm_idx_temp[ napermnoset  ] ]
        
        #if (sum(table(y)>0)>1) break 
        
        if (sum(table(y)>0)==1){
          savetemp =  (table(permno.NA_fill[ firm_idx_temp[ FE$cusip[firm_idx_temp] == y[1] ]]))
          savetemp =  names( savetemp[savetemp>0])  
          if (     length(savetemp)==1 & sum(!is.na((savetemp)>0) ) ) permno.NA_fill[firm_idx_temp[ napermnoset ] ] = savetemp
        }
        
      } 
      
      # match compnay name
      napermnoset = which( is.na(permno.NA_fill[firm_idx_temp] ) )
      if (  length(napermnoset )>0  &  sum(!is.na(napermnoset))>0 & sum(!is.na(permno.NA_fill[firm_idx_temp]))>0 ) {  
        #for( kk in c(1:length(napermnoset)) ) { 
          
          x = FE$companyname[firm_idx_temp[ napermnoset  ] ] 
          napermnoset =  napermnoset[x!=""]
          x = FE$companyname[firm_idx_temp[ napermnoset  ] ] 
          
          #if (sum(table(x)>0)>1) break 
           
          if (sum(table(x)>0)==1){
          savetemp =  ( table(permno.NA_fill[ firm_idx_temp[ FE$companyname[firm_idx_temp] == x[1] ]]) )
          savetemp =  names( savetemp[savetemp>0])
          if (   length(savetemp)==1 &  sum(!is.na((savetemp)>0)) ) permno.NA_fill[firm_idx_temp[ napermnoset  ] ] = savetemp
          }
      }
        
    
      
    } 
    
  }
   
 
  sum(!is.na(permno.NA_fill))
  sum(!is.na( FE$permno ))
   
      
  
  
## (II) fill the missing firm age based on permno  
  firmage.NA_fill = FE$firmage
  for (jj in seq( 1 ,no_firms))
  {      
    firm_idx_temp = firm_begin[jj]:firm_end[jj] 
    if (   sum( !is.na(ye_scaled_set[firm_idx_temp])) >= maxlength        )  {
      
      
      report_date_index = as.numeric(substr(strptime( FE$report_date[ firm_idx_temp] , "%d %b %Y")  ,1,4) )+
        as.numeric(substr(strptime( FE$report_date[ firm_idx_temp] , "%d %b %Y")  ,6,7))/12 
      #x = report_date_index-FE$firmage[ firm_idx_temp]
      firmage_ref = report_date_index - FE$firmage[ firm_idx_temp]
      
      nafirmage_idx_temp = is.na( FE$firmage[ firm_idx_temp] )
      nafirmage_idx_temp_all = firm_idx_temp[ nafirmage_idx_temp ] 
      
      x = tapply(firmage_ref, permno.NA_fill[ firm_idx_temp ], median, na.rm=T)
      
      savetemp = report_date_index[nafirmage_idx_temp]- 
        x[ match( permno.NA_fill[nafirmage_idx_temp_all]  , as.numeric(names(x))) ]
      
      #savetemp[savetemp<0] =NA
      
      firmage.NA_fill[ nafirmage_idx_temp_all ] = savetemp
           
      
    }
  }   
  
  sum(is.na(FE$firmage))  
  sum(is.na( firmage.NA_fill))
       
 
  
### (III) Identify the firms that should be seperated  
  tempcheck=  rep(NA, no_firms)  
  for (jj in seq( 1 ,no_firms))
  {      
    firm_idx_temp = firm_begin[jj]:firm_end[jj] 
    if (   sum( !is.na(ye_scaled_set[firm_idx_temp])) >= maxlength        )  {
      
      
      report_date_index = as.numeric(substr(strptime( FE$report_date[ firm_idx_temp] , "%d %b %Y")  ,1,4) )+
        as.numeric(substr(strptime( FE$report_date[ firm_idx_temp] , "%d %b %Y")  ,6,7))/12 
      
      tempcheck[jj] =  diff(range(report_date_index- firmage.NA_fill[ firm_idx_temp],na.rm=T))
    }
  }    
  
  diff_age_set = which(  tempcheck > 0.1    )
   
 
  tempcheck11=  rep(NA, no_firms) 
  for (jj in  seq( 1 ,no_firms))
  {      
    firm_idx_temp = firm_begin[jj]:firm_end[jj] 
 
    if (   sum( !is.na(ye_scaled_set[firm_idx_temp])) >= maxlength        )  {
    tempcheck11[jj] = sum(!is.na(ye_scaled_set[firm_idx_temp])  &  is.na(firmage.NA_fill[ firm_idx_temp]))
}

  }    
  
 which( tempcheck11 > 0     )
  sum(tempcheck11==0,na.rm=T)
 

  
  
  if (1>2) {
    
    
    tempcheck8=  rep(NA, no_firms) 
    for (jj in seq( 1 ,no_firms))
    {      
      
      if (    no_eps[jj]>=20  &  (jj %in% diff_age_set)    )  {
        firm_idx_temp = firm_begin[jj]:firm_end[jj]  
        
        tempcheck8[jj] = sum(   FE$companyname[firm_idx_temp]==""   &    FE$cusip[firm_idx_temp]==""   &   is.na(permno.NA_fill[firm_idx_temp])  & is.na(FE$actual_eps[firm_idx_temp]-FE$median_estimate[firm_idx_temp] )  )
        
      }
    }  
    table(tempcheck8)
    
    
    
    # firm separation by permno? (mostly M&A) too many NA in-between? too much gap in beginyear?
    # too much change in ye or yef?  
    for (jj in seq( 1 ,no_firms))
    {      
      
      if (    no_eps[jj]>=3        )  {
        firm_idx_temp = firm_begin[jj]:firm_end[jj] 
        
        report_date_index = as.numeric(substr(strptime( FE$report_date[ firm_idx_temp] , "%d %b %Y")  ,1,4) )+
          as.numeric(substr(strptime( FE$report_date[ firm_idx_temp] , "%d %b %Y")  ,6,7))/12 
        #x = report_date_index-FE$firmage[ firm_idx_temp]
        firmage_ref = report_date_index - FE$firmage[ firm_idx_temp]
        
        
        nafirmage_idx_temp = is.na( firmage.NA_fill[ firm_idx_temp] )  &  is.na( FE$permno[ firm_idx_temp] )
        nafirmage_idx_temp_all = firm_idx_temp[ nafirmage_idx_temp ] 
        x = tapply(firmage_ref, paste(FE$cusip[ firm_idx_temp ]), median, na.rm=T)
        
        savetemp = as.numeric( report_date_index[nafirmage_idx_temp]- 
                                 x[ match(paste(FE$cusip[nafirmage_idx_temp_all])  , paste(names(x)) ) ] )
        
        savetemp[savetemp<0] =NA
        firmage.NA_fill[ nafirmage_idx_temp_all ] = savetemp
        
        
        
      }
    }   
    sum(is.na(FE$firmage))  
    sum(is.na( firmage.NA_fill))
    sum(   is.na( firmage.NA_fill[!is.na(FE$median_estimate) ])   )
    
    
    
    tempcheck2=  rep(NA, no_firms)  
    for (jj in   diff_age_set )
    {      
      
      firm_idx_temp = firm_begin[jj]:firm_end[jj]  
      tempcheck2[jj] =   sum(is.na(FE$permno[firm_idx_temp[!is.na(FE$median_estimate[firm_idx_temp])]        ]))  
    } 
    diff_age_set2 = which(  tempcheck2 > 0    )
    
    
    tempcheck2=  rep(NA, no_firms)  
    for (jj in   diff_age_set )
    {      
      
        firm_idx_temp = firm_begin[jj]:firm_end[jj]  
        tempcheck2[jj] =   sum(is.na(FE$permno[firm_idx_temp[!is.na(FE$median_estimate[firm_idx_temp])]        ])>0)  
    }  
    
    
    tempcheck5=  rep(NA, no_firms)  
    tempcheck6=  rep(NA, no_firms)  
    for (jj in   diff_age_set )
    {      
      
      firm_idx_temp = firm_begin[jj]:firm_end[jj]  
      
         aa= sum( !is.na(FE$actual_eps[firm_idx_temp]-FE$median_estimate[firm_idx_temp])  )
      
      if  (aa>0){
         bb = which(!is.na(FE$actual_eps[firm_idx_temp]-FE$median_estimate[firm_idx_temp]) )
         tempcheck5[jj] = (TS_index[firm_idx_temp])[bb[length(bb)]]-(TS_index[firm_idx_temp])[bb[1]] +1 -aa
         tempcheck6[jj] =exp(diff(log(range(tapply(FE$actual_eps[firm_idx_temp]-FE$median_estimate[firm_idx_temp], (FE$permno[ firm_idx_temp ]), sd, na.rm=T)))))
         
      }
       
      
    }  
  
    
    sum(tempcheck6 < 2  &  tempcheck5<12  &   tempcheck <3, na.rm=T)
    which(tempcheck6 < 2  &  tempcheck5<12  &   tempcheck <3 )
    which(tempcheck6 < 2  &  tempcheck5<5)
    
     for ( jj in which(tempcheck6 < 2  &  tempcheck5<12  &   tempcheck <3 ) ){
    firm_idx_temp = firm_begin[jj]:firm_end[jj]
    print(FE[firm_idx_temp ,])
     }
  
    
    
    tempcheck7=  rep(NA, no_firms)   
    for (jj in  ( seq( 1 ,no_firms)  ) )
    {      
      if (!( jj   %in%   diff_age_set  )){
      firm_idx_temp = firm_begin[jj]:firm_end[jj]  
      
      aa= sum( !is.na(FE$actual_eps[firm_idx_temp]-FE$median_estimate[firm_idx_temp])  )
      
      if  (aa>19){
        bb = which(!is.na(FE$actual_eps[firm_idx_temp]-FE$median_estimate[firm_idx_temp]) )
        tempcheck7[jj] =(TS_index[firm_idx_temp])[bb[length(bb)]]-(TS_index[firm_idx_temp])[bb[1]] +1 -aa
      }
      
      
      }
    }  
    
    table(tempcheck7)
    
    sum(tempcheck7 < 7   , na.rm=T)
    
    which(tempcheck7==94)
  }
  
  
 
  
  
  
  if (1>2) {
    
    
    
    
    
    
    
    for (jj in seq( 1 ,no_firms))
    {      
      
      if (    no_eps[jj]>=3        )  {
        firm_idx_temp = firm_begin[jj]:firm_end[jj]  
        
        report_date_index = as.numeric(substr(strptime( FE$report_date[ firm_idx_temp] , "%d %b %Y")  ,1,4) )+
          as.numeric(substr(strptime( FE$report_date[ firm_idx_temp] , "%d %b %Y")  ,6,7))/12 
        
        na_firmage_idx_temp = is.na(FE$firmage[ firm_idx_temp])
        
        firmage_ref = report_date_index - FE$firmage[ firm_idx_temp]
        
        if( sum(!is.na(firmage_ref)   )>0  &  tempcheck[jj]>0.1  & no_eps[jj]>=20 ) {
          val1 = (firmage_ref[ !is.na(firmage_ref) ])[1]      
          if ( FE$cusip[firm_idx_temp[1]]!=""  & FE$cusip[firm_idx_temp[1]] != (FE$cusip[firm_idx_temp[ !is.na(firmage_ref)  ]])[1] ) print(jj)
          
          
          val1 = (firmage_ref[ !is.na(firmage_ref) ])[  sum(!is.na(firmage_ref))    ]
          if ( FE$cusip[firm_idx_temp[length(firm_idx_temp)]]!=""  & FE$cusip[firm_idx_temp[length(firm_idx_temp)]] != (FE$cusip[firm_idx_temp[ !is.na(firmage_ref)  ]])[sum(!is.na(firmage_ref))] ) print(jj)
        }
      }
    }
    
    jj =  which(tempcheck0>1)[2]; FE[firm_begin[jj]:firm_end[jj] ,]
    table(round(firmage_ref,2))
    
    
    
    firmage.NA_fill = FE$firmage
    
    for (jj in seq( 1 ,no_firms))
    {      
      
      if (    no_eps[jj]>=3        )  {
        firm_idx_temp = firm_begin[jj]:firm_end[jj]  
        
        report_date_index = as.numeric(substr(strptime( FE$report_date[ firm_idx_temp] , "%d %b %Y")  ,1,4) )+
          as.numeric(substr(strptime( FE$report_date[ firm_idx_temp] , "%d %b %Y")  ,6,7))/12 
        
        na_firmage_idx_temp = is.na(FE$firmage[ firm_idx_temp])
        
        firmage_ref = report_date_index - FE$firmage[ firm_idx_temp]
        val1 = (firmage_ref[ !is.na(firmage_ref) ])[1]      
        if (FE$cusip[firm_idx_temp[1]] == (FE$cusip[firm_idx_temp[ !is.na(firmage_ref)  ]])[1] ) firmage_ref[ 1:(which( !is.na(firmage_ref) )[1]) ] = val1   
        
        
        val1 = (firmage_ref[ !is.na(firmage_ref) ])[  sum(!is.na(firmage_ref))    ]
        if (FE$cusip[firm_idx_temp[length(firm_idx_temp)]] == (FE$cusip[firm_idx_temp[ !is.na(firmage_ref)  ]])[sum(!is.na(firmage_ref))] ) firmage_ref[  (which( !is.na(firmage_ref) )[sum(!is.na(firmage_ref)) ]):length(firmage_ref)    ] = val1
        
        for (  kk in 1:length(firmage_ref )   ){        
          
          
          if ( is.na(firmage_ref[kk] ))
            firmage_ref[kk]
          
          
          
          (firmage_ref[ !is.na(firmage_ref) ])[ ]
          
          
          
        }
        
        kk
        
        firmage_ref
        
        
        firmage.NA_fill  = report_date_index - firmage_ref
        
        
        
        
      }
    }   
    
    
    
    
      table(tempcheck)
      which(  tempcheck > 11 & tempcheck  <12 & no_eps[jj]>=20 )
      sum(tempcheck > 0.1,na.rm=T)
      length(tempcheck)
      sum(tempcheck > 0.1 & no_eps[jj]>=20 ,na.rm=T)
      
      
      
      jj=2774; firm_idx_temp =firm_begin[jj]:firm_end[jj] 
      
      jj=691; firm_idx_temp =firm_begin[jj]:firm_end[jj] 
      FE[firm_idx_temp     ,]
      
      
      ### missing cusip
      tempcheck3=  rep(NA, no_firms)  
      for (jj in seq( 1 ,no_firms))
      {    
        if (     no_eps[jj]>=3        )  {
          firm_idx_temp = firm_begin[jj]:firm_end[jj] 
          firm_idx_temp = firm_idx_temp[primary_idx[firm_idx_temp]]  
           
          tempcheck3[jj] =   sum(FE$cusip[firm_idx_temp]=="",na.rm=T)
        }
      }      
      table(tempcheck3)
      
      
      tempcheck4=  rep(NA, no_firms)  
      for (jj in seq( 1 ,no_firms))
      {   
        if (    tempcheck[jj]>= 0.1 & no_eps[jj]>=3      )  {
          firm_idx_temp = firm_begin[jj]:firm_end[jj] 
          firm_idx_temp = firm_idx_temp[primary_idx[firm_idx_temp]]  
           
          
          tempcheck4[jj] = 0
          for ( lev  in  levels(FE$cusip[firm_idx_temp])[table(FE$cusip[firm_idx_temp])!=0] ){ 
          
            ttempidx = FE$cusip[firm_idx_temp] == lev
            
            report_date_index = as.numeric(substr(strptime( (FE$report_date[ firm_idx_temp])[ttempidx] , "%d %b %Y")  ,1,4) )+
            as.numeric(substr(strptime( (FE$report_date[ firm_idx_temp])[ttempidx] , "%d %b %Y")  ,6,7))/12 
    
            print(diff(range(report_date_index - (FE$firmage[ firm_idx_temp])[ttempidx],na.rm=T)) )
            if (!is.na(diff(range(report_date_index - (FE$firmage[ firm_idx_temp])[ttempidx],na.rm=T)) )) {
              tempcheck4[jj] = tempcheck4[jj]+ diff(range(report_date_index - (FE$firmage[ firm_idx_temp])[ttempidx],na.rm=T)) 
            }
          }
          
          
        }
      }    
      table(tempcheck4)
      
    
      
      
      for (jj in    which(tempcheck4>4))
      {
        
        
      
      }
      
      
      tempcheck2=  rep(NA, no_firms)  
      for (jj in  seq(1, no_firms )[tempcheck >=  1  &  no_eps >=20] ){
        firm_idx_temp = firm_begin[jj]:firm_end[jj] 
        firm_idx_temp = firm_idx_temp[primary_idx[firm_idx_temp]]  
        
        tempcheck2[jj] =  sum(table(FE$cusip[firm_idx_temp])!=0) - c( ""  %in% levels(FE$cusip[firm_idx_temp])[table(FE$cusip[firm_idx_temp])!=0] )
      }  
      table(tempcheck2)
      
      
      sum(is.na(report_date_index))
      sum(is.na(FE$fpedats)) 
      sum(is.na(FE$ticker))
      sum(is.na(ticker_fixed))
      sum(ticker_fixed=="",na.rm=        T)
      sum(FE$cusip=="",na.rm=        T)
      sum( is.na(FE$permno),na.rm=        T)
      
      sum( is.na(FE$firmage) ,na.rm=        T)
      sum( is.na(FE$yyyymm) ,na.rm=        T)
      
      FE$yyyymm[is.na(FE$firmage)]
      
      
      sum( (FE$permno)=="",na.rm=        T)
      
      
      sum(abs(is.na(ticker_fixed)-is.na(FE$cusip)))
          
      table(table(FE$cusip))
      table(table(ticker_fixed))
      
      
      length(table(FE$cusip))
      length(table(ticker_fixed))
      
      
      sort(table(FE$cusip),decreasing=TRUE) [1:10]
      sort(table(ticker_fixed),decreasing=TRUE) [1:10]
       
      
  
}
  
  
  
   
  
  
  
  
  
  nn=1
  firm_end_deflated[1] = 0;
  
  ss=1
  
  for (jj in seq( 1 ,no_firms))
  {  
      
    if (    no_eps[jj]>=maxlength        )  {
      
      

      
      
      firm_idx_temp = firm_begin[jj]:firm_end[jj] 
      firm_idx_temp = firm_idx_temp[primary_idx[firm_idx_temp]]      
              
      ## (IV)  Separate the firms based on permno. 
      no_subfirm = 1
      
      if ( separ_firms &  jj %in%  diff_age_set   ){        
        no_subfirm = sum( table(permno.NA_fill[firm_idx_temp])>0 ,na.rm=T)
        subfirm_permno_set = as.numeric( names( table(permno.NA_fill[firm_idx_temp])[table(permno.NA_fill[firm_idx_temp])>0] ) )
        permno.NA_fill_temp = permno.NA_fill[firm_idx_temp]
      }
      
      jjs =1
      firm_idx_temp_save = firm_idx_temp
      
      ## (IV)  Separate the firms based on permno. 
      while (jjs <= no_subfirm ) {          
        
          if (  separ_firms &  jj %in% diff_age_set) {
            firm_idx_temp = firm_idx_temp_save[ permno.NA_fill_temp == subfirm_permno_set[jjs] ]
            firm_idx_temp = firm_idx_temp[ !is.na(firm_idx_temp)]
          }
            
          yyy1  =  ye_scaled_set[firm_idx_temp,ss]  
          yyy2  =  yf_scaled_set[firm_idx_temp,ss]    
          
          trunc_idx = !is.na(yyy1)
  #      if (option$ye_trunc|option$yf_trunc|option$yef_trunc) {
  #       data_trunc = matrix(cbind( yyy1,yyy2,yyy1-yyy2), ncol=3)[, c(option$ye_trunc,option$yf_trunc,option$yef_trunc) ]
  #         if (option$truncate=="quantile") trunc_idx <- (fcn_truncate_idx(  data_trunc , option$truncate_crt ))    
  #         if (option$truncate=="IQR2")     trunc_idx <- (fcn_IQR_truncate_idx2(  data_trunc , option$truncate_crt))                                              
  #       }
           
        
          
          #if (sum( trunc_idx)>=maxlength &   !(jj %in%  non_converge_set )) {     
          if (sum( trunc_idx)>=maxlength    ) {               
             
           #if ( )   {
            
            # Construct Time-series with proper missing observations & timing
            TS_index_temp = TS_index[firm_idx_temp[trunc_idx]]   
            TS_index_temp = TS_index_temp - min(TS_index_temp,na.rm=T) + 1
            yyy1 = rep(NA, max(TS_index_temp )  )
            yyy2 = yyy1
            
            yyy1[TS_index_temp] =  ye_scaled_set[firm_idx_temp[trunc_idx],ss]  
            yyy2[TS_index_temp] =  yf_scaled_set[firm_idx_temp[trunc_idx],ss]  
                      
            # store
            ye_scaled[firm_idx_temp[trunc_idx]] = ye_scaled_set[firm_idx_temp[trunc_idx],ss] 
            yf_scaled[firm_idx_temp[trunc_idx]] = yf_scaled_set[firm_idx_temp[trunc_idx],ss]  
            
                
            firm_begin_deflated[nn] = firm_end_deflated[max(nn-1,1)]+1
            firm_end_deflated[nn] = firm_begin_deflated[nn]+length(yyy1)-1
            b_idx = firm_begin_deflated[nn]
            e_idx = firm_end_deflated[nn]
            ye_firm_deflated[b_idx:e_idx] = yyy1
            yf_firm_deflated[b_idx:e_idx] = yyy2
            ye_firm_deflated_mu[b_idx:e_idx] = yyy1-mean(yyy1,na.rm=T)
            yf_firm_deflated_mu[b_idx:e_idx] = yyy2-mean(yyy2,na.rm=T)    
            TS_index_firm_deflated[b_idx:e_idx] = seq(1,  length(yyy1))
            
            sd_estimate.firm[(b_idx:e_idx)[TS_index_temp]] =  DISP[firm_idx_temp[trunc_idx]]  
            marketvalue.firm[(b_idx:e_idx)[TS_index_temp]] =  FE$marketvalue[firm_idx_temp[trunc_idx]] 
            at.firm[(b_idx:e_idx)[TS_index_temp]] =  FE$at[firm_idx_temp[trunc_idx]] 
            sales.firm[(b_idx:e_idx)[TS_index_temp]] =  FE$sales[firm_idx_temp[trunc_idx]] 
            be.firm[(b_idx:e_idx)[TS_index_temp]] =  FE$be[firm_idx_temp[trunc_idx]] 
            size_cat.firm[(b_idx:e_idx)[TS_index_temp]] =  FE$size_cat[firm_idx_temp[trunc_idx]] 
            ffindustry.firm[(b_idx:e_idx)[TS_index_temp]] =  FE$ffindustry[firm_idx_temp[trunc_idx]] 
            firmage.firm[(b_idx:e_idx)[TS_index_temp]] =  firmage.NA_fill[firm_idx_temp[trunc_idx]] 
            volatility.firm[(b_idx:e_idx)[TS_index_temp]] =  FE$volatility[firm_idx_temp[trunc_idx]] 
            nocov.firm[(b_idx:e_idx)[TS_index_temp]] = FE$no_of_estimates[firm_idx_temp[trunc_idx]] 
   
            temp_year_qtr = rep(NA, max(TS_index_temp )  )
            temp_year_qtr[TS_index_temp] = fpe_year[ firm_idx_temp[trunc_idx] ]+0.25*qtr_index[ firm_idx_temp[trunc_idx] ]
            #temp_save_idx = c(b_idx:e_idx)[!is.na(yyy1)]          
            year_qtr_idx[b_idx:e_idx] = temp_year_qtr
            firm_deflated_nn_idx[b_idx:e_idx] = nn
            
            
            trunc_idx_used[firm_idx_temp] = trunc_idx
            #logR_mad_all = logR_mad[ss]
     
            
            
            nonNAidx = !is.na( yyy1)
            
            yyy1 =  yyy1[nonNAidx]
            yyy2 =  yyy2[nonNAidx] 
            
            lyyy = sum(nonNAidx)
            no_obs[jj] = lyyy
            no_obs_deflated[nn] = lyyy
            #scaling_idx[jj] = ss
   
            firm_match_idx[jj] = nn 
            
            
            for (kk in c(1:3)) {
              if (kk==1) tempidx = rep(TRUE, lyyy)
              if (kk==2) tempidx = c( rep(TRUE, floor(lyyy/2)), rep(FALSE, ceiling(lyyy/2))   )
              if (kk==3) tempidx = c( rep(FALSE, floor(lyyy/2)), rep(TRUE, ceiling(lyyy/2))   )
              
              sd_ye[nn,kk] = sd(yyy1[nonNAidx][tempidx], na.rm=T)
              sd_yf[nn,kk] = sd(yyy2[nonNAidx][tempidx], na.rm=T)
              sd_yef[nn,kk] = sd(yyy1[nonNAidx][tempidx]-yyy2[nonNAidx][tempidx], na.rm=T)
              
              skew_ye[nn,kk] = skewness(yyy1[nonNAidx][tempidx], na.rm=T)
              kurt_ye[nn,kk] = kurtosis(yyy1[nonNAidx][tempidx], na.rm=T)
              mad_ye[nn,kk] = mean(abs( yyy1[nonNAidx][tempidx]-median( yyy1[nonNAidx][tempidx],na.rm=T)),na.rm=T)  
              
   
              skew_yf[nn,kk] = skewness(yyy2[nonNAidx][tempidx], na.rm=T)
              kurt_yf[nn,kk] = kurtosis(yyy2[nonNAidx][tempidx], na.rm=T)
              
              skew_yef[nn,kk] = skewness((yyy1-yyy2)[nonNAidx][tempidx], na.rm=T)
              kurt_yef[nn,kk] = kurtosis((yyy1-yyy2)[nonNAidx][tempidx], na.rm=T)
             }
             
             
            nn=nn+1  
            
        }
        
        jjs = jjs+1
          
      }
       
    }
    
    if (jj %% round(no_firms/100) == 0)   print(jj/no_firms) 
    
  } 
  
  
    
  
  
  
  no_obs_deflated = no_obs_deflated[1:(nn-1)]
  firm_begin_deflated = firm_begin_deflated[1:(nn-1)]
  firm_end_deflated = firm_end_deflated[1:(nn-1)]
  ye_firm_deflated = ye_firm_deflated[1:firm_end_deflated[nn-1]]
  yf_firm_deflated = yf_firm_deflated[1:firm_end_deflated[nn-1]]
  ye_firm_deflated_mu = ye_firm_deflated_mu[1:firm_end_deflated[nn-1]]
  yf_firm_deflated_mu = yf_firm_deflated_mu[1:firm_end_deflated[nn-1]] 
  TS_index_firm_deflated = TS_index_firm_deflated[1:firm_end_deflated[nn-1]] 
  
  year_qtr_idx = year_qtr_idx[1:firm_end_deflated[nn-1]]
  firm_deflated_nn_idx= firm_deflated_nn_idx[1:firm_end_deflated[nn-1]]
  
  
  sd_estimate.firm= sd_estimate.firm[1:firm_end_deflated[nn-1]]
  marketvalue.firm= marketvalue.firm[1:firm_end_deflated[nn-1]]
  at.firm= at.firm[1:firm_end_deflated[nn-1]]
  sales.firm= sales.firm[1:firm_end_deflated[nn-1]]
  be.firm= be.firm[1:firm_end_deflated[nn-1]]
  size_cat.firm= size_cat.firm[1:firm_end_deflated[nn-1]]
  ffindustry.firm= ffindustry.firm[1:firm_end_deflated[nn-1]]
  firmage.firm=  firmage.firm[1:firm_end_deflated[nn-1]]
volatility.firm = volatility.firm[1:firm_end_deflated[nn-1]]
nocov.firm = nocov.firm[1:firm_end_deflated[nn-1]]
  
  
  firm_TS_data = list(trunc_idx_used=trunc_idx_used, 
                     truncate_rate=truncate_rate,
                     scaling_idx=scaling_idx,
                      no_obs_deflated=no_obs_deflated,                      
                      firm_begin_deflated = firm_begin_deflated,
                      firm_end_deflated = firm_end_deflated,
                      ye_firm_deflated = ye_firm_deflated,
                      yf_firm_deflated = yf_firm_deflated,
                      ye_firm_deflated_mu = ye_firm_deflated_mu,
                      yf_firm_deflated_mu = yf_firm_deflated_mu ,    
                      firm_match_idx=firm_match_idx,
                      sd_ye=sd_ye,
                     sd_yf=sd_yf,
                     sd_yef=sd_yef,
                     skew_ye=skew_ye,
                     kurt_ye=kurt_ye,
                      skew_yf=skew_yf,
                      skew_yef=skew_yef,
                      kurt_yf=kurt_yf,
                      kurt_yef=kurt_yef,
                     mad_ye=mad_ye,
                     no_obs=no_obs,
                     ye_scaled=ye_scaled,
                     yf_scaled=yf_scaled,
                      year_qtr_idx =  year_qtr_idx,
                      firm_deflated_nn_idx=firm_deflated_nn_idx,
                      TS_index_firm_deflated=TS_index_firm_deflated,  
                    sd_estimate.firm= sd_estimate.firm,
                    marketvalue.firm= marketvalue.firm,
                    at.firm= at.firm,
                    sales.firm= sales.firm,
                    be.firm= be.firm,
                    size_cat.firm= size_cat.firm,
                    ffindustry.firm= ffindustry.firm,
                    firmage.firm=  firmage.firm  ,
                    volatility.firm=volatility.firm,
                    nocov.firm = nocov.firm )
                    
 
  
  
  filename = fcn_filename_maker(scaleVset=scaleVset, option=option )
 


  save(firm_TS_data, file =  filename  )
 
  
}


 


