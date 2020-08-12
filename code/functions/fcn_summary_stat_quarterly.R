fcn_summary_stat_quarterly <- function( scaleVset, option) {  

  filename = fcn_filename_maker(scaleVset=scaleVset, option=option )
  load(file=filename)
 
  est = firm_TS_data
  
  
  tempidx = abs(firm_TS_data$ye_scaled + 0.0006614201)<0.00000001
  tempidx[is.na(tempidx)] = FALSE
  
  est$ye_scaled[ tempidx] = NA
  est$yf_scaled[  tempidx] = NA
  
  year_qtr_idx = fpe_year+qtr_index/4
  year_qtr_idx[is.na(est$ye_scaled)] = NA
  
  year_qtr_set = as.numeric(names(table(year_qtr_idx )))
  
  
  ye_stat = matrix( rep(NA,length(year_qtr_set )*13), nrow=13)
  yf_stat=ye_stat 
  yef_stat=ye_stat 
   
  
  jj=1
  for (ii in year_qtr_set) {
    
    
     tempidx = year_qtr_idx == ii 
     
     yyy1 = est$ye_scaled[tempidx]
     yyy2 = est$yf_scaled[tempidx]
     yyy3 = yyy1-yyy2
     
     zzz = yyy1
     ye_stat[,jj]  = c(mean(zzz,na.rm=T),sd(zzz,na.rm=T),skewness(zzz,na.rm=T),kurtosis(zzz,na.rm=T),quantile(zzz  ,c(0.05,0.25,0.5,0.75,0.95) ,na.rm=T)
     ,mean(zzz>0,na.rm=T),mean(zzz<0,na.rm=T),mean(zzz==0,na.rm=T), NA)
     
     zzz = yyy2
     yf_stat[,jj] = c(mean(zzz,na.rm=T),sd(zzz,na.rm=T),skewness(zzz,na.rm=T),kurtosis(zzz,na.rm=T),quantile(zzz  ,c(0.05,0.25,0.5,0.75,0.95) ,na.rm=T)
                      ,mean(zzz>0,na.rm=T),mean(zzz<0,na.rm=T),mean(zzz==0,na.rm=T),NA)
     
     zzz = yyy3
     yef_stat[,jj]  = c(mean(zzz,na.rm=T),sd(zzz,na.rm=T),skewness(zzz,na.rm=T),kurtosis(zzz,na.rm=T),quantile(zzz  ,c(0.05,0.25,0.5,0.75,0.95) ,na.rm=T)
                        ,mean(zzz>0,na.rm=T),mean(zzz<0,na.rm=T),mean(zzz==0,na.rm=T),NA)
     
     print(jj)
     jj=jj+1
  }
  
  qtrsumstat = list(ye_stat=ye_stat,yf_stat=yf_stat,yef_stat=yef_stat)
  
  return(qtrsumstat)
  
}
  

#  In terms of Table 1: Summary Statistics, Panel A, I was thinking of the following: 
#    it might make sense to compute these statistics separately for EVERY QUARTER 
#and then take the time-series average of these statistics. 
#The reason is that the current sample is weighted heavily 
#towards the end of the sample due to increase in coverage. 
#By doing the statistics period-by-period and then taking the average, 
#we give equal weight to every time period. Something like this is often done
#in the asset pricing literature (see Lewellen 2012 on the cross-section of expected returns
#, for example) because the distributions move so much over time & the number of firms in the cross-section changes substantially.
#So assigns 'quarter' labels to firms (198501 for firms that release the earnings
#in January, February, or March of 1985; 198502 for firms that release in April-June of 1985; 198503 
# for July-September 1985; 198505 for October-December 1985; 198601 for firms that release the earnings
#in January-March of 1986; and so forth) and then take statistics separately every quarter.#

#also, I'd report the following statistics:

#Mean
#SD
#5th percentile
#25th percentile
#50th percentile
#75th percentile
#95th percentile
#fraction positive
#fraction negative
#fraction zero##

#Min and max are always weird & thus uninformative. 
#The statistics should be the same for Panel B 
#but of course there the whole point is that every firm is "one observation," 
#so no need to change anything else.




