fcn_summary_stat <- function( scaleVset, option) {  

  
  filename = fcn_filename_maker(scaleVset=scaleVset, option=option )
  load(file=filename)
  
  est = firm_TS_data
  
  print('mean    median   sd    min   max')
  
  summary_set= cbind(est$ye_scaled, est$yf_scaled, est$ye_scaled-est$yf_scaled  )
  summary_set=matrix(cbind(apply(summary_set,2,mean,na.rm=T),apply(summary_set,2,median,na.rm=T),
               apply(summary_set,2,sd,na.rm=T),apply(summary_set,2,min,na.rm=T),
               apply(summary_set,2,max,na.rm=T)),ncol=5)
  ps=cbind(round(summary_set[,c(1,2)]*10^4, 3), round(summary_set[,c(3,4,5)], 4))
  print(matrix( sprintf('%1$3.3f & %2$3.3f & %3$3.4f & %4$3.3f & %5$3.3f',ps[,1],ps[,2],ps[,3],ps[,4],ps[,5]  ), nrow=3 ))
  
  print(format(sum(est$no_obs ,na.rm=T), big.mark = "," ))
  
  
  summary_set=matrix(c(est$no_obs) ,ncol=1)
  summary_set=matrix(cbind(apply(summary_set,2,mean,na.rm=T),apply(summary_set,2,median,na.rm=T),
                           apply(summary_set,2,sd,na.rm=T),apply(summary_set,2,min,na.rm=T),
                           apply(summary_set,2,max,na.rm=T), sum(!is.na(no_obs))),ncol=6)
  ps= format(round(summary_set , 0), big.mark = "," )
  print( sprintf('%s & %s & %s & %s & %s & %s',ps[,1],ps[,2],ps[,3],ps[,4],ps[,5],ps[,6]  ) )
           
  
  print('mean    median   sd    min   max')
  

  
  
  summary_set= cbind(est$sd_ye[,1],   est$skew_ye[,1] , est$kurt_ye[,1] )
  summary_set=matrix(cbind(apply(summary_set,2,mean,na.rm=T),apply(summary_set,2,median,na.rm=T),
                           apply(summary_set,2,sd,na.rm=T),apply(summary_set,2,min,na.rm=T),
                           apply(summary_set,2,max,na.rm=T)),ncol=5)
  ps=cbind(round(summary_set[,c(1,2)]*10^4, 3), round(summary_set[,c(3,4,5)], 4))
  print(matrix( sprintf('%1$3.3f & %2$3.3f & %3$3.4f & %4$3.3f & %5$3.3f',ps[,1],ps[,2],ps[,3],ps[,4],ps[,5]  ), nrow=3 ))
  
  print(format(sum(est$no_obs ,na.rm=T), big.mark = "," ))
  
   
  
  
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




