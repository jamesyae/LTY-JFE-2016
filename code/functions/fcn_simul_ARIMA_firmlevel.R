fcn_simul_ARIMA_firmlevel <- function(scaleVset, option , savefilename, arcoef_set,  sigmaa2_sigmae2_set, KK) {
  
  filename = fcn_filename_maker(scaleVset=scaleVset, option=option )
  load(file=filename)
   
  NN = length(firm_TS_data$firm_begin_deflated) # number of firms
  MM =  length(arcoef_set) # number of parameter sets for simulations
  KK # number of iteration
 
  artemp=  matrix(rep(NA, NN*KK*MM), ncol=MM )
  matemp=  artemp
  ar1temp=  artemp
  
  
  for (kk in seq(1,KK)) {
  
    for (mm in seq(1,MM)) {
    
      jj_start = 1
       
      while (jj_start <=NN  &  is.na(artemp[kk*NN]+matemp[kk*NN]+ar1temp[kk*NN])   ) 
      {
        for (jj in seq( jj_start , NN ))
        {  
          
          firm_idx_temp = firm_TS_data$firm_begin_deflated[jj]:firm_TS_data$firm_end_deflated[jj]
          TT = length(firm_idx_temp)
          
          arcoef = arcoef_set[mm]
         
          sigmaa2_sigmae2 = sigmaa2_sigmae2_set[mm]
          
          if ( sigmaa2_sigmae2==0) { macoef = 0}
            else
            {
          a=1;b=(1+arcoef^2)/arcoef + 1/sigmaa2_sigmae2/arcoef;c=1
          macoef = (-b+sqrt(b^2-4*a*c))/2/a
            }
            
          y1=  arima.sim(n = TT  , list(order = c(1,0,1),ar = arcoef, ma=  macoef )  ) 

            
          
          naidx = is.na(firm_TS_data$ye_firm_deflated[firm_idx_temp])
          y1 = as.numeric(y1)
          y1[naidx] = NA 
          
          
          
          erroroccur = FALSE
          tryCatch({
            m1= arima(y1,c(1,0,0), method = c("ML"),optim.control = list(maxit = 300000 )) ;
            m2= arima(y1,c(1,0,1),  method = c("ML"),optim.control = list(maxit = 300000 ))
          }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")
                               erroroccur = TRUE} )
            
          
          ar1temp[(kk-1)*NN+jj,mm] = m1$coef[1]              
          artemp[(kk-1)*NN+jj,mm] = m2$coef[1]
          matemp[(kk-1)*NN+jj,mm] = m2$coef[2] 
          
          
           
          
          
          if (erroroccur == TRUE)  jj=jj-1
          
        }
        
        jj_start = jj
      }
      
      
      firmsimul = list(ar1temp=ar1temp,artemp=artemp,matemp=matemp,arcoef_set=arcoef_set,sigmaa2_sigmae2_set=sigmaa2_sigmae2_set) 
      save(firmsimul, file = savefilename)
      
    }
    
  }
  

  
}
  