fcn_data_treatment_step1_IBES <- function(dataset ) {  


# If  rescale="lagprc", EPS is scaled by lagged stock price
# If aggregateforecast="median", the aggregate forecast is the cross-sectional median of forecasts.
# If aggregateforecast="mean", the aggregate forecast is the cross-sectional mean of forecasts.
  
N_row = dim(dataset)[1] 
attach(dataset)  



##########################################################
##########################################################
##########################################################
##########   Data fix( missing ticker) #############
##########################################################
##########################################################
##########################################################

# dataset[ is.na(dataset$ticker), ] 
# sum( dataset$ticker==""  ,na.rm=T)
ticker_fixed = as.character( ticker )
ticker_fixed[ is.na(dataset$ticker)] = "ticker01"


ff=1  ## index of firms
jj=1 ## index of rows
firm_end = rep(NA,length(table(ticker_fixed)) )
firm_begin = rep(NA,length(table(ticker_fixed)) )

while (jj<=N_row)   
{  
  firm_begin[ff] = jj 
    
  while ( ticker_fixed[jj+1]==ticker_fixed[jj]    )
  {   
    jj=jj+1     
    if (jj==N_row) {break }
  }
  firm_end[ff]   = jj
  
  if (jj<N_row)  {
    jj = jj+1
    ff = ff+1 
  }
  
  if (jj==N_row)
  { 
    firm_end[ff]   = jj    
    break 
  }  
  
  if (jj %% round(N_row/1000) == 0) 
  {
    print(jj/N_row)
  }
} 
no_firms = ff   ### Total number of firms in data
firm_end=firm_end[1:ff] ### number of data rows per firm
firm_begin=firm_begin[1:ff]  ### number of data rows per firm
no_eps = firm_end-firm_begin+1  ### number of data rows per firm
#sum(no_eps)==N_row ## should be TRUE

if (sum(no_eps)==N_row & length(table(ticker_fixed))==no_firms)
{cat("No error")}
if (!(sum(no_eps)==N_row & length(table(ticker_fixed))==no_firms))
{cat("Error")}
    
dataset <- transform(dataset, ticker =  ticker_fixed )

 

#fpe_number =  (as.numeric(substr(strptime( fpe , "%d %b %y")  ,1,4))) +
#             (as.numeric(substr(strptime( fpe, "%d %b %y")  ,6,7))-1)/12+
#             (as.numeric(substr(strptime( fpe, "%d %b %y")  ,9,10)))/12/30

#rdq_number =  (as.numeric(substr( rdq, 6,9))) +
#             (as.numeric(substr(strptime( rdq, "%d %b %y")  ,6,7))-1)/12+
#             (as.numeric(substr(strptime( rdq, "%d %b %y")  ,9,10)))/12/30

#repdats_number =  (as.numeric(substr( repdats, 6,9))) +
#                (as.numeric(substr(strptime( repdats, "%d %b %y")  ,6,7))-1)/12+
#                (as.numeric(substr(strptime( repdats, "%d %b %y")  ,9,10)))/12/30
 
qtr_index = ceiling(as.numeric(substr(strptime( fpedats, "%d %b %Y")  ,6,7))/3) 
month_index =  as.numeric(substr(strptime( fpedats, "%d %b %Y")  ,6,7)) 
fpe_year = (as.numeric(substr(strptime( fpedats , "%d %b %Y")  ,1,4)))

TS_index = rep(NA, N_row)
primary_idx= rep(FALSE, N_row)

for (jj in seq( 1,no_firms))
{   
  firm_idx_temp = firm_begin[jj]:firm_end[jj] 
  
  TS_index[firm_begin[jj]] = 1
  
  if (firm_begin[jj] < firm_end[jj] )
  {
    
    Eseason_idx = month_index[firm_idx_temp] %% 3 
    Eseason_type = as.numeric(names(table(Eseason_idx  ))[which.max(Eseason_idx)])
        
    TS_index[ (firm_begin[jj]+1):firm_end[jj] ] =
          1 + 4*(fpe_year[ (firm_begin[jj]+1):firm_end[jj] ]-fpe_year[firm_begin[jj]]) +
          (qtr_index[ (firm_begin[jj]+1):firm_end[jj] ]-qtr_index[firm_begin[jj]]) 
   
    if (sum(diff(TS_index[   firm_idx_temp  ])==0)==0) {
      primary_idx[ firm_idx_temp ] = TRUE
    }
    
    if (sum(diff(TS_index[   firm_idx_temp  ])==0)>0) {
      print('multi-obs in the same quarter')  
      multiE_TS_index_set = which(  table( TS_index[ firm_idx_temp ] )>1  )
      primary_idx[ firm_idx_temp ][ !(TS_index[ firm_idx_temp ]  %in%  multiE_TS_index_set) ] = TRUE
       
      for (kkk in multiE_TS_index_set )
      {
         one_multiE_TS_index_idx = TS_index[ firm_idx_temp ] == kkk           
         Eseason_idx_temp = Eseason_idx
         Eseason_idx_temp[!one_multiE_TS_index_idx] = NA
         
         temp_idx = length(Eseason_idx) +1-which.min( rev(abs( Eseason_idx_temp -Eseason_type )) )
         primary_idx[ firm_idx_temp ][temp_idx] = TRUE
      }     
    }  
    
    
    
  }
}
 
save( no_firms,firm_end,firm_begin,no_eps,qtr_index,
     fpe_year,  ticker_fixed, TS_index, primary_idx ,file = "new_variables_IBES_common.Rdata")

 
detach(dataset)  
}


