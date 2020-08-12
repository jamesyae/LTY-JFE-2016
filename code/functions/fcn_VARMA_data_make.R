fcn_VARMA_data_make <- function(scaleVset, option, fixedeffect) {  
  
filename = fcn_filename_maker(scaleVset=scaleVset, option=option )
load(file=filename)
 
if (fixedeffect){
  pdataset = list( y=firm_TS_data$ye_firm_deflated_mu , 
                   begin_idx =firm_TS_data$firm_begin_deflated, end_idx = firm_TS_data$firm_end_deflated)
  yyy1 = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
  pdataset = list( y=firm_TS_data$yf_firm_deflated_mu , 
                   begin_idx =firm_TS_data$firm_begin_deflated, end_idx = firm_TS_data$firm_end_deflated)
  yyy2 = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
}
else {
  pdataset = list( y=firm_TS_data$ye_firm_deflated  , 
                   begin_idx =firm_TS_data$firm_begin_deflated, end_idx = firm_TS_data$firm_end_deflated)
  yyy1 = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
  pdataset = list( y=firm_TS_data$yf_firm_deflated  , 
                   begin_idx =firm_TS_data$firm_begin_deflated, end_idx = firm_TS_data$firm_end_deflated)
  yyy2 = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  )
}

yt = matrix(rbind(yyy1,yyy2),nrow=2) 


return(yt)

}