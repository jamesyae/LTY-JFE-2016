fcn_VARMA11_LTY_random_data_BT<- function(scaleVset, option ,   fixedeffect) {
  
 
  
  filename = fcn_filename_maker(scaleVset=scaleVset, option=option )
  load(file=filename)
  
  NN = length(firm_TS_data$firm_begin_deflated)
  
  sample_idx = sample.int(NN, size = NN, replace = TRUE)
  
  if (fixedeffect){
    pdataset = list( y=firm_TS_data$ye_firm_deflated_mu , 
                     begin_idx =firm_TS_data$firm_begin_deflated[sample_idx], end_idx = firm_TS_data$firm_end_deflated[sample_idx])
    ytsim1set = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  ) 
    pdataset = list( y=firm_TS_data$yf_firm_deflated_mu , 
                     begin_idx =firm_TS_data$firm_begin_deflated[sample_idx], end_idx = firm_TS_data$firm_end_deflated[sample_idx])
    ytsim2set = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  ) 
  }
  else {
    pdataset = list( y=firm_TS_data$ye_firm_deflated  , 
                     begin_idx =firm_TS_data$firm_begin_deflated[sample_idx], end_idx = firm_TS_data$firm_end_deflated[sample_idx])
    ytsim1set = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  ) 
    pdataset = list( y=firm_TS_data$yf_firm_deflated  , 
                     begin_idx =firm_TS_data$firm_begin_deflated[sample_idx], end_idx = firm_TS_data$firm_end_deflated[sample_idx])
    ytsim2set = fcn_pooled_TS_make(pdataset, initial_NA=100 , inbetween_NA=20  ) 
  }
  
  
  yt = matrix(rbind(ytsim1set,ytsim2set),nrow=2) 
  
 return(yt)
  
}