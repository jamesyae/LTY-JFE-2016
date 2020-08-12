fcn_filename_maker<- function(scaleVset=scaleVset, option=option ) {

cc0 = ""
for (ii in c(1:length(scaleVset))) cc0 = paste(cc0,scaleVset[ii],sep="")

cc1 = ""
if (option$ye_trunc) cc1 = paste(cc1,"ye",sep="")
if (option$yf_trunc) cc1 = paste(cc1,"yf",sep="")
if (option$yef_trunc) cc1 = paste(cc1,"yef",sep="")
cc1 = paste(cc1,substr(option$truncate,1,4),sep="_")
cc1 = paste(cc1,substr(as.character(option$truncate_crt*10000),1,4),sep="_")

filename = sprintf("TS_firm_data_%s_%sbp.Rdata",cc0, cc1)

return(filename)
}