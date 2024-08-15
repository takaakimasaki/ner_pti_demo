rm(list=ls())
gc()

#####################################################################################################################
# #set your path here. 
username <- Sys.info()[7]
if(username=="WB495141" | username=="wb495141"){
  path <- paste0("C:/Users/",username,"/OneDrive - WBG/poverty/niger")
  dir_input <- paste0(path,"/input")
  dir_temp <- paste0(path,"/temp")
  dir_output <- "./output"
  dir_figure <- "./figure"
  dir_code <- "./code"
  dir_input_gl <- paste0("C:/Users/",username,"/OneDrive - WBG/poverty/data/global/")
}
