
library(lubridate)
library(irr)
library(irrCAC)



####################################################
## import CSV files from a child recoding project ##
####################################################
#' 
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
extractDataCR <- function(ChildRecordings,set.type) {
  
  if(!is(ChildRecordings, "ChildRecordings")){
    print("ChildRecordings value is not a ChildRecordings class retrun null result")
    return(NULL)
  }
  
  path = ChildRecordings$path
  all.meta = ChildRecordings$all.meta
  all.meta = all.meta[grepl(set.type, all.meta$set), ]
  
  ### Data extraction loop 
  data <- data.frame()
  for (row in 1:nrow(all.meta)) {
    temps.data <- file.openner(all.meta[row, ],ChildRecordings)
    
    # bind data
    data <- rbind(data, temps.data)
    
  }
  
  data
  
}