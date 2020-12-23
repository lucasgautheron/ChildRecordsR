
#' file importation from annotator
#'
#' Import all the file from a specific annotators
#'
#' @param ChildRecordings : a ChildRecordings class
#' @param set.type : a string containing th name of the annotator present in the "set" column meta
#'
#' @return : a data.frame
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
