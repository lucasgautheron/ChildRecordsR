
#' file importation from annotator
#'
#' Import all the file from a specific annotators
#'
#' @param ChildRecordings : a ChildRecordings class
#' @param set.type : a string containing th name of the annotator present in the "set" column meta
#' @param LENA.OL : add the LENA overlap method to handle onset and offset
#'
#' @return  A data.frame with all the aggregated data
#'
#' @examples
#' library(ChildRecordsR)
#' path = "A_childrecord_data_path"
#' CR = ChildRecordings(path)
#' rez = extractDataCR( "Coder_Name", CR)
#' # With LENA overlap methode
#' rez = extractDataCR("Coder_Name", CR, LENA.OL = T)
#'
extractDataCR <- function(set.type,ChildRecordings,LENA.OL = F,verbose=T) {

  if(!is(ChildRecordings, "ChildRecordings")){
    print("ChildRecordings value is not a ChildRecordings class retrun null result")
    return(NULL)
  }

  path = ChildRecordings$path
  all.meta = ChildRecordings$all.meta
  all.meta = all.meta[grepl(set.type, all.meta$set), ]

  ### Data extraction loop
  data <- data.frame()
  start <- Sys.time()
  for (row in 1:nrow(all.meta)) {


    ### aggregate data


    if (LENA.OL){
      temps.data <- LENA.overlap(all.meta[row, ],ChildRecordings)
    }else{
      temps.data <- file.openner(all.meta[row, ],ChildRecordings)
    }

    # bind data
    data <- rbind(data, temps.data)

    ### Progress bar
    if(verbose){
      t <- Sys.time()
      extra <- nchar('||100%')
      width <- options(width = 80)$width
      step <- round(row / nrow(all.meta) * (width - extra))
      step.time <- as.numeric(difftime(t, start, units = "secs")/row)
      est.duration = step.time*nrow(all.meta)/60
      est.remain=step.time*(nrow(all.meta)-row)/60
      text <- sprintf('|%s%s|% 3s%% time by step : %ss estimate duration : %sm remain : %sm', strrep('=', step),
                      strrep(' ', width - step - extra), round(row / nrow(all.meta) * 100),
                      round(step.time) ,
                      round(est.duration),
                      round(est.remain)
      )
      cat(text,"\n")
    }


  }

  data

}
