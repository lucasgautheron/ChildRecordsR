
#' file importation from annotator
#'
#' Import all the file from a specific annotators
#'
#' @param ChildRecordings : a ChildRecordings class
#' @param set.type : a string containing th name of the annotator present in the "set" column meta
#' @param LENA.OL : add the LENA overlap method. The overlap method will correct onset and offset when speech overlap by removing part that overlap.
#'
#' @return  A data.frame with all the aggregated data
#'
#' @examples
#' library(ChildRecordsR)
#' path = "/mnt/94707AA4707A8CAC/CNRS/namibia-data/"
#' CR = ChildRecordings(path)
#' rez = extractDataCR( "textgrid_m1", CR, verbose = F )
#' head(rez)
#' # With LENA overlap method
#' rez = extractDataCR("textgrid_m1", CR, LENA.OL = T, verbose = F)
#' head(rez)
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
  rez.list <- vector("list", nrow(all.meta))
  for (row in 1:nrow(all.meta)) {


    ### aggregate data


    # if (LENA.OL){
    #   temps.data <- LENA.overlap(all.meta[row, ],ChildRecordings)
    # }else{
    #   temps.data <- file.openner(all.meta[row, ],ChildRecordings)
    # }
    #
    # # bind data
    # data <- rbind(data, temps.data)

    if (LENA.OL){
      rez.list[[row]] <- LENA.overlap(all.meta[row, ],ChildRecordings)
    }else{
      rez.list[[row]] <- file.openner(all.meta[row, ],ChildRecordings)
    }



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

  data <- do.call("rbind", rez.list)

  data

}
