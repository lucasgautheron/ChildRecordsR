#' aggregation of annnotations data
#'
#' Base on the result of an find.ratting.segment return or similar data.frame
#' the function will extract data from annotation file in a raterData Class
#'
#' the data will be organize un raw annotation format and a long segmented format
#'

#' @param ChildRecordings : a ChildRecordings class
#' @param data : find.ratting.segment return or similar data.frame
#' @param cut : time size in second for the unit segment
#'
#' @return Soon
#'
#'
#'
#'



aggregate.rating <- function(ChildRecordings, data, cut=0.100){

  if(!is(ChildRecordings, "ChildRecordings")){
    print(paste( substitute(ChildRecordings), "is not a ChildRecordings class retrun null result"))
    return(NULL)
  }



  attach(data)
  data <- data[order(filename,set,true_onset),]
  detach(data)
  all.meta <- ChildRecordings$all.meta
  ratersID <- as.character(unique(data$set))

  rater <- list()

  for(rat in ratersID){
    tmp.data <- data[data$set==rat,]
    # print(tmp.data)
    raw_files <- data.frame()
    long_files <- data.frame()


    for (row in 1:nrow(tmp.data)){
      row <- tmp.data[row,]
      annotation_filename <- row$annotation_filename
      true_onset <- row$true_onset
      true_offset <- row$true_offset
      # print(row)

      meta.row <- all.meta[all.meta$annotation_filename==annotation_filename,]


      raw_file <- file.openner(meta.row,ChildRecordings)
      long_file <- convertor_long_cut(raw_file,true_onset,true_offset,cut=cut)
      long_file <- data_to_OneHotEnc(long_file)

      raw_files<-rbind(raw_files,raw_file)
      long_files <- rbind(long_files,long_file)

    }
    # print(head(raw_files))
    # print(head(long_files))
    rater[[rat]]$raw_file <- raw_files
    rater[[rat]]$long_file <- long_files

  }


  value <- list(
    rater= rater,
    args = list(ratersID = ratersID,
                cut = cut,
                search = data)
  )
  attr(value, "class") <- "raterData"


  print.raterData(value)
  invisible(value)
}


print.raterData <- function(raterData){

  ### Print results
  recording.length <- sum(raterData$args$search$true_offset -raterData$args$search$true_onset)


  cat("number of annotators", length(raterData$args$ratersID),"\n")
  cat("length of reccording annotation", recording.length,"seconds or ", recording.length/3600, "hours\n")
  cat("Reccord span ", recording.length/length(raterData$args$ratersID),"seconds or ", recording.length/length(raterData$args$ratersID)/3600, "hours\n\n")

}



