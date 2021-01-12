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
#' @examples
#' library(ChildRecordsR)
#' path = "A_childrecord_data_path"
#' CR = ChildRecordings(path)
#'
#' # finding segments on wav file for designated rater
#' raters <- c("Coder1","Coder2","Coder3")
#' search <- find.ratting.segment(CR,"Wav_file_name", raters, range_from = t1, range_to = t2)
#' ratting1  = aggregate.rating(search, CR, 0.1)
#'
#' # try to analyse a larger number of file
#' wave_file <- unique(CR$all.meta$filename) # get all the wav files
#' ratters <- c("Coder1","Coder2","Coder3") # Define raters you are interested in
#'
#' # bind all the results
#' search2 <- data.frame()
#' for (file in wave_file[1:10]){
#'   print(file)
#'   search2 <- rbind(search2, find.ratting.segment(CR, file, ratters)) # could take some time
#' }
#' ratting2  = aggregate.rating(search2, CR, 0.1)
#'
#'



aggregate.rating <- function(data, ChildRecordings, cut=0.100,verbose=T){

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

  ### init progress bar
  start <- Sys.time()
  Nrow <- 1

  for(rat in ratersID){
    tmp.data <- data[data$set==rat,]
    raw_files <- data.frame()
    long_files <- data.frame()


    for (row in 1:nrow(tmp.data)){
      row <- tmp.data[row,]
      annotation_filename <- row$annotation_filename
      true_onset <- row$true_onset
      true_offset <- row$true_offset



      meta.row <- all.meta[all.meta$annotation_filename==annotation_filename,]
      raw_file <- file.openner(meta.row,ChildRecordings)
      long_file <- convertor_long_cut(raw_file,true_onset,true_offset,cut=cut)
      long_file <- data_to_OneHotEnc(long_file)

      raw_files<-rbind(raw_files,raw_file[raw_file$segment_onset>=true_onset & raw_file$segment_offset<=true_offset,])
      long_files <- rbind(long_files,long_file)


      ### Progress bar

      if(verbose){
        t <- Sys.time()
        extra <- nchar('||100%')
        width <- options(width = 80)$width
        step <- round(Nrow / nrow(data) * (width - extra))
        step.time <- as.numeric(difftime(t, start, units = "secs")/Nrow)
        est.duration = step.time*nrow(data)/60
        est.remain=step.time*(nrow(data)-Nrow)/60
        text <- sprintf('|%s%s|% 3s%% time by step : %ss estimate duration : %sm remain : %sm', strrep('=', step),
                        strrep(' ', width - step - extra), round(Nrow / nrow(data) * 100),
                        round(step.time) ,
                        round(est.duration),
                        round(est.remain))
        cat(text,"\n")
        Nrow = Nrow + 1
      }
      ###



    }

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



