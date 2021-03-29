#' Search function on Childrecordings class
#'
#' Search function in Childrecordings class
#' This search function will help you to provide file name and time windows
#' for common coding in raters
#'
#' @param ChildRecordings : a ChildRecordings class
#' @param filename : a wav file name to consider
#' @param annotators : an optional argument providing le annotators to look at
#' @param range_from and range_to : an optional time windows for the search
#'
#' @return A data.frame containing commun annoation segment for each annotator
#'
#' @examples
#' library(ChildRecordsR)
#' path = "/mnt/94707AA4707A8CAC/CNRS/corpus/namibia-data/"
#' CR = ChildRecordings(path)
#'
#' # if no time windows is specified, this function will only return at table for all the know raters
#' # To work, all annotators must have at least one common annotation segment.
#' find.rating.segment(CR, "aiku/namibie_aiku_20160715_1.wav")
#'
#' # However, if a time windows is provided, this function will find all the data that
#' # overlaps with the time windows provided.
#' # For instance, you can shift the window it will give you the same result
#' Wav_file_name = "aiku/namibie_aiku_20160715_1.wav"
#' t1 = 27180000
#' t2 = 27000000
#' t3 = 27240000
#' find.rating.segment(CR, Wav_file_name, range_from = t1, range_to = t3)
#' find.rating.segment(CR, Wav_file_name, range_from = t1, range_to = t3)
#' find.rating.segment(CR, Wav_file_name, range_from = t2, range_to = t3)
#'
#' # finding segments on wav file for designated rater
#' raters <-  c("textgrid_ak", "textgrid_mm", "textgrid_m1")
#' find.rating.segment(CR,"Wav_file_name", raters)
#'
#' # finding segments on wav file for the designated windows in second and rater
#' search <- find.rating.segment(CR,"Wav_file_name", raters, range_from = t1, range_to = t2)
#'
#' # try to analyse a larger number of file
#' wave_file <- unique(CR$all.meta$filename) # get all the wav files
#' raters <- c("textgrid_ak", "textgrid_mm", "textgrid_m1") # Define raters you are interested in
#'
#' # bind all the results
#' search2 <- data.frame()
#' for (file in wave_file[1:10]){
#'   print(file)
#'   search2 <- rbind(search2, find.rating.segment(CR, file, raters)) # could take some time
#' }
#'
#' head(search2)
#'
#'
#'
#'
#'





find.rating.segment <- function(ChildRecordings,recording_filename,annotators=NULL,range_from=NULL,range_to=NULL){

  if(!is(ChildRecordings, "ChildRecordings")){
    print(paste( substitute(ChildRecordings), "is not a ChildRecordings class"))
    return(NULL)
  }

  ### table
  tbl <- ChildRecordings$all.meta
  tbl <- tbl[tbl$recording_filename==recording_filename,]
  tbl$true_onset <- tbl$time_seek + tbl$range_onset
  tbl$true_offset <- tbl$time_seek + tbl$range_offset

  ### select annotators
  if(is.null(annotators)){
    annotators <- unique(tbl$set)
    n.annotator = length(annotators)
  } else {
    tbl <- tbl[tbl$set %in% annotators,]
    n.annotator = length(annotators)
  }

  if (nrow(tbl)==0){
    return(NULL)
  }

  ### Range windows selection, if mentioned
  if(!is.null(range_from) & !is.null(range_to)){
    tbl <- tbl[true_time_seg_finder(range_from,range_to,tbl),]
  }else{
    range_from= min(tbl$true_onset)
    range_to= max(tbl$true_offset)
  }

  ### return if empty
  if (nrow(tbl)==0){
    return(NULL)
  }

  commun.annoatation <-c()
  for(row in 1:nrow(tbl)){
    ol.with <-c()
    for(row2 in 1:nrow(tbl)){

      t = DescTools::Overlap(
        x=c(tbl[row,]$true_onset,tbl[row,]$true_offset),
        y=c(tbl[row2,]$true_onset,tbl[row2,]$true_offset)
      )
      # print(t)
      if(t>0){
        ol.with <- c(ol.with,as.character(tbl[row2,]$set))
      }
    }
    commun.annoatation<- c(commun.annoatation,length(unique(ol.with)))
  }

  # select commun segment possible
  tbl$commun.annoatation <- commun.annoatation
  tbl <- tbl[tbl$commun.annoatation==n.annotator,]
  if (nrow(tbl)==0){
    return(NULL)
  }

  # Define time segment

  range_from <- max( c(min(tbl$true_onset),range_from ))
  range_to <- min( c(max(tbl$true_offset),range_to ))
  time =  seq(range_from-1000, range_to+1000, 1000)
  time.line = data.frame(time = time, count = 0)

  for (row in 1:nrow(tbl)){
    time.line[time>= tbl[row,]$true_onset & time<= tbl[row,]$true_offset,]$count=  time.line[time>= tbl[row,]$true_onset & time<= tbl[row,]$true_offset,]$count +1
  }
  time.line$segment <- ifelse(time.line$count==n.annotator,1,0 )
  time.line[time==range_from-1000 | time==range_to+1000,]$segment = 0 # add border

  # find segments
  time.code <- time.line[which(diff(time.line$segment)!=0),]$time
  time.code <- as.data.frame(matrix(time.code,ncol=2,byrow = T))
  names(time.code) <- c("true_onset","true_offset")

  ### Format data to be returned
  rez <- data.frame()
  for( row in 1:nrow(time.code)) {
    true_onset= time.code[row,]$true_onset
    true_offset = time.code[row,]$true_offset
    tmp <- true_time_seg_finder(true_onset,true_offset,tbl)

    tmp <- data.frame( recording_filename=tbl[tmp,]$recording_filename,
                       set= tbl[tmp,]$set,
                       annotation_filename = tbl[tmp,]$annotation_filename,
                       stringsAsFactors = F)


    tmp$true_onset <- true_onset+1000
    tmp$true_offset <- true_offset

    rez <- rbind(rez,tmp)

  }


  rez


}
