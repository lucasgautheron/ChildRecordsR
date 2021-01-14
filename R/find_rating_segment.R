#' Search function on Childrecordings class
#'
#' Search function in Childrecordings class
#' This search function will help you to provide file name and time windows
#' for common coding in raters
#'
#' @param ChildRecordings : a ChildRecordings class
#' @param filename : a wav file name to look
#' @param annotators : an optional argument providing le annotators to look at
#' @param range_from and range_to : an optional time windows for the search
#'
#' @return A data.frame containing commun annoation segment for each annotator
#'
#' @examples
#' library(ChildRecordsR)
#' path = "A_childrecord_data_path"
#' CR = ChildRecordings(path)
#' CR
#' # if no time windows is specified, this function will only return at table for all the know raters
#' # All the rater need to ratter any segment find
#' find.ratting.segment(CR,"Wav_file_name")
#'
#' # However, if a time windows is provided, this function will find all the data that
#' # overlaps with the time windows provided.
#' # For instance, you can shift the window it will give you the same result
#' find.ratting.segment(CR,"Wav_file_name",range_from = t1, range_to = t3)
#' find.ratting.segment(CR,"Wav_file_name",range_from = t1, range_to = t3)
#' find.ratting.segment(CR,"Wav_file_name",range_from = t2, range_to = t3)
#'
#' # finding segments on wav file for designated rater
#' raters <- c("Coder1","Coder2","Coder3")
#' find.ratting.segment(CR,"Wav_file_name",raters)
#'
#' # finding segments on wav file for the designated windows in second and rater
#' search <- find.ratting.segment(CR,"Wav_file_name", raters, range_from = t1, range_to = t2)
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
#'
#'
#'
#'
#'
#'
#'








find.rating.segment <- function(ChildRecordings,filename,annotators=NULL,range_from=NULL,range_to=NULL){

  if(!is(ChildRecordings, "ChildRecordings")){
    print(paste( substitute(ChildRecordings), "is not a ChildRecordings class retrun null result"))
    return(NULL)
  }




  tbl <- ChildRecordings$all.meta
  tbl <- tbl[tbl$filename==filename,]

  ### True time from time seek

  tbl$true_onset <- tbl$time_seek + tbl$range_onset
  tbl$true_offset <- tbl$time_seek + tbl$range_offset

  ### Ranger windows selection
  if(!is.null(range_from) & !is.null(range_to)){
    tbl <- tbl[true_time_seg_finder(range_from,range_to,tbl),]
  }

  ### Rater selection if mention
  if (is.null(annotators)){
    annotators <- unique(tbl$set)
  } else {
    tbl <- tbl[tbl$set %in% annotators,]
  }

  if (nrow(tbl)==0){
    return(NULL)
  }


  ### Find segment of ratter common segment
  find_time_code_data<-data.frame(time_seg= seq( min(tbl$true_onset)-1, max(tbl$true_offset),1))
  annotator_nbr <- c()
  for (time in find_time_code_data$time_seg){
    annotator_nbr <- c(annotator_nbr,sum(time>=tbl$true_onset & time<=tbl$true_offset & tbl$true_onset!=tbl$true_offset))
  }

  find_time_code_data$annotator_nbr <- annotator_nbr
  find_time_code_data$segments <- as.numeric(find_time_code_data$annotator_nbr==length(annotators))

  # if no ratting segment find return null
  if( sum(find_time_code_data$segments) == 0 ){return(NULL)}

  time_code <- find_time_code_data$time_seg[which(diff(find_time_code_data$segments)!=0)]

  # Adding an ending time if necessary
  if(length(time_code)%%2!=0){time_code<-c(time_code,max(tbl$true_offset))}

  time_code <- as.data.frame(matrix(time_code,ncol=2,byrow = T))
  names(time_code) <- c("true_onset","true_offset")


  ### Building return format
  rez <- data.frame()
  for( row in 1:nrow(time_code)) {
    true_onset= time_code[row,]$true_onset
    true_offset = time_code[row,]$true_offset
    tmp <- true_time_seg_finder(true_onset,true_offset,tbl)

    tmp <- data.frame( filename=tbl[tmp,]$filename,
                       set= tbl[tmp,]$set,
                       annotation_filename = tbl[tmp,]$annotation_filename,
                       stringsAsFactors = F)


    tmp$true_onset <- true_onset+1
    tmp$true_offset <- true_offset

    rez <- rbind(rez,tmp)

  }
  rez
}
