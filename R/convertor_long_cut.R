
#'  Converts annotation format to long format
#'
#'  Convert annotation data.frame to a long format sequence where the length of the
#'  time window bins is defined by cut (in seconds)
#'
#' @param data : a data.frame annotation
#' @param onset : time of the data where the conversion should start
#' @param offset : time of the data where the conversion should stop
#' @param cut : length of the bins in millisecond
#'
#' @return  A data.frame in a long format version of the classic raw annotation file
#' 
#' @export
#'
#' @examples
#' 
#' \dontrun{
#' 
#' library(ChildRecordsR)
#' path = "/mnt/94707AA4707A8CAC/CNRS/corpus/vandam-daylong-demo"
#' CR = ChildRecordings(path)
#' raw_file <- file.opener(CR$all.meta[1,],CR, use_data_table = TRUE,
#'                         threads = parallel::detectCores())
#' long_file <- convertor_long_cut(raw_file, onset = 1, offset = 100000, cut = 1)
#' head(long_file)
#' colSums(long_file)
#' }

convertor_long_cut <- function(data,
                               onset,
                               offset,
                               cut = 0.100){
  min.time = onset
  max.time = offset
  time.seq <- seq.default(onset,offset,cut)
  data = data[data$segment_onset>=min.time & data$segment_offset<=max.time,]

  if(nrow(data)<1){
    return(data.frame("time.seq"=time.seq,
                      "MAL"=0,
                      "CHI"=0,
                      "OCH"=0,
                      "FEM"=0))
  }
  
  tmp.data = convertor_long_cut_loop(time_seq = time.seq,
                                     segment_onset_vec = data$segment_onset, 
                                     segment_offset_vec = data$segment_offset,
                                     speaker_type_vec = data$speaker_type)
  return(tmp.data)
}

