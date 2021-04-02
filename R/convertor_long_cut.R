#'  Converts annotation format to long format
#'
#'  Convert annotation data.frame to a long format sequence where the length of the
#'  time window bins is defined by cut (in seconds)
#'
#' @param data : a data.frame annotation
#' @param onset offset : time of the data where the conversion should start and stop
#' @param cut : length of the bins in millisecond
#'
#' @return  A data.frame in a long format version of the classic raw annotation file
#'
#' @examples
#' library(ChildRecordsR)
#' path = "/mnt/94707AA4707A8CAC/CNRS/corpus/vandam-daylong-demo"
#' CR = ChildRecordings(path)
#' raw_file <- file.opener(CR$all.meta[1,],CR)
#' long_file <- convertor_long_cut(raw_file, onset = 1, offset = 500, cut = 1)
#' head(long_file)
#'
#'
#'
#'
#'
#'




convertor_long_cut <- function(data,onset,offset,cut = 0.100){
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

  tmp.data <- data.frame(time.seq,"MAL"=0, "CHI"=0, "OCH"=0, "FEM"=0)

  for (row in 1:nrow(data)){
    segment_onset <- data$segment_onset[row]
    segment_offset <- data$segment_offset[row]
    speaker_type <- as.character(data$speaker_type[row])

    if(speaker_type %in% c("MAL", "CHI", "OCH", "FEM")){
      tmp.data[time.seq>=segment_onset & time.seq<=segment_offset,speaker_type] =
        tmp.data[time.seq>=segment_onset & time.seq<=segment_offset,speaker_type] + 1
    }

  }
  return(tmp.data)
}



