#'  Converte annotation format to long format
#'
#'  Convert annotation data.frame to a long format sequence when the length of the
#'  time windows is define by the cut
#'
#' @param data : a data.frame annotation
#' @param onset offset : time of the data where the conversion should start and stop
#' @param cut : lenght of the windoaws in second
#'
#' @result a data.frame
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
# convertor_long_cut <- function(data,onset,offset,cut = 0.100){
#
#   min.time = onset
#   max.time = offset
#   time.seq <- seq.default(onset,offset,cut)
#
#
#   listOfspeaker_type <- c("MAL", "CHI", "OCH", "FEM")
#   rez <- c()
#   for (type in listOfspeaker_type) {
#
#     vec <- c()
#     for (t in time.seq){
#
#       test = sum(t >= data$segment_onset & t <= data$segment_offset & data$speaker_type==type, na.rm = T)
#       if(is.na(test)){ test = 0 }
#
#       vec <- c(vec, test)
#
#     }
#
#     rez <- cbind(rez,  vec)
#   }
#
#   rez <- as.data.frame(rez)
#   names(rez) <- listOfspeaker_type
#   rez <- cbind(time.seq,rez)
#   return(rez)
# }



convertor_long_cut <- function(data,onset,offset,cut = 0.100){

  if(nrow(data)<1){
    return(data.frame("time.seq"=as.numeric(),
                      "MAL"=as.numeric(),
                      "CHI"=as.numeric(),
                      "OCH"=as.numeric(),
                      "FEM"=as.numeric()))
  }

  min.time = onset
  max.time = offset
  time.seq <- seq.default(onset,offset,cut)

  tmp.data <- data.frame(time.seq,"MAL"=0, "CHI"=0, "OCH"=0, "FEM"=0)

  for (row in 1:nrow(data)){
    segment_onset <- data[row,]$segment_onset
    segment_offset <- data[row,]$segment_offset
    speaker_type <- as.character(data[row,]$speaker_type)

    # print(data[row,1:7])
    # print(speaker_type)
    # print(speaker_type %in% c("MAL", "CHI", "OCH", "FEM"))
    if(speaker_type %in% c("MAL", "CHI", "OCH", "FEM")){
      # tmp.data[time.seq>=segment_onset & time.seq<=segment_offset,speaker_type] + 1
      d = tmp.data[time.seq>=segment_onset & time.seq<=segment_offset,][,speaker_type] + 1
      tmp.data[time.seq>=segment_onset & time.seq<=segment_offset,][,speaker_type] = d

    }

  }
  return(tmp.data)
}



