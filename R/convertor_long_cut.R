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
#'
#'
convertor_long_cut <- function(data,onset,offset,cut = 0.100){
  
  min.time = onset
  max.time = offset
  time.seq <- seq.default(onset,offset,cut)
  
  
  listOfspeaker_type <- c("MAL", "CHI", "OCH", "FEM")
  rez <- c()
  for (type in listOfspeaker_type) {
    
    vec <- c()
    for (t in time.seq){
      
      test = sum(t >= data$segment_onset & t <= data$segment_offset & data$speaker_type==type, na.rm = T)
      if(is.na(test)){ test = 0 }
      
      vec <- c(vec, test)
      
    }
    
    rez <- cbind(rez,  vec)
  }
  
  rez <- as.data.frame(rez)
  names(rez) <- listOfspeaker_type
  rez <- cbind(time.seq,rez)
  return(rez)
}