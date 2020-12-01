





file.openner <- function(meta_row,ChildRecordings){
  path =  paste0(ChildRecordings$path,"annotations/",meta_row$annotation_filename)
  temps.data <- read.csv(path)
  
  if (nrow(temps.data) == 0) {
    print(paste("no row (i.e., no annotated speech) in ", path))
    return(temps.data)
  }
  
  # Child metadata 
  temps.data$child_id <- meta_row$child_id
  temps.data$date_iso <- meta_row$date_iso
  
  # Adjusting segment in regard to time seek
  temps.data$segment_onset <- temps.data$segment_onset + meta_row$time_seek
  temps.data$segment_offset <- temps.data$segment_offset + meta_row$time_seek
  
  # Building time variable 
  if (is.na(meta_row$start_time)){
    # print(paste("no start_time in ", path))
    
    temps.data$POSIXct_time_onset <- NA
    temps.data$POSIXct_time_offset <- NA
    
    temps.data$year <- NA
    temps.data$month <- NA
    temps.data$day <- NA
    
    
  } else {
    temps.data$POSIXct_time_onset <-
      as.POSIXct(meta_row$date_iso) +
      period_to_seconds(hm(meta_row$start_time)) +
      temps.data$segment_onset
    temps.data$POSIXct_time_offset <-
      as.POSIXct(meta_row$date_iso) +
      period_to_seconds(hm(meta_row$start_time)) +
      temps.data$segment_offset
    
    temps.data$year <- year(temps.data$POSIXct_time_onset)
    temps.data$month <- month(temps.data$POSIXct_time_onset)
    temps.data$day <- day(temps.data$POSIXct_time_onset)
    
  }
  
  temps.data$filename <- meta_row$filename
  
  return(temps.data)
  
} 





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






data_to_OneHotEnc <- function(data){
  rez <- c()
  for (row in 1:nrow(data)){
    # print(data[row,])
    
    if (rowSums(data[row,-1])==0){ 
      trez= "silence"
    }else{
      if(rowSums(data[row,-1])>1) {
        # trez = NA
        trez = "overlap"
      }else{
        if(data[row,]$CHI>=1) trez = "CHI"
        if(data[row,]$FEM>=1) trez = "FEM"
        if(data[row,]$MAL>=1) trez = "MAL"
        if(data[row,]$OCH>=1) trez = "OCH"
      }
    }
    # row =row +1
    rez <- c(rez,trez)
  }
  
  data$composit <- rez
  data
}



true_time_seg_finder <- function(range_from,range_to,all.meta.table){
  
  larger = all.meta.table$true_onset<range_from & all.meta.table$true_offset>range_to
  displaceleft =  all.meta.table$true_onset < range_from &  all.meta.table$true_offset <= range_to & all.meta.table$true_offset > range_from
  displaceright = all.meta.table$true_onset >= range_from &  all.meta.table$true_offset > range_to & all.meta.table$true_onset < range_to
  smaller = all.meta.table$true_onset>=range_from & all.meta.table$true_offset<=range_to
  # same = all.meta.table$true_onset==range_from & all.meta.table$true_offset==range_to
  
  
  # print(larger)
  # print(displaceleft)
  # print(displaceright)
  # print(smaller)
  return(larger | displaceleft | displaceright | smaller)
}
