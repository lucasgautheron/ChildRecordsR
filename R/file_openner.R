

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