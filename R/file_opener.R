#'
#' Opening a data file in ChildRecordingsData class
#'
#' Open a csv file from a ChildRecordingsData class.
#' The function provides transformation of data such as creating a true segment_onset
#' and segment_offset based on meta data. It will also provide POSIXct time info if
#' start_time of the recording is provided in the meta
#'
#' @param meta_row : a row from the meta containing all the info of the cvs file
#' @param ChildRecordings : a ChildRecordingsData class
#' @return A data.frame
#'
#' @examples
#' library(ChildRecordsR)
#' path = "/mnt/94707AA4707A8CAC/CNRS/corpus/vandam-daylong-demo"
#' CR = ChildRecordings(path)
#' raw_file <- file.opener(CR$all.meta[1,],CR)
#' head(raw_file)
#'

file.opener <- function(meta_row,ChildRecordings){
  path =  paste0(ChildRecordings$path,"annotations/",meta_row$set,"/converted/",meta_row$annotation_filename)
  temps.data <- read.csv(path)

  if (nrow(temps.data) == 0) {
    # print(paste("no row (i.e., no annotated speech) in ", path))

    tmp = data.frame(child_id=NA,date_iso=NA,POSIXct_time_onset=NA,POSIXct_time_offset=NA,year=NA,month=NA,day=NA)
    tmp = tmp[!is.na(tmp$child_id),]
    temps.data = cbind(temps.data,tmp)
    return(temps.data)
  }

  # Child metadata
  temps.data$date_iso <- meta_row$date_iso
  temps.data$child_id <- meta_row$child_id

  # Recording metadata
  temps.data$range_offset <- meta_row$range_offset
  temps.data$range_onset <- meta_row$range_onset

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
      lubridate::period_to_seconds(lubridate::hm(meta_row$start_time)) +
      temps.data$segment_onset
    temps.data$POSIXct_time_offset <-
      as.POSIXct(meta_row$date_iso) +
      lubridate::period_to_seconds(lubridate::hm(meta_row$start_time)) +
      temps.data$segment_offset

    temps.data$year <- lubridate::year(temps.data$POSIXct_time_onset)
    temps.data$month <- lubridate::month(temps.data$POSIXct_time_onset)
    temps.data$day <- lubridate::day(temps.data$POSIXct_time_onset)

  }

  temps.data$filename <- meta_row$filename

  return(temps.data)

}
