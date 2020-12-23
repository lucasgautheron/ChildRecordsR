#'
#' Openning a data file in childrecording class
#'
#' Open a csv file from a ChildRecordings class.
#' The function provide transformation on data such as create a true segment_onset
#' and segment_offset based on meta data. Will also provide POSIXct time info if
#' strat_time of the recording is provided in the meta
#'
#' @param meta_row : a row from the meta containing all the info of the cvs file
#' @param ChildRecordings : a ChildRecordings class
#' @return A data.frame
#'
#'
#'
#'

file.openner <- function(meta_row,ChildRecordings){
  path =  paste0(ChildRecordings$path,"annotations/",meta_row$annotation_filename)
  temps.data <- read.csv(path)

  if (nrow(temps.data) == 0) {
    # print(paste("no row (i.e., no annotated speech) in ", path))
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
