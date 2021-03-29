#'
#' Overlap LENA function
#'
#' Open an annotation file and add 2 columns to find overlap according to the LENA-style overlap definition
#'
#' @param meta_row : a row from the meta containing all the info of the cvs file
#' @param ChildRecordings : a ChildRecordings class
#' @return A data.frame with 2 added columns
#' 
#' @examples
#' library(ChildRecordsR)
#' path = "/mnt/94707AA4707A8CAC/CNRS/corpus/namibia-data/"
#' CR = ChildRecordings(path)
#' raw_file <- LENA.overlap(CR$all.meta[1,],CR)
#' head(raw_file)
#'
#'
#'





LENA.overlap <- function(meta_row,ChildRecordings){

  data = file.opener(meta_row,ChildRecordings)
  if (nrow(data) == 0) {
    # print(paste("no row (i.e., no annotated speech) in ", path))
    return(data)
  }

  data$LENA_OL_segment_onset <- data$segment_onset
  data$LENA_OL_segment_offset <- data$segment_offset
  # print(data)

  for (row in 1:nrow(data)){
    speaker_type <- data[row,]$speaker_type
    segment_onset <- data[row,]$LENA_OL_segment_onset
    segment_offset <- data[row,]$LENA_OL_segment_offset

    if (speaker_type!= "SPEECH" | is.na(speaker_type)){

      ### overlap left
      # find left overlap
      l.test <- data$LENA_OL_segment_offset>segment_onset & data$LENA_OL_segment_offset<segment_offset & data$LENA_OL_segment_onset<=segment_onset
      if(sum(l.test)>0){
        # Change row overlap
        data[l.test,]$LENA_OL_segment_offset <- segment_onset
        # change evaluated row
        segment_onset <- max(data[l.test,]$segment_offset)
      }

      ### overlap right
      # find left overlap
      r.test <- data$LENA_OL_segment_onset<segment_offset & data$LENA_OL_segment_onset>segment_onset & data$LENA_OL_segment_offset>=segment_offset
      if(sum(r.test)>0){
        # Change rows overlap
        data[r.test,]$LENA_OL_segment_onset <- segment_offset
        # change evaluated row
        segment_offset <- max(data[r.test,]$segment_onset)
      }

      # modification of lena row
      segment_onset -> data[row,]$LENA_OL_segment_onset
      segment_offset -> data[row,]$LENA_OL_segment_offset


      ### Print results
      # if(sum(r.test)>0 | sum(l.test)>0){
      # print("######################")
      # print(paste("row :",row,"; test :", sum(l.test+r.test)))
      # print(data[row,c(1,2,5,20,21)])
      # print(data[l.test | r.test,c(1,2,5,20,21)])
      # }

    }
  }
  return(data)
}
