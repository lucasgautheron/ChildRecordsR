

#' Overlap LENA function
#'
#' Open an annotation file and add 2 columns to find overlap according to the LENA-style overlap definition
#'
#' @param meta_row : a row from the meta containing all the info of the cvs file
#' @param ChildRecordings : a ChildRecordings class
#' @param use_data_table : use the data.table package to read the .csv annotation data (depending on the operating system and the number of threads used it can be 3 to 5 times faster than 'read.csv')
#' @param verbose : if TRUE information will be printed out in the console
#' @param threads the number of threads to run in parallel
#'
#' @return A data.frame with 2 added columns
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
#' raw_file <- LENA.overlap(CR$all.meta[1,], CR, use_data_table = TRUE,
#'                          verbose = TRUE, threads = parallel::detectCores())
#' head(raw_file)
#'
#' }

LENA.overlap <- function(meta_row,
                         ChildRecordings,
                         use_data_table = FALSE,
                         verbose = FALSE,
                         threads = 1) {

  if (verbose) start <- proc.time()

  data = file.opener(meta_row, ChildRecordings, use_data_table = use_data_table, threads = threads)

  if (nrow(data) == 0) {
    # print(paste("no row (i.e., no annotated speech) in ", path))
    return(data)
  }

  idx_nas = which(is.na(data$speaker_type))                                      # work around to deal with character string NA's without calling the '<Rinternals.h>' in Rcpp
  if (length(idx_nas) > 0) {                                                     # keep track of the NA's and then back-convert (the 'SPEECH' term won't be taken into account in the for-loop)
    data$speaker_type[idx_nas] = 'SPEECH'
  }

  dat_overl = LENA_overlap_loop(segment_onset_vec = data$segment_onset,
                                segment_offset_vec = data$segment_offset,
                                speaker_type_vec = data$speaker_type)

  if (length(idx_nas) > 0) {                                                      # back conversion of the NA's if they exist
    data$speaker_type[idx_nas] = NA_character_
  }

  data$LENA_OL_segment_onset <- dat_overl$LENA_OL_segment_onset                   # !!!!! the 'LENA_OL_segment_onset' column will be of type 'numeric' and not 'integer' (let me know if I have to change the data type)
  data$LENA_OL_segment_offset <- dat_overl$LENA_OL_segment_offset                 # !!!!! the 'LENA_OL_segment_offset' column will be of type 'numeric' and not 'integer' (let me know if I have to change the data type)

  if (verbose) {
    end_t = proc.time()
    cat('Time to compute the data object of the ChildRecordings:', round(((end_t['elapsed'] - start['elapsed']) / 60) %% 60, 4), 'minutes.\n')
  }

  return(data)
}


