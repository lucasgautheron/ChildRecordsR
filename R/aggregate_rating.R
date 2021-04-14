
#' aggregation of annnotations data
#'
#' Base on the result of an find.ratting.segment return or similar data.frame
#' the function will extract data from annotation file in a raterData Class
#'
#' the data will be organize un raw annotation format and a long segmented format
#'
#' @param ChildRecordings : a ChildRecordings class
#' @param data : find.ratting.segment return or similar data.frame
#' @param cut : time size in millisecond for the unit segment
#' @param verbose : if TRUE information will be printed out in the console
#' @param use_data_table : use the data.table package to read the .csv annotation data (depending on the operating system and the number of threads used it can be 3 to 5 times faster than 'read.csv')
#' @param threads the number of threads to run in parallel
#'
#' @export
#' 
#' @return A raterData class containing with original format and long format for every annotators.
#' 
#' @importFrom methods is
#'
#' @examples
#' 
#' \dontrun{
#' 
#' library(ChildRecordsR)
#' path = "/mnt/94707AA4707A8CAC/CNRS/corpus/vandam-daylong-demo"
#' CR = ChildRecordings(path)
#'
#' # if no time windows is specified, this function will only return at table for all the know raters
#' # All the rater need to ratter any segment find
#' 
#' search = find.rating.segment(CR, "BN32_010007.mp3")
#' rez = aggregate.rating(search, CR, cut=100, verbose=T, use_data_table = TRUE, threads = 2)
#'
#' }

aggregate.rating <- function(data, 
                             ChildRecordings, 
                             cut=100, 
                             verbose=FALSE,
                             use_data_table = FALSE,
                             threads = 1){
  
  if (verbose) start <- proc.time()

  if(!methods::is(ChildRecordings, "ChildRecordings")){
    print(paste( substitute(ChildRecordings), "is not a ChildRecordings class"))
    return(NULL)
  }
  # attach(data)
  data <- data[order(data$recording_filename,data$set,data$true_onset),]
  # detach(data)
  all.meta <- ChildRecordings$all.meta
  ratersID <- as.character(unique(data$set))
  Nrow <- 1

  rater = parallel::mclapply(1:length(ratersID), function(idx) {
    
    rat = ratersID[idx]
    tmp.data <- data[data$set==rat,]
    
    raw_files = long_files = list()

    for (idx_row in 1:nrow(tmp.data)) {
      row <- tmp.data[idx_row,]
      annotation_filename <- row$annotation_filename
      true_onset <- row$true_onset
      true_offset <- row$true_offset
      
      meta.row <- all.meta[all.meta$annotation_filename==annotation_filename & all.meta$set==rat,]
      raw_file <- file.opener(meta.row, ChildRecordings, use_data_table = use_data_table, threads = 1)      # use by default a single thread because I already run mclapply-parallel
      long_file <- convertor_long_cut(raw_file, true_onset, true_offset, cut=cut)                           # this takes the longest depending on the input data size
      long_file <- data_to_OneHotEnc(long_file)
      
      raw_file <- raw_file[raw_file$segment_onset>=true_onset & raw_file$segment_offset<=true_offset,]
      
      raw_files[[idx_row]] = raw_file
      long_files[[idx_row]] = long_file
    }
    
    if (use_data_table) {
      raw_files = data.table::rbindlist(raw_files)
      long_files = data.table::rbindlist(long_files)
    }
    else {
      raw_files = do.call(rbind, raw_files)
      long_files = do.call(rbind, long_files)
    }
    
    tmp_rater = list(raw_file = raw_files, long_file = long_files)
    tmp_rater
  }, mc.cores = threads)
  
  names(rater) = ratersID

  value <- list(
    rater= rater,
    args = list(ratersID = ratersID,
                cut = cut,
                search = data)
  )
  attr(value, "class") <- "raterData"
  
  if (verbose) {
    end_t = proc.time()
    cat('Time to compute the', nrow(data), 'rows of the input data:', round(((end_t['elapsed'] - start['elapsed']) / 60) %% 60, 4), 'minutes.\n')
  }

  print.raterData(value)
  invisible(value)
}


#' @export

print.raterData <- function(raterData){

  ### Print results
  recording.length <- sum(raterData$args$search$true_offset -raterData$args$search$true_onset)

  cat("number of annotators", length(raterData$args$ratersID),"\n")
  cat("length of recording annotation for each annotator ", recording.length/length(raterData$args$ratersID),"ms or ", recording.length/length(raterData$args$ratersID)/3600000, "hours\n\n")

}

