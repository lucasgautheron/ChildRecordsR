
#' Reliability analyse
#'
#' Provide a reliability analyse return in Krippen alpha, fleiss kappa and gwet AC1
#'
#' @param raterData : a raterData class
#' @param summary a boolean. If TRUE then the Reliability indicator will be printed out in the console
#' @param threads the number of threads to run in parallel
#'
#' @return an object of class analyse
#' 
#' @export
#' 
#' @importFrom irrCAC krippen.alpha.raw fleiss.kappa.raw gwet.ac1.raw
#' @importFrom data.table data.table
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
#' get.reliability(rez, summary = TRUE, threads = 5)
#'
#' }

get.reliability <- function(raterData, 
                            summary=TRUE,
                            threads = 1){
  
  ratting.type = c("CHI","OCH","FEM","MAL","composit")
  ratting_by_type <- list()
  
  for(type in ratting.type){
    a = lapply(raterData[["rater"]], "[", 2)
    b = lapply(a,"[[",1)
    
    # data <- data.frame(lapply(b, "[", type))
    nams = names(b)
    data = lapply(1:length(nams), function(x) {
      iter_dat = b[[nams[x]]]
      iter_col = iter_dat[[type]]
      iter_col
    })
    names(data) <- raterData$args$ratersID
    data = data.table::data.table(do.call(cbind, data))
    ratting_by_type[[type]] <- data
  }                                                                                # this code snippet just subsets the data  (modified to allow processing of both a data.frame or a data.table)

  reliability = parallel::mclapply(1:length(ratting_by_type), function(type) {
    a = irrCAC::krippen.alpha.raw(ratting_by_type[[type]])
    k = irrCAC::fleiss.kappa.raw(ratting_by_type[[type]])
    ac1 = irrCAC::gwet.ac1.raw(ratting_by_type[[type]])
    iter_out = rbind(a$est, k$est, ac1$est)
    iter_out
  }, mc.cores = threads)
  
  names(reliability) = names(ratting_by_type)

  value <- list(type = ratting_by_type,
                reliability = reliability)
  
  class(value)="analyse"

  if(summary) print.analyse(value)
  invisible(value)
}


#' @export

print.analyse <- function(analyse){

  types <- names(analyse$reliability)
  cat("Reliability indicator by type","\n\n")
  for (type in types){
    cat("#", type,"\n\n" )
    print(analyse$reliability[[type]])
    cat("\n\n" )
  }
}

