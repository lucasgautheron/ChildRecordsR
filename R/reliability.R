#' Reliability analyse
#'
#' Provide a reliability analyse returnin Krippen alpha, fleiss kappa and gwet AC1
#'
#' @param raterData : a raterData class
#'
#' @return Soon
#'
#' @examples
#' library(ChildRecordsR)
#' path = "A_childrecord_data_path"
#' CR = ChildRecordings(path)
#'
#' # finding segments on wav file for designated rater
#' raters <- c("Coder1","Coder2","Coder3")
#' search <- find.ratting.segment(CR,"Wav_file_name", raters, range_from = t1, range_to = t2)
#' ratting1  = aggregate.rating(search, CR, 0.1)
#' reliability(ratting1)
#'
#' # try to analyse a larger number of file
#' wave_file <- unique(CR$all.meta$filename) # get all the wav files
#' ratters <- c("Coder1","Coder2","Coder3") # Define raters you are interested in
#'
#'
#' # bind all the results
#' search2 <- data.frame()
#' for (file in wave_file[1:10]){
#'   print(file)
#'   search2 <- rbind(search2, find.ratting.segment(CR, file, ratters)) # could take some time
#' }
#' ratting2  = aggregate.rating(search2, CR, 0.1)
#' reliability(ratting2)
#'
#'
#'



# analyse <- function(x,summary) UseMethod("analyse", x)

reliability <- function(raterData, summary=TRUE){
  ratting.type = c("CHI","OCH","FEM","MAL","composit")

  ratting_by_type <- list()
  for(type in ratting.type){

    a = lapply(raterData[["rater"]], "[", 2)
    b = lapply(a,"[[",1)
    data <- data.frame(lapply(b,"[",type))
    names(data) <- raterData$args$ratersID
    ratting_by_type[[type]] <- data
  }


  reliability <- list()
  for (type in 1:length(ratting_by_type)){


    a = irrCAC::krippen.alpha.raw(ratting_by_type[[type]])
    k = irrCAC::fleiss.kappa.raw(ratting_by_type[[type]])
    ac1 = irrCAC::gwet.ac1.raw(ratting_by_type[[type]])
    reliability[[names(ratting_by_type[type])]]<-rbind(a$est,k$est,ac1$est)

  }




  value <- list(type  =  ratting_by_type,
                reliability = reliability
  )
  class(value)="analyse"


  if(summary) print.analyse(value)
  invisible(value)
}


print.analyse <- function(analyse){

  types <- names(analyse$reliability)
  cat("Reliability indicator by type","\n\n")
  for (type in types){
    cat("#", type,"\n\n" )
    print(analyse$reliability[[type]])
    cat("\n\n" )

  }



}




