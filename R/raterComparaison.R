

#' Compare Annotator reliability
#'
#' Provide indication of quality for each annotator.
#' Following classical test theory, each annotator is removed from the data and a reliability indicator is provided for the annotation as a whole.
#' Removing a good annotator should decrease the indicator.
#' Additionally, a mean of precision, recall and F-score are provided by rater.
#'
#' @param raterData : a raterData class
#' @param verbose : if TRUE information will be printed out in the console
#' @param threads the number of threads to run in parallel
#'
#' @return an object of class 'raterComp'
#' 
#' @export
#' 
#' @importFrom irrCAC krippen.alpha.raw fleiss.kappa.raw gwet.ac1.raw
#' @importFrom stringr str_remove str_split
#' @importFrom gridExtra grid.arrange
#' @importFrom methods is
#' @import ggplot2
#'
#' @examples
#' 
#' \dontrun{
#' 
#' library(ChildRecordsR)
#' path = "/mnt/94707AA4707A8CAC/CNRS/corpus/vandam-daylong-demo"
#' CR = ChildRecordings(path)
#'
#' # Add a dummy anotation for the example
#' 
#' New.annotations(row.meta = CR$all.meta[2,], ChildRecordings = CR, time.sd = 1500,
#'                 change.cat.prob = 0.10, new.name = "vtc_BAD")
#'
#' # if no time windows is specified, this function will only return at table for all the know raters
#' # All the rater need to ratter any segment find
#' 
#' search = find.rating.segment(CR, "BN32_010007.mp3")
#' rez = aggregate.rating(search, CR, cut=100, verbose=T, use_data_table = TRUE, threads = 2)
#'
#' comparaison = compare.rating(rez, verbose = TRUE, threads = 5)
#' plot(comparaison)
#' 
#' }

compare.rating <- function(raterData, 
                           verbose = FALSE, 
                           threads = 1,
                           reference = NULL){

  if (verbose) start <- proc.time()
  
  if(!methods::is(raterData, "raterData")){
    print(paste( substitute(raterData), "is not a raterData class"))
    return(NULL)
  }

  raters <- raterData$args$ratersID
  global.reliability <- get.reliability(raterData,summary=F, threads = threads)
  global.reliability <- global.reliability$reliability[["composit"]]
 
  raters <- raters[!raters %in% reference]

  rater.result = parallel::mclapply(1:length(raters), function(idx_r) {
    
    rater = raters[idx_r]
    
    if (is.null(reference)) {
        comp.raters <- raters[!raters %in% rater]
    } else {
        comp.raters <- c(reference)
    }

    ## CTT data build
    if (is.null(reference)) {
      ctt.data<- data.frame(row.names=1:length(raterData[["rater"]][[rater]]$long_file$composit))
    } else {
      ctt.data<- data.frame(raterData[["rater"]][[rater]]$long_file$composit))

    }
    sdt.list <- list()

    for(comp.rater in comp.raters){

      ratting = raterData[["rater"]][[comp.rater]]$long_file$composit
      ctt.data <- cbind(ctt.data,data.frame(ratting))
      tmp<-get.classification(raterData,c(rater,comp.rater),plot=F,summary=F)
      # print(tmp)
      sdt.list[[comp.rater]]<-tmp
    }

    names(ctt.data) <- comp.raters
    a = irrCAC::krippen.alpha.raw(ctt.data)
    k = irrCAC::fleiss.kappa.raw(ctt.data)
    ac1 = irrCAC::gwet.ac1.raw(ctt.data)

    substract.reliability<-rbind(a$est, k$est, ac1$est)
    iter_rater <- list(substract.reliability = substract.reliability, sdt.list = sdt.list)
    iter_rater
  }, mc.cores = threads)
  
  names(rater.result) = raters

  ###### Build print dataframe
  ### CTT
  ctt <- data.frame()
  
  for (rater in raters){
    ### Global
    tmp <- global.reliability[,c("coeff.val","conf.int")]
    names(tmp) <- paste(names(tmp),"before")
    coeff.name=global.reliability[,c("coeff.name")]
    tmp<-cbind(coeff.name,tmp)

    ### after removing rater
    #if (reference != rater) {
      tmp2 <- rater.result[[rater]][["substract.reliability"]][,c("coeff.val","conf.int")]
      names(tmp2) <- paste(names(tmp2),"after")
      ctt.rez <- cbind(rater,tmp,tmp2)
      ctt.rez$coeff.difference <- ctt.rez$"coeff.val before" - ctt.rez$"coeff.val after"
      ctt <-rbind(ctt,ctt.rez)
    #}
  }
  
  ### SDT
  sdt <- data.frame()
  for (rater in raters){
    if (is.null(reference)) {
        comp.raters <- raters[!raters %in% rater]
    } else {
        comp.raters <- c(reference)
    }
    rater.sdt <- matrix(0,ncol=2,nrow=3)
    # print(rater.sdt)
    for (comp.rater in comp.raters){
      tmp <- rater.result[[rater]][["sdt.list"]][[comp.rater]]$macro
      # tmp <- as.matrix(tmp)
      # print(tmp[,2:3])
      rater.sdt = rater.sdt + tmp[,2:3]
      indic <-tmp[,1]
    }
    rater.sdt <- cbind(rater,indic,rater.sdt/length(comp.raters))
    sdt <-rbind(sdt,rater.sdt)
  }

  ### Print results
  recording.length <- sum(raterData$args$search$true_offset -raterData$args$search$true_onset)

  # cat("number of annotators", length(raters),"\n")
  # cat("length of reccording annotation", recording.length,"seconds or ", recording.length/3600, "hours\n")
  # cat("Record span ", recording.length/length(raters),"seconds or ", recording.length/length(raters)/3600, "hours\n\n")
  # print(sdt)
  # print(ctt)
  
  value <- list(global.reliability=global.reliability,
                rater.result=rater.result,
                ctt = ctt,
                sdt=sdt,
                args = list(raters = raters,
                            recording.length = recording.length,
                            raterData = deparse(substitute(raterData))))
  if (verbose) {
    end_t = proc.time()
    cat('Time to compute the', length(raters), 'ratersID:', round(((end_t['elapsed'] - start['elapsed']) / 60) %% 60, 4), 'minutes.\n')
  }

  class(value) <- "raterComp"
  print.raterComp(value)
  invisible(value)
}


#' @export

print.raterComp <- function(raterComp){
  raters = raterComp$args$raters
  nRaters = length(raters)
  recording.length = raterComp$args$recording.length

  cat("number of annotators", nRaters,"\n")
  cat("length of reccording annotation", recording.length,"seconds or ", recording.length/3600, "hours\n")
  cat("Record span ", recording.length/nRaters,"seconds or ", recording.length/nRaters/3600, "hours\n\n")

  ctt = raterComp$ctt
  ctt[,c(3,5,7)] <- round(ctt[,c(3,5,7)],3)
  sdt = raterComp$sdt
  sdt[,c(3,4)] <- round(sdt[,c(3,4)],3)

  for (rater in raters){

    cat("### Annotator",rater,"###\n\n")
    tmp.ctt <- ctt[ctt$rater==rater,]
    rownames(tmp.ctt) <- tmp.ctt[,2]
    print(tmp.ctt[,c(-1,-2)])

    cat("\n")
    tmp.sdt <- sdt[sdt$rater==rater,]
    rownames(tmp.sdt) <- tmp.sdt[,2]
    print(tmp.sdt[,c(-1,-2)])
    cat("\n")
  }
}


setClass("raterComp")
setGeneric("plot")
setMethod("plot",signature = "raterComp",
          function(x, plots){
            print('selection')
            print(plots)
            ggplots <- list(ctt = NULL, sdt = NULL)
            # CTT
            if ('ctt' %in% plots) {
              ctt <- x$ctt
              ctt$`conf.int after` <- as.character(ctt$`conf.int after`)
              ctt$`conf.int after` <- stringr::str_remove(ctt$`conf.int after`,"\\(")
              ctt$`conf.int after` <- stringr::str_remove(ctt$`conf.int after`,"\\)")
              tmp <- as.data.frame(stringr::str_split(ctt$`conf.int after`,",", n = Inf, simplify = T),stringsAsFactors = F)
              tmp <- as.data.frame(sapply(tmp,as.numeric))
              names(tmp) <- c("conf.inf","conf.sup")
              ctt <- cbind(ctt,tmp)

              ggplots$ctt <- ggplot2::ggplot(data = ctt,ggplot2::aes(x=rater,y=`coeff.val after`,color=coeff.name))+
                ggplot2::geom_point( position=ggplot2::position_dodge(width=0.3))+
                ggplot2::geom_errorbar(mapping = ggplot2::aes(ymin = conf.inf , ymax = conf.sup),width=0.1,position=ggplot2::position_dodge(width=0.3))+
                ggplot2::ylim(0,1)+
                ggplot2::ggtitle("Reliability")
             }
            
            if ('sdt' %in% plots) {

              # SDT
              sdt <- x$sdt

              ggplots$sdt <- ggplot2::ggplot(sdt,ggplot2::aes(x=rater,y=weight,color=indic))+
                ggplot2::geom_point( position=ggplot2::position_dodge(width=0.3))+
                ggplot2::ylim(0,1)+ 
                ggplot2::ggtitle("Mean Weighted Signal Detection Theory after rater retraction")
             }
            
             ggplots <- list.clean(ggplots, fun = is.null)
             ggplots
             grid.arrange(ggplots$ctt)
          }
)

