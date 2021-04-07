
#' Signal detection theory to compare two annotators
#'
#' Provide a SDT indicator to compare two rater and graph
#' @param raterData : a raterData class
#' @param raters : a vector containing two string with the names of the two raters
#' @param plot a boolean. If TRUE then the plot will be returned
#' @param summary a boolean. If TRUE then the summary statistics will be printed out in the console
#'
#' @return a Class to print a graph and return a list containing a confusion matrix class from caret package and a macro SDT indicator
#'
#' @importFrom caret confusionMatrix
#' @importFrom forcats fct_rev
#' @importFrom gridExtra grid.arrange
#' @import ggplot2
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
#'
#' # finding segments on wav file for designated rater
#'
#' raters <- c("its", "vtc")
#' search <- find.rating.segment(CR,""BN32_010007.mp3",range_from = 0, range_to = 5000000)
#' ratting1  = aggregate.rating(search, CR, 100, use_data_table = TRUE, threads = 2)
#' get.classification(ratting1, c("textgrid/ak","textgrid/mm"))
#'
#' }


get.classification <- function(raterData,
                               raters,
                               plot=TRUE,
                               summary=TRUE){

  levels = c("CHI","FEM","MAL","OCH","overlap","silence")
  rater1 <- raterData$rater[[raters[1]]]$long_file
  rater2 <- raterData$rater[[raters[2]]]$long_file

  cof_mat <- caret::confusionMatrix(
    factor(rater1$composit,levels =levels ),
    factor(rater2$composit,levels =levels),
    dnn = raters)
  conf_tab=cof_mat$table
  # Precision & recall confusion matrix graphs
  colsums=colSums(conf_tab)
  my_conf_tab=conf_tab
  for(i in 1:dim(conf_tab)[2]) my_conf_tab[,i]=my_conf_tab[,i]/colsums[i]

  prop_cat=data.frame(my_conf_tab*100) #generates precision because columns
  names(prop_cat)[1:2] <- raters
  prop_cat$id=paste(
    lapply(prop_cat[raters[1]], as.character)[[1]],
    lapply(prop_cat[raters[2]], as.character)[[1]]
  )
  colnames(prop_cat)[3]<-"pr"
  data.frame(conf_tab)->stall
  names(stall)[1:2] <- raters
  stall$id=paste(
    lapply(stall[raters[1]], as.character)[[1]],
    lapply(stall[raters[2]], as.character)[[1]]
  )
  stall=merge(stall,prop_cat[c("id","pr")])

  cof_mat[["precision"]]<- stall

  # repeat for recall
  rowsums=rowSums(conf_tab)
  my_conf_tab=conf_tab
  for(i in 1:dim(conf_tab)[2]) my_conf_tab[,i]=my_conf_tab[,i]/rowsums[i]

  prop_cat=data.frame(conf_tab/rowSums(conf_tab)*100)  #generates recall because rows
  names(prop_cat)[1:2] <- raters
  prop_cat$id=paste(
    lapply(prop_cat[raters[1]], as.character)[[1]],
    lapply(prop_cat[raters[2]], as.character)[[1]]
  )
  colnames(prop_cat)[3]<-"rec"
  data.frame(conf_tab)->stall
  names(stall)[1:2] <- raters
  stall$id=paste(
    lapply(stall[raters[1]], as.character)[[1]],
    lapply(stall[raters[2]], as.character)[[1]]
  )
  stall=merge(stall,prop_cat[c("id","rec")])
  cof_mat[["recall"]]<- stall

  tbl <- data.frame(t(cof_mat[["byClass"]]))

  macro.recall <- sum(tbl["Recall",],na.rm=T)/length(tbl["Recall",])
  macro.precision <- sum(tbl["Precision",],na.rm=T)/length(tbl["Precision",])
  macro.f1 <- sum(tbl["F1",],na.rm=T)/length(tbl["F1",])
  unweight <- c(macro.recall,macro.precision,macro.f1)


  macro.recall.w <- sum(tbl["Recall",]*colsums,na.rm=T)/sum(colsums)
  macro.precision.w <- sum(tbl["Precision",]*colsums,na.rm=T)/sum(colsums)
  macro.f1.w <- sum(tbl["F1",]*colsums,na.rm=T)/sum(colsums)
  weight <- c(macro.recall.w,macro.precision.w,macro.f1.w)

  type <- c("Recall","Precision","F1")
  macro <- data.frame(type, unweight, weight)

  value <- list(
    table = list("Confusion Matrix" = cof_mat$table,
                 "Recall Matrix" = cof_mat$recall,
                 "Precision Matrix" = cof_mat$precision),
    byClass = cof_mat$byClass,
    macro = macro,
    raters = raters
  )
  class(value)="SDT.rater"

  if(summary) print.SDT.rater(value)
  if(plot) plot(value)

  invisible(value)
}


#' @export

print.SDT.rater <- function(SDT.rater){

  cat("Confusion matrix : \n\n")
  print(SDT.rater$table$"Confusion Matrix")
  cat("\n\n")

  cat("STD by class : \n\n")
  print(t(SDT.rater$byClass))
  cat("\n\n")

  cat("STD macro indicators : \n\n")
  print(SDT.rater$macro)
}


setClass("SDT.rater")
setGeneric("plot")
setMethod("plot",signature = "SDT.rater",
          function(x){

            raters = x$raters

            prec = x$table$"Precision Matrix"
            prec[raters[1]] <- forcats::fct_rev(prec[[raters[1]]])

            recall = x$table$"Recall Matrix"
            recall[raters[1]] <- forcats::fct_rev(recall[[raters[1]]])

            prec_plot = ggplot2::ggplot(data = prec, mapping = ggplot2::aes(y = get(raters[1]), x= get(raters[2]))) +
              ggplot2::geom_tile(ggplot2::aes(fill= scales::rescale(pr)), colour = "white") +
              ggplot2::geom_text(ggplot2::aes(label = paste(round(pr),"%")), vjust = -1,size=4) +
              ggplot2::geom_text(ggplot2::aes(label = Freq), vjust = 1,size=4) +
              ggplot2::scale_fill_gradient(low = "white", high = "red", name = "Percentage") +
              ggplot2::theme(legend.position = "none") +
              ggplot2::xlab(raters[2]) + ggplot2::ylab(raters[1]) +
              ggplot2::ggtitle("Precision")+ ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))

            rec_plot = ggplot2::ggplot(data = recall, mapping = ggplot2::aes(y = get(raters[1]), x= get(raters[2]))) +
              ggplot2::geom_tile(ggplot2::aes(fill= scales::rescale(rec)), colour = "white") +
              ggplot2::geom_text(ggplot2::aes(label = paste(round(rec),"%")), vjust = -1,size=4) +
              ggplot2::geom_text(ggplot2::aes(label = Freq), vjust = 1,size=4) +
              ggplot2::scale_fill_gradient(low = "white", high = "red", name = "Percentage") +
              ggplot2::theme(legend.position = "none") +
              ggplot2::xlab(raters[2]) + ggplot2::ylab(raters[1]) +
              ggplot2::ggtitle("Recall")+ ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))

            gridExtra::grid.arrange(prec_plot,rec_plot)
          }
)

