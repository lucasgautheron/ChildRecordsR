
library(reshape)
library(ggplot2)
library(GGally)
library(gridExtra)


#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'


# setClass("ratterCompCR")
# setGeneric("summary")
# setMethod(
#   "summary",
#   signature = "raterCompCR",
#   # c(object = "ratterCompCR"),
#   function(object){
#
#     ### General info
#
#     cat("### General summary ### \n\n")
#     cat("Data come from",object$variable$file,"csv ratters related files\n")
#     cat("they range from ",object$variable$range_from,"to",object$variable$range_to,"seconds\n" )
#     cat("lenght of the segmentation for reliability was set to ", object$variable$seg,"second\n")
#
#
#     ## reliability summary
#     # rat1 <- ratti$reliability$`Krippendorff's alpha`
#     # rat2 <- object$reliability$`Fleiss' Kappa for m Raters`
#
#     rat1 <- object$reliability[[1]]$est
#     rat2 <- object$reliability[[2]]$est
#     rat3 <- object$reliability[[3]]$est
#
#
#
#     cat("\n\n### Reliability indicators ###\n")
#     # cat("INFO : number of subect could be diffrent depending of the indactor and how it handle overlap\n\n")
#
#     print(rbind(rat1,rat2,rat3))
#     # cat("\n ### \n \n")
#     # print(rat2)
#
#
#   })
