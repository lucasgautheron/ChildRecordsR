
library(reshape)
library(ggplot2)
library(GGally)
library(gridExtra)






analyse <- function(x) UseMethod("analyse", x)

analyse.raterData <- function(raterData){
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
    
    
    a = krippen.alpha.raw(ratting_by_type[[type]])
    k = fleiss.kappa.raw(ratting_by_type[[type]])
    ac1 = gwet.ac1.raw(ratting_by_type[[type]])
    reliability[[names(ratting_by_type[type])]]<-rbind(a$est,k$est,ac1$est)
    
  }
  
  # if(print.rez){print(reliability)}
  return(
    list(
      type  =  ratting_by_type,
      reliability = reliability)
  )
  
}
