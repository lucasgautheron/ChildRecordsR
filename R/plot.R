
library(reshape)
library(ggplot2)


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

library(GGally)
library(gridExtra)
setClass("raterCompCR")
setGeneric("plot")
setMethod(
  "plot",signature = "raterCompCR",
  
  function(x){
    data <- x$rating_by_comp
    compo.names <- names(data)
    
    for ( compo in 1:length(data)){
      
      a <- ggpairs(data[[compo]][complete.cases(data[[compo]]),-1])
      
      data.tmp <- melt(data[[compo]][complete.cases(data[[compo]]),],id="time.seq")
      b  <- ggplot(data.tmp,aes(x=time.seq,y=value))+
        # geom_line(alpha=0.5)+
        geom_ribbon(aes(fill=variable, ymax=pmax(value,0), ymin=0), alpha=0.5)+
        # geom_density_ridges(alpha=0.6, stat="binline", bins=1)+
        theme(legend.position = c(0.2, 0.8), 
              legend.text = element_text(size = 10),
              legend.title = element_blank())
      
      # print(a)
      # print(b)
      grid.arrange(
        ggmatrix_gtable(a),
        b,
        nrow=1,
        top = compo.names[compo]
      )
      
      
    }
  }
)

