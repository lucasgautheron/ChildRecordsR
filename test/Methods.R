

library(reshape)
library(ggplot2)
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





setClass("ratterCompCR")
setGeneric("summary")
setMethod(
  "summary",
  signature = "raterCompCR",
  # c(object = "ratterCompCR"),
  function(object){
    
    ### General info
    
    cat("### General summary ### \n\n")
    cat("Data come from",object$variable$file,"csv ratters related files\n")
    cat("they range from ",object$variable$range_from,"to",object$variable$range_to,"seconds\n" )
    cat("lenght of the segmentation for reliability was set to ", object$variable$seg,"second\n")
    
    
    ## reliability summary
    # rat1 <- ratti$reliability$`Krippendorff's alpha`
    # rat2 <- object$reliability$`Fleiss' Kappa for m Raters`
    
    rat1 <- object$reliability[[1]]$est
    rat2 <- object$reliability[[2]]$est
    rat3 <- object$reliability[[3]]$est
    
    
    
    cat("\n\n### Reliability indicators ###\n")
    # cat("INFO : number of subect could be diffrent depending of the indactor and how it handle overlap\n\n")
    
    print(rbind(rat1,rat2,rat3))
    # cat("\n ### \n \n")
    # print(rat2)
    
    
  })



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




ratterComparaison <- function(ratterData){
  raters <- ratterData$args$ratersID
  global.reliability <- analyse(ratterData)
  global.reliability <- global.reliability$reliability[["composit"]]
  
  rater.result <- list()
  
  
  for (rater in raters){
    tmp.data=data.frame()
    comp.raters <- raters[!raters %in% rater]
    
    
    ## CTT data build
    ctt.data<- data.frame(row.names=1:length(ratterData[["rater"]][[rater]]$long_file$composit))
    sdt.list <- list()
    
    for(comp.rater in comp.raters){
      
      ratting = ratterData[["rater"]][[comp.rater]]$long_file$composit
      
      ctt.data <- cbind(ctt.data,data.frame(ratting))
      
      tmp<-SDT.raterData(ratterData,c(rater,comp.rater))
      # print(tmp)
      sdt.list[[comp.rater]]<-tmp
    }
    
    names(ctt.data) <- comp.raters
    a = krippen.alpha.raw(ctt.data)
    k = fleiss.kappa.raw(ctt.data)
    ac1 = gwet.ac1.raw(ctt.data)
    
    
    substract.reliability<-rbind(a$est,k$est,ac1$est)
    
    rater.result[[rater]] <-list(
      substract.reliability=substract.reliability,
      sdt.list=sdt.list)
    
  }
  
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
    tmp2 <- rater.result[[rater]][["substract.reliability"]][,c("coeff.val","conf.int")]
    names(tmp2) <- paste(names(tmp2),"after")
    ctt.rez <- cbind(rater,tmp,tmp2)
    ctt.rez$coeff.difference <- ctt.rez$"coeff.val before" - ctt.rez$"coeff.val after" 
    ctt <-rbind(ctt,ctt.rez)
  }
  
  print(ctt)
  
  ### SDT 
  sdt <- data.frame()
  for (rater in raters){
    comp.raters <- raters[!raters %in% rater]
    rater.sdt <- matrix(0,ncol=2,nrow=3)
    # print(rater.sdt)
    for (comp.rater in comp.raters){
      tmp <- rater.result[[rater]][["sdt.list"]][[comp.rater]]$macro
      # tmp <- as.matrix(tmp)
      # print(tmp[,2:3])
      rater.sdt = rater.sdt+tmp[,2:3]
      indic <-tmp[,1]
    }
    rater.sdt <- cbind(rater,indic,rater.sdt/length(comp.raters))
    sdt <-rbind(sdt,rater.sdt)
  }
  print(sdt)
  
  
  return(
    list(
      global.reliability=global.reliability,
      rater.result=rater.result,
      ctt = ctt,
      sdt=sdt
      
    )
  )
}

