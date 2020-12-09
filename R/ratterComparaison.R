
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





ratterComparaison <- function(ratterData){
  
  if(!is(ratterData, "ratterData")){
    print(paste( substitute(ratterData), "is not a ratterData class retrun null result"))
    return(NULL)
  }
  
  
  
  
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

