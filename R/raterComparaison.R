
#' Compare Annotator reliability
#'
#' Compare a rater from a raterData class and provide indication of quality for each annotator.
#' In classical test theory tested annotator is retracted from the data and reliabily indicator are provided.
#' A good annotator should should decrase the indicator after retraction.
#' In signal detection theory a mean of precision, recal and F-score are provided by rater.
#'
#' @param raterData : a raterData class
#'
#' @result Soon
#'
#'
#'
#'
#'





raterComparaison <- function(raterData){

  if(!is(raterData, "raterData")){
    print(paste( substitute(raterData), "is not a raterData class retrun null result"))
    return(NULL)
  }




  raters <- raterData$args$ratersID
  global.reliability <- analyse(raterData)
  global.reliability <- global.reliability$reliability[["composit"]]

  rater.result <- list()


  for (rater in raters){
    tmp.data=data.frame()
    comp.raters <- raters[!raters %in% rater]


    ## CTT data build
    ctt.data<- data.frame(row.names=1:length(raterData[["rater"]][[rater]]$long_file$composit))
    sdt.list <- list()

    for(comp.rater in comp.raters){

      ratting = raterData[["rater"]][[comp.rater]]$long_file$composit

      ctt.data <- cbind(ctt.data,data.frame(ratting))

      tmp<-SDT.raterData(raterData,c(rater,comp.rater),plot=F)
      # print(tmp)
      sdt.list[[comp.rater]]<-tmp
    }

    names(ctt.data) <- comp.raters
    a = irrCAC::krippen.alpha.raw(ctt.data)
    k = irrCAC::fleiss.kappa.raw(ctt.data)
    ac1 = irrCAC::gwet.ac1.raw(ctt.data)


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

