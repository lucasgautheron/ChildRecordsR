#'
#' Create a composit verctor
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

data_to_OneHotEnc <- function(data){
  rez <- c()
  for (row in 1:nrow(data)){
    # print(data[row,])

    if (rowSums(data[row,-1])==0){
      trez= "silence"
    }else{
      if(rowSums(data[row,-1])>1) {
        # trez = NA
        trez = "overlap"
      }else{
        if(data[row,]$CHI>=1) trez = "CHI"
        if(data[row,]$FEM>=1) trez = "FEM"
        if(data[row,]$MAL>=1) trez = "MAL"
        if(data[row,]$OCH>=1) trez = "OCH"
      }
    }
    # row =row +1
    rez <- c(rez,trez)
  }

  data$composit <- rez
  data
}
