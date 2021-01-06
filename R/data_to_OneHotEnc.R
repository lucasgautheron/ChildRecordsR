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

# data_to_OneHotEnc <- function(data){
#   rez <- c()
#   for (row in 1:nrow(data)){
#     # print(data[row,])
#
#     if (rowSums(data[row,-1])==0){
#       trez= "silence"
#     }else{
#       if(rowSums(data[row,-1])>1) {
#         # trez = NA
#         trez = "overlap"
#       }else{
#         if(data[row,]$CHI>=1) trez = "CHI"
#         if(data[row,]$FEM>=1) trez = "FEM"
#         if(data[row,]$MAL>=1) trez = "MAL"
#         if(data[row,]$OCH>=1) trez = "OCH"
#       }
#     }
#     # row =row +1
#     rez <- c(rez,trez)
#   }
#
#   data$composit <- rez
#   data
# }


data_to_OneHotEnc <- function(data){
  if(nrow(data)==0){
    return(cbind(data,data.frame("composit"=as.character())))
  }
  # print(data)
  data$sum <- rowSums(data[,-1])
  data$composit <- 0

  tmp.data <- data[data$sum!=0,]
  # print(nrow(tmp.data))


  if (sum(tmp.data$sum>1)>=1){
    tmp.data[tmp.data$sum>1,][,"composit"]<-"overlap"
  }

  variables <- names(tmp.data[,!names(tmp.data) %in% c("time.seq","sum","composit")])

  for (var in variables){
    tmp.data[tmp.data[,var]==1 & tmp.data$sum==1,"composit"]<-var
  }
  data[data$sum!=0,] <- tmp.data

  return(data[,!names(data) %in% c("sum")])

}



