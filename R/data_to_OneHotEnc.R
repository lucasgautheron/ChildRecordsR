

#' Create a composite vector
#'
#' @keywords internal


data_to_OneHotEnc <- function(data){
  if(nrow(data)==0){
    return(cbind(data,data.frame("composit"=as.character())))
  }
  # print(data)
  data$sum <- rowSums(data[,-1])
  data$composit <- "silence"

  tmp.data <- data[data$sum!=0,]
  # print(nrow(tmp.data))

  if (sum(tmp.data$sum>1)>=1){
    tmp.data[tmp.data$sum>1,][,"composit"] <- "overlap"
  }

  variables <- names(tmp.data[,!names(tmp.data) %in% c("time.seq","sum","composit")])

  for (var in variables){
    tmp.data[tmp.data[,var]==1 & tmp.data$sum==1,"composit"] <- var
  }
  data[data$sum!=0,] <- tmp.data

  return(data[,!names(data) %in% c("sum")])
}
