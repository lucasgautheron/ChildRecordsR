

library(scales)
library(caret)
SDT.raterData <- function(raterData,raters){
  levels = c("CHI","FEM","MAL","OCH","overlap","silence")
  rater1 <- raterData$rater[[raters[1]]]$long_file
  rater2 <- raterData$rater[[raters[2]]]$long_file
  
  cof_mat <- confusionMatrix(
    factor(rater1$composit,levels =levels ),
    factor(rater2$composit,levels =levels), 
    dnn = raters)
  conf_tab=cof_mat$table
  # Precision & recall confusion matrix graphs
  colsums=colSums(conf_tab)
  my_conf_tab=conf_tab
  for(i in 1:dim(conf_tab)[2]) my_conf_tab[,i]=my_conf_tab[,i]/colsums[i]
  #colSums(my_conf_tab) #check
  prop_cat=data.frame(my_conf_tab*100) #generates precision because columns
  prop_cat$id=paste(
    lapply(prop_cat[raters[1]], as.character)[[1]],
    lapply(prop_cat[raters[2]], as.character)[[1]]
  )
  colnames(prop_cat)[3]<-"pr"
  data.frame(conf_tab)->stall
  stall$id=paste(
    lapply(stall[raters[1]], as.character)[[1]],
    lapply(stall[raters[2]], as.character)[[1]]
  )
  stall=merge(stall,prop_cat[c("id","pr")])
  
  prec_plot = ggplot(data = stall, mapping = aes(y = get(raters[1]), x= get(raters[2]))) +
    geom_tile(aes(fill= rescale(pr)), colour = "white") +
    geom_text(aes(label = paste(round(pr),"%")), vjust = -1,size=4) +
    geom_text(aes(label = Freq), vjust = 1,size=4) +
    scale_fill_gradient(low = "white", high = "red", name = "Percentage") +
    theme(legend.position = "none") +
    xlab(raters[2]) + ylab(raters[1]) +
    ggtitle("Precision")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  # repeat for recall
  rowsums=rowSums(conf_tab)
  my_conf_tab=conf_tab
  for(i in 1:dim(conf_tab)[2]) my_conf_tab[,i]=my_conf_tab[,i]/rowsums[i]
  #rowSums(my_conf_tab)
  prop_cat=data.frame(conf_tab/rowSums(conf_tab)*100)  #generates recall because rows
  prop_cat$id=paste(
    lapply(prop_cat[raters[1]], as.character)[[1]],
    lapply(prop_cat[raters[2]], as.character)[[1]]
  )
  colnames(prop_cat)[3]<-"rec"
  data.frame(conf_tab)->stall
  stall$id=paste(
    lapply(stall[raters[1]], as.character)[[1]],
    lapply(stall[raters[2]], as.character)[[1]]
  )
  stall=merge(stall,prop_cat[c("id","rec")])
  rec_plot = ggplot(data = stall, mapping = aes(y = get(raters[1]), x= get(raters[2]))) +
    geom_tile(aes(fill= rescale(rec)), colour = "white") +
    geom_text(aes(label = paste(round(rec),"%")), vjust = -1,size=4) +
    geom_text(aes(label = Freq), vjust = 1,size=4) +
    scale_fill_gradient(low = "white", high = "red", name = "Percentage") +
    theme(legend.position = "none") +
    xlab(raters[2]) + ylab(raters[1]) +
    ggtitle("Recall")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  
  print(grid.arrange(prec_plot,rec_plot))
  
  tbl <- data.frame(t(cof_mat[["byClass"]]))
  
  macro.recall <- sum(tbl["Recall",],na.rm=T)/length(tbl["Recall",])
  macro.precision <- sum(tbl["Precision",],na.rm=T)/length(tbl["Precision",])
  macro.f1 <- sum(tbl["F1",],na.rm=T)/length(tbl["F1",])
  unweigth <- c(macro.recall,macro.precision,macro.f1)
  
  
  macro.recall.w <- sum(tbl["Recall",]*colsums,na.rm=T)/sum(colsums)
  macro.precision.w <- sum(tbl["Precision",]*colsums,na.rm=T)/sum(colsums)
  macro.f1.w <- sum(tbl["F1",]*colsums,na.rm=T)/sum(colsums)
  weigth <- c(macro.recall.w,macro.precision.w,macro.f1.w)
  
  type <- c("Recall","Precision","F1")
  
  macro <- data.frame(type, unweigth, weigth)
  
  
  return(list("confusion_mat" = cof_mat,
              "macro" = macro)) 
  
}
