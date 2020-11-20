#  
# 
#  creator : Nicolas Rochat 
#  contact : nicolas.r.rochat@gmail.com 
#  data : 30/10/2020
#'
#' extract child speech counts, duration and conversational overlap and turns
#' 
#' overlatp.f.ddply and turn.f.ddply are function to label overlap and turn 
#' They are mant to be use in the main function and only provide label
#' 
#' dataCreation is the main function it return a list providing the original 
#' dataset with label such as the number of the segment, overlap and turn
#' 
#' @data : a input data.frame from a join rttms files of multiple day and childree 
#' @startMinute : where should start the analyse in regard to the start or recording in minutes 
#' @endMinute : where should end the analyse in regard to the end or recording in minutes
#' @segmentMinute : how long should be each segment of the data in minutes
#' 
#' 
#' 



###############
### package ###
###############

library(plyr)
library(dplyr)
# library(doMC)
# registerDoMC(getDoParWorkers()-2)
library(doParallel)
nodes <- detectCores()
cl <- makeCluster(nodes-1)
registerDoParallel(cl)

########################################
### turn and overlap ddplyr function ###
########################################

### Overlap 

overlatp.f.ddply <- function(data){
  
  overlap <- rep(0,nrow(data))
  
  for (row in 1:nrow(data)){
    
    speach.start <- data[row,]$start
    speach.end <- data[row,]$end
    
    test = sum( speach.start>=data$start & speach.start <=data$end & data$tier!="SPEECH"| speach.end>=data$start & speach.start<=data$end & data$tier!="SPEECH" )>1
    detect.SPEECH = data[row,]$tier != "SPEECH"
    
    if ( test & detect.SPEECH ){ overlap[row]<-1 }
    
  }
  data$overlap<-overlap
  return(data)
}

### Turn

turn.f.ddply <- function(data){
  
  turn <- rep(0,nrow(data))
  time.lag=1
  
  for (row in 1:nrow(data)){
    
    speach.start <- data[row,]$start
    speach.end <- data[row,]$end
    overlap <- data[row,]$overlap
    
    test = sum( speach.start >= data$end  & speach.start <= data$end+time.lag & data$tier!="SPEECH" )>1 
    if ( test ){ turn[row]<-1 }
    if ( overlap==1 ){ turn[row]<-1 }
    
  }
  data$turn <- turn
  return(data)
}



#####################################
### Extracting data from Raw data ###
#####################################


dataCreation <-function(data,startMinute,endMinute,segmentMinute){
  # Convert timer in second
  start = startMinute * 60
  end = endMinute * 60
  segment = segmentMinute * 60 
  
  # Check if segmentation is in the bonduary on "start" and "end" 
  if((end-start)%%segment!=0){
    print("In regard to your start and end your segment will not fit the lengt of the data")
    print("Last partial segment wil be ignore")
  }
  
  
  ### Selection windows data
  dt = data[data$start>=start & data$start<end, ]
  
  ### find Overlap speech
  print("compute overlap")
  dt = ddply(dt,.(Participant,Year,Month,Day), overlatp.f.ddply, .parallel=T, .progress = "text")
  
  ### Find turn
  print("compute Turn")
  dt = ddply(dt,.(Participant,Year,Month,Day), turn.f.ddply, .parallel=T, .progress = "text")
  
  ### Loop to segment data 
  loop = seq(1,(end - start)/segment,1) #nbr of loop
  print(loop)
  initialseg<-c(start,start+segment)
  seg <- rep(0,nrow(dt))
  
  for (l in loop){
    for (row in seq(1,nrow(dt))){
      if(dt$start[row]>=initialseg[1] & dt$start[row]<initialseg[2] ){
        seg[row]<-l
      }
    }
    initialseg = initialseg + segment
  }
  
  dt$segment <- seg 
  
  ### Aggregate data by segment 
  rez = dt %>% group_by(Participant,Year,Month,Day,tier,segment)%>%
    summarise(
      "sumInSec" = sum(duration),
      "count" = n(),
      "NbrTurn" = sum(turn),
      "NbrOverlap" = sum(overlap)
    )
  
  return(list( "anoted.data" = dt, "aggreagete" = data.frame(rez) ))
}




