library(lubridate)



####################################################
######### Building childrecordings class ###########
####################################################

ChildRecordings <- function(path) {
  ChildRecordingsPath = path
  
  ### Metadata
  annotation <- read.csv(paste0(path, "metadata/annotations.csv"), stringsAsFactors = F)
  recordings <- read.csv(paste0(path, "metadata/recordings.csv"), stringsAsFactors = F)
  all.meta <-
    merge(recordings, annotation, by.x = "filename", by.y = "recording_filename")
  
  ### Check data test  
  
  # Meta file 
  referenced.file <- length(all.meta$annotation_filename)
  annotations.list.file <- list.files(paste0(ChildRecordingsPath,"annotations/"),recursive = T)
  nbr.file <- length(annotations.list.file)
  files.missing <- all.meta[!all.meta$annotation_filename %in% annotations.list.file,]$annotation_filename
  files.unreferenced <- annotations.list.file[!annotations.list.file %in% all.meta$annotation_filename]
  
  # check time start 
  test <- is.na(all.meta$start_time)  
  nbr.start.time <- sum(test)
  missing.start.time <- all.meta[test, ]$annotation_filename
  
  
  ### Print Info summary 
  
  cat("###############################################\n")
  cat("Hello Wellcome to the ChildRecordings R Project \n\n")
  
  cat("Your ChildRecording project path containe : \n",
      nbr.file," annotations files \n",
      referenced.file, " are referenced in the metadata \n")
  
  if(length(files.missing)==0 & length(files.unreferenced)==0){
    cat("\n All files seems to be present and referenced in the metadata (good news ;) )\n")
  }
  
  if(length(files.missing)!=0 ){
    cat( length(files.missing), " files seems to be missing in the annotations folders")
    cat("\t more infos inChildRecordings$integrity_test$files.missing \n")
  }
  
  if(length(files.unreferenced)!=0 ){
    cat( length(files.unreferenced), " files seems to be unreferenced in the metadata \n")
    cat("\t more infos in  ChildRecordings$integrity_test$files.unreferenced \n")
  }
  
  cat("\n")
  
  if(length(missing.start.time)!=0){
    cat(" ", length(missing.start.time), " metadata don't have a start recording time (a.k.a start.time) \n" )
    cat("\t therefore time indicators wil not be build for those files \n")
    cat("\t more infos in ChildRecordings$integrity_test$missing.start.time \n")
  }
  
  value <-
    list(
      annotation = annotation,
      recordings = recordings,
      all.meta = all.meta,
      path = ChildRecordingsPath,
      integrity_test = list("files.missing"=files.missing,
                            "files.unreferenced"=files.unreferenced,
                            "missing.start.time"=missing.start.time)
    )
  attr(value, "class") <- "ChildRecordings"
  value
}

####################################################
## import CSV files from a child recoding project ##
####################################################

extractDataCR <- function(set.type, ChildRecordings) {
  
  path = ChildRecordings$path
  all.meta = ChildRecordings$all.meta
  all.meta = all.meta[grepl(set.type, all.meta$set), ]
  
  ### Data extraction loop 
  data <- data.frame()
  for (row in 1:nrow(all.meta)) {
    temps.data <- file.openner(all.meta[row, ],ChildRecordings)
    
    # bind data
    data <- rbind(data, temps.data)
    
  }
  
  data
  
}





####################################################
#### find rater from a specific file wav file #####
####################################################


find_raters_wav <- function(ChildRecordings,file,range_from=NULL,range_to=NULL,cut=0.100){
  
  tbl <- ChildRecordings$all.meta
  path <- ChildRecordings$path
  tbl <- tbl[tbl$filename==file,]
  
  ### True time from time seek 
  
  tbl$true_onset <- tbl$time_seek + tbl$range_onset
  tbl$true_offset <- tbl$time_seek + tbl$range_offset
  
  ### seek file with appropriet range 
  
  if (!is.null(range_from) & !is.null(range_to)){
    
    larger = tbl$true_onset<=range_from & tbl$true_offset>=range_to
    displaceleft =  tbl$true_onset <= range_from &  tbl$true_offset <= range_to & tbl$true_offset > range_from
    displaceright = tbl$true_onset >= range_from &  tbl$true_offset >= range_to & tbl$true_onset < range_to
    smaller = tbl$true_onset>=range_from & tbl$true_offset<=range_to
    
    tbl <- tbl[larger | smaller | displaceleft | displaceright,]
    
  }
  
  ### Converting CSV to long rating format
  
  ratings <- list()
  
  if (!is.null(range_from) & !is.null(range_to)){
    for (row in 1:nrow(tbl)){
      # print(tbl[row,])
      
      temps.data <- file.openner(tbl[row, ],ChildRecordings)
      ratings$annotations[[as.character(tbl[row,]$set)]] <- temps.data
      
      
      # long format and composit
      temps.data2 <- convertor_long_cut(temps.data,range_from,range_to,cut=cut)
      temps.data2 <- data_to_OneHotEnc(temps.data2)
      # print(head(temps.data2))
      
      ratings$long_cut[[as.character(tbl[row,]$set)]] <- temps.data2
      
    }
  }
  ### Formating data to analyses
  
  ratings_comp <- list()
  
  if (!is.null(range_from) & !is.null(range_to)){
    listOfRating <- names(ratings$long_cut[[1]])
    listOfRating <- listOfRating[-1]
    for (rating in listOfRating) {
      rat <-data.frame("time.seq"=ratings$long_cut[[1]]$time.seq)
      
      for (rater in 1:length(ratings$long_cut)){
        
        rater_name <-names(ratings$long_cut)[rater]
        rat <- cbind(rat,ratings$long_cut[[rater]][,rating])
        names(rat)[length(rat)] <- paste0(rater_name,"_",rating)
        
      }
      
      
      # print(rat)
      ratings_comp[[rating]] <- rat
      
    }
    
  }
  
  
  ### Formating return
  value <-
    list(
      variable = list(ChildRecordings=ChildRecordings,
                      file=file,
                      range_from=range_from,
                      range_to=range_to),
      table = tbl,
      rating_data = ratings,
      rating_by_comp = ratings_comp
    )
  attr(value, "class") <- "raterCompCR"
  value
}









