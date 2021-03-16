#' Building childrecordings class
#'
#' Main class of the package. Help to check data integrity and provide data
#' organization. Only provide the path folder of the childrecordings project.
#' Provid some integrity check such as missing files or missing meta
#'
#' @param path path to the childrecoding project folder
#' @return  A ChildRecordings class containing meta data path
#'
#'
#' @examples
#' library(ChildRecordsR)
#' path = "/mnt/94707AA4707A8CAC/CNRS/corpus/namibia-data/"
#' CR = ChildRecordings(path)
#'
#'
#'
#'
#'
#'
#'


ChildRecordings <- function(path) {
  ChildRecordingsPath = paste0(path,"/")

  ### Metadata
  annotations <- read.csv(paste0(path, "metadata/annotations.csv"), stringsAsFactors = F)
  recordings <- read.csv(paste0(path, "metadata/recordings.csv"), stringsAsFactors = F)
  children <- read.csv(paste0(path, "metadata/children.csv"), stringsAsFactors = F)
  all.meta <-
    merge(recordings, annotations, by = "recording_filename")

  all.meta <-
    merge(all.meta, children, by= "child_id")

  error.conversion <- all.meta[ nchar(all.meta$error)>=1 & !is.na(nchar(all.meta$error)),]
  all.meta <- all.meta[is.na(all.meta$error) | all.meta$error=="",]


  ### Check data test

  ## Meta file

  ## referenced files

  n.referenced.file.raw <-length(unique(all.meta$raw_filename))
  path.referenced.file.raw <- paste0(all.meta$set,"/raw/",all.meta$raw_filename)

  n.referenced.file.annotation <-length(unique(all.meta$annotation_filename))
  path.referenced.file.annotation <- paste0(all.meta$set,"/converted/",all.meta$annotation_filename)

  n.referenced.file =  n.referenced.file.annotation


  ## Files
  path.file <- list.files(paste0(ChildRecordingsPath,"annotations/"),recursive = T)
  n.file <- length(path.file)

  # Check file
  files.missing <-path.referenced.file.annotation[!path.referenced.file.annotation %in% path.file]
  files.unreferenced <- path.file[!(path.file %in% path.referenced.file.annotation |path.file %in% path.referenced.file.raw)]

  # check time start
  test <- is.na(all.meta$start_time)
  nbr.start.time <- sum(test)
  missing.start.time <- all.meta[test, ]$annotation_filename

  # check empty file
  empty.files <- c()
  for (file in paste0(all.meta$set,"/converted/",all.meta$annotation_filename)){

    # if(nchar(file)!=0){
      tmp <- read.csv(paste0(path,"/annotations/",file))
      if(nrow(tmp)==0){empty.files <-c(empty.files,file)}
    # }
  }


  value <-
    list(
      annotations = annotations,
      recordings = recordings,
      children = children,
      all.meta = all.meta,
      path = ChildRecordingsPath,
      integrity_test = list("nbr.file"=n.file,
                            "referenced.file"= n.referenced.file,
                            "files.missing"=files.missing,
                            "files.unreferenced"=files.unreferenced,
                            "missing.start.time"=missing.start.time,
                            "empty.files"=empty.files,
                            "error.conversion"=error.conversion)
    )
  attr(value, "class") <- "ChildRecordings"

  print.ChildRecordings(value)
  invisible(value)
}




print.ChildRecordings <- function(ChildRecordings){


  # Meta file
  nbr.file <- ChildRecordings$integrity_test$nbr.file
  coders <- unique(ChildRecordings$all.meta$set)
  referenced.file <- ChildRecordings$integrity_test$referenced.file
  files.missing <- ChildRecordings$integrity_test$files.missing
  files.unreferenced <- ChildRecordings$integrity_test$files.unreferenced
  error.conversion <- ChildRecordings$integrity_test$error.conversion

  # check time start

  missing.start.time <- ChildRecordings$integrity_test$missing.start.time

  # check empty file
  empty.files <- ChildRecordings$integrity_test$empty.files

  ### Print Info summary

  cat("###############################################\n")
  cat("Hello Wellcome to the ChildRecordings R Project \n\n")

  cat("Your ChildRecording project path contained : \n",
      nbr.file," annotations files \n",
      referenced.file, " are referenced in the metadata \n",
      length(coders), " coders were found : ",  coders, "\n")

  if(length(files.missing)==0 & length(files.unreferenced)==0){
    cat("\n All files seems to be present and referenced in the metadata (good news ;) )\n")
  }

  cat("\n")

  if(length(files.missing)!=0 ){
    cat(" ", length(files.missing), " file(s) seem(s) to be missing in the annotations folders")
    cat("\t more infos in ChildRecordings$integrity_test$files.missing \n")
  }

  if(length(files.unreferenced)!=0 ){
    cat( length(files.unreferenced), " file(s) seem(s) to be unreferenced in the metadata \n")
    cat("\t more infos in  ChildRecordings$integrity_test$files.unreferenced \n")
  }

  cat("\n")

  if(length(missing.start.time)!=0){
    cat(" ", length(missing.start.time), " metadata don't have a start recording time (a.k.a start.time) \n" )
    cat("\t therefore time indicators will not be built for those files \n")
    cat("\t more infos in ChildRecordings$integrity_test$missing.start.time \n")
  }

  if(length(empty.files)!=0){
    cat(" ", length(empty.files), " files are empty \n" )
    cat("\t This should normally mean that no annotation were provided by annotator \n")
    cat("\t more infos in ChildRecordings$integrity_test$empty.files \n")
  }

  if (nrow(error.conversion)!=0){
    cat(" ", nrow(error.conversion), " files with conversion error\n" )
    cat("\t This should normally mean that your conversion fail \n")
    cat("\t Those files are remove from package analysis \n")
    cat("\t more infos in ChildRecordings$integrity_test$erro.convertion \n")
  }


}


















