#' Building childrecordings class
#'
#' Main class of the package. Help to check data integrity and provide data
#' organization. Only provide the path folder of the childrecordings project.
#' Provid some integrity check such as missing files or missing meta
#'
#' @param path path to the childrecoding project folder
#' @return A ChildRecordings class
#'
#'
#'
#'
#'


ChildRecordings <- function(path) {
  ChildRecordingsPath = path

  ### Metadata
  annotations <- read.csv(paste0(path, "metadata/annotations.csv"), stringsAsFactors = F)
  recordings <- read.csv(paste0(path, "metadata/recordings.csv"), stringsAsFactors = F)
  children <- read.csv(paste0(path, "metadata/children.csv"), stringsAsFactors = F)
  all.meta <-
    merge(recordings, annotations, by.x = "filename", by.y = "recording_filename")

  all.meta <-
    merge(all.meta, children, by.x = "child_id", by.y = "child_id")

  erro.convertion <- all.meta[all.meta$error!="",]
  all.meta <- all.meta[all.meta$error=="",]

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

  # check empty file
  empty.files <- c()

  for (file in all.meta$annotation_file){

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
      integrity_test = list("nbr.file"=nbr.file,
                            "referenced.file"= referenced.file,
                            "files.missing"=files.missing,
                            "files.unreferenced"=files.unreferenced,
                            "missing.start.time"=missing.start.time,
                            "empty.files"=empty.files,
                            "erro.convertion"=erro.convertion)
    )
  attr(value, "class") <- "ChildRecordings"

  print.ChildRecordings(value)
  invisible(value)
}




print.ChildRecordings <- function(ChildRecordings){


  # Meta file
  nbr.file <- ChildRecordings$integrity_test$nbr.file
  referenced.file <- ChildRecordings$integrity_test$referenced.file
  files.missing <- ChildRecordings$integrity_test$files.missing
  files.unreferenced <- ChildRecordings$integrity_test$files.unreferenced
  erro.convertion <- ChildRecordings$integrity_test$erro.convertion

  # check time start

  missing.start.time <- ChildRecordings$integrity_test$missing.start.time

  # check empty file
  empty.files <- ChildRecordings$integrity_test$empty.files

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

  if(length(empty.files)!=0){
    cat(" ", length(empty.files), " files are empty \n" )
    cat("\t This should normally mean that no annotation were provided by annotator \n")
    cat("\t more infos in ChildRecordings$integrity_test$empty.files \n")
  }

  if (nrow(erro.convertion)!=0){
    cat(" ", nrow(erro.convertion), " files with convertion error\n" )
    cat("\t This should normally mean that your conversion fail \n")
    cat("\t more infos in ChildRecordings$integrity_test$erro.convertion \n")
  }


  # print(table(x[[2]]))
  # invisible(x)
  # NextMethod()
}


















