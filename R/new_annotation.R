#' Create new dummy annotation
#'
#' Use a real annotation to create a dummy annotation with degraded ratings and
#' directly add info to the ChildRecordings class
#'
#' @param row.meta : a row meta from ChildRecordings$all.meta to transform into dummy annotation
#' @param time.sd : a time deviation in millisecond to apply to the segments on using a normal law
#' @param change.cat.prob : The probability of a change in speaker_type
#' @param new.name : a string containing a new annotator name
#' @param ChildRecordings : a ChildRecordings class
#'
#'
#' @examples
#'
#' library(ChildRecordsR)
#' path = "/mnt/94707AA4707A8CAC/CNRS/corpus/vandam-daylong-demo"
#' CR = ChildRecordings(path)
#'
#' time.sd = 300
#' change.cat.prob = 0.05
#' row.meta = CR$all.meta[2,]
#' new.name = "vtc_mod"
#' New.annotations(row.meta, time.sd, change.cat.prob, new.name, CR)







New.annotations <- function(row.meta, time.sd = 300, change.cat.prob = 0.05, new.name = NULL, ChildRecordings){
  path = ChildRecordings$path
  CR.name = deparse(substitute(ChildRecordings))
  file = row.meta$annotation_filename
  tmp.file = file.opener(row.meta,ChildRecordings)
  cat = as.character(unique(tmp.file$speaker_type))

  tmp.file$segment_onset = tmp.file$segment_onset + rnorm(nrow(tmp.file), 0, time.sd)
  tmp.file$segment_offset = tmp.file$segment_onset + rnorm(nrow(tmp.file), 0, time.sd)
  change.prob <- runif(nrow(tmp.file), 0, 1)
  tmp.file$speaker_type <- ifelse(change.prob<=change.cat.prob,sample(cat, nrow(tmp.file),replace = T),as.character(tmp.file$speaker_type))

  N.meta <- row.meta
  N.meta$set <- new.name

  ChildRecordings$all.meta <- rbind(ChildRecordings$all.meta,N.meta)
  assign(CR.name, ChildRecordings, envir = globalenv())

  dir.create(file.path(path, "annotations/",new.name,"/converted/" ), recursive = TRUE,showWarnings = F)
  write.csv(tmp.file,file.path(path, "annotations/",new.name,"/converted/",N.meta$annotation_filename ))

}
