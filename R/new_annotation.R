

#' Create new dummy annotation
#'
#' Use a real annotation to create a dummy annotation with degraded ratings and
#' directly add info to the ChildRecordings class
#'
#' @param row.meta : a row meta from ChildRecordings$all.meta to transform into dummy annotation
#' @param ChildRecordings : a ChildRecordings class
#' @param time.sd : a time deviation in millisecond to apply to the segments on using a normal law
#' @param change.cat.prob : The probability of a change in speaker_type
#' @param new.name : a string containing a new annotator name
#'
#' @export
#'
#' @importFrom stats rnorm runif
#' @importFrom utils write.csv
#'
#' @examples
#'
#' \dontrun{
#'
#' library(ChildRecordsR)
#' path = "/mnt/94707AA4707A8CAC/CNRS/corpus/vandam-daylong-demo"
#' CR = ChildRecordings(path)
#'
#' CR$all.meta
#'
#' New.annotations(row.meta = CR$all.meta[2,], ChildRecordings = CR, time.sd = 1500,
#'                 change.cat.prob = 0.10, new.name = "vtc_BAD")
#'
#' CR$all.meta
#'
#' }

New.annotations <- function(row.meta,
                            ChildRecordings,
                            time.sd = NULL,
                            change.cat.prob = NULL,
                            new.name = NULL) {

  path = ChildRecordings$path
  CR.name = deparse(substitute(ChildRecordings))
  tmp.file = file.opener(row.meta,ChildRecordings)
  cat = as.character(unique(tmp.file$speaker_type))

  tmp.file$segment_onset = tmp.file$segment_onset + round(stats::rnorm(nrow(tmp.file), 0, time.sd),0)
  tmp.file$segment_offset = tmp.file$segment_offset + round(stats::rnorm(nrow(tmp.file), 0, time.sd),0)
  change.prob <- stats::runif(nrow(tmp.file), 0, 1)
  tmp.file$speaker_type <- ifelse(change.prob<change.cat.prob,sample(cat, nrow(tmp.file),replace = T),as.character(tmp.file$speaker_type))

  N.meta <- row.meta
  N.meta$set <- new.name

  ChildRecordings$all.meta <- rbind(ChildRecordings$all.meta,N.meta)
  rownames(ChildRecordings$all.meta) <- NULL
  assign(CR.name, ChildRecordings, envir = globalenv())

  dir.create(file.path(path, "annotations",new.name,"converted" ), recursive = TRUE,showWarnings = F)
  utils::write.csv(tmp.file,
                   file = file.path(path, "annotations",new.name,"converted",N.meta$annotation_filename),
                   row.names = F)
}
