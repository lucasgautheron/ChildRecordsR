
#' file importation from annotator
#'
#' Import all the files from a specific annotator
#'
#' @param set.type : a string containing th name of the annotator present in the "set" column meta
#' @param ChildRecordings : a ChildRecordings class
#' @param LENA.OL : add the LENA overlap method. The overlap method will correct onset and offset when speech overlap by removing part that overlap.
#' @param verbose : if TRUE information will be printed out in the console
#' @param use_data_table : use the data.table package to read the .csv annotation data (depending on the operating system and the number of threads used it can be 3 to 5 times faster than 'read.csv')
#' @param threads the number of threads to run in parallel
#'
#' @return A class extractDataCR with a data.frame with all the aggregated data
#' 
#' @importFrom data.table rbindlist
#' @importFrom methods is
#' 
#' @export
#'
#' @examples
#' 
#' \dontrun{
#' 
#' library(ChildRecordsR)
#' path = "/mnt/94707AA4707A8CAC/CNRS/corpus/vandam-daylong-demo"
#' CR = ChildRecordings(path)
#' 
#' rez = extractDataCR( "vtc", CR, verbose = T, use_data_table = T, threads = 2)
#' head(rez$data)
#' 
#' # With LENA overlap method
#' 
#' rez = extractDataCR("vtc", CR, LENA.OL = T, verbose = T, use_data_table = T, threads = 2)
#'
#' }

extractDataCR <- function(set.type,
                          ChildRecordings,
                          LENA.OL = FALSE,
                          verbose = TRUE,
                          use_data_table = FALSE,
                          threads = 1) {
  
  if (verbose) start <- proc.time()

  if(!methods::is(ChildRecordings, "ChildRecordings")){
    print("is not a ChildRecordings class")
    return(NULL)
  }

  path = ChildRecordings$path
  all.meta = ChildRecordings$all.meta
  all.meta = all.meta[grepl(set.type, all.meta$set), ]

  ### Data extraction loop
  rez = parallel::mclapply(1:nrow(all.meta), function(row) {

    ### aggregate data
    if (LENA.OL){
      tmp <- LENA.overlap(all.meta[row, ],ChildRecordings, use_data_table = use_data_table, verbose = FALSE, threads = 1)
    }
    else{
      tmp <- file.opener(all.meta[row, ],ChildRecordings, use_data_table = use_data_table, threads = 1)
    }
    tmp <- merge(tmp, ChildRecordings$children, by = "child_id")
    tmp$date_iso <- as.Date(tmp$date_iso, format = "%Y-%m-%d")
    tmp$child_dob <- as.Date(tmp$child_dob, format = "%Y-%m-%d")
    tmp$age_in_day <- as.numeric(difftime(tmp$date_iso, tmp$child_dob, units = "days"))
    tmp
  }, mc.cores = threads)
  
  if (use_data_table) {
    rez = data.table::rbindlist(rez)
  }
  else {
    rez <- do.call(rbind, rez)
  }
  
  rez <-  list(data = rez)
  attr(rez, "class") <- "extractDataCR"
  
  if (verbose) {
    end_t = proc.time()
    cat('Time to compute the all.meta object of the ChildRecordings:', round(((end_t['elapsed'] - start['elapsed']) / 60) %% 60, 4), 'minutes.\n')
  }

  return(rez)
}



#' statistics from imported annotator
#'
#' @param extractDataCR : a extractDataCR class
#'
#' @details
#' The summary function for extractDataCR provides the computational methods for the classical indicators for speaker_type.
#' These indicators are then used to evaluate the interclass correlation in a mixed lme4 model.
#' The ICC should indicate how similar the units in the same group are.
#' The indicators will also be analyzed in a mixed LME4 model to provide insight into the impact of age on these indicators.
#' In this model, the indicator and age in days are scaled to provide a comparison between the indicators.
#' 
#' @importFrom lme4 lmer  
#' @importFrom insight get_variance_intercept get_variance_residual
#' @importFrom stats as.formula 
#' @import ggplot2 
#' @import magrittr
#' @import dplyr
#' 
#' @export
#'
#' @return  A list with summary data and indicators,  ICC and regression for child speaker.
#'
#' @examples
#' 
#' \dontrun{
#' 
#' library(ChildRecordsR)
#' path = "/mnt/94707AA4707A8CAC/CNRS/corpus/namibia-data/"
#' CR = ChildRecordings(path)
#' rez = extractDataCR( "textgrid/m1", CR, verbose = F )
#' summary(rez)
#'
#' }

summary.extractDataCR <- function(extractDataCR){
  `%>%` <- magrittr::`%>%`
  data <- extractDataCR$data
  data <- data %>%
    dplyr::mutate(duration = segment_offset - segment_onset) %>%
    dplyr::arrange(child_id,age_in_day) %>%
    dplyr::group_by(child_id,age_in_day,speaker_type,experiment) %>%
    dplyr::summarise(
      voc = dplyr::n(),
      voc_ph = dplyr::n()/(max(range_offset)/3600),
      avg_voc_dur = mean(duration),
      voc_dur = sum(duration),
      voc_dur_ph = sum(duration)/(max(range_offset)/3600),

    ) %>%
    dplyr::group_by(child_id,age_in_day,experiment) %>%
    dplyr::mutate(
      prop_voc = voc / sum(voc),
    ) %>%
    dplyr::ungroup()

  outputs <- c("voc","prop_voc", "voc_ph", "avg_voc_dur", "voc_dur", "voc_dur_ph")

  table_icc <- data.frame()
  modelfits <- list()
  for(output in outputs) {

    if(length(unique(data$experiment))>1){
      Formula <- paste(output," ~ (1|experiment) + (1 | experiment:child_id)")
    } else {
      Formula <- paste(output," ~ (1|child_id)")
    }

    LMM <- lme4::lmer(as.formula(Formula), data = data %>% dplyr::filter(speaker_type == "CHI"))

    icc_corp_chi <- sum(insight::get_variance_intercept(LMM))/
      (insight::get_variance_residual(LMM)+
         sum(insight::get_variance_intercept(LMM)))

    modelfits[[output]]$Formula <- Formula
    modelfits[[output]]$LMM <- LMM
    modelfits[[output]]$icc_corp_chi <- as.numeric(icc_corp_chi )

    table_icc <- rbind(table_icc, data.frame(corpus = "all",
                                             indicator=output,
                                             ICC = as.numeric(icc_corp_chi ),
                                             formulat = Formula))
    if(length(unique(data$experiment))>1){

      for(thiscor in unique(data$experiment)){
        LMM <- lme4::lmer(stats::as.formula(Formula), data = data %>% dplyr::filter(speaker_type == "CHI"),
                          subset = c(experiment!=thiscor))

        icc_corp_chi <- sum(insight::get_variance_intercept(LMM))/
          (insight::get_variance_residual(LMM)+
             sum(insight::get_variance_intercept(LMM)))

        modelfits[[output]]$Formula <- Formula
        modelfits[[output]]$LMM <- LMM
        modelfits[[output]]$icc_corp_chi <- as.numeric(icc_corp_chi )

        table_icc <- rbind(table_icc, data.frame(corpus = thiscor ,
                                                 indicator=output,
                                                 ICC = as.numeric(icc_corp_chi ),
                                                 formulat = Formula))
      }
    }
  }

  regression_table<-data.frame()
  for(output in outputs) {

    if(length(unique(data$experiment))>1){
      Formula <- paste("scale(",output,") ~ scale(age_in_day) + (1|experiment) + (1 | experiment:child_id)")
    } else {
      Formula <- paste("scale(",output,") ~ scale(age_in_day) + (1|child_id)")
    }

    LMM <- lme4::lmer(stats::as.formula(Formula), data = data %>% dplyr::filter(speaker_type == "CHI"))

    # cat(output,"beta : ", LMM@beta[2],"\n")
    regression_table <- rbind(regression_table,
                              data.frame(predicted.variable = output,
                                         "Beta.age" = LMM@beta[2],
                                         "intercept" = LMM@beta[1],
                                         formulat = Formula)
    )
  }
  Plot = ggplot2::ggplot(table_icc,
                         ggplot2::aes(x= indicator, y = ICC, color = corpus))+
    ggplot2::geom_violin(ggplot2::aes(group=indicator)) +
    ggplot2::geom_jitter(height = 0, width = 0.3,size=3)+
    ggplot2::ylim(0,1)+
    ggplot2::coord_flip()+
    ggplot2::theme(legend.text=ggplot2::element_text(size=14),axis.text.y=ggplot2::element_text(size=14))+
    ggplot2::labs(title= "Indicator Intraclass correlation for child_id and corpus")

  cat("###########################","\n")
  cat("ExtractDataCR data analysis","\n")
  cat("\n")

  cat("Indicator Intraclass correlation for child speaker type only :",'\n')
  cat("\n")
  print(as.data.frame(table_icc))
  cat("\n")

  cat("Regression on scaled variables for child speaker type only :",'\n')
  cat("\n")
  print(as.data.frame(regression_table))
  cat("\n")

  suppressWarnings(print(Plot))

  invisible(
  list(data = data ,
       icc = as.data.frame(table_icc),
       regression = as.data.frame(regression_table),
       modelfits_icc = modelfits)
  )
}

