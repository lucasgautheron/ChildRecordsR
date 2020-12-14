

# library(lubridate)
# library(irr)
# library(irrCAC)






####################################################
#### find rater from a specific file wav file #####
####################################################

# 
# find_raters_wav <- function(ChildRecordings,file,range_from=NULL,range_to=NULL,cut=0.100){
#   
#   tbl <- ChildRecordings$all.meta
#   path <- ChildRecordings$path
#   tbl <- tbl[tbl$filename==file,]
#   
#   ### True time from time seek 
#   
#   tbl$true_onset <- tbl$time_seek + tbl$range_onset
#   tbl$true_offset <- tbl$time_seek + tbl$range_offset
#   
#   ### seek file with appropriet range 
#   
#   if (!is.null(range_from) & !is.null(range_to)){
#     
#     # larger = tbl$true_onset<=range_from & tbl$true_offset>=range_to
#     # displaceleft =  tbl$true_onset <= range_from &  tbl$true_offset <= range_to & tbl$true_offset > range_from
#     # displaceright = tbl$true_onset >= range_from &  tbl$true_offset >= range_to & tbl$true_onset < range_to
#     # smaller = tbl$true_onset>=range_from & tbl$true_offset<=range_to
#     # 
#     # tbl <- tbl[larger | smaller | displaceleft | displaceright,]
#     
#     tbl <- tbl[true_time_seg_finder(range_from,range_to,tbl),]
#     
#     
#   }
#   
#   ### Converting CSV to long rating format
#   
#   ratings <- list()
#   
#   if (!is.null(range_from) & !is.null(range_to)){
#     for (row in 1:nrow(tbl)){
#       
#       temps.data <- file.openner(tbl[row, ],ChildRecordings)
#       ratings$annotations[[as.character(tbl[row,]$set)]] <- temps.data
#     
#       # long format and composit
#       temps.data2 <- convertor_long_cut(temps.data,range_from,range_to,cut=cut)
#       temps.data2 <- data_to_OneHotEnc(temps.data2)
#       
#       ratings$long_cut[[as.character(tbl[row,]$set)]] <- temps.data2
#     }
#   }
#   
#   ### Formating data to analyses
#   
#   ratings_comp <- list()
#   
#   if (!is.null(range_from) & !is.null(range_to)){
#     listOfRating <- names(ratings$long_cut[[1]])
#     listOfRating <- listOfRating[-1]
#     for (rating in listOfRating) {
#       rat <-data.frame("time.seq"=ratings$long_cut[[1]]$time.seq)
#       
#       for (rater in 1:length(ratings$long_cut)){
#         rater_name <-names(ratings$long_cut)[rater]
#         rat <- cbind(rat,ratings$long_cut[[rater]][,rating])
#         names(rat)[length(rat)] <- paste0(rater_name,"_",rating)
#       }
#       ratings_comp[[rating]] <- rat
#     }
#   }
#   
#   ### Compute reliability 
#   
#   reliability <- list()
#   if (!is.null(range_from) & !is.null(range_to)){
#     # a = kripp.alpha(t(ratings_comp[["composit"]][,-1]), method="nominal")
#     # reliability[[a$method]] <- a
#     # k = kappam.fleiss(ratings_comp[["composit"]][,-1])
#     # reliability[[k$method]] <- k
#     # ac1 <- gwet.ac1.raw(ratings_comp[["composit"]][,-1])
#     # reliability[["ac1"]] <- ac1
#     
#     a = krippen.alpha.raw(ratings_comp[["composit"]][,-1])
#     reliability[[as.character(a$est$coeff.name)]] <- a
#     k = fleiss.kappa.raw(ratings_comp[["composit"]][,-1])
#     reliability[[as.character(k$est$coeff.name)]] <- k
#     ac1 = gwet.ac1.raw(ratings_comp[["composit"]][,-1])
#     reliability[[as.character(ac1$est$coeff.name)]] <- ac1
#     
#     
#   }
#   
#   
#   
#   
#   
#   ### Formating return
#   value <-
#     list(
#       variable = list(ChildRecordings=ChildRecordings,
#                       file=file,
#                       range_from=range_from,
#                       range_to=range_to,
#                       seg=cut),
#       table = tbl,
#       rating_data = ratings,
#       rating_by_comp = ratings_comp,
#       reliability = reliability
#     )
#   attr(value, "class") <- "raterCompCR"
#   value
#   
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 


























