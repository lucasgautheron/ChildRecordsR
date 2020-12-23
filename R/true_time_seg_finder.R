
#' 
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
true_time_seg_finder <- function(range_from,range_to,all.meta.table){
  
  larger = all.meta.table$true_onset<range_from & all.meta.table$true_offset>range_to
  displaceleft =  all.meta.table$true_onset < range_from &  all.meta.table$true_offset <= range_to & all.meta.table$true_offset > range_from
  displaceright = all.meta.table$true_onset >= range_from &  all.meta.table$true_offset > range_to & all.meta.table$true_onset < range_to
  smaller = all.meta.table$true_onset>=range_from & all.meta.table$true_offset<=range_to
  # same = all.meta.table$true_onset==range_from & all.meta.table$true_offset==range_to
  
  
  # print(larger)
  # print(displaceleft)
  # print(displaceright)
  # print(smaller)
  return(larger | displaceleft | displaceright | smaller)
}
