###########################
#                         #
#  Try what you want here #
#                         #
#                         #
###########################

install_github("LAAC-LSCP/ChildRecordsR",auth_token = "b58fc95b3bb1b34662803e7692a2de308aac5511",ref="Rpackage",user="nrochat")


library(ChildRecordsR)
ChildRecordingsPath = "/mnt/94707AA4707A8CAC/CNRS/namibia-data/"

CR = ChildRecordings(ChildRecordingsPath)

wave_file <- unique(CR$all.meta$filename)[0:3] # get all the wav files
ratters <- c("textgrid_ak","textgrid_mm","textgrid_m1") # Define raters you are interested in

# bind all the results
search2 <- data.frame()
for (file in wave_file){
  print(file)
  search2 <- rbind(search2, find.ratting.segment(CR, file, ratters)) # could take some time
}
# analyze all the result
ratting2  = aggregate.rating(CR,search2,0.1)
rez1 = analyse(ratting2)
rez1
# composit Alpha = 0.41 Kappa = 0.41 ACI = 0.64
# compare the raters in SDT
ratercomp <- c("textgrid_ak","textgrid_m1")
SDT.raterData(ratting2,ratercomp)



#################################################
#                                               #
#    Investigate rater perfomances              #
#                                               #
#################################################
# 2 methods are you to investigate performance
# the first one use indicator or reliability by removing one rater from the pool
# and look at the change in indicator.
# If indicator drop then the rater as a good on reliability else no.
# The second on is using SDT and merging the results for every 1rater VS 1rater
# possible combination giving us a Mean macro precision recall and F1 score.

comparaison = raterComparaison(ratting2)


# try the analyze without MM rater
ratters <- c("textgrid_ak","textgrid_m1") # Define raters you are interested in
search3 <- data.frame()
for (file in wave_file){
  print(file)
  search3 <- rbind(search3, find.ratting.segment(CR, file, ratters))
}
ratting3  = aggregate.rating(CR, search3, 0.1)
rez2 = analyse(ratting3)
rez2
SDT.raterData(ratting3,ratters)


