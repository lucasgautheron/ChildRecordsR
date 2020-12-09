###########################
#                         #
#  Try what you want here #
#                         #
#                         #
###########################



for (file in list.files("R")){  source(paste0("R/",file))}




ChildRecordingsPath = "/mnt/94707AA4707A8CAC/CNRS/namibia-data/"
# ChildRecordingsPath = "/Users/alejandrinacristia/Dropbox/namidia-data/"
CR = ChildRecordings(ChildRecordingsPath)



search1 <- find.ratting.segment(CR,"aiku/namibie_aiku_20160715_1.wav")


ratting1  = aggregate.rating(CR,search1 ,0.1)
rez = analyse(ratting1)



rez = ratterComparaison(ratting1)

rater.result = rez[["rater.result"]]








