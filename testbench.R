#' Hello all 
#' here is a test file to see what the functions in this package can do
#' I put some comments to guide you 
#' I hope this will be useful
#' Do not change this file except for adapting the following path
ChildRecordingsPath = "/mnt/94707AA4707A8CAC/CNRS/namibia-data/"
# ChildRecordingsPath = "/Users/acristia/Documents/gitrepos/namibia-data/" # change the path
# ChildRecordingsPath = "/Users/alejandrinacristia/Dropbox/namidia-data/"

### Import the function 
source("test/Secondary_function.R")
source("test/ChildRecordings.R")
source("test/Methods.R")

### Create a ChildRecordings class
# Here you will create a class by specifing the root folder of the ChildRecording
# The class provides basic control such as missing files or unreferenced files in the meta data 
# Try to add, misplace or erase some files to see if it works
# All other functions will be based on the class to mitigate problems as much as possible

CR = ChildRecordings(ChildRecordingsPath)


###############################################
#                                             #
#                Basic function               #
#                                             #
###############################################
### Find some ratings
# this function will extract data from the "set" variable in metadata. 
# In this example, the set is textgrid_m1 and will extract all ratings by the rater M1.
# In this example, files had been annotated from clips extracted from the longer audiofile.
# Therefore, onset and offset of annotations referred to the clip, rather than the longer audiofile.
# To fix this, the onset and offset will be modified to reflect their true value
# with respect to the original longer audiofile.
# This is only done if time_seek is provided.
# Date and hours of the recording will be computed if start_time and date_iso of the recording are provided. 
# This procedure will raise a message if a file is empty (no annotation).

rez = extractDataCR(CR, "textgrid_m1") 
rez = rez[rez$child_id=="aiku",]
rez = rez[rez$date_iso=="2016-07-15",]
head(rez)
### Function to convert the file into long format (ie adapt onset times to the long-form audio)
# Useful to future rater reliability 
long = convertor_long_cut(rez,min(rez$segment_onset),max(rez$segment_offset),1)
head(long)

###############################################
#                                             #
#               Search function               #
#                                             #
###############################################

### Search function for ratting segment

# if no time windows is specified, this function will only return at table for all the know raters 
# All the rater need to ratter any segment find
find.ratting.segment(CR,"aiku/namibie_aiku_20160715_1.wav")

# However, if a time windows is provided, this function will find all the data that 
# overlaps with the time windows provided. 
# For instance, you can shift the window it will give you the same result  
find.ratting.segment(CR,"aiku/namibie_aiku_20160715_1.wav",range_from = 27180, range_to = 27240)
find.ratting.segment(CR,"aiku/namibie_aiku_20160715_1.wav",range_from = 27000, range_to = 27250)
find.ratting.segment(CR,"aiku/namibie_aiku_20160715_1.wav",range_from = 27180, range_to = 27260)

# finding segments on wav file for designated rater
raters <- c("textgrid_ak","textgrid_mm","textgrid_m1")
find.ratting.segment(CR,"aiku/namibie_aiku_20160715_1.wav",ratters)

# finding segments on wav file for the designated windows in second and rater
search <- find.ratting.segment(CR,"aiku/namibie_aiku_20160715_1.wav", raters, range_from = 27180, range_to = 27240)


###############################################
#                                             #
#           aggregating  function             #
#                                             #
###############################################
raters <- c("textgrid_ak","textgrid_mm","textgrid_m1")
ratting1  = aggregate.rating(CR,search,0.1)
rez = analyse(ratting1)
raters.comp <- c("textgrid_ak","textgrid_mm")
SDT = SDT.raterData(ratting1,raters.comp)
SDT

#################################################
#                                               #
#    finding all segment ratting across files   #
#                                               #
#################################################
#
# Let'zs try to analyse a larger number of file

wave_file <- unique(CR$all.meta$filename)[0:40] # get all the wav files
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

comparaison = ratterComparaison(ratting2)


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
# composit Alpha = 0.55 Kappa = 0.55 ACI = 0.74 obviously that seem "better"





































# 
# 
# # When a time range is provided, the function also extracts the actual data, 
# # and formats it into a table where each line represents a cut-second slice of the annotation
# # by default the length of the "cut" variable is 0.100 seconde but you can change it like this
# rating_27200_cut1 = find_raters_wav(CR,"aiku/namibie_aiku_20160715_1.wav",27200,27300,cut=1)
# rating_27200_cut1$table
# 
# plot(rating_27200_cut1) # This prints 5 graphs. You can open them in a window for a better view
# 
# 
# ### reliability in raterCompCR class 
# # you can extract reliability using the summary method
# summary(rating_27200_cut1)
# 
# # detail information can be found here 
# rating_27200_cut1$reliability$`Krippendorff's Alpha`
# rating_27200_cut1$reliability$`Fleiss' Kappa`
# rating_27200_cut1$reliability$AC1
# 
# ### Find ratting segment in wav file ###
# 
# #first step is using a search function to identify segment of rating
# ratters <- c("textgrid_ak","textgrid_mm") # Define raters you are interested in
# # Let's try to find the same segment as before
# search = find.ratting.segment(CR,"aiku/namibie_aiku_20160715_1.wav",ratters)
# search # here it is
# 
# # Now that you have all the rating for this wave file you can extract and agreagate results
# ratting  = aggregate.rating(search ,CR,0.1)
# # Now let's provide some statistics
# # You will be provide multiple indicator for each of the speaker and the overall
# # For the composite on you can verify that the results are the same as rating_27200
# rez = analyse(ratting)
# rez
# 
# # why not to try to find multiple segment in multiple wav file
# # Let's try our two raters on multiple files
# ratters <- c("textgrid_ak","textgrid_mm") # Define raters you are interested in
# wave_file <- unique(CR$all.meta$filename)
# head(wave_file) # some wav file name
# search1 <- data.frame() # saving all search result
# for (file in wave_file[30:40]){ # Some files doesn't seem to work 
#   find.ratting.segment(CR, file, ratters)
#   search1 <- rbind(search1, find.ratting.segment(CR, file, ratters))
# }
# search1
# ratting1  = aggregate.rating(search1 ,CR,0.1) # build a raterData class
# rez = analyse(ratting1)
# 
# 
# # And now with ours 3 raters
# ratters <- c("textgrid_ak","textgrid_mm","textgrid_m1") # Define raters you are interested in
# 
# search2 <- data.frame() 
# for (file in wave_file[30:40]){ 
#   find.ratting.segment(CR, file, ratters)
#   search2 <- rbind(search2, find.ratting.segment(CR, file, ratters))
# }
# ratting2  = aggregate.rating(search2 ,CR,0.1)
# rez = analyse(ratting2)
# # it seem that m1 increase agreement
# 
# ratters <- c("textgrid_ak","textgrid_m1") # Define raters you are interested in
# 
# search3 <- data.frame() 
# for (file in wave_file[30:40]){ 
#   find.ratting.segment(CR, file, ratters)
#   search3 <- rbind(search3, find.ratting.segment(CR, file, ratters))
# }
# ratting3  = aggregate.rating(search3 ,CR,0.1)
# rez = analyse(ratting3)
# 
# # rater MM seem to impair the raters reliability
# 
# 
# 
# 
# 
