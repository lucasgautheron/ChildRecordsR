#' Hello all
#' here is a test file to see what the functions in this package can do
#' I put some comments to guide you
#' I hope this will be useful
#' Do not change this file except for adapting the following path
#'
#' If you just git pull the project and use Rstudio on the menu select Build
#' And select 'install and restart' or Ctrl + Shift + B
#'


library(ChildRecordsR)
ChildRecordingsPath = "/mnt/94707AA4707A8CAC/CNRS/corpus/namibia-data/"
# ChildRecordingsPath = "/Users/acristia/Documents/gitrepos/namibia-data/" # change the path
# ChildRecordingsPath = "/Users/alejandrinacristia/Dropbox/namidia-data/"

### Import the function
# for (file in list.files("R")){  source(paste0("R/",file))}

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

rez = extractDataCR( "textgrid/m1",CR)
rez = rez$data[rez$data$child_id=="aiku",] # select a specific child ID
rez = rez[rez$date_iso=="2016-07-15",] # select a specific date
head(rez)


# Sometime researcher need to remove overlaps speech duration for further analysis
# One of the method was develop for LENA and remove speech segment that overlap
# This method was implemented by using a variable option call LENA.OL

rez2 = extractDataCR( "textgrid/m1",CR,LENA.OL = T)
summary(rez2)

### Function to convert the file into long format (ie adapt onset times to the long-form audio)
# Useful to future rater reliability
long = convertor_long_cut(rez,min(rez$segment_onset),max(rez$segment_offset),cut = 100)
head(long,20)

###############################################
#                                             #
#               Search function               #
#                                             #
###############################################

### Search function for rating segment

# if no time windows is specified, this function will only return at table for all the know raters
# All the rater need to rater any segment find
find.rating.segment(CR,"aiku/namibie_aiku_20160715_1.wav")

# finding segments on wav file for designated rater
raters <- c("textgrid/ak","textgrid/mm","textgrid/m1")
find.rating.segment(CR,"aiku/namibie_aiku_20160715_1.wav",raters)

# finding segments on wav file for the designated windows in second and rater
search <- find.rating.segment(CR,"aiku/namibie_aiku_20160715_1.wav", raters, range_from = 27180000, range_to = 27240000)


###############################################
#                                             #
#           aggregating  function             #
#                                             #
###############################################
# Once you fine the files and time for the raters you can use the aggregate function
# The function will help you to join table and convert data into long format
# this could be useful to analyse the all corpus for example after

rating1  = aggregate.rating(search,CR,100)
# this function contains list with raters data with original format and long format


#################################################
#                                               #
#    finding all segment rating across files   #
#                                               #
#################################################
#
# If you need to analyze the all corpus you may need to bind different search result as follow

wave_file <- unique(CR$all.meta$recording_filename) # get all the wav files
raters <- c("textgrid/ak","textgrid/mm","textgrid/m1") # Define raters you are interested in

# bind all the results
search2 <- data.frame()
for (file in wave_file[]){
  print(file)
  search2 <- rbind(search2, find.rating.segment(CR, file, raters)) # could take some time
}
# analyze all the result
rating2  = aggregate.rating(search2,CR,100)



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

rez1 = reliability(rating2)
rez1


# You can also analyse the reliability of each rater
comparaison = raterComparaison(rating2)
plot(comparaison)

# compare the  two raters in classification
ratercomp <- c("textgrid/ak","textgrid/m1")
SDT.raterData(rating2,ratercomp)


# try the analyze without MM rater
raters <- c("textgrid/ak","textgrid/m1") # Define raters you are interested in
search3 <- data.frame()
for (file in wave_file){
  print(file)
  search3 <- rbind(search3, find.rating.segment(CR, file, raters))
}


rating3  = aggregate.rating(search3, CR, 100)
rez2 = reliability(rating3)
rez2
SDT.raterData(rating3,raters)
# composit Alpha = 0.55 Kappa = 0.55 ACI = 0.74 obviously that seem "better"






