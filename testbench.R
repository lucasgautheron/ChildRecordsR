#' Hello all 
#' here is a test file to see what the functions in this package can do
#' I put some comments to guide you 
#' I hope this will be useful
#' Do not change this file except for adapting the following path
# ChildRecordingsPath = "/mnt/94707AA4707A8CAC/CNRS/namibia-data/"
ChildRecordingsPath = "/Users/acristia/Documents/gitrepos/namibia-data/" # change the path

### Import the function 
source("test/Secondary_function.R")
source("test/ChildRecordings.R")

### Create a ChildRecordings class
# Here you will create a class by specifing the root folder of the ChildRecording
# The class provides basic control such as missing files or unreferenced files in the meta data 
# Try to add, misplace or erase some files to see if it works
# All other functions will be based on the class to mitigate problems as much as possible

CR = ChildRecordings(ChildRecordingsPath)

### Find some ratings
# this function will extract data from the "set" variable in metadata. 
# In this example, the set is textgrid_m1 and will extract all ratings by the rater M1.
# In this example, files had been annotated from clips extracted from the longer audiofile.
# Therefore, onset and offset of annotations referred to the clip, rather than the longer audiofile.
# To fix this, the onset and offset will be modified to reflect their true value
# with respect to the original longer audiofile.
# This is only done if time_seek is provided.
# Date and hours of the recording will me computed if start_time and date_iso of the recording are provided. 
# This procedure will raise a message if a file is empty (no annotation).

rez = extractDataCR("textgrid_m1",CR) 
rez = rez[rez$child_id=="aiku",]
rez = rez[rez$date_iso=="2016-07-15",]
head(rez)
### Function to convert the file into long format (ie adapt onset times to the long-form audio)
# Useful to future rater reliability 
long = convertor_long_cut(rez,min(rez$segment_onset),max(rez$segment_offset),1)
head(long)
### Find rater of a wave file 
#if no time range is specified, this function will only return at table with all raters
rating = find_raters_wav(CR,"aiku/namibie_aiku_20160715_1.wav")
table = rating$table
head(table)
# However, if a time range is provided, this function will find all the data that 
# overlaps with the time range provided. For instance,  
rating_27000 = find_raters_wav(CR,"aiku/namibie_aiku_20160715_1.wav",27000,27300)
rating_27000$table
plot(rating_27000)
# here is another example of the same file with different time code 
rating_27200 = find_raters_wav(CR,"aiku/namibie_aiku_20160715_1.wav",27200,27300)
rating_27200$table
plot(rating_27200)

# When a time range is provided, the function also extracts the actual data, 
# and formats it into a table where each line represents a cut-second slice of the annotation
# by default the length of the "cut" variable is 0.100 seconde but you can change it like this
rating_27200_cut1 = find_raters_wav(CR,"aiku/namibie_aiku_20160715_1.wav",27200,27300,cut=1)
rating$table
plot(rating) # This prints 5 graphs. You can open them in a window for a better view



# some reliability test
data = rating$rating_by_comp$composit
data = t(data[,-1])
library(irr)
a = kripp.alpha(data, method="nominal")
a

