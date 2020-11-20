#' Hello all 
#' here is the test file to see what the scrip can do
#' I put some comments to guide you 
#' I hope this will be usefull
#' Do not change this file.

### Import the function 
source("test/Secondary_function.R")
source("test/ChildRecordings.R")

### Create a ChildRecordings class
# Here you will create a classe by specifing the root folder of the ChildRecorging
# The classe provide basic control such as missing files or unreferenced files in the meta data 
# Try to add, misplace or erase some files to see if it work
# All other function or plus will be based on the class to mitigate problem as much as possible

# ChildRecordingsPath = "/mnt/94707AA4707A8CAC/CNRS/namibia-data/"
ChildRecordingsPath = "/Users/alejandrinacristia/Dropbox/namidia-data/" # change the path
CR = ChildRecordings(ChildRecordingsPath)

### Find some ratings
# this function will extract data from the set variable in metadata 
# Here the set is textgrid_m1 and will extract all ratting from M1 ratter
# the onset and offset will be modify to reflect there true value 
# in regard to the wave file if time_seek is provides
# Date and hours of the reccording will me compute is start_time and date_iso of the recording are provided 
# will raise a message if a file is empty (no ratting)

rez = extractDataCR("textgrid_m1",CR) 
rez = rez[rez$child_id=="aiku",]
rez = rez[rez$date_iso=="2016-07-15",]

### Function to convert the file in to long format
# Usefull to futur ratter relaibility 
long = convertor_long_cut(rez,min(rez$segment_onset),max(rez$segment_offset))

### Find ratter of a wave file 
#if no time is specify it will only retur at table with all ratter
ratting = find_raters_wav(CR,"aiku/namibie_aiku_20160715_1.wav")
table = ratting$table

# if time code are provided the function extract the data and could use some method 
ratting = find_raters_wav(CR,"aiku/namibie_aiku_20160715_1.wav",27000,27300,cut = 1)
ratting$table
plot(ratting)

# the function will find all the data who will overlap the time code 
# here same file with different time code 
ratting = find_raters_wav(CR,"aiku/namibie_aiku_20160715_1.wav",27200,27300)
ratting$table
plot(ratting)

# by default the longer of segment is 0.100 seconde but you can change it 
ratting = find_raters_wav(CR,"aiku/namibie_aiku_20160715_1.wav",27200,27555,cut=0.100)
ratting$table
plot(ratting)

data = ratting$ratting_by_comp$composit
data = t(data[,-1])
library(irr)
a = kripp.alpha(data, method="nominal")
