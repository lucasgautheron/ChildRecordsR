# ChildRecordsR
R package for ChildRecordsData

## Getting started

 1. Make sure you have installed [ChildRecordsData](https://github.com/LAAC-LSCP/ChildRecordsData#installation) 
 2. Make sure you have some data within the ChildRecordsData format. More information about this format can be found in the [ChildRecordsData docs](https://github.com/LAAC-LSCP/ChildRecordsData#data-formatting-and-structure)
 3. If you are in the LAAC team, you can import a dataset that has already been formatted. See Appendix for details. 
- get annotation

If you want to test the firsts functions please use the testbench.R fil, do not change this file. Use playground.R to test what you want. 

## What it can do 

 - Checking referenced file in the childrecording folder and meta (try to mess it to check)
 - retrieve file and provide "True" onset and offset
 - If date_iso and time_start are provided then it gives the real date and hour of the ratting line (useful if you want to look at the file by year/day/hours)
 - formatting data to future reliability test (0 = silence, 1=child, 2=FEM, 3=MAL, 4=OCHI, NA=overlap)
 - You can choose the granulation of the cut, by default it is 0.100 second

## Need to implementation 

 - Currently, research functions lack  flexibility 
 - Function do not know how to handle empty ratting file
 - A summary method to provide reliability of ratter is not implemented
 - The code could be refine.
 - implemetation of overlap labeling and lena method on overlap
 - Description of CLass/function will come later as well as structure
 
# if you want to change code 

- Create a new branch from *develop* and a merge request on develop i will handle the merge. 
 
 ## Appendix: getting a LAAC dataset that has already been formatted.
 
You can find the list of formatted LAAC datasets as well as instructions to get them [here](https://github.com/LAAC-LSCP/ChildRecordsData/blob/f314c7a536ba48422bf42ce0161ef1a2c55106e2/docs/templates/PROJECTS.md#list-of-available-projects)

All you need to do these analyses are the contents of annotations/. So typically, you'll need to:

- [Install datalad](https://github.com/LAAC-LSCP/ChildRecordsData/blob/f314c7a536ba48422bf42ce0161ef1a2c55106e2/docs/templates/PROJECTS.md#installing-datalad)
- [install the dataset you want](https://github.com/LAAC-LSCP/ChildRecordsData/blob/f314c7a536ba48422bf42ce0161ef1a2c55106e2/docs/templates/PROJECTS.md#installing-a-dataset). Note that this just installs the structure of the dataset, but not the contents.
- Get the contents of annotations/ by doing, from within your local copy of the dataset, `datalad get annotations/`.

If any issues arise, feel free to post them [here](https://github.com/LAAC-LSCP/ChildRecordsData/issues)