# ChildRecordsR
R package for ChildRecordsData

---

## Getting started

 1. Make sure you have installed [ChildRecordsData](https://github.com/LAAC-LSCP/ChildRecordsData#installation) 
 2. Make sure you have some data within the ChildRecordsData format. 
 
     2.a. More information about this format can be found in the [ChildRecordsData docs](https://github.com/LAAC-LSCP/ChildRecordsData#data-formatting-and-structure).
 
     2.b. If you are in the LAAC team, you can import a dataset that has already been formatted. See Appendix for details. 
     
 3. Make sure that you have some annotations (see Appendix for an example).

---

## Install


### Using R (only if you are a co-developer)
``` 
install.packages("devtools")
library(devtools)
install_github("LAAC-LSCP/ChildRecordsR",ref="develop",auth_token = "YourPersonalToken")
```


auth_token : your Personal access tokens see doc [here](https://docs.github.com/en/free-pro-team@latest/github/authenticating-to-github/creating-a-personal-access-token) only select repo


---


## Testing the package

1. Download this file : (https://github.com/LAAC-LSCP/ChildRecordsR/blob/develop/Demo.R).
2. All of the instructions should work as is. If they don't, please open an issue [here](https://github.com/LAAC-LSCP/ChildRecordsR/issues)

At any point, you can also use the help in R and Rstudio, using `?function_name` where function_name is the name of a function that you've seen in Demo. This will provide you with basic descriptions of the available functions. 
Note that we are still working on the docs, so sometimes "annotators" may be called "raters" or *vice versa*.


---

## What the package can do 

 - When you import a dataset, the package automatically checks that all referenced files in the childrecording folder and meta match up (you can try to mess your dataset to test the checking function)
 - For each annotation file, the package can retrieve file and provide "true" onset and offset related to the audiorecording (in case annotation was not done on the full audio file)
 - If date_iso (recording date) and time_start (time at which recording was started) are provided then the package will calculate the real date and hour of the annotation (useful if you want to look at the file by year/day/hours)
 - You can choose the granulation of the cut, by default it is 0.100 second
 - The search function provide common windows of annotations

---

## Need to implementation 

 - A summary method to provide reliability of ratter is not implemented
 - The code could be refine.
 - implemetation of overlap labeling and lena method on overlap

---
 
# For developers 

- Create a new branch from *develop* and a merge request on develop 
 
## Appendix: getting a LAAC dataset that has already been formatted.
 
You can find the list of formatted LAAC datasets as well as instructions to get them [here](https://github.com/LAAC-LSCP/ChildRecordsData/blob/f314c7a536ba48422bf42ce0161ef1a2c55106e2/docs/templates/PROJECTS.md#list-of-available-projects)

All you need to do these analyses are the contents of annotations/. So typically, you'll need to:

- [Install datalad](https://github.com/LAAC-LSCP/ChildRecordsData/blob/f314c7a536ba48422bf42ce0161ef1a2c55106e2/docs/templates/PROJECTS.md#installing-datalad)
- [install the dataset you want](https://github.com/LAAC-LSCP/ChildRecordsData/blob/f314c7a536ba48422bf42ce0161ef1a2c55106e2/docs/templates/PROJECTS.md#installing-a-dataset). Note that this just installs the structure of the dataset, but not the contents.
- Get the contents of annotations/ by doing, from within your local copy of the dataset, `datalad get annotations/`.

If any issues arise, feel free to post them [here](https://github.com/LAAC-LSCP/ChildRecordsData/issues)
