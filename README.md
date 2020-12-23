# ChildRecordsR
R package for ChildRecordsData

---

## Getting started

 1. Make sure you have installed [ChildRecordsData](https://github.com/LAAC-LSCP/ChildRecordsData#installation) 
 2. Make sure you have some data within the ChildRecordsData format. More information about this format can be found in the [ChildRecordsData docs](https://github.com/LAAC-LSCP/ChildRecordsData#data-formatting-and-structure)
 3. If you are in the LAAC team, you can import a dataset that has already been formatted. See Appendix for details. 
- get annotation

---

## Install

### Usign R 

`install.packages("devtools") `

`library(devtools)`

`install_github("LAAC-LSCP/ChildRecordsR",ref="develop",auth_token = "YourPersonalToken")`

auth_token : your Personal access tokens see doc [here](https://docs.github.com/en/free-pro-team@latest/github/authenticating-to-github/creating-a-personal-access-token) only select repo


### Usign git and Rstudio (recommanded for now)

 - Open terminal
 - `git clone https://github.com/LAAC-LSCP/ChildRecordsR.git`
 - `git checkout develop`
 - Open folder in Rstudio
 - On the right panel select build and click on ** Install and Restart **  

---


## testing 

for any installation type For you could use the help in R and Rstudio and you can download this file : (https://github.com/LAAC-LSCP/ChildRecordsR/blob/develop/testbench.R).
this will provide you basic function available. (sometime annotators may be call raters or *vice versa*)


---

## What it can do 

 - Checking referenced file in the childrecording folder and meta (try to mess it to check)
 - retrieve file and provide "True" onset and offset
 - If date_iso and time_start are provided then it gives the real date and hour of the ratting line (useful if you want to look at the file by year/day/hours)
 - formatting data to future reliability test (0 = silence, 1=child, 2=FEM, 3=MAL, 4=OCHI, NA=overlap)
 - You can choose the granulation of the cut, by default it is 0.100 second
 - The search function provide common windows of annotations

---

## Need to implementation 

 - A summary method to provide reliability of ratter is not implemented
 - The code could be refine.
 - implemetation of overlap labeling and lena method on overlap

---
 
# if you want to change code 


- Create a new branch from *develop* and a merge request on develop i will handle the merge. 
 
## Appendix: getting a LAAC dataset that has already been formatted.
 
You can find the list of formatted LAAC datasets as well as instructions to get them [here](https://github.com/LAAC-LSCP/ChildRecordsData/blob/f314c7a536ba48422bf42ce0161ef1a2c55106e2/docs/templates/PROJECTS.md#list-of-available-projects)

All you need to do these analyses are the contents of annotations/. So typically, you'll need to:

- [Install datalad](https://github.com/LAAC-LSCP/ChildRecordsData/blob/f314c7a536ba48422bf42ce0161ef1a2c55106e2/docs/templates/PROJECTS.md#installing-datalad)
- [install the dataset you want](https://github.com/LAAC-LSCP/ChildRecordsData/blob/f314c7a536ba48422bf42ce0161ef1a2c55106e2/docs/templates/PROJECTS.md#installing-a-dataset). Note that this just installs the structure of the dataset, but not the contents.
- Get the contents of annotations/ by doing, from within your local copy of the dataset, `datalad get annotations/`.

If any issues arise, feel free to post them [here](https://github.com/LAAC-LSCP/ChildRecordsData/issues)
