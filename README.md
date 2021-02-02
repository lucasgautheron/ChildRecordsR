# ChildRecordsR

ChildrecordsR is an R package for the Childrecords project, it provides and facilitates the analysis of annotations in the [ChildRecordsData](https://github.com/LAAC-LSCP/ChildRecordsData#installation)  format. The main functions are data aggregation and analysis of the reliability of human or machine annotators, whether the annotations relate to the day or to small segments. 





---

## Getting started

Before you start, make sure you have some data within the [ChildRecordsData](https://github.com/LAAC-LSCP/ChildRecordsData#installation) format.
Documentation concerning the setup and installation of corpuses in ChildRecords-data format can be found [here](https://laac-lscp.github.io/ChildRecordsData/). Make sure that you have some annotations (see Appendix for an example).


## Install

In R terminal : 

``` 
install.packages("devtools")
library(devtools)
install_github("LAAC-LSCP/ChildRecordsR")
```


## What the package can do 

 - Checking the integrity of you ChilRocordsData folder
 - The package provide search function to help you find time segment and files common to multiples annotators
 - The aggregation of the annotation data and their transformation to the long format 
 - Calculates indicators of annotation reliability 
 - Evaluation of the quality of the annotators with reliability and classification indicators




## Tutorial 

An R tutorial/vignette can be found [here](https://laac-lscp.github.io/ChildRecordsR/articles/ChildRecordsR.html), i will provide you a guide to the basic function and analyses from the package.

## Help
At any point, you can also use the help in R and Rstudio, using `?function_name` where function_name is the name of a function or in the help panel if you are using Rstudio. You can also found function documentation on the [https://laac-lscp.github.io/ChildRecordsR](https://laac-lscp.github.io/ChildRecordsR/reference/index.html).

If any issues arise, feel free to post them [here](https://github.com/LAAC-LSCP/ChildRecordsData/issues)

---

---

## Appendix: getting a LAAC dataset that has already been formatted.
 
You can find the list of formatted LAAC datasets as well as instructions to get them [here](https://github.com/LAAC-LSCP/ChildRecordsData/blob/f314c7a536ba48422bf42ce0161ef1a2c55106e2/docs/templates/PROJECTS.md#list-of-available-projects)

All you need to do these analyses are the contents of annotations/. So typically, you'll need to:

- [Install datalad](https://github.com/LAAC-LSCP/ChildRecordsData/blob/f314c7a536ba48422bf42ce0161ef1a2c55106e2/docs/templates/PROJECTS.md#installing-datalad)
- [install the dataset you want](https://github.com/LAAC-LSCP/ChildRecordsData/blob/f314c7a536ba48422bf42ce0161ef1a2c55106e2/docs/templates/PROJECTS.md#installing-a-dataset). Note that this just installs the structure of the dataset, but not the contents.
- Get the contents of annotations/ by doing, from within your local copy of the dataset, `datalad get annotations/`.


