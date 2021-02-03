# ChildRecordsR

ChildRecordsR is an R package that provides and facilitates the analysis of annotations (often from daylong recordings) formatted using [ChildRecordsData](https://github.com/LAAC-LSCP/ChildRecordsData). The main functions in this package are data aggregation and analysis of the reliability of (human or machine) annotators. 

---

## Getting started

Before you start, make sure you have some data formatted using ChildRecordsData. A description of the format is provided [here](https://laac-lscp.github.io/ChildRecordsData/FORMATTING.html). 
Documentation concerning the setup and installation of corpora in ChildRecordsData format can be found [here](https://laac-lscp.github.io/ChildRecordsData/#installation). Available corpora are listed [here](https://laac-lscp.github.io/ChildRecordsData/EXTANT.html). See Appendix for an example.

## Install

In R terminal: 

``` 
install.packages("devtools")
library(devtools)
install_github("LAAC-LSCP/ChildRecordsR")
```

## What the package can do 

 - Checking the integrity of your ChildRecordsData folder.
 - Finding times and files common to multiple annotators
 - Aggregating of their annotations and their transformation to multiple formats 
 - Calculating indicators of annotation reliability 
 - Evaluating the annotations' quality with reliability and classification indicators

## Tutorial 

An R tutorial/vignette can be found [here](https://laac-lscp.github.io/ChildRecordsR/articles/ChildRecordsR.html), which provides you a guide to the basic functions and analyses from the package.

## Help
At any point, you can also use the help in R and Rstudio, using `?function_name` where function_name is the name of a function or in the help panel if you are using Rstudio. You can also found functions' documentation on the [https://laac-lscp.github.io/ChildRecordsR](https://laac-lscp.github.io/ChildRecordsR/reference/index.html).

If any issues arise, feel free to post them [here](https://github.com/LAAC-LSCP/ChildRecordsData/issues)

-----

---

## Appendix: getting a LAAC dataset that has already been formatted.
 
You can find the list of formatted LAAC datasets as well as instructions to get them [here](https://github.com/LAAC-LSCP/ChildRecordsData/blob/f314c7a536ba48422bf42ce0161ef1a2c55106e2/docs/templates/PROJECTS.md#list-of-available-projects)

All you need to do these analyses are the contents of annotations/. So typically, you'll need to:

- [Install datalad](https://github.com/LAAC-LSCP/ChildRecordsData/blob/f314c7a536ba48422bf42ce0161ef1a2c55106e2/docs/templates/PROJECTS.md#installing-datalad)
- [install the dataset you want](https://github.com/LAAC-LSCP/ChildRecordsData/blob/f314c7a536ba48422bf42ce0161ef1a2c55106e2/docs/templates/PROJECTS.md#installing-a-dataset). Note that this just installs the structure of the dataset, but not the contents.
- Get the contents of annotations/ by doing, from within your local copy of the dataset, `datalad get annotations/`.


