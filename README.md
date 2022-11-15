# SurveySolutions2R
Import "Tabular Format" Data Exported from Survey Solutions HQ into R

Survey solutions HQ allows users to export data in SPSS, Stata or Tabular Format. 
Except the tabular format comes out as unlabelled with Stata syntax files, which means some work is still needed for a nice R import.
This package allows for importing all the data files contained within the zipfile being exported from Survey Solutions to be read directly into R with labels attached to both column names and factor levels.

## Install & Load Package

    devtools::install_github("stats4sd/SurveySolutions2R")

    library(SurveySolutions2R)

## Use Package
1. Download your survey data in tabular format. If you don't actually use Survey Solutions but want to play with their data they have a free demo server:

    https://demo.mysurvey.solutions

    Login: Headquarters1 Password: Headquarters1

2. Run a simple line of code to import the data into R and then get going with your data analysis

    SurveySolutions2R("Example-FP_31_Tabular_All.zip",saveData=FALSE)

    This should also work nicely in conjunction with the API, if you are using this to download the data:

    https://rstudio-pubs-static.s3.amazonaws.com/239851_1bc298ae651c41c7a65e09ce82f9053f.html
