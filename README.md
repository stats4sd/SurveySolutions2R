# SurveySolutions2R
Import "Tabular Format" Data Exported from Survey Solutions HQ into R

Survey solutions HQ allows users to export data in SPSS, Stata or Tabular Format. 
Except the tabular format comes out as unlabelled with Stata syntax files, which means some work is still needed for a nice R import.
This package allows for importing all the data files contained within the zipfile being exported from Survey Solutions to be read directly into R with labels attached to both column names and factor levels.

Usage is simple:
1. Install the package into R & load:
devtools::install_github("stats4sd/SurveySolutions2R")
library(SurveySolutions2R)

2. Download your survey data in tabular format. If you don't actually use Survey Solutions but want to play with their data they have a free demo server:
https://demo.mysurvey.solutions
Login: Headquarters1 Password: Headquarters1

3. Run a simple line of code to import the data into R and then get going with your data analysis
SurveySolutions2R("Example-FP_31_Tabular_All.zip",saveData=FALSE)