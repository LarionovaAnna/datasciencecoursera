---
title: "Getting and Cleaning Data: Course Project"
author: "LarionovaAnna"
date: "Sunday, March 22, 2015"
output: html_document
---

* Unzip the source (https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip) into your current working directory
* Use source("run_analysis.R") command in RStudio
* Now you will find two output files are generated in the current working directory:
- merged_clean_data.txt (8.1 Mb): it contains a data frame called cleanedData with 10299*68 dimension
- data_set_with_the_averages.txt (220 Kb): it contains a data frame called result with 180*68 dimension
* Use data <- read.table("data_set_with_the_averages.txt") command in RStudio to read the file (it is 180x68 because there are 30 subjects and 6 activities)