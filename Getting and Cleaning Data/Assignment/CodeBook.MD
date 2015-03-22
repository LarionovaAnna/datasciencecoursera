---
title: 'Getting and Cleaning Data: Course Project'
author: "LarionovaAnna"
date: "Sunday, March 22, 2015"
output: html_document
---
Source of the original data: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

Original description: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

The attached R script (run_analysis.R) performs the following to clean up the data:

* Merges the training and test sets to create one data set
* Reads features.txt and extracts only the measurements on the mean and standard deviation for each measurement
* Reads activity_labels.txt and applies descriptive activity names to name the activities in the data set
* The script also appropriately labels the data set with descriptive names: all feature names (attributes) and activity names are converted to lower case, underscores and brackets () are removed. Then it merges the data frame containing features with data frames containing activity labels and subject IDs. The result is saved as merged_clean_data.txt
* The script creates a tidy data set with the average of each measurement for each activity and each subject. The result is saved as data_set_with_the_averages.txt