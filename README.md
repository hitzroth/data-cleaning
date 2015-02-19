The script "run_analysis.R" was written with the expectation that it would be run in a directory that already contains the "UCI HAR Dataset" directory that itself contains all the uncompressed relevant data for analysis. The appropriate data may be found in the "getdata-projectfiles-UCI HAR Dataset.zip" file available here: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip If the link does not work, you are out of luck.

The script may be executed from the R command line or within the R IDE of your choice with the command "setwd("file_path"); source("run_analysis.R")". It outputs to the working directory a plain text file entitled "project.txt" that comprises the manipulated data. The output file may be read back into R with the command "read.table(file_path, header = TRUE)".

The output consists of manipulated data. See CodeBook.md for additional details on those manipulations, along with a list of the variables and their descriptions.

The script uses the dplyr package to manipulate data. Details of the procedure are available in the script's comments.

While the script should work on any operating system, it has only been tested on a Linux system. It makes no effort to handle errors or to accommodate any data other than the expected version of the dataset. In fact, no warranty or support is offered for this script.

This script and the related documents within this repo, "README.md" (this document) and "CodeBook.md", are available free for use in part or in whole without restriction. As this was written for a course, however, submitting it as your own work would be ill-advised.