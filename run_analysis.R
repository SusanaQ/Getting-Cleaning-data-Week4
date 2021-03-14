## Third assignment R Course 
## SQY. March 13 2021
## Susana Quinonez

  w4proj <- function() {

  if (!require("data.table")) {
    install.packages("data.table")
  }
  
  if (!require("reshape2")) {
    install.packages("reshape2")
  }
  
  if (!require("dtplyr")) {
    install.packages("dtplyr")
  }

  if (!require("tidyverse")) {
    install.packages("tidyverse")
  }
  require("data.table")
  require("reshape2")
  library(dplyr);
  library(tidyverse);
  
  zipFile <- "week4Project.zip"
  # Checking if archieve already exists.
  if (!file.exists(zipFile)) {
    zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(zipUrl, zipFile, method="curl")
  }  
  
    if (!file.exists("UCI HAR Dataset")) { 
       unzip(zipFile) 
  }  
  print("Week 4 project");
  
  
  ## I must read the files first and create the dataset with each file:
  ##Getting the features(col names)  first
  ## Getting the  data column names and activities 
  ## dataset features contains the description of the features
  ## dataset activities contains the activity id and the the 
  ## activity description 
  
  features <- read.table("./UCI HAR Dataset/features.txt")[,2]
  activities <- read.table("./UCI HAR Dataset/activity_labels.txt");
  names(activities) <- c("activity_id", "activity_desc");
  ##print("************************************");

  ##getting training sets:

  x_train <- read.table("UCI HAR Dataset/train/X_train.txt");
  y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
  
  names(x_train) <- features;
  names(y_train) <- c("activity_id");
  head("y_train,2********************************************************")
  ##print(head(y_train,2));
  subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
  names(subject_train) <- c("subject_No");
  subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
  names(subject_test) <- c("subject_No");  
  x_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
  y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
  names(y_test)<- c("activity_id")
  ##print("y_test: ******************")
  ##print(head(y_test,3))
  names(x_test) <- features;

  #1.Merges the training and the test sets to create one data set
  x <- rbind(x_train, x_test);
  y <- rbind(y_train, y_test);
  subjects <- rbind(subject_train,subject_test);
  ## 1. the Merge
  subjects_data <- cbind(subjects, x,y);  ## This is the merge data set
  
  ##print("*************final*************")
  ##print(names(subjects_data))
  
  ## ***************2.0 Extracts only the measurements on the mean and standard *******
  ## ***************deviation for each measurement*****************************
  mean_StandardDev <- subjects_data %>%  select(subject_No,activity_id,
                                             contains("mean"), contains("std"));
  
## *******3.Uses descriptive activity names to 
## name the activities in the data set 
     ## To every record in mean_StandardDev, I replace the activity_id, which
     ## contains the activity code and I replace it with the description 
     ## matching that code in the activities dataset
     mean_StandardDev$activity_id <- activities[mean_StandardDev$activity_id, 2] ;
      
## 4.Appropriately labels the data set with descriptive variable names.
      
    
      library(stringr); ## so I can manipule strings
      names(mean_StandardDev)<-gsub("^t", "Time", names(mean_StandardDev))
      names(mean_StandardDev)<-gsub("^f", "Frequency", names(mean_StandardDev))
      names(mean_StandardDev)<-gsub("-", " ", names(mean_StandardDev))
      names(mean_StandardDev)<-gsub("()", "", names(mean_StandardDev))
      ##print(names(mean_StandardDev))
   
##5.From the data set in step 4, creates a second, independent tidy data set 
##with the average of each variable for each activity and each subject.
## With the tidy data I can group by subject_No ( the subject) and activity
## and then getting the mean from all columns
      
      ## Here I am renaming the column name of activity_id to 
      ## activity because I replaced its content with the activity
      ## description in question No. 3
      colnames(mean_StandardDev)[2]  <- "activity";
            
  
  ind_tidy_data <- mean_StandardDev %>%
                    group_by(subject_No, activity) %>%
                    summarise_all(funs(mean))
      
    ## Writing the result data into the ind_tidy_data.txt 
      
  write.table(ind_tidy_data, "ind_tidy_data.txt", row.name=FALSE)
  
  
  
   
}