# Name of Script : run_analysis.R
# Purpose  : Getting and Cleaning Data Project 
#       - You should create one R script called run_analysis.R that does the following. 
#       - Merges the training and the test sets to create one data set.
#       - Extracts only the measurements on the mean and standard deviation for each measurement. 
#       - Uses descriptive activity names to name the activities in the data set
#       - Appropriately labels the data set with descriptive variable names. 
#       - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# Author  : G Mensah 

        setwd("K:/coursera/getting-and-cleaning-data/project")
        
#load pacakages if necessary
        
        if (!require("plyr")) {
                install.packages("plyr")
                require("plyr")
        }
        
        if (!require("reshape2")) {
                install.packages("reshape2")
                require("reshape2")
        }

#Here are the data for the project:
#https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

        zipurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        #download.file(zipurl,destfile = "getdata-projectfiles-UCI HAR Dataset.zip")
       # unzip("getdata-projectfiles-UCI HAR Dataset.zip", exdir = ".")
        
#read the extracted files
        x_train         <- read.table("UCI HAR Dataset/train/X_train.txt")     
        y_train         <- read.table("UCI HAR Dataset/train/y_train.txt")
        subject_train   <- read.table("UCI HAR Dataset/train/subject_train.txt")
        x_test          <- read.table("UCI HAR Dataset/test/X_test.txt")
        y_test          <- read.table("UCI HAR Dataset/test/y_test.txt")
        subject_test    <- read.table("UCI HAR Dataset/test/subject_test.txt")
        activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
        features        <- read.table("UCI HAR Dataset/features.txt")

#1. Merges the training and the test sets to create one data set.
        mrgd_x <- rbind(x_train,x_test)
        head(mrgd_x,2) #qa 

#2. Extracts only the measurements on the mean and standard deviation for each measurement.
        colnames(mrgd_x) <- c(as.character(features[,2]))     # create dataset column names
        mean <- grep("mean()",colnames(mrgd_x),fixed=TRUE)    # calculate mean for each column
        std <- grep("std()",colnames(mrgd_x),fixed=TRUE)      # calculate  std for each column
        mean_std <-mrgd_x[,c(mean,std)]                       # merge measurements

#3. Uses descriptive activity names to name the activities in the data set
        mrgd_y <- rbind(y_train,y_test)
        activities <- cbind(mrgd_y,mean_std) # combine measurements with activities
        colnames(activities)[1] <- "activity"
        head(activities,5) # qa

#4. Appropriately labels the data set with descriptive variable names.
        activity_labels[,2] <- as.character(activity_labels[,2])
        
        for(i in 1:length(activities[,1])){
                activities[i,1] <- activity_labels[activities[i,1],2]
        }
        head(activities,5) # qa

#5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
        mrgd_subject <- rbind(subject_train,subject_test)
        mrgd_data<-cbind(mrgd_subject,activities)
        colnames(mrgd_data)[1] <- "subject"
        tidy_data <- aggregate( mrgd_data[,3] ~ subject+activity, data = mrgd_data, FUN= "mean" )
        
        for(i in 4:ncol(mrgd_data)){
                tidy_data[,i] <- aggregate( mrgd_data[,i] ~ subject+activity, data = mrgd_data, FUN= "mean" )[,3]
        }
        
        colnames(tidy_data)[3:ncol(tidy_data)] <- colnames(mean_std)
        write.table(tidy_data, file = "tidy_data.txt", row.name=FALSE)