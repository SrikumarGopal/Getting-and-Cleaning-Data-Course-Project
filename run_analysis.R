## you will need to install plyr & reshape2 package before loading the library using the below command
## install.packages("plyr")
## install.packages("reshape2")

library(plyr)
library(reshape2)

## Goals
## 1. each variable should be in one column
## 2. each observation of that variable should be in a diferent row
## 3. include ids to link tables together

root.dir <- "./data/getting&CleaningData/UCI HAR Dataset"

## Merges training and test sets 
data.set <- list()

# Checks for data directory and creates one if it doesn't exist"
if (!file.exists("data/getting&CleaningData")) 
{
    message("Creating data directory")
    dir.create("data/getting&CleaningData")
} else message("data/getting&CleaningData directory already exists")


if (!file.exists("data/getting&CleaningData/UCI_HAR_data.zip")) 
{
    # download the data
    fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    ## zipfile="data/getting&CleaningData/UCI_HAR_data.zip"
    zipfile="data/getting&CleaningData/UCI_HAR_data.zip"

    message("Downloading data")
    download.file(fileURL, destfile=zipfile) #curl doesnt work for me, not sure why, removed this method: method="curl"
    
    message('unzip the file')
    unzip(zipfile, exdir="data/getting&CleaningData")
    message("downloading & unzip of file complete")
} else message("zip file already exists")

data.set$features <- read.table(paste(root.dir, "features.txt", sep="/"), col.names=c('id', 'name'), stringsAsFactors=FALSE)
message("features.txt loading complete")

data.set$activity_labels <- read.table(paste(root.dir, "activity_labels.txt", sep="/"), col.names=c('id', 'Activity'))
message("activity_features.txt complete")


data.set$test <- cbind(subject=read.table(paste(root.dir, "test", "subject_test.txt", sep="/"), col.names="Subject"),
                       y=read.table(paste(root.dir, "test", "y_test.txt", sep="/"), col.names="Activity.ID"),
                       x=read.table(paste(root.dir, "test", "x_test.txt", sep="/")))
message("loading test set complete")

data.set$train <- cbind(subject=read.table(paste(root.dir, "train", "subject_train.txt", sep="/"), col.names="Subject"),
                        y=read.table(paste(root.dir, "train", "y_train.txt", sep="/"), col.names="Activity.ID"),
                        x=read.table(paste(root.dir, "train", "X_train.txt", sep="/")))
message("loading train set complete")

rename.features <- function(col) {
    col <- gsub("tBody", "Time.Body", col)
    col <- gsub("tGravity", "Time.Gravity", col)
    
    col <- gsub("fBody", "FFT.Body", col)
    col <- gsub("fGravity", "FFT.Gravity", col)
    
    col <- gsub("\\-mean\\(\\)\\-", ".Mean.", col)
    col <- gsub("\\-std\\(\\)\\-", ".Std.", col)
    
    col <- gsub("\\-mean\\(\\)", ".Mean", col)
    col <- gsub("\\-std\\(\\)", ".Std", col)
    
    return(col)
}

## Extracts only the measurements on the mean and standard deviation for each measurement.
tidy <- rbind(data.set$test, data.set$train)[,c(1, 2, grep("mean\\(|std\\(", data.set$features$name) + 2)]

## Uses descriptive activity names to name the activities in the data set
names(tidy) <- c("Subject", "Activity.ID", rename.features(data.set$features$name[grep("mean\\(|std\\(", data.set$features$name)]))

## Appropriately labels the data set with descriptive activity names.
tidy <- merge(tidy, data.set$activity_labels, by.x="Activity.ID", by.y="id")
tidy <- tidy[,!(names(tidy) %in% c("Activity.ID"))]

## Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidy.mean <- ddply(melt(tidy, id.vars=c("Subject", "Activity")), .(Subject, Activity), summarise, MeanSamples=mean(value))

tidy.mean.file <-  "./data/getting&CleaningData/tidy.mean.txt"
tidy.file <-  "./data/getting&CleaningData/tidy.txt"

## Create the output files
write.csv(tidy.mean, file = tidy.mean.file ,row.names = FALSE)
write.csv(tidy, file = tidy.file ,row.names = FALSE)
message("## tidy.mean.txt & tidy.file creation complete ##")
