## Download the Data to local workspace

filesPath <- "/Users/taejinjo/OneDrive/Data_Analytics/03.Getting and cleaning data/Week4/Assignment4"
setwd(filesPath)

## Check if "data" folder exist
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip",method="curl")

###Unzip DataSet to /data directory
unzip(zipfile="./data/Dataset.zip",exdir="./data")

##Load required packages
library(dplyr)
library(data.table)
library(tidyr)

##Files in folder 'UCI HAR Dataset' that will be used are:
##  1.	SUBJECT FILES
##   	test/subject_test.txt
##		train/subject_train.txt
##	2.	ACTIVITY FILES
##		test/X_test.txt
##		train/X_train.txt
##	3.	DATA FILES
##		test/y_test.txt
##	4.	features.txt - Names of column variables in the dataTable
##		activity_labels.txt - Links the class labels with their activity name

## Read the above files and create data tables. Once the zip file is unzipped, sub-directory "UCI HAR Dataset" is created. Therefore, for convinience, filesPath were re-assigned
filesPath <- "/Users/taejinjo/OneDrive/Data_Analytics/03.Getting and cleaning data/Week4/Assignment4/data/UCI HAR Dataset"

# Read subject files
dataSubjectTrain <- tbl_df(read.table(file.path(filesPath, "train", "subject_train.txt")))
dataSubjectTest  <- tbl_df(read.table(file.path(filesPath, "test" , "subject_test.txt" )))

# Read activity files
dataActivityTrain <- tbl_df(read.table(file.path(filesPath, "train", "Y_train.txt")))
dataActivityTest  <- tbl_df(read.table(file.path(filesPath, "test" , "Y_test.txt" )))

#Read data files.
dataTrain <- tbl_df(read.table(file.path(filesPath, "train", "X_train.txt" )))
dataTest  <- tbl_df(read.table(file.path(filesPath, "test" , "X_test.txt" )))

## Q1. Merges the training and the test sets to create one data set.
# for both Activity and Subject files this will merge the training and the test sets by row binding and rename variables "subject" and "activityNum"
alldataSubject <- rbind(dataSubjectTrain, dataSubjectTest)
setnames(alldataSubject, "V1", "subject")
alldataActivity<- rbind(dataActivityTrain, dataActivityTest)
setnames(alldataActivity, "V1", "activityNum")

# combine the DATA training and test files -> this file will be used to Q2~Q5
dataTable <- rbind(dataTrain, dataTest)

# name variables according to feature e.g.(V1 = "tBodyAcc-mean()-X")
dataFeatures <- tbl_df(read.table(file.path(filesPath, "features.txt")))
setnames(dataFeatures, names(dataFeatures), c("featureNum", "featureName"))
colnames(dataTable) <- dataFeatures$featureName

#column names for activity labelsdt
activityLabels<- tbl_df(read.table(file.path(filesPath, "activity_labels.txt")))
setnames(activityLabels, names(activityLabels), c("activityNum","activityName"))

# Merge columns
alldataSubjAct<- cbind(alldataSubject, alldataActivity)
dataTable <- cbind(alldataSubjAct, dataTable)

## Q2. Extracts only the measurements on the mean and standard deviation for each measurement.
# Reading "features.txt" and extracting only the mean and standard deviation
dataFeaturesMeanStd <- grep("mean\\(\\)|std\\(\\)",dataFeatures$featureName,value=TRUE) #var name

# Taking only measurements for the mean and standard deviation and add "subject","activityNum"

dataFeaturesMeanStd <- union(c("subject","activityNum"), dataFeaturesMeanStd)
dataTable<- subset(dataTable,select=dataFeaturesMeanStd) 

## 3. Uses descriptive activity names to name the activities in the data set
##enter name of activity into dataTable
dataTable <- merge(activityLabels, dataTable , by="activityNum", all.x=TRUE)
dataTable$activityName <- as.character(dataTable$activityName)

## create dataTable with variable means sorted by subject and Activity
dataTable$activityName <- as.character(dataTable$activityName)
dataAggr<- aggregate(. ~ subject - activityName, data = dataTable, mean) 
dataTable<- tbl_df(arrange(dataAggr,subject,activityName))


## 4. Appropriately labels the data set with descriptive variable names.
## 1.	leading t or f is based on time or frequency measurements.
## 2.	Body = related to body movement.
## 3.	Gravity = acceleration of gravity
## 4.	Acc = accelerometer measurement
## 5.	Gyro = gyroscopic measurements
## 6.	Jerk = sudden movement acceleration
## 7.	Mag = magnitude of movement
## 8.	mean and SD are calculated for each subject for each activity for each mean and SD measurements. The units given are g???s for the accelerometer and rad/sec for the gyro and g/sec and rad/sec/sec for the corresponding jerks.

#Names before
head(str(dataTable),2)

names(dataTable)<-gsub("std()", "SD", names(dataTable))
names(dataTable)<-gsub("mean()", "MEAN", names(dataTable))
names(dataTable)<-gsub("^t", "time", names(dataTable))
names(dataTable)<-gsub("^f", "frequency", names(dataTable))
names(dataTable)<-gsub("Acc", "Accelerometer", names(dataTable))
names(dataTable)<-gsub("Gyro", "Gyroscope", names(dataTable))
names(dataTable)<-gsub("Mag", "Magnitude", names(dataTable))
names(dataTable)<-gsub("BodyBody", "Body", names(dataTable))

# Names after
head(str(dataTable),6)

## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
##write to text file on disk
write.table(dataTable, "TidyData.txt", row.name=FALSE)
## The tidy data set a set of variables for each activity and each subject. 10299 instances are split into 180 groups (30 subjects and 6 activities) and 66 mean and standard deviation features are averaged for each group. The resulting data table has 180 rows and 69 columns ??? 33 Mean variables + 33 Standard deviation variables + 1 Subject( 1 of of the 30 test subjects) + ActivityName + ActivityNum . The tidy data set???s first row is the header containing the names for each column.
