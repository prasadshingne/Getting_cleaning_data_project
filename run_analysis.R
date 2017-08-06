rm(list = ls()) ## clear workspace
setwd("C://Users//Prasad.Shingne//Documents//coursera//getting_cleaning_data//week4") ## set working directory


## 1. Merges the training and the test sets to create one data set.

## Read necessary files
features      = read.table("./UCI HAR Dataset/features.txt", header = FALSE)
activityType  = read.table("./UCI HAR Dataset/activity_labels.txt", header = FALSE)
subjectTrain  = read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE)
xTrain        = read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE)
yTrain        = read.table("./UCI HAR Dataset/train/y_train.txt", header = FALSE)

## Name columns
colnames(activityType) = c('activityId', 'activityType')
colnames(subjectTrain) = "subjectId"
colnames(xTrain)       = features[,2]
colnames(yTrain)       = "activityId"

## Create training set
trainingData = cbind(yTrain, subjectTrain, xTrain)

## Read test files
subjectTest = read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE)
xTest       = read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE)
yTest       = read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE)

## Assign column names
colnames(subjectTest) = "subjectId"
colnames(xTest)       = features[,2]
colnames(yTest)       = "activityId"

## Create test set
testData  = cbind(yTest, subjectTest, xTest)

## Combine training and test data to create merged data
mergeData = rbind(trainingData, testData)

## Create vector of column names from mergeData
colNames  = colnames(mergeData)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement.

## Create a logical vector that contains TRUE for ID, mean() & stddev() and false for others
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))

# Retain only the requred variables in mergeData
mergeData = mergeData[logicalVector==TRUE]

## 3. Uses descriptive activity names to name the activities in the data set

# Merge mergeData with activityType table to include descriptive activity names

mergeData = merge(mergeData, activityType, by = 'activityId', all.x=TRUE)

# Update colNames to include the new column names
colNames = colnames(mergeData)

# 4. Appropriately labels the data set with descriptive variable names.

# Cleaning up the variable names
for (i in 1:length(colNames)) 
{
        colNames[i] = gsub("\\()","",colNames[i])
        colNames[i] = gsub("-std$","StdDev",colNames[i])
        colNames[i] = gsub("-mean","Mean",colNames[i])
        colNames[i] = gsub("^(t)","time",colNames[i])
        colNames[i] = gsub("^(f)","freq",colNames[i])
        colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
        colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
        colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
        colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
        colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
        colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
        colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

# Reassign new column names to merged data
colnames(mergeData) = colNames

## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Create a new table, finalMergeData without the activityType column
finalMergeData = mergeData[,names(mergeData) != 'activityType']

# Summerizing the finalMergeData table to include just mean of each variable for each activity and each subject
tidyData = aggregate(finalMergeData[,names(finalMergeData)!= c('activityId','subjectId')],by=list(activityId=finalMergeData$activityId,subjectId = finalMergeData$subjectId),mean)

# Merging tidyData with activityType to include descriptive activity names
tidyData = merge(tidyData, activityType, by='activityId', all.x=TRUE)

# Export tidyData
write.table(tidyData, "./tidyData.txt", row.names = FALSE, sep = '\t')
