##########################################################################################################

## Coursera Getting and Cleaning Data Course Project
## Aravind Raghavan
## 2015-10-24

# runAnalysis.r File Description:

#   This script will perform the following steps on the UCI HAR Dataset downloaded from 
#       https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

##########################################################################################################

library("reshape2")

filename <- "Dataset.zip"

# Check if file exisits before downloading.
if (!file.exists(filename)) {
    fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileUrl, destfile=filename, method="curl")
} 

# Extract the contents of Dataset.zip
if (!file.exists("UCI HAR Dataset")){
    unzip(filename)   
}

# Read features
features <- read.table("UCI HAR Dataset/features.txt")
features[,2] <- as.character(features[,2])

# Have a variable that will filter out columns that 
# are mean() / std() and store the names of those
# columns in another variable.
featuresWanted <- grep("mean\\(\\)|std\\(\\)", features[,2])
featureNames <- features[featuresWanted,2]
featureNames = gsub('-mean()', 'Mean', featureNames)
featureNames = gsub('-std()', 'Std', featureNames)
featureNames <- gsub('[-()]', '', featureNames)

# Read activity labels
activity <- read.table("UCI HAR Dataset/activity_labels.txt")
activity[,2] <- as.character(activity[,2])

# Extracts only the measurements on the mean and standard deviation for each measurement. 
testing <- read.table("UCI HAR Dataset/test/X_test.txt")[featuresWanted]
testing_activity <- read.table("UCI HAR Dataset/test/y_test.txt")
test_subject <- read.table("UCI HAR Dataset/test/subject_test.txt")
testing <- cbind(test_subject, testing_activity, testing)

# Extracts only the measurements on the mean and standard deviation for each measurement. 
training <- read.table("UCI HAR Dataset/train/X_train.txt")[featuresWanted]
training_activity <- read.table("UCI HAR Dataset/train/y_train.txt")
train_subject <- read.table("UCI HAR Dataset/train/subject_train.txt")
training <- cbind(train_subject, training_activity, training)

# Merges the training and the test sets to create one data set.
data <- rbind(training, testing)

# Appropriately labels the data set with descriptive variable names. 
colnames(data) <- c("subject", "activity", featureNames)

# Uses descriptive activity names to name the activities in the data set
data$activity <- factor(data$activity, levels = activity[,1], labels = activity[,2])
data$subject <- as.factor(data$subject)

# From the data set in step 4, creates a second, independent tidy data 
# set with the average of each variable for each activity and each subject
otherData <- melt(data, id=c("subject", "activity"))
dataMean <- dcast(otherData, subject + activity ~ variable, mean)

# write the tidy data to a table
write.table(dataMean, "tidy.txt", row.names=FALSE)