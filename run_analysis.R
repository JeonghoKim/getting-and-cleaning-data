##
## Programming Assignment COurse3 : run_analysis.R
##

## download files
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "dataFile.zip")
unzip(zipfile = "dataFile.zip")

# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.:measurements
# 3. Uses descriptive activity names to name the activities in the data set : activity
# 4. Appropriately labels the data set with descriptive variable names. : labels
# 5. From the data set in step 4, creates a second, independent tidy data set 
#    with the average of each variable for each activity and each subject.
# Load Packages and get the Data : package -data.table, reshape2, DataSet - train, test

library(data.table)
library(reshape2)

# Load activity labels and features
activityLabels <- fread(file.path("UCI HAR Dataset/activity_labels.txt")
                        , col.names = c("classLabels", "activityName"))
features <- fread(file.path("UCI HAR Dataset/features.txt")
                  , col.names = c("index", "featureNames"))
featuresWanted <- grep("(mean|std)\\(\\)", features[, featureNames])
measurements <- features[featuresWanted, featureNames]
measurements <- gsub('[()]', '', measurements)

# Load train datasets
train <- fread(file.path("UCI HAR Dataset/train/X_train.txt"))[, featuresWanted, with = FALSE]
data.table::setnames(train, colnames(train), measurements)
trainActivities <- fread(file.path("UCI HAR Dataset/train/Y_train.txt")
                         , col.names = c("Activity"))
trainSubjects <- fread(file.path("UCI HAR Dataset/train/subject_train.txt")
                       , col.names = c("SubjectNum"))
train <- cbind(trainSubjects, trainActivities, train)

# Load test datasets
test <- fread(file.path("UCI HAR Dataset/test/X_test.txt"))[, featuresWanted, with = FALSE]
data.table::setnames(test, colnames(test), measurements)
testActivities <- fread(file.path("UCI HAR Dataset/test/Y_test.txt")
                        , col.names = c("Activity"))
testSubjects <- fread(file.path("UCI HAR Dataset/test/subject_test.txt")
                      , col.names = c("SubjectNum"))
test <- cbind(testSubjects, testActivities, test)

# merge datasets and add labels
mergedData <- rbind(train, test)
head(mergedData)

# Convert classLabels to activityName basically. More explicit. 
mergedData[["Activity"]] <- factor(mergedData[, Activity]
                                 , levels = activityLabels[["classLabels"]]
                                 , labels = activityLabels[["activityName"]])
mergedData[["SubjectNum"]] <- as.factor(mergedData[, SubjectNum])
mergedData <- reshape2::melt(data = mergedData, id = c("SubjectNum", "Activity"))
mergedData <- reshape2::dcast(data = mergedData, SubjectNum + Activity ~ variable, 
                              fun.aggregate = mean)
data.table::fwrite(x = mergedData, file = "tidyData.csv", quote = FALSE)
head(mergedData)

write.table(mergedData, "tidyData.txt", row.names = FALSE, quote = FALSE)
