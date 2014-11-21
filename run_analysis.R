library(data.table)
# Merges the training and the test sets to create one data set.
xtest <- read.table('./UCI HAR Dataset/test/X_test.txt', header = FALSE)
ytest <- read.table('./UCI HAR Dataset/test/Y_test.txt', header = FALSE)
subjecttest <- read.table('./UCI HAR Dataset/test/subject_test.txt', header = FALSE)
test <- cbind(xtest, ytest, subjecttest)
xtrain <- read.table('./UCI HAR Dataset/train/X_train.txt', header = FALSE)
ytrain <- read.table('./UCI HAR Dataset/train/Y_train.txt', header = FALSE)
subjecttrain <- read.table('./UCI HAR Dataset/train/subject_train.txt', header = FALSE)
train <- cbind(xtrain, ytrain, subjecttrain)
combined <- rbind(test, train)

# Add variables
features <- read.table('./UCI HAR Dataset/features.txt', header = FALSE, stringsAsFactor = FALSE)

# Extracts only the measurements on the mean and standard deviation for each measurement. 
meanstd <- sort(c(grep("mean()", features[,2], fixed = TRUE), grep("std()", features[,2], fixed = TRUE)))
meanstdcombined  <- combined[,c(meanstd, 562, 563)]

# Uses descriptive activity names to name the activities in the data set
activity <- read.table('./UCI HAR Dataset/activity_labels.txt', header = FALSE)
meanstdcombined[,67] <- factor(meanstdcombined[,67], labels = activity[,2])

# Appropriately labels the data set with descriptive variable names. 
names(meanstdcombined) <- c(features[meanstd,2], "activity", "subject")

# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
splitBySubj <- split(meanstdcombined, meanstdcombined$subject)

actAvg <- lapply(splitBySubj, function(x){
      result <- aggregate(x[1:66], by = list(x$activity), mean)}
)

#recombine list
i = 0
output <- data.frame()
for(i in 1:length(actAvg)){
      temp <- cbind(subject = i, test2[[i]])
      output <- rbind(output, temp)
}
names(output)[2] <- "activity"

write.table(output, file = "output.txt", row.name = FALSE)
