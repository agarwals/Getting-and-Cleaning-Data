
# Source of data for the project:
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
#
# This R script does the following:
# 1. Merges the training and the test sets to create one data set.

X_train<- read.table("train/X_train.txt")
X_test<- read.table("test/X_test.txt")
X <- rbind(X_train, X_test)

S_train <- read.table("train/subject_train.txt")
S_test <- read.table("test/subject_test.txt")
S <- rbind(S_train, S_test)

y_train <- read.table("train/y_train.txt")
y_test <- read.table("test/y_test.txt")
Y <- rbind(y_train, y_test)


# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
features <- read.table("features.txt")
idx_of_features_measured <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, idx_of_features_measured]
names(X) <- features[idx_of_features_measured, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))

# 3. Uses descriptive activity names to name the activities in the data set

activities <- read.table("activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
Y[,1] = activities[Y[,1], 2]
names(Y) <- "activity"

# 4. Appropriately labels the data set with descriptive activity names.

names(S) <- "subject"
merged_tidy_Data <- cbind(S, Y, X)
write.table(merged_tidy_Data, "merged_tidy_data.txt")


# 5. Creates an independent tidy data set with the average of each variable for each activity and each subject.
uniqueSubjects = unique(S)[,1]
numSubjects = length(unique(S)[,1])
numActivities = length(activities[,1])
numCols = dim(merged_tidy_Data)[2]
result = merged_tidy_Data[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
  for (a in 1:numActivities) {
    result[row, 1] = uniqueSubjects[s]
    result[row, 2] = activities[a, 2]
    tmp <- merged_tidy_Data[merged_tidy_Data$subject==s & merged_tidy_Data$activity==activities[a, 2], ]
    result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
    row = row+1
  }
}
write.table(result, "data_set_with_the_averages.txt")
