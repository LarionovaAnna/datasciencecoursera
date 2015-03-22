# 1. Merges the training and the test sets to create one data set.

xtraindata <- read.table("train/X_train.txt")
xtestdata <- read.table("test/X_test.txt")
x <- rbind(xtraindata, xtestdata)

straindata <- read.table("train/subject_train.txt")
stestdata <- read.table("test/subject_test.txt")
s <- rbind(straindata, stestdata)

ytraindata <- read.table("train/y_train.txt")
ytestdata <- read.table("test/y_test.txt")
y <- rbind(ytraindata, ytestdata)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

features <- read.table("features.txt")
meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
x <- x[, meanStdIndices]
names(x) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) # remove "()"
names(x) <- gsub("mean", "Mean", names(x)) # capitalize M
names(x) <- gsub("std", "Std", names(x)) # capitalize S
names(x) <- gsub("-", "", names(x)) # remove "-" in column names 

# 3. Uses descriptive activity names to name the activities in the data set. 

activities <- read.table("activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
y[,1] = activities[y[,1], 2]
names(y) <- "activity"

# 4. Appropriately labels the data set with descriptive activity names.

names(s) <- "subject"
cleaned <- cbind(s, y, x)
write.table(cleaned, "merged_clean_data.txt")

# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.

uniqueSubjects = unique(s)[,1]
numSubjects = length(unique(s)[,1])
numActivities = length(activities[,1])
numCols = dim(cleaned)[2]
result = cleaned[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
    for (a in 1:numActivities) {
        result[row, 1] = uniqueSubjects[s]
        result[row, 2] = activities[a, 2]
        tmp <- cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
        result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
        row = row+1
    }
}
write.table(result, "data_set_with_the_averages.txt")