
## This code make tidy dataset from original "UCI HAR Dataset" 

## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation 
##    for each measurement. 
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names. 
## 5. From the data set in step 4, creates a second, independent tidy data set 
##    with the average of each variable for each activity and each subject.

library(dplyr)
library(tidyr)
library(reshape2)

features <- read.table("./UCI HAR Dataset/features.txt")
activities <- read.table("./UCI HAR Dataset/activity_labels.txt")
names(activities) <- c("index", "activities")

Table_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
test_labels <- read.table("./UCI HAR Dataset/test/y_test.txt")
test_subject <- read.table("./UCI HAR Dataset/test/subject_test.txt")
Table_test <- cbind(test_subject, test_labels, Table_test)


Table_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
train_labels <- read.table("./UCI HAR Dataset/train/y_train.txt")
train_subject <- read.table("./UCI HAR Dataset/train/subject_train.txt")
Table_train <- cbind(train_subject, train_labels, Table_train)

Table1 <- rbind(Table_test, Table_train)

TblNames <- c("subject","labels", as.character(features[,2]))

Table2 <- Table1[c(1,2,grep("mean\\(|std\\(",TblNames))]


names(Table2) <- TblNames[c(1,2,grep("mean\\(|std\\(",TblNames))]
Table3 <- Table2%>%
        merge(activities,by.x = "labels",by.y = "index", all = FALSE)%>%
        select(-labels)%>%
        arrange(subject, activities)

Table4 <- Table3

Table5 <- Table4%>%
        gather(key = signal_parameter_coordinate, value, -subject, -activities)%>%
        separate(signal_parameter_coordinate, into = c("signal", "parameter", "coordinate"))%>%
        dcast(subject + activities + signal + coordinate ~ parameter, mean)%>%
        arrange(subject, activities)


Table5$signal <- as.factor(Table5$signal)
order_signal <- readLines("./UCI HAR Dataset/features_info.txt", skip = 23 )   
order_signal <- data.frame(signal = order_signal[13:29])%>%
        separate(signal, into = c("signal","coord"), extra = "drop")%>%
        select(signal)
levels(Table5$signal) <- order_signal$signal







