## Script used to transform the UCI HAR Dataset in tidy data

## Packages
library(dplyr)
library(tidyr)
library(reshape2)

### Variables to name the variables and labels of the data frame
features <- read.table("./UCI HAR Dataset/features.txt")
activities <- read.table("./UCI HAR Dataset/activity_labels.txt")
names(activities) <- c("index", "activities")

### Downloading "test" and "train" datasets 
Table_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
test_labels <- read.table("./UCI HAR Dataset/test/y_test.txt")
test_subject <- read.table("./UCI HAR Dataset/test/subject_test.txt")
Table_test <- cbind(test_subject, test_labels, Table_test)


Table_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
train_labels <- read.table("./UCI HAR Dataset/train/y_train.txt")
train_subject <- read.table("./UCI HAR Dataset/train/subject_train.txt")
Table_train <- cbind(train_subject, train_labels, Table_train)

## Creating Table1. Merging both dataset
Table1 <- rbind(Table_test, Table_train)

## Creating Table2. Only selects the mean() and std()
TblNames <- c("subject","labels", as.character(features[,2]))
Table2 <- Table1[c(1,2,grep("mean\\(|std\\(",TblNames))]

## Creating Table3. Names the labels with activity names.
names(Table2) <- TblNames[c(1,2,grep("mean\\(|std\\(",TblNames))]
Table3 <- Table2%>%
        merge(activities,by.x = "labels",by.y = "index", all = FALSE)%>%
        select(-labels)%>%
        arrange(subject, activities)

## Creating Table 4. It is already created from last step 
Table4 <- Table3

## Creating Table 5.  
Table5 <- Table4%>%
        gather(key = signal_parameter_coordinate, value, -subject, -activities)%>%
        separate(signal_parameter_coordinate, into = c("signal", "parameter", "coordinate"))%>%
        dcast(subject + activities + signal + coordinate ~ parameter, mean)%>%
        arrange(subject, activities)

## Ordering signals in a organised way
Table5$signal <- as.factor(Table5$signal)
order_signal <- readLines("./UCI HAR Dataset/features_info.txt", skip = 23 )   
order_signal <- data.frame(signal = order_signal[13:29])%>%
        separate(signal, into = c("signal","coord"), extra = "drop")%>%
        select(signal)
levels(Table5$signal) <- order_signal$signal

## Creating a text file
write.table(Table5, "./UCI_tidy.txt", row.name = FALSE)




