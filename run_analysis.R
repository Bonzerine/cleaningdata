## Note: install package "dplyr" if not already
## clear workspace
rm(list = ls())
## setting directory
setwd("X:/R/Project")

##############################################################################################
## obtaining variable names
feature <- read.table("X:/R/Project/features.txt", col.names = c("VarNum", "VariableName"))


## obtaining activity labels
activity_labels <-read.table("./activity_labels.txt",col.names = c("activitycode", "description"))

####################################################################################################
## Putting together train data
## These information can be merged together on the order of the observation

## X_train.txt refers to the measurements
x_train <-read.table("./train/X_train.txt")
## placing variable names on x_train
feature$VarNum <-sub("^", "V", feature$VarNum)
colnames(x_train) <- feature[1:ncol(x_train), "VariableName"]

## y_train.txt refers to the activities
y_train <-read.table("./train/y_train.txt", col.names = "activitycode")
y_train_updated <- merge(y_train, activity_labels, by="activitycode")

## subject_train contains information to identify the subjects
subject_train <- read.table("./train/subject_train.txt", col.names = "Subject_Num")

##Merging all the datasets together
traindata <- cbind(y_train_updated, subject_train, x_train)

####################################################################################################
##Putting together test data

## X_test.txt refers to the measurements
x_test <-read.table("./test/X_test.txt")
## placing variable names on x_test
##feature$VarNum <-sub("^", "V", feature$VarNum) ## already done above
colnames(x_test) <- feature[1:ncol(x_test), "VariableName"]

## y_test.txt refers to the activities
y_test <-read.table("./test/y_test.txt", col.names = "activitycode")
y_test_updated <- merge(y_test, activity_labels, by="activitycode")

## subject_test contains information to identify the subjects
subject_test <- read.table("./test/subject_test.txt", col.names = "Subject_Num")

##Merging all the datasets together
testdata <- cbind(y_test_updated, subject_test, x_test)

#####################################################################################################
##1. Merges the training and the test sets to create one data set.

alldata <- rbind(traindata, testdata)
## There are invalid characters in the column names
## correct this## There are invalid characters in the column names
valid_column_names <- make.names(names = names(alldata), unique = TRUE, allow_ = TRUE)
names(alldata) <-valid_column_names

##################################################################################################
##2. Extracts only the measurements on the mean and standard deviation
##These are the variables with "mean" or "std"
## use dplyr package to select these variables
library("dplyr")
## convert data to tbl class
alldata_tbl <- tbl_df(alldata)
dim(alldata_tbl)
alldata_tbl_selected <- select(alldata_tbl, activitycode:Subject_Num, contains("mean"), contains("std"))
dim(alldata_tbl_selected)
##################################################################################################
##3. Uses descriptive activity names to name the activities in the data set
##4. Appropriately labels the data set with descriptive variable names. 
##Step 3 and 4 are done above when activity labels and descriptive variable names were added.

#################################################################################################
##5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
##description is factor, change it to character
alldata_tbl_selected$description <- as.character(alldata_tbl_selected$description)
alldata_tbl_selected <- rename(alldata_tbl_selected, activitydesc = description)
finaldata <- alldata_tbl_selected %>% 
  group_by(activitycode, activitydesc, Subject_Num) %>%
    summarise_each(funs(mean))

## Creating the dataset in csv file
write.table(finaldata, "finaldata.txt",row.names=FALSE)
###############################################################################################