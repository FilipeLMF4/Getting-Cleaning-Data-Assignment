## Getting and Cleaning Data Assignment

##Load necessary libraries
library(dplyr)

##Download files and set working directory
if(!file.exists("./Assignment")) {dir.create("./Assignment")}
setwd("./Assignment")

fileURL <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
download.file(fileURL,'data.zip')
unzip('data.zip')

dataDir <- './UCI HAR Dataset/'

##Extract features, test and training data
features <- read.table(paste0(dataDir,'features.txt'))
x_test <- read.table(paste0(dataDir,'test/X_test.txt'))
x_train <- read.table(paste0(dataDir,'train/X_train.txt'))
y_test <- read.table(paste0(dataDir,'test/y_test.txt'),
                     col.names='activity')
y_train <- read.table(paste0(dataDir,'train/y_train.txt'),
                      col.names = 'activity')
subject_test <- read.table(paste0(dataDir,'test/subject_test.txt'),
                           col.names = 'subject')
subject_train <- read.table(paste0(dataDir,'train/subject_train.txt'),
                            col.names = 'subject')

##1.
##Merge tables into one data frame
test_merged <- cbind(subject_test,y_test,x_test)
train_merged <- cbind(subject_train,y_train,x_train)
merged_df <- rbind(test_merged,train_merged)

##2.
##Extract only the measurements on the mean and standard deviation 
##for each measurement
regexp <- '-(mean|std)\\(\\)'
ind <- grep(regexp,features[,2]) + 2
mean_std_measurements <- merged_df[,ind]

#3.Explicitly name the activities in the data set
act_labels <- read.table(paste0(dataDir,'activity_labels.txt'),
                         col.names = c('activity','activity_expl'))
merged_df <- merge(merged_df,act_labels,by.x = 'activity',by.y = 'activity')

##Remove repeated activity column and rearrange so activity_expl is 1st column
merged_df <- select(merged_df,-activity)
col_idx <- grep('activity_expl',names(merged_df))
final_df <- merged_df[, c(1,col_idx, (2:ncol(merged_df))[-col_idx+1])]

##4.
##Name columns with appropriate labels
colNames <- c('subject','activity',features[,2])
names(final_df) <- colNames

##Add index to columns with same name. Necessary for merging
names(final_df) <- make.unique(names(final_df),sep = "_")

##5.
##Create data set with the average of each variable for each activity and 
##each subject.
grouped_df <- final_df %>%
            group_by(subject,activity) %>%
            summarize_all(funs(mean))

write.table(grouped_df,'Assignment.txt',row.names = FALSE)
