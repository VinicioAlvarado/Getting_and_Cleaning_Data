# Library

library(plyr)
library(data.table)
setwd("~/DataScience/CourseraDS/CursoR/Data/UCI HAR Dataset/")
#1. Merges the training and the test sets to create one data set.
# Loading raw data sets.

subjectTrain = read.table("~/DataScience/CourseraDS/CursoR/Data/UCI HAR Dataset/train/subject_train.txt",header=FALSE)
xTrain = read.table("~/DataScience/CourseraDS/CursoR/Data/UCI HAR Dataset/train/X_train.txt",header=FALSE)
yTrain = read.table("~/DataScience/CourseraDS/CursoR/Data/UCI HAR Dataset/train/y_train.txt",header=FALSE)

subjectTest = read.table("~/DataScience/CourseraDS/CursoR/Data/UCI HAR Dataset/test/subject_test.txt",header=FALSE)
xTest = read.table("~/DataScience/CourseraDS/CursoR/Data/UCI HAR Dataset/test/X_test.txt",header=FALSE)
yTest = read.table("~/DataScience/CourseraDS/CursoR/Data/UCI HAR Dataset/test/y_test.txt",header=FALSE)

# Organizing and combining raw data sets into single one.

xDataSet <- rbind(xTrain, xTest)
yDataSet <- rbind(yTrain, yTest)
subjectDataSet <- rbind(subjectTrain, subjectTest)
dim(xDataSet)

dim(yDataSet)

dim(subjectDataSet)

# 2. Extract only the measurements on the mean and standard deviation for each measurement.
# xData subset based on the logical vector to keep only desired columns, i.e. mean() and std().

xDataSet_mean_std <- xDataSet[, grep("-(mean|std)\\(\\)", read.table("features.txt")[, 2])]
names(xDataSet_mean_std) <- read.table("features.txt")[grep("-(mean|std)\\(\\)", read.table("features.txt")[, 2]), 2] 
View(xDataSet_mean_std)
dim(xDataSet_mean_std)

#3. Use descriptive activity names to name the activities in the data set.
yDataSet[, 1] <- read.table("activity_labels.txt")[yDataSet[, 1], 2]
names(yDataSet) <- "Activity"
View(yDataSet)

# 4. Appropriately label the data set with descriptive activity names.

names(subjectDataSet) <- "Subject"
summary(subjectDataSet)

# Organizing and combining all data sets into single one.

singleDataSet <- cbind(xDataSet_mean_std, yDataSet, subjectDataSet)

# Defining descriptive names for all variables.

names(singleDataSet) <- make.names(names(singleDataSet))
names(singleDataSet) <- gsub('Acc',"Acceleration",names(singleDataSet))
names(singleDataSet) <- gsub('GyroJerk',"AngularAcceleration",names(singleDataSet))
names(singleDataSet) <- gsub('Gyro',"AngularSpeed",names(singleDataSet))
names(singleDataSet) <- gsub('Mag',"Magnitude",names(singleDataSet))
names(singleDataSet) <- gsub('^t',"TimeDomain.",names(singleDataSet))
names(singleDataSet) <- gsub('^f',"FrequencyDomain.",names(singleDataSet))
names(singleDataSet) <- gsub('\\.mean',".Mean",names(singleDataSet))
names(singleDataSet) <- gsub('\\.std',".StandardDeviation",names(singleDataSet))
names(singleDataSet) <- gsub('Freq\\.',"Frequency.",names(singleDataSet))
names(singleDataSet) <- gsub('Freq$',"Frequency",names(singleDataSet))

View(singleDataSet)

# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

names(singleDataSet)

Data2<-aggregate(. ~Subject + Activity, singleDataSet, mean)
Data2<-Data2[order(Data2$Subject,Data2$Activity),]
write.table(Data2, file = "tidydata.txt",row.name=FALSE)














#### h####
library(reshape2)

#reading features and activity data
features <- read.table("~/DataScience/CourseraDS/CursoR/Data/UCI HAR Dataset/features.txt")
activities <- read.table("~/DataScience/CourseraDS/CursoR/Data/UCI HAR Dataset/activity_labels.txt")

#reading train data
train <- read.table("~/DataScience/CourseraDS/CursoR/Data/UCI HAR Dataset/train/X_train.txt") #features data
colnames(train) <- features$V2 #descriptive column names for data (STEP 4)
y_train <- read.table("~/DataScience/CourseraDS/CursoR/Data/UCI HAR Dataset/train/y_train.txt") #activity labels
train$activity <- y_train$V1
subject_train <- read.table("~/DataScience/CourseraDS/CursoR/Data/UCI HAR Dataset/train/subject_train.txt") #subjects
train$subject <- factor(subject_train$V1)


#reading test data
test <- read.table("~/DataScience/CourseraDS/CursoR/Data/UCI HAR Dataset/test/X_test.txt")
colnames(test) <- features$V2
y_test <- read.table("~/DataScience/CourseraDS/CursoR/Data/UCI HAR Dataset/test/y_test.txt") 
test$activity <- y_test$V1
subject_test <- read.table("~/DataScience/CourseraDS/CursoR/Data/UCI HAR Dataset/test/subject_test.txt")
test$subject <- factor(subject_test$V1)

#merge train and test sets (STEP 1)
dataset <- rbind(test, train) 

#filter column names (STEP 2)
column.names <- colnames(dataset)
#get only columns for standard deviation and mean values, also saves activity and subject values 
column.names.filtered <- grep("std\\(\\)|mean\\(\\)|activity|subject", column.names, value=TRUE)
dataset.filtered <- dataset[, column.names.filtered] 

#adding descriptive values for activity labels (STEP 3)
dataset.filtered$activitylabel <- factor(dataset.filtered$activity, labels= c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING"))

#creating a tidy dataset with mean values for each subject and activity
features.colnames = grep("std\\(\\)|mean\\(\\)", column.names, value=TRUE)
dataset.melt <-melt(dataset.filtered, id = c('activitylabel', 'subject'), measure.vars = features.colnames)
dataset.tidy <- dcast(dataset.melt, activitylabel + subject ~ variable, mean)

#creating a tidy dataset file  
write.table(dataset.tidy, file = "tidydataset.txt" row.names = FALSE)