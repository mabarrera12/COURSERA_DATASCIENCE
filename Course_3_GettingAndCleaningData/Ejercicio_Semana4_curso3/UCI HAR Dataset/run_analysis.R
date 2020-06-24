#run_analysis.R -by Alejandra Barrera
# You should create one R script called run_analysis.R that does the following.
# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement.
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names.
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

if (!require(data.table)) {install.packages("data.table")}
library(data.table)

## TEST
# Read files.
xtest <- read.table("./test/X_test.txt")
ytest <- read.table("./test/y_test.txt")

# Read features and assign them to xtest
features_label <- read.table("./features.txt")[,2]
setnames(xtest,features_label)

# Read the activity labels
Act_labels <- read.table("./activity_labels.txt")[,2]
ytest[,2] <- Act_labels[ytest[,1]]
setnames(ytest, c("Act_id","Activity"))

# Read the subject test file
subjecttest <- read.table("./test/subject_test.txt")
setnames(subjecttest, c("Subject"))

# Features with mean or std
mean_std_features <- grepl("mean|std", features_label)
xtest <- xtest[,mean_std_features]

# Create a table with the data
data_test <- cbind(subjecttest,ytest,xtest)

## TRAIN
# Read files. 
xtrain <- read.table("./train/X_train.txt")
ytrain <- read.table("./train/y_train.txt")

# Assign features to xtrain and y train
setnames(xtrain,features_label)
ytrain[,2] <- Act_labels[ytrain[,1]]
setnames(ytrain, c("Act_id","Activity"))

# Read the subject test file
subjecttrain <- read.table("./train/subject_train.txt")
setnames(subjecttrain, c("Subject"))

# Features with mean or std
xtrain <- xtrain[,mean_std_features]

# Create a table with the data
data_train <- cbind(subjecttrain,ytrain,xtrain)

data_full <- rbind(data_test,data_train)

#Now we have to calculate the mean. 
id_vars      <- names(data_full)[1:3]
measure_vars <- names(data_full)[4:length(data_full)]
data_full2   <- melt(data_full, id.vars = id_vars, measure.vars= measure_vars)

library(reshape2)
data_full3   <- dcast(data_full2, Subject + Activity ~ variable, mean)
write.table(data_full, file="Cleaned_data.txt", row.names = FALSE)

