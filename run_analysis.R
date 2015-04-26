## FILE: run_analysis.R
## April 2015
##
## Written for submission as solution to course project requirement
##  for Johns Hopkins University Coursera class Getting and Cleaning Data.
##
## REQUIREMENT NOTE 1: This file must be in the same directory as the
##  "activities_labels.txt" file from the data set. This is the top-level of the data.
## REQUIREMENT NOTE 2: This script requires the 'dplyr' package to be installed.
##
## Please note: I have not yet taken the Reproducible Research class nor
##  the Exploratory Data Analysis class. The current prerequisites listing
##  does not mention those classes as being helpful for this Getting and
##  Cleaning Data class. Therefore this script and the README.md file
##  are likely not formatted as well as they should be.

Num_of_Steps <- 7
step_num <- 1
cat("\014")

## User introduction
print("Starting run_analysis.R.")
print("")
print("This script must be placed in the same")
print("directory as the data's file, activity_labels.txt.")
print("")

## Reading in tables
print(paste0("Step ", step_num, " of ", Num_of_Steps, ": Reading in tables.")); step_num <- step_num + 1

activity_labels <- read.table(file = "./activity_labels.txt")
measurement_names <- read.table(file = "./features.txt")
subject_train <- read.table(file = "./train/subject_train.txt")
subject_test <- read.table(file = "./test/subject_test.txt")
activities_train <- read.table(file = "./train/y_train.txt")
activities_test <- read.table(file = "./test/y_test.txt")
data_test <- read.table(file = "./test/X_test.txt")
data_train <- read.table(file = "./train/X_train.txt")

## Renaming tables measurements
print(paste0("Step ", step_num, " of ", Num_of_Steps, ": Renaming tables measurements.")); step_num <- step_num + 1

names_vector <- as.character(measurement_names$V2)
data_test <- setNames(data_test, names_vector)
data_train <- setNames(data_train, names_vector)

## extracting needed measurements
print(paste0("Step ", step_num, " of ", Num_of_Steps, ": Extracting needed measurements.")); step_num <- step_num + 1

data_test <- data_test[,grepl("mean|std", colnames(data_test))]
data_test <- data_test[,!grepl("Freq", colnames(data_test))]
data_train <- data_train[,grepl("mean|std", colnames(data_train))]
data_train <- data_train[,!grepl("Freq", colnames(data_train))]

## Combining test tables and training tables
##  (merge insists on re-arranging the order of the rows,
##  so code must account for that by recording an order first
##  and using that later to recover the original order.)
print(paste0("Step ", step_num, " of ", Num_of_Steps, ": Combining test tables and training tables.")); step_num <- step_num + 1

activities_test$id <- 1:nrow(activities_test)
activities_train$id <- 1:nrow(activities_train)

merged_test_activities <- merge(activities_test, activity_labels, by.x = "V1", by.y = "V1")
merged_train_activities <- merge(activities_train, activity_labels, by.x = "V1", by.y = "V1")

ordered_merged_test_act <- merged_test_activities[order(merged_test_activities$id), ]
ordered_merged_train_act <- merged_train_activities[order(merged_train_activities$id), ]


data_test <- cbind(subject = subject_test$V1, activity = ordered_merged_test_act$V2, data_test)
data_train <- cbind(subject = subject_train$V1, activity = ordered_merged_train_act$V2, data_train)

## Merging test and training data
print(paste0("Step ", step_num, " of ", Num_of_Steps, ": Merging test and training data.")); step_num <- step_num + 1

total_data <- rbind(data_test, data_train)

## Organize data as required by project requirements.
print(paste0("Step ", step_num, " of ", Num_of_Steps, ": Organizing data.")); step_num <- step_num + 1

suppressWarnings(library(dplyr))  ## Load dplyr library

total_data$subject <- as.factor(total_data$subject)
tidy_data <- total_data %>% group_by(activity, subject) %>% summarise_each(funs(mean))

## Rename columns to reflect the taking of the means above

col_indicies <- 3:ncol(tidy_data)
for(i in col_indicies)names(tidy_data)[names(tidy_data)==names(tidy_data)[i]]=paste0("Avg-", names(tidy_data)[i])

## Write file of final product to disk.
## Data written to format similar to the input data format.
## ***  Add a comment character "#" in front of line if you don't want file to actually be written.  ***
print(paste0("Step ", step_num, " of ", Num_of_Steps, ": Writing final output to disk.")); step_num <- step_num + 1

write.table(tidy_data, file = "./tidy_data.txt", sep = " ", row.names = FALSE)
