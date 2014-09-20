
#Abu Nayeem Getting and Cleaning Project Straight Code

YTest <- read.table("Y_test.txt")
XTest <- read.table("X_test.txt")
SubjectTest <- read.table("subject_test.txt")
YTrain <- read.table("Y_train.txt")
XTrain <- read.table("X_train.txt")
SubjectTrain <- read.table("subject_train.txt")
Features <- read.table("features.txt")
colnames(XTrain) <- t(Features[2])
colnames(XTest) <- t(Features[2])
# Notice the transpose feature!

#Let's complete one data source first
#It's having faith the merge of X and Y Train set allign since there is no common ID

XTrain$activities <- YTrain[, 1]
XTrain$participants <- SubjectTrain[, 1]
XTest$activities <- YTest[, 1]
XTest$participants <- SubjectTest[, 1]

#Assingment1 

Master <- rbind(XTrain, XTest)
duplicated(colnames(Master))
Master <- Master[, !duplicated(colnames(Master))]
#Complete

#Assignment2
# Mean
Mean <- grep("mean()", names(Master), value = FALSE, fixed = TRUE)
#In addition, we need to include 555:559 as they have means and are associated with the gravity terms
Mean <- append(Mean, 471:477)
InstrumentMeanMatrix <- Master[Mean]

#STD
STD <- grep("std()", names(Master), value = FALSE)
InstrumentSTDMatrix <- Master[STD]

#COMPLETE 

# Assignment 3
# Changing the class is useful for replacing strings
Master$activities <- as.character(Master$activities)

Master$activities[Master$activities == 1] <- "Walking"
Master$activities[Master$activities == 2] <- "Walking Upstairs"
Master$activities[Master$activities == 3] <- "Walking Downstairs"
Master$activities[Master$activities == 4] <- "Sitting"
Master$activities[Master$activities == 5] <- "Standing"
Master$activities[Master$activities == 6] <- "Laying"

Master$activities <- as.factor(Master$activities)

#COMPLETE

# Assignment 4

names(Master)
#Here is a list of unclear acronyms and pairs; Note I will not change the operation name because it explains the command that was used and they are lengthy; for more info read the data library or R help
#^t-time; ^f-frequency; Acc-Accelerator; Gyro-Gyroscope; Mag-Magnitude 

names(Master) <- gsub("Acc", "Accelerator", names(Master))
names(Master) <- gsub("Mag", "Magnitude", names(Master))
names(Master) <- gsub("Gyro", "Gyroscope", names(Master))
names(Master) <- gsub("^t", "time", names(Master))
names(Master) <- gsub("^f", "frequency", names(Master))

#Change Participants names
Master$participants <- as.character(Master$participants)

Master$participants[Master$participants == 1] <- "Participant 1"
Master$participants[Master$participants == 2] <- "Participant 2"
Master$participants[Master$participants == 3] <- "Participant 3"
Master$participants[Master$participants == 4] <- "Participant 4"
Master$participants[Master$participants == 5] <- "Participant 5"
Master$participants[Master$participants == 6] <- "Participant 6"
Master$participants[Master$participants == 7] <- "Participant 7"
Master$participants[Master$participants == 8] <- "Participant 8"
Master$participants[Master$participants == 9] <- "Participant 9"
Master$participants[Master$participants == 10] <- "Participant 10"
Master$participants[Master$participants == 11] <- "Participant 11"
Master$participants[Master$participants == 12] <- "Participant 12"
Master$participants[Master$participants == 13] <- "Participant 13"
Master$participants[Master$participants == 14] <- "Participant 14"
Master$participants[Master$participants == 15] <- "Participant 15"
Master$participants[Master$participants == 16] <- "Participant 16"
Master$participants[Master$participants == 17] <- "Participant 17"
Master$participants[Master$participants == 18] <- "Participant 18"
Master$participants[Master$participants == 19] <- "Participant 19"
Master$participants[Master$participants == 20] <- "Participant 20"
Master$participants[Master$participants == 21] <- "Participant 21"
Master$participants[Master$participants == 22] <- "Participant 22"
Master$participants[Master$participants == 23] <- "Participant 23"
Master$participants[Master$participants == 24] <- "Participant 24"
Master$participants[Master$participants == 25] <- "Participant 25"
Master$participants[Master$participants == 26] <- "Participant 26"
Master$participants[Master$participants == 27] <- "Participant 27"
Master$participants[Master$participants == 28] <- "Participant 28"
Master$participants[Master$participants == 29] <- "Participant 29"
Master$participants[Master$participants == 30] <- "Participant 30"

Master$participants <- as.factor(Master$participants)

# COMPLETE. One remark it is more descriptive but a lot more wordy.

# Assignment 5- Create a tidy data set

library(plyr)
library(data.table)
Master.dt = data.table(Master)
#This takes the mean of every column broken down by participants and activities
TidyData <- Master.dt[, lapply(.SD, mean), by = 'participants, activities']
write.table(TidyData, file = "Tidy.txt", row.names = FALSE)

#Complete [see annotations in ReadMe]