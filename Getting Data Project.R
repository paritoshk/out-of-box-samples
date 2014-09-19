# Abu Nayeem
# Getting and Cleaning Data Project
# make sure all files are in working directory

YTest<- read.table("Y_test.txt")
XTest<- read.table("X_test.txt")
SubjectTest<- read.table("subject_test.txt")
YTrain<- read.table("Y_train.txt")
XTrain<- read.table("X_train.txt")
SubjectTrain<- read.table("subject_train.txt")

#First we will dat-amine this massive dataset, first I need to obtain connections
# Connection1: the features.txt has 561 labels which is the exact amount for train data
# Connection2: the activities txt has exactly 6 variables making it the label for the YTrain/Test 
# this should be motivation to get going, but first lets import the other data

Features<- read.table("features.txt")
colnames(XTrain)<-t(Features[2])
colnames(XTest)<-t(Features[2])
# Notice the transpose feature!

#Let's merge the smaller data.frames
XTrain$activities<- YTrain[,1]
XTrain$participants<- SubTrain[,1]
XTest$activities<- YTest[,1]
XTest$participants<- SubTest[,1]

#Note: YTrain[1] takes a list! not a vector, made it difficult to merge

#Assingment1 
#Merge dataset- It should have added observations
Master<-rbind(XTrain,XTest)

#remove duplicate columns; first I check if there are duplicates and second line says take columns where duplicates is false
duplicated(colnames(Master))
Master<-Master[, !duplicated(colnames(Master))]

#Assignment2

#It is vague about what features we are after, I will be interested in the all the means and standard deviations not related to frequency 

#Strategy- I am searching for the string mean and see which ones I prefer and then choose accordingly
#The final result is Matrix full of mean values, we do the same for STD
Mean<-grep("mean", names(Master), value = TRUE, fixed=TRUE)
Mean
Mean<-grep("mean()", names(Master), value = FALSE, fixed=TRUE)
#In addition, we need to include 555:559 as they have means and are associated with the gravity terms
Mean<-append(Mean,471:477)
InstrumentMeanMatrix<-Master[Mean]

#Do similar procedure for standard deviation 
STD<-grep("std", names(Master), value = TRUE)
STD
STD<-grep("std()", names(Master), value = FALSE)
InstrumentSTDMatrix<-Master[STD]

#COMPLETE I have extracted two matrices that holds the mean and std respectively

#Assignment 3

#We need to change the activities place value(numeric) into something descriptive variables

#for computation ease- convert to character variable first
Master$activities<- as.character(Master$activities)

# We alter the strings accordingly by choosing the cell of interest and where to implement the change
Master$activities[Master$activities==1]<-"Walking"
Master$activities[Master$activities==2]<-"Walking Upstairs"
Master$activities[Master$activities==3]<-"Walking Downstairs"
Master$activities[Master$activities==4]<-"Sitting"
Master$activities[Master$activities==5]<-"Standing"
Master$activities[Master$activities==6]<-"Laying"

#Convert to factors as it is more useful
Master$activities<- as.factor(Master$activities)
#Check my work
summary(Master$activities)
#COMPLETE

# Assignment 4

#Q: Appropriately labels the data set with descriptive variable names. 

#Exactly what needs to be labeled?
names(Master)
#Here is a list of unclear acronyms and pairs; Note I will not change the operation name because it explains the command that was used and they are lengthy; for more info read the data library or R help
#^t-time; ^f-frequency; Acc-Accelerator; Gyro-Gyroscope; Mag-Magnitude 

names(Master)<- gsub("Acc","Accelerator", names(Master))
names(Master)<- gsub("Mag","Magnitude", names(Master))
names(Master)<- gsub("Gyro","Gyroscope", names(Master))
names(Master)<- gsub("^t","time", names(Master))
names(Master)<- gsub("^f","frequency", names(Master))
# note the ^ symbol marks the bigging of cell

# finally lets change the subjects into factor variablesl similar to assignment 3

#for computation ease- convert to character variable first
Master$participants<- as.character(Master$participants)

# We alter the strings accordingly by choosing the cell of interest and where to implement the change
Master$participants[Master$participants==1]<-"Participant 1"
Master$participants[Master$participants==2]<-"Participant 2"
Master$participants[Master$participants==3]<-"Participant 3"
Master$participants[Master$participants==4]<-"Participant 4"
Master$participants[Master$participants==5]<-"Participant 5"
Master$participants[Master$participants==6]<-"Participant 6"
Master$participants[Master$participants==7]<-"Participant 7"
Master$participants[Master$participants==8]<-"Participant 8"
Master$participants[Master$participants==9]<-"Participant 9"
Master$participants[Master$participants==10]<-"Participant 10"
Master$participants[Master$participants==11]<-"Participant 11"
Master$participants[Master$participants==12]<-"Participant 12"
Master$participants[Master$participants==13]<-"Participant 13"
Master$participants[Master$participants==14]<-"Participant 14"
Master$participants[Master$participants==15]<-"Participant 15"
Master$participants[Master$participants==16]<-"Participant 16"
Master$participants[Master$participants==17]<-"Participant 17"
Master$participants[Master$participants==18]<-"Participant 18"
Master$participants[Master$participants==19]<-"Participant 19"
Master$participants[Master$participants==20]<-"Participant 20"
Master$participants[Master$participants==21]<-"Participant 21"
Master$participants[Master$participants==22]<-"Participant 22"
Master$participants[Master$participants==23]<-"Participant 23"
Master$participants[Master$participants==24]<-"Participant 24"
Master$participants[Master$participants==25]<-"Participant 25"
Master$participants[Master$participants==26]<-"Participant 26"
Master$participants[Master$participants==27]<-"Participant 27"
Master$participants[Master$participants==28]<-"Participant 28"
Master$participants[Master$participants==29]<-"Participant 29"
Master$participants[Master$participants==30]<-"Participant 30"
#Convert to factors as it is more useful
Master$participants<- as.factor(Master$participants)

# COMPLETE. One remark it is more descriptive but a lot more wordy.

# Assignment 5- Create a tidy data set
# More specifically From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#Translation: we want the mean of each variable, subset by person and activity; Note this is crucial in how to approach this

library(plyr)
library(data.table)
Master.dt = data.table(Master)

#This takes the mean of every column broken down by participants and activities
TidyData<-Master.dt[,lapply(.SD, mean),by='participants,activities']

# the number of observations is correct 30 times 6= 180

write.table(TidyData,file="Tidy.txt", row.names=FALSE)

# Note nnother way to tidy data is shown below 
#mean <- function(x) mean(x)
#TidyMaster<- ddply(Master.dt,.(participants,activities),colwise(mean,c(1:476)))


