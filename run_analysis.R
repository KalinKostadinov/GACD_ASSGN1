# GACD_Assignment1.R
# Coursera -- Getting and Cleaning Data -- Assignment 1
# Creating a Tidy Dataset
# file created 2014-11-20 by kkostadinov

#  run_analysis.R 
#1 Merges the training and the test sets to create one data set.
#2 Extracts only the measurements on the mean and standard deviation for each measurement. 
#3 Uses descriptive activity names to name the activities in the data set
#4 Appropriately labels the data set with descriptive variable names. 
#5 From the data set in step 4, creates a second, independent tidy data set 
#       with the average of each variable for each activity and each subject.



#Step1 Merges the training and the test sets to create one data set.
#David Hood (Community TA) diagram in the discussion linked below helped how to fit the piece together
#https://class.coursera.org/getdata-009/forum/thread?thread_id=58
#Step1.1 Measurements Sets (readings of the instruments)
measurements_train_data_set<-read.table("./Data/GACD ASSGN1 UCI HAR Dataset/train/X_train.txt")
measurements_test_data_set<-read.table("./Data/GACD ASSGN1 UCI HAR Dataset/test/X_test.txt")

measurements_full_data_set<-rbind(measurements_train_data_set,measurements_test_data_set)

#while we are at it, change the names of the columns with more descriptive ones
features<-read.delim("./Data/GACD ASSGN1 UCI HAR Dataset/features.txt",header=FALSE)
names(measurements_full_data_set)<-unlist(features)


rm(measurements_train_data_set)
rm(measurements_test_data_set)
#End of Step1.1

#Step1.2 Subject Sets (Number codes of the volunteers doing the measuerments)
subject_train_data_set<-read.table("./Data/GACD ASSGN1 UCI HAR Dataset/train/subject_train.txt")
subject_test_data_set<-read.table("./Data/GACD ASSGN1 UCI HAR Dataset/test/subject_test.txt")

subject_full_data_set<-rbind(subject_train_data_set,subject_test_data_set)
names(subject_full_data_set)<-"Subject Code"

rm(subject_train_data_set)
rm(subject_test_data_set)
#End of Step1.2

#Step1.3 Activity Sets (Number codes of the Activity corresponding the measuerments)
activity_train_data_set<-read.table("./Data/GACD ASSGN1 UCI HAR Dataset/train/y_train.txt")
activity_test_data_set<-read.table("./Data/GACD ASSGN1 UCI HAR Dataset/test/y_test.txt")

activity_full_data_set<-rbind(activity_train_data_set,activity_test_data_set)
names(activity_full_data_set)<-"Activity Code"

rm(activity_train_data_set)
rm(activity_test_data_set)
#End of Step1.3

#Step1.4 Create a set with Subject,Activity, and All Measurements as columns
#no index keys, so assuming the three sets are order-coordinated
full_data_set<-cbind(subject_full_data_set,activity_full_data_set,measurements_full_data_set)
#End of Step1.4

rm(measurements_full_data_set)
rm(subject_full_data_set)
rm(activity_full_data_set)

#check what we got
#names(full_data_set)
#dim(full_data_set) #10299 observations of 563 variables
#End of Step1



#Step2 Extracts only the measurements on the mean and standard deviation for each measurement.
#cast features (from Step1) elements as character strings for use in grep
features<-as.character(features[,1])
#look up the indices of the elements that contain either -mean() or -std(); --ignore things like meanFreq
col_idx<-grep("-mean\\(\\)|-std\\(\\)",features)
#shift by 2 to account for the Subject&Activity columns (and add them)
col_idx<-col_idx+2
col_idx<-append(1:2,col_idx)
meanstd_data_set=full_data_set[,col_idx]

rm(full_data_set)
rm(features)

#check what we got
#head(meanstd_data_set)  #look's ok
#dim(meanstd_data_set) #10299 observations of 68 (=2+66 for mean|std) variables
#End of Step2


#Step3 Replace each Activity Code with the respective Activity Name
activities_code2name<-read.table("./Data/GACD ASSGN1 UCI HAR Dataset/activity_labels.txt")
meanstd_data_set[,"Activity Code"]=activities_code2name[meanstd_data_set[,"Activity Code"],2]

#check what we got
#levels(meanstd_data_set[,2])
#End of Step 3


#Step4 Appropriately label the data set with descriptive variable names.
#removing the numbers, the parentheses and the spaces (column names labeled from features.txt in Step1)
x<-names(meanstd_data_set)
x<-gsub("[0-9]|\\(|\\)| ","",x)
names(meanstd_data_set)<-x
rm(x)
#check what we go
#names(meanstd_data_set)
#End of Step4


#5 From the data set in step 4, creates a second, independent tidy data set 
#       with the average of each variable for each activity and each subject.
#will use aggregate with FUN=mean; melt&dcast seems like an overkill

#split the set into part to be aggregated and part to aggregate by
measurements_part=meanstd_data_set[,3:68]
subjects_part=meanstd_data_set[,"SubjectCode"]
activities_part=meanstd_data_set[,"ActivityCode"]

tidy_data_set<-aggregate(measurements_part,by=list(subjects_part,activities_part),FUN=mean)

rm(measurements_part)
rm(subjects_part)
rm(activities_part)
  
#relabel the columns
names(tidy_data_set)<-paste("AVG-",names(tidy_data_set))
names(tidy_data_set)[1]<-"SubjectCode"
names(tidy_data_set)[2]<-"Acvtivity"

#produce the output

write.table(tidy_data_set,"./Data/GACD ASSGN1 UCI HAR Dataset/tidy_data_set.txt",row.names=FALSE)
#check what we got
#dim(tidy_data_set)  #180 obs (6act X 30 subj) of 66 (2+64)
#tidy_data_set[120:125,1:5]
#End of Step5