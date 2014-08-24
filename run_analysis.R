#Get Activity Name function
get_act_name<-function(x){
  switch(x,"1"={"WALKING"}
         ,"2"={"WALKING_UPSTAIRS"}
         ,"3"={"WALKING_DOWNSTAIRS"}
         ,"4"={"SITTING"}
         ,"5"={"STANDING"}
         ,"6"={"LAYING"})
}

#Reading Training Data
atrain<-scan("./UCI HAR Dataset/train/X_train.txt")
train.data<-matrix(atrain,ncol=561,byrow=T)

traindf<-data.frame(train.data)

#Reading Test Data
atest<-scan("./UCI HAR Dataset/test/X_test.txt")
test.data<-matrix(atest,ncol=561,byrow=T)

testdf<-data.frame(test.data)

#####################################################################
#STEP1.Merges the training and the test sets to create one data set.
#####################################################################
combinedf<-rbind(traindf,testdf)

#Feature Names
f<-read.csv("./UCI HAR Dataset/features.txt",sep=" ",header=F,as.is=T)
n<-f[,2]
n<-gsub("\\(", "",n)
n<-gsub("\\)", "",n)
n<-gsub("-", "_",n)
n<-gsub(",", "_",n)

#Give Names to columns of train data
colnames(combinedf)<-n


#Read activity labels and attach these lables with traindf
train_label<-scan("./UCI HAR Dataset/train/y_train.txt")
train_subject_info<-scan("./UCI HAR Dataset/train/subject_train.txt")
datatype_train<-rep("train",length(train_label))

#Read activity labels and attach these lables with testdf
test_label<-scan("./UCI HAR Dataset/test/y_test.txt")
test_subject_info<-scan("./UCI HAR Dataset/test/subject_test.txt")
datatype_test<-rep("test",length(test_label))

#######################################################################
#Combine both activity label,subject info and Add Columns to combine data
#######################################################################

activity_label<-c(train_label,test_label)
subject_info<-c(train_subject_info,test_subject_info)
datatype<-c(datatype_train,datatype_test)
activity_name<-sapply(activity_label,get_act_name)

#Extract only mean and std variables 

combinedf<-cbind(datatype,subject.Code=subject_info,activity.Code=activity_label,combinedf)

cname<-names(combinedf)

#2.Extracts only the measurements on the mean and standard deviation for each measurement. 

comMeanStdDf<-combinedf[,grepl("[Mm]ean",cname) | grepl("[Ss]td",cname)]

#3.Uses descriptive activity names to name the activities in the data set

comMeanStdDf<-cbind(datatype,subject.Code=subject_info,activity.name=activity_name,comMeanStdDf)

#4.Appropriately labels the data set with descriptive variable names. 

#5.Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
tidyData<-aggregate(.~subject.Code+activity.name,data=comMeanStdDf,mean)

tidyData$datatype<-NULL

#Saving Final dataset
write.table(tidyData,"tidydata.csv",sep=",",row.names=FALSE)
