## Course project of "Getting and Cleaning Data"
## Read data from txt files, merge all data into one data set, 
## extract some cols, change colnames, replace some of the contents,
## calculate the average of variables for each activity and each subject

run_analysis <-function(){
  ## Load all the files needed
  yTest<-read.table("UCI HAR Dataset/test/y_test.txt")
  xTest<-read.table("UCI HAR Dataset/test/x_test.txt")
  subjectTest<-read.table("UCI HAR Dataset/test/subject_test.txt")
  subjectTrain<-read.table("UCI HAR Dataset/train/subject_train.txt")
  yTrain<-read.table("UCI HAR Dataset/train/y_train.txt")
  xTrain<-read.table("UCI HAR Dataset/train/x_train.txt")  
  activityLabels<-read.table("UCI HAR Dataset/activity_labels.txt")
  features <- read.table("UCI HAR Dataset/features.txt")
  ##Step1: Merges all the data into a dataframe "data0"
  data0<-cbind(rbind(xTrain,xTest),rbind(subjectTrain,subjectTest),rbind(yTrain,yTest))
  
  ##Step 2: Extracts only the measurements related with mean or standard deviation(std)
  ## New dataframe is called "data"
  colNo1<-grep(".mean.",features$V2)
  colNo2<-grep(".std.",features$V2)
  colNo<-c(colNo1,colNo2)
  data <- data0[,c(colNo,562:563)]
  
  ##Step 3: Uses descriptive activity names to name the activities in the data set
  ## "()" and "-" are removed from the colume names.
  colName0<-gsub("\\()","",as.character(features$V2[colNo]))
  colName0<-gsub("-","",colName0)
  colnames(data)<-c(colName0,"Subject","Activity")
  
  ##Step 4: Appropriately labels the data set with descriptive activity names. 
  ## Change the col "Activity" into factor and then change the levels
  data$Activity<-as.factor(data$Activity)
  levels(data$Activity)=activityLabels$V2
  
  ##Step 5: Creates a tidy data set with the average of each variable for each activity and each subject.
  ## ref.lecture notes 3.4 melting data frames
  library(reshape2)
  datamelt<-melt(data,id=c("Subject","Activity"),measure.vars=colName0)
  tidydata<-dcast(datamelt,Subject+Activity~variable,mean)
  
  ## output
  tidydata
}
