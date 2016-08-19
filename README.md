# Getting_and_cleaning_data_project
This is the project for Coursera Getting and Cleaning data. It includes run_analysis.R and Readme.md files.

Prepare: Download and unzip files from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

 to working directory. Place run_analysis.R in the working directory. Run it and it would create a file called second_dt.txt, which includes the mean of original data by activity types.
 
##0. Read the files and name the column names
features     = read.table('./UCI HAR Dataset/features.txt')

activityLabels = read.table('./UCI HAR Dataset/activity_labels.txt')

subjectTrain = read.table('./UCI HAR Dataset/train/subject_train.txt')

subjectTest = read.table('./UCI HAR Dataset/test/subject_test.txt')

xTrain       = read.table('./UCI HAR Dataset/train/X_train.txt')

xTest       = read.table('./UCI HAR Dataset/test/X_test.txt')

yTrain       = read.table('./UCI HAR Dataset/train/y_train.txt')

yTest       = read.table('./UCI HAR Dataset/test/y_test.txt')

colnames(activityLabels) = c('activityN','activityType')

colnames(subjectTrain) = "subjectId";colnames(subjectTest) = "subjectId"

colnames(xTrain) = features[,2] ;colnames(xTest)= features[,2]

colnames(yTrain) = "activityN"; colnames(yTest) = "activityN"


## 1. Merges the training and the test sets to create
##    one data set (into dt).

x=rbind(xTrain, xTest)

y=rbind(yTrain,yTest)

subject = rbind(subjectTrain, subjectTest)

dt=cbind(y,subject, x)


## 2. Extracts only the measurements on the mean and 
##    standard deviation for each measurement. (into extracted_dt)

colnames= colnames(dt)

extracted = grepl("mean\\(\\)|std\\(\\)", colnames)

extracted[1:2]=TRUE

extracted_dt=dt[extracted==TRUE]


## 3. Uses descriptive activity names to name the activities in the data set

extracted_dt=merge(extracted_dt, activityLabels, by.y = "activityN", all.x = TRUE)

extracted_colnames=colnames(extracted_dt)

## 4. Appropriately labels the data set with descriptive variable names.

for (i in 1:length(extracted_colnames)) 

{

  extracted_colnames[i] = gsub("\\()","",extracted_colnames[i])
  
  extracted_colnames[i] = gsub("-std$","StdDev",extracted_colnames[i])
  
  extracted_colnames[i] = gsub("-mean","Mean",extracted_colnames[i])
  
  extracted_colnames[i] = gsub("^(t)","time",extracted_colnames[i])
  
  extracted_colnames[i] = gsub("^(f)","freq",extracted_colnames[i])
  
  extracted_colnames[i] = gsub("([Gg]ravity)","Gravity",extracted_colnames[i])
  
  extracted_colnames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",extracted_colnames[i])
  
  extracted_colnames[i] = gsub("[Gg]yro","Gyro",extracted_colnames[i])
  
  extracted_colnames[i] = gsub("AccMag","AccMagnitude",extracted_colnames[i])
  
  extracted_colnames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",extracted_colnames[i])
  
  extracted_colnames[i] = gsub("JerkMag","JerkMagnitude",extracted_colnames[i])
  
  extracted_colnames[i] = gsub("GyroMag","GyroMagnitude",extracted_colnames[i])
  
}

extracted_colnames=colnames(extracted_dt)

## 5. From the data set in step 4, creates a second, 
##    independent tidy data set with the average of each
##    variable for each activity and each subject. 
##    (saved as "second_dt.txt" )

temp=extracted_dt[,names(extracted_dt) != "activityType"]

second_dt= aggregate(temp[,names(temp) != c("activityN","subjectId")],by=list(activityN=temp$activityN,subjectId = temp$subjectId),mean)

second_dt= merge(second_dt,activityLabels, by.x="activityN",by.y="activityN",all.x=TRUE)

write.table(second_dt, './second_dt.txt',row.names=FALSE,sep='\t')


