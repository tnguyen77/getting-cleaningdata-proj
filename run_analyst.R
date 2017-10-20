# 1. Merges the training and the test sets to create one data set. 
# Getting data
features     = read.table('./features.txt',header=FALSE); 
activityType = read.table('./activity_labels.txt',header=FALSE); 
subjectTrain = read.table('./train/subject_train.txt',header=FALSE); 
xTrain       = read.table('./train/x_train.txt',header=FALSE);
yTrain       = read.table('./train/y_train.txt',header=FALSE); 

# Name column data imported
colnames(activityType)  = c('activityId','activityType');
colnames(subjectTrain)  = "subjectId";
colnames(xTrain)        = features[,2]; 
colnames(yTrain)        = "activityId";

# Final training dataset (merging yTrain, subjectTrain, and xTrain)
trainingData = cbind(yTrain,subjectTrain,xTrain);

# Getting test data
subjectTest = read.table('./test/subject_test.txt',header=FALSE); 
xTest       = read.table('./test/x_test.txt',header=FALSE); 
yTest       = read.table('./test/y_test.txt',header=FALSE); 

# Name columns to test data imported
colnames(subjectTest) = "subjectId";
colnames(xTest)       = features[,2]; 
colnames(yTest)       = "activityId";


# Final test dataset (merging the xTest, yTest and subjectTest data)
testData = cbind(yTest,subjectTest,xTest);


# Merging training and test data for final dataset
finalData = rbind(trainingData,testData);

# Create a vector for the column names from the finalData for desired mean() & stddev() columns to use
colNames  = colnames(finalData); 

# 2. Extract only the measurements on the mean and standard deviation for each measurement. 

# Create a logicalVector with TRUE values for the ID, mean() & stddev() columns and FALSE for others
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

# Subset finalData table using logicalVector for only desired columns
finalData = finalData[logicalVector==TRUE];

# 3. Use descriptive activity names to name the activities in the data set

# Merge finalData with acitivityType table to include descriptive activity names
finalData = merge(finalData,activityType,by='activityId',all.x=TRUE);

# Updating colNames for new column names
colNames  = colnames(finalData); 

# 4. Appropriately label the data set with descriptive activity names. 

# Cleaning up the variable names
for (i in 1:length(colNames)) 
   {
         colNames[i] = gsub("\\()","",colNames[i])
         colNames[i] = gsub("-std$","StdDev",colNames[i])
         colNames[i] = gsub("-mean","Mean",colNames[i])
         colNames[i] = gsub("^(t)","time",colNames[i])
         colNames[i] = gsub("^(f)","freq",colNames[i])
         colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
         colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
         colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
         colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
         colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
         colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
         colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
     };
# Reassigning the new descriptive column names to the finalData set
colnames(finalData) = colNames;

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Create a new finalDataNoActivityType table with no activityType column
finalDataNoActivityType  = finalData[,names(finalData) != 'activityType'];

# Summarizing the finalDataNoActivityType table to show only the mean of each variable for each activity and each subject
tidyData    = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);

# Merging the tidyData and activityType to have descriptive acitvity names
tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE);

# Write to the tidyData text file 

write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');