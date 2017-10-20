## Steps to perform the assignment are listed in the instructions. The R file also sectioned out the code accordingly to these steps

1. Merging the training and the test sets to create one data set.
2. Extracting only the measurements on the mean and standard deviation for each measurement. 
3. Using descriptive activity names to name the activities in the data set
4. Appropriately labeling the data set with descriptive activity names. 
5. Creating a second, independent tidy data set with the average of each variable for each activity and each subject. 
## Data are read from:
- features.txt
- activity_labels.txt
- subject_train.txt
- x_train.txt
- y_train.txt
- subject_test.txt
- x_test.txt
- y_test.txt
Assign column names and merge to create one data set.
## Calculated means and standard deviations of these set of data - one each for the X,Y, and Z dimentions:
- tBodyAcc-X
- tBodyAcc-Y
- tBodyAcc-Z
- tBodyGyro-X
- tBodyGyro-Y
- tBodyGyro-Z

## Mean and Standard Deviation versions for each of the *tBodyAcc* and *tBodyGyro*
- tBodyAcc-mean()-X: Mean of base body acceleration data in the X dimension
- tBodyGyro-std()-Y: Standard deviation of base body gyroscope measurement in the Y dimension
