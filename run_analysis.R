run_analysis<-function(){
  #Get training data
  training = read.csv("UCI HAR Dataset/train/X_train.txt", sep="", header=FALSE)
  training[,562] = read.csv("UCI HAR Dataset/train/Y_train.txt", sep="", header=FALSE)
  training[,563] = read.csv("UCI HAR Dataset/train/subject_train.txt", sep="", header=FALSE)
  
  #Get testing data
  testing = read.csv("UCI HAR Dataset/test/X_test.txt", sep="", header=FALSE)
  testing[,562] = read.csv("UCI HAR Dataset/test/Y_test.txt", sep="", header=FALSE)
  testing[,563] = read.csv("UCI HAR Dataset/test/subject_test.txt", sep="", header=FALSE)
  
  activityLabels = read.csv("UCI HAR Dataset/activity_labels.txt", sep="", header=FALSE)
  
  # Read features 
  features = read.csv("UCI HAR Dataset/features.txt", sep="", header=FALSE)
  features[,2] = gsub('-mean', 'Mean', features[,2])
  features[,2] = gsub('-std', 'Std', features[,2])
  features[,2] = gsub('[-()]', '', features[,2])
  
  # Merge training and test sets together
  allData = rbind(training, testing)
  
  # Get mean and std. dev.
  colsNeeded <- grep(".*Mean.*|.*Std.*", features[,2])
  # First reduce the features table to what we want
  features <- features[colsNeeded,]
  # Now add subject and activity
  colsNeeded <- c(colsNeeded, 562, 563)
  # And remove the unwanted columns from allData
  allData <- allData[,colsNeeded]
  # Add the column names (features) to allData
  colnames(allData) <- c(features$V2, "Activity", "Subject")
  colnames(allData) <- tolower(colnames(allData))
  
  currentActivity = 1
  for (currentActivityLabel in activityLabels$V2) {
    allData$activity <- gsub(currentActivity, currentActivityLabel, allData$activity)
    currentActivity <- currentActivity + 1
  }
  
  allData$activity <- as.factor(allData$activity)
  allData$subject <- as.factor(allData$subject)
  
  tidypretty = aggregate(allData, by=list(activity = allData$activity, subject=allData$subject), mean)
  # Remove unwanted columns
  tidypretty[,90] = NULL
  tidypretty[,89] = NULL
  write.table(tidypretty, "tidypretty.txt", sep="\t",row.names=FALSE)
  
}