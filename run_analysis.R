library(dplyr)

#get some variable and factor names
features <-
  read.table(
    file.path("UCI HAR Dataset", "features.txt"),                       ,
    col.names = c("index", "feature"),
    stringsAsFactors = F)
activity <-
  read.table(
    file.path("UCI HAR Dataset", "activity_labels.txt"),
    col.names = c("index", "activity"),
    stringsAsFactors = F)

#read the training data
X_train <-
  read.table(
    file.path("UCI HAR Dataset", "train", "X_train.txt"),
    col.names = features[,2])
subject_train <-
  read.table(
    file.path("UCI HAR Dataset", "train", "subject_train.txt"),
    col.names = "subject")
y_train <-
  read.table(
    file.path("UCI HAR Dataset", "train", "y_train.txt"),
    col.names = "activity")

#read the test data
X_test <-
  read.table(
    file.path("UCI HAR Dataset", "test", "X_test.txt"),
    col.names = features[,2])
subject_test <-
    read.table(file.path("UCI HAR Dataset", "test", "subject_test.txt"),
    col.names = "subject")
y_test <-
    read.table(file.path("UCI HAR Dataset", "test", "y_test.txt"),
    col.names = "activity")

#convert necessary variables to factors
y_train$activity <- factor(y_train$activity, 
                           levels = activity[[1]],
                           labels = activity[[2]])
y_test$activity <- factor(y_test$activity, 
                          levels = activity[[1]],
                          labels = activity[[2]])
subject_train$subject <- factor(subject_train$subject)
subject_test$subject <- factor(subject_test$subject)

#strip the undesired columns from the X files
useful1 <- c(
  grep("std", features$feature, fixed=T),
  grep("mean", features$feature, fixed=T))
useful2 <- grep("meanFreq", features$feature, fixed=T, invert=T)
useful <- sort(intersect(useful1,useful2))
X_test_culled <- select(X_test, useful)
X_train_culled <- select(X_train, useful)

#make the full data frame
temp <- rbind(cbind(subject_train, y_train, X_train_culled),
               cbind(subject_test, y_test, X_test_culled))

#group full data frame, apply mean, and return a data frame
final <- temp %>%
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

#write the table to file
write.table(final, "project.txt", row.name = F)