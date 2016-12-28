rm(list=ls())

library(data.table)
library(reshape2)

## Merges the training and the test sets to create one data set
train <- read.table("X_train.txt",header=F)
trainlab <- read.table("y_train.txt",header=F)
subjecttrain <- read.table("subject_train.txt")

test <- read.table("X_test.txt",header=F)
testlab <- read.table("y_test.txt",header=F)
subjecttest <- read.table("subject_test.txt")

setDT(train)
setDT(test)
setDT(subjecttest)
setDT(subjecttrain)
setDT(trainlab)
setDT(testlab)
subjects <- rbind(subjecttrain,subjecttest)
colnames(subjects) <- "subject"
labs <- rbind(trainlab,testlab)
colnames(labs)<- "lable"
dt <- rbind(train,test)
dtsubject <- cbind(subjects,labs)
dt <- cbind(dtsubject,dt)
setDT(dt)
setkey(dt,subject,lable)
save(dt,file="Mergeddt.Rdata")

## Extracts only the measurements on the mean and standard deviation for each measurement
load("Mergeddt.Rdata")
features <- read.table("features.txt")
colnames(features)<- c("featureNum", "featureName")
setDT(features)
featlist <- features[grepl("mean\\(\\)|std\\(\\)", featureName)]
featlist$code <- featlist[,paste0("V",featureNum)]
select <- c(key(dt), featlist$code)
dt <- dt[, select, with=FALSE]

## Uses descriptive activity names to name the activities in the data set
Actnames <- read.table("activity_labels.txt")
colnames(Actnames) <- c("activityNum", "activityName")
setDT(Actnames)
dt <- merge(dt, Actnames, by.x="lable",by.y="activityNum", all.x=TRUE)
setkey(dt, subject, lable, activityName)
dt <- data.table(melt(dt, key(dt), variable.name="featureCode"))
dt$lable <- as.character(dt$lable)
dt <- merge(dt, featlist[, list(featureNum, code, featureName)], by.x="featureCode", by.y="code")
dt$activity <- factor(dt$activityName)
dt$feature <- factor(dt$featureName)
## Appropriately labels the data set with descriptive activity names
grepthis <- function (regex) {
  grepl(regex, dt$feature)
}
n <- 3
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol=nrow(y))
dt$featAxis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))
## Creates a second, independent tidy data set with the average of each variable for each activity and each subject
setkey(dt, subject, activity, featAxis)
dtTidy <- dt[, list(count = .N, average = mean(value)), by=key(dt)]
save(dtTidy,file="dtTidy.Rdata")
load("dtTidy.Rdata")
write.table(dtTidy,"dtTidy.txt", row.name=FALSE)
