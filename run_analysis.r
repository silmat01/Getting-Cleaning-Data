setwd("C:/Users/Lotus/Downloads/coursera/Course3/project")
## Load packages.

#packages <- c("data.table", "reshape2")
#sapply(packages, require, character.only = TRUE, quietly = TRUE)

path <- getwd()
#path
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
f <- "Dataset.zip"
if (!file.exists(path)) {dir.create(path)}
download.file(url, file.path(path, f))

executable <- file.path("C:", "Program Files", "WinRAR", "WinRar.exe")
parameters <- "x"
cmd <- paste(paste0("\"", executable, "\""), parameters, paste0("\"", file.path(path, f), "\""))
system(cmd)

pathIn <- file.path(path, "UCI HAR Dataset")
list.files(pathIn, recursive=TRUE)
setwd(pathIn)

# Step1
trainData <- read.table("./train/X_train.txt")
dim(trainData) 
head(trainData)
trainLabel <- read.table("./train/y_train.txt")
table(trainLabel)
trainSubject <- read.table("./train/subject_train.txt")
testData <- read.table("./test/X_test.txt")
dim(testData)
testLabel <- read.table("./test/y_test.txt") 
table(testLabel) 
testSubject <- read.table("./test/subject_test.txt")
joinData <- rbind(trainData, testData)
dim(joinData) 
joinLabel <- rbind(trainLabel, testLabel)
dim(joinLabel) 
joinSubject <- rbind(trainSubject, testSubject)
dim(joinSubject)

#Step2
features <- read.table("./features.txt")
dim(features)  
meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
length(meanStdIndices) 
joinData <- joinData[, meanStdIndices]
dim(joinData)

#Step3
activity <- read.table("./activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activityLabel <- activity[joinLabel[, 1], 2]
joinLabel[, 1] <- activityLabel
names(joinLabel) <- "activity"

#Step4
names(joinSubject) <- "subject"
cleanedData <- cbind(joinSubject, joinLabel, joinData)
dim(cleanedData) 
write.table(cleanedData, "mergedData.txt") 

#Step5
subjectLen <- length(table(joinSubject)) 
activityLen <- dim(activity)[1] 
columnLen <- dim(cleanedData)[2]
result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
result <- as.data.frame(result)
colnames(result) <- colnames(cleanedData)
row <- 1
for(i in 1:subjectLen) {
    for(j in 1:activityLen) {
        result[row, 1] <- sort(unique(joinSubject)[, 1])[i]
        result[row, 2] <- activity[j, 2]
        bool1 <- i == cleanedData$subject
        bool2 <- activity[j, 2] == cleanedData$activity
        result[row, 3:columnLen] <- colMeans(cleanedData[bool1&bool2, 3:columnLen])
        row <- row + 1
    }
}
head(result)
write.table(result, "MeanData.txt", row.names = FALSE) 