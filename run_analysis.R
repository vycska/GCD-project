#get features names, remove '()' in them
names <- read.table(".//UCI HAR Dataset//features.txt",colClasses="character")
names <- gsub("()","",names[[2]],fixed=T)

#prepare train data
traindata <- read.table(".//UCI HAR Dataset//train//X_train.txt",na.strings="N/A",colClasses="numeric")
names(traindata) <- names
traindata$activity <- readLines(".//UCI HAR Dataset//train//y_train.txt")
traindata$subject <- readLines(".//UCI HAR Dataset//train//subject_train.txt")

#prepare test data
testdata <- read.table(".//UCI HAR Dataset//test//X_test.txt",na.strings="N/A",colClasses="numeric")
names(testdata) <- names
testdata$activity <- readLines(".//UCI HAR Dataset//test//y_test.txt")
testdata$subject <- readLines(".//UCI HAR Dataset//test//subject_test.txt")

#merge 'train' and 'test' data sets
data <- rbind(traindata,testdata)

#remove unnecessary variables
rm("traindata","testdata")

#extract measurements on the mean and std,
#and reorder column names so that 'activity' and 'subject' go first

data <- data[,c(562,563,grep("[Mm]ean|[Ss]td",names))]

#substitute activity codes with names
activities <- c("walking","walking.upstairs","walking.downstairs","sitting","standing","laying")
for(i in seq_along(activities)) 
  data$activity <- gsub(as.character(i),activities[i],data$activity)

#convert variables
data$activity <- as.factor(data$activity)
data$subject <- as.integer(data$subject)

#create new empty data frame with existing column/variable names and populate it with
#averages of every subject for every activity
data2 <- data[0,]
subjects <- sort(unique(data$subject))
k <- 1
for(i in subjects) {
  for(j in activities) {
    data2[k,"subject"] <- i
    data2[k,"activity"] <- j
    for(l in 3:length(names(data))) {
      data2[k,l] <- mean(data[data$subject==i & data$activity==j,l],na.rm=T)
    }
    k <- k+1
  }
}
write.table(data2,"data2.csv",quote=FALSE,sep=",",row.names=FALSE)
