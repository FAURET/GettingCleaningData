library(dplyr)
library(tidyr)
library(lubridate)

#urlb<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
#download.file(urlb,"data.zip",method="libcurl")
#unzip("data.zip")


# READING OF TRAINING DATA

# number of the subject from 1 to 30
subject.train<-read.table("UCI HAR Dataset/train/subject_train.txt")

# list of performances for each futures
x.train<-read.table("UCI HAR Dataset/train/X_train.txt")

#activity from 1 to 6
y.train<-read.table("UCI HAR Dataset/train/y_train.txt")



# READING OF TEST DATA

# number of the subject from 1 to 30
subject.test<-read.table("UCI HAR Dataset/test/subject_test.txt")
# list of performances for each futures
x.test<-read.table("UCI HAR Dataset/test/X_test.txt")
#activity from 1 to 6
y.test<-read.table("UCI HAR Dataset/test/y_test.txt")


#READING OF FEATURES DATA

list.features<-read.table("UCI HAR Dataset/features.txt")

#We keep only the features for whose labels contain mean of std
list.features<-list.features[grep("mean|std", list.features[,2]),]

# Cleaning the labels by removing the parentessis and the "-"
list.features[,2]<-gsub("-",".",list.features[,2])
list.features[,2]<-gsub("\\()","",list.features[,2])

# calcul of the new number of features analyzed (with mean and std)

nb.features<-nrow(list.features)


# Filter on mean and STD values given for the different features

x.test<-x.test[,list.features[,1]]
x.train<-x.train[,list.features[,1]]



#READING OF ACTIVITIES TABLE : WALKING, WLAKING_UPSTAIRS

list.activities<-read.table("UCI HAR Dataset/activity_labels.txt")
colnames(list.activities)<-c("nb.activity","label.activity")



# MERGING DIFFRENT DATA bASES TO HAVE AN UNIQUE DATA BASE WITH
#     *  train data
#     *  test data
#     *  labels of activity

bdd.train<-cbind(subject.train,y.train,x.train)
colnames(bdd.train)<-c("subject","activity",list.features[1:nrow(list.features),2])
bdd.test<-cbind(subject.test, y.test,x.test)
colnames(bdd.test)<-c("subject","activity",list.features[1:nrow(list.features),2])
bdd<-rbind(bdd.train,bdd.test) %>%
merge(list.activities,by.x="activity",by.y="nb.activity") %>%
select(- activity)


#############################################
# STEP 2 of the assignment 
#############################################

bdd2<-bdd
bdd2<-gather(bdd2,key=label.feature, value=value,2:(nb.features+1), na.rm= TRUE)
grp_cols <- names(bdd2)[1:3]
# Convert character vector to list of symbols
dots <- lapply(grp_cols, as.symbol)
bdd2$label.feature<-as.factor(bdd2$label.feature)
bdd2$subject<-as.factor(bdd2$subject)
bdd2<-group_by_(bdd2,.dots=dots)
#Calculs of the mean for eachn combination of subject/activity/feature
bdd2<-summarise(bdd2,mean.value=mean(value,na.rm=TRUE))

# Writting of the results in tidydata.txt
write.table(bdd2, file = "tidydata.txt",row.name=FALSE)

