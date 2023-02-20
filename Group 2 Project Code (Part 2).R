#################################################################

# IST687 M003 Final Project Code (Part 2)
# 1. Association rule mining
# 2. Decision tree model
# 3. SVM
#----------------------------------------------------------------
# GROUP 2:
# WENZHE YANG, TIEHAO CHEN, CHENG LI, SAHEB SINGH, YIYUAN CHENG

#################################################################

# setwd()

library(tidyverse)
library(jsonlite)
library(dplyr)
library(randomForest)
require(caTools)
library(caret)
library(ggplot2)
library(devtools)

df<-jsonlite::fromJSON("fall2019-survey-M03.json")

# delete the information of state in cities 
# substitude the white space and the state
df$Destination.City<-as.factor(gsub("(.*),.*", "\\1",as.character(df$Destination.City)))
df$Origin.City<-as.factor(gsub("(.*),.*", "\\1",as.character(df$Origin.City)))
df$Origin.State<-tolower(df$Origin.State)
df$Destination.State<-tolower(df$Destination.State)

#-------------------------------Prepare for modeling--------------------------------
# drop some columns
ndf <- df[,-which(names(df)%in%c("olat", "dlat","olong", "dlong", "freeText",'Partner.Code'))]
# check the data
ndf

# Add in the new column the day of the week 
ndf$DATE<-as.Date(df$Flight.date,format = "%m/%d/%y")
ndf$DayOfWeek<-lapply(ndf$DATE,function(x)weekdays(as.POSIXct(x), abbreviate = T))
ndf$month<- lapply(df$Flight.date,function(x)substr(x,0,1))
# apply the function in each row to get the length of the users' usage
ndf$Flight.Years <-lapply(ndf$Year.of.First.Flight,function(x) 2014-x)

# add in holiday features
ndf$holiday<-'No'
ndf$holiday[which(ndf$DATE=='2014-01-01')]<-'Yes'
ndf$holiday[which(ndf$DATE>='2014-01-18' & ndf$DATE<='2014-01-20')]<-'Yes'
ndf$holiday[which(ndf$DATE>='2014-02-15' & ndf$DATE<='2014-02-17')]<-'Yes'
ndf$holiday[which(ndf$DATE>='2014-03-09' & ndf$DATE<='2014-03-15')]<-'Yes'
ndf$holiday<-factor(ndf$holiday)
summary(ndf$holiday)


ndf <- ndf[,-which(names(ndf)%in%c('Flight.date','DATE','Year.of.First.Flight',"Origin.State","Destination.State"))]
colnames(ndf)

names(ndf)<-c("desti","orig","status","age","gender","price.sensitiv","flights.per.year",
              "loyalty","type.of.travel","total.freq.accts","shopping.amount",
              "eating.and.drinking","class","day.of.month","partner.name","scheduled.depart.hour",
              "depart.delay.in.minutes","arrival.delay.in.minutes","flight.cancelled",
              "flight.time.in.minutes","flight.distance","likelihood.to.recommend","day.of.week",
              "month","flight.years",'holiday')

# change column order
#ndf<-ndf[c("desti","orig","status","age","gender","price.sensitiv","flights.per.year","day.of.week",
#            "flight.years","month","type.of.travel","total.freq.accts","shopping.amount",
#            "eating.and.drinking","class","day.of.month","partner.name","scheduled.depart.hour",
#            "depart.delay.in.minutes","arrival.delay.in.minutes","flight.cancelled",
#            "flight.time.in.minutes","flight.distance","loyalty","likelihood.to.recommend")]


# delete the rows that the likelyhood of recomandation is null
sum(is.na(ndf$likelihood.to.recommend))
ndf<- ndf[-which(is.na(ndf$likelihood.to.recommend)==TRUE),]
ndf$likelihood.to.recommend<-as.numeric(ndf$likelihood.to.recommend)

str(ndf$likelihood.to.recommend)

# creat new columns called NPS
ndf$NPS<-'detractors'
ndf$NPS[which(ndf$likelihood.to.recommend>=7 & ndf$likelihood.to.recommend<=8)]<-'passive'
ndf$NPS[which(ndf$likelihood.to.recommend>8)]<-'promoters'
#ndf$NPS<-factor(ndf$NPS)

# the range and distribution of loyalty
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-0.9722 -0.7000 -0.4286 -0.2750  0.0588  1.0000 
summary(as.numeric(ndf$loyalty))

#the loyalty seperate into three classes low, median, and high
ndf$loyal <- 'low'
ndf$loyal[which(ndf$loyalty>-0.7 & ndf$loyalty<= 0.0588)]<- 'median'
ndf$loyal[which(ndf$loyalty>0.0588)]<- 'high'

# some are list and need to transfer into vactors
ndf$day.of.week <- vapply(ndf$day.of.week, paste, collapse = ", ", character(1L))
ndf$flight.years <- vapply(ndf$flight.years, paste, collapse = ", ", character(1L))
ndf$month <- vapply(ndf$month, paste, collapse = ", ", character(1L))

# group the recommendation socre into two classes (for svm classification)
# add in new column that change the likelihood of recommandation into three classes in dicision tree
# the score [0-7] means the user is not likely to recommand,labled them 'neg', [7-10]means the user is more likely to recommand,and welabeled them as 'pos',
# and the score [7-8]is regard as passive and I made another document that conatians this group of people and put into decision tree moedling

# ----------------deal with NA------------------
# Flight.cancelled influence a lot on the result, so we will generate a dataset without any NA
noCancelled<-ndf[which(ndf$flight.cancelled=='No'),]
# Check again what kind of NA is missed
sum(is.na(noCancelled))
# find the columns where contains the NA
index <- unique(unlist(lapply (noCancelled, function (x) which (is.na (x)))))

str(noCancelled[index,])

# there ara 39 rows we removed thoes because it is a small part of the data when comparing to over 10 thousand rows
# so we delete thoes flights instead of replace the Na with anything that is not exist 
ndf<- ndf[-index,]
# now there are 10262 rows

#-------------seperate the cancelled data----------------
# for the flight is cancled
Cancelled<-ndf[which(ndf$flight.cancelled=='Yes'),]
sum(is.na(Cancelled))
# find the columns where contains the NA there are 223 rows of cancled data
# we cannot simply delete the row but the NA is there. 
index <- unique(unlist(lapply (Cancelled, function (x) which (is.na (x)))))
#View(Cancelled[index,])
# regard it as a different group
str(Cancelled)
canceldf <- Cancelled[index,]
# drop the columns
canceldf <- canceldf[,!colnames(canceldf)%in%c('flight.time.in.minutes','depart.delay.in.minutes','arrival.delay.in.minutes')]
# check data
str(canceldf)
# save it to csv
write.csv(canceldf,'cancelled.csv',row.names = FALSE)

# save the nocancelled data 
str(noCancelled)
write.csv(noCancelled,'nocancelled.csv',row.names = FALSE)

#----------------------general-------------------
# all of them transfer into factors for the next modeling step
ndf$status <- as.factor(ndf$status)
ndf$age <- as.factor(ndf$age)
ndf$gender<- as.factor(ndf$gender)
ndf$price.sensitiv<-as.factor(ndf$price.sensitiv)
ndf$flights.per.year<-as.factor(ndf$flights.per.year)
ndf$day.of.week<-as.factor(ndf$day.of.week)
ndf$flight.years<-as.factor(ndf$flight.years)
ndf$month<-as.factor(ndf$month)
ndf$type.of.travel<-as.factor(ndf$type.of.travel)
ndf$total.freq.accts<-as.factor(ndf$total.freq.accts)
ndf$shopping.amount <-as.factor(ndf$shopping.amount)
ndf$eating.and.drinking<-as.factor(ndf$eating.and.drinking)
ndf$class<-as.factor(ndf$class)
ndf$day.of.month<-as.factor(ndf$day.of.month)
ndf$partner.name<-as.factor(ndf$partner.name)
ndf$scheduled.depart.hour<-as.factor(ndf$scheduled.depart.hour)
ndf$depart.delay.in.minutes<-as.factor(ndf$depart.delay.in.minutes)
ndf$arrival.delay.in.minutes<-as.factor(ndf$arrival.delay.in.minutes)
ndf$flight.cancelled<-as.factor(ndf$flight.cancelled)
ndf$flight.time.in.minutes<-as.factor(ndf$flight.time.in.minutes)
ndf$flight.distance<-as.factor(ndf$flight.distance)
ndf$loyalty<-as.factor(ndf$loyalty)
ndf$likelihood.to.recommend<-as.factor(ndf$likelihood.to.recommend)

# check the data
str(ndf)

# seperate the cacelled flight as another one because they are totally different group
# code next 


# save the result to csv first 
# fall2019 survey2 row.names = FALSE
write.csv(ndf,'fall2019-survey2.csv',row.names = FALSE)


ndf<- read_csv('nocancelled.csv')
cdf <- read_csv('cancelled.csv')

#sum(is.na(ndf))
#ndf<-ndf[complete.cases(ndf),]
#ndf<-ndf[-which(is.na(ndf)==TRUE),]
#ndf<- na.omit(ndf)
ndf<-na.omit(ndf)
sum(is.na(ndf))

#str(ndf)

# add in the punctuation which is about the delay
index1 <- which(ndf$depart.delay.in.minutes != 0 & ndf$arrival.delay.in.minutes != 0)
#warnings()
index2 <- which(ndf$depart.delay.in.minutes != 0 & ndf$arrival.delay.in.minutes == 0)
index3 <- which(ndf$depart.delay.in.minutes == 0 & ndf$arrival.delay.in.minutes != 0)
index4 <- which(ndf$depart.delay.in.minutes == 0 & ndf$arrival.delay.in.minutes == 0)

ndf$punctuality <- 0
#ndf$punctuality[index1] <- "Both delay"
ndf$punctuality[index1] <- "delay"
#ndf$punctuality[index2] <- "Depart delay"
ndf$punctuality[index2] <- "delay"
#ndf$punctuality[index3] <- "Arrival delay"
ndf$punctuality[index3] <- "delay"
ndf$punctuality[index4] <- "Both on time"
ndf$punctuality <- factor(ndf$punctuality, ordered = TRUE,
                          levels = c("delay", "Both on time"))
summary(ndf$punctuality)


# convert into factors
ndf[sapply(ndf, is.character)] <- lapply(ndf[sapply(ndf, is.character)], as.factor)
cdf[sapply(cdf, is.character)] <- lapply(cdf[sapply(cdf, is.character)], as.factor)
# check the data
str(ndf)
str(cdf)


#-------------use RF to see the general pattern-------------
# check the data if the variables are in right 
sapply(ndf, class)
# drop some columns that are not use c('desti','orig','loyalty','likelihood.to.recommend')
rdf<- ndf[, !colnames(ndf)%in%c('desti','orig','loyalty','likelihood.to.recommend')]
summary(rdf)

#-------------creat substes-------------
# remove some columns 


# devided by the type of travel first
p_travel<- rdf%>%
  filter(type.of.travel=='Personal Travel')%>%
  filter(status=='Blue')

nrow(p_travel[p_travel$NPS=='detractors',])# 1684 people are detractors
nrow(p_travel[p_travel$NPS=='passive',])  # 559 people are premoter 
nrow(p_travel[p_travel$NPS=='promoters',]) # 58 promoters in this group of people

##---------- AHA! so we can do the dicision tree classification on it(for passive and detractors)------------
#inTraining <- createDataPartition(p_travel$NPS, p = .75, list = FALSE)
#training <- p_travel[ inTraining,]
#testing  <- p_travel[-inTraining,]

ddf<- rdf[, !colnames(rdf)%in%c('depart.delay.in.minutes','arrival.delay.in.minutes')]
p_travel2<- rdf%>%
  filter(type.of.travel=='Personal Travel')%>%
  filter(status=='Blue')


inTraining <- createDataPartition(p_travel$NPS, p = .75, list = FALSE)
training <- p_travel[ inTraining,]
testing  <- p_travel[-inTraining,]


dim(training)
dim(testing)

#summary(training)
#summary(testing)

# train the model
# decision Tree
dt_model<- tree(NPS~., data=training)
dt_pred <- predict(dt_model, testing, type="class")
with(testing, table(dt_pred, NPS))
# the accuracy is 34.9% the decision tree model is not good 
dt_model
summary(dt_model)

library(rpart)
library(rpart.plot)
fit <- rpart(NPS~., data = training, method = 'class')
rpart.plot(fit, box.palette="RdBu", shadow.col="gray", nn=TRUE )

# the decison tree cannot detect people who are promoters but it works well on detecting the detractors
# as the result we could see age > 64.5 is more likely to be a detractor
# the flight year > 32.5 is more likely to be detractor
# the arrive delay is more than 5.5 minute may cause the unsatisfaction
nrow(p_travel[p_travel$flights.per.year>32.5,]) # 846 people
nrow(p_travel[p_travel$age>64.5&p_travel$flights.per.year>32.5,]) # 475 people
nrow(p_travel[p_travel$age>64.5&p_travel$flights.per.year>32.5&p_travel$arrival.delay.in.minutes>5.5,]) # 159 people
nrow(p_travel[p_travel$age>64.5&p_travel$flights.per.year>32.5
              &p_travel$arrival.delay.in.minutes>5.5&p_travel$NPS=='detractors',])# 159 
## ok we find thoes who have very high probability to be detractors:
## people who take perosonal travel

# Random Forest
rf_model<- randomForest(NPS~.,data = training,importance=TRUE,proximity=TRUE,ntree=400,mtry = 2, do.trace=100)
rf_pred <- predict(rf_model,testing,type='class')
with(testing, table(rf_pred, NPS))


####################################################################################
#----------------------------------Decision tree------------------------------------

library(ISLR)
require(tree)
require(CHAID)
require(purrr)
require(caret)
#install.packages("partykit")
#install.packages("CHAID", repos="http://R-Forge.R-project.org")
#dt_model<- tree(NPS~., data=training)

# for chaid prediction
asdf<-training
asdf[sapply(asdf, is.numeric)] <- lapply(asdf[sapply(asdf, is.numeric)], as.factor)
#asdfx<- as(asdf,'transactions')
#asdfx

# dt_model<-chaid(NPS~., data=asdf)
dt_pred <- predict(dt_model, testing, type="class")
with(testing, table(dt_pred, NPS))
# the accuracy is 34.9% the decision tree model is not good 
dt_model
summary(dt_model)

# under the type of travel by gender
# but the miss classification rate is as high as 0.394, this model need to further modify 

## --------------------------------Associate rule mining----------------------------
## for different genders 
# the Female/male customers who take bule status
wb_df<- rdf%>%
  filter(type.of.travel=='Personal Travel')%>%
  filter(status=='Blue')

library(arules)
library(arulesViz)
# turn all of the data into factors they are numeric
str(wb_df)
#asdf<-wb_df[, !colnames(rdf)%in%c('gender','status','punctuality','flight.cancelled')]
asdf<-wb_df[, !colnames(rdf)%in%c('flight.cancelled','status','type.of.travel')]

asdf[sapply(asdf, is.numeric)] <- lapply(asdf[sapply(asdf, is.numeric)], as.factor)
asdfx<- as(asdf,'transactions')
asdfx
itemLabels(asdfx)
rule1 <- apriori(data=asdf, parameter=list (supp=0.02,conf = 0.5), 
                 appearance = list (rhs=('NPS=detractors'),default='lhs'))
#inspect(head(sort(rule1, by = "lift"), 3))
#inspectDT(head(sort(rule1, by = "lift"), 3))
#high_lift1<- rule1[quality(rule1)$lift>2.5]
#inspect(high_lift1) 
# for male there are 3187 rules to detect the promoters but for the female there 20
inspect(head(sort(rule1, by = "lift"), 100))
write(rule1, file = "Asrule_detractor_subset.csv", sep = ",")


####################################################################################
#-------------------------------------SVM-------------------------------------------

dev.off() # Clear the graph window
cat('\014')  # Clear the console
rm(list=ls()) # Clear all user objects from the environment!!!

library(kernlab)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyverse)
library(caret)
library(e1071)
library(readr)

df <- read_csv('updated.csv')

# View(df)
# Using svm to predict recommendation
# Making training and testing dataset
# Testing dataset will be 2/3 of our data


randIndex <- sample(1:dim(df)[1])
cutPoint2_3 <- floor(2 * dim(df)[1]/3)

# Dropping Date
df <- df[, -36]

# Building our training set as two-third of original dataset
trainData <- df[randIndex[1:cutPoint2_3],] 

# Building our test set as one-third of original dataset
testData <- df[randIndex[(cutPoint2_3+1):dim(df)[1]],] 

# Dimension of the dataset
dim(trainData) # 6854 observations and 43 columns
dim(testData) # 28488 observations and 43 columns

# Applying SVM
svmOutput <- ksvm(Likelihood.to.recommend ~.,data=trainData,kernel="rbfdot",kpar="automatic",C = 5,cross = 3,prob.model = TRUE)
#svmOutput <- ksvm(NPS ~.,data=trainData,kernel="rbfdot",kpar="automatic",C = 5,cross = 3,prob.model = TRUE)

# -----------Predicting SVM using testdata------------
# Removing NA from testData
testData <- na.omit(testData)
# Using svm to predict testData
svmPred <- predict(svmOutput, testData, type = 'votes')

# -------- Making comptable with all the predictions-------
View(svmPred)

predData <- testData[, 27]
preLabels <- testData[, 42]

#-------- Adding Labels to Predicted Data ------------
compTable <- data.frame('Original_Data' = predData, 'Prediction' = svmPred[, 1], 'Original_Labels' = preLabels)
#View(compTable)
compTable$Prediction_Label<-'detractors'
compTable$Prediction_Label[which(compTable$Prediction>=7 &compTable$Prediction<=8)]<-'passive'
compTable$Prediction_Label[which(compTable$Prediction>8)]<-'promoters'
compTable$Prediction_Label<-factor(compTable$Prediction_Label)

# ------- Making a table to calculate accuracy and error rate ------
s <- table(compTable[, 3], compTable[, 4])
s

# ------- Calculating Error Rate -------
errorrate <- ((sum(c(s[1,2],s[1,3], s[2, 1], s[2, 3], s[3, 1], s[3, 2])))/sum(c(s[1,1],s[1,2],s[1,3],s[2,1], s[2, 2], s[2, 3], s[3, 1], s[3, 2], s[3, 3])))*100
errorrate

# ------- Calculating Accuracy Rate ------
accuracy <- ((sum(c(s[1,1],s[2,2], s[3,3])))/sum(c(s[1,1],s[1,2],s[1,3],s[2,1], s[2,2], s[2, 3], s[3, 1], s[3, 2], s[3, 3])))*100
accuracy

# -------- Saving the data-------
write.csv(compTable, "SVM.csv")
