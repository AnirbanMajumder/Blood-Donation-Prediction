#######################Title: Blood Donation Prediction####################################
#######################Author: Anirban Majumder############################################
#######################Date: November 25th, 2015###########################################

getwd()

setwd('path')

train <- read.csv('/path/BloodDonation.csv')

test <- read.csv('/path/TestData.csv')


#Finding the structure of train and test data provided
str(train)

str(test)

#Check the sample test and train data
head(train)

head(test)

#Renaming the column names of the train data

names(train)[1] <- "ID"

names(train)[2] <- "Months_SLD"

names(train)[3] <- "No_Of_Donations"

names(train)[4] <- "Volume"

names(train)[5] <- "Months_SFD"

names(train)[6] <- "freq"

names(train)[7] <- "Donate"

#Renaming the column names of the test data

names(test)[1] <- "ID"

names(test)[2] <- "Months_SLD"

names(test)[3] <- "No_Of_Donations"

names(test)[4] <- "Volume"

names(test)[5] <- "Months_SFD"

#MOdifying the data type of the predictor to factor

train$Donate <- factor(train$Donate)

#Checking for missing values in the train data

sapply(train[1:5], function (x) {sum(is.na(x))})

#Generate summary statistics of the train data

summary(train)

#Check for correlation between variables in the train data

cor(train[,unlist(lapply(train, is.numeric))])


# Feature Reengineering

#Remove VOlume from both train and test data

train$Volume <-NULL

test$Volume <-NULL

#Creata a new column called freq (Frequency)

train$freq <- (train$Months_SFD - train$Months_SLD)/train$No_Of_Donations

test$freq <- (test$Months_SFD - test$Months_SLD)/test$No_Of_Donations


#Generate summary statistics for the train data
summary(train)

#install packages for building a decision tree

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#Building Decision Tree

train_dt <- rpart(Donate ~ Months_SLD+No_Of_Donations+Months_SFD+freq, data = train, method = "class")

plot(train_dt)

text(train_dt)

fancyRpartPlot(train_dt)

my_prediction <- predict(train_dt, test, type="class")

my_solution <- data.frame(ID=test$ID,Donate=my_prediction)

head(my_solution)

write.csv(my_solution,"/path/Sol_DT1.csv")


#Building Random Forest 

install.packages("randomForest")

library(randomForest)

str(train)

my_forest <- randomForest(as.factor(Donate) ~ Months_SLD+No_Of_Donations+Months_SFD+freq, data = train, importance = TRUE, ntree = 1000, sampsize=10 )

my_prediction <- predict(my_forest, test, type="prob")

my_solution <- data.frame(test$ID,my_prediction)

head(my_solution)

write.csv(my_solution,"/path/Sol_RF11.csv")





