# PatientReadmission.R
# Copyright Ernst Henle 2016

# Previously:
# QuizOnClassification.R

# Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")
source("UtilitiesConfM.R")

url <- "PatientReadmission.csv"
#url <- "https://www.dropbox.com/s/72sldg5t0jssprx/PatientReadmission.csv?dl=1"
Patients <- read.csv(url, header=TRUE, stringsAsFactors=TRUE)
ReadmitFx <- Readmitted ~ .

# Partition the data the exact way
fractionOfTest=0.25 # do not change
set.seed(2) # do not change
randoms <- runif(nrow(Patients))
cutoff <- quantile(randoms, fractionOfTest)
testFlag <- randoms <= cutoff
testData <- Patients[testFlag, ]
trainData <- Patients[!testFlag, ]

# Get Package
reposURL <- "http://cran.rstudio.com/"
if (!require("randomForest")) {install.packages("randomForest", dep=TRUE, repos=reposURL)} else {" randomForest is already installed "}
# Now that the package is installed, we want to load the package so that we can use its functions
library(randomForest)
if (!require("nnet")) {install.packages("nnet", dep=TRUE, repos=reposURL)} else {" nnet is already installed "}
# Now that the package is installed, we want to load the package so that we can use its functions
library(nnet)
if (!require("rpart")) {install.packages("rpart", dep=TRUE, repos=reposURL)} else {" rpart is already installed "}
# Now that the package is installed, we want to load the package so that we can use its functions
library(rpart)

# Q4:  What is d2? Look ahead a few line to see how it is used
d2 <- trainData # Change this line

# The following lines of code create classifications.
# The classifications are a neural net, a random forest, and a decision tree.
set.seed(4)  # do not change
Model.NN <- nnet(formula=ReadmitFx, data=d2, size=10, maxit=200)
Model.RF <- randomForest(formula=ReadmitFx, data=d2)
Model.DT <- rpart(formula=ReadmitFx, data=d2)

#plot(Model.DT)
#plot(Model.RF)

# Q5:  What is d1? Look ahead a few line to see how it is used
d1 <- testData # Change this line

# The following lines of code calculate probabilities.
# These probabilities will be used to test the accuracy of their respectives models. 
prob.NN <- predict(Model.NN, newdata=d1, type="raw")
prob.RF <- predict(Model.RF, newdata=d1, type="prob")[,2]
prob.DT <- predict(Model.DT, newdata=d1, type="prob")[,2]

# Q7 and Q8
# Threshold probabilities to get predictions (see homework solution for examples)
threshold <- 0.6
#actual <- ifelse(testData$Readmitted, "Yes", "No")
actual <- testData$Readmitted


# convert the predicted probabilities to predictions using a threshold
# ********** add code here
predicted.NN <- ifelse(prob.NN > threshold, "Yes", "No")
print(" ")
print(" -------------------------------- ")
print("Confusion Matrix for NN")
# create a table to compare predicted values to actual values
# ********** add code here
Table.NN <- table(predicted.NN,actual,dnn = c("Predicted","Actual"))
print(Table.NN)
#Confusion Matrix for Naive Bayes
# convert the predicted probabilities to predictions using a threshold
# ********** add code here
predicted.RF <- ifelse(prob.RF > threshold, "Yes", "No")
print(" ")
print(" -------------------------------- ")
print("Confusion Matrix RF")
# create a table to compare predicted values to actual values
# ********** add code here
#print(table(predicted.NaiveBayes[,1],actual,dnn = c("Predicted","Actual")))
Table.RF <- table(predicted.RF,actual,dnn = c("Predicted","Actual"))
print(Table.RF)

predicted.DT <- ifelse(prob.DT > threshold, "Yes", "No")
print(" ")
print(" -------------------------------- ")
print("Confusion Matrix DT")
# create a table to compare predicted values to actual values
# ********** add code here
#print(table(predicted.NaiveBayes[,1],actual,dnn = c("Predicted","Actual")))
Table.DT <- table(predicted.DT,actual,dnn = c("Predicted","Actual"))
print(Table.DT)

#Accuracy
#Accuracy.NN <- (Table.NN[1,1]+Table.NN[2,2])/(Table.NN[1,1]+Table.NN[1,2]+Table.NN[2,1]+Table.NN[2,2])
Accuracy.NN <- Accuracy(Table.NN)
print("Accuracy of NN")
print(Accuracy.NN)
Sensitivity.NN <- Sensitivity(Table.NN)
print("Sensitivity of NN")
print(Sensitivity.NN)
FPR.NN <- FPR(Table.NN)
print("FPR of NN")
print(FPR.NN)


# Create a table to compare predicted values to actual values (see homework solution for examples)
#Accuracy.RF <- (Table.RF[1,1]+Table.RF[2,2])/(Table.RF[1,1]+Table.RF[1,2]+Table.RF[2,1]+Table.RF[2,2])
Accuracy.RF <- Accuracy(Table.RF)
print("Accuracy of RF")
print(Accuracy.RF)
Sensitivity.RF <- Sensitivity(Table.RF)
print("Sensitivity of RF")
print(Sensitivity.RF)
FPR.RF <- FPR(Table.RF)
print("FPR of RF")
print(FPR.RF)


# Calculate Accuracy:  Correct/Total (see homework solution for examples)

#Accuracy.DT <- (Table.DT[1,1]+Table.DT[2,2])/(Table.DT[1,1]+Table.DT[1,2]+Table.DT[2,1]+Table.DT[2,2])
Accuracy.DT <- Accuracy(Table.DT)
print("Accuracy of DT")
print(Accuracy.DT)
Sensitivity.DT <- Sensitivity(Table.DT)
print("Sensitivity of DT")
print(Sensitivity.DT)
FPR.DT <- FPR(Table.DT)
print("FPR of DT")
print(FPR.DT)