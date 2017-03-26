# Clear objects from Memory
rm(list=ls())
# Clear Console:
cat("\014")
source("UtilitiesConfM.R")
#1.Training vs Test Data
#a)In general, for any modeling data, why are accuracy measures better on training data than on test data?
cat("1(a)Answer : Expertly labelled data is used to train the model, this is data which has been primarly used to create the model so it would be logical to have higher accuracy measures on this data. A very high accuracy might not to be a good idea as this could lead to 'Overfitting'.\n")
#b)Given modeling data, how do you determine which of this data will become training data and which data will become test data?
cat("1(b)Answer : Paritioning should be always random to divide the data between training and test data sets.\n")
#c)You have two datasets. You used one to train the model and the other to test the model. You lost the test results and forgot which one you used for training or testing. How can you determine which of these datasets is the testing data?
cat("1(c)Answer : I would rerun the predictions on both training and test data and check the accuracy results.Test data will have a lower accuracy rate than training data.\n")

#2.Beware, this problem contains irrelevant data while some important numbers are not explicitly 
#presented. A model was trained on 300 individuals where 149 had the cold and 151 were healthy. 
#The model was tested on 100 individuals where 10 were actually ill. The model correctly predicted that 85 of the healthy individuals were indeed healthy and correctly predicted that 7 of the ill individuals were indeed ill. The other predictions were incorrect. Consult Wikipedia: http://en.wikipedia.org/wiki/Precision_and_recall. Present the confusion matrix and the following:
# Create Confusion Matrix.
Table.X <- matrix(c(85, 5, 3, 7), nrow=2)
#Table.X <- matrix(c(7, 3, 5, 85), nrow=2)

Accuracy.X <- Accuracy(Table.X)
Sensitivity.X <- Sensitivity(Table.X)
Specificity.X <- Specificity(Table.X)
Precision.X <- Precision(Table.X)
Recall.X <- Recall(Table.X)

#Accuracy.X <- (Table.X[1,1]+Table.X[2,2])/(Table.X[1,1]+Table.X[1,2]+Table.X[2,1]+Table.X[2,2])
#Sensitivity.X <- (Table.X[1,1])/(Table.X[1,1]+Table.X[2,1])
#Specificity.X <- (Table.X[2,2])/(Table.X[1,2]+Table.X[2,2])
#Precision.X <- (Table.X[1,1])/(Table.X[1,1]+Table.X[1,2])
#Recall.X <- (Table.X[1,1])/(Table.X[1,1]+Table.X[2,1])

cat("2(a)Sensitivity : ", Sensitivity.X)
cat("\n")
cat("2(b)Specificity : ", Specificity.X)
cat("\n")
cat("2(c)Accuracy : ", Accuracy.X)
cat("\n")
cat("2(d)Precision : ", Precision.X)
cat("\n")
cat("2(e)Recall : ", Recall.X)
cat("\n")

#3.The probability threshold for a classification varies in an ROC chart from 0 to 1.
#a)What point of the graph corresponds to a threshold of zero?
cat("3(a) Answer : Bottom left hand corener")
cat("\n")
#b)What point of the graph corresponds to a threshold of one?
cat("3(b) Answer : Top right hand corner")
cat("\n")
#c)What point of the graph corresponds to a threshold of 0.5? (trick question)
cat("3(c) Answer : You cannot guess the point with just the threshold of 0.5")
cat("\n")
#4.A Classification is tested on 1000 cases. In the approximate middle of its ROC chart there is a point where the false positive rate is 0.4, the true positive rate is 0.8, and the accuracy is 0.7.
#a)What does the confusion matrix look like?
#b)What can you say about the probability threshold at that point? (trick question)

Table.Y <- matrix(c(400, 100, 200, 300), nrow=2)

Accuracy.Y = Accuracy(Table.Y)
TPR.Y = Sensitivity(Table.Y)
FPR.Y = FPR(Table.Y)
#Accuracy.Y <- (Table.Y[1,1]+Table.Y[2,2])/(Table.Y[1,1]+Table.Y[1,2]+Table.Y[2,1]+Table.Y[2,2])
#TPR.Y <- (Table.Y[1,1])/(Table.Y[1,1]+Table.Y[2,1])
#FPR.Y <- (Table.Y[1,2])/(Table.Y[1,2]+Table.Y[2,2])

cat("4(b): Confusion Matrix")
cat("\n")
print(Table.Y)

#Using R to calculate Confusion matrix
A <- matrix(data=c(1, 1, 1, 1, 1, 0, 0, 1, -0.2, 0, 0.8, 0, 0,-0.6, 0, 0.4), nrow=4, ncol=4, byrow=TRUE)
b <- matrix(data=c(1000, 700, 0, 0), nrow=4, ncol=1, byrow=FALSE)
round(solve(A, b), 4)
#

cat("4(b)TPR : ", TPR.Y)
cat("\n")
cat("4(b)FPR : ", FPR.Y)
cat("\n")
cat("4(b)Accuracy : ", Accuracy.Y)
cat("\n")
cat("4(b) You cannot guess the threshold by looking at the chart.")
cat("\n")
