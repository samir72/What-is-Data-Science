# KMeansNormTest.R

# Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")
TestObservations <- as.matrix(read.csv("TestObservations.csv"))
TestCenters <- matrix(c(1, 1, -2, -2, 2, -2), nrow=3)

#quiz03
#TestObservations <- matrix(c(1,1,2,2,10,11,12,1,1,2,2,1,2,1,2,1,12,1,12), nrow=6)
#TestCenters <- matrix(c(1,1,2,2), nrow=2)
#TestObservations <- matrix(c(4,2,6,10,5,3), nrow=2)
#TestCenters <- matrix(c(1,1), nrow=1)
#Quiz03

source("KMeans.R")
source("KMeansHelper.R")
source("KMeansNorm_complete.R")
#source("KMeansNorm.R")

#par(mfrow=c(1,2))
#hist(TestObservations[,2],xlab="Observation # 2",col="lightblue",main="")
#hist(TestObservations[,1],xlab="Observation # 1",col="lightblue",main="")

# TestObservations Distribution in second dimension
hist(TestObservations[,2], col=rgb(1,1,0,1))

# TestObservations Distribution in first dimension
hist(TestObservations[,1], col=rgb(0,0,1,0.25), add=T)

# What is the single most obvious difference between these two distributions?

# Test 1
KMeansNorm(clusterCenters = TestCenters, observations = TestObservations, normD1=F, normD2=F)
# Does clustering occur along one or two dimensions?  Which dimensions?  Why?

# Test 2
KMeansNorm(clusterCenters = TestCenters, observations = TestObservations, normD1=T, normD2=F)
# Does clustering occur along one or two dimensions?  Which dimensions?  Why?

# Test 3
KMeansNorm(clusterCenters = TestCenters, observations = TestObservations, normD1=F, normD2=T)
# Does clustering occur along one or two dimensions?  Which dimensions?  Why?

# Test 4
KMeansNorm(clusterCenters = TestCenters, observations = TestObservations, normD1=T, normD2=T)
# Does clustering occur along one or two dimensions?  Which dimensions?  Why?

# Put answers to assignment questions here:

