# CollegeStudentsDataset.R
# Copyright 2016 by Ernst Henle

# Read the data.  Do not change the following block
#url <- "https://www.dropbox.com/s/3uuh9nk57votrnc/SudentPlans.csv?dl=1"
url <- "StudentPlans.csv"
#url <- "StudentPlans_heavier.csv"
Students <- read.csv(url, header=T, stringsAsFactors=FALSE)
Students <- Students[order(-Students$ParentIncome), ]
Students$CollegePlans <- as.numeric(Students$CollegePlans == "Plans to attend")
formula <- CollegePlans ~ Gender + ParentIncome + IQ + ParentEncouragement

# Set repeatable random seed. Do not change the following line
set.seed(4)

PartitionWrong <- function(dataSet, fractionOfTest = 0.3)
{
  #browser()
  numberOfRows <- nrow(dataSet)
  numberOfTestRows <- fractionOfTest * numberOfRows
  testFlag <- 1:numberOfRows <= numberOfTestRows
  testingData <- dataSet[testFlag, ]
  trainingData <- dataSet[!testFlag, ]
  dataSetSplit <- list(trainingData=trainingData, testingData=testingData)
  return(dataSetSplit)
}

PartitionExact <- function(dataSet, fractionOfTest = 0.3)
{
  # ********** Add code here
  #browser()
  random <-runif(nrow(dataSet))
  #Quantif <- qunif(random,fractionOfTest)
  #Using Quant
  quant <- quantile(random,fractionOfTest)
  testFlag <- random <= quant
  testingData <- dataSet[testFlag, ]
  trainingData <- dataSet[!testFlag, ]
  dataSetSplit <- list(trainingData=trainingData, testingData=testingData)
  #Using Sort
  #st <- sort(random)
  #testFlagst <- random <= fractionOfTest
  #testingDatast <- dataSet[testFlagst, ]
  #trainingDatast <- dataSet[!testFlagst, ]
  #dataSetSplit <- list(trainingData=trainingDatast, testingData=testingDatast)
  #---------------------------
  return(dataSetSplit)
}

PartitionFast <- function(dataSet, fractionOfTest = 0.3)
{
  # ********** Add code here
  #browser()
  random <-runif(nrow(dataSet))
  testFlag <- random <=fractionOfTest
  testingData <- dataSet[testFlag, ]
  trainingData <- dataSet[!testFlag, ]
  dataSetSplit <- list(trainingData=trainingData, testingData=testingData)
  
  return(dataSetSplit)
}
