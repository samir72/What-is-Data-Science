# Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")

#url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/00225/Indian%20Liver%20Patient%20Dataset%20(AdultData).csv"
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
url1 <- "http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names"

#3 Download the datasets
AdultData <- read.table(url, header=FALSE, sep = ",", stringsAsFactors=FALSE)
AdultNames <- read.csv2(url1, header=FALSE, sep = ":")
#Delete the salary projection calculated by NaiveBayes Model.
AdultData[15] <- list(NULL)

#4 Get header details from adult.names dataset and add column headers to the dataset.
headers <- c("age", "workclass","fnlwgt","education","education-num","marital-status","occupation","relationship","race","sex","capital-gain","capital-loss","hours-per-week","native-country")
#4 Add headers to the dataset.
names(AdultData) <- headers

#5 Binarize sex to a boolean value, check the data for whitespace before applying the logic to binarize.
# I am using trimews to remove leading and trailing whitespace.
AdultData$sex_num <- ifelse(trimws(AdultData$sex) == "Male", 0, 1)


#6 Display first 6 rows
head(AdultData)

#7 I am treating ? as NA in this assignment.
# There are three columns (workclass, occupation,native country with ? data. )
# Remove all rows with a ? data and provide me the count of rows deleted for each instance.

Badrowcount <- 0
NumCols <- ncol(AdultData) # Get number of columns for the loop.
for (i in 1:NumCols) {
  #browser()
  rowcount = nrow(subset(AdultData, trimws(AdultData[, i]) =="?"))
  if (nrow(subset(AdultData, trimws(AdultData[, i])=="?")) == 0) {
    cat("Column", names(AdultData[i]), "is being skipped as it contains no bad data","\n" )
    next
  }
  cat("\n")
  cat("Column", names(AdultData[i]), "contains ? hence" , rowcount, "rows are being deleted" ,"\n" )
  AdultData <- subset(AdultData, trimws(AdultData[, i])!="?")
  Badrowcount <- Badrowcount + rowcount
  cat("\n")
}

#8 Provide the total number of rows deleted with ? data.
cat("A total of", Badrowcount , "rows are being deleted" ,"\n" )


#6 Determine the standard mean,median and SD using the base R functions with user friendly messages wherein I can see which 
#columns are being calculated and who is being skipped in the calcultion.
NumCols <- ncol(AdultData) # Get number of columns for the loop.
for (i in 1:NumCols) {
  if (is.character(AdultData[, i])) {
    cat("Column", names(AdultData[i]), "is being skipped as its non numeric","\n" )
    next
  }
  cat("\n")
  SD <- sd(AdultData[, i],na.rm=TRUE)
  cat("SD of Column", names(AdultData[i]), "is ", SD, "\n" )
  ME <- mean(AdultData[, i],na.rm=TRUE)
  cat("Mean of Column ", names(AdultData[i]), "is ", ME, "\n" )
  MED <- median(AdultData[, i],na.rm=TRUE)
  cat("Median of Column", names(AdultData[i]), "is ", MED, "\n\n" )
  
}

#7 Show me Histogram of all the numerical values by using a For loop. I would also like to see good heading and 
#meaningfull X & Y coordinates
for (i in 1:NumCols) {
  if (is.character(AdultData[, i])) {
    next
  }
  hist(AdultData[,i], col=rgb(0,1,0,.5), xlab = names(AdultData[i]), main = paste("Histogram of " , names(AdultData[i])))
}

#8 Remove all non-numeric columns from the data frame and then make a scatter plot, I would also like to see the column name
# of all the removed non-numeric columns.
NumCols <- ncol(AdultData)
j <- 1
while (j <= NumCols) {
      #browser()
      if (is.character(AdultData[, j])) {
        cat("Column", names(AdultData[j]), "is being removed for plotting as it contains non numeric data","\n" )
        AdultData[j] <- list(NULL)
        NumCols <- ncol(AdultData)
        j = j - 1
      }
  j = j +1
}

#8 Plot AdultData object.
plot(AdultData)

#9a How can you tell if a vector contains continuous numbers or binary data?
# Binary data appear as dots in a straight line, vector # 10(Selector) is a good example.
#9b Which two vectors are most strongly correlated?
# TB and DB seems to be strongly correlated as the plot appears in an approximately 45 degree line.
#9c Give an example of two vectors that have little correlation
# Age & Selector have no correlation. 

#10 Remove Outlier:
#Clear workspace
rm(list=ls())

x <- c(1, -1, -1, 1, 1, 17, -3, 1, 1, 3)
highLimit <- mean(x) + 2*sd(x)
lowLimit <- mean(x) - 2*sd(x)
goodFlag <- (x < highLimit) & (x > lowLimit)
x[goodFlag]
x
x <- x[goodFlag]
x
#cat("Outlier has been removed : ", x,"\n" )

#11 Relabel this:  

#Clear workspace
rm(list=ls())

x <- c('BS', 'MS', 'PhD', 'HS', 'Bachelors', 'Masters', 'High School', 'MS', 'BS', 'MS')
# All Bachelors should be BS
x[x == "Bachelors"] <- "BS"
x
# All Masters should be MS
x[x == "Masters"] <- "MS"
x
# All High School should be HS
x[x == "High School"] <- "HS" 
x
#cat("Relabelled vector : ", x,"\n" )

#12 Min-Max Normalization:

#Clear workspace
rm(list=ls())
x <- c(1, -1, -1, 1, 1, 17, -3, 1, 1, 3)
y <- 1000*x
a <- min(x)
b <- max(x) - min(x)
normalized <- (x - a) / b
normalized

a <- min(y)
b <- max(y) - min(y)
normalized <- (y - a) / b
normalized
#cat("Normalized vector through Min-Max method : ", normalized,"\n" )

#13 z-Score Normalization
a <- mean(x)
b <- sd(x)
normalized <- (x - a) / b
normalized

a <- mean(y)
b <- sd(y)
normalized <- (y - a) / b
normalized
#cat("Normalized vector through Z-Score method : ", normalized,"\n" )

#14 Binarize

#Clear workspace
rm(list=ls())

x <- c('Red', 'Green', 'Blue', 'Green', 'Blue', 'Blue', 'Red', 'Blue', 'Green', 'Blue')
isRed <- x == 'Red'
isGreen <- x == 'Green'
isBlue <- x == 'Blue'

isRed
isGreen
isBlue

# You can cast T/F into 1/0
isRed <- as.numeric(isRed)
isGreen <- as.numeric(isGreen)
isBlue <- as.numeric(isBlue)

# Better Presentation:
isRed; isGreen; isBlue
# Or, as a data frame
data.frame(isRed, isGreen, isBlue)
#cat("Binarized numeric data for Red: ", isRed,"\n" )
#cat("Binarized numeric data for Blue: ", isBlue,"\n" )
#cat("Binarized numeric data for Green: ", isGreen,"\n" )

#15 Discretization into 3 bins

#Clear workspace
rm(list=ls())

x <- c(81, 3, 3, 4, 4, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 9, 12, 24, 24, 25)
range <- max(x) - min(x)
binWidth <- range / 3
bin1Min <- -Inf
bin1Max <- min(x) + binWidth
bin2Max <- min(x) + 2*binWidth
bin3Max <- Inf
xDiscretized <- rep(NA, length(x))
xDiscretized
xDiscretized[bin1Min < x & x <= bin1Max] <- "Low"
xDiscretized[bin1Max < x & x <= bin2Max] <- "Medium"
xDiscretized[bin2Max < x & x <= bin3Max] <- "High"
xDiscretized

#16 Discretization into 3 bins using equalization technique

#Clear workspace
rm(list=ls())

x <- c(81, 3, 3, 4, 4, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 9, 12, 24, 24, 25)
sort(x)
# [1] 3  3  4  4  5  5  5  5  5  5  5  6  6  6  6  6  7  7  7  7  8  8  9 12 24 24 25 81
#    <---------------------------------|--------------------------|----------|--------|->
# Kindergarteners:  3  3  4  4  5  5  5  5  5  5  5  
veryLowMin <- -Inf
VeryLowMax <- 5
# Older Siblings:  6  6  6  6  6 7  7  7  7  8  8  9 12
LowMax <- 12
# Teachers:  24 24 25
HighMax <- 25
# Grandfather:  81
VeryHighMax <- Inf
xDiscretized <- x
xDiscretized[veryLowMin < x & x <=  VeryLowMax] <- "Kinder"
xDiscretized[VeryLowMax < x & x <=      LowMax] <- "olderSib"
xDiscretized[LowMax     < x & x <=     HighMax] <- "Teacher"
xDiscretized[HighMax    < x & x <= VeryHighMax] <- "Grandfather"
xDiscretized
