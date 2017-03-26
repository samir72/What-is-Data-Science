# Clear Workspace
rm(list=ls())
# Clear Console
cat("\014")

#1.	Download census income data. Location (a) provides you the data and (b) provides the header details for the file.
#a.	http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data
#b.	http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.names
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
url1 <- "http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names"
AdultData <- read.table(url, header=FALSE, sep = ",", stringsAsFactors=FALSE)
AdultNames <- read.csv2(url1, header=FALSE, sep = ":")

#2.	Location (a) provides you the census income data. Delete the last Attribute of the downloaded census income data. 
#Review the data from location (b) and add the Attribute headers to the census data.
#Delete the salary projection as no projection is being done in this exercise.
AdultData[15] <- list(NULL)
headers <- c("age", "workclass","fnlwgt","education","education-num","marital-status","occupation","relationship","race","sex","capital-gain","capital-loss","hours-per-week","native-country")
names(AdultData) <- headers

#3.	Binarize the sex Attribute to a numeric value. Pay special attention to the data while performing this task.
# I am using trimews to remove leading and trailing whitespace.
AdultData$sex_num <- ifelse(trimws(AdultData$sex) == "Male", 0, 1)

#4.	Display the first 6 rows of census income data. (This would be excluding the header data).
head(AdultData)

#5.	Remove all rows with ‘?’ in any of the attributes. Provide names of all ? filled attributes and the respective count of deleted rows.
#a.	A bonus question would be to provide the names of all the healthy attributes and the total count of all the removed records. 
Badrowcount <- 0
NumCols <- ncol(AdultData) # Get number of Attributes for the loop.
for (i in 1:NumCols) {
  #browser()
  rowcount = nrow(subset(AdultData, trimws(AdultData[, i]) =="?"))
  if (nrow(subset(AdultData, trimws(AdultData[, i])=="?")) == 0) {
    cat("Attribute", names(AdultData[i]), "is being skipped as it contains no missing data.","\n" )
    next
  }
  cat("\n")
  cat("Attribute", names(AdultData[i]), "contains ? hence" , rowcount, "rows are being deleted." ,"\n" )
  AdultData <- subset(AdultData, trimws(AdultData[, i])!="?")
  Badrowcount <- Badrowcount + rowcount
  cat("\n")
}
#Provide the total number of rows deleted with ? data.
cat("A total of", Badrowcount , "rows are being deleted." ,"\n" )

#6.	Determine the standard deviation, mean and median using the base R functions. 
#a.	Above calculations are not possible for all the attributes, provide names of all such exception attributes.
NumCols <- ncol(AdultData) # Get number of Attributes for the loop.
for (i in 1:NumCols) {
  if (is.character(AdultData[, i])) {
    cat("Attribute", names(AdultData[i]), "is being skipped as its non numeric.","\n" )
    next
  }
  cat("\n")
  SD <- sd(AdultData[, i],na.rm=TRUE)
  cat("SD of Attribute", names(AdultData[i]), "is ", SD, "\n" )
  ME <- mean(AdultData[, i],na.rm=TRUE)
  cat("Mean of Attribute ", names(AdultData[i]), "is ", ME, "\n" )
  MED <- median(AdultData[, i],na.rm=TRUE)
  cat("Median of Attribute", names(AdultData[i]), "is ", MED, "\n\n" )
  }

#7.	Create histogram for all the numerical values by using a For loop. Provide a good heading, X & Y coordinates for each histogram.
#a.	Goal is not to write function hist for dozen of times.
for (i in 1:NumCols) {
  if (is.character(AdultData[, i])) {
    next
  }
  hist(AdultData[,i], col=rgb(0,1,0,.5), xlab = names(AdultData[i]), main = paste("Histogram of " , names(AdultData[i])))
}

#8.	Remove all non-numeric attributes from the data frame and then make a scatter plot with all the numeric attributes.
#a.	Provide names of all the attributes which cannot be plotted.
NumCols <- ncol(AdultData)
j <- 1
while (j <= NumCols) {
      #browser()
      if (is.character(AdultData[, j])) {
        cat("Attribute", names(AdultData[j]), "is being removed for plotting as it contains non numeric data.","\n" )
        AdultData[j] <- list(NULL)
        NumCols <- ncol(AdultData)
        j = j - 1
      }
  j = j +1
}

#Plot AdultData object.
plot(AdultData)


#9 How can you tell if a vector contains binary data?
# Binary data appear as dots in a straight line, plot for sex_num is a good example.
#10 What is the relationship between education and capital gain ?
# A positive linear regression with outliers.
#11 Give an example of two vectors that have little correlation
# Education and hours per week. 

#12 Display outliers in education_num using boxplot and then remove those outliers and show the
# change in the boxplot
boxplot(AdultData$`education-num`)

highLimit <- mean(AdultData$`education-num`) + 2*sd(AdultData$`education-num`)
lowLimit <- mean(AdultData$`education-num`) - 2*sd(AdultData$`education-num`)
goodFlag <- (AdultData$`education-num` < highLimit) & (AdultData$`education-num` > lowLimit)
x <- AdultData$`education-num`[goodFlag]
# Compare updated boxplots
boxplot(x, AdultData$`education-num`)

#13 Replace outliers with mean and see the change in boxplot.
y <- ifelse((AdultData$`education-num` < lowLimit), mean(AdultData$`education-num`), AdultData$`education-num`)
boxplot(y, AdultData$`education-num`)

#14.Relabel the race vector.
#a.	You would have to reload the data as during the course of the previous questions, all the character attributes have been removed.
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
url1 <- "http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names"
AdultData <- read.table(url, header=FALSE, sep = ",", stringsAsFactors=FALSE)
AdultNames <- read.csv2(url1, header=FALSE, sep = ":")
#Delete the salary projection calculated by NaiveBayes Model.
AdultData[15] <- list(NULL)
headers <- c("age", "workclass","fnlwgt","education","education-num","marital-status","occupation","relationship","race","sex","capital-gain","capital-loss","hours-per-week","native-country")
names(AdultData) <- headers

# Relabel all the race values as a new attribute in the table.
AdultData$race_label <- ifelse(trimws(AdultData$race) == "White", "W", ifelse(trimws(AdultData$race) == "Black","B",ifelse(trimws(AdultData$race) == "Asian-Pac-Islander","API",ifelse(trimws(AdultData$race) == "Amer-Indian-Eskimo","AIE","O"))))

#15.	Normalize fnlwgt vector using Min-Max Normalization.
#a.	Provide a histogram of normalized vector.

x <- AdultData$fnlwgt
y <- 1000*x
a <- min(x)
b <- max(x) - min(x)
normalized <- (x - a) / b
hist(normalized, col=rgb(0,1,0,.5),main = paste("Histogram of fnlwgt using min-max normalization."))

a <- min(y)
b <- max(y) - min(y)
normalized <- (y - a) / b
hist(normalized, col=rgb(0,1,0,.5),main = paste("Histogram(*1000) of fnlwgt using min-max normalization."))

#16.	Normalize fnlwgt vector using Z-Score method.
#a.	Provide a histogram of normalized vector.
x <- AdultData$fnlwgt
y <- 1000*x
a <- mean(x)
b <- sd(x)
normalized <- (x - a) / b
hist(normalized, col=rgb(0,1,0,.5),main = paste("Histogram of fnlwgt using z-score normalization."))

a <- mean(y)
b <- sd(y)
normalized <- (y - a) / b
hist(normalized, col=rgb(0,1,0,.5),main = paste("Histogram(*1000) of fnlwgt using z-score normalization."))


#17.	Binarize the relationship Attribute and present the result in a data frame.
x <- AdultData$relationship
isNotFamily <- x == ' Not-in-family'
isHusband <- x == ' Husband'
isWife <- x == ' Wife'
isUnmarried <- x == ' Unmarried'
isOwnChild <- x == ' Own-child'
isOtherrelative <- x == ' Other-relative'

# You can cast True/False into 1/0
isNotFamily <- as.numeric(isNotFamily)
isHusband <- as.numeric(isHusband)
isWife <- as.numeric(isWife)
isUnmarried <- as.numeric(isUnmarried)
isOwnChild <- as.numeric(isOwnChild)
isOtherrelative <- as.numeric(isOtherrelative)

# Presentation as a data frame.
binary_relationship <- data.frame(isNotFamily, isHusband, isWife, isUnmarried, isOwnChild, isOtherrelative)


#18.	Discretize age using equal range discretization.
#a.	Use table(DiscretizedVector) to summarise the result.
x <- AdultData$age
range <- max(x) - min(x)
binWidth <- range / 3
bin1Min <- -Inf
bin1Max <- min(x) + binWidth
bin2Max <- min(x) + 2*binWidth
bin3Max <- Inf
xDiscretized <- rep(NA, length(x))
#xDiscretized
xDiscretized[bin1Min < x & x <= bin1Max] <- "L"
xDiscretized[bin1Max < x & x <= bin2Max] <- "M"
xDiscretized[bin2Max < x & x <= bin3Max] <- "H"
table(xDiscretized)
cat("Discretisized age summary using equal range method:", "\n",table(xDiscretized),"\n" )


#19.	Discretize age using equal amount discretization.
#a.	Use table(DiscretizedVector) to summarise the result.
x <- AdultData$age
numberOfBins <- 3
vSorted <- sort(x)
binRange <- length(vSorted) / numberOfBins
bin1Min <- -Inf
bin1Max <- vSorted[round(binRange)]
bin2Max <- vSorted[round(2*binRange)]
bin3Max <- +Inf
vdiscretized <- x
vdiscretized[bin1Min < x & x <= bin1Max] <- "L"
vdiscretized[bin1Max < x & x <= bin2Max] <- "M"
vdiscretized[bin2Max < x & x <  bin3Max] <- "H"
table(vdiscretized)
cat("Discretisized age summary using equal amount method:", "\n",table(vdiscretized),"\n" )