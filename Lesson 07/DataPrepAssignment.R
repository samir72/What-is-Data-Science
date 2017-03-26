# Clear Workspace
rm(list=ls())
# Clear Console
cat("\014")

#	Item3: Download census income data from the following location. the file.
#	http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
AdultData <- read.table(url, header=FALSE, sep = ",", stringsAsFactors=FALSE)
#	Binarize the sex attribute to a numeric value.
AdultData$V10 <- ifelse(trimws(AdultData$V10) == "Male", 0, 1)


#	Item 4: Add the following headers to the census data.
headers <- c("age", "workclass","fnlwgt","education","education-num","marital-status","occupation","relationship","race","sex","capital-gain","capital-loss","hours-per-week","native-country","Income")
names(AdultData) <- headers
#	Take a copy of this data as we would be using this copy later in the assignment.
AdultData_Copy <- AdultData

# Item 5: Display the first 6 rows of census income data. (This would be excluding the header data).
head(AdultData)

# Item 6: Remove all rows with '?' in any of the attributes.(I am treating ?s as NAs in this Question). 
AdultData <- subset(AdultData, trimws(AdultData[, 1])!="?")
AdultData <- subset(AdultData, trimws(AdultData[, 2])!="?")
AdultData <- subset(AdultData, trimws(AdultData[, 3])!="?")
AdultData <- subset(AdultData, trimws(AdultData[, 4])!="?")
AdultData <- subset(AdultData, trimws(AdultData[, 5])!="?")
AdultData <- subset(AdultData, trimws(AdultData[, 6])!="?")
AdultData <- subset(AdultData, trimws(AdultData[, 7])!="?")
AdultData <- subset(AdultData, trimws(AdultData[, 8])!="?")
AdultData <- subset(AdultData, trimws(AdultData[, 9])!="?")
AdultData <- subset(AdultData, trimws(AdultData[, 10])!="?")
AdultData <- subset(AdultData, trimws(AdultData[, 11])!="?")
AdultData <- subset(AdultData, trimws(AdultData[, 12])!="?")
AdultData <- subset(AdultData, trimws(AdultData[, 13])!="?")
AdultData <- subset(AdultData, trimws(AdultData[, 14])!="?")
AdultData <- subset(AdultData, trimws(AdultData[, 15])!="?")
# Once the dataset is clean calculate the standard deviation, mean and median using the sapply function. 
sapply(AdultData, mean, na.rm=T)
sapply(AdultData, sd, na.rm=T)
sapply(AdultData, median, na.rm=T)

#Item 7: Create histogram for all the numerical attributes with a good heading, X & Y Coordinates. 
#Provide names of all attributes causing problem with this function.
hist(AdultData$age, col=rgb(0,1,0,.5), xlab = names(AdultData)[1], main = paste("Histogram of " , names(AdultData)[1]))
hist(AdultData$workclass, col=rgb(0,1,0,.5), xlab = names(AdultData)[2], main = paste("Histogram of " , names(AdultData)[2]))
hist(AdultData$fnlwgt, col=rgb(0,1,0,.5), xlab = names(AdultData)[3], main = paste("Histogram of " , names(AdultData)[3]))
hist(AdultData$education, col=rgb(0,1,0,.5), xlab = names(AdultData)[4], main = paste("Histogram of " , names(AdultData)[4]))
hist(AdultData$`education-num`, col=rgb(0,1,0,.5), xlab = names(AdultData)[5], main = paste("Histogram of " , names(AdultData)[5]))
hist(AdultData$`marital-status`, col=rgb(0,1,0,.5), xlab = names(AdultData)[6], main = paste("Histogram of " , names(AdultData)[6]))
hist(AdultData$occupation, col=rgb(0,1,0,.5), xlab = names(AdultData)[7], main = paste("Histogram of " , names(AdultData)[7]))
hist(AdultData$relationship, col=rgb(0,1,0,.5), xlab = names(AdultData)[8], main = paste("Histogram of " , names(AdultData)[8]))
hist(AdultData$race, col=rgb(0,1,0,.5), xlab = names(AdultData)[9], main = paste("Histogram of " , names(AdultData)[9]))
hist(AdultData$sex, col=rgb(0,1,0,.5), xlab = names(AdultData)[10], main = paste("Histogram of " , names(AdultData)[10]))
hist(AdultData$`capital-gain`, col=rgb(0,1,0,.5), xlab = names(AdultData)[11], main = paste("Histogram of " , names(AdultData)[11]))
hist(AdultData$`capital-loss`, col=rgb(0,1,0,.5), xlab = names(AdultData)[12], main = paste("Histogram of " , names(AdultData)[12]))
hist(AdultData$`hours-per-week`, col=rgb(0,1,0,.5), xlab = names(AdultData)[13], main = paste("Histogram of " , names(AdultData)[13]))
hist(AdultData$`native-country`, col=rgb(0,1,0,.5), xlab = names(AdultData)[14], main = paste("Histogram of " , names(AdultData)[14]))
hist(AdultData$Income, col=rgb(0,1,0,.5), xlab = names(AdultData)[15], main = paste("Histogram of " , names(AdultData)[15]))

#Item 8: Remove all non-numeric attributes from the data frame
AdultData$workclass <- NULL
AdultData$education <- NULL
AdultData$`marital-status` <- NULL
AdultData$occupation <- NULL
AdultData$relationship <- NULL
AdultData$race <- NULL
AdultData$`native-country` <- NULL
AdultData$Income <- NULL
# then take take a random sample of 10% census data to make a scatter plot with all the numeric attributes.
random <-runif(nrow(AdultData))
subsampleflg <- random <= .1
subsample <- AdultData[subsampleflg, ]
#Plot sub-sample of AdultData object.
plot(subsample)


# Item 9: Look at the plots from previous question and answer the following.
# How can you tell if a vector contains binary data?
# Binary data appear as dots in a straight line, plot for sex is a good example.
# What is the relationship between education-num and capital gain ?
# Straight lines with outliers are visible between them.
# Give an example of two vectors that have little correlation
# Education-num and hours per week. 

# Item 10: Display outliers in education_num using boxplot and then remove those 
# Outliers and show the change in the boxplot.

boxplot(AdultData$`education-num`)

highLimit <- mean(AdultData$`education-num`) + 2*sd(AdultData$`education-num`)
lowLimit <- mean(AdultData$`education-num`) - 2*sd(AdultData$`education-num`)
goodFlag <- (AdultData$`education-num` < highLimit) & (AdultData$`education-num` > lowLimit)
x <- AdultData$`education-num`[goodFlag]
# Compare updated boxplots
boxplot(x, AdultData$`education-num`)
# Replace outliers with mean and see the change in boxplot.
y <- ifelse((AdultData$`education-num` < lowLimit), mean(AdultData$`education-num`), AdultData$`education-num`)
boxplot(y, AdultData$`education-num`)

#Item 11: Relabel categories in attribute race as a new attribute race_label.
#You would have to use the copy of original data from Item 4 as all the character attributes have been removed during the course of this assignment.
AdultData_Copy$race_label <- ifelse(trimws(AdultData_Copy$race) == "White", "W", ifelse(trimws(AdultData_Copy$race) == "Black","B",ifelse(trimws(AdultData_Copy$race) == "Asian-Pac-Islander","API",ifelse(trimws(AdultData_Copy$race) == "Amer-Indian-Eskimo","AIE","O"))))

# Item 12: Normalize fnlwgt vector using Min-Max Normalization.
x <- AdultData$fnlwgt
a <- min(x)
b <- max(x) - min(x)
normalized <- (x - a) / b
# Provide a histogram of normalized vector.
hist(normalized, col=rgb(0,1,0,.5),main = paste("Histogram of fnlwgt using min-max normalization."))

# Item 13: Normalize fnlwgt vector using Z-Score method.
x <- AdultData$fnlwgt
a <- mean(x)
b <- sd(x)
normalized <- (x - a) / b
#	Provide a histogram of normalized vector.
hist(normalized, col=rgb(0,1,0,.5),main = paste("Histogram of fnlwgt using z-score normalization."))

# Item 14: Binarize the relationship Attribute and present the result in a data frame.
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


# Item 15: Discretize age using equal range discretization.
x <- AdultData$age
range <- max(x) - min(x)
binWidth <- range / 3
bin1Min <- -Inf
bin1Max <- min(x) + binWidth
bin2Max <- min(x) + 2*binWidth
bin3Max <- Inf
xDiscretized <- rep(NA, length(x))
xDiscretized[bin1Min < x & x <= bin1Max] <- "L"
xDiscretized[bin1Max < x & x <= bin2Max] <- "M"
xDiscretized[bin2Max < x & x <= bin3Max] <- "H"
# Use table(DiscretizedVector) to summarise the result.
table(xDiscretized)
cat("Discretisized age summary using equal range method:", "\n",table(xDiscretized),"\n" )


# Item 16: Discretize age using equal amount discretization.
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
#	Use table(DiscretizedVector) to summarise the result.
table(vdiscretized)
cat("Discretisized age summary using equal amount method:", "\n",table(vdiscretized),"\n" )