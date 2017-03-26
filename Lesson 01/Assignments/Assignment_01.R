#Clear workspace
rm(list=ls())
# assign a url to variable "url"
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/00225/Indian%20Liver%20Patient%20Dataset%20(ILPD).csv"
#4 Download the dataset
ILPD <- read.csv(url, header=FALSE, stringsAsFactors=FALSE)
#4 Create headers.
headers <- c("Age Age of the patient", "Gender Gender of the patient","TB Total Bilirubin","DB Direct Bilirubin","Alkphos Alkaline Phosphotase","Sgpt Alamine Aminotransferase","Sgot Aspartate Aminotransferase","TP Total Protiens","ALB Albumin","A/G Ratio Albumin and Globulin Ratio","Selector field used to split the data into two sets (labeled by the experts)")
#4 Add headers to the dataset.
names(ILPD) <- headers
#5 view first 6 rows
head(ILPD)
#6 determine the standard mean,median and SD.
#Loop to calculate SD, Mean and median of all numeric columns. 
#Second column of gender contains non-numeric values hence has been skipped for these calculations.
NumCols <- ncol(ILPD) # Get number of columns for the loop.
for (i in 1:NumCols) {
  if (is.character(ILPD[, i])) {
    cat("Column # ", i, "is being skipped as its non numeric","\n" )
    next
  }
  SD <- sd(ILPD[, i],na.rm=TRUE)
  cat("SD of Column # ", i, "is ", SD, "\n" )
  ME <- mean(ILPD[, i],na.rm=TRUE)
  cat("Mean of Column # ", i, "is ", ME, "\n" )
  MED <- median(ILPD[, i],na.rm=TRUE)
  cat("Median of Column # ", i, "is ", MED, "\n" )
  
}

#7 Histograms for all numerical values.
for (i in 1:NumCols) {
  if (is.character(ILPD[, i])) {
    next
  }
  hist(ILPD[,i], col=rgb(0,1,0,.5))
}

#8 Removing all non-numeric columns from the data frame.
NumCols <- ncol(ILPD)
j <- 1
while (j <= NumCols) {
#      browser()
      if (is.character(ILPD[, j])) {
        cat("Column # ", j, "is being removed for plotting as it contains non numeric data","\n" )
        ILPD[j] <- list(NULL)
        NumCols <- ncol(ILPD)
      }
  j = j +1
}

#8 Plot ILPD object.
plot(ILPD)

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

# Min max normalization of a multidimensional matix
rm(list=ls())
#x <- matrix(c(5, 7, 2, 1, -1, 0, 1, 1), nrow=4)
x <- matrix(c(230, 2.21, 0.84, 3.54, 1.90, 11, 1, 1,3,4), nrow=5)
y <- 1000*x

a <- min(x[,1])
b <- max(x[,1]) - min(x[,1])
normalized_1 <- (x[,1] - a) / b


a <- min(x[,2])
b <- max(x[,2]) - min(x[,2])
normalized_2 <- (x[,2] - a) / b

normalized<- as.matrix(data.frame(normalized_1, normalized_2))
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

x <- c(81,3, 3, 4, 4, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 9, 12, 24, 24, 25)
quantile(x)
IQR(x)
sort(x)
# [1] 3  3  4  4  5  5  5  5  5  5  5  6  6  6  6  6  7  7  7  7  8  8  9 12 24 24 25 81
#    <-----------------------------------------------|----------------------|--------|-->
# Kindergarteners:  3  3  4  4  5  5  5  5  5  5  5  6  6  6  6  6
veryLowMin <- -Inf
VeryLowMax <- 5
# Older Siblings:  7  7  7  7  8  8  9 12
LowMax <- 6
# Teachers:  24 24 25
HighMax <- 8
# Grandfather:  81
VeryHighMax <- Inf
xDiscretized <- x
xDiscretized[veryLowMin < x & x <=  VeryLowMax] <- "Kinder"
xDiscretized[VeryLowMax < x & x <=      LowMax] <- "olderSib"
#xDiscretized[LowMax     < x & x <=     HighMax] <- "Teacher"
xDiscretized[LowMax     < x & x <=     VeryHighMax] <- "Teacher"
#xDiscretized[HighMax    < x & x <= VeryHighMax] <- "Grandfather"
xDiscretized
