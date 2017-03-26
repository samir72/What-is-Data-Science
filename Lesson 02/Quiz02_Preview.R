#Quiz02_Preview.txt
################################################
#Match each of the following english statements with one line of code
#English Statement:
#Replace outliers in numeric vector x
#Replace all low values in x with a lower bound
#Normalize vector x by Min-Max or Range (Feature Scaling)
#Keep only the outliers of vector x
#Remove from data frame x any row that has one or more NAs
#Keep only the rows in data frame x that have one or more NAs

# Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")
x <- c("WA", "Washington", "Wash", "UT", "Utah", "Utah", "UT", "Utah", "IO") 
x[x == "Washington"] <- "WA"; x[x == "Wash"] <- "WA"; x[x == "Utah"] <- "UT"
#x<-c(1,2,3,4,5,6,7,8,9,23)
x <- c("B", "A", "B", "B", "B", "A")
x <- as.numeric(x == "A")
x
x <- na.omit(x)
x <- as.character(x)
#x <- x[(x > mean(x) + 2*sd(x)) | (x < mean(x) - 2*sd(x))]
x

#Code:
x[(x > mean(x) + 2*sd(x)) | (x < mean(x) - 2*sd(x))] <- median(x)
x <- x[(x < mean(x) + 2*sd(x)) & (x > mean(x) - 2*sd(x))]
x <- x[(x > mean(x) + 2*sd(x)) | (x < mean(x) - 2*sd(x))]
x <- (x - min(x))/(max(x) - min(x))
x <- (x - mean(x))/sd(x)
x[x < mean(x) - 2*sd(x)] <- mean(x) - 2*sd(x)
x[x < mean(x) + 2*sd(x)] <- mean(x) - 2*sd(x)
x[x < mean(x) - 2*sd(x)] <- mean(x) + 2*sd(x)
x[x < mean(x) + 2*sd(x)] <- mean(x) + 2*sd(x)
x <- x == NA
#x <- x[!complete.cases(x),]
#x <- x != NA
x <- na.omit(x)
x <- x[x = !is.na(x)]
x <- x[!is.na(x)]
################################################
#Match each of the following lines of code with one english statement 
#Code:
x[x > mean(x) + 2*sd(x)] <- mean(x) + 2*sd(x)
x[x < mean(x) -2*sd(x)] <- (x - mean(x))/sd(x)
x <- (x - mean(x))/sd(x)

#English Statement:
#replace high outliers with an upper bound
#replace low outliers with a lower bound
#z-normalize the values
#perform feature scaling
################################################
#The following three vectors are the result of creating dummy variables (binarization):
isred:  c(1, 0, 1, 0, 1, 0, 0, 0)
ispink:  c(0, 0, 0, 1, 0, 0, 0, 1)
ismagenta: c(0, 1, 0, 0, 0, 0, 0, 0)

#What did the original vector look like?

isred <-  c(1, 0, 1, 0, 1, 0, 0, 0)
ispink <-  c(0, 0, 0, 1, 0, 0, 0, 1)
ismagenta <- c(0, 1, 0, 0, 0, 0, 0, 0)

isred[isred == 1 ] <- 'R'
ispink[ispink == 1 ] <- 'P'
ismagenta[ismagenta == 1 ] <- 'M'
isred[isred == 0 ] <- NA
ispink[ispink == 0 ] <- NA
ismagenta[ismagenta == 0 ] <- NA
isred <- isred[!is.na(isred)]
ispink <- ispink[!is.na(ispink)]
ismagenta <- ismagenta[!is.na(ismagenta)]
ZZ <- c(isred,ispink,ismagenta)

################################################
#Simplify the vector x by relabeling categories:
x <- c("WA", "Washington", "Wash", "UT", "Utah", "Utah", "UT", "Utah", "IO")
################################################
