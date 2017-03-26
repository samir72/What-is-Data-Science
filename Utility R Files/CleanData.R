OuterJoin <- function(Data1)
# I am treating ? as NA in this assignment.
# There are three columns (workclass, occupation,native country with ? data. )
# Remove all rows with a ? data and provide me the count of rows deleted for each instance.
Badrowcount <- 0
NumCols <- ncol(Data1) # Get number of columns for the loop.
for (i in 1:NumCols) {
  #browser()
  rowcount = nrow(subset(Data1, trimws(Data1[, i]) =="?"))
  if (nrow(subset(Data1, trimws(Data1[, i])=="?")) == 0) {
    cat("Column", names(Data1[i]), "is being skipped as it contains no bad data","\n" )
    next
  }
  cat("\n")
  cat("Column", names(Data1[i]), "contains ? hence" , rowcount, "rows are being deleted" ,"\n" )
  Data1 <- subset(Data1, trimws(Data1[, i])!="?")
  Badrowcount <- Badrowcount + rowcount
  cat("\n")
  return(Data1)
}