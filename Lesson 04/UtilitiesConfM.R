Accuracy <- function(Table.X)
{
  Accuracy.X <- (Table.X[1,1]+Table.X[2,2])/(Table.X[1,1]+Table.X[1,2]+Table.X[2,1]+Table.X[2,2])
  return(Accuracy.X)
}
Sensitivity <- function(Table.X)
{
  Sensitivity.X <- (Table.X[1,1])/(Table.X[1,1]+Table.X[2,1])
  return(Sensitivity.X)
}
Specificity <- function(Table.X)
{
  Specificity.X <- (Table.X[2,2])/(Table.X[1,2]+Table.X[2,2])
  return(Specificity.X)
}
Precision <- function(Table.X)
{
  Precision.X <- (Table.X[1,1])/(Table.X[1,1]+Table.X[1,2])
  return(Precision.X)
}
Recall <- function(Table.X)
{
  Recall.X <- (Table.X[1,1])/(Table.X[1,1]+Table.X[2,1])
  return(Recall.X)
}  
FPR <- function(Table.X)
{
  FPR.X <- (Table.X[1,2])/(Table.X[1,2]+Table.X[2,2])
  return(FPR.X)
}
