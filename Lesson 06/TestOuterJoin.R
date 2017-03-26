# TestOuterJoin.R
# Copyright 2016 by Ernst Henle

# Clear Workspace
rm(list=ls())
# Clear Console:
cat("\014")

library(sqldf)
A.A <- c(1,2,4)
A.M <- c("m", "n", "o")
B.A <- c(2,3,5)
B.N <- c("p", "q", "r")

# Try out your function in place of the following:
OuterJoin <- function(table_A, table_B, keyA, keyB)
{
  condition <- paste("table_A.",keyA," = ", "table_B.",keyB, sep="")
  SqlStatementLeft <- paste("select table_A.*, table_B.* from table_A left join table_B on", condition)
  SqlStatementRight <- paste("select table_A.*, table_B.* from table_B left join table_A on", condition)
  SqlStatement <- paste(SqlStatementLeft, "union", SqlStatementRight)
  sqldf(SqlStatement, stringsAsFactors = FALSE)
} # OuterJoin
browser()
tryCatch({
  table_A <- data.frame(A=A.A, M=A.M)
  table_B <- data.frame(A=B.A, N=B.N)
  print("##########################")
  print("OuterJoin column names are not unique")
  print(OuterJoin(table_A, table_B, "A", "A"))
  print("##########################")
},
warning = function(war){cat("Unexpected warning\n");   print(war); success <- war; return("Balderdash")}, 
error =   function(err){cat("Unhandeled exception\n"); print(err); success <- err; return(success)}) # tryCatch

tryCatch({
  table_A <- data.frame(A=A.A, M=A.M)
  table_B <- data.frame(B=B.A, N=B.N)
  print("##########################")
  print("OuterJoin unique column names")
  print(OuterJoin(table_A, table_B, "A", "B"))
  print("##########################")
},
warning = function(war){cat("Unexpected warning\n");   print(war); success <- war; return("Balderdash")}, 
error =   function(err){cat("Unhandeled exception\n"); print(err); success <- err; return(success)}) # tryCatch

tryCatch({
  table_A <- data.frame(keyA=A.A, M=A.M)
  table_B <- data.frame(keyB=B.A, N=B.N)
  print("##########################")
  print("OuterJoin hard-coded key column names to keyA and keyB")
  print(OuterJoin(table_A, table_B, "keyA", "keyB"))
  print("##########################")
},
warning = function(war){cat("Unexpected warning\n");   print(war); success <- war; return("Balderdash")}, 
error =   function(err){cat("Unhandeled exception\n"); print(err); success <- err; return(success)}) # tryCatch

tryCatch({
  print("##########################")
  print("OuterJoin arguments are table names as opposed to actual data frames")
  print(OuterJoin("table_A", "table_B", "keyA", "keyB"))
  print("##########################")
},
warning = function(war){cat("Unexpected warning\n");   print(war); success <- war; return("Balderdash")}, 
error =   function(err){cat("Unhandeled exception\n"); print(err); success <- err; return(success)}) # tryCatch
