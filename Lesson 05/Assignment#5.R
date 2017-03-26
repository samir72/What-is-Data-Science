# Clear objects from Memory
rm(list=ls())
# Clear Console:
cat("\014")
source("Assignment#5_Function.R")
# Set seed data
set.seed(23)
L3 <- LETTERS[1:5]
Table1 <- data.frame(id=1:10, Alpha=sample(L3, 10, replace = TRUE))
print("Table1")
print(Table1)
Table2 <- subset(Table1, Alpha == 'A')
print("Table2")
print(Table2)

JoinClause1 <- sqldf('select * from Table1 left outer join Table2 ON Table1.Alpha=Table2.Alpha')
#print("JoinClause1")
#print(JoinClause1)

JoinClause2 <- sqldf('select * from Table2 left outer join Table1 ON Table1.Alpha=Table2.Alpha')
#print("JoinClause2")
#print(JoinClause2)

JoinClausewithUnion <- sqldf('select * from Table1 left outer join Table2 ON Table1.Alpha=Table2.Alpha UNION 
                             select * from Table2 left outer join Table1 ON Table1.Alpha=Table2.Alpha')

FullJoin <- rbind(JoinClause1, JoinClause2)
print("FullJoin without function call")
print(FullJoin)

CallFullJoin <- OuterJoin(Table1,Table2,Alpha,Alpha)
print("FullJoin with a function call")
print(CallFullJoin)

Result <- identical(FullJoin,CallFullJoin)
cat("-----------------------------------------------------------------\n")
cat("Are data frames identical with and without function call ? ", Result)

