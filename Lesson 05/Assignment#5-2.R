# Clear objects from Memory
rm(list=ls())
# Clear Console:
cat("\014")
source("Assignment#5_Function-3.R")
#library(sqldf)
# Set seed data
set.seed(23)
L3 <- LETTERS[1:2]
Table1 <- data.frame(id=1:3, Alpha=sample(L3, 3, replace = TRUE))
print("Table1")
print(Table1)
Table2 <- subset(Table1, Alpha == 'A')
print("Table2")
print(Table2)

Table3 <- data.frame(id=1:4,Alpha=c('B','A','B','A'))
print("Table3")
print(Table3)

Table4 <- data.frame(id=1:4,Alpha=sample(L3, 4, replace = TRUE),Alpha=sample(L3, 4, replace = TRUE))
print("Table4")
print(Table4)

Table5 <- data.frame(Alpha=sample(L3, 10, replace = TRUE))
print("Table5")
print(Table5)

Table6 <- data.frame(id=c('1','3'))
print("Table6")
print(Table6)

JoinClause1 <- sqldf('select * from Table1, Table3 where Table1.id = Table3.id')
print("JoinClause1")
print(JoinClause1)

JoinClause2 <- sqldf('select * from Table4')
print("JoinClause2")
print(JoinClause2)

JoinClausewithUnion <- sqldf('select Table1.id, Table5.Alpha from Table1,Table5 UNION 
                             select * from Table1')
print("FullJoin with Union")
print(JoinClausewithUnion)
RemoveDupsfromUnion <- unique(JoinClausewithUnion)
print("RemoveDupsfromUnion")
print(RemoveDupsfromUnion)

#FullJoin <- rbind(JoinClause1, JoinClause2)
#print("FullJoin without function call")
#print(FullJoin)

CallFullJoin <- OuterJoin("Table3","Table6","id","id")
print("FullJoin with a function call")
print(CallFullJoin)


CallFullJoin2 <- OuterJoin("Table3","Table4","id","id")
print("FullJoin with a function call #2")
print(CallFullJoin2)

CartersionProduct <- sqldf('select Table1.id, Table5.Alpha from Table1,Table5')
print("CartersionProduct")
print(CartersionProduct)

ThetaJoin <- sqldf('select * from Table3 join Table6 where Table3.id = Table6.id' )
print("ThetaJoin")
print(ThetaJoin)

Projection <- sqldf('select Alpha from Table5')
print("Projection")
print(Projection)

CrossUnion <- sqldf('select * from Table1, Table5 where Table1.Alpha = Table5.Alpha
                    UNION select * from Table4')
print("CrossUnion")
print(CrossUnion)

Crossintersection <- sqldf('select * from Table1, Table5 where Table1.Alpha = Table5.Alpha
                           INTERSECT select * from Table4')
print("Crossintersection")
print(Crossintersection)

#Result <- identical(FullJoin,CallFullJoin)
cat("-----------------------------------------------------------------\n")
#cat("Are data frames identical with and without function call ? ", Result)

