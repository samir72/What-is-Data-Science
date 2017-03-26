OuterJoin <- function(tableA,tableB, KeyA, KeyB)
{
#  browser()
  LeftOuterJoinSQL1 <- sprintf("select %s.*, %s.* from %s left outer join %s ON %s.%s=%s.%s", tableA, tableB,tableA, tableB, tableA,KeyA,tableB,KeyB)
  LeftOuterJoin1 <- sqldf(LeftOuterJoinSQL1)
  LeftOuterJoinSQL2 <- sprintf("select %s.*, %s.* from %s left outer join %s ON %s.%s=%s.%s", tableA, tableB,tableB, tableA, tableA,KeyA,tableB,KeyB)
  LeftOuterJoin2 <- sqldf(LeftOuterJoinSQL2)
  FullJoin <- rbind(LeftOuterJoin1, LeftOuterJoin2)
  #FullJoin=rbindlist(list(LeftOuterJoin1,LeftOuterJoin2), use.names=TRUE, fill=FALSE)
  RemoveDups <- unique(FullJoin)
  return(RemoveDups)
}