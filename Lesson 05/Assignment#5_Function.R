OuterJoin <- function(tableA,tableB, KeyA, KeyB)
{
  #browser()
  LeftOuterJoinSQL1 <- sprintf("select * from %s left outer join %s ON %s.%s=%s.%s", "tableA", "tableB", "tableA","Alpha", "tableB","Alpha")
  LeftOuterJoin1 <- sqldf(LeftOuterJoinSQL1)
  LeftOuterJoinSQL2 <- sprintf("select * from %s left outer join %s ON %s.%s=%s.%s", "tableB", "tableA", "tableA","Alpha", "tableB","Alpha")
  LeftOuterJoin2 <- sqldf(LeftOuterJoinSQL2)
  FullJoin <- rbind(LeftOuterJoin1, LeftOuterJoin2)
  return(FullJoin)
}