# Clear objects from Memory
rm(list=ls())
# Clear Console:
cat("\014")
FileName <- 'EnergyEfficiencyData._Copycsv.csv'
ILPD <- read.csv(FileName, header=TRUE, stringsAsFactors=FALSE)
head(ILPD)
heating.load <- ILPD$Heating.Load
NumCols <- ncol(ILPD)
NumRows <- nrow(ILPD)
#loop to group a row
for (i in 1:NumRows) {
  if (ILPD$Relative.Compactness[i] == '0.71') {
    ILPD[i,NumCols+1] <- ILPD$Heating.Load[i]
    t <- ILPD$Heating.Load[i]
    u <- ILPD$Overall.Height[i]
    next
  }
}
