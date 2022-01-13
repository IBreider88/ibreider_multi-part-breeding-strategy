#Breider et al. 2022 DOI: 10.21203/rs.3.rs-1246254/v1
#
library(dplyr)
library(MASS)
Results<-read.table("SummaryofResultsScenarios.txt")
SortedResults<-arrange(Results,desc(Results$V9))

SortedResults<-as.matrix(SortedResults)

write.table(SortedResults[1:10,],file = "Top10Scenarios.txt",sep=" ",quote=FALSE,row.names=FALSE,col.names=FALSE)

