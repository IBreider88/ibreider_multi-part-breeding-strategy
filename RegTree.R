#Breider et al. 2022 DOI: 10.21203/rs.3.rs-1246254/v1
#
## Load required packages
library(package="party")        # for ctree() 

setwd("~/Tree")
Results<-read.table("MatrixOfCovariatesY100EYT1.txt",header = FALSE)
ColNames<-c("Scenario","GenGain","Acc","nBridges","nCyclesBr","IR","BR","RR","ER")
colnames(Results)<-ColNames
BaseLines<-read.table("MatrixOfCovariatesY100EYT1Base.txt",header=FALSE)
BaseLines <- BaseLines[c(1,3:4,6)]
ColNames<-c("Scenario","GenGainBase","Acc","nCyclesBr")
colnames(BaseLines)<-ColNames

Results <- merge(BaseLines,Results,by=c("Acc","nCyclesBr"))  ##Baselines needs to include nCyclesBr=1, as baseline for multipart nCyclesBr=2.

Results[12]<-Results$GenGain-Results$GenGainBase
names(Results)[12]<-"GenGainDiff"

for(i in 1:33885){
  if(Results$nBridges[i]==1){
    Results$BR[i]<-0}
}

Results<-Results[c(1:2,6:12)]

Results$Acc<-factor(Results$Acc)
Results$nBridges<-factor(Results$nBridges)
Results$nCyclesBr<-factor(Results$nCyclesBr)
Results$ER<-factor(Results$ER)


linearMod<-lm(GenGainDiff ~ Acc+ ##all pars, no ints => all sig, incl Intercept
                nBridges+ #not sig
                nCyclesBr+
                IR+
                BR+
                RR+
                ER+
                #nBridges*nCyclesBr+ #not sig
                nBridges*Acc+ #SIG
                nCyclesBr*Acc+ #SIG
                IR*Acc+ #SIG 
                BR*Acc+ #SIG
                RR*Acc+ #SIG 
                ER*Acc+ #SIG
                BR*IR+ #SIG
                BR*RR+ #SIG 
                #BR*ER+ #not sig
                RR*IR+ #SIG
                RR*ER+ #SIG
                nBridges*RR+ #SIG
                nBridges*IR+ #SIG
                nBridges*BR+ #SIG
                nBridges*ER+ #SIG
                nCyclesBr*IR+ #SIG 
                nCyclesBr*BR+ #SIG
                nCyclesBr*RR+ #SIG
                nCyclesBr*ER+ #Not sure, keep
                ER*ER
              ,data=Results)
summary(linearMod)
par(mfrow=c(2,2))
plot(linearMod)

FigPar=c(60, 20, 24)
FileOutput <- paste("RegTreeBonFerroniSimple")
FileTxtOutput <- paste0(FileOutput, ".txt")
FileFigOutput <- paste0(FileOutput, ".pdf")

FitCTree <- ctree(formula=linearMod, data=Results,
                  control=ctree_control(testtype="Bonferroni", mincriterion=0.95,maxdepth = 4))
                  #control=ctree_control(testtype="Bonferroni", mincriterion=0.99,maxdepth = 3))
#cat(paste0("\n* Regression tree for ", MetricInternal, ":\n"))
#print(FitCTree)
plot(FitCTree,type='simple')
sink()

## Plot the regression tree
pdf(file=FileFigOutput, width=FigPar[1], height=FigPar[2], pointsize=FigPar[3])
#plot(FitCTree, type="simple", drop_terminal=TRUE)
plot(FitCTree, type="simple",drop_terminal=TRUE)
dev.off()



