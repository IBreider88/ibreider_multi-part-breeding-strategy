#Breider et al. 2022 DOI: 10.21203/rs.3.rs-1246254/v1
#
library(AlphaSimR) 
args <-commandArgs(trailingOnly = TRUE)

n=as.numeric(args[1])  #5 or 50
JOBREP=as.numeric(args[2]) 

load(paste0("MultiPart_Acc1nBridges2nCyclesBr6IR0.5BR0.45RR0.45ER1_",JOBREP,".rda"))

a = SP$traits[[1]]@addEff

listBinLength<-seq(1, 10000, n) #seq(1, 10000, 5 or 50)
listExoticY1<-list()
listExoticY2<-list()
listExoticY3<-list()
listExoticY4<-list()
listExoticY5<-list()
listExoticY6<-list()
listExoticY7<-list()
listExoticY8<-list()
listExoticY9<-list()
listExoticY10<-list()
listExoticY20<-list()
listExoticY30<-list()
listExoticY40<-list()
listExoticY50<-list()
listExoticY60<-list()
listExoticY70<-list()
listExoticY80<-list()
listExoticY90<-list()
listExoticY100<-list()

listEliteStartOfPIY1<-list()
listEliteStartOfPIY2<-list()
listEliteStartOfPIY3<-list()
listEliteStartOfPIY4<-list()
listEliteStartOfPIY5<-list()
listEliteStartOfPIY6<-list()
listEliteStartOfPIY7<-list()
listEliteStartOfPIY8<-list()
listEliteStartOfPIY9<-list()
listEliteStartOfPIY10<-list()
listEliteStartOfPIY20<-list()
listEliteStartOfPIY30<-list()
listEliteStartOfPIY40<-list()
listEliteStartOfPIY50<-list()
listEliteStartOfPIY60<-list()
listEliteStartOfPIY70<-list()
listEliteStartOfPIY80<-list()
listEliteStartOfPIY90<-list()
listEliteStartOfPIY100<-list()

for(i in listBinLength){
  ##All exotics
  seg1Exotic<-pullQtlHaplo(F1Br1_SnapShot_Year_2)[,i:(i+n-1)]
  seg1ExoticY1St<-seg1Exotic%*%(a[i:(i+n-1)])
  
  seg1Exotic<-pullQtlHaplo(VarPop_SnapShot_Year_2)[,i:(i+n-1)]
  seg1ExoticY2St<-seg1Exotic%*%(a[i:(i+n-1)])
  
  seg1Exotic<-pullQtlHaplo(VarPop_SnapShot_Year_3)[,i:(i+n-1)]
  seg1ExoticY3St<-seg1Exotic%*%(a[i:(i+n-1)])
  
  seg1Exotic<-pullQtlHaplo(VarPop_SnapShot_Year_4)[,i:(i+n-1)]
  seg1ExoticY4St<- seg1Exotic%*%(a[i:(i+n-1)])
  
  seg1Exotic<-pullQtlHaplo(VarPop_SnapShot_Year_5)[,i:(i+n-1)]
  seg1ExoticY5St<- seg1Exotic%*%(a[i:(i+n-1)])
  
  seg1Exotic<-pullQtlHaplo(VarPop_SnapShot_Year_6)[,i:(i+n-1)]
  seg1ExoticY6St<-seg1Exotic%*%(a[i:(i+n-1)])
  
  seg1Exotic<-pullQtlHaplo(VarPop_SnapShot_Year_7)[,i:(i+n-1)]
  seg1ExoticY7St<- seg1Exotic%*%(a[i:(i+n-1)])
  
  seg1Exotic<-pullQtlHaplo(VarPop_SnapShot_Year_8)[,i:(i+n-1)]
  seg1ExoticY8St<- seg1Exotic%*%(a[i:(i+n-1)])
  
  seg1Exotic<-pullQtlHaplo(VarPop_SnapShot_Year_9)[,i:(i+n-1)]
  seg1ExoticY9St<-seg1Exotic%*%(a[i:(i+n-1)])
  
  seg1Exotic<-pullQtlHaplo(VarPop_SnapShot_Year_10)[,i:(i+n-1)]
  seg1ExoticY10St<-seg1Exotic%*%(a[i:(i+n-1)])
  
  seg1Exotic<-pullQtlHaplo(VarPop_SnapShot_Year_20)[,i:(i+n-1)]
  seg1ExoticY20St<-seg1Exotic%*%(a[i:(i+n-1)])
  
  seg1Exotic<-pullQtlHaplo(VarPop_SnapShot_Year_30)[,i:(i+n-1)]
  seg1ExoticY30St<-seg1Exotic%*%(a[i:(i+n-1)])
  
  seg1Exotic<-pullQtlHaplo(VarPop_SnapShot_Year_40)[,i:(i+n-1)]
  seg1ExoticY40St<-seg1Exotic%*%(a[i:(i+n-1)])
 
  seg1Exotic<-pullQtlHaplo(VarPop_SnapShot_Year_50)[,i:(i+n-1)]
  seg1ExoticY50St<- seg1Exotic%*%(a[i:(i+n-1)])
  
  seg1Exotic<-pullQtlHaplo(VarPop_SnapShot_Year_60)[,i:(i+n-1)]
  seg1ExoticY60St<-seg1Exotic%*%(a[i:(i+n-1)])
  
  seg1Exotic<-pullQtlHaplo(VarPop_SnapShot_Year_70)[,i:(i+n-1)]
  seg1ExoticY70St<-seg1Exotic%*%(a[i:(i+n-1)])
  
  seg1Exotic<-pullQtlHaplo(VarPop_SnapShot_Year_80)[,i:(i+n-1)]
  seg1ExoticY80St<-seg1Exotic%*%(a[i:(i+n-1)])
  
  seg1Exotic<-pullQtlHaplo(VarPop_SnapShot_Year_90)[,i:(i+n-1)]
  seg1ExoticY90St<-seg1Exotic%*%(a[i:(i+n-1)])
  
  seg1Exotic<-pullQtlHaplo(VarPop_SnapShot_Year_100)[,i:(i+n-1)]
  seg1ExoticY100St<- seg1Exotic%*%(a[i:(i+n-1)])
  
  ##All EliteStartOfPI
  seg1Elite<-pullQtlHaplo(StartofPI_SnapShot_Year_1)[,i:(i+n-1)]
  seg1EliteStartOfPIY1St<-seg1Elite%*%(a[i:(i+n-1)])
  
  seg1Elite<-pullQtlHaplo(StartofPI_SnapShot_Year_2)[,i:(i+n-1)]
  seg1EliteStartOfPIY2St<-seg1Elite%*%(a[i:(i+n-1)])
  
  seg1Elite<-pullQtlHaplo(StartofPI_SnapShot_Year_3)[,i:(i+n-1)]
  seg1EliteStartOfPIY3St<-seg1Elite%*%(a[i:(i+n-1)])
  
  seg1Elite<-pullQtlHaplo(StartofPI_SnapShot_Year_4)[,i:(i+n-1)]
  seg1EliteStartOfPIY4St<-seg1Elite%*%(a[i:(i+n-1)])
  
  seg1Elite<-pullQtlHaplo(StartofPI_SnapShot_Year_5)[,i:(i+n-1)]
  seg1EliteStartOfPIY5St<-seg1Elite%*%(a[i:(i+n-1)])
  
  seg1Elite<-pullQtlHaplo(StartofPI_SnapShot_Year_6)[,i:(i+n-1)]
  seg1EliteStartOfPIY6St<-seg1Elite%*%(a[i:(i+n-1)])
  
  seg1Elite<-pullQtlHaplo(StartofPI_SnapShot_Year_7)[,i:(i+n-1)]
  seg1EliteStartOfPIY7St<-seg1Elite%*%(a[i:(i+n-1)])
  
  seg1Elite<-pullQtlHaplo(StartofPI_SnapShot_Year_8)[,i:(i+n-1)]
  seg1EliteStartOfPIY8St<-seg1Elite%*%(a[i:(i+n-1)])
  
  seg1Elite<-pullQtlHaplo(StartofPI_SnapShot_Year_9)[,i:(i+n-1)]
  seg1EliteStartOfPIY9St<-seg1Elite%*%(a[i:(i+n-1)])
  
  seg1Elite<-pullQtlHaplo(StartofPI_SnapShot_Year_10)[,i:(i+n-1)]
  seg1EliteStartOfPIY10St<-seg1Elite%*%(a[i:(i+n-1)])
  
  seg1Elite<-pullQtlHaplo(StartofPI_SnapShot_Year_20)[,i:(i+n-1)]
  seg1EliteStartOfPIY20St<-seg1Elite%*%(a[i:(i+n-1)])
  
  seg1Elite<-pullQtlHaplo(StartofPI_SnapShot_Year_30)[,i:(i+n-1)]
  seg1EliteStartOfPIY30St<-seg1Elite%*%(a[i:(i+n-1)])
  
  seg1Elite<-pullQtlHaplo(StartofPI_SnapShot_Year_40)[,i:(i+n-1)]
  seg1EliteStartOfPIY40St<-seg1Elite%*%(a[i:(i+n-1)])
  
  seg1Elite<-pullQtlHaplo(StartofPI_SnapShot_Year_50)[,i:(i+n-1)]
  seg1EliteStartOfPIY50St<-seg1Elite%*%(a[i:(i+n-1)])
  
  seg1Elite<-pullQtlHaplo(StartofPI_SnapShot_Year_60)[,i:(i+n-1)]
  seg1EliteStartOfPIY60St<-seg1Elite%*%(a[i:(i+n-1)])
  
  seg1Elite<-pullQtlHaplo(StartofPI_SnapShot_Year_70)[,i:(i+n-1)]
  seg1EliteStartOfPIY70St<-seg1Elite%*%(a[i:(i+n-1)])
  
  seg1Elite<-pullQtlHaplo(StartofPI_SnapShot_Year_80)[,i:(i+n-1)]
  seg1EliteStartOfPIY80St<-seg1Elite%*%(a[i:(i+n-1)])
  
  seg1Elite<-pullQtlHaplo(StartofPI_SnapShot_Year_90)[,i:(i+n-1)]
  seg1EliteStartOfPIY90St<-seg1Elite%*%(a[i:(i+n-1)])
  
  seg1Elite<-pullQtlHaplo(StartofPI_SnapShot_Year_100)[,i:(i+n-1)]
  seg1EliteStartOfPIY100St<-seg1Elite%*%(a[i:(i+n-1)])
 
  listExoticY1<-c(listExoticY1,mean(seg1ExoticY1St))
  listExoticY2<-c(listExoticY2,mean(seg1ExoticY2St))
  listExoticY3<-c(listExoticY3,mean(seg1ExoticY3St))
  listExoticY4<-c(listExoticY4,mean(seg1ExoticY4St))
  listExoticY5<-c(listExoticY5,mean(seg1ExoticY5St))
  listExoticY6<-c(listExoticY6,mean(seg1ExoticY6St))
  listExoticY7<-c(listExoticY7,mean(seg1ExoticY7St))
  listExoticY8<-c(listExoticY8,mean(seg1ExoticY8St))
  listExoticY9<-c(listExoticY9,mean(seg1ExoticY9St))
  listExoticY10<-c(listExoticY10,mean(seg1ExoticY10St))
  listExoticY20<-c(listExoticY20,mean(seg1ExoticY20St))
  listExoticY30<-c(listExoticY30,mean(seg1ExoticY30St))
  listExoticY40<-c(listExoticY40,mean(seg1ExoticY40St))
  listExoticY50<-c(listExoticY50,mean(seg1ExoticY50St))
  listExoticY60<-c(listExoticY60,mean(seg1ExoticY60St))
  listExoticY70<-c(listExoticY70,mean(seg1ExoticY70St))
  listExoticY80<-c(listExoticY80,mean(seg1ExoticY80St))
  listExoticY90<-c(listExoticY90,mean(seg1ExoticY90St))
  listExoticY100<-c(listExoticY100,mean(seg1ExoticY100St))
  
  listEliteStartOfPIY1<-c(listEliteStartOfPIY1,mean(seg1EliteStartOfPIY1St))
  listEliteStartOfPIY2<-c(listEliteStartOfPIY2,mean(seg1EliteStartOfPIY2St))
  listEliteStartOfPIY3<-c(listEliteStartOfPIY3,mean(seg1EliteStartOfPIY3St))
  listEliteStartOfPIY4<-c(listEliteStartOfPIY4,mean(seg1EliteStartOfPIY4St))
  listEliteStartOfPIY5<-c(listEliteStartOfPIY5,mean(seg1EliteStartOfPIY5St))
  listEliteStartOfPIY6<-c(listEliteStartOfPIY6,mean(seg1EliteStartOfPIY6St))
  listEliteStartOfPIY7<-c(listEliteStartOfPIY7,mean(seg1EliteStartOfPIY7St))
  listEliteStartOfPIY8<-c(listEliteStartOfPIY8,mean(seg1EliteStartOfPIY8St))
  listEliteStartOfPIY9<-c(listEliteStartOfPIY9,mean(seg1EliteStartOfPIY9St))
  listEliteStartOfPIY10<-c(listEliteStartOfPIY10,mean(seg1EliteStartOfPIY10St))
  listEliteStartOfPIY20<-c(listEliteStartOfPIY20,mean(seg1EliteStartOfPIY20St))
  listEliteStartOfPIY30<-c(listEliteStartOfPIY30,mean(seg1EliteStartOfPIY30St))
  listEliteStartOfPIY40<-c(listEliteStartOfPIY40,mean(seg1EliteStartOfPIY40St))
  listEliteStartOfPIY50<-c(listEliteStartOfPIY50,mean(seg1EliteStartOfPIY50St))
  listEliteStartOfPIY60<-c(listEliteStartOfPIY60,mean(seg1EliteStartOfPIY60St))
  listEliteStartOfPIY70<-c(listEliteStartOfPIY70,mean(seg1EliteStartOfPIY70St))
  listEliteStartOfPIY80<-c(listEliteStartOfPIY80,mean(seg1EliteStartOfPIY80St))
  listEliteStartOfPIY90<-c(listEliteStartOfPIY90,mean(seg1EliteStartOfPIY90St))
  listEliteStartOfPIY100<-c(listEliteStartOfPIY100,mean(seg1EliteStartOfPIY100St))
}

save.image(paste0("HaploBlockAnalysesInterim",n,"QTL_",JOBREP,".rda"))

####
OutPutExoticVsEliteStartOfPIY1<-mapply(FUN = function(RD, CD) {
  RD>CD
}, RD = listExoticY1, CD = listEliteStartOfPIY1)

OutPutExoticVsEliteStartOfPIY1Diff<-mapply(FUN = function(RD, CD) {
  if(RD>CD){RD-CD}else{0}
}, RD = listExoticY1, CD = listEliteStartOfPIY1)
####
OutPutExoticVsEliteStartOfPIY2<-mapply(FUN = function(RD, CD) {
  RD>CD
}, RD = listExoticY2, CD = listEliteStartOfPIY2)

OutPutExoticVsEliteStartOfPIY2Diff<-mapply(FUN = function(RD, CD) {
  if(RD>CD){RD-CD}else{0}
}, RD = listExoticY2, CD = listEliteStartOfPIY2)

####
OutPutExoticVsEliteStartOfPIY3<-mapply(FUN = function(RD, CD) {
  RD>CD
}, RD = listExoticY3, CD = listEliteStartOfPIY3)

OutPutExoticVsEliteStartOfPIY3Diff<-mapply(FUN = function(RD, CD) {
  if(RD>CD){RD-CD}else{0}
}, RD = listExoticY3, CD = listEliteStartOfPIY3)
####
OutPutExoticVsEliteStartOfPIY4<-mapply(FUN = function(RD, CD) {
  RD>CD
}, RD = listExoticY4, CD = listEliteStartOfPIY4)

OutPutExoticVsEliteStartOfPIY4Diff<-mapply(FUN = function(RD, CD) {
  if(RD>CD){RD-CD}else{0}
}, RD = listExoticY4, CD = listEliteStartOfPIY4)
####
OutPutExoticVsEliteStartOfPIY5<-mapply(FUN = function(RD, CD) {
  RD>CD
}, RD = listExoticY5, CD = listEliteStartOfPIY5)

OutPutExoticVsEliteStartOfPIY5Diff<-mapply(FUN = function(RD, CD) {
  if(RD>CD){RD-CD}else{0}
}, RD = listExoticY5, CD = listEliteStartOfPIY5)
####
OutPutExoticVsEliteStartOfPIY6<-mapply(FUN = function(RD, CD) {
  RD>CD
}, RD = listExoticY6, CD = listEliteStartOfPIY6)

OutPutExoticVsEliteStartOfPIY6Diff<-mapply(FUN = function(RD, CD) {
  if(RD>CD){RD-CD}else{0}
}, RD = listExoticY6, CD = listEliteStartOfPIY6)
####
OutPutExoticVsEliteStartOfPIY7<-mapply(FUN = function(RD, CD) {
  RD>CD
}, RD = listExoticY7, CD = listEliteStartOfPIY7)

OutPutExoticVsEliteStartOfPIY7Diff<-mapply(FUN = function(RD, CD) {
  if(RD>CD){RD-CD}else{0}
}, RD = listExoticY7, CD = listEliteStartOfPIY7)
####
OutPutExoticVsEliteStartOfPIY8<-mapply(FUN = function(RD, CD) {
  RD>CD
}, RD = listExoticY8, CD = listEliteStartOfPIY8)

OutPutExoticVsEliteStartOfPIY8Diff<-mapply(FUN = function(RD, CD) {
  if(RD>CD){RD-CD}else{0}
}, RD = listExoticY8, CD = listEliteStartOfPIY8)
####
OutPutExoticVsEliteStartOfPIY9<-mapply(FUN = function(RD, CD) {
  RD>CD
}, RD = listExoticY9, CD = listEliteStartOfPIY9)

OutPutExoticVsEliteStartOfPIY9Diff<-mapply(FUN = function(RD, CD) {
  if(RD>CD){RD-CD}else{0}
}, RD = listExoticY9, CD = listEliteStartOfPIY9)
####
OutPutExoticVsEliteStartOfPIY10<-mapply(FUN = function(RD, CD) {
  RD>CD
}, RD = listExoticY10, CD = listEliteStartOfPIY10)

OutPutExoticVsEliteStartOfPIY10Diff<-mapply(FUN = function(RD, CD) {
  if(RD>CD){RD-CD}else{0}
}, RD = listExoticY10, CD = listEliteStartOfPIY10)
####
OutPutExoticVsEliteStartOfPIY20<-mapply(FUN = function(RD, CD) {
  RD>CD
}, RD = listExoticY20, CD = listEliteStartOfPIY20)

OutPutExoticVsEliteStartOfPIY20Diff<-mapply(FUN = function(RD, CD) {
  if(RD>CD){RD-CD}else{0}
}, RD = listExoticY20, CD = listEliteStartOfPIY20)
####
OutPutExoticVsEliteStartOfPIY30<-mapply(FUN = function(RD, CD) {
  RD>CD
}, RD = listExoticY30, CD = listEliteStartOfPIY30)

OutPutExoticVsEliteStartOfPIY30Diff<-mapply(FUN = function(RD, CD) {
  if(RD>CD){RD-CD}else{0}
}, RD = listExoticY30, CD = listEliteStartOfPIY30)
####
OutPutExoticVsEliteStartOfPIY40<-mapply(FUN = function(RD, CD) {
  RD>CD
}, RD = listExoticY40, CD = listEliteStartOfPIY40)

OutPutExoticVsEliteStartOfPIY40Diff<-mapply(FUN = function(RD, CD) {
  if(RD>CD){RD-CD}else{0}
}, RD = listExoticY40, CD = listEliteStartOfPIY40)
####
OutPutExoticVsEliteStartOfPIY50<-mapply(FUN = function(RD, CD) {
  RD>CD
}, RD = listExoticY50, CD = listEliteStartOfPIY50)

OutPutExoticVsEliteStartOfPIY50Diff<-mapply(FUN = function(RD, CD) {
  if(RD>CD){RD-CD}else{0}
}, RD = listExoticY50, CD = listEliteStartOfPIY50)
####
OutPutExoticVsEliteStartOfPIY60<-mapply(FUN = function(RD, CD) {
  RD>CD
}, RD = listExoticY60, CD = listEliteStartOfPIY60)

OutPutExoticVsEliteStartOfPIY60Diff<-mapply(FUN = function(RD, CD) {
  if(RD>CD){RD-CD}else{0}
}, RD = listExoticY60, CD = listEliteStartOfPIY60)
####
OutPutExoticVsEliteStartOfPIY70<-mapply(FUN = function(RD, CD) {
  RD>CD
}, RD = listExoticY70, CD = listEliteStartOfPIY70)

OutPutExoticVsEliteStartOfPIY70Diff<-mapply(FUN = function(RD, CD) {
  if(RD>CD){RD-CD}else{0}
}, RD = listExoticY70, CD = listEliteStartOfPIY70)
####
OutPutExoticVsEliteStartOfPIY80<-mapply(FUN = function(RD, CD) {
  RD>CD
}, RD = listExoticY80, CD = listEliteStartOfPIY80)

OutPutExoticVsEliteStartOfPIY80Diff<-mapply(FUN = function(RD, CD) {
  if(RD>CD){RD-CD}else{0}
}, RD = listExoticY80, CD = listEliteStartOfPIY80)
####
OutPutExoticVsEliteStartOfPIY90<-mapply(FUN = function(RD, CD) {
  RD>CD
}, RD = listExoticY90, CD = listEliteStartOfPIY90)

OutPutExoticVsEliteStartOfPIY90Diff<-mapply(FUN = function(RD, CD) {
  if(RD>CD){RD-CD}else{0}
}, RD = listExoticY90, CD = listEliteStartOfPIY90)
####
OutPutExoticVsEliteStartOfPIY100<-mapply(FUN = function(RD, CD) {
  RD>CD
}, RD = listExoticY100, CD = listEliteStartOfPIY100)

OutPutExoticVsEliteStartOfPIY100Diff<-mapply(FUN = function(RD, CD) {
  if(RD>CD){RD-CD}else{0}
}, RD = listExoticY100, CD = listEliteStartOfPIY100)
####

if(dim(table(OutPutExoticVsEliteStartOfPIY1))==2){Y1<-table(OutPutExoticVsEliteStartOfPIY1)
}else if(OutPutExoticVsEliteStartOfPIY1[1]==FALSE){
  Temp<-table(OutPutExoticVsEliteStartOfPIY1)
  col2<-0
  col2<-as.table(col2)
  rownames(col2)<-("TRUE")
  Y1<-c(Temp,col2)}else{
    Temp<-table(OutPutExoticVsEliteStartOfPIY1)
    col1<-0
    col1<-as.table(col1)
    rownames(col1)<-("FALSE")
    Y1<-c(col1,Temp)
  }

if(dim(table(OutPutExoticVsEliteStartOfPIY2))==2){Y2<-table(OutPutExoticVsEliteStartOfPIY2)
}else if(OutPutExoticVsEliteStartOfPIY2[1]==FALSE){
  Temp<-table(OutPutExoticVsEliteStartOfPIY2)
  col2<-0
  col2<-as.table(col2)
  rownames(col2)<-("TRUE")
  Y2<-c(Temp,col2)}else{
    Temp<-table(OutPutExoticVsEliteStartOfPIY2)
    col1<-0
    col1<-as.table(col1)
    rownames(col1)<-("FALSE")
    Y2<-c(col1,Temp)
  }
if(dim(table(OutPutExoticVsEliteStartOfPIY3))==2){Y3<-table(OutPutExoticVsEliteStartOfPIY3)
}else if(OutPutExoticVsEliteStartOfPIY3[1]==FALSE){
  Temp<-table(OutPutExoticVsEliteStartOfPIY3)
  col2<-0
  col2<-as.table(col2)
  rownames(col2)<-("TRUE")
  Y3<-c(Temp,col2)}else{
    Temp<-table(OutPutExoticVsEliteStartOfPIY3)
    col1<-0
    col1<-as.table(col1)
    rownames(col1)<-("FALSE")
    Y3<-c(col1,Temp)
  }
if(dim(table(OutPutExoticVsEliteStartOfPIY4))==2){Y4<-table(OutPutExoticVsEliteStartOfPIY4)
}else if(OutPutExoticVsEliteStartOfPIY4[1]==FALSE){
  Temp<-table(OutPutExoticVsEliteStartOfPIY4)
  col2<-0
  col2<-as.table(col2)
  rownames(col2)<-("TRUE")
  Y4<-c(Temp,col2)}else{
    Temp<-table(OutPutExoticVsEliteStartOfPIY4)
    col1<-0
    col1<-as.table(col1)
    rownames(col1)<-("FALSE")
    Y4<-c(col1,Temp)
  }
if(dim(table(OutPutExoticVsEliteStartOfPIY5))==2){Y5<-table(OutPutExoticVsEliteStartOfPIY5)
}else if(OutPutExoticVsEliteStartOfPIY5[1]==FALSE){
  Temp<-table(OutPutExoticVsEliteStartOfPIY5)
  col2<-0
  col2<-as.table(col2)
  rownames(col2)<-("TRUE")
  Y5<-c(Temp,col2)}else{
    Temp<-table(OutPutExoticVsEliteStartOfPIY5)
    col1<-0
    col1<-as.table(col1)
    rownames(col1)<-("FALSE")
    Y5<-c(col1,Temp)
  }
if(dim(table(OutPutExoticVsEliteStartOfPIY6))==2){Y6<-table(OutPutExoticVsEliteStartOfPIY6)
}else if(OutPutExoticVsEliteStartOfPIY6[1]==FALSE){
  Temp<-table(OutPutExoticVsEliteStartOfPIY6)
  col2<-0
  col2<-as.table(col2)
  rownames(col2)<-("TRUE")
  Y6<-c(Temp,col2)}else{
    Temp<-table(OutPutExoticVsEliteStartOfPIY6)
    col1<-0
    col1<-as.table(col1)
    rownames(col1)<-("FALSE")
    Y6<-c(col1,Temp)
  }
if(dim(table(OutPutExoticVsEliteStartOfPIY7))==2){Y7<-table(OutPutExoticVsEliteStartOfPIY7)
}else if(OutPutExoticVsEliteStartOfPIY7[1]==FALSE){
  Temp<-table(OutPutExoticVsEliteStartOfPIY7)
  col2<-0
  col2<-as.table(col2)
  rownames(col2)<-("TRUE")
  Y7<-c(Temp,col2)}else{
    Temp<-table(OutPutExoticVsEliteStartOfPIY7)
    col1<-0
    col1<-as.table(col1)
    rownames(col1)<-("FALSE")
    Y7<-c(col1,Temp)
  }
if(dim(table(OutPutExoticVsEliteStartOfPIY8))==2){Y8<-table(OutPutExoticVsEliteStartOfPIY8)
}else if(OutPutExoticVsEliteStartOfPIY8[1]==FALSE){
  Temp<-table(OutPutExoticVsEliteStartOfPIY8)
  col2<-0
  col2<-as.table(col2)
  rownames(col2)<-("TRUE")
  Y8<-c(Temp,col2)}else{
    Temp<-table(OutPutExoticVsEliteStartOfPIY8)
    col1<-0
    col1<-as.table(col1)
    rownames(col1)<-("FALSE")
    Y8<-c(col1,Temp)
  }
if(dim(table(OutPutExoticVsEliteStartOfPIY9))==2){Y9<-table(OutPutExoticVsEliteStartOfPIY9)
}else if(OutPutExoticVsEliteStartOfPIY9[1]==FALSE){
  Temp<-table(OutPutExoticVsEliteStartOfPIY9)
  col2<-0
  col2<-as.table(col2)
  rownames(col2)<-("TRUE")
  Y9<-c(Temp,col2)}else{
    Temp<-table(OutPutExoticVsEliteStartOfPIY9)
    col1<-0
    col1<-as.table(col1)
    rownames(col1)<-("FALSE")
    Y9<-c(col1,Temp)
  }
if(dim(table(OutPutExoticVsEliteStartOfPIY10))==2){Y10<-table(OutPutExoticVsEliteStartOfPIY10)
}else if(OutPutExoticVsEliteStartOfPIY10[1]==FALSE){
  Temp<-table(OutPutExoticVsEliteStartOfPIY10)
  col2<-0
  col2<-as.table(col2)
  rownames(col2)<-("TRUE")
  Y10<-c(Temp,col2)}else{
    Temp<-table(OutPutExoticVsEliteStartOfPIY10)
    col1<-0
    col1<-as.table(col1)
    rownames(col1)<-("FALSE")
    Y10<-c(col1,Temp)
  }
if(dim(table(OutPutExoticVsEliteStartOfPIY20))==2){Y20<-table(OutPutExoticVsEliteStartOfPIY20)
}else if(OutPutExoticVsEliteStartOfPIY20[1]==FALSE){
  Temp<-table(OutPutExoticVsEliteStartOfPIY20)
  col2<-0
  col2<-as.table(col2)
  rownames(col2)<-("TRUE")
  Y20<-c(Temp,col2)}else{
    Temp<-table(OutPutExoticVsEliteStartOfPIY20)
    col1<-0
    col1<-as.table(col1)
    rownames(col1)<-("FALSE")
    Y20<-c(col1,Temp)
  }
if(dim(table(OutPutExoticVsEliteStartOfPIY30))==2){Y30<-table(OutPutExoticVsEliteStartOfPIY30)
}else if(OutPutExoticVsEliteStartOfPIY30[1]==FALSE){
  Temp<-table(OutPutExoticVsEliteStartOfPIY30)
  col2<-0
  col2<-as.table(col2)
  rownames(col2)<-("TRUE")
  Y30<-c(Temp,col2)}else{
    Temp<-table(OutPutExoticVsEliteStartOfPIY30)
    col1<-0
    col1<-as.table(col1)
    rownames(col1)<-("FALSE")
    Y30<-c(col1,Temp)
  }
if(dim(table(OutPutExoticVsEliteStartOfPIY40))==2){Y40<-table(OutPutExoticVsEliteStartOfPIY40)
}else if(OutPutExoticVsEliteStartOfPIY40[1]==FALSE){
  Temp<-table(OutPutExoticVsEliteStartOfPIY40)
  col2<-0
  col2<-as.table(col2)
  rownames(col2)<-("TRUE")
  Y40<-c(Temp,col2)}else{
    Temp<-table(OutPutExoticVsEliteStartOfPIY40)
    col1<-0
    col1<-as.table(col1)
    rownames(col1)<-("FALSE")
    Y40<-c(col1,Temp)
  }
if(dim(table(OutPutExoticVsEliteStartOfPIY50))==2){Y50<-table(OutPutExoticVsEliteStartOfPIY50)
}else if(OutPutExoticVsEliteStartOfPIY50[1]==FALSE){
  Temp<-table(OutPutExoticVsEliteStartOfPIY50)
  col2<-0
  col2<-as.table(col2)
  rownames(col2)<-("TRUE")
  Y50<-c(Temp,col2)}else{
    Temp<-table(OutPutExoticVsEliteStartOfPIY50)
    col1<-0
    col1<-as.table(col1)
    rownames(col1)<-("FALSE")
    Y50<-c(col1,Temp)
  }
if(dim(table(OutPutExoticVsEliteStartOfPIY60))==2){Y60<-table(OutPutExoticVsEliteStartOfPIY60)
}else if(OutPutExoticVsEliteStartOfPIY60[1]==FALSE){
  Temp<-table(OutPutExoticVsEliteStartOfPIY60)
  col2<-0
  col2<-as.table(col2)
  rownames(col2)<-("TRUE")
  Y60<-c(Temp,col2)}else{
    Temp<-table(OutPutExoticVsEliteStartOfPIY60)
    col1<-0
    col1<-as.table(col1)
    rownames(col1)<-("FALSE")
    Y60<-c(col1,Temp)
  }
if(dim(table(OutPutExoticVsEliteStartOfPIY70))==2){Y70<-table(OutPutExoticVsEliteStartOfPIY70)
}else if(OutPutExoticVsEliteStartOfPIY70[1]==FALSE){
  Temp<-table(OutPutExoticVsEliteStartOfPIY70)
  col2<-0
  col2<-as.table(col2)
  rownames(col2)<-("TRUE")
  Y70<-c(Temp,col2)}else{
    Temp<-table(OutPutExoticVsEliteStartOfPIY70)
    col1<-0
    col1<-as.table(col1)
    rownames(col1)<-("FALSE")
    Y70<-c(col1,Temp)
  }
if(dim(table(OutPutExoticVsEliteStartOfPIY80))==2){Y80<-table(OutPutExoticVsEliteStartOfPIY80)
}else if(OutPutExoticVsEliteStartOfPIY80[1]==FALSE){
  Temp<-table(OutPutExoticVsEliteStartOfPIY80)
  col2<-0
  col2<-as.table(col2)
  rownames(col2)<-("TRUE")
  Y80<-c(Temp,col2)}else{
    Temp<-table(OutPutExoticVsEliteStartOfPIY80)
    col1<-0
    col1<-as.table(col1)
    rownames(col1)<-("FALSE")
    Y80<-c(col1,Temp)
  }
if(dim(table(OutPutExoticVsEliteStartOfPIY90))==2){Y90<-table(OutPutExoticVsEliteStartOfPIY90)
}else if(OutPutExoticVsEliteStartOfPIY90[1]==FALSE){
  Temp<-table(OutPutExoticVsEliteStartOfPIY90)
  col2<-0
  col2<-as.table(col2)
  rownames(col2)<-("TRUE")
  Y90<-c(Temp,col2)}else{
    Temp<-table(OutPutExoticVsEliteStartOfPIY90)
    col1<-0
    col1<-as.table(col1)
    rownames(col1)<-("FALSE")
    Y90<-c(col1,Temp)
  }
if(dim(table(OutPutExoticVsEliteStartOfPIY100))==2){Y100<-table(OutPutExoticVsEliteStartOfPIY100)
}else if(OutPutExoticVsEliteStartOfPIY100[1]==FALSE){
  Temp<-table(OutPutExoticVsEliteStartOfPIY100)
  col2<-0
  col2<-as.table(col2)
  rownames(col2)<-("TRUE")
  Y100<-c(Temp,col2)}else{
    Temp<-table(OutPutExoticVsEliteStartOfPIY100)
    col1<-0
    col1<-as.table(col1)
    rownames(col1)<-("FALSE")
    Y100<-c(col1,Temp)
  }

OutPutTable<-rbind(Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9,Y10,Y20,Y30,Y40,Y50,Y60,Y70,Y80,Y90,Y100)

rownames(OutPutTable) <- c("OutPutExoticVsEliteStartOfPIY1",
                           "OutPutExoticVsEliteStartOfPIY2",
                           "OutPutExoticVsEliteStartOfPIY3",
                           "OutPutExoticVsEliteStartOfPIY4",
                           "OutPutExoticVsEliteStartOfPIY5",
                           "OutPutExoticVsEliteStartOfPIY6",
                           "OutPutExoticVsEliteStartOfPIY7",
                           "OutPutExoticVsEliteStartOfPIY8",
                           "OutPutExoticVsEliteStartOfPIY9",
                           "OutPutExoticVsEliteStartOfPIY10",
                           "OutPutExoticVsEliteStartOfPIY20",
                           "OutPutExoticVsEliteStartOfPIY30",
                           "OutPutExoticVsEliteStartOfPIY40",
                           "OutPutExoticVsEliteStartOfPIY50",
                           "OutPutExoticVsEliteStartOfPIY60",
                           "OutPutExoticVsEliteStartOfPIY70",
                           "OutPutExoticVsEliteStartOfPIY80",
                           "OutPutExoticVsEliteStartOfPIY90",
                           "OutPutExoticVsEliteStartOfPIY100"
                           )

OutPutDiff<-rbind(cbind(mean(unlist(OutPutExoticVsEliteStartOfPIY1Diff)),
                        min(unlist(OutPutExoticVsEliteStartOfPIY1Diff)),
                        max(unlist(OutPutExoticVsEliteStartOfPIY1Diff))),
                  cbind(mean(unlist(OutPutExoticVsEliteStartOfPIY2Diff)),
                        min(unlist(OutPutExoticVsEliteStartOfPIY2Diff)),
                        max(unlist(OutPutExoticVsEliteStartOfPIY2Diff))),
                  cbind(mean(unlist(OutPutExoticVsEliteStartOfPIY3Diff)),
                        min(unlist(OutPutExoticVsEliteStartOfPIY3Diff)),
                        max(unlist(OutPutExoticVsEliteStartOfPIY3Diff))),
                  cbind(mean(unlist(OutPutExoticVsEliteStartOfPIY4Diff)),
                        min(unlist(OutPutExoticVsEliteStartOfPIY4Diff)),
                        max(unlist(OutPutExoticVsEliteStartOfPIY4Diff))),
                  cbind(mean(unlist(OutPutExoticVsEliteStartOfPIY5Diff)),
                        min(unlist(OutPutExoticVsEliteStartOfPIY5Diff)),
                        max(unlist(OutPutExoticVsEliteStartOfPIY5Diff))),
                  cbind(mean(unlist(OutPutExoticVsEliteStartOfPIY6Diff)),
                        min(unlist(OutPutExoticVsEliteStartOfPIY6Diff)),
                        max(unlist(OutPutExoticVsEliteStartOfPIY6Diff))),
                  cbind(mean(unlist(OutPutExoticVsEliteStartOfPIY7Diff)),
                        min(unlist(OutPutExoticVsEliteStartOfPIY7Diff)),
                        max(unlist(OutPutExoticVsEliteStartOfPIY7Diff))),
                  cbind(mean(unlist(OutPutExoticVsEliteStartOfPIY8Diff)),
                        min(unlist(OutPutExoticVsEliteStartOfPIY8Diff)),
                        max(unlist(OutPutExoticVsEliteStartOfPIY8Diff))),
                  cbind(mean(unlist(OutPutExoticVsEliteStartOfPIY9Diff)),
                        min(unlist(OutPutExoticVsEliteStartOfPIY9Diff)),
                        max(unlist(OutPutExoticVsEliteStartOfPIY9Diff))),
                  cbind(mean(unlist(OutPutExoticVsEliteStartOfPIY10Diff)),
                        min(unlist(OutPutExoticVsEliteStartOfPIY10Diff)),
                        max(unlist(OutPutExoticVsEliteStartOfPIY10Diff))),

                  cbind(mean(unlist(OutPutExoticVsEliteStartOfPIY20Diff)),
                        min(unlist(OutPutExoticVsEliteStartOfPIY20Diff)),
                        max(unlist(OutPutExoticVsEliteStartOfPIY20Diff))),
                  cbind(mean(unlist(OutPutExoticVsEliteStartOfPIY30Diff)),
                        min(unlist(OutPutExoticVsEliteStartOfPIY30Diff)),
                        max(unlist(OutPutExoticVsEliteStartOfPIY20Diff))),
                  cbind(mean(unlist(OutPutExoticVsEliteStartOfPIY40Diff)),
                        min(unlist(OutPutExoticVsEliteStartOfPIY40Diff)),
                        max(unlist(OutPutExoticVsEliteStartOfPIY40Diff))),
                  cbind(mean(unlist(OutPutExoticVsEliteStartOfPIY50Diff)),
                        min(unlist(OutPutExoticVsEliteStartOfPIY50Diff)),
                        max(unlist(OutPutExoticVsEliteStartOfPIY40Diff))),
                  cbind(mean(unlist(OutPutExoticVsEliteStartOfPIY60Diff)),
                        min(unlist(OutPutExoticVsEliteStartOfPIY60Diff)),
                        max(unlist(OutPutExoticVsEliteStartOfPIY60Diff))),
                  cbind(mean(unlist(OutPutExoticVsEliteStartOfPIY70Diff)),
                        min(unlist(OutPutExoticVsEliteStartOfPIY70Diff)),
                        max(unlist(OutPutExoticVsEliteStartOfPIY70Diff))),
                  cbind(mean(unlist(OutPutExoticVsEliteStartOfPIY80Diff)),
                        min(unlist(OutPutExoticVsEliteStartOfPIY80Diff)),
                        max(unlist(OutPutExoticVsEliteStartOfPIY80Diff))),
                  cbind(mean(unlist(OutPutExoticVsEliteStartOfPIY90Diff)),
                        min(unlist(OutPutExoticVsEliteStartOfPIY90Diff)),
                        max(unlist(OutPutExoticVsEliteStartOfPIY90Diff))),
                  cbind(mean(unlist(OutPutExoticVsEliteStartOfPIY100Diff)),
                        min(unlist(OutPutExoticVsEliteStartOfPIY100Diff)),
                        max(unlist(OutPutExoticVsEliteStartOfPIY100Diff))))

rownames(OutPutDiff) <- c("HaploBlockEffectSurplusExoticY1",
                          "HaploBlockEffectSurplusExoticY2",
                          "HaploBlockEffectSurplusExoticY3",
                          "HaploBlockEffectSurplusExoticY4",
                          "HaploBlockEffectSurplusExoticY5",
                          "HaploBlockEffectSurplusExoticY6",
                          "HaploBlockEffectSurplusExoticY7",
                          "HaploBlockEffectSurplusExoticY8",
                          "HaploBlockEffectSurplusExoticY9",
                          "HaploBlockEffectSurplusExoticY10",
                          "HaploBlockEffectSurplusExoticY20",
                          "HaploBlockEffectSurplusExoticY30",
                          "HaploBlockEffectSurplusExoticY40",
                          "HaploBlockEffectSurplusExoticY50",
                          "HaploBlockEffectSurplusExoticY60",
                          "HaploBlockEffectSurplusExoticY70",
                          "HaploBlockEffectSurplusExoticY80",
                          "HaploBlockEffectSurplusExoticY90",
                          "HaploBlockEffectSurplusExoticY100")

colnames(OutPutDiff) <- c("mean","min","max")

rm(list= ls()[!(ls() %in% c("VarPop_SnapShot_Year_1",
                            "VarPop_SnapShot_Year_2",
                            "VarPop_SnapShot_Year_3",
                            "VarPop_SnapShot_Year_4",
                            "VarPop_SnapShot_Year_5",
                            "VarPop_SnapShot_Year_6",
                            "VarPop_SnapShot_Year_7",
                            "VarPop_SnapShot_Year_8",
                            "VarPop_SnapShot_Year_9",
                            "VarPop_SnapShot_Year_10",
                            "VarPop_SnapShot_Year_20",
                            "VarPop_SnapShot_Year_30",
                            "VarPop_SnapShot_Year_40",
                            "VarPop_SnapShot_Year_50",
                            "VarPop_SnapShot_Year_60",
                            "VarPop_SnapShot_Year_70",
                            "VarPop_SnapShot_Year_80",
                            "VarPop_SnapShot_Year_90",
                            "VarPop_SnapShot_Year_100",
                            "StartofPI_SnapShot_Year_1",
                            "StartofPI_SnapShot_Year_2",
                            "StartofPI_SnapShot_Year_3",
                            "StartofPI_SnapShot_Year_4",
                            "StartofPI_SnapShot_Year_5",
                            "StartofPI_SnapShot_Year_6",
                            "StartofPI_SnapShot_Year_7",
                            "StartofPI_SnapShot_Year_8",
                            "StartofPI_SnapShot_Year_9",
                            "StartofPI_SnapShot_Year_10",
                            "StartofPI_SnapShot_Year_20",
                            "StartofPI_SnapShot_Year_30",
                            "StartofPI_SnapShot_Year_40",
                            "StartofPI_SnapShot_Year_50",
                            "StartofPI_SnapShot_Year_60",
                            "StartofPI_SnapShot_Year_70",
                            "StartofPI_SnapShot_Year_80",
                            "StartofPI_SnapShot_Year_90",
                            "StartofPI_SnapShot_Year_100",
                            "OutPutExoticVsEliteStartOfPIY1",
                            "OutPutExoticVsEliteStartOfPIY2",
                            "OutPutExoticVsEliteStartOfPIY3",
                            "OutPutExoticVsEliteStartOfPIY4",
                            "OutPutExoticVsEliteStartOfPIY5",
                            "OutPutExoticVsEliteStartOfPIY6",
                            "OutPutExoticVsEliteStartOfPIY7",
                            "OutPutExoticVsEliteStartOfPIY8",
                            "OutPutExoticVsEliteStartOfPIY9",
                            "OutPutExoticVsEliteStartOfPIY10",
                            "OutPutExoticVsEliteStartOfPIY20",
                            "OutPutExoticVsEliteStartOfPIY30",
                            "OutPutExoticVsEliteStartOfPIY40",
                            "OutPutExoticVsEliteStartOfPIY50",
                            "OutPutExoticVsEliteStartOfPIY60",
                            "OutPutExoticVsEliteStartOfPIY70",
                            "OutPutExoticVsEliteStartOfPIY80",
                            "OutPutExoticVsEliteStartOfPIY90",
                            "OutPutExoticVsEliteStartOfPIY100",
                            
                            
                            "OutPutExoticVsEliteStartOfPIY1Diff",
                            "OutPutExoticVsEliteStartOfPIY2Diff",
                            "OutPutExoticVsEliteStartOfPIY3Diff",
                            "OutPutExoticVsEliteStartOfPIY4Diff",
                            "OutPutExoticVsEliteStartOfPIY5Diff",
                            "OutPutExoticVsEliteStartOfPIY6Diff",
                            "OutPutExoticVsEliteStartOfPIY7Diff",
                            "OutPutExoticVsEliteStartOfPIY8Diff",
                            "OutPutExoticVsEliteStartOfPIY9Diff",
                            "OutPutExoticVsEliteStartOfPIY10Diff",
                            "OutPutExoticVsEliteStartOfPIY20Diff",
                            "OutPutExoticVsEliteStartOfPIY30Diff",
                            "OutPutExoticVsEliteStartOfPIY40Diff",
                            "OutPutExoticVsEliteStartOfPIY50Diff",
                            "OutPutExoticVsEliteStartOfPIY60Diff",
                            "OutPutExoticVsEliteStartOfPIY70Diff",
                            "OutPutExoticVsEliteStartOfPIY800Diff",
                            "OutPutExoticVsEliteStartOfPIY90Diff",
                            "OutPutExoticVsEliteStartOfPIY100Diff",
                            "listExoticY1",
                            "listExoticY2",
                            "listExoticY3",
                            "listExoticY4",
                            "listExoticY5",
                            "listExoticY6",
                            "listExoticY7",
                            "listExoticY8",
                            "listExoticY9",
                            "listExoticY10",
                            "listExoticY20",
                            "listExoticY30",
                            "listExoticY40",
                            "listExoticY50",
                            "listExoticY60",
                            "listExoticY70",
                            "listExoticY80",
                            "listExoticY90",
                            "listExoticY100",
                            "listEliteStartOfPIY1",
                            "listEliteStartOfPIY2",
                            "listEliteStartOfPIY3",
                            "listEliteStartOfPIY4",
                            "listEliteStartOfPIY5",   
                            "listEliteStartOfPIY6",
                            "listEliteStartOfPIY7",
                            "listEliteStartOfPIY8",
                            "listEliteStartOfPIY9",
                            "listEliteStartOfPIY10", 
                            "listEliteStartOfPIY20",
                            "listEliteStartOfPIY30",
                            "listEliteStartOfPIY40",
                            "listEliteStartOfPIY50",
                            "listEliteStartOfPIY60", 
                            "listEliteStartOfPIY70",
                            "listEliteStartOfPIY80",
                            "listEliteStartOfPIY90",
                            "listEliteStartOfPIY100",
                            "OutPutTable",
			                      "OutPutDiff",
                            "n",
                            "JOBREP"))])

save.image(paste0("HaploBlockAnalyses",n,"QTL_",JOBREP,".rda"))
