#Breider et al. 2022 DOI: 10.21203/rs.3.rs-1246254/v1
#
#!/exports/cmvm/eddie/eb/groups/hickey_group/R-Folder/R-3.5.1/bin/Rscript
#$ -cwd
#$ -R y
#$ -pe sharedmem 8  #nr of cores
#$ -l h_vmem=16G #memory per core
#$ -l h_rt=48:00:00
#$ -l h=!node1f01.ecdf.ed.ac.uk&!node1f02.ecdf.ed.ac.uk&!node2j01.ecdf.ed.ac.uk&!node2j02.ecdf.ed.ac.uk&!node2j03.ecdf.ed.ac.uk&!node2j04.ecdf.ed.ac.uk&!node1c08.ecdf.ed.ac.uk
#$ -j y
#$ -V

library(AlphaSimR)

args <- commandArgs(trailingOnly = TRUE) 

#Global Parameters
#Parameters to set by user
nYears=100
#GenoInPI=6000 

#Parameters passed on from commandline
chosenAcc=as.numeric(args[1]) 
nBridges=as.numeric(args[2]) 
nCyclesBr=as.numeric(args[3]) 
IntrogressionRate=as.numeric(args[4])   #In ratio, not %
BridgingRate=as.numeric(args[5])        #In ratio, not %
ReturnRate=as.numeric(args[6])         #In ratio, not %
ExitRate=as.numeric(args[7])           #Cannot be bigger than nCyclesBr
ParentsPI=as.numeric(args[8])  #100 males 100 females (males and females separate, but same number) or 50 and 50
GenoInPI=as.numeric(args[9])
JOBREP=as.numeric(args[10]) #JOBREP 1 to 10, make sure right burnin is read
nQTL=1000

#Calculations within Script
GenoInBR=GenoInPI*0.2
BRIntensity=floor(GenoInBR/(nBridges*nCyclesBr))
if(nCyclesBr==1){PIIntensity=GenoInPI/2}else{PIIntensity=GenoInPI/nCyclesBr}
#To start bridges off #This means, bridges that don't exist won't be set to initiate=TRUE. Whether bridges get initiated or not is determined in InitiateBridges.R
if(nBridges>0){InitBR1=TRUE}else{InitBR1=FALSE}
if(nBridges>1){InitBR2=TRUE}else{InitBR2=FALSE}
if(nBridges>2){InitBR3=TRUE}else{InitBR3=FALSE}
if(nBridges>3){InitBR4=TRUE}else{InitBR4=FALSE}
if(nBridges>4){InitBR5=TRUE}else{InitBR5=FALSE}
if(nBridges>5){InitBR6=TRUE}else{InitBR6=FALSE}
if(nBridges>6){InitBR7=TRUE}else{InitBR7=FALSE}
BR2Initiated=FALSE
BR3Initiated=FALSE
BR4Initiated=FALSE
BR5Initiated=FALSE
BR6Initiated=FALSE
BR7Initiated=FALSE
LASTBR=FALSE
#To enable set accuracy
setAcc = function(pop,acc){
  G = diag(varG(pop))
  return(G/(acc^2)-G)
}
#Output saved in
genValDH = genVarDH = genicVarDH = matrix(0,nrow=nYears+1,ncol=1)
genValEYT1 = genVarEYT1 = genicVarEYT1 = matrix(0,nrow=nYears+1,ncol=1)
genValPI = genVarPI = genicVarPI = matrix(0,nrow=nYears+1,ncol=1)
#genValDHex1 = genicVarDHex1 = matrix(0,nrow=nYears+1,ncol=1)
#genValDHex2 = genicVarDHex2 = matrix(0,nrow=nYears+1,ncol=1)
#genValDHex3 = genicVarDHex3 = matrix(0,nrow=nYears+1,ncol=1)
#genValDHex4 = genicVarDHex4 = matrix(0,nrow=nYears+1,ncol=1)

#Load burnins
if(IntrogressionRate>0){load(paste0("BurnInExoticExPVPsFirst",JOBREP,".rda"))}
if(IntrogressionRate>0){load(paste0("BurnInExoticExPVPsThird",JOBREP,".rda"))}
if(IntrogressionRate>0){load(paste0("BurnInExoticExPVPsSecond",JOBREP,".rda"))}
if(IntrogressionRate>0){load(paste0("BurnInExoticExPVPsFourth",JOBREP,".rda"))}
load(paste0("BurnInEliteExPVPs",JOBREP,".rda"))
Temp<-pullIbdHaplo(F1)
Temp2<-((Temp)%/%2)+1
Temp3<-table(Temp2)
Temp4<-dimnames(Temp3)
EliteAnch<-Temp4$Temp2
Temp<-pullIbdHaplo(DH)
Temp2<-((Temp)%/%2)+1
Temp3<-table(Temp2)
Temp4<-dimnames(Temp3)
EliteAnch<-c(EliteAnch,Temp4$Temp2)
Temp<-pullIbdHaplo(HDRW)
Temp2<-((Temp)%/%2)+1
Temp3<-table(Temp2)
Temp4<-dimnames(Temp3)
EliteAnch<-c(EliteAnch,Temp4$Temp2)
Temp<-pullIbdHaplo(PYT)
Temp2<-((Temp)%/%2)+1
Temp3<-table(Temp2)
Temp4<-dimnames(Temp3)
EliteAnch<-c(EliteAnch,Temp4$Temp2)
Temp<-pullIbdHaplo(AYT)
Temp2<-((Temp)%/%2)+1
Temp3<-table(Temp2)
Temp4<-dimnames(Temp3)
EliteAnch<-c(EliteAnch,Temp4$Temp2)
Temp<-pullIbdHaplo(EYT1)
Temp2<-((Temp)%/%2)+1
Temp3<-table(Temp2)
Temp4<-dimnames(Temp3)
EliteAnch<-c(EliteAnch,Temp4$Temp2)
Temp<-pullIbdHaplo(EYT2)
Temp2<-((Temp)%/%2)+1
Temp3<-table(Temp2)
Temp4<-dimnames(Temp3)
EliteAnch<-c(EliteAnch,Temp4$Temp2)
EliteAnch<-table(EliteAnch)
EliteAnch<-dimnames(EliteAnch)
EliteAnch<-EliteAnch$EliteAnch


#Start simulation
for(YEAR in 1:nYears){
  print(YEAR)
  source("ProdDev.R")
  #
  for(i in 1:nCyclesBr){
    source("LookUpExitReceive.R")
    if((YEAR==1)&(i==1)){source("Start2Part.R")}  #just to set up initial parents
    if((YEAR==2)&(i==1)&(IntrogressionRate>0)){
                  #Create initial exotics for initiating bridge 1 48 inds
                  VarPop=mergePops(list(VARex1,VARex2,VARex3,VARex4))
                  F1Br1=randCross2(VarPop,PIToFirstCross,48)
                  rm(VarPop)
                  rm(PIToFirstCross)}
    if((YEAR>=3)&(nBridges>0)){source("OneBR1Cycle.R")}
    if((YEAR>=4)&(nBridges>1)){source("OneBR2Cycle.R")} 
    if((YEAR>=5)&(nBridges>2)){source("OneBR3Cycle.R")}
    if((YEAR>=6)&(nBridges>3)){source("OneBR4Cycle.R")}  
    if((YEAR>=7)&(nBridges>4)){source("OneBR5Cycle.R")}
    if((YEAR>=8)&(nBridges>5)){source("OneBR6Cycle.R")}
    if((YEAR>=9)&(nBridges>6)){source("OneBR7Cycle.R")}
    if(nCyclesBr==1){SecondCycle=FALSE} #just to reset counter
    source("One2PartCycle.R")
    if(nCyclesBr==1){
      SecondCycle=TRUE
      source("One2PartCycle.R")}
  }
  rm(exit)
  rm(NextReceives)
  #
  if(IntrogressionRate>0){source("ExPVPs.R")}
  genValDH[YEAR+1] = meanG(DH)-interceptDH
  genVarDH[YEAR+1] = varG(DH)
  genicVarDH[YEAR+1] = genicVarG(DH)
  genValEYT1[YEAR+1] = meanG(EYT1)-interceptEYT1
  genVarEYT1[YEAR+1] = varG(EYT1)
  genicVarEYT1[YEAR+1] = genicVarG(EYT1)
  genValPI[YEAR+1] = meanG(mergePops(list(ProduceFemales,ProduceMales)))-interceptPI
  genVarPI[YEAR+1] = varG(mergePops(list(ProduceFemales,ProduceMales)))
  genicVarPI[YEAR+1] = genicVarG(mergePops(list(ProduceFemales,ProduceMales)))
 # genValDHex1[YEAR+1] = meanG(DHex1)-interceptDHex1
 # genicVarDHex1[YEAR+1] = genicVarG(DHex1)
 # genValDHex2[YEAR+1] = meanG(DHex2)-interceptDHex2
 # genicVarDHex2[YEAR+1] = genicVarG(DHex2)
 # genValDHex3[YEAR+1] = meanG(DHex3)-interceptDHex3
 # genicVarDHex3[YEAR+1] = genicVarG(DHex3)
 # genValDHex4[YEAR+1] = meanG(DHex4)-interceptDHex4
 # genicVarDHex4[YEAR+1] = genicVarG(DHex4)
  
}

rm(HDRW)
rm(PYT)
rm(AYT)
rm(EYT2)
#rm(VARex1)
#rm(EYT2ex1)
#rm(EYT1ex1)
#rm(AYTex1)
#rm(PYTex1)
#rm(HDRWex1)
#rm(DHex1)
#rm(F1ex1)
#rm(VARex2)
#rm(EYT2ex2)
#rm(EYT1ex2)
#rm(AYTex2)
#rm(PYTex2)
#rm(HDRWex2)
#rm(DHex2)
#rm(F1ex2)
#rm(VARex3)
#rm(EYT2ex3)
#rm(EYT1ex3)
#rm(AYTex3)
#rm(PYTex3)
#rm(HDRWex3)
#rm(DHex3)
#rm(F1ex3)
#rm(VARex4)
#rm(EYT2ex4)
#rm(EYT1ex4)
#rm(AYTex4)
#rm(PYTex4)
#rm(HDRWex4)
#rm(DHex4)
#rm(F1ex4)

JOB = Sys.getenv("SGE_TASK_ID")
mainDir <- getwd()
subDir <- paste0("Acc",chosenAcc,"nBridges",nBridges,
          "nCyclesBr",nCyclesBr,"IR",IntrogressionRate,"BR",BridgingRate,
          "RR",ReturnRate,"ER",ExitRate,"GenoInPI",GenoInPI)

dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
setwd(file.path(mainDir, subDir))

save.image(paste0("MultiPart_Acc",chosenAcc,"nBridges",nBridges,
          "nCyclesBr",nCyclesBr,"IR",IntrogressionRate,"BR",BridgingRate,
                  "RR",ReturnRate,"ER",ExitRate,"GenoInPI",GenoInPI,"_",JOBREP,".rda"))

rm(list= ls()[!(ls() %in% c('chosenAcc','genValDH','genVarDH','genicVarDH',
                            'genValEYT1','genVarEYT1','genicVarEYT1',
                            'genValPI','genVarPI','genicVarPI',
                            'genValDHex1','genicVarDHex1','genValDHex2',
                            'genicVarDHex2','genValDHex3','genicVarDHex3',
                            'genValDHex4','genicVarDHex4','JOBREP',
                            'Acc','GenLag','nBridges','nCyclesBr','IntrogressionRate',
                            'BridgingRate','ReturnRate','ExitRate','nQTL','GenoInPI'))])

save.image(paste0("MultiPart_Acc",chosenAcc,"nBridges",nBridges,
                  "nCyclesBr",nCyclesBr,"IR",IntrogressionRate,"BR",BridgingRate,
                  "RR",ReturnRate,"ER",ExitRate,"GenoInPI",GenoInPI,"_",JOBREP,"Output.rda"))
