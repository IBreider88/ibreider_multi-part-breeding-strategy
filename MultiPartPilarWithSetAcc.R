#Author: I.S. Breider
#This script uses R version 4.0.0 (2020-04-24) "Arbor Day" and AlphaSimR 0.12.2 

library(AlphaSimR) #Load AlphaSimR

args <- commandArgs(trailingOnly = TRUE) #Read in arguments from commandline

#Global Parameters
#Parameters to set by user
nYears=100 #Years of future breeding
nQTL=1000 #Number of QLTs simulated per chromosome

#Parameters passed on from commandline
chosenAcc=as.numeric(args[1]) #GS accuracy
nBridges=as.numeric(args[2]) #Number of bridges
nCyclesBr=as.numeric(args[3]) #Number of breeding cycles per bridge/PI
IntrogressionRate=as.numeric(args[4]) #In ratio, not %, maximum of 0.5
BridgingRate=as.numeric(args[5]) #In ratio, not %
ReturnRate=as.numeric(args[6]) #In ratio, not %
ExitRate=as.numeric(args[7]) #Cannot be bigger than nCyclesBr
ParentsPI=as.numeric(args[8])  #50 males 50 females
GenoInPI=as.numeric(args[9]) #Genotyping budget for Population Improvement
JOBREP=as.numeric(args[10]) #JOBREP 1 to 10, make sure right burn-in input file is read in


#Calculations within Script
GenoInBR=GenoInPI*0.2 #Setting genotyping budget of bridging component to 20% of Population Improvement
BRIntensity=floor(GenoInBR/(nBridges*nCyclesBr)) #Genotyping budget for each breeding cycle in the bridging component
if(nCyclesBr==1){PIIntensity=GenoInPI/2}else{PIIntensity=GenoInPI/nCyclesBr} #Genotyping budget for each breeding cycle in the population improvement, where the population improvement has a minimum of 2 cycles per year
#To start bridges off #This means, bridges that don't exist won't be set to initiate=TRUE. Whether bridges get initiated or not is determined in InitiateBridges.R
if(nBridges>0){InitBR1=TRUE}else{InitBR1=FALSE} #If more than 0 bridges, initiation of bridge 1 will be enabled.
if(nBridges>1){InitBR2=TRUE}else{InitBR2=FALSE} #If more than 1 bridges, initiation of bridge 2 will be enabled.
if(nBridges>2){InitBR3=TRUE}else{InitBR3=FALSE} #If more than 2 bridges, initiation of bridge 3 will be enabled.
if(nBridges>3){InitBR4=TRUE}else{InitBR4=FALSE} #If more than 3 bridges, initiation of bridge 4 will be enabled.
if(nBridges>4){InitBR5=TRUE}else{InitBR5=FALSE} #If more than 4 bridges, initiation of bridge 5 will be enabled.
if(nBridges>5){InitBR6=TRUE}else{InitBR6=FALSE} #If more than 5 bridges, initiation of bridge 6 will be enabled.
if(nBridges>6){InitBR7=TRUE}else{InitBR7=FALSE} #If more than 6 bridges, initiation of bridge 7 will be enabled.
#no BR1Initiated, as BR1 is initiated from F1 EXpvp/Elite//Elite
BR2Initiated=FALSE #Status of BR2, not yet initiated at start of analysis
BR3Initiated=FALSE #Status of BR3, not yet initiated at start of analysis
BR4Initiated=FALSE #Status of BR4, not yet initiated at start of analysis
BR5Initiated=FALSE #Status of BR5, not yet initiated at start of analysis
BR6Initiated=FALSE #Status of BR6, not yet initiated at start of analysis
BR7Initiated=FALSE #Status of BR7, not yet initiated at start of analysis
LASTBR=FALSE #Status of the last bridge in the simulation, not yet initiated at start of analysis

#Definition to enable to set accuracy for GS
setAcc = function(pop,acc){
  G = diag(varG(pop))
  return(G/(acc^2)-G)
}
#Matrices to save output
genValDH = genVarDH = genicVarDH = matrix(0,nrow=nYears+1,ncol=1)
genValEYT1 = genVarEYT1 = genicVarEYT1 = matrix(0,nrow=nYears+1,ncol=1)
genValPI = genVarPI = genicVarPI = matrix(0,nrow=nYears+1,ncol=1)
genValDHex1 = genicVarDHex1 = matrix(0,nrow=nYears+1,ncol=1)
genValDHex2 = genicVarDHex2 = matrix(0,nrow=nYears+1,ncol=1)
genValDHex3 = genicVarDHex3 = matrix(0,nrow=nYears+1,ncol=1)
genValDHex4 = genicVarDHex4 = matrix(0,nrow=nYears+1,ncol=1)

#Load burnins
if(IntrogressionRate>0){load(paste0("BurnInExoticExPVPsFirst",JOBREP,".rda"))} #Load burn-in ExPVP breeding program 1
if(IntrogressionRate>0){load(paste0("BurnInExoticExPVPsThird",JOBREP,".rda"))} #Load burn-in ExPVP breeding program 2
if(IntrogressionRate>0){load(paste0("BurnInExoticExPVPsSecond",JOBREP,".rda"))} #Load burn-in ExPVP breeding program 3
if(IntrogressionRate>0){load(paste0("BurnInExoticExPVPsFourth",JOBREP,".rda"))} #Load burn-in ExPVP breeding program 4
load(paste0("BurnInEliteExPVPs",JOBREP,".rda")) #Load burn-in elite breeding program 

#To save the IDs of my elite founders for haploblock tracking
source("SaveEliteAnch.R")

#Start simulation
for(YEAR in 1:nYears){
  source("ProdDev.R") #Moving Product development forward with one year
  #
  for(i in 1:nCyclesBr){ #Loop to perform breeding cycles within year
    source("LookUpExitReceive.R") #Table to set variables based on whether or not germplasm exchange takes place this cycle
    if((YEAR==1)&(i==1)){source("Start2Part.R")}  #Initial cycle of Parent Improvement, uses parents from burn-in and transitions breeding program from conventional to two-part
    if((YEAR==2)&(i==1)&(IntrogressionRate>0)){ #Starts transitioning breeding program to multipart
      VarPop=mergePops(list(VARex1,VARex2,VARex3,VARex4)) #Create initial exotics for initiating bridge 1, 48 inds, originating from ExPVP breeding programs
      founderPop2 = formFounderPop(VarPop) #To reset IDs for haploblock origin tracking
      VarPop = newPop(founderPop2) #To reset IDs for haploblock origin tracking
      assign(paste("F1Br1_SnapShot_Year", YEAR, sep = "_"),VarPop) #Snapshot of population, both IDs and genotypes, for further analyses
      F1Br1=randCross2(VarPop,PIToFirstCross,48) #Create F1 cross for the start of bridge 1. PIToFirstCross created in "InitiateBridges.R".
      rm(VarPop) #Remove VarPop as no longer of use
      rm(PIToFirstCross)} #Used in One2PartCycle.R -> InitiateBridges.R, remove as no longer of use
    if((YEAR>=3)&(nBridges>0)){source("OneBR1Cycle.R")} #One BR1 cycle
    if((YEAR>=4)&(nBridges>1)){source("OneBR2Cycle.R")} #One BR2 cycle
    if((YEAR>=5)&(nBridges>2)){source("OneBR3Cycle.R")} #One BR3 cycle
    if((YEAR>=6)&(nBridges>3)){source("OneBR4Cycle.R")} #One BR4 cycle 
    if((YEAR>=7)&(nBridges>4)){source("OneBR5Cycle.R")} #One BR5 cycle
    if((YEAR>=8)&(nBridges>5)){source("OneBR6Cycle.R")} #One BR6 cycle
    if((YEAR>=9)&(nBridges>6)){source("OneBR7Cycle.R")} #One BR7 cycle
    if(nCyclesBr==1){SecondCycle=FALSE} #Just to reset counter. If nCyclesBr==1, the PI has not cycled yet, so we're not at the second cycle yet.
    source("One2PartCycle.R") #One PI cycle
    if(nCyclesBr==1){ #This only happens when there is 1 bridging cycle per year, as PI always cycles at least twice a year
      SecondCycle=TRUE # If nCyclesBr==1, the PI has now cycled once, so we're now at the second cycle.
      source("One2PartCycle.R") } #Second PI cycle if bridges only cycle once.
  }
  rm(exit) #Indicates whether germplasm exchange takes place (Exits component). Is created every year. Remove to avoid mix up over years
  rm(NextReceives) #Indicates whether germplasm exchange takes place (Component receives). Is created every year. Remove to avoid mix up over years
  
  if(IntrogressionRate>0){source("ExPVPs.R")} #Moving all 4 ExPVP breeding programs forward one year.
  
  #Saving results of the current year (+1 as vector starts at 0)
  genValDH[YEAR+1] = meanG(DH)-interceptDH #Intercept from burn-in, to scale all repeats to the same start.
  genVarDH[YEAR+1] = varG(DH) #genetic variance
  genicVarDH[YEAR+1] = genicVarG(DH) #genic variance
  genValEYT1[YEAR+1] = meanG(EYT1)-interceptEYT1 #Intercept from burn-in, to scale all repeats to the same start.
  genVarEYT1[YEAR+1] = varG(EYT1) #genetic variance
  genicVarEYT1[YEAR+1] = genicVarG(EYT1) #genic variance
  genValPI[YEAR+1] = meanG(mergePops(list(ProduceFemales,ProduceMales)))-interceptPI #Mean genetic value of parents in population improvement. #Intercept from burn-in, to scale all repeats to the same start.
  genVarPI[YEAR+1] = varG(mergePops(list(ProduceFemales,ProduceMales))) #Mean genetic variance of parents in population improvement.
  genicVarPI[YEAR+1] = genicVarG(mergePops(list(ProduceFemales,ProduceMales))) #Mean genic variance of parents in population improvement.
  genValDHex1[YEAR+1] = meanG(DHex1)-interceptDHex1 #Intercept from burn-in, to scale all repeats to the same start.
  genicVarDHex1[YEAR+1] = genicVarG(DHex1) #genic variance
  genValDHex2[YEAR+1] = meanG(DHex2)-interceptDHex2 #Intercept from burn-in, to scale all repeats to the same start.
  genicVarDHex2[YEAR+1] = genicVarG(DHex2) #genic variance
  genValDHex3[YEAR+1] = meanG(DHex3)-interceptDHex3 #Intercept from burn-in, to scale all repeats to the same start.
  genicVarDHex3[YEAR+1] = genicVarG(DHex3) #genic variance
  genValDHex4[YEAR+1] = meanG(DHex4)-interceptDHex4 #Intercept from burn-in, to scale all repeats to the same start.
  genicVarDHex4[YEAR+1] = genicVarG(DHex4) #genic variance
  
}

#Remove populations that aren't needed for future analyses, to save memory.
rm(HDRW)
rm(PYT)
rm(AYT)
rm(EYT2)
rm(VARex1)
rm(EYT2ex1)
rm(EYT1ex1)
rm(AYTex1)
rm(PYTex1)
rm(HDRWex1)
rm(DHex1)
rm(F1ex1)
rm(VARex2)
rm(EYT2ex2)
rm(EYT1ex2)
rm(AYTex2)
rm(PYTex2)
rm(HDRWex2)
rm(DHex2)
rm(F1ex2)
rm(VARex3)
rm(EYT2ex3)
rm(EYT1ex3)
rm(AYTex3)
rm(PYTex3)
rm(HDRWex3)
rm(DHex3)
rm(F1ex3)
rm(VARex4)
rm(EYT2ex4)
rm(EYT1ex4)
rm(AYTex4)
rm(PYTex4)
rm(HDRWex4)
rm(DHex4)
rm(F1ex4)

#Create file name and folder to save results in 
mainDir <- getwd()
subDir <- paste0("Acc",chosenAcc,"nBridges",nBridges,
                 "nCyclesBr",nCyclesBr,"IR",IntrogressionRate,"BR",BridgingRate,
                 "RR",ReturnRate,"ER",ExitRate)
dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
setwd(file.path(mainDir, subDir))

#Save output incl populations for further analyses
save.image(paste0("MultiPart_Acc",chosenAcc,"nBridges",nBridges,
                  "nCyclesBr",nCyclesBr,"IR",IntrogressionRate,"BR",BridgingRate,
                  "RR",ReturnRate,"ER",ExitRate,"_",JOBREP,".rda"))

#Remove anything except essentials, to save memory
rm(list= ls()[!(ls() %in% c('chosenAcc','genValDH','genVarDH','genicVarDH',
                            'genValEYT1','genVarEYT1','genicVarEYT1',
                            'genValPI','genVarPI','genicVarPI',
                            'genValDHex1','genicVarDHex1','genValDHex2',
                            'genicVarDHex2','genValDHex3','genicVarDHex3',
                            'genValDHex4','genicVarDHex4','JOBREP',
                            'Acc','GenLag','nBridges','nCyclesBr','IntrogressionRate',
                            'BridgingRate','ReturnRate','ExitRate','nQTL'))])

#Save output only, excl populations for further analyses
save.image(paste0("MultiPart_Acc",chosenAcc,"nBridges",nBridges,
                  "nCyclesBr",nCyclesBr,"IR",IntrogressionRate,"BR",BridgingRate,
                  "RR",ReturnRate,"ER",ExitRate,"_",JOBREP,"Output.rda"))
