#Breider et al. 2022 DOI: 10.21203/rs.3.rs-1246254/v1
#
#Origin of parents at start of bridge
if(InitBR1==FALSE){ #Either nBridges==0 , BR1 is already initiated, or it is too early to initiate BR1)
  NextBR1=NextBR1[sample.int(nInd(NextBR1))]#Randomises the order of individuals in the population
  #Introgression on parents, preparing female and male parent sub populations.
  if(exists("VarPop")==TRUE){ #VarPop originates from ExPVP.R. These are 8 ExPVP individuals to introgress. VarPop only exists at the start of the year, when introgression into the first bridge starts.
    Females=NextBR1[1:(50-nInd(VarPop))]}else{Females=NextBR1[1:50]} #If VarPop exists 50-nInd(VarPop) females need to be selected from the last bridging cycle, if VarPop doesn't exist 50 females need to be selected from the last bridging cycle.
  if((exists(paste("PIToBR1", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))==TRUE)&(ReturnRate>=0.5)){Males=NextBR1[0]}else{Males=NextBR1[(nInd(Females)+1):nInd(NextBR1)]} #If PIToBR1_[last cycle] exists and ReturnRate is 0.5, no males need to be selected from the last bridging cycle, otherwise the individuals left in NextBR1 after females are assigned are assigned to males (50 when no introgression, 50-nInd(PIToBR1_[last cycle]).
  #Actual introgression. VarPop is introgressed into the female sub-population, PIToBR1 is introgressed into the male sub-population.
  if(exists("VarPop")==TRUE){ #VarPop only exists at the start of the year, when introgression into the first bridge starts.
    Females=mergePops(list(Females,VarPop)) #Combines female parents and VarPop to one population (50 inds in total).
    rm(VarPop)} #VarPop no longer needed, removed to avoid re-introgressing them in the next breeding cycle of the year.
  if(exists(paste("PIToBR1", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))==TRUE){ #PIToBr1_[last cycle] only exists when germplasm exchange takes place.
    Males=mergePops(list(Males,get(paste("PIToBR1", if(i-1>0){i-1}else{nCyclesBr}, sep = "_")))) #Combines male parents and PIToBr1_[last cycle] to one population (50 inds in total).
    rm(list=paste("PIToBR1", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))} #Remove PIToBr1_[last cycle] as no longer needed.
} 

if(InitBR1==TRUE){ #Time to initiate BR1
  Females=F1Br1 #F1 cross for the start of bridge 1, originates from MutliPArtPilarWithSetAcc.R
  Males=get(paste("PIToBR1", if(i-1>0){i-1}else{nCyclesBr}, sep = "_")) #Assign males used to perform the first BR1 breeding cycle, originating from the last PI breeding cycle 
  rm(list=paste("PIToBR1", if(i-1>0){i-1}else{nCyclesBr}, sep = "_")) #Remove the variable above males were stored in before assigning
  assign(paste("F1Br_SnapShot_Year", YEAR, sep = "_"),F1Br1) #Take a snapshot of IDs and genomes present in F1Br1 for further analyses
  rm(F1Br1) #Remove F1Br1 as individuals are now assigned to Females
  InitBR1=FALSE #Set InitBR1 to FALSE as BR1 has now been initiated
}

#print(paste("BR1","nMales",nInd(Males),"nFemales",nInd(Females))) #To check numbers are correct.

#Produce inds for next cycle and to pass on
ProduceCross=randCross2(Females,Males,BRIntensity) #Creates the next generation, limited by the max number of Inds that can be genotyped (budget restrain)
rm(Females) #Female parents no longer needed, remove to avoid unexpected reuse
rm(Males) #Male parents no longer needed, remove to avoid unexpected reuse
ProduceCross=setPheno(ProduceCross,varE=setAcc(ProduceCross,chosenAcc)) #Set phenotype for new generation, based on accuracy chosen for GS
#Numbers needed for next cycle
#For next BR1
if(NextReceives==TRUE){ #If the next breeding cycle receives germplasm from previous component, then == TRUE
  if(i==nCyclesBr){nNextBR1=100-100*ReturnRate-8 #Nr of individuals needed for next breeding cycle when introgression and germplasm exchange take place
  }else{nNextBR1=100-100*ReturnRate} #Nr of individuals needed for next breeding cycle when germplasm exchange takes place, but no introgression
}else{nNextBR1=100} #Nr of individuals needed for next breeding cycle when no introgression or germplasm exchange takes place

#Numbers of inds to pass on to next BR
if(exit==TRUE){ #If germplasm leaves this bridge at the end of the breeding cycle, then == TRUE
  if(BR2Initiated==TRUE){ #If the next bridge is established, then == TRUE
    if(nBridges>1){nToNextBR=100*BridgingRate}else{nToNextBR=100*IntrogressionRate} #If there is more than 1 bridge, nInd depends on BridgingRate, if there is 1 bridge, nInd depends on IntrogressionRate (Inds go to PI)
  }else if(InitBR2==TRUE){ #If it is time to initiate bridge 2, then ==TRUE
    if(i==nCyclesBr){nToNextBR=50 #These are the number of females to initiate the next bridge
    InitBR2="InProgress" #Set the status of bridge 2 to in progress, meaning individuals are available for the first cross, but this cross has not been performed yet
    }else{nToNextBR=0}  #If bridge 2 is not initiated yet, no individuals will be assigned to move to this bridge
  }else if((InitBR2=="InProgress")|(InitBR2==FALSE)){ #If the next cycle of bridge 2 receives germplasm from bridge 1, then==TRUE. Note that InitBR2=="InProgress" is only true the year that bridge 2 is established, therefore the next year needs to receive germplasm, if exchange takes place.
    if(nBridges>1){nToNextBR=100*BridgingRate}else{nToNextBR=100*IntrogressionRate}} #Sets the number of individuals to move to the next bridge when this bridge is established, or PI when no next bridge
}else{nToNextBR=0} #when exit is FALSE

#print(paste("Bridge 1 nToNextBR",nToNextBR)) #Check of number of inds moving to next bridge.
#Total needed
nSelect=nNextBR1+nToNextBR #Total number of individuals to be selected from the last cross
#select best individuals
BestProduceCross=selectInd(ProduceCross,nSelect,use="pheno",selectTop=TRUE)  #Select best individuals from the last cross. As setPheno was set based on acc chosen for GS, this selection reflects GS.
rm(ProduceCross) #Remove population selected individuals were selected from.
#randomise as now randomly split between going back into the bridge and passing on to the next
BestProduceCross=BestProduceCross[sample.int(nInd(BestProduceCross))] #Randomise position of individuals within population to assure individuals are randomly assigned to remain in the bridge or move to the next component.
if(nNextBR1>0){NextBR1=BestProduceCross[1:nNextBR1]}else{NextBR1=BestProduceCross[0]} #To avoid problems when setting up the bridges, nNextBR==0 when BR1 is being initiated, therefore NextBR1 becomes an empty population.
#IF ONE BrCYCLE PER YEAR
if(nCyclesBr==1){
  if((exit==TRUE)&(InitBR2==TRUE)){ #This avoids the wrong number of inds to be created before initBR2 is set up at the end of the year, when InitBR2 is not TRUE. When InitBR2==TRUE, BR2 has been intiated.
    if(nBridges>1){assign(paste("BR1ToBR2", YEAR, sep = "_Y"),BestProduceCross[(nNextBR1+1):nSelect]) #If more than 1 bridge, create population to move from BR1 to BR2 to initiate BR2.
    }else{assign(paste("ToPI", YEAR, sep = "_Y"),BestProduceCross[(nNextBR1+1):nSelect])} #If no next BR, create population to move to PI
    BR2Initiated=TRUE #The first time this happens BR2Initiated is set to TRUE. As InitBR2 is no longer needed it has been set to InProgress (above) or FALSE (OneBR2Cycle.R), this if statement won't be in use any longer. 
  }else if((exit==TRUE)&(BR2Initiated==TRUE)||(exit==TRUE)&(InitBR2=="InProgress")){ #BR2 is initiated or InProgress (of being initiated).
    if(nBridges>1){assign(paste("BR1ToBR2", YEAR, sep = "_Y"),BestProduceCross[(nNextBR1+1):nSelect]) #If more than 1 bridge, create population to move from BR1 to BR2.
    }else{assign(paste("ToPI", YEAR, sep = "_Y"),BestProduceCross[(nNextBR1+1):nSelect])} #If no next BR, create population to move to PI
  }else if((exit==TRUE)&(InitBR2==FALSE)){ #If no second bridge exists, or the second bridge has not been initiated yet.
    if(nBridges>1){assign(paste("BR1ToBR2", YEAR, sep = "_Y"),BestProduceCross[(nNextBR1+1):nSelect]) #If more than 1 bridge, create population to move from BR1 to BR2 to set up BR2.
    }else if(nToNextBR>0){assign(paste("ToPI", YEAR, sep = "_Y"),BestProduceCross[(nNextBR1+1):nSelect]) #If there is no BR2, but inds move to PI
    }else if(nToNextBR==0){assign(paste("ToPI", YEAR, sep = "_Y"),selectInd(BestProduceCross,0))} #If there is no BR2, and germplasm moves through the breeding program, but inds don't move to PI (as this always only happens once/year)
  }
}  
#IF MORE THAN ONE BrCYCLE PER YEAR 
if(nCyclesBr!=1){
  if((exit==TRUE)&(InitBR2==TRUE)&(i==nCyclesBr)&(nCyclesBr!=1)){ #This avoids inds to be created before initBR2 is set up at the end of the year, when InitBR2 is not TRUE
    if(nBridges>1){assign(paste("BR1ToBR2", i, sep = "_"),BestProduceCross[(nNextBR1+1):nSelect]) #If more than 1 bridge, create population to move from BR1 to BR2 to initiate BR2.
    }else{assign(paste("ToPI", i, sep = "_"),BestProduceCross[(nNextBR1+1):nSelect])} #If no next BR, create population to move to PI
    BR2Initiated=TRUE #The first time this happens BR2Initiated is set to TRUE. As InitBR2 is no longer needed it has been set to InProgress (above) or FALSE (OneBR2Cycle.R), this if statement won't be in use any longer. 
  }else if((exit==TRUE)&(BR2Initiated==TRUE)||(exit==TRUE)&(InitBR2=="InProgress")){  #BR2 is initiated or InProgress (of being initiated).
    if(nBridges>1){assign(paste("BR1ToBR2", i, sep = "_"),BestProduceCross[(nNextBR1+1):nSelect]) #If more than 1 bridge, create population to move from BR1 to BR2.
    }else{assign(paste("ToPI", i, sep = "_"),BestProduceCross[(nNextBR1+1):nSelect])} #If no next BR, create population to move to PI
  }else if((exit==TRUE)&(InitBR2==FALSE)){ #If no second bridge exists, or the second bridge has not been initiated yet.
    if(nBridges>1){assign(paste("BR1ToBR2", i, sep = "_"),BestProduceCross[(nNextBR1+1):nSelect]) #If more than 1 bridge, create population to move from BR1 to BR2 to set up BR2.
    }else if(nToNextBR>0){assign(paste("ToPI", i, sep = "_"),BestProduceCross[(nNextBR1+1):nSelect]) #If there is no BR2, but inds move to PI
    }else if(nToNextBR==0){assign(paste("ToPI", i, sep = "_"),selectInd(BestProduceCross,0))} #If there is no BR2, and germplasm moves through the breeding program, but inds don't move to PI (as this always only happens once/year)
  }
}
#
#
#clean up
rm(nNextBR1)
rm(nSelect)
rm(nToNextBR)
rm(BestProduceCross) 

