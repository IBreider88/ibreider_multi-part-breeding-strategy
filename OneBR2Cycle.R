#Breider et al. 2022 DOI: 10.21203/rs.3.rs-1246254/v1
#
if(InitBR2==FALSE){ #Either nBridges <2, BR2 is already initiated, or it is too early to initiate BR2)
  NextBR2=NextBR2[sample.int(nInd(NextBR2))] #Randomises the order of individuals in the population
  #Introgression on parents, preparing female and male parent sub populations.
  if(exists(paste("BR1ToBR2", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))==TRUE){ #This originates from BR1 , if nCycles>1
    if(BridgingRate>=0.5){Females=NextBR2[0]}else{Females=NextBR2[1:(50-nInd(get(paste("BR1ToBR2", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))))]} #If BR==0.5, no females need to be selected from the last bridge, otherwise 50-nInd(BR1ToBR2) females need to be selected from the last bridging cycle.
    }else if(exists(paste("BR1ToBR2", YEAR-1, sep = "_Y"))==TRUE){ #if nCycles==1
    if(BridgingRate>=0.5){Females=NextBR2[0]}else{Females=NextBR2[1:(50-nInd(get(paste("BR1ToBR2", YEAR-1, sep = "_Y"))))]} #If BR==0.5, no females need to be selected from the last bridge, otherwise 50-nInd(BR1ToBR2) females need to be selected from the last bridging cycle.
    }else{Females=NextBR2[1:50]} #If BR1ToBR2 doesn't exist, 50 females need to be selected from the last bridge.
  if((exists(paste("PIToBR2", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))==TRUE)&(ReturnRate>=0.5)){Males=NextBR2[0]}else{Males=NextBR2[(nInd(Females)+1):nInd(NextBR2)]} #If PIToBR2_[last cycle] exists and ReturnRate is 0.5, no males need to be selected from the last bridging cycle, otherwise the individuals left in NextBR2 after females are assigned are assigned to males (50 when no introgression, 50-nInd(PIToBR2_[last cycle]).
  #Actual introgression. BR1ToBR2 is introgressed into the female sub-population, PIToBR2 is introgressed into the male sub-population.
  if(exists(paste("BR1ToBR2", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))==TRUE){ #If nCyc>1
    Females=mergePops(list(Females,get((paste("BR1ToBR2", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))))) #Merge females to 1 population
    rm(list=paste("BR1ToBR2", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))} #Remove BR1ToBR2 as n longer needed
  if(exists(paste("BR1ToBR2", YEAR-1, sep = "_Y"))==TRUE){ #If nCyc==1 #Merge females to 1 population
    Females=mergePops(list(Females,get((paste("BR1ToBR2", YEAR-1, sep = "_Y"))))) #Merge females to 1 population
    rm(list=paste("BR1ToBR2", YEAR-1, sep = "_Y"))} #Remove BR1ToBR2 as n longer needed
  if(exists(paste("PIToBR2", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))==TRUE){ #PIToBr2_[last cycle] only exists when germplasm exchange takes place. 
    Males=mergePops(list(Males,get(paste("PIToBR2", if(i-1>0){i-1}else{nCyclesBr}, sep = "_")))) #Combines male parents and PIToBr2_[last cycle] to one population (50 inds in total).
    rm(list=paste("PIToBR2", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))} #Remove PIToBr2_[last cycle] as no longer needed.
}  
if((InitBR2=="InProgress")&(nCyclesBr>1)){ #When BR2 is being initiated and nCyc>1
  Females=get(paste("BR1ToBR2", if(i-1>0){i-1}else{nCyclesBr}, sep = "_")) #All females coming from BR1ToBR2
  Males=get(paste("PIToBR2", if(i-1>0){i-1}else{nCyclesBr}, sep = "_")) #All males coming from PIToBR2
  rm(list=paste("BR1ToBR2", if(i-1>0){i-1}else{nCyclesBr}, sep = "_")) #Remove BR1ToBR2 as no longer needed
  rm(list=paste("PIToBR2", if(i-1>0){i-1}else{nCyclesBr}, sep = "_")) #Remove PIToBR2 as no longer needed
  InitBR2=FALSE #BR2 no longer needs to start initiating.
}  
if((InitBR2=="InProgress")&(nCyclesBr==1)){ #When BR2 is being initiated and nCyc>1
  Females=get(paste("BR1ToBR2", YEAR-1, sep = "_Y")) #All females coming from BR1ToBR2
  Males=get(paste("PIToBR2", if(i-1>0){i-1}else{nCyclesBr}, sep = "_")) #All males coming from PIToBR2
  rm(list=paste("BR1ToBR2", YEAR-1, sep = "_Y")) #Remove BR1ToBR2 as no longer needed
  rm(list=paste("PIToBR2", if(i-1>0){i-1}else{nCyclesBr}, sep = "_")) #Remove PIToBR2 as no longer needed
  InitBR2=FALSE #BR2 no longer needs to start initiating.
} 

print(paste("BR2","nMales",nInd(Males),"nFemales",nInd(Females))) #Sanity check ot see if nrs are correct

#Produce inds for next cycle and to pass on
ProduceCross=randCross2(Females,Males,BRIntensity) #Creates the next generation, limited by the max number of Inds that can be genotyped (budget restrain)
rm(Females) #Female parents no longer needed, remove to avoid unexpected reuse
rm(Males) #Male parents no longer needed, remove to avoid unexpected reuse
ProduceCross=setPheno(ProduceCross,varE=setAcc(ProduceCross,chosenAcc)) #Set phenotype for new generation, based on accuracy chosen for GS

#Numbers needed for next cycle
#For next BR2
if(NextReceives==TRUE){ #If the next breeding cycle receives germplasm from previous component, then == TRUE
  nNextBR2=100-100*ReturnRate-100*BridgingRate #Nr of individuals needed for next breeding cycle when germplasm exchange takes place
}else{nNextBR2=100} #Nr of individuals needed for next breeding cycle when no introgression or germplasm exchange takes place

#Numbers of inds to pass on to next BR
if(exit==TRUE){  #If germplasm leaves this bridge at the end of the breeding cycle, then == TRUE
  if(BR3Initiated==TRUE){ #If the next bridge is established, then == TRUE
    if(nBridges>2){nToNextBR=100*BridgingRate}else{nToNextBR=100*IntrogressionRate} #If there is more than 1 bridge, nInd depends on BridgingRate, if there is 1 bridge, nInd depends on IntrogressionRate (Inds go to PI)
  }else if(InitBR3==TRUE){ #If it is time to initiate bridge 3, then ==TRUE
    if(i==nCyclesBr){nToNextBR=50 #These are the number of females to initiate the next bridge
    InitBR3="InProgress" #Set the status of bridge 2 to in progress, meaning individuals are available for the first cross, but this cross has not been performed yet
    }else{nToNextBR=0}  #If bridge 3 is not initiated yet, no individuals will be assigned to move to this bridge
  }else if((InitBR3=="InProgress")|(InitBR3==FALSE)){ #If the next cycle of bridge 3 receives germplasm from bridge 2, then==TRUE. Note that InitBR3=="InProgress" is only true the year that bridge 3 is established, therefore the next year needs to receive germplasm, if exchange takes place.
    if(nBridges>2){nToNextBR=100*BridgingRate}else{nToNextBR=100*IntrogressionRate}} #Sets the number of individuals to move to the next bridge when this bridge is established, or PI when no next bridge
}else{nToNextBR=0} #when exit is FALSE


#print(paste("Bridge 2 nToNextBR",nToNextBR))
#Total needed
nSelect=nNextBR2+nToNextBR #Total number of individuals to be selected from the last cross
#select best individuals
BestProduceCross=selectInd(ProduceCross,nSelect,use="pheno",selectTop=TRUE) #Select best individuals from the last cross. As setPheno was set based on acc chosen for GS, this selection reflects GS.
rm(ProduceCross) #Remove population selected individuals were selected from.
#randomise as now randomly split between going back into the bridge and passing on to the next
BestProduceCross=BestProduceCross[sample.int(nInd(BestProduceCross))] #Randomise position of individuals within population to assure individuals are randomly assigned to remain in the bridge or move to the next component.
if(nNextBR2>0){NextBR2=BestProduceCross[1:nNextBR2]}else{NextBR2=BestProduceCross[0]} #To avoid problems when setting up the bridges, nNextBR==0 when BR2 is being initiated, therefore NextBR2 becomes an empty population.
#IF ONE BrCYCLE PER YEAR
if(nCyclesBr==1){
  if((exit==TRUE)&(InitBR3==TRUE)){ #This avoids the wrong number of inds to be created before initBR3 is set up at the end of the year, when InitBR3 is not TRUE. When InitBR3==TRUE, BR3 has been intiated.
    if(nBridges>2){assign(paste("BR2ToBR3", YEAR, sep = "_Y"),BestProduceCross[(nNextBR2+1):nSelect]) #If more than 2 bridges, create population to move from BR2 to BR3to initiate BR3.
    }else{assign(paste("ToPI", YEAR, sep = "_Y"),BestProduceCross[(nNextBR2+1):nSelect])} #If no next BR, create population to move to PI
    BR3Initiated=TRUE #The first time this happens BR3Initiated is set to TRUE. As InitBR3 is no longer needed it has been set to InProgress (above) or FALSE (OneBR3Cycle.R), this if statement won't be in use any longer. 
  }else if((exit==TRUE)&(BR3Initiated==TRUE)||(exit==TRUE)&(InitBR3=="InProgress")){ #BR3 is initiated or InProgress (of being initiated).
    if(nBridges>2){assign(paste("BR2ToBR3", YEAR, sep = "_Y"),BestProduceCross[(nNextBR2+1):nSelect]) #If more than 2 bridges, create population to move from BR2 to BR3.
    }else{assign(paste("ToPI", YEAR, sep = "_Y"),BestProduceCross[(nNextBR2+1):nSelect])} #If no next BR, create population to move to PI
  }else if((exit==TRUE)&(InitBR3==FALSE)){ #If no second bridge exists, or the second bridge has not been initiated yet.
    if(nBridges>2){assign(paste("BR2ToBR3", YEAR, sep = "_Y"),BestProduceCross[(nNextBR2+1):nSelect]) #If more than 3 bridges, create population to move from BR2 to BR3 to set up BR3.
    }else if(nToNextBR>0){assign(paste("ToPI", YEAR, sep = "_Y"),BestProduceCross[(nNextBR2+1):nSelect]) #If there is no BR3, but inds move to PI
    }else if(nToNextBR==0){assign(paste("ToPI", YEAR, sep = "_Y"),selectInd(BestProduceCross,0))} #If there is no BR3, and germplasm moves through the breeding program, but inds don't move to PI (as this always only happens once/year)
  }
}  
#IF MORE THAN ONE BrCYCLE PER YEAR 
if(nCyclesBr!=1){ 
  if((exit==TRUE)&(InitBR3==TRUE)&(i==nCyclesBr)&(nCyclesBr!=1)){ #This avoids inds to be created before initBR3 is set up at the end of the year, when InitBR3 is not TRUE
    if(nBridges>2){assign(paste("BR2ToBR3", i, sep = "_"),BestProduceCross[(nNextBR2+1):nSelect]) #If more than 2 bridges, create population to move from BR2 to BR3 to initiate BR3.
    }else{assign(paste("ToPI", i, sep = "_"),BestProduceCross[(nNextBR2+1):nSelect])} #If no next BR, create population to move to PI 
    BR3Initiated=TRUE #The first time this happens BR3Initiated is set to TRUE. As InitBR3 is no longer needed it has been set to InProgress (above) or FALSE (OneBR3Cycle.R), this if statement won't be in use any longer. 
  }else if((exit==TRUE)&(BR3Initiated==TRUE)||(exit==TRUE)&(InitBR3=="InProgress")){ #BR3 is initiated or InProgress (of being initiated).
    if(nBridges>2){assign(paste("BR2ToBR3", i, sep = "_"),BestProduceCross[(nNextBR2+1):nSelect]) #If more than 2 bridges, create population to move from BR2 to BR3.
    }else{assign(paste("ToPI", i, sep = "_"),BestProduceCross[(nNextBR2+1):nSelect])} #If no next BR, create population to move to PI
  }else if((exit==TRUE)&(InitBR3==FALSE)){ #If no third bridge exists, or the third bridge has not been initiated yet.
    if(nBridges>2){assign(paste("BR2ToBR3", i, sep = "_"),BestProduceCross[(nNextBR2+1):nSelect]) #If more than 2 bridges, create population to move from BR2 to BR3 to set up BR3.
    }else if(nToNextBR>0){assign(paste("ToPI", i, sep = "_"),BestProduceCross[(nNextBR2+1):nSelect]) #If there is no BR3, but inds move to PI
    }else if(nToNextBR==0){assign(paste("ToPI", i, sep = "_"),selectInd(BestProduceCross,0))} #If there is no BR3, and germplasm moves through the breeding program, but inds don't move to PI (as this always only happens once/year)
  }
}

#
#clean up
rm(nNextBR2)
rm(nSelect)
rm(nToNextBR)
rm(BestProduceCross)
