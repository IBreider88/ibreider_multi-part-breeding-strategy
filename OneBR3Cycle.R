if(InitBR3==FALSE){ #Either nBridges <3, BR3 is already initiated, or it is too early to initiate BR3)
  NextBR3=NextBR3[sample.int(nInd(NextBR3))] #Randomises the order of individuals in the population
  #Introgression on parents, preparing female and male parent sub populations.
  if(exists(paste("BR2ToBR3", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))==TRUE){ #This originates from BR2 , if nCycles>1
    if(BridgingRate>=0.5){Females=NextBR2[0]}else{Females=NextBR3[1:(50-nInd(get(paste("BR2ToBR3", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))))]} #If BR==0.5, no females need to be selected from the last bridge, otherwise 50-nInd(BR2ToBR3) females need to be selected from the last bridging cycle.
  }else if(exists(paste("BR2ToBR3", YEAR-1, sep = "_Y"))==TRUE){ #if nCycles==1
    if(BridgingRate>=0.5){Females=NextBR2[0]}else{Females=NextBR2[1:(50-nInd(get(paste("BR2ToBR3", YEAR-1, sep = "_Y"))))]} #If BR==0.5, no females need to be selected from the last bridge, otherwise 50-nInd(BR2ToBR3) females need to be selected from the last bridging cycle.
  }else{Females=NextBR3[1:50]} #If BR2ToBR3 doesn't exist, 50 females need to be selected from the last bridge.
  if((exists(paste("PIToBR3", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))==TRUE)&(ReturnRate>=0.5)){Males=NextBR2[0]}else{Males=NextBR3[(nInd(Females)+1):nInd(NextBR3)]} #If PIToBR3_[last cycle] exists and ReturnRate is 0.5, no males need to be selected from the last bridging cycle, otherwise the individuals left in NextBR3 after females are assigned are assigned to males (50 when no introgression, 50-nInd(PIToBR3_[last cycle]).   
  #Actual introgression. BR2ToBR3 is introgressed into the female sub-population, PIToBR3 is introgressed into the male sub-population.
  if(exists(paste("BR2ToBR3", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))==TRUE){ #If nCyc>1
    Females=mergePops(list(Females,get(paste("BR2ToBR3", if(i-1>0){i-1}else{nCyclesBr}, sep = "_")))) #Merge females to 1 population
    rm(list=paste("BR2ToBR3", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))} #Remove BR2ToBR3 as n longer needed
  if(exists(paste("BR2ToBR3", YEAR-1, sep = "_Y"))==TRUE){ #If nCyc==1 #Merge females to 1 population
    Females=mergePops(list(Females,get((paste("BR2ToBR3", YEAR-1, sep = "_Y"))))) #Merge females to 1 population
    rm(list=paste("BR2ToBR3", YEAR-1, sep = "_Y"))} #Remove BR2ToBR3 as n longer needed
  if(exists(paste("PIToBR3", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))==TRUE){ #PIToBr3_[last cycle] only exists when germplasm exchange takes place.  
    Males=mergePops(list(Males,get(paste("PIToBR3", if(i-1>0){i-1}else{nCyclesBr}, sep = "_")))) #Combines male parents and PIToBr3_[last cycle] to one population (50 inds in total).
    rm(list=paste("PIToBR3", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))} #Remove PIToBr3_[last cycle] as no longer needed.
}  
if((InitBR3=="InProgress")&(nCyclesBr>1)){ #When BR3 is being initiated and nCyc>1
  Females=get(paste("BR2ToBR3", if(i-1>0){i-1}else{nCyclesBr}, sep = "_")) #All females coming from BR2ToBR3
  Males=get(paste("PIToBR3", if(i-1>0){i-1}else{nCyclesBr}, sep = "_")) #All males coming from PIToBR3
  rm(list=paste("PIToBR3", if(i-1>0){i-1}else{nCyclesBr}, sep = "_")) #Remove BR2ToBR3 as no longer needed
  rm(list=paste("BR2ToBR3", if(i-1>0){i-1}else{nCyclesBr}, sep = "_")) #Remove PIToBR3 as no longer needed
  InitBR3=FALSE #BR3 no longer needs to start initiating.
}  
if((InitBR3=="InProgress")&(nCyclesBr==1)){ #When BR3 is being initiated and nCyc>1
  Females=get(paste("BR2ToBR3", YEAR-1, sep = "_Y")) #All females coming from BR2ToBR3
  Males=get(paste("PIToBR3", if(i-1>0){i-1}else{nCyclesBr}, sep = "_")) #All males coming from PIToBR3
  rm(list=paste("BR2ToBR3", YEAR-1, sep = "_Y")) #Remove BR2ToBR3 as no longer needed
  rm(list=paste("PIToBR3", if(i-1>0){i-1}else{nCyclesBr}, sep = "_")) #Remove PIToBR3 as no longer needed
  InitBR3=FALSE #BR3 no longer needs to start initiating.
} 

print(paste("BR3","nMales",nInd(Males),"nFemales",nInd(Females))) #Sanity check ot see if nrs are correct

#Produce inds for next cycle and to pass on
ProduceCross=randCross2(Females,Males,BRIntensity) #Creates the next generation, limited by the max number of Inds that can be genotyped (budget restrain)
rm(Females) #Female parents no longer needed, remove to avoid unexpected reuse
rm(Males) #Male parents no longer needed, remove to avoid unexpected reuse
ProduceCross=setPheno(ProduceCross,varE=setAcc(ProduceCross,chosenAcc)) #Set phenotype for new generation, based on accuracy chosen for GS

#Numbers needed for next cycle
#For next BR3
if(NextReceives==TRUE){ #If the next breeding cycle receives germplasm from previous component, then == TRUE
  nNextBR3=100-100*ReturnRate-100*BridgingRate #Nr of individuals needed for next breeding cycle when germplasm exchange takes place
}else{nNextBR3=100} #Nr of individuals needed for next breeding cycle when no introgression or germplasm exchange takes place

#Numbers of inds to pass on to next BR
if(exit==TRUE){  #If germplasm leaves this bridge at the end of the breeding cycle, then == TRUE
  if(BR4Initiated==TRUE){ #If the next bridge is established, then == TRUE
    if(nBridges>3){nToNextBR=100*BridgingRate}else{nToNextBR=100*IntrogressionRate} #If there is more than 3 bridges, nInd depends on BridgingRate, if there is 3 bridges, nInd depends on IntrogressionRate (Inds go to PI)
  }else if(InitBR4==TRUE){ #If it is time to initiate bridge 4, then ==TRUE
    if(i==nCyclesBr){nToNextBR=50 #These are the number of females to initiate the next bridge
    InitBR4="InProgress" #Set the status of bridge 4 to in progress, meaning individuals are available for the first cross, but this cross has not been performed yet
    }else{nToNextBR=0}  #If bridge 4 is not initiated yet, no individuals will be assigned to move to this bridge
  }else if((InitBR4=="InProgress")|(InitBR4==FALSE)){ #If the next cycle of bridge 4 receives germplasm from bridge 3, then==TRUE. Note that InitBR4=="InProgress" is only true the year that bridge 4 is established, therefore the next year needs to receive germplasm, if exchange takes place.
    if(nBridges>3){nToNextBR=100*BridgingRate}else{nToNextBR=100*IntrogressionRate}} #Sets the number of individuals to move to the next bridge when this bridge is established, or PI when no next bridge
}else{nToNextBR=0} #when exit is FALSE


#print(paste("Bridge 3 nToNextBR",nToNextBR))
#Total needed
nSelect=nNextBR3+nToNextBR #Total number of individuals to be selected from the last cross
#select best individuals
BestProduceCross=selectInd(ProduceCross,nSelect,use="pheno",selectTop=TRUE) #Select best individuals from the last cross. As setPheno was set based on acc chosen for GS, this selection reflects GS.
rm(ProduceCross) #Remove population selected individuals were selected from.
#randomise as now randomly split between going back into the bridge and passing on to the next
BestProduceCross=BestProduceCross[sample.int(nInd(BestProduceCross))] #Randomise position of individuals within population to assure individuals are randomly assigned to remain in the bridge or move to the next component.
if(nNextBR3>0){NextBR3=BestProduceCross[1:nNextBR3]}else{NextBR3=BestProduceCross[0]} #To avoid problems when setting up the bridges, nNextBR==0 when BR2 is being initiated, therefore NextBR2 becomes an empty population.
#IF ONE BrCYCLE PER YEAR
if(nCyclesBr==1){
  if((exit==TRUE)&(InitBR4==TRUE)){ #This avoids the wrong number of inds to be created before initBR4 is set up at the end of the year, when InitBR4 is not TRUE. When InitBR4==TRUE, BR4 has been intiated.
    if(nBridges>3){assign(paste("BR3ToBR4", YEAR, sep = "_Y"),BestProduceCross[(nNextBR3+1):nSelect]) #If more than 3 bridges, create population to move from BR3 to BR4to initiate BR4.
    }else{assign(paste("ToPI", YEAR, sep = "_Y"),BestProduceCross[(nNextBR3+1):nSelect])} #If no next BR, create population to move to PI
    BR4Initiated=TRUE#The first time this happens BR4Initiated is set to TRUE. As InitBR4 is no longer needed it has been set to InProgress (above) or FALSE (OneBR4Cycle.R), this if statement won't be in use any longer. 
  }else if((exit==TRUE)&(BR4Initiated==TRUE)||(exit==TRUE)&(InitBR4=="InProgress")){ #BR4 is initiated or InProgress (of being initiated).
    if(nBridges>3){assign(paste("BR3ToBR4", YEAR, sep = "_Y"),BestProduceCross[(nNextBR3+1):nSelect]) #If more than 3 bridges, create population to move from BR3 to BR4.
    }else{assign(paste("ToPI", YEAR, sep = "_Y"),BestProduceCross[(nNextBR3+1):nSelect])} #If no next BR, create population to move to PI
  }else if((exit==TRUE)&(InitBR4==FALSE)){ #If no fourth bridge exists, or the fourth bridge has not been initiated yet.
    if(nBridges>3){assign(paste("BR3ToBR4", YEAR, sep = "_Y"),BestProduceCross[(nNextBR3+1):nSelect]) #If more than 3 bridges, create population to move from BR3 to BR4 to set up BR4.
    }else if(nToNextBR>0){assign(paste("ToPI", YEAR, sep = "_Y"),BestProduceCross[(nNextBR3+1):nSelect]) #If there is no BR4, but inds move to PI
    }else if(nToNextBR==0){assign(paste("ToPI", YEAR, sep = "_Y"),selectInd(BestProduceCross,0))} #If there is no BR4, and germplasm moves through the breeding program, but inds don't move to PI (as this always only happens once/year)
  }
}  
#IF MORE THAN ONE BrCYCLE PER YEAR 
if(nCyclesBr!=1){
  if((exit==TRUE)&(InitBR4==TRUE)&(i==nCyclesBr)&(nCyclesBr!=1)){ #This avoids inds to be created before initBR4 is set up at the end of the year, when InitBR4 is not TRUE
    if(nBridges>3){assign(paste("BR3ToBR4", i, sep = "_"),BestProduceCross[(nNextBR3+1):nSelect]) #If more than 3 bridges, create population to move from BR3 to BR4 to initiate BR4.
    }else{assign(paste("ToPI", i, sep = "_"),BestProduceCross[(nNextBR3+1):nSelect])} #If no next BR, create population to move to PI 
    BR4Initiated=TRUE #The first time this happens BR4Initiated is set to TRUE. As InitBR4 is no longer needed it has been set to InProgress (above) or FALSE (OneBR4Cycle.R), this if statement won't be in use any longer. 
  }else if((exit==TRUE)&(BR4Initiated==TRUE)||(exit==TRUE)&(InitBR4=="InProgress")){ #BR4 is initiated or InProgress (of being initiated).
    if(nBridges>3){assign(paste("BR3ToBR4", i, sep = "_"),BestProduceCross[(nNextBR3+1):nSelect]) #If more than 3 bridges, create population to move from BR3 to BR4.
    }else{assign(paste("ToPI", i, sep = "_"),BestProduceCross[(nNextBR3+1):nSelect])} #If no next BR, create population to move to PI
  }else if((exit==TRUE)&(InitBR4==FALSE)){ #If no fourth bridge exists, or the fourth bridge has not been initiated yet.
    if(nBridges>3){assign(paste("BR3ToBR4", i, sep = "_"),BestProduceCross[(nNextBR3+1):nSelect]) #If more than 3 bridges, create population to move from BR3 to BR4 to set up BR4.
    }else if(nToNextBR>0){assign(paste("ToPI", i, sep = "_"),BestProduceCross[(nNextBR3+1):nSelect]) #If there is no BR4, but inds move to PI
    }else if(nToNextBR==0){assign(paste("ToPI", i, sep = "_"),selectInd(BestProduceCross,0))} #If there is no BR4, and germplasm moves through the breeding program, but inds don't move to PI (as this always only happens once/year)
  }
}
#
#
#clean up
rm(nNextBR3)
rm(nSelect)
rm(nToNextBR)
rm(BestProduceCross)
