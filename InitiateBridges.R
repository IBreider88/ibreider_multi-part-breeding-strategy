#initiate bridges
#Prepare for initial cross. Select Elite individuals for F1 cross.
if(((YEAR==1)&(nCyclesBr==1)&(SecondCycle==TRUE))||((YEAR==1)&(i>1)&(i==nCyclesBr))){  #The year before the first bridge is initiated and last cycle of the year.
  assign(("PIToFirstCross"),selectInd(ProduceExcess[!(ProduceExcess@id %in% ToPD@id)],48,use='rand')) #Making sure that inds assigned to ToPD don't also get assigned to PIToFirstCross.
}

#Prepare to build first Bridge
if(((YEAR==2)&(nCyclesBr==1)&(SecondCycle==TRUE))||((YEAR==2)&(i>1)&(i==nCyclesBr))){  #The year before the first bridge is build (year that first bridge is initiated) and last cycle of the year.
  assign(paste("PIToBR1", i, sep = "_"),selectInd(ProduceExcess[!(ProduceExcess@id %in% ToPD@id)],52,use='rand')) #Create PIToBR1. Making sure that inds assigned to ToPD don't also get assigned to PIToBR1
  if(nBridges==1){LASTBR=TRUE} #If there is only 1 bridge in the simulation, the last bridge is now initiated.
}

if(nBridges>1){
#prepare to build second Bridge
#Prepare germplasm to return from PI to BR1
  if(YEAR==3){
    nToBR1=(100*ReturnRate) #Nr of inds needed to return to BR1
    #Create individuals for BR1 and 2
    if(((i==nCyclesBr)&(i!=1))||((nCyclesBr==1)&(SecondCycle==TRUE))){ #If this is the last cycle of the year, start the next bridge
      PIToBR=selectInd(ProduceExcess[!(ProduceExcess@id %in% ToPD@id)],(50+nToBR1),use='rand') #Create PIToBR. Making sure that inds assigned to ToPD don't also get assigned to PIToBR.
      assign(paste("PIToBR2", i, sep = "_"),PIToBR[1:50]) #Create population of 50 inds to initiate bridge 2.
      assign(paste("PIToBR1", i, sep = "_"),PIToBR[51:nInd(PIToBR)]) #Create population to return to BR1.
      rm(PIToBR) #Remove as no longer needed.
      if(nBridges==2){LASTBR=TRUE} #If there are only 2 bridges in the simulation, the last bridge is now initiated.
    }
    if((exit==TRUE)&(i!=nCyclesBr)){ #When germplasm exchange takes place, but this isn't the last cycle of the year. So never if nCycles==1.
      if(exists("ToPD")==TRUE){assign(paste("PIToBR1", i, sep = "_"),
                                      selectInd(ProduceExcess[!(ProduceExcess@id %in% ToPD@id)],
                                                nToBR1,use='rand'))} #Create PIToBR1. Making sure that inds assigned to ToPD don't also get assigned to PIToBR1.
      else{assign(paste("PIToBR1", i, sep = "_"),selectInd(ProduceExcess,nToBR1,use='rand'))}} #Create PIToBR1.
    rm(nToBR1) #Remove as no longer needed.
  }
}

if(nBridges>2){
#prepare to build third Bridge
#Prepare germplasm to return from PI to BR1 & 2
  if(YEAR==4){
    nToBR1=(100*ReturnRate) #Nr of inds needed to return to BR1
    nToBR2=(100*ReturnRate) #Nr of inds needed to return to BR2
    #create germplasm for BR1,2 & 3
    if(((i==nCyclesBr)&(i!=1))||((nCyclesBr==1)&(SecondCycle==TRUE))){ #if this is the last cycle of the year, start the next bridge
      if(exists("ToPD")==TRUE){PIToBR=selectInd(ProduceExcess[!(ProduceExcess@id %in% ToPD@id)],
                                                (50+nToBR1+nToBR2),use='rand') #Create PIToBR. Making sure that inds assigned to ToPD don't also get assigned to PIToBR.
      }else{PIToBR=selectInd(ProduceExcess,(50+nToBR1+nToBR2),use='rand')} #Create PIToBR.
      assign(paste("PIToBR3", i, sep = "_"),PIToBR[1:50]) #Create population of 50 inds to initiate bridge 3.
      assign(paste("PIToBR1", i, sep = "_"),PIToBR[51:(50+nToBR1)]) #Create population to return to BR1.
      assign(paste("PIToBR2", i, sep = "_"),PIToBR[(50+nToBR1+1):nInd(PIToBR)]) #Create population to return to BR2.
      rm(PIToBR) #Remove as no longer needed.
      if(nBridges==3){LASTBR=TRUE} #If there are only 3 bridges in the simulation, the last bridge is now initiated.
      }
    if((exit==TRUE)&(i!=nCyclesBr)){ #When germplasm exchange takes place, but this isn't the last cycle of the year. So never if nCycles==1.
      if(exists("ToPD")==TRUE){PIToBR=selectInd(ProduceExcess[!(ProduceExcess@id %in% ToPD@id)],
                                                (nToBR1+nToBR2),use='rand')} #Create PIToBR. Making sure that inds assigned to ToPD don't also get assigned to PIToBR.
      else{PIToBR=selectInd(ProduceExcess,(nToBR1+nToBR2),use='rand')} #Create PIToBR.
      assign(paste("PIToBR1", i, sep = "_"),PIToBR[1:nToBR1]) #Create PIToBR1.
      assign(paste("PIToBR2", i, sep = "_"),PIToBR[(nToBR1+1):nInd(PIToBR)]) #Create PIToBR2.
      rm(PIToBR)} #Remove as no longer needed.
    rm(nToBR1) #Remove as no longer needed.
    rm(nToBR2) #Remove as no longer needed.
  }
}

if(nBridges>3){
#prepare to build fourth Bridge
#Prepare germplasm to return to BR1,2&3
  if(YEAR==5){
    nToBR1=(100*ReturnRate) #Nr of inds needed to return to BR1
    nToBR2=(100*ReturnRate) #Nr of inds needed to return to BR2
    nToBR3=(100*ReturnRate) #Nr of inds needed to return to BR3
    #create germplasm for BR1,2,3 & 4
    if(((i==nCyclesBr)&(i!=1))||((nCyclesBr==1)&(SecondCycle==TRUE))){ #if this is the last cycle of the year, start the next bridge
      if(exists("ToPD")==TRUE){PIToBR=selectInd(ProduceExcess[!(ProduceExcess@id %in% ToPD@id)],
                                                (50+nToBR1+nToBR2+nToBR3),use='rand') #Create PIToBR. Making sure that inds assigned to ToPD don't also get assigned to PIToBR.
      }else{PIToBR=selectInd(ProduceExcess,(50+nToBR1+nToBR2+nToBR3),use='rand')} #Create PIToBR.
      assign(paste("PIToBR4", i, sep = "_"),PIToBR[1:50]) #Create population of 50 inds to initiate bridge 4.
      assign(paste("PIToBR1", i, sep = "_"),PIToBR[51:(50+nToBR1)]) #Create population to return to BR1.
      assign(paste("PIToBR2", i, sep = "_"),PIToBR[(50+nToBR1+1):(50+nToBR1+nToBR2)]) #Create population to return to BR2.
      assign(paste("PIToBR3", i, sep = "_"),PIToBR[(50+nToBR1+nToBR2+1):nInd(PIToBR)]) #Create population to return to BR3.
      rm(PIToBR) #Remove as no longer needed.
      if(nBridges==4){LASTBR=TRUE} #If there are only 4 bridges in the simulation, the last bridge is now initiated.
      }
    if((exit==TRUE)&(i!=nCyclesBr)){ #When germplasm exchange takes place, but this isn't the last cycle of the year. So never if nCycles==1.
      if(exists("ToPD")==TRUE){PIToBR=selectInd(ProduceExcess[!(ProduceExcess@id %in% ToPD@id)],
                                                (nToBR1+nToBR2+nToBR3),use='rand')} #Create PIToBR. Making sure that inds assigned to ToPD don't also get assigned to PIToBR.
      else{PIToBR=selectInd(ProduceExcess,(nToBR1+nToBR2+nToBR3),use='rand')} #Create PIToBR.   
      assign(paste("PIToBR1", i, sep = "_"),PIToBR[1:nToBR1]) #Create PIToBR1.
      assign(paste("PIToBR2", i, sep = "_"),PIToBR[(nToBR1+1):(nToBR1+nToBR2)]) #Create PIToBR2.
      assign(paste("PIToBR3", i, sep = "_"),PIToBR[(nToBR1+nToBR2+1):nInd(PIToBR)]) #Create PIToBR3.
      rm(PIToBR)} #Remove as no longer needed.
    rm(nToBR1) #Remove as no longer needed.
    rm(nToBR2) #Remove as no longer needed.
    rm(nToBR3) #Remove as no longer needed.
  }
}

if(nBridges>4){
#prepare to build fifth Bridge
#Prepare germplasm to return to BR1,2,3 & 4
  if(YEAR==6){
    nToBR1=(100*ReturnRate) #Nr of inds needed to return to BR1
    nToBR2=(100*ReturnRate) #Nr of inds needed to return to BR2
    nToBR3=(100*ReturnRate) #Nr of inds needed to return to BR3
    nToBR4=(100*ReturnRate) #Nr of inds needed to return to BR4
    #create germplasm for BR1,2,3,4&5
    if(((i==nCyclesBr)&(i!=1))||((nCyclesBr==1)&(SecondCycle==TRUE))){ #if this is the last cycle of the year, start the next bridge
      if(exists("ToPD")==TRUE){PIToBR=selectInd(ProduceExcess[!(ProduceExcess@id %in% ToPD@id)],
                                                (50+nToBR1+nToBR2+nToBR3+nToBR4),use='rand') #Create PIToBR. Making sure that inds assigned to ToPD don't also get assigned to PIToBR.
      }else{PIToBR=selectInd(ProduceExcess,(50+nToBR1+nToBR2+nToBR3+nToBR4),use='rand')} #Create PIToBR.
      assign(paste("PIToBR5", i, sep = "_"),PIToBR[1:50]) #Create population of 50 inds to initiate bridge 5.
      assign(paste("PIToBR1", i, sep = "_"),PIToBR[51:(50+nToBR1)]) #Create population to return to BR1.
      assign(paste("PIToBR2", i, sep = "_"),PIToBR[(50+nToBR1+1):(50+nToBR1+nToBR2)]) #Create population to return to BR2.
      assign(paste("PIToBR3", i, sep = "_"),PIToBR[(50+nToBR1+nToBR2+1):(50+nToBR1+nToBR2+nToBR3)]) #Create population to return to BR3.
      assign(paste("PIToBR4", i, sep = "_"),PIToBR[(50+nToBR1+nToBR2+nToBR3+1):nInd(PIToBR)]) #Create population to return to BR4.
      if(nBridges==5){LASTBR=TRUE} #If there are only 5 bridges in the simulation, the last bridge is now initiated.
      rm(PIToBR) #Remove as no longer needed.
    }
    if((exit==TRUE)&(i!=nCyclesBr)){ #When germplasm exchange takes place, but this isn't the last cycle of the year. So never if nCycles==1.
      if(exists("ToPD")==TRUE){PIToBR=selectInd(ProduceExcess[!(ProduceExcess@id %in% ToPD@id)],
                                                (nToBR1+nToBR2+nToBR3),use='rand')} #Create PIToBR. Making sure that inds assigned to ToPD don't also get assigned to PIToBR.
      else{PIToBR=selectInd(ProduceExcess,(nToBR1+nToBR2+nToBR3+nToBR4),use='rand')} #Create PIToBR.       
      assign(paste("PIToBR1", i, sep = "_"),PIToBR[1:nToBR1]) #Create PIToBR1.
      assign(paste("PIToBR2", i, sep = "_"),PIToBR[(nToBR1+1):(nToBR1+nToBR2)]) #Create PIToBR2.
      assign(paste("PIToBR3", i, sep = "_"),PIToBR[(nToBR1+nToBR2+1):(nToBR1+nToBR2+nToBR3)]) #Create PIToBR3.
      assign(paste("PIToBR4", i, sep = "_"),PIToBR[(nToBR1+nToBR2+nToBR3+1):nInd(PIToBR)]) #Create PIToBR4.
      rm(PIToBR)} #Remove as no longer needed.
    rm(nToBR1) #Remove as no longer needed.
    rm(nToBR2) #Remove as no longer needed.
    rm(nToBR3) #Remove as no longer needed.
    rm(nToBR4) #Remove as no longer needed.
  }
}

if(nBridges>5){
#prepare to build sixth Bridge 
#Prepare germplasm to return to BR1,2,3,4 & 5
  if(YEAR==7){
    nToBR1=(100*ReturnRate) #Nr of inds needed to return to BR1
    nToBR2=(100*ReturnRate) #Nr of inds needed to return to BR2
    nToBR3=(100*ReturnRate) #Nr of inds needed to return to BR3
    nToBR4=(100*ReturnRate) #Nr of inds needed to return to BR4
    nToBR5=(100*ReturnRate) #Nr of inds needed to return to BR5
    #create germplasm for BR1,2,3,4,5 & 6
    if(((i==nCyclesBr)&(i!=1))||((nCyclesBr==1)&(SecondCycle==TRUE))){ #if this is the last cycle of the year, start the next bridge
      if(exists("ToPD")==TRUE){PIToBR=selectInd(ProduceExcess[!(ProduceExcess@id %in% ToPD@id)],
                                                (50+nToBR1+nToBR2+nToBR3+nToBR4+nToBR5),use='rand') #Create PIToBR. Making sure that inds assigned to ToPD don't also get assigned to PIToBR.
      }else{PIToBR=selectInd(ProduceExcess,(50+nToBR1+nToBR2+nToBR3+nToBR4+nToBR5),use='rand')} #Create PIToBR.
      assign(paste("PIToBR6", i, sep = "_"),PIToBR[1:50]) #Create population of 50 inds to initiate bridge 6.
      assign(paste("PIToBR1", i, sep = "_"),PIToBR[51:(50+nToBR1)]) #Create population to return to BR1.
      assign(paste("PIToBR2", i, sep = "_"),PIToBR[(50+nToBR1+1):(50+nToBR1+nToBR2)]) #Create population to return to BR2.
      assign(paste("PIToBR3", i, sep = "_"),PIToBR[(50+nToBR1+nToBR2+1):(50+nToBR1+nToBR2+nToBR3)]) #Create population to return to BR3.
      assign(paste("PIToBR4", i, sep = "_"),PIToBR[(50+nToBR1+nToBR2+nToBR3+1):(50+nToBR1+nToBR2+nToBR3+nToBR4)]) #Create population to return to BR4.
      assign(paste("PIToBR5", i, sep = "_"),PIToBR[(50+nToBR1+nToBR2+nToBR3+nToBR4+1):nInd(PIToBR)]) #Create population to return to BR5.
      if(nBridges==6){LASTBR=TRUE} #If there are only 6 bridges in the simulation, the last bridge is now initiated.
      rm(PIToBR) #Remove as no longer needed.
    }
    if((exit==TRUE)&(i!=nCyclesBr)){ #When germplasm exchange takes place, but this isn't the last cycle of the year. So never if nCycles==1.
      if(exists("ToPD")==TRUE){PIToBR=selectInd(ProduceExcess[!(ProduceExcess@id %in% ToPD@id)],
                                                (nToBR1+nToBR2+nToBR3),use='rand')} #Create PIToBR. Making sure that inds assigned to ToPD don't also get assigned to PIToBR.
      else{PIToBR=selectInd(ProduceExcess,(nToBR1+nToBR2+nToBR3+nToBR4),use='rand')} #Create PIToBR.
      assign(paste("PIToBR1", i, sep = "_"),PIToBR[1:nToBR1]) #Create PIToBR1.
      assign(paste("PIToBR2", i, sep = "_"),PIToBR[(nToBR1+1):(nToBR1+nToBR2)]) #Create PIToBR2.
      assign(paste("PIToBR3", i, sep = "_"),PIToBR[(nToBR1+nToBR2+1):nInd(nToBR1+nToBR2+nToBR3)]) #Create PIToBR3.
      assign(paste("PIToBR4", i, sep = "_"),PIToBR[(nToBR1+nToBR2+nToBR3+1):(nToBR1+nToBR2+nToBR3+nToBR4)]) #Create PIToBR4.
      assign(paste("PIToBR5", i, sep = "_"),PIToBR[(nToBR1+nToBR2+nToBR3+nToBR4+1):nInd(PIToBR)]) #Create PIToBR5.
      rm(PIToBR)} #Remove as no longer needed.
    rm(nToBR1) #Remove as no longer needed.
    rm(nToBR2) #Remove as no longer needed.
    rm(nToBR3) #Remove as no longer needed.
    rm(nToBR4) #Remove as no longer needed.
    rm(nToBR5) #Remove as no longer needed.
  }
}

if(nBridges>6){
#prepare to build seventh Bridge 
#Prepare germplasm to return to BR1,2,3,4,5 & 6
  if(YEAR==8){
    nToBR1=(100*ReturnRate) #Nr of inds needed to return to BR1
    nToBR2=(100*ReturnRate) #Nr of inds needed to return to BR2
    nToBR3=(100*ReturnRate) #Nr of inds needed to return to BR3
    nToBR4=(100*ReturnRate) #Nr of inds needed to return to BR4
    nToBR5=(100*ReturnRate) #Nr of inds needed to return to BR5
    nToBR6=(100*ReturnRate) #Nr of inds needed to return to BR6
    #create germplasm for BR1,2,3,4,5,6 & 7
    if(((i==nCyclesBr)&(i!=1))||((nCyclesBr==1)&(SecondCycle==TRUE))){ #if this is the last cycle of the year, start the next bridge
      if(exists("ToPD")==TRUE){PIToBR=selectInd(ProduceExcess[!(ProduceExcess@id %in% ToPD@id)],
                                                (50+nToBR1+nToBR2+nToBR3+nToBR4+nToBR5+nToBR6),use='rand') #Create PIToBR. Making sure that inds assigned to ToPD don't also get assigned to PIToBR.
      }else{PIToBR=selectInd(ProduceExcess,(50+nToBR1+nToBR2+nToBR3+nToBR4+nToBR5+nToBR6),use='rand')} #Create PIToBR.
      assign(paste("PIToBR7", i, sep = "_"),PIToBR[1:50]) #Create population of 50 inds to initiate bridge 7.
      assign(paste("PIToBR1", i, sep = "_"),PIToBR[51:(50+nToBR1)]) #Create population to return to BR1.
      assign(paste("PIToBR2", i, sep = "_"),PIToBR[(50+nToBR1+1):(50+nToBR1+nToBR2)]) #Create population to return to BR2.
      assign(paste("PIToBR3", i, sep = "_"),PIToBR[(50+nToBR1+nToBR2+1):(50+nToBR1+nToBR2+nToBR3)]) #Create population to return to BR3.
      assign(paste("PIToBR4", i, sep = "_"),PIToBR[(50+nToBR1+nToBR2+nToBR3+1):(50+nToBR1+nToBR2+nToBR3+nToBR4)]) #Create population to return to BR4.
      assign(paste("PIToBR5", i, sep = "_"),PIToBR[(50+nToBR1+nToBR2+nToBR3+nToBR4+1):(50+nToBR1+nToBR2+nToBR3+nToBR4+nToBR5)]) #Create population to return to BR5.
      assign(paste("PIToBR6", i, sep = "_"),PIToBR[(50+nToBR1+nToBR2+nToBR3+nToBR4+nToBR5+1):nInd(PIToBR)]) #Create population to return to BR6.
      if(nBridges==7){LASTBR=TRUE} #If there are only 7 bridges in the simulation, the last bridge is now initiated.
      rm(PIToBR) #Remove as no longer needed.
    }
    if((exit==TRUE)&(i!=nCyclesBr)){ #When germplasm exchange takes place, but this isn't the last cycle of the year. So never if nCycles==1.
      if(exists("ToPD")==TRUE){PIToBR=selectInd(ProduceExcess[!(ProduceExcess@id %in% ToPD@id)],
                                                (nToBR1+nToBR2+nToBR3),use='rand')} #Create PIToBR. Making sure that inds assigned to ToPD don't also get assigned to PIToBR.
      else{PIToBR=selectInd(ProduceExcess,(nToBR1+nToBR2+nToBR3+nToBR4),use='rand')} #Create PIToBR.
      assign(paste("PIToBR1", i, sep = "_"),PIToBR[1:nToBR1]) #Create PIToBR1.
      assign(paste("PIToBR2", i, sep = "_"),PIToBR[(nToBR1+1):(nToBR1+nToBR2)]) #Create PIToBR2.
      assign(paste("PIToBR3", i, sep = "_"),PIToBR[(nToBR1+nToBR2+1):nInd(nToBR1+nToBR2+nToBR3)]) #Create PIToBR3.
      assign(paste("PIToBR4", i, sep = "_"),PIToBR[(nToBR1+nToBR2+nToBR3+1):(nToBR1+nToBR2+nToBR3+nToBR4)]) #Create PIToBR4.
      assign(paste("PIToBR5", i, sep = "_"),PIToBR[(nToBR1+nToBR2+nToBR3+nToBR4+1):(nToBR1+nToBR2+nToBR3+nToBR4+nToBR5)]) #Create PIToBR5.
      assign(paste("PIToBR6", i, sep = "_"),PIToBR[(nToBR1+nToBR2+nToBR3+nToBR4+nToBR5+1):nInd(PIToBR)]) #Create PIToBR6.
      rm(PIToBR)} #Remove as no longer needed.
    rm(nToBR1) #Remove as no longer needed.
    rm(nToBR2) #Remove as no longer needed.
    rm(nToBR3) #Remove as no longer needed.
    rm(nToBR4) #Remove as no longer needed.
    rm(nToBR5) #Remove as no longer needed.
    rm(nToBR6) #Remove as no longer needed.
  }
}
