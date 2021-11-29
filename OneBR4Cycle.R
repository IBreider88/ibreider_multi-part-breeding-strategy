#Not in use for manuscript
if(InitBR4==FALSE){
  NextBR4=NextBR4[sample.int(nInd(NextBR4))]
  #randomly divide in males and females
  if(exists(paste("BR3ToBR4", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))){
    Females=NextBR4[1:(50-nInd(get(paste("BR3ToBR4", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))))]
  }else{Females=NextBR4[1:50]}
  Males=NextBR4[(nInd(Females)+1):nInd(NextBR4)]
  #Introgression on females and males
  if(exists(paste("BR3ToBR4", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))){
    Females=mergePops(list(Females,get(paste("BR3ToBR4", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))))
    rm(list=paste("BR3ToBR4", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))}
  if(exists(paste("PIToBR4", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))){ 
    Males=mergePops(list(Males,get(paste("PIToBR4", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))))
    rm(list=paste("PIToBR4", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))}
  #print(paste("BR4","nMales",nInd(Males),"nFemales",nInd(Females)))
}  
if(InitBR4==TRUE){
  Females=get(paste("BR3ToBR4", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))
  Males=get(paste("PIToBR4", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))
  rm(list=paste("PIToBR4", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))
  rm(list=paste("BR3ToBR4", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))
  InitBR4=FALSE
  #print(paste("BR4","nMales",nInd(Males),"nFemales",nInd(Females)))
}  

#Produce inds for next cycle and to pass on
ProduceCross=randCross2(Females,Males,BRIntensity) #creates max inds that can be genotyped   
rm(Females)
rm(Males)
ProduceCross=setPheno(ProduceCross,varE=setAcc(ProduceCross,chosenAcc))

#Numbers needed for next part of year
#For next BR4
if(NextReceives==TRUE){
  nNextBR4=100-100*ReturnRate-100*BridgingRate
}else{nNextBR4=100}

#To pass on to next BR
if(exit==TRUE){
  if(InitBR5==TRUE){
    if(i==nCyclesBr){nToNextBR=50    ##This starts off the next BR   
    }else if(BR5Initiated==TRUE){  ##If preparations for starting off bridge have taken place, prepare germplasm for next cycle
      if(nBridges>4){nToNextBR=100*BridgingRate}else{nToNextBR=100*IntrogressionRate}
    }else{nToNextBR=0}  ##If next br is not initiated yet
  }else if(nBridges>4){nToNextBR=100*BridgingRate
  }else{nToNextBR=100*IntrogressionRate} ##If br is established
}else{nToNextBR=0} #when exit is FALSE

#Total needed
nSelect=nNextBR4+nToNextBR

#select best individuals
BestProduceCross=selectInd(ProduceCross,nSelect,use="pheno",selectTop=TRUE)
rm(ProduceCross)

#randomise as now randomly split between going back into the bridge and passing on to the next
BestProduceCross=BestProduceCross[sample.int(nInd(BestProduceCross))]
NextBR4=BestProduceCross[1:nNextBR4]
if((exit==TRUE)&(InitBR5==TRUE)&(i==nCyclesBr)){ #this avoids inds to be created before initBR2 is set up at the end of the year
  if(nBridges>4){assign(paste("BR4ToBR5", i, sep = "_"),BestProduceCross[(nNextBR4+1):nSelect])
  }else{assign(paste("ToPI", i, sep = "_"),BestProduceCross[(nNextBR4+1):nSelect])}
  BR5Initiated=TRUE
}else if((exit==TRUE)&(BR5Initiated==TRUE)){
  if(nBridges>4){assign(paste("BR4ToBR5", i, sep = "_"),BestProduceCross[(nNextBR4+1):nSelect])
  }else{assign(paste("ToPI", i, sep = "_"),BestProduceCross[(nNextBR4+1):nSelect])}
}else if((exit==TRUE)&(InitBR5==FALSE)){
  if(nBridges>4){assign(paste("BR4ToBR5", i, sep = "_"),BestProduceCross[(nNextBR4+1):nSelect])
  }else if(nToNextBR>0){assign(paste("ToPI", i, sep = "_"),BestProduceCross[(nNextBR4+1):nSelect])
  }else if(nToNextBR==0){assign(paste("ToPI", i, sep = "_"),selectInd(BestProduceCross,0))}
}

#
#
#clean up
rm(nNextBR4)
rm(nSelect)
rm(nToNextBR)
rm(BestProduceCross)