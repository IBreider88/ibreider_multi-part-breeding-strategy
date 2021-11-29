#Not in use for manuscript
if(InitBR5==FALSE){
  NextBR5=NextBR5[sample.int(nInd(NextBR5))]
  #randomly divide in males and females
  if(exists(paste("BR4ToBR5", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))){
    Females=NextBR5[1:(50-nInd(get(paste("BR4ToBR5", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))))]
  }else{Females=NextBR5[1:50]}
  Males=NextBR5[(nInd(Females)+1):nInd(NextBR5)]
  #Introgression on females and males
  if(exists(paste("BR4ToBR5", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))){
    Females=mergePops(list(Females,get(paste("BR4ToBR5", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))))
    rm(list=paste("BR4ToBR5", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))}
  if(exists(paste("PIToBR5", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))){ 
    Males=mergePops(list(Males,get(paste("PIToBR5", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))))
    rm(list=paste("PIToBR5", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))}
  #print(paste("BR5","nMales",nInd(Males),"nFemales",nInd(Females)))
}  
if(InitBR5==TRUE){
  Females=get(paste("BR4ToBR5", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))
  Males=get(paste("PIToBR5", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))
  rm(list=paste("PIToBR5", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))
  rm(list=paste("BR4ToBR5", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))
  InitBR5=FALSE
  #print(paste("BR5","nMales",nInd(Males),"nFemales",nInd(Females)))
}  

#Produce inds for next cycle and to pass on
ProduceCross=randCross2(Females,Males,BRIntensity) #creates max inds that can be genotyped   
rm(Females)
rm(Males)
ProduceCross=setPheno(ProduceCross,varE=setAcc(ProduceCross,chosenAcc))

#Numbers needed for next part of year
#For next BR5
if(NextReceives==TRUE){
  nNextBR5=100-100*ReturnRate-100*BridgingRate
}else{nNextBR5=100}

#To pass on to next BR
if(exit==TRUE){
  if(InitBR6==TRUE){
    if(i==nCyclesBr){nToNextBR=50    ##This starts off the next BR   
    }else if(BR6Initiated==TRUE){  ##If preparations for starting off bridge have taken place, prepare germplasm for next cycle
      if(nBridges>5){nToNextBR=100*BridgingRate}else{nToNextBR=100*IntrogressionRate}
    }else{nToNextBR=0}  ##If next br is not initiated yet
  }else if(nBridges>5){nToNextBR=100*BridgingRate
  }else{nToNextBR=100*IntrogressionRate} ##If br is established
}else{nToNextBR=0} #when exit is FALSE

#Total needed
nSelect=nNextBR5+nToNextBR

#select best individuals
BestProduceCross=selectInd(ProduceCross,nSelect,use="pheno",selectTop=TRUE)
rm(ProduceCross)

#randomise as now randomly split between going back into the bridge and passing on to the next
BestProduceCross=BestProduceCross[sample.int(nInd(BestProduceCross))]
NextBR5=BestProduceCross[1:nNextBR5]
if((exit==TRUE)&(InitBR6==TRUE)&(i==nCyclesBr)){ #this avoids inds to be created before initBR2 is set up at the end of the year
  if(nBridges>5){assign(paste("BR5ToBR6", i, sep = "_"),BestProduceCross[(nNextBR5+1):nSelect])
  }else{assign(paste("ToPI", i, sep = "_"),BestProduceCross[(nNextBR5+1):nSelect])}
  BR6Initiated=TRUE
}else if((exit==TRUE)&(BR6Initiated==TRUE)){
  if(nBridges>5){assign(paste("BR5ToBR6", i, sep = "_"),BestProduceCross[(nNextBR5+1):nSelect])
  }else{assign(paste("ToPI", i, sep = "_"),BestProduceCross[(nNextBR5+1):nSelect])}
}else if((exit==TRUE)&(InitBR6==FALSE)){
  if(nBridges>5){assign(paste("BR5ToBR6", i, sep = "_"),BestProduceCross[(nNextBR5+1):nSelect])
  }else if(nToNextBR>0){assign(paste("ToPI", i, sep = "_"),BestProduceCross[(nNextBR5+1):nSelect])
  }else if(nToNextBR==0){assign(paste("ToPI", i, sep = "_"),selectInd(BestProduceCross,0))}
}

#
#
#clean up
rm(nNextBR5)
rm(nSelect)
rm(nToNextBR)
rm(BestProduceCross)