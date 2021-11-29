#Not in use for manuscript
if(InitBR6==FALSE){
  NextBR6=NextBR6[sample.int(nInd(NextBR6))]
  #randomly divide in males and females
  if(exists(paste("BR5ToBR6", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))){
    Females=NextBR6[1:(50-nInd(get(paste("BR5ToBR6", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))))]
  }else{Females=NextBR6[1:50]}
  Males=NextBR6[(nInd(Females)+1):nInd(NextBR6)]
  #Introgression on females and males
  if(exists(paste("BR5ToBR6", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))){
    Females=mergePops(list(Females,get(paste("BR5ToBR6", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))))
    rm(list=paste("BR5ToBR6", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))}
  if(exists(paste("PIToBR6", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))){ 
    Males=mergePops(list(Males,get(paste("PIToBR6", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))))
    rm(list=paste("PIToBR6", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))}
  #print(paste("BR6","nMales",nInd(Males),"nFemales",nInd(Females)))
}  
if(InitBR6==TRUE){
  Females=get(paste("BR5ToBR6", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))
  Males=get(paste("PIToBR6", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))
  rm(list=paste("PIToBR6", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))
  rm(list=paste("BR5ToBR6", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))
  InitBR6=FALSE
  #print(paste("BR6","nMales",nInd(Males),"nFemales",nInd(Females)))
}  

#Produce inds for next cycle and to pass on
ProduceCross=randCross2(Females,Males,BRIntensity) #creates max inds that can be genotyped   
rm(Females)
rm(Males)
ProduceCross=setPheno(ProduceCross,varE=setAcc(ProduceCross,chosenAcc))

#Numbers needed for next part of year
#For next BR6
if(NextReceives==TRUE){
  nNextBR6=100-100*ReturnRate-100*BridgingRate
}else{nNextBR6=100}

#To pass on to next BR
if(exit==TRUE){
  if(InitBR7==TRUE){
    if(i==nCyclesBr){nToNextBR=50    ##This starts off the next BR   
    }else if(BR7Initiated==TRUE){  ##If preparations for starting off bridge have taken place, prepare germplasm for next cycle
      if(nBridges>6){nToNextBR=100*BridgingRate}else{nToNextBR=100*IntrogressionRate}
    }else{nToNextBR=0}  ##If next br is not initiated yet
  }else if(nBridges>6){nToNextBR=100*BridgingRate
  }else{nToNextBR=100*IntrogressionRate} ##If br is established
}else{nToNextBR=0} #when exit is FALSE

#Total needed
nSelect=nNextBR6+nToNextBR

#select best individuals
BestProduceCross=selectInd(ProduceCross,nSelect,use="pheno",selectTop=TRUE)
rm(ProduceCross)

#randomise as now randomly split between going back into the bridge and passing on to the next
BestProduceCross=BestProduceCross[sample.int(nInd(BestProduceCross))]
NextBR6=BestProduceCross[1:nNextBR6]
if((exit==TRUE)&(InitBR7==TRUE)&(i==nCyclesBr)){ #this avoids inds to be created before initBR2 is set up at the end of the year
  if(nBridges>6){assign(paste("BR6ToBR7", i, sep = "_"),BestProduceCross[(nNextBR6+1):nSelect])
  }else{assign(paste("ToPI", i, sep = "_"),BestProduceCross[(nNextBR6+1):nSelect])}
  BR7Initiated=TRUE
}else if((exit==TRUE)&(BR7Initiated==TRUE)){
  if(nBridges>6){assign(paste("BR6ToBR7", i, sep = "_"),BestProduceCross[(nNextBR6+1):nSelect])
  }else{assign(paste("ToPI", i, sep = "_"),BestProduceCross[(nNextBR6+1):nSelect])}
}else if((exit==TRUE)&(InitBR7==FALSE)){
  if(nBridges>6){assign(paste("BR6ToBR7", i, sep = "_"),BestProduceCross[(nNextBR6+1):nSelect])
  }else if(nToNextBR>0){assign(paste("ToPI", i, sep = "_"),BestProduceCross[(nNextBR6+1):nSelect])
  }else if(nToNextBR==0){assign(paste("ToPI", i, sep = "_"),selectInd(BestProduceCross,0))}
}

#
#
#clean up
rm(nNextBR6)
rm(nSelect)
rm(nToNextBR)
rm(BestProduceCross)
