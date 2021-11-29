#Not in use for manuscript
if(InitBR7==FALSE){
  NextBR7=NextBR7[sample.int(nInd(NextBR7))]
  #randomly divide in males and females
  if(exists(paste("BR6ToBR7", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))){
    Females=NextBR7[1:(50-nInd(get(paste("BR6ToBR7", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))))]
  }else{Females=NextBR7[1:50]}
  Males=NextBR7[(nInd(Females)+1):nInd(NextBR7)]
  #Introgression on females and males
  if(exists(paste("BR6ToBR7", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))){
    Females=mergePops(list(Females,get(paste("BR6ToBR7", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))))
    rm(list=paste("BR6ToBR7", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))}
  if(exists(paste("PIToBR7", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))){ 
    Males=mergePops(list(Males,get(paste("PIToBR7", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))))
    rm(list=paste("PIToBR7", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))}
  #print(paste("BR7","nMales",nInd(Males),"nFemales",nInd(Females)))
}  
if(InitBR7==TRUE){
  Females=get(paste("BR6ToBR7", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))
  Males=get(paste("PIToBR7", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))
  rm(list=paste("PIToBR7", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))
  rm(list=paste("BR6ToBR7", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))
  InitBR7=FALSE
  #print(paste("BR7","nMales",nInd(Males),"nFemales",nInd(Females)))
}  

#Produce inds for next cycle and to pass on
ProduceCross=randCross2(Females,Males,BRIntensity) #creates max inds that can be genotyped   
rm(Females)
rm(Males)
ProduceCross=setPheno(ProduceCross,varE=setAcc(ProduceCross,chosenAcc))

#Numbers needed for next part of year
#For next BR7
if(NextReceives==TRUE){
  nNextBR7=100-100*ReturnRate-100*BridgingRate
}else{nNextBR7=100}

#To pass on to next BR
if(exit==TRUE){nToNextBR=100*IntrogressionRate}else{nToNextBR=0}

#Total needed
nSelect=nNextBR7+nToNextBR

#select best individuals
BestProduceCross=selectInd(ProduceCross,nSelect,use="pheno",selectTop=TRUE)
rm(ProduceCross)

#randomise as now randomly split between going back into the bridge and passing on to the next
BestProduceCross=BestProduceCross[sample.int(nInd(BestProduceCross))]
NextBR7=BestProduceCross[1:nNextBR7]
if(exit==TRUE){assign(paste("ToPI", i, sep = "_"),BestProduceCross[(nNextBR7+1):nSelect])}

#
#
#clean up
rm(nNextBR7)
rm(nSelect)
rm(nToNextBR)
rm(BestProduceCross)
