#Breider et al. 2022 DOI: 10.21203/rs.3.rs-1246254/v1
#
#SnapShots taken for further analyses
if(YEAR==1|YEAR==2|YEAR==3|YEAR==4|YEAR==5|YEAR==6|YEAR==7|YEAR==8|YEAR==9|YEAR==10|YEAR==20|YEAR==30|YEAR==40|YEAR==50|YEAR==60|YEAR==70|YEAR==80|YEAR==90|YEAR==100){assign(paste("StartofPI_SnapShot_Year", YEAR, sep = "_"),mergePops(list(ProduceFemales,ProduceMales)))}
#Start next cycle by choosing 100 males and 100 females
if(LASTBR==FALSE){ #Last bridge has not been initiated yet, so no introgression to PI takes place.
  Females=selectWithinFam(ProduceFemales,1,use="pheno",selectTop = TRUE,famType="F") #Select 50 females, max 1 per family.
  Males=selectWithinFam(ProduceMales,1,use="pheno",selectTop = TRUE,famType="F") #Select 50 males, max 1 per family.
  print(paste("PI","nMales",nInd(Males),"nFemales",nInd(Females))) #Sanity check to check nr of parents.
  #SnapShots taken for further analyses
  if(YEAR==1|YEAR==2|YEAR==3|YEAR==4|YEAR==5|YEAR==6|YEAR==7|YEAR==8|YEAR==9|YEAR==10|YEAR==20|YEAR==30|YEAR==40|YEAR==50|YEAR==60|YEAR==70|YEAR==80|YEAR==90|YEAR==100){assign(paste("SelectedPI_SnapShot_Year", YEAR, sep = "_"),mergePops(list(Females,Males)))}
  }
if(LASTBR==TRUE){ #Last bridge has been initiated, introgression to PI can take place.
  Females=selectWithinFam(ProduceFemales,1,use="pheno",selectTop = TRUE,famType="F") #Select 50 females (max 1 per family), as introgression only takes place on male population.
  if(exists(paste("ToPI", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))==TRUE){ #If ToPI exists (at start of each new year, from start of introgression onwards).
    Males=selectInd(selectWithinFam(ProduceMales,1,use="pheno",selectTop = TRUE,famType="F"), #Select the best male parents. 50-introgression rate. As setPheno is based on chosenAcc, this reflects GS.
                  (ParentsPI-100*IntrogressionRate),selectTop=TRUE,use="pheno")
    print(paste("ToPI",if(i-1>0){i-1}else{nCyclesBr}, sep = "_",nInd(get(paste("ToPI", if(i-1>0){i-1}else{nCyclesBr}, sep = "_"))))) #Sanity check, check nr of inds
    print(paste("nMales before merge",nInd(Males))) #Sanity check
    Males=mergePops(list(Males,get(paste("ToPI", if(i-1>0){i-1}else{nCyclesBr}, sep = "_")))) #Merge males from ToPI with males from previous PI cycle.
    rm(list=paste("ToPI", if(i-1>0){i-1}else{nCyclesBr}, sep = "_")) #Remove to PI as no longer needed
  }else{
    Males=selectWithinFam(ProduceMales,1,use="pheno",selectTop = TRUE,famType="F") #When the last bridge is initiated, but no introgression takes place in the current breeding cycle.
  }
  print(paste("PI","nMales",nInd(Males),"nFemales",nInd(Females))) #Sanity check, check nr of inds
  #SnapShots taken for further analyses
  if(YEAR==1|YEAR==2|YEAR==3|YEAR==4|YEAR==5|YEAR==6|YEAR==7|YEAR==8|YEAR==9|YEAR==10|YEAR==20|YEAR==30|YEAR==40|YEAR==50|YEAR==60|YEAR==70|YEAR==80|YEAR==90|YEAR==100){assign(paste("SelectedPI_SnapShot_Year", YEAR, sep = "_"),mergePops(list(Females,Males)))}
}

#Make crosses to produce next parent populations
ProduceMales=randCross2(Females,Males,(PIIntensity/2)) #Create new male parents
ProduceFemales=randCross2(Females,Males,(PIIntensity/2)) #Create new female parents
ProduceMales=setPheno(ProduceMales,varE=setAcc(ProduceMales,chosenAcc)) #Set phenotype based on chosenAcc to model GS
ProduceFemales=setPheno(ProduceFemales,varE=setAcc(ProduceFemales,chosenAcc)) #Set phenotype based on chosenAcc to model GS
#Produce inds for exchange with other components (ToPD, PIToBR), these individuals are not genotyped (there is no budget to do so)! Therefore, these inds can only be randomly assigned to other populations. 
ProduceExcess=randCross2(Females,Males,1000) #As these are not genotyped, the number of crosses is arbitrary.
rm(Females) #Remove as no longer needed.
rm(Males) #Remove as no longer needed.

#If this is the last PI cycle of the year, produce ToPD for next year
if(nCyclesBr==1){ #This means ToPD only gets created at last cycle of PI
  if(SecondCycle==TRUE){
  ToPD=selectInd(ProduceExcess,200,use="rand")} #use is "rand" as not genotyped
}else if(i==nCyclesBr){ #This means ToPD only gets created at last cycle of PI. If nCyclesBr==1 this else if isn't used.
  ToPD=selectInd(ProduceExcess,200,use="rand")} #use is "rand" as not genotyped


#When all bridges exist
if(LASTBR==TRUE){
  #Prepares germplasm to go into next bridging cycles
  #create germplasm for BR1,2,3,4,5,6 & 7
  if(((nCyclesBr==1)&(SecondCycle==TRUE))||(exit==TRUE)){
    nToBR=(100*ReturnRate)*nBridges #nInds to return to BR, total over all BR.
    if(exists("ToPD")==TRUE){ PIToBR=selectInd(ProduceExcess[!(ProduceExcess@id %in% ToPD@id)],
                                              nToBR,use='rand')} #Making sure that inds assigned to ToPD don't also get assigned to PIToBR.
    else{PIToBR=selectInd(ProduceExcess,nToBR,use='rand')} #If ToPD doesn't exist, inds can be assigned without restrictions.
    assign(paste("PIToBR1", i, sep = "_"),PIToBR[1:(nToBR/nBridges)]) #Create pop to go back into BR1
    if(nBridges>1){assign(paste("PIToBR2", i, sep = "_"),PIToBR[(nToBR/nBridges+1):(nToBR/nBridges*2)])} #Create pop to go back into BR2
    if(nBridges>2){assign(paste("PIToBR3", i, sep = "_"),PIToBR[(nToBR/nBridges*2+1):(nToBR/nBridges*3)])} #Create pop to go back into BR3
    if(nBridges>3){assign(paste("PIToBR4", i, sep = "_"),PIToBR[(nToBR/nBridges*3+1):(nToBR/nBridges*4)])} #Create pop to go back into BR4
    if(nBridges>4){assign(paste("PIToBR5", i, sep = "_"),PIToBR[(nToBR/nBridges*4+1):(nToBR/nBridges*5)])} #Create pop to go back into BR5
    if(nBridges>5){assign(paste("PIToBR6", i, sep = "_"),PIToBR[(nToBR/nBridges*5+1):(nToBR/nBridges*6)])} #Create pop to go back into BR6
    if(nBridges>6){assign(paste("PIToBR7", i, sep = "_"),PIToBR[(nToBR/nBridges*6+1):(nToBR/nBridges*7)])} #Create pop to go back into BR7
    rm(nToBR) #Remove as no longer needed
    rm(PIToBR)} #Remove as no longer needed
}

#Initiate bridges until all exist
#Prepares germplasm to go into next bridging cycles. Next bridge can only be initiated after this germplasm is created, therefore this is part of PI cycle.
if((LASTBR==FALSE)&(IntrogressionRate>0)){ #Last bridge has not been initiated yet. Introgression takes place in scenario.
  if(nCyclesBr>1){SecondCycle=FALSE} #Just to reset counter. If nCyclesBr>1, second cycle isn't needed as PI cycles at same speed as bridges.
  source("InitiateBridges.R")} #Calls sub-script to initiate bridges.


