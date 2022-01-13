#Breider et al. 2022 DOI: 10.21203/rs.3.rs-1246254/v1

JOB = Sys.getenv("SGE_TASK_ID") #JOBREP 1 to 10, make sure right createdParents file is read in

library(AlphaSimR)
load(paste0("CreatedParentsExPVPs",JOB,".rda")) #Read in created parents

nBurninYears = 50 #Years of burn-in breeding 

#Matrices to save output
BurninGenGainDH = matrix(0,nrow=nBurninYears+1,ncol=1)
BurninGenicVarDH = matrix(0,nrow=nBurninYears+1,ncol=1)
BurninGenGainEYT1 = matrix(0,nrow=nBurninYears+1,ncol=1)
BurninGenicVarEYT1 = matrix(0,nrow=nBurninYears+1,ncol=1)

#Function to reset individual IDs for tracking purposes.
formFounderPop = function(pop,simParam=NULL){
  if(is.null(simParam)){
    simParam = get("SP",envir=.GlobalEnv)
  }
  return(new("MapPop",
             nInd=pop@nInd,
             nChr=pop@nChr,
             ploidy=pop@ploidy,
             nLoci=simParam$segSites,
             geno=pop@geno,
             genMap=simParam$genMap,
             centromere=simParam$centromere))
}

EliteFounder = formFounderPop(EliteParents) #Reset IDs for tracking purposes
EliteParents = newPop(EliteFounder) #Reset IDs for tracking purposes

#Fill pipeline.This sets up the breeding program from scratch.
for(year in 1:7){
  F1 = randCross(EliteParents,100) #F1 is created from 100 random crosses between elite parents.
  if(year<7){
    DH = makeDH(F1,nDH=62) #62 DH are created from each F1 (100 inds), total 6200 DHs.   
  }
  if(year<6){
    HDRW = setPheno(DH,varE=SP$varE*5) #Phenotype is set for HDRW, varE is set to 1/5th of a repeat.
  }
  if(year<5){
    PYT = selectWithinFam(HDRW,5) #This year's PYT is selected from last year's HDRW. First 5 individuals are selected per female,
    PYT = selectInd(PYT,500) #then the 500 best performing individuals are selected
    PYT = setPheno(PYT) #Phenotype is set for PYT
  }
  if(year<4){
    AYT = selectInd(PYT,50) #This year's AYT is selected from last year's PYT.
    AYT = setPheno(AYT,varE=SP$varE/4) #Phenotype is set for this year's EYT, varE is set with 4 locations in mind
  }
  if(year<3){
    EYT1 = selectInd(AYT,10) #This year's EYT1 is selected from last year's AYT
    EYT1 = setPheno(EYT1,varE=SP$varE/8) #Phenotype is set for this year's EYT, varE is set with 8 locations in mind
  }
  if(year<2){
    EYT2 = EYT1 #Last year's EYT1 becomes this year's EYT2
    EYT2 = setPheno(EYT2,varE=SP$varE/8) #Phenotype is set for this year's EYT, varE is set with 8 locations in mind
  }
}

for(YEAR in 1:nBurninYears){
  #Advance 1 year
  EYT2 = EYT1 #Last year's EYT1 becomes this year's EYT2
  EYT2 = setPheno(EYT2,varE=SP$varE/8)  #Phenotype is set for this year's EYT, varE is set with 8 locations in mind
  EYT1 = selectInd(AYT,10) #This year's EYT1 is selected from last year's AYT
  EYT1 = setPheno(EYT1,varE=SP$varE/8) #Phenotype is set for this year's EYT, varE is set with 8 locations in mind
  AYT = selectInd(PYT,50) #This year's AYT is selected from last year's PYT.
  AYT = setPheno(AYT,varE=SP$varE/4) #Phenotype is set for this year's EYT, varE is set with 4 locations in mind
  PYT = selectWithinFam(HDRW,5) #This year's PYT is selected from last year's HDRW. First 5 individuals are selected per female,
  PYT = selectInd(PYT,500) #then the 500 best performing individuals are selected
  PYT = setPheno(PYT) #Phenotype is set for PYT
  HDRW = setPheno(DH,varE=SP$varE*5) #Phenotype is set for HDRW, varE is set to 1/5th of a repeat.
  DH = makeDH(F1,nDH=62) #62 DH are created from each F1 (100 inds), total 6200 DHs. 
  F1 = randCross(mergePops(list(AYT,EYT1,EYT2)),100) #F1 is created from 100 random crosses between elite parents.
  #Saving results
  BurninGenGainDH[YEAR+1,1] = meanG(DH)  
  BurninGenicVarDH[YEAR+1,1] = varG(DH) 
  BurninGenGainEYT1[YEAR+1,1] = meanG(EYT1) 
  BurninGenicVarEYT1[YEAR+1,1] = varG(EYT1) 
}
interceptDH = meanG(DH) #Saving intercept at end of burn-in
interceptEYT1 = meanG(EYT1) #Saving intercept at end of burn-in

Start2Part = c(selectInd(PYT,430), #Creating population used as start of PI
               AYT,
               EYT1,
               EYT2)

rm(EliteParents) #Remove as no longer needed.
rm(ExoticParents) #Remove as no longer needed.
save.image(paste0("BurnInEliteExPVPs",JOB,".rda")) #Save burn-in.
