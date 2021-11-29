JOB = Sys.getenv("SGE_TASK_ID")
library(AlphaSimR)
load(paste0("CreatedParentsExPVPs",JOB,".rda"))

nBurninYears = 30 

#Matrices to save output
BurninGenGainDHex2 = matrix(0,nrow=nBurninYears+1,ncol=1)
BurninGenicVarDHex2 = matrix(0,nrow=nBurninYears+1,ncol=1)
BurninGenGainEYT1ex2 = matrix(0,nrow=nBurninYears+1,ncol=1)
BurninGenicVarEYT1ex2 = matrix(0,nrow=nBurninYears+1,ncol=1)

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

ExoticFounder = formFounderPop(ExoticParents) #Reset IDs for tracking purposes
ExoticParents = newPop(ExoticFounder) #Reset IDs for tracking purposes

#Fill pipeline.This sets up the breeding program from scratch.
for(year in 1:7){
  F1ex2 = randCross(ExoticParents,100) #F1 is created from 100 random crosses between elite parents.
  if(year<7){
    DHex2 = makeDH(F1ex2,nDH=62) #62 DH are created from each F1 (100 inds), total 6200 DHs.   
  }
  if(year<6){
    HDRWex2 = setPheno(DHex2,varE=SP$varE*5) #Phenotype is set for HDRW, varE is set to 1/5th of a repeat.
  }
  if(year<5){
    PYTex2 = selectWithinFam(HDRWex2,5) #This year's PYT is selected from last year's HDRW. First 5 individuals are selected per female,
    PYTex2 = selectInd(PYTex2,500) #then the 500 best performing individuals are selected
    PYTex2 = setPheno(PYTex2) #Phenotype is set for PYT
  }
  if(year<4){
    AYTex2 = selectInd(PYTex2,50) #This year's AYT is selected from last year's PYT.
    AYTex2 = setPheno(AYTex2,varE=SP$varE/4) #Phenotype is set for this year's EYT, varE is set with 4 locations in mind
  }
  if(year<3){
    EYT1ex2 = selectInd(AYTex2,10) #This year's EYT1 is selected from last year's AYT
    EYT1ex2 = setPheno(EYT1ex2,varE=SP$varE/8) #Phenotype is set for this year's EYT, varE is set with 8 locations in mind
  }
  if(year<2){
    EYT2ex2 = EYT1ex2 #Last year's EYT1 becomes this year's EYT2
    EYT2ex2 = setPheno(EYT2ex2,varE=SP$varE/8) #Phenotype is set for this year's EYT, varE is set with 8 locations in mind
  }
}

for(YEAR in 1:nBurninYears){
  #Advance 1 year
  VARex2 = selectInd(EYT2ex2,2) #Two varieties are produced
  EYT2ex2 = EYT1ex2 #Last year's EYT1 becomes this year's EYT2
  EYT2ex2 = setPheno(EYT2ex2,varE=SP$varE/8) #Phenotype is set for this year's EYT, varE is set with 8 locations in mind
  EYT1ex2 = selectInd(AYTex2,10) #This year's EYT1 is selected from last year's AYT
  EYT1ex2 = setPheno(EYT1ex2,varE=SP$varE/8) #Phenotype is set for this year's EYT, varE is set with 8 locations in mind
  AYTex2 = selectInd(PYTex2,50) #This year's AYT is selected from last year's PYT.
  AYTex2 = setPheno(AYTex2,varE=SP$varE/4) #Phenotype is set for this year's EYT, varE is set with 4 locations in mind
  PYTex2 = selectWithinFam(HDRWex2,5) #This year's PYT is selected from last year's HDRW. First 5 individuals are selected per female,
  PYTex2 = selectInd(PYTex2,500) #then the 500 best performing individuals are selected
  PYTex2 = setPheno(PYTex2) #Phenotype is set for PYT
  HDRWex2 = setPheno(DHex2,varE=SP$varE*5) #Phenotype is set for HDRW, varE is set to 1/5th of a repeat.
  DHex2 = makeDH(F1ex2,nDH=62) #62 DH are created from each F1 (100 inds), total 6200 DHs. 
  F1ex2 = randCross(mergePops(list(AYTex2,EYT1ex2,EYT2ex2)),100) #F1 is created from 100 random crosses between elite parents.
  #Saving results
  BurninGenGainDHex2[YEAR+1,1] = meanG(DHex2)
  BurninGenicVarDHex2[YEAR+1,1] = varG(DHex2)
  BurninGenGainEYT1ex2[YEAR+1,1] = meanG(EYT1ex2)
  BurninGenicVarEYT1ex2[YEAR+1,1] = varG(EYT1ex2)
}
interceptDHex2 = meanG(DHex2) #Saving intercept at end of burn-in
interceptEYT1ex2 = meanG(EYT1ex2) #Saving intercept at end of burn-in

rm(ExoticParents) #Remove as no longer needed.
rm(EliteParents) #Remove as no longer needed.
save.image(paste0("BurnInExoticExPVPsSecond",JOB,".rda")) #Save burn-in.