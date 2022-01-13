#Breider et al. 2022 DOI: 10.21203/rs.3.rs-1246254/v1
#
JOB = Sys.getenv("SGE_TASK_ID")
library(AlphaSimR)
load(paste0("CreatedParentsExPVPs",JOB,".rda"))

nBurninYears = 30 

#Matrices to save output
BurninGenGainDHex3 = matrix(0,nrow=nBurninYears+1,ncol=1)
BurninGenicVarDHex3 = matrix(0,nrow=nBurninYears+1,ncol=1)
BurninGenGainEYT1ex3 = matrix(0,nrow=nBurninYears+1,ncol=1)
BurninGenicVarEYT1ex3 = matrix(0,nrow=nBurninYears+1,ncol=1)

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
  F1ex3 = randCross(ExoticParents,100) #F1 is created from 100 random crosses between elite parents.
  if(year<7){
    DHex3 = makeDH(F1ex3,nDH=62) #62 DH are created from each F1 (100 inds), total 6200 DHs.   
  }
  if(year<6){
    HDRWex3 = setPheno(DHex3,varE=SP$varE*5) #Phenotype is set for HDRW, varE is set to 1/5th of a repeat.
  }
  if(year<5){
    PYTex3 = selectWithinFam(HDRWex3,5) #This year's PYT is selected from last year's HDRW. First 5 individuals are selected per female,
    PYTex3 = selectInd(PYTex3,500) #then the 500 best performing individuals are selected
    PYTex3 = setPheno(PYTex3) #Phenotype is set for PYT
  }
  if(year<4){
    AYTex3 = selectInd(PYTex3,50) #This year's AYT is selected from last year's PYT.
    AYTex3 = setPheno(AYTex3,varE=SP$varE/4) #Phenotype is set for this year's EYT, varE is set with 4 locations in mind
  }
  if(year<3){
    EYT1ex3 = selectInd(AYTex3,10) #This year's EYT1 is selected from last year's AYT
    EYT1ex3 = setPheno(EYT1ex3,varE=SP$varE/8) #Phenotype is set for this year's EYT, varE is set with 8 locations in mind
  }
  if(year<2){
    EYT2ex3 = EYT1ex3 #Last year's EYT1 becomes this year's EYT2
    EYT2ex3 = setPheno(EYT2ex3,varE=SP$varE/8) #Phenotype is set for this year's EYT, varE is set with 8 locations in mind
  }
}

for(YEAR in 1:nBurninYears){
  #Advance 1 year
  VARex3 = selectInd(EYT2ex3,2) #Two varieties are produced
  EYT2ex3 = EYT1ex3 #Last year's EYT1 becomes this year's EYT2
  EYT2ex3 = setPheno(EYT2ex3,varE=SP$varE/8) #Phenotype is set for this year's EYT, varE is set with 8 locations in mind
  EYT1ex3 = selectInd(AYTex3,10) #This year's EYT1 is selected from last year's AYT
  EYT1ex3 = setPheno(EYT1ex3,varE=SP$varE/8) #Phenotype is set for this year's EYT, varE is set with 8 locations in mind
  AYTex3 = selectInd(PYTex3,50) #This year's AYT is selected from last year's PYT.
  AYTex3 = setPheno(AYTex3,varE=SP$varE/4) #Phenotype is set for this year's EYT, varE is set with 4 locations in mind
  PYTex3 = selectWithinFam(HDRWex3,5) #This year's PYT is selected from last year's HDRW. First 5 individuals are selected per female,
  PYTex3 = selectInd(PYTex3,500) #then the 500 best performing individuals are selected
  PYTex3 = setPheno(PYTex3) #Phenotype is set for PYT
  HDRWex3 = setPheno(DHex3,varE=SP$varE*5) #Phenotype is set for HDRW, varE is set to 1/5th of a repeat.
  DHex3 = makeDH(F1ex3,nDH=62) #62 DH are created from each F1 (100 inds), total 6200 DHs. 
  F1ex3 = randCross(mergePops(list(AYTex3,EYT1ex3,EYT2ex3)),100) #F1 is created from 100 random crosses between elite parents.
  #Saving results
  BurninGenGainDHex3[YEAR+1,1] = meanG(DHex3)
  BurninGenicVarDHex3[YEAR+1,1] = varG(DHex3)
  BurninGenGainEYT1ex3[YEAR+1,1] = meanG(EYT1ex3)
  BurninGenicVarEYT1ex3[YEAR+1,1] = varG(EYT1ex3)
}
interceptDHex3 = meanG(DHex3) #Saving intercept at end of burn-in
interceptEYT1ex3 = meanG(EYT1ex3) #Saving intercept at end of burn-in

rm(ExoticParents) #Remove as no longer needed.
rm(EliteParents) #Remove as no longer needed.
save.image(paste0("BurnInExoticExPVPsThird",JOB,".rda")) #Save burn-in.
