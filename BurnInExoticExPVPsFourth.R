#Breider et al. 2022 DOI: 10.21203/rs.3.rs-1246254/v1
#
JOB = Sys.getenv("SGE_TASK_ID")
library(AlphaSimR)
load(paste0("CreatedParentsExPVPs",JOB,".rda"))

nBurninYears = 30 

#Matrices to save output
BurninGenGainDHex4 = matrix(0,nrow=nBurninYears+1,ncol=1)
BurninGenicVarDHex4 = matrix(0,nrow=nBurninYears+1,ncol=1)
BurninGenGainEYT1ex4 = matrix(0,nrow=nBurninYears+1,ncol=1)
BurninGenicVarEYT1ex4 = matrix(0,nrow=nBurninYears+1,ncol=1)

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
  F1ex4 = randCross(ExoticParents,100) #F1 is created from 100 random crosses between elite parents.
  if(year<7){
    DHex4 = makeDH(F1ex4,nDH=62) #62 DH are created from each F1 (100 inds), total 6200 DHs.   
  }
  if(year<6){
    HDRWex4 = setPheno(DHex4,varE=SP$varE*5) #Phenotype is set for HDRW, varE is set to 1/5th of a repeat.
  }
  if(year<5){
    PYTex4 = selectWithinFam(HDRWex4,5) #This year's PYT is selected from last year's HDRW. First 5 individuals are selected per female,
    PYTex4 = selectInd(PYTex4,500) #then the 500 best performing individuals are selected
    PYTex4 = setPheno(PYTex4) #Phenotype is set for PYT
  }
  if(year<4){
    AYTex4 = selectInd(PYTex4,50) #This year's AYT is selected from last year's PYT.
    AYTex4 = setPheno(AYTex4,varE=SP$varE/4) #Phenotype is set for this year's EYT, varE is set with 4 locations in mind
  }
  if(year<3){
    EYT1ex4 = selectInd(AYTex4,10) #This year's EYT1 is selected from last year's AYT
    EYT1ex4 = setPheno(EYT1ex4,varE=SP$varE/8) #Phenotype is set for this year's EYT, varE is set with 8 locations in mind
  }
  if(year<2){
    EYT2ex4 = EYT1ex4 #Last year's EYT1 becomes this year's EYT2
    EYT2ex4 = setPheno(EYT2ex4,varE=SP$varE/8) #Phenotype is set for this year's EYT, varE is set with 8 locations in mind
  }
}

for(YEAR in 1:nBurninYears){
  #Advance 1 year
  VARex4 = selectInd(EYT2ex4,2) #Two varieties are produced
  EYT2ex4 = EYT1ex4 #Last year's EYT1 becomes this year's EYT2
  EYT2ex4 = setPheno(EYT2ex4,varE=SP$varE/8) #Phenotype is set for this year's EYT, varE is set with 8 locations in mind
  EYT1ex4 = selectInd(AYTex4,10) #This year's EYT1 is selected from last year's AYT
  EYT1ex4 = setPheno(EYT1ex4,varE=SP$varE/8) #Phenotype is set for this year's EYT, varE is set with 8 locations in mind
  AYTex4 = selectInd(PYTex4,50) #This year's AYT is selected from last year's PYT.
  AYTex4 = setPheno(AYTex4,varE=SP$varE/4) #Phenotype is set for this year's EYT, varE is set with 4 locations in mind
  PYTex4 = selectWithinFam(HDRWex4,5) #This year's PYT is selected from last year's HDRW. First 5 individuals are selected per female,
  PYTex4 = selectInd(PYTex4,500) #then the 500 best performing individuals are selected
  PYTex4 = setPheno(PYTex4) #Phenotype is set for PYT
  HDRWex4 = setPheno(DHex4,varE=SP$varE*5) #Phenotype is set for HDRW, varE is set to 1/5th of a repeat.
  DHex4 = makeDH(F1ex4,nDH=62) #62 DH are created from each F1 (100 inds), total 6200 DHs. 
  F1ex4 = randCross(mergePops(list(AYTex4,EYT1ex4,EYT2ex4)),100) #F1 is created from 100 random crosses between elite parents.
  #Saving results
  BurninGenGainDHex4[YEAR+1,1] = meanG(DHex4)
  BurninGenicVarDHex4[YEAR+1,1] = varG(DHex4)
  BurninGenGainEYT1ex4[YEAR+1,1] = meanG(EYT1ex4)
  BurninGenicVarEYT1ex4[YEAR+1,1] = varG(EYT1ex4)
}
interceptDHex4 = meanG(DHex4) #Saving intercept at end of burn-in
interceptEYT1ex4 = meanG(EYT1ex4) #Saving intercept at end of burn-in

rm(ExoticParents) #Remove as no longer needed.
rm(EliteParents) #Remove as no longer needed.
save.image(paste0("BurnInExoticExPVPsFourth",JOB,".rda")) #Save burn-in.
