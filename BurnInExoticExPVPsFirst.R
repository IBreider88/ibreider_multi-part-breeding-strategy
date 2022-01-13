#Breider et al. 2022 DOI: 10.21203/rs.3.rs-1246254/v1
JOB = Sys.getenv("SGE_TASK_ID") #JOBREP 1 to 10, make sure right createdParents file is read in
library(AlphaSimR)
load(paste0("CreatedParentsExPVPs",JOB,".rda"))  #Read in created parents

nBurninYears = 30 #Years of burn-in breeding  

#Matrices to save output
BurninGenGainDHex1 = matrix(0,nrow=nBurninYears+1,ncol=1)
BurninGenicVarDHex1 = matrix(0,nrow=nBurninYears+1,ncol=1)
BurninGenGainEYT1ex1 = matrix(0,nrow=nBurninYears+1,ncol=1)
BurninGenicVarEYT1ex1 = matrix(0,nrow=nBurninYears+1,ncol=1)

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
  F1ex1 = randCross(ExoticParents,100) #F1 is created from 100 random crosses between elite parents.
  if(year<7){
    DHex1 = makeDH(F1ex1,nDH=62) #62 DH are created from each F1 (100 inds), total 6200 DHs.   
  }
  if(year<6){
    HDRWex1 = setPheno(DHex1,varE=SP$varE*5) #Phenotype is set for HDRW, varE is set to 1/5th of a repeat.
  }
  if(year<5){
    PYTex1 = selectWithinFam(HDRWex1,5) #This year's PYT is selected from last year's HDRW. First 5 individuals are selected per female,
    PYTex1 = selectInd(PYTex1,500) #then the 500 best performing individuals are selected
    PYTex1 = setPheno(PYTex1) #Phenotype is set for PYT
  }
  if(year<4){
    AYTex1 = selectInd(PYTex1,50) #This year's AYT is selected from last year's PYT.
    AYTex1 = setPheno(AYTex1,varE=SP$varE/4) #Phenotype is set for this year's EYT, varE is set with 4 locations in mind
  }
  if(year<3){
    EYT1ex1 = selectInd(AYTex1,10) #This year's EYT1 is selected from last year's AYT
    EYT1ex1 = setPheno(EYT1ex1,varE=SP$varE/8) #Phenotype is set for this year's EYT, varE is set with 8 locations in mind
  }
  if(year<2){
    EYT2ex1 = EYT1ex1 #Last year's EYT1 becomes this year's EYT2
    EYT2ex1 = setPheno(EYT2ex1,varE=SP$varE/8) #Phenotype is set for this year's EYT, varE is set with 8 locations in mind
  }
}

for(YEAR in 1:nBurninYears){
  #Advance 1 year
  VARex1 = selectInd(EYT2ex1,2) #Two varieties are produced
  EYT2ex1 = EYT1ex1 #Last year's EYT1 becomes this year's EYT2
  EYT2ex1 = setPheno(EYT2ex1,varE=SP$varE/8) #Phenotype is set for this year's EYT, varE is set with 8 locations in mind
  EYT1ex1 = selectInd(AYTex1,10) #This year's EYT1 is selected from last year's AYT
  EYT1ex1 = setPheno(EYT1ex1,varE=SP$varE/8) #Phenotype is set for this year's EYT, varE is set with 8 locations in mind
  AYTex1 = selectInd(PYTex1,50) #This year's AYT is selected from last year's PYT.
  AYTex1 = setPheno(AYTex1,varE=SP$varE/4) #Phenotype is set for this year's EYT, varE is set with 4 locations in mind
  PYTex1 = selectWithinFam(HDRWex1,5) #This year's PYT is selected from last year's HDRW. First 5 individuals are selected per female,
  PYTex1 = selectInd(PYTex1,500) #then the 500 best performing individuals are selected
  PYTex1 = setPheno(PYTex1) #Phenotype is set for PYT
  HDRWex1 = setPheno(DHex1,varE=SP$varE*5) #Phenotype is set for HDRW, varE is set to 1/5th of a repeat.
  DHex1 = makeDH(F1ex1,nDH=62) #62 DH are created from each F1 (100 inds), total 6200 DHs. 
  F1ex1 = randCross(mergePops(list(AYTex1,EYT1ex1,EYT2ex1)),100) #F1 is created from 100 random crosses between elite parents.
  #Saving results
  BurninGenGainDHex1[YEAR+1,1] = meanG(DHex1)
  BurninGenicVarDHex1[YEAR+1,1] = varG(DHex1)
  BurninGenGainEYT1ex1[YEAR+1,1] = meanG(EYT1ex1)
  BurninGenicVarEYT1ex1[YEAR+1,1] = varG(EYT1ex1)
}
interceptDHex1 = meanG(DHex1) #Saving intercept at end of burn-in
interceptEYT1ex1 = meanG(EYT1ex1) #Saving intercept at end of burn-in

rm(ExoticParents) #Remove as no longer needed.
rm(EliteParents) #Remove as no longer needed.
save.image(paste0("BurnInExoticExPVPsFirst",JOB,".rda")) #Save burn-in.
