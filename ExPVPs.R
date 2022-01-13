#Breider et al. 2022 DOI: 10.21203/rs.3.rs-1246254/v1
#
#exPVP breeding program 1
VARex1 = selectInd(EYT2ex1,2) #Two varieties are produced
EYT2ex1 = EYT1ex1 #Last year's EYT1ex1 becomes this year's EYT2ex1
EYT2ex1 = setPheno(EYT2ex1,varE=SP$varE/8) #Phenotype is set for this year's EYT, varE is set with 8 locations in mind
EYT1ex1 = selectInd(AYTex1,10) #This year's EYT1 is selected from last year's AYT
EYT1ex1 = setPheno(EYT1ex1,varE=SP$varE/8) #Phenotype is set for this year's EYT, varE is set with 8 locations in mind
AYTex1 = selectInd(PYTex1,50) #This year's AYT is selected from last year's PYT.
AYTex1 = setPheno(AYTex1,varE=SP$varE/4) #Phenotype is set for this year's EYT, varE is set with 4 locations in mind
PYTex1 = selectWithinFam(HDRWex1,5) #This year's PYT is selected from last year's HDRW. First 5 individuals are selected per female,
PYTex1 = selectInd(PYTex1,500) #then the 500 best performing individuals are selected
PYTex1 = setPheno(PYTex1) #Phenotype is set for PYT, varE is set using the chosen acc for GS to enable GS from PYT to AYT stage
HDRWex1 = setPheno(DHex1,varE=SP$varE*5) #Phenotype is set for HDRW, varE is set to 1/5th of a repeat.
DHex1 = makeDH(F1ex1,nDH=62) #62 DH are created from each F1 (100 inds), total 6200 DHs  
F1ex1 = randCross(mergePops(list(AYTex1,EYT1ex1,EYT2ex1)),100) #F1 is created from a random cross (resulting in 100 inds) of all current AYT,EYT1 and EYT2 individuals.

#exPVP breeding program 2
VARex2 = selectInd(EYT2ex2,2) #Two varieties are produced
EYT2ex2 = EYT1ex2 #Last year's EYT1ex2 becomes this year's EYT2ex2
EYT2ex2 = setPheno(EYT2ex2,varE=SP$varE/8) #Phenotype is set for this year's EYT, varE is set with 8 locations in mind
EYT1ex2 = selectInd(AYTex2,10) #This year's EYT1 is selected from last year's AYT
EYT1ex2 = setPheno(EYT1ex2,varE=SP$varE/8) #Phenotype is set for this year's EYT, varE is set with 8 locations in mind
AYTex2 = selectInd(PYTex2,50) #This year's AYT is selected from last year's PYT.
AYTex2 = setPheno(AYTex2,varE=SP$varE/4) #Phenotype is set for this year's EYT, varE is set with 4 locations in mind
PYTex2 = selectWithinFam(HDRWex2,5) #This year's PYT is selected from last year's HDRW. First 5 individuals are selected per female,
PYTex2 = selectInd(PYTex2,500) #then the 500 best performing individuals are selected
PYTex2 = setPheno(PYTex2) #Phenotype is set for PYT, varE is set using the chosen acc for GS to enable GS from PYT to AYT stage
HDRWex2 = setPheno(DHex2,varE=SP$varE*5) #Phenotype is set for HDRW, varE is set to 1/5th of a repeat.
DHex2 = makeDH(F1ex2,nDH=62) #62 DH are created from each F1 (100 inds), total 6200 DHs  
F1ex2 = randCross(mergePops(list(AYTex2,EYT1ex2,EYT2ex2)),100) #F1 is created from a random cross (resulting in 100 inds) of all current AYT,EYT1 and EYT2 individuals.

#exPVP breeding program 3
VARex3 = selectInd(EYT2ex3,2) #Two varieties are produced
EYT2ex3 = EYT1ex3 #Last year's EYT1ex3 becomes this year's EYT2ex3
EYT2ex3 = setPheno(EYT2ex3,varE=SP$varE/8) #Phenotype is set for this year's EYT, varE is set with 8 locations in mind
EYT1ex3 = selectInd(AYTex3,10) #This year's EYT1 is selected from last year's AYT
EYT1ex3 = setPheno(EYT1ex3,varE=SP$varE/8) #Phenotype is set for this year's EYT, varE is set with 8 locations in mind
AYTex3 = selectInd(PYTex3,50) #This year's AYT is selected from last year's PYT.
AYTex3 = setPheno(AYTex3,varE=SP$varE/4) #Phenotype is set for this year's EYT, varE is set with 4 locations in mind
PYTex3 = selectWithinFam(HDRWex3,5) #This year's PYT is selected from last year's HDRW. First 5 individuals are selected per female,
PYTex3 = selectInd(PYTex3,500) #then the 500 best performing individuals are selected
PYTex3 = setPheno(PYTex3) #Phenotype is set for PYT, varE is set using the chosen acc for GS to enable GS from PYT to AYT stage
HDRWex3 = setPheno(DHex3,varE=SP$varE*5) #Phenotype is set for HDRW, varE is set to 1/5th of a repeat.
DHex3 = makeDH(F1ex3,nDH=62) #62 DH are created from each F1 (100 inds), total 6200 DHs  
F1ex3 = randCross(mergePops(list(AYTex3,EYT1ex3,EYT2ex3)),100) #F1 is created from a random cross (resulting in 100 inds) of all current AYT,EYT1 and EYT2 individuals.

#exPVP breeding program 4
VARex4 = selectInd(EYT2ex4,2) #Two varieties are produced
EYT2ex4 = EYT1ex4 #Last year's EYT1ex4 becomes this year's EYT2ex4
EYT2ex4 = setPheno(EYT2ex4,varE=SP$varE/8) #Phenotype is set for this year's EYT, varE is set with 8 locations in mind
EYT1ex4 = selectInd(AYTex4,10) #This year's EYT1 is selected from last year's AYT
EYT1ex4 = setPheno(EYT1ex4,varE=SP$varE/8) #Phenotype is set for this year's EYT, varE is set with 8 locations in mind
AYTex4 = selectInd(PYTex4,50) #This year's AYT is selected from last year's PYT.
AYTex4 = setPheno(AYTex4,varE=SP$varE/4) #Phenotype is set for this year's EYT, varE is set with 4 locations in mind
PYTex4 = selectWithinFam(HDRWex4,5) #This year's PYT is selected from last year's HDRW. First 5 individuals are selected per female,
PYTex4 = selectInd(PYTex4,500) #then the 500 best performing individuals are selected
PYTex4 = setPheno(PYTex4) #Phenotype is set for PYT, varE is set using the chosen acc for GS to enable GS from PYT to AYT stage
HDRWex4 = setPheno(DHex4,varE=SP$varE*5) #Phenotype is set for HDRW, varE is set to 1/5th of a repeat.
DHex4 = makeDH(F1ex4,nDH=62) #62 DH are created from each F1 (100 inds), total 6200 DHs  
F1ex4 = randCross(mergePops(list(AYTex4,EYT1ex4,EYT2ex4)),100) #F1 is created from a random cross (resulting in 100 inds) of all current AYT,EYT1 and EYT2 individuals.

#Create pop to introgress
VarPop=mergePops(list(VARex1,VARex2,VARex3,VARex4)) #Merge all exotic varieties of the year into 1 population.
founderPop2 = formFounderPop(VarPop) #Reset IDs for tracking purposes
VarPop = newPop(founderPop2) #Reset IDs for tracking purposes
assign(paste("VarPop_SnapShot_Year", YEAR, sep = "_"),VarPop) #Take a snapshot for further analyses

if(YEAR==1|YEAR==2){rm(VarPop)} #For year 1 and 2 VarPop is not needed.
