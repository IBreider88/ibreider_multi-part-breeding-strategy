#Advance breeding program by 1 year
#Works backwards through pipeline to avoid copying data, breeding program still works forwards

  EYT2 = EYT1 #Last year's EYT1 becomes this year's EYT2
  EYT2 = setPheno(EYT2,varE=SP$varE/8) #Phenotype is set for this year's EYT, varE is set with 8 locations in mind

  EYT1 = selectInd(AYT,10,use="pheno") #This year's EYT1 is selected from last year's AYT
  EYT1 = setPheno(EYT1,varE=SP$varE/8) #Phenotype is set for this year's EYT, varE is set with 8 locations in mind

  #For selection of AYT from PYT GS is used. As accuracy of GS in this study is fixed, accuracy is set at the PYT stage,
  #and AYT is selected using the "pheno" setting in selectInd. This then reflects GS with the chosen accuracy.
  AYT = selectInd(PYT,50,use="pheno") #This year's AYT is selected from last year's PYT.
  AYT = setPheno(AYT,varE=SP$varE/4) #Phenotype is set for this year's EYT, varE is set with 4 locations in mind

  PYT = selectWithinFam(HDRW,5,use="pheno")  #This year's PYT is selected from last year's HDRW. First 5 individuals are selected per female,
  PYT = selectInd(PYT,500,use="pheno") #then the 500 best performing individuals are selected
  PYT = setPheno(PYT,varE=setAcc(PYT,chosenAcc)) #Phenotype is set for PYT, varE is set using the chosen acc for GS to enable GS from PYT to AYT stage

  HDRW = setPheno(DH,varE=SP$varE*5) #Phenotype is set for HDRW, varE is set to 1/5th of a repeat.
  
  rm(DH) #DH are removed to make sure a new DH is created for this year.
  if(YEAR<2){ #Before transition to 2-part
    DH = makeDH(F1,nDH=62) #62 DH are created from each F1 (100 inds), total 6200 DHs  
    rm(F1) #F1 is no longer needed and therefore removed
  }else{ #After transition to 2-part
    DH = makeDH(ToPD,nDH=31) #31 DH are created from each ToPD (200 inds), total 6200 DHs  
    rm(ToPD)} #ToPd is no longer needed and therefore removed
  