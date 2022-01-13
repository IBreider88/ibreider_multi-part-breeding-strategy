#Breider et al. 2022 DOI: 10.21203/rs.3.rs-1246254/v1
#
#Start TwoPart off from burnin, burnin gives 500 inds (PYT, AYT,EYT)
Start2Part=Start2Part[sample.int(nInd(Start2Part))] #Randomises the order of individuals in the population
Start2Part=randCross(Start2Part,(GenoInPI/nCyclesBr)) #Randomly crosses individuals in the population, number of crosses depends on genotyping budget for the breeding cycle
interceptPI = meanG(Start2Part) #Save mean genetic value of population when initiating the parent improvement, to use as benchmark
Start2Part=setPheno(Start2Part,varE=setAcc(Start2Part,chosenAcc)) #Set phenotypes at start of cycle, varE set depending on the acc for GS chosen
Start2Part=Start2Part[sample.int(nInd(Start2Part))] #Randomises the order of the new Start2Part individuals
#Divide Start2Part population in females and males equally (and randomly, due to previous line)
ProduceFemales=Start2Part[1:(nInd(Start2Part)/2)]
ProduceMales=Start2Part[(nInd(Start2Part)/2+1):nInd(Start2Part)]
rm(Start2Part) #Remove Start2Part as no longer needed
ProduceFemales=selectInd(selectWithinFam(ProduceFemales,1,use="pheno",selectTop = TRUE,famType="F"),
                      ParentsPI,use="pheno",selectTop = TRUE) #Select female parents for the next cycle, first 1 ind from each female half-sib family, then the top [ParentsPI] individuals.
ProduceMales=selectInd(selectWithinFam(ProduceMales,1,use="pheno",selectTop = TRUE,famType="F"), #Select male parents for the next cycle, first 1 ind from each female half-sib family, then the top [ParentsPI] individuals.
                      ParentsPI,use="pheno",selectTop = TRUE)
