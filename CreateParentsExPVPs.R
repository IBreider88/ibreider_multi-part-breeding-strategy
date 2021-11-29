JOB = Sys.getenv("SGE_TASK_ID") #JOBREP 1 to 10

library(AlphaSimR) #Load library

#Create Parents
founderPop = runMacs(70,10,1000,inbred=TRUE,species= "MAIZE",
                     split=0,nThreads=10) #Create a founder population with 70 individuals, 10 chromosomes, 1,000 QTLs per chromosome and species history for Maize.


#Simulation parameters
SP = SimParam$
  new(founderPop[1:70])$
  addTraitA(1000,mean=0,var=1)$ #Add an additive trait influenced by 1,000 QTLs per chromosome, a genetic value of 0 and genetic variance 1.
  setVarE(h2=0.3)$ #default error variance based on a h2 of 0.3
  setTrackRec(TRUE) #Sets recombination tracking to TRUE, this also turns on pedigree tracking.

#Create initial lines and exotic pool
EliteParents = newPop(founderPop[1:70]) #Has haplotypes 1:140 
ExoticParents = newPop(founderPop[1:70]) #Has haplotypes 141:280

rm(founderPop) #Remove as no longer needed
rm(.Random.seed) #Remove so analyses are really independent.

save.image(paste0("CreatedParentsExPVPs",JOB,".rda")) #Save created parents.