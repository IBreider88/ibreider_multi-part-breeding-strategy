#Breider et al. 2022 DOI: 10.21203/rs.3.rs-1246254/v1
#
JOB = Sys.getenv("SGE_TASK_ID") #JOBREP 1 to 10, to make sure right createParents input file is read in
library(AlphaSimR)
source("CreateParentsExPVPs.R") #Script to create parents.
rm(list=ls()) #Remove everything before start elite burn-in
source("BurnInEliteExPVPs.R") #Burn-in breeding for elite program  
rm(list=ls()) #Remove everything before start first exotic burn-in
source("BurnInExoticExPVPsFirst.R") #Burn-in breeding for first exotic program
rm(list=ls()) #Remove everything before start second exotic burn-in
source("BurnInExoticExPVPsSecond.R") #Burn-in breeding for second exotic program
rm(list=ls()) #Remove everything before start third exotic burn-in
source("BurnInExoticExPVPsThird.R") #Burn-in breeding for third exotic program
rm(list=ls()) #Remove everything before start fourth exotic burn-in
source("BurnInExoticExPVPsFourth.R") #Burn-in breeding for fourth exotic program
rm(list=ls()) #Remove everything after burn-ins
