#Breider et al. 2022 DOI: 10.21203/rs.3.rs-1246254/v1
#
#To save the IDs of the elite population at the end of the burnin. To be used to gain insight in haploblock origin.
#This should have been done in burnin, but I forgot.
#To extract ancestor IDs from F1
Temp<-pullIbdHaplo(F1) #IBD haplo of F1 at end of burnin
Temp2<-((Temp-1)%/%2)+1 #Converting IBD haplo to haploblock origin IDs
Temp3<-table(Temp2) #Turn into a table to get a summary of the ancestor IDs in the population.
Temp4<-dimnames(Temp3) #Extract ancestor IDs
EliteAnch<-Temp4$Temp2 #Save ancestor IDs table
#To extract ancestor IDs from DH
Temp<-pullIbdHaplo(DH) 
Temp2<-((Temp-1)%/%2)+1 
Temp3<-table(Temp2) #
Temp4<-dimnames(Temp3) 
EliteAnch<-c(EliteAnch,Temp4$Temp2) #Add to full table of ancestor IDs table
#To extract ancestor IDs from HDRW
Temp<-pullIbdHaplo(HDRW)
Temp2<-((Temp-1)%/%2)+1
Temp3<-table(Temp2)
Temp4<-dimnames(Temp3)
EliteAnch<-c(EliteAnch,Temp4$Temp2)
#To extract ancestor IDs from PYT
Temp<-pullIbdHaplo(PYT)
Temp2<-((Temp-1)%/%2)+1
Temp3<-table(Temp2)
Temp4<-dimnames(Temp3)
EliteAnch<-c(EliteAnch,Temp4$Temp2)
#To extract ancestor IDs from AYT
Temp<-pullIbdHaplo(AYT)
Temp2<-((Temp-1)%/%2)+1
Temp3<-table(Temp2)
Temp4<-dimnames(Temp3)
EliteAnch<-c(EliteAnch,Temp4$Temp2)
#To extract ancestor IDs from EYT1
Temp<-pullIbdHaplo(EYT1)
Temp2<-((Temp-1)%/%2)+1
Temp3<-table(Temp2)
Temp4<-dimnames(Temp3)
EliteAnch<-c(EliteAnch,Temp4$Temp2)
#To extract ancestor IDs from EYT2
Temp<-pullIbdHaplo(EYT2)
Temp2<-((Temp-1)%/%2)+1
Temp3<-table(Temp2)
Temp4<-dimnames(Temp3)
EliteAnch<-c(EliteAnch,Temp4$Temp2)
#To extract ancestor IDs from Start2Part
Temp<-pullIbdHaplo(Start2Part)
Temp2<-((Temp-1)%/%2)+1
Temp3<-table(Temp2)
Temp4<-dimnames(Temp3)
EliteAnch<-c(EliteAnch,Temp4$Temp2)

#Create summary table of ancestor IDs over all trials
EliteAnch<-table(EliteAnch)  #To remove duplicate IDs from the table (e.g. when ancestors are contributing to multiple trials in the burn-in)
EliteAnch<-dimnames(EliteAnch) #To remove duplicate IDs from the table
EliteAnch<-EliteAnch$EliteAnch #To remove duplicate IDs from the table
