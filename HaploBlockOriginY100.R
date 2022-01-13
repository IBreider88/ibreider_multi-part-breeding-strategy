#Breider et al. 2022 DOI: 10.21203/rs.3.rs-1246254/v1
#
library(AlphaSimR)
JOBREP= Sys.getenv("SGE_TASK_ID")

load(paste0("HaploBlockOriginY100",JOBREP,".rda"))
n=1 #n of alleles/haploblock
listBinLength<-seq(1, 10000, n) #seq(1, 10000, n of alleles/haploblock), can be adapted to make bigger haploblocks

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
} 

#VarPop are 8 varieties, 2 from each exPVP breeding program. Snapshots taken in ExPVPs.R
ExoticIDsY1<-VarPop_SnapShot_Year_1@id #These go into F1Br1 in year 2 to start the first bridge in year 3.
F1Br1Y2<-F1Br1_SnapShot_Year_2@id
ExoticIDsY2<-VarPop_SnapShot_Year_2@id #These should be skipped, as they are produced in year 2, ready for year 3, but in year 3 F1Br1 is used. I left this in as this should result in zero (fault check).
ExoticIDsY3<-VarPop_SnapShot_Year_3@id #Introgressed in year 4
ExoticIDsY4<-VarPop_SnapShot_Year_4@id #" " 5
ExoticIDsY5<-VarPop_SnapShot_Year_5@id #etc.
ExoticIDsY6<-VarPop_SnapShot_Year_6@id
ExoticIDsY7<-VarPop_SnapShot_Year_7@id
ExoticIDsY8<-VarPop_SnapShot_Year_8@id
ExoticIDsY9<-VarPop_SnapShot_Year_9@id
ExoticIDsY10<-c(VarPop_SnapShot_Year_10@id,
                VarPop_SnapShot_Year_11@id,
                VarPop_SnapShot_Year_12@id,
                VarPop_SnapShot_Year_13@id,
                VarPop_SnapShot_Year_14@id,
                VarPop_SnapShot_Year_15@id,
                VarPop_SnapShot_Year_16@id,
                VarPop_SnapShot_Year_17@id,
                VarPop_SnapShot_Year_18@id,
                VarPop_SnapShot_Year_19@id)
ExoticIDsY20<-c(VarPop_SnapShot_Year_20@id,
                VarPop_SnapShot_Year_21@id,
                VarPop_SnapShot_Year_22@id,
                VarPop_SnapShot_Year_23@id,
                VarPop_SnapShot_Year_24@id,
                VarPop_SnapShot_Year_25@id,
                VarPop_SnapShot_Year_26@id,
                VarPop_SnapShot_Year_27@id,
                VarPop_SnapShot_Year_28@id,
                VarPop_SnapShot_Year_29@id)
ExoticIDsY30<-c(VarPop_SnapShot_Year_30@id,
                VarPop_SnapShot_Year_31@id,
                VarPop_SnapShot_Year_32@id,
                VarPop_SnapShot_Year_33@id,
                VarPop_SnapShot_Year_34@id,
                VarPop_SnapShot_Year_35@id,
                VarPop_SnapShot_Year_36@id,
                VarPop_SnapShot_Year_37@id,
                VarPop_SnapShot_Year_38@id,
                VarPop_SnapShot_Year_39@id)
ExoticIDsY40<-c(VarPop_SnapShot_Year_40@id,
                VarPop_SnapShot_Year_41@id,
                VarPop_SnapShot_Year_42@id,
                VarPop_SnapShot_Year_43@id,
                VarPop_SnapShot_Year_44@id,
                VarPop_SnapShot_Year_45@id,
                VarPop_SnapShot_Year_46@id,
                VarPop_SnapShot_Year_47@id,
                VarPop_SnapShot_Year_48@id,
                VarPop_SnapShot_Year_49@id)
ExoticIDsY50<-c(VarPop_SnapShot_Year_50@id,
                VarPop_SnapShot_Year_51@id,
                VarPop_SnapShot_Year_52@id,
                VarPop_SnapShot_Year_53@id,
                VarPop_SnapShot_Year_54@id,
                VarPop_SnapShot_Year_55@id,
                VarPop_SnapShot_Year_56@id,
                VarPop_SnapShot_Year_57@id,
                VarPop_SnapShot_Year_58@id,
                VarPop_SnapShot_Year_59@id)
ExoticIDsY60<-c(VarPop_SnapShot_Year_60@id,
                VarPop_SnapShot_Year_61@id,
                VarPop_SnapShot_Year_62@id,
                VarPop_SnapShot_Year_63@id,
                VarPop_SnapShot_Year_64@id,
                VarPop_SnapShot_Year_65@id,
                VarPop_SnapShot_Year_66@id,
                VarPop_SnapShot_Year_67@id,
                VarPop_SnapShot_Year_68@id,
                VarPop_SnapShot_Year_69@id)
ExoticIDsY70<-c(VarPop_SnapShot_Year_70@id,
                VarPop_SnapShot_Year_71@id,
                VarPop_SnapShot_Year_72@id,
                VarPop_SnapShot_Year_73@id,
                VarPop_SnapShot_Year_74@id,
                VarPop_SnapShot_Year_75@id,
                VarPop_SnapShot_Year_76@id,
                VarPop_SnapShot_Year_77@id,
                VarPop_SnapShot_Year_78@id,
                VarPop_SnapShot_Year_79@id)
ExoticIDsY80<-c(VarPop_SnapShot_Year_80@id,
                VarPop_SnapShot_Year_81@id,
                VarPop_SnapShot_Year_82@id,
                VarPop_SnapShot_Year_83@id,
                VarPop_SnapShot_Year_84@id,
                VarPop_SnapShot_Year_85@id,
                VarPop_SnapShot_Year_86@id,
                VarPop_SnapShot_Year_87@id,
                VarPop_SnapShot_Year_88@id,
                VarPop_SnapShot_Year_89@id)
ExoticIDsY90<-c(VarPop_SnapShot_Year_90@id,
                VarPop_SnapShot_Year_91@id,
                VarPop_SnapShot_Year_92@id,
                VarPop_SnapShot_Year_93@id,
                VarPop_SnapShot_Year_94@id,
                VarPop_SnapShot_Year_95@id,
                VarPop_SnapShot_Year_96@id,
                VarPop_SnapShot_Year_97@id,
                VarPop_SnapShot_Year_98@id,
                VarPop_SnapShot_Year_99@id)

HaploIDs<-((SelectedPIHaploIbd-1)%/%2)+1 #Converts HaploIbd code into anchestor IDs. SelectedPIHaploIbd created in HaploBlockOriginPullY100.R

for(i in 1:dim(HaploIDs)[1]){
  for(j in 1:dim(HaploIDs)[2]){
    if(HaploIDs[i,j] %in% EliteAnch){HaploIDs[i,j]<-"EliteIDs"
    }else if(HaploIDs[i,j] %in% ExoticIDsY1){HaploIDs[i,j]<-"ExoticIDsY1"
    }else if(HaploIDs[i,j] %in% F1Br1Y2){HaploIDs[i,j]<-"F1Br1Y2"
    }else if(HaploIDs[i,j] %in% ExoticIDsY2){HaploIDs[i,j]<-"ExoticIDsY2"
    }else if(HaploIDs[i,j] %in% ExoticIDsY3){HaploIDs[i,j]<-"ExoticIDsY3"
    }else if(HaploIDs[i,j] %in% ExoticIDsY4){HaploIDs[i,j]<-"ExoticIDsY4"    
    }else if(HaploIDs[i,j] %in% ExoticIDsY5){HaploIDs[i,j]<-"ExoticIDsY5"
    }else if(HaploIDs[i,j] %in% ExoticIDsY6){HaploIDs[i,j]<-"ExoticIDsY6" 
    }else if(HaploIDs[i,j] %in% ExoticIDsY7){HaploIDs[i,j]<-"ExoticIDsY7"
    }else if(HaploIDs[i,j] %in% ExoticIDsY8){HaploIDs[i,j]<-"ExoticIDsY8"
    }else if(HaploIDs[i,j] %in% ExoticIDsY9){HaploIDs[i,j]<-"ExoticIDsY9"    
    }else if(HaploIDs[i,j] %in% ExoticIDsY10){HaploIDs[i,j]<-"ExoticIDsY10"
    }else if(HaploIDs[i,j] %in% ExoticIDsY20){HaploIDs[i,j]<-"ExoticIDsY20"
    }else if(HaploIDs[i,j] %in% ExoticIDsY30){HaploIDs[i,j]<-"ExoticIDsY30"
    }else if(HaploIDs[i,j] %in% ExoticIDsY40){HaploIDs[i,j]<-"ExoticIDsY40"
    }else if(HaploIDs[i,j] %in% ExoticIDsY50){HaploIDs[i,j]<-"ExoticIDsY50"
    }else if(HaploIDs[i,j] %in% ExoticIDsY60){HaploIDs[i,j]<-"ExoticIDsY60"
    }else if(HaploIDs[i,j] %in% ExoticIDsY70){HaploIDs[i,j]<-"ExoticIDsY70"
    }else if(HaploIDs[i,j] %in% ExoticIDsY80){HaploIDs[i,j]<-"ExoticIDsY80"
    }else if(HaploIDs[i,j] %in% ExoticIDsY90){HaploIDs[i,j]<-"ExoticIDsY90"}
  }
}

#CondensedMatrix<-matrix(0,nrow=dim(HaploIDs)[1],ncol=(dim(HaploIDs)[2]/n))
#k=0
#for(i in listBinLength){
#  k=k+1
#  for(j in 1:dim(HaploIDs)[1]){
#    CondensedMatrix[j,k]<-getmode(HaploIDs[j,i:(i+n-1)])
#  }
#}    

CondensedMatrix=HaploIDs

NrHBs<-dim(CondensedMatrix)[1]
CondensedMatrixPlus<-matrix(0,nrow=dim(CondensedMatrix)[1]+20,ncol=dim(CondensedMatrix)[2])
CondensedMatrixPlus[1:dim(CondensedMatrix)[1],1:dim(CondensedMatrix)[2]]<-CondensedMatrix

CondensedMatrixPlus[NrHBs+1,]<-"ExoticIDsY1"
CondensedMatrixPlus[NrHBs+2,]<-"ExoticIDsY2"
CondensedMatrixPlus[NrHBs+3,]<-"ExoticIDsY3"
CondensedMatrixPlus[NrHBs+4,]<-"ExoticIDsY4"
CondensedMatrixPlus[NrHBs+5,]<-"ExoticIDsY5"
CondensedMatrixPlus[NrHBs+6,]<-"ExoticIDsY6"
CondensedMatrixPlus[NrHBs+7,]<-"ExoticIDsY7"
CondensedMatrixPlus[NrHBs+8,]<-"ExoticIDsY8"
CondensedMatrixPlus[NrHBs+9,]<-"ExoticIDsY9"
CondensedMatrixPlus[NrHBs+10,]<-"ExoticIDsY10"
CondensedMatrixPlus[NrHBs+11,]<-"ExoticIDsY20"
CondensedMatrixPlus[NrHBs+12,]<-"ExoticIDsY30"
CondensedMatrixPlus[NrHBs+13,]<-"ExoticIDsY40"
CondensedMatrixPlus[NrHBs+14,]<-"ExoticIDsY50"
CondensedMatrixPlus[NrHBs+15,]<-"ExoticIDsY60"
CondensedMatrixPlus[NrHBs+16,]<-"ExoticIDsY70"
CondensedMatrixPlus[NrHBs+17,]<-"ExoticIDsY80"
CondensedMatrixPlus[NrHBs+18,]<-"ExoticIDsY90"
CondensedMatrixPlus[NrHBs+19,]<-"EliteIDs"
CondensedMatrixPlus[NrHBs+20,]<-"F1Br1Y2"


for(i in 1:dim(CondensedMatrixPlus)[2]){
  HB<-table(CondensedMatrixPlus[,i])-1
  if(i==1){FreqOriginHB<-HB
  }else{FreqOriginHB<-cbind(FreqOriginHB,HB)}
}

save.image(paste0("HaploBlockOriginY100",n,"QTL_",JOBREP,".rda"))

rm(list= ls()[!(ls() %in% c("FreqOriginHB","n","JOBREP"))])

save.image(paste0("HaploBlockOriginY100Table",n,"QTL_",JOBREP,".rda"))
