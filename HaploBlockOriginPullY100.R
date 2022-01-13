#Breider et al. 2022 DOI: 10.21203/rs.3.rs-1246254/v1
#
library(AlphaSimR)

JOBREP = Sys.getenv("SGE_TASK_ID")
load(paste0("MultiPart_Acc1nBridges2nCyclesBr6IR0.5BR0.45RR0.45ER1_",JOBREP,".rda"))

Chr1IbD<-pullIbdHaplo(SelectedPI_SnapShot_Year_100,1)
Chr2IbD<-pullIbdHaplo(SelectedPI_SnapShot_Year_100,2)
Chr3IbD<-pullIbdHaplo(SelectedPI_SnapShot_Year_100,3)
Chr4IbD<-pullIbdHaplo(SelectedPI_SnapShot_Year_100,4)
Chr5IbD<-pullIbdHaplo(SelectedPI_SnapShot_Year_100,5)
Chr6IbD<-pullIbdHaplo(SelectedPI_SnapShot_Year_100,6)
Chr7IbD<-pullIbdHaplo(SelectedPI_SnapShot_Year_100,7)
Chr8IbD<-pullIbdHaplo(SelectedPI_SnapShot_Year_100,8)
Chr9IbD<-pullIbdHaplo(SelectedPI_SnapShot_Year_100,9)
Chr10IbD<-pullIbdHaplo(SelectedPI_SnapShot_Year_100,10)


SelectedPIHaploIbd<-cbind(Chr1IbD,Chr2IbD,Chr3IbD,Chr4IbD,Chr5IbD,
                          Chr6IbD,Chr7IbD,Chr8IbD,Chr9IbD,Chr10IbD)

save.image(paste0("HaploBlockOriginY100",JOBREP,".rda"))
