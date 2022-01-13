#Breider et al. 2022 DOI: 10.21203/rs.3.rs-1246254/v1
#
load(Sys.glob("*Combined.rda")) #loads combined results
fileName <- Sys.glob("*5Output.rda") #gets me the correct name for the variable containing genGain
fileName<-strsplit(fileName, "_") #gets me the correct name for the variable containing genGain
fileName<-fileName[[1]][2]
#MaxGenGain<-mean(get(paste0("genValDH",fileName))[dim(get(paste0("genValDH",fileName)))[1],])  ##gets me max gain last year I'm saving the dimensions of the matrix as a thing and then refer to the first element (so Temp<-dim(thing I'm measuring), gives [1] is n rows of dim)
MaxGenGainEYT1<-mean(get(paste0("genValEYT1",fileName))[101,]) #year 50, row 51

Acc<-strsplit(fileName,"Acc")
Acc<-strsplit(Acc[[1]][2],"nBr")
Acc<-Acc[[1]][1]

nBridges<-strsplit(fileName,"Bridges")
nBridges<-strsplit(nBridges[[1]][2],"nCyc")
nBridges<-nBridges[[1]][1]

nCyclesBr<-strsplit(fileName,"nCyclesBr")
nCyclesBr<-strsplit(nCyclesBr[[1]][2],"IR")
nCyclesBr<-nCyclesBr[[1]][1]

IR<-strsplit(fileName,"IR")
IR<-strsplit(IR[[1]][2],"BR")
IR<-IR[[1]][1]

BR<-strsplit(fileName,"BR")
BR<-strsplit(BR[[1]][2],"RR")
BR<-BR[[1]][1]

RR<-strsplit(fileName,"RR")
RR<-strsplit(RR[[1]][2],"ER")
RR<-RR[[1]][1]

ER<-strsplit(fileName,"ER")
ER<-strsplit(ER[[1]][2],"_1O")
ER<-ER[[1]][1]

Result<-paste(fileName,"MaxGenGainEYT1",MaxGenGainEYT1,Acc,nBridges,nCyclesBr,IR,BR,RR,ER)

write(Result, file="../MatrixOfCovariatesY100EYT1Base.txt", sep="\n", append=TRUE)  ###Will end up in one folder up from cwd this file works in, so directory array job was started in 

