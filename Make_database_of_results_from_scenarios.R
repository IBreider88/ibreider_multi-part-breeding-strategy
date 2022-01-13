#Breider et al. 2022 DOI: 10.21203/rs.3.rs-1246254/v1
#
load(Sys.glob("*Combined.rda")) #loads combined results
fileName <- Sys.glob("*1Output.rda") #gets me the correct name for the variable containing genGain
fileName<-strsplit(fileName, "_") #gets me the correct name for the variable containing genGain
fileName<-fileName[[1]][2]
MaxGenGainDH<-mean(get(paste0("genValDH",fileName))[dim(get(paste0("genValDH",fileName)))[1],])  ##gets me max gain last year I'm saving the dimensions of the matrix as a thing and then refer to the first element (so Temp<-dim(thing I'm measuring), gives [1] is n rows of dim)
MaxGenGainEYT1<-mean(get(paste0("genValEYT1",fileName))[dim(get(paste0("genValEYT1",fileName)))[1],])  ##g

Acc<-strsplit(fileName,"Acc")
Acc<-strsplit(Acc[[1]][2],"Gen")
Acc<-Acc[[1]][1]

GenLag<-strsplit(fileName,"Lag")
GenLag<-strsplit(GenLag[[1]][2],"nBr")
GenLag<-GenLag[[1]][1]

Result<-paste(fileName,"Acc",Acc,"GenLag",GenLag,"MaxGenGainDH",MaxGenGainDH,"MaxGenGainDH1",MaxGenGainEYT1)

write(Result, file="../SummaryofResultsScenarios.txt", sep="\n", append=TRUE)  ###Will end up in one folder up from cwd this file works in, so directory array job was started in 

