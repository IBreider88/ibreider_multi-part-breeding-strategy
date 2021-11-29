fileNames <- Sys.glob("*Output.rda")
if(length(fileNames)!=10){
  print("Not 10 repeats!")
  if(isTRUE(grep("*1Output.rda",fileNames,value=TRUE) %in% fileNames)){print("1")}else{
 	ToWrite<-paste(fileNames[1],1)
 	write(ToWrite,file="../NotTenReps.txt",append=TRUE)}
  if(isTRUE(grep("*2Output.rda",fileNames,value=TRUE) %in% fileNames)){print("2")}else{
        ToWrite<-paste(fileNames[1],2)
        write(ToWrite,file="../NotTenReps.txt",append=TRUE)}
  if(isTRUE(grep("*3Output.rda",fileNames,value=TRUE) %in% fileNames)){print("3")}else{
        ToWrite<-paste(fileNames[1],3)
        write(ToWrite,file="../NotTenReps.txt",append=TRUE)}
  if(isTRUE(grep("*4Output.rda",fileNames,value=TRUE) %in% fileNames)){print("4")}else{
        ToWrite<-paste(fileNames[1],4)
        write(ToWrite,file="../NotTenReps.txt",append=TRUE)}
  if(isTRUE(grep("*5Output.rda",fileNames,value=TRUE) %in% fileNames)){print("5")}else{
        ToWrite<-paste(fileNames[1],5)
        write(ToWrite,file="../NotTenReps.txt",append=TRUE)}
  if(isTRUE(grep("*6Output.rda",fileNames,value=TRUE) %in% fileNames)){print("6")}else{
 	ToWrite<-paste(fileNames[1],6)
 	write(ToWrite,file="../NotTenReps.txt",append=TRUE)} 
  if(isTRUE(grep("*7Output.rda",fileNames,value=TRUE) %in% fileNames)){print("7")}else{
 	ToWrite<-paste(fileNames[1],7)
 	write(ToWrite,file="../NotTenReps.txt",append=TRUE)}
  if(isTRUE(grep("*8Output.rda",fileNames,value=TRUE) %in% fileNames)){print("8")}else{
 	ToWrite<-paste(fileNames[1],8)
 	write(ToWrite,file="../NotTenReps.txt",append=TRUE)}
  if(isTRUE(grep("*9Output.rda",fileNames,value=TRUE) %in% fileNames)){print("9")}else{
 	ToWrite<-paste(fileNames[1],9)
 	write(ToWrite,file="../NotTenReps.txt",append=TRUE)}
  if(isTRUE(grep("*10Output.rda",fileNames,value=TRUE) %in% fileNames)){print("10")}else{
 	ToWrite<-paste(fileNames[1],10)
 	write(ToWrite,file="../NotTenReps.txt",append=TRUE)}
}else{

GluedgenValDH<-list()
GluedgenVarDH<-list()
GluedgenicVarDH<-list()
GluedgenValEYT1<-list()
GluedgenVarEYT1<-list()
GluedgenicVarEYT1<-list()
GluedgenValPI<-list()
GluedgenVarPI<-list()
GluedgenicVarPI<-list()
nCols<-length(fileNames)

for (fileName in fileNames) {
  if(file.exists(fileName)){
    load(fileName)
    GluedgenValDH<-cbind2(GluedgenValDH,genValDH)
    GluedgenVarDH<-cbind2(GluedgenVarDH,genVarDH)
    GluedgenicVarDH<-cbind2(GluedgenicVarDH,genicVarDH)
    GluedgenValEYT1<-cbind2(GluedgenValEYT1,genValEYT1)
    GluedgenVarEYT1<-cbind2(GluedgenVarEYT1,genVarEYT1)
    GluedgenicVarEYT1<-cbind2(GluedgenicVarEYT1,genicVarEYT1)
    GluedgenValPI<-cbind2(GluedgenValPI,genValPI)
    GluedgenVarPI<-cbind2(GluedgenVarPI,genVarPI)
    GluedgenicVarPI<-cbind2(GluedgenicVarPI,genicVarPI)
  }
  rm(list= ls()[!(ls() %in% c('GluedgenValDH','GluedgenVarDH','GluedgenicVarDH',
                              'GluedgenValEYT1','GluedgenVarEYT1','GluedgenicVarEYT1',
                              'GluedgenValPI','GluedgenVarPI','GluedgenicVarPI',
                              'fileName','nCols'))])  
}

VarName<-strsplit(fileName, "_") #to access VarName[[1]][n]

assign(paste0("genValDH",VarName[[1]][2]),matrix(unlist(GluedgenValDH),ncol=nCols,byrow=FALSE))
assign(paste0("genVarDH",VarName[[1]][2]),matrix(unlist(GluedgenVarDH),ncol=nCols,byrow=FALSE))
assign(paste0("genicVarDH",VarName[[1]][2]),matrix(unlist(GluedgenicVarDH),ncol=nCols,byrow=FALSE))

assign(paste0("genValEYT1",VarName[[1]][2]),matrix(unlist(GluedgenValEYT1),ncol=nCols,byrow=FALSE))
assign(paste0("genVarEYT1",VarName[[1]][2]),matrix(unlist(GluedgenVarEYT1),ncol=nCols,byrow=FALSE))
assign(paste0("genicVarEYT1",VarName[[1]][2]),matrix(unlist(GluedgenicVarEYT1),ncol=nCols,byrow=FALSE))

assign(paste0("genValPI",VarName[[1]][2]),matrix(unlist(GluedgenValPI),ncol=nCols,byrow=FALSE))
assign(paste0("genVarPI",VarName[[1]][2]),matrix(unlist(GluedgenVarPI),ncol=nCols,byrow=FALSE))
assign(paste0("genicVarPI",VarName[[1]][2]),matrix(unlist(GluedgenicVarPI),ncol=nCols,byrow=FALSE))

rm(list= ls()[!(ls() %in% c("VarName",paste0("genValDH",VarName[[1]][2]),paste0("genVarDH",VarName[[1]][2]),
                            paste0("genicVarDH",VarName[[1]][2]),paste0("genValPI",VarName[[1]][2]),
                            paste0("genVarPI",VarName[[1]][2]),paste0("genicVarPI",VarName[[1]][2]),
                            paste0("genValEYT1",VarName[[1]][2]),paste0("genVarEYT1",VarName[[1]][2]),
                            paste0("genicVarEYT1",VarName[[1]][2])))])

save.image(paste0("MultiPart_",VarName[[1]][2],"Combined.rda"))
  }
