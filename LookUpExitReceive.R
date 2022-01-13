#Breider et al. 2022 DOI: 10.21203/rs.3.rs-1246254/v1
#
#This script sets the variables exit and NextReceives depending on the number of breeding cycles per year and exit rate.
#exit is used to determine whether germplasm leaves a breeding component in this cycle, NextReceives determines whether the next cycle receives germplasm.
#When number of breeding cycles > exit rate, germplasm leaves the breeding components in the later cycles. 
#E.g. 6 cycles/year with an exit rate of 1 means germplasm leaves the breeding components in the last cycle of the year (cycle 6).

#Six cycles a year
if((nCyclesBr==6) & (ExitRate==1)){
  if(i<6){exit=FALSE}else{exit=TRUE}
  if(i==6){NextReceives=TRUE}else{NextReceives=FALSE}
}
if((nCyclesBr==6) & (ExitRate==2)){
  if(i<5){exit=FALSE}else{exit=TRUE}
  if(i>=5){NextReceives=TRUE}else{NextReceives=FALSE}
}
if((nCyclesBr==6) & (ExitRate==3)){
  if(i<4){exit=FALSE}else{exit=TRUE}
  if(i>=4){NextReceives=TRUE}else{NextReceives=FALSE}
}
if((nCyclesBr==6) & (ExitRate==4)){
  if(i<3){exit=FALSE}else{exit=TRUE}
  if(i>=3){NextReceives=TRUE}else{NextReceives=FALSE}
}
if((nCyclesBr==6) & (ExitRate==5)){
  if(i<2){exit=FALSE}else{exit=TRUE}
  if(i>=2){NextReceives=TRUE}else{NextReceives=FALSE}
}
#Five cycles a year
if((nCyclesBr==5) & (ExitRate==1)){
  if(i<5){exit=FALSE}else{exit=TRUE}
  if(i==5){NextReceives=TRUE}else{NextReceives=FALSE}
}
if((nCyclesBr==5) & (ExitRate==2)){
  if(i<4){exit=FALSE}else{exit=TRUE}
  if(i>=4){NextReceives=TRUE}else{NextReceives=FALSE}
}
if((nCyclesBr==5) & (ExitRate==3)){
  if(i<3){exit=FALSE}else{exit=TRUE}
  if(i>=3){NextReceives=TRUE}else{NextReceives=FALSE}
}
if((nCyclesBr==5) & (ExitRate==4)){
  if(i<2){exit=FALSE}else{exit=TRUE}
  if(i>=2){NextReceives=TRUE}else{NextReceives=FALSE}
}
#Four cycles a year
if((nCyclesBr==4) & (ExitRate==1)){
  if(i<4){exit=FALSE}else{exit=TRUE}
  if(i==4){NextReceives=TRUE}else{NextReceives=FALSE}
}
if((nCyclesBr==4) & (ExitRate==2)){
  if(i<3){exit=FALSE}else{exit=TRUE}
  if(i>=3){NextReceives=TRUE}else{NextReceives=FALSE}
}
if((nCyclesBr==4) & (ExitRate==3)){
  if(i<2){exit=FALSE}else{exit=TRUE}
  if(i>=2){NextReceives=TRUE}else{NextReceives=FALSE}
}
#Three cycles a year
if((nCyclesBr==3) & (ExitRate==1)){
  if(i<3){exit=FALSE}else{exit=TRUE}
  if(i==3){NextReceives=TRUE}else{NextReceives=FALSE}
}
if((nCyclesBr==3) & (ExitRate==2)){
  if(i<2){exit=FALSE}else{exit=TRUE}
  if(i>=2){NextReceives=TRUE}else{NextReceives=FALSE}
}
#Two cycles a year
if((nCyclesBr==2) & (ExitRate==1)){
  if(i<2){exit=FALSE}else{exit=TRUE}
  if(i==2){NextReceives=TRUE}else{NextReceives=FALSE}
}
#When nCycles==ExitRate all is TRUE
if((nCyclesBr==ExitRate)){
  exit=TRUE
  NextReceives=TRUE
}
