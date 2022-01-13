#Breider et al. 2022 DOI: 10.21203/rs.3.rs-1246254/v1
#
library(plyr)
library(ggplot2)

#colfunc <- colorRampPalette(c("lightskyblue", "dodgerblue4"))
#colfunc(10)
#plot(rep(1,6),col=colfunc(6),pch=19,cex=3)

setwd("~/Graphs")
#Baselines
load("~/MultiPart_Acc0.5nBridges0nCyclesBr6IR0BR0RR0ER1GenoInPI7200Combined.rda")
load("~/MultiPart_Acc0.7nBridges0nCyclesBr6IR0BR0RR0ER1GenoInPI7200Combined.rda")
load("~/MultiPart_Acc1nBridges0nCyclesBr6IR0BR0RR0ER1GenoInPI7200Combined.rda")

#Absolute best
load("~/MultiPart_Acc1nBridges2nCyclesBr6IR0.5BR0.45RR0.45ER1Combined.rda")
#Acc variations
load("~/MultiPart_Acc0.7nBridges2nCyclesBr6IR0.5BR0.45RR0.45ER1Combined.rda")
load("~/MultiPart_Acc0.5nBridges2nCyclesBr6IR0.5BR0.45RR0.45ER1Combined.rda")

nReps=10
nScenarios=6

colorVec = c('gray74','gray48','black',"#87CEFA","#579ACD","#2767A1")
lineType = c(rep("dashed",3),rep("solid",3))#,rep("dashed",0))

#"#87CEFA" "#6FB4E3" "#579ACD" "#3F81B7" "#2767A1" "#104E8B"

df = data.frame(Year=rep(0:100,nScenarios*nReps),
                Scenario=rep(c("1a.TwoPartBaselineAcc0.5","1b.TwoPartBaselineAcc0.7","1c.TwoPartBaselineAcc1",
                               "2a.MultiPartAcc0.5","2b.MultiPartAcc0.7","2c.MultiPartAcc1"),
                             each=101*nReps),
                Mean=c(c(genValEYT1Acc0.5nBridges0nCyclesBr6IR0BR0RR0ER1GenoInPI7200),
                       c(genValEYT1Acc0.7nBridges0nCyclesBr6IR0BR0RR0ER1GenoInPI7200),
                       c(genValEYT1Acc1nBridges0nCyclesBr6IR0BR0RR0ER1GenoInPI7200),
                       c(genValEYT1Acc0.5nBridges2nCyclesBr6IR0.5BR0.45RR0.45ER1),
                       c(genValEYT1Acc0.7nBridges2nCyclesBr6IR0.5BR0.45RR0.45ER1),
                       c(genValEYT1Acc1nBridges2nCyclesBr6IR0.5BR0.45RR0.45ER1)))
tmp = ddply(df,c("Year","Scenario"), summarize,
            MEAN = mean(Mean),
            SE = sd(Mean)/sqrt(nReps))
Plot<-ggplot(tmp,aes(x=Year,y=MEAN,color=Scenario))+
  geom_ribbon(aes(x=Year,ymin=MEAN-SE,ymax=MEAN+SE,
                  fill=Scenario),alpha=0.2,linetype=0)+
  geom_line(aes(linetype=Scenario),size=1)+
  theme_bw()+
  theme(text=element_text(size = 30),legend.position = "none",axis.text = element_text(size = 30))+
  scale_color_manual(values=colorVec)+
  scale_fill_manual(values=colorVec)+
  scale_linetype_manual(values=lineType)+
  guides(alpha=FALSE)+
  scale_x_continuous(limits=c(0, 100))+
  scale_y_continuous(limits=c(0, 60))+
  xlab("Year")+
  ylab("Genetic Gain")
Plot
#last_plot()+aes(group=rev(Scenario))
ggsave(plot = Plot, filename = "OptimalScenY100_Acc_GenGainEYT1_7200.jpg", dpi = 350, height = 20, width = 30, unit = "cm") 

####
####

df = data.frame(Year=rep(1:100,nScenarios*nReps),
                Scenario=rep(c("1a.TwoPartBaselineAcc0.5","1b.TwoPartBaselineAcc0.7","1c.TwoPartBaselineAcc1",
                               "2a.MultiPartAcc0.5","2b.MultiPartAcc0.7","2c.MultiPartAcc1"),
                             each=100*nReps),
                Mean=c(c(genicVarEYT1Acc0.5nBridges0nCyclesBr6IR0BR0RR0ER1GenoInPI7200[2:101,]),
                       c(genicVarEYT1Acc0.7nBridges0nCyclesBr6IR0BR0RR0ER1GenoInPI7200[2:101,]),
                       c(genicVarEYT1Acc1nBridges0nCyclesBr6IR0BR0RR0ER1GenoInPI7200[2:101,]),
                       c(genicVarEYT1Acc0.5nBridges2nCyclesBr6IR0.5BR0.45RR0.45ER1[2:101,]),
                       c(genicVarEYT1Acc0.7nBridges2nCyclesBr6IR0.5BR0.45RR0.45ER1[2:101,]),
                       c(genicVarEYT1Acc1nBridges2nCyclesBr6IR0.5BR0.45RR0.45ER1[2:101,])))
tmp = ddply(df,c("Year","Scenario"), summarize,
            MEAN = mean(Mean),
            SE = sd(Mean)/sqrt(nReps))
Plot<-ggplot(tmp,aes(x=Year,y=MEAN,color=Scenario))+
  geom_ribbon(aes(x=Year,ymin=MEAN-SE,ymax=MEAN+SE,
                  fill=Scenario),alpha=0.2,linetype=0)+
  geom_line(aes(linetype=Scenario),size=1)+
  theme_bw()+
  theme(text=element_text(size = 30),legend.position = "none",axis.text = element_text(size = 30))+
  scale_color_manual(values=colorVec)+
  scale_fill_manual(values=colorVec)+
  scale_linetype_manual(values=lineType)+
  guides(alpha=FALSE)+
  scale_x_continuous(limits=c(0, 100))+
  scale_y_continuous(limits=c(0, 0.5))+
  xlab("Year")+
  ylab("Genic Variance")
Plot
#last_plot()+aes(group=rev(Scenario))
ggsave(plot = Plot, filename = "OptimalScenY100_Acc_GenicVarEYT1_7200.jpg", dpi = 350, height = 20, width = 30, unit = "cm") 


#########################
