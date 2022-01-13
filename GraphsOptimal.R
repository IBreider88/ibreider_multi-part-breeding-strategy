#Breider et al. 2022 DOI: 10.21203/rs.3.rs-1246254/v1
#
library(plyr)
library(ggplot2)

#colfunc <- colorRampPalette(c("lightskyblue", "dodgerblue4"))
#colfunc(10)
#plot(rep(1,6),col=colfunc(6),pch=19,cex=3)

setwd("~/Graphs")
#Baseline
load("~/MultiPart_Acc1nBridges0nCyclesBr6IR0BR0RR0ER1GenoInPI7200Combined.rda")

#Absolute best
load("~/MultiPart_Acc1nBridges2nCyclesBr6IR0.5BR0.45RR0.45ER1Combined.rda")

nReps=10
nScenarios=2

colorVec = c('black',rep("#2767A1",1))
lineType = c(rep("dashed",1),rep("solid",1))#,rep("dashed",4))

#"#87CEFA" "#6FB4E3" "#579ACD" "#3F81B7" "#2767A1" "#104E8B"

df = data.frame(Year=rep(0:100,nScenarios*nReps),
                Scenario=rep(c("1.TwoPartBaseline","2.MultiPartOptimal"), 
                             each=101*nReps),
                Mean=c(c(genValEYT1Acc1nBridges0nCyclesBr6IR0BR0RR0ER1GenoInPI7200),
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
ggsave(plot = Plot, filename = "OptimalScenY100_GenGainEYT1_7200.jpg", dpi = 350, height = 20, width = 30, unit = "cm") 

####
####

df = data.frame(Year=rep(1:100,nScenarios*nReps),
                Scenario=rep(c("1.TwoPartBaseline","2.MultiPartOptimal"),
                             each=100*nReps),
                Mean=c(c(genicVarEYT1Acc1nBridges0nCyclesBr6IR0BR0RR0ER1GenoInPI7200[2:101,]),
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
ggsave(plot = Plot, filename = "OptimalScenY100_GenicVarEYT1_7200.jpg", dpi = 350, height = 20, width = 30, unit = "cm") 


#########################

