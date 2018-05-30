library(randomForest)
library(ggplot2)
load("Data/Models/hab.rf.wp.Rdata")
load("Data/Models/wq.rf.wp.Rdata")

###User inputs
#Biology
csci.i<-0.45 #CSCI score. Enter a number between 0 and ~1.4
asci.i<-25 #ASCI score (H20, for now). Enter a number between 0 and 100
#Chemistry
tn.i<-10.1 #Total N (mg/L). Enter a number greater than zero
tp.i<- 10.001 #Total P (mg/L). Enter a number greater than zero
cond.i<-450 #Specific conductance (uS/cm). Enter a number greater than zero
#Habitat
cram.i<- 75 #CRAM score. Enter a number between 0 and 100.
safn.i<- 35 #% sands and fines. Enter a number between 0 and 100
aqhab.i<- 1.5 #SW diversity of aquatic habitats. Enter a number between 0 and ~2.5
subnat.i<- 1.9 #SW diversity of streambed substrate. Enter a number between 0 and ~2.5
flowhab.i<- 0.5 #Evenness of flow habitat types (e.g., riffles, pools). Enter a number between zero and 1
veg.i<- 200 #Riparian veg cover. Enter a number from 0 to ~265

mydf.prediction<-data.frame(
  CSCI=csci.i,
  ASCI=asci.i,
  TN2=tn.i,
  TP=tp.i,
  Cond=cond.i,
  indexscore_cram=cram.i,
  PCT_SAFN=safn.i,
  H_AqHab=aqhab.i,
  H_SubNat=subnat.i,
  Ev_FlowHab=flowhab.i,
  XCMG=veg.i
)

mydf.prediction$pChem<-predict(wq.rf.wp, newdata=mydf.prediction, type="prob")[,2]
mydf.prediction$pHab<-predict(hab.rf.wp, newdata=mydf.prediction, type="prob")[,2]
mydf.prediction$pChemHab<-mydf.prediction$pChem*mydf.prediction$pHab

mydf.prediction$BiologicalCondition<-ifelse(mydf.prediction$CSCI>=0.79 & mydf.prediction$ASCI>=60,"Healthy",
                                  ifelse(mydf.prediction$CSCI<0.79 & mydf.prediction$ASCI<60,"Impacted for BMI and algae",
                                         ifelse(mydf.prediction$CSCI<0.79 & mydf.prediction$ASCI>=60,"Impacted for BMI",
                                                ifelse(mydf.prediction$CSCI>=0.79 & mydf.prediction$ASCI<60,"Impacted for algae", "XXXXX"
                                                ))))
mydf.prediction$WaterChemistryCondition<-ifelse(mydf.prediction$pChem>=0.67,"Low stress",ifelse(mydf.prediction$pChem>=0.33,"Moderate stress","Severe stress"))
mydf.prediction$HabitatCondition<-ifelse(mydf.prediction$pHab>=0.67,"Low stress",ifelse(mydf.prediction$pHab>=0.33,"Moderate stress","Severe stress"))
mydf.prediction$OverallStressCondition<-ifelse(mydf.prediction$pChemHab>=0.67,"Low stress",ifelse(mydf.prediction$pChemHab>=0.33,"Moderate stress","Severe stress"))
mydf.prediction$OverallStressCondition_detail<-ifelse(mydf.prediction$pChemHab>=0.67,"Low stress",
                                            ifelse(mydf.prediction$pChemHab<0.67 & mydf.prediction$pHab<0.67, "Stressed by chemistry and habitat degradation",
                                                   ifelse(mydf.prediction$pChemHab<0.67 & mydf.prediction$pHab>=0.67, "Stressed by chemistry degradation",
                                                          ifelse(mydf.prediction$pChemHab>=0.67 & mydf.prediction$pHab<0.67, "Stressed by habitat degradation",
                                                                 ifelse(mydf.prediction$pChemHab>=0.67 & mydf.prediction$pHab>=0.67, "Stressed by low levels of chemistry or habitat degradation",
                                                                        "XXXXX")))))

mydf.prediction$StreamHealthIndex<-ifelse(mydf.prediction$BiologicalCondition=="Healthy" & mydf.prediction$OverallStressCondition=="Low stress","Healthy and unstressed",
                                ifelse(mydf.prediction$BiologicalCondition=="Healthy" & mydf.prediction$OverallStressCondition!="Low stress","Healthy and resilient",
                                       ifelse(mydf.prediction$BiologicalCondition!="Healthy" & mydf.prediction$OverallStressCondition=="Low stress","Impacted by unknown stress",
                                              ifelse(mydf.prediction$BiologicalCondition!="Healthy" & mydf.prediction$OverallStressCondition!="Low stress","Impacted and stressed",
                                                     "XXXXX"))))


print(paste("Stream Health Index-Overall:",mydf.prediction$StreamHealthIndex))
print(paste("Stream Health Index-Biological condition:",mydf.prediction$BiologicalCondition))
print(paste("Stream Health Index-Stress condition:",mydf.prediction$OverallStressCondition))
print(paste("Stream Health Index-Stress condition detail:",mydf.prediction$OverallStressCondition_detail))


len.x<-20
plot.dat.chem<-expand.grid(
  TN2=seq(from=0,to=5, length.out=len.x*2.5),
  TP=seq(from=0,to=2, length.out=len.x*2.5),
  Cond=seq(from=0,to=2000, length.out=len.x*2.5))

plot.dat.chem$pChem<-predict(wq.rf.wp, newdata=plot.dat.chem, type="prob")[,2]

#Select the subset of rows with similar conductivity
closest.cond<-plot.dat.chem$Cond[which.min(abs(plot.dat.chem$Cond - mydf.prediction$Cond))]
plot.dat.chem_tntp<-plot.dat.chem[which(plot.dat.chem$Cond==closest.cond),]
closest.tn<-plot.dat.chem$TN2[which.min(abs(plot.dat.chem$TN2 - mydf.prediction$TN2))]
plot.dat.chem_tpcond<-plot.dat.chem[which(plot.dat.chem$TN2==closest.tn),]
closest.tp<-plot.dat.chem$TP[which.min(abs(plot.dat.chem$TP - mydf.prediction$TP))]
plot.dat.chem_tncond<-plot.dat.chem[which(plot.dat.chem$TP==closest.tp),]

#TN-TP plot
my.pred.plot<-mydf.prediction
my.pred.plot$TN2<-pmin(max(plot.dat.chem$TN2),my.pred.plot$TN2)#*1.01
my.pred.plot$TP<-pmin(max(plot.dat.chem$TP),my.pred.plot$TP)#*1.01
my.pred.plot$Cond<-pmin(max(plot.dat.chem$Cond),my.pred.plot$Cond)#*1.01

ggplot(data=plot.dat.chem_tntp, aes(x=TN2, y=TP))+
  geom_tile(aes(fill=pChem))+
  scale_fill_gradient2(low="#d7191c", mid="#ffffbf", high="#2c7bb6", midpoint=0.5)+
  geom_point(data=my.pred.plot, shape=21, fill="white")+
  theme_minimal()+
  xlab("Total N (mg/L)")+ ylab("Total P (mg/L)")+
  ggtitle(paste("Sp Cond at",mydf.prediction$Cond))

ggplot(data=plot.dat.chem_tncond, aes(x=TN2, y=Cond))+
  geom_tile(aes(fill=pChem))+
  scale_fill_gradient2(low="#d7191c", mid="#ffffbf", high="#2c7bb6", midpoint=0.5)+
  geom_point(data=my.pred.plot, shape=21, fill="white")+
  theme_minimal()+
  xlab("Total N (mg/L)")+ ylab("Sp Cond (uS/cm)")

#Skip the habitat plot
plot.dat.hab<-expand.grid(
  indexscore_cram=seq(from=24,to=100, length.out=len.x/2),
  PCT_SAFN=seq(from=0,to=100, length.out=len.x/2),
  H_AqHab=seq(from=0,to=2.5, length.out=len.x/2),
  H_SubNat=seq(from=0,to=2.5, length.out=len.x/2),
  Ev_FlowHab=seq(from=0,to=1, length.out=len.x/2),
  XCMG=seq(from=0,to=264, length.out=len.x/2)
  )

plot.dat.hab$pHab<-predict(hab.rf.wp, newdata=plot.dat.hab, type="prob")[,2]
