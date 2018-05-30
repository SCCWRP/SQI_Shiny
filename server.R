library(shiny)
library(tidyverse)
library(randomForest)
library(gridExtra)

# load rf models
load(file = 'data/hab.rf.wp.Rdata')
load(file = 'data/wq.rf.wp.Rdata')

# plot data
len.x<-20
plot.dat.chem<-expand.grid(
  TN2=seq(from=0,to=5, length.out=len.x*2.5),
  TP=seq(from=0,to=2, length.out=len.x*2.5),
  Cond=seq(from=0,to=2000, length.out=len.x*2.5))

plot.dat.chem$pChem<-predict(wq.rf.wp, newdata=plot.dat.chem, type="prob")[,2]

# server logic
server <- function(input, output, session) {
  
  # values to predict
  toprd <- reactive({
    
    out <- data.frame(
      CSCI = input$csci,
      ASCI = input$asci,
      TN2 = input$tn,
      TP = input$tp,
      Cond = input$cond,
      indexscore_cram = input$cram,
      PCT_SAFN = input$safn,
      H_AqHab = input$aqhab,
      H_SubNat = input$subnat,
      Ev_FlowHab = input$flowhab,
      XCMG = input$veg
    )
    
    return(out)
    
  })
  
  # categories from predictions
  cats <- reactive({
    
    mydf.prediction <- toprd()
    
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
  
    return(mydf.prediction)
    
  })
  
  # text output
  output$overall <- renderText(paste0('Overall: ', cats()$StreamHealthIndex))
  output$biolcon <- renderText(paste0('Biological condition: ', cats()$BiologicalCondition))
  output$strscon <- renderText(paste0('Stress condition: ', cats()$OverallStressCondition))
  output$strsdet <- renderText(paste0('Stress condition detail: ', cats()$OverallStressCondition_detail))

  # plots
  output$plos <- renderPlot({
    
    mydf.prediction <- cats()
    
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
    
    p1 <- ggplot(data=plot.dat.chem_tntp, aes(x=TN2, y=TP))+
      geom_tile(aes(fill=pChem))+
      scale_fill_gradient2(low="#d7191c", mid="#ffffbf", high="#2c7bb6", midpoint=0.5)+
      geom_point(data=my.pred.plot, shape=21, size = 4, fill="white")+
      theme_minimal(base_size = 14, base_family = 'serif') +
      xlab("Total N (mg/L)")+ ylab("Total P (mg/L)")+
      ggtitle(paste("Sp Cond at",mydf.prediction$Cond))
    
    p2 <- ggplot(data=plot.dat.chem_tncond, aes(x=TN2, y=Cond))+
      geom_tile(aes(fill=pChem))+
      scale_fill_gradient2(low="#d7191c", mid="#ffffbf", high="#2c7bb6", midpoint=0.5)+
      geom_point(data=my.pred.plot, shape=21, size = 4, fill="white")+
      theme_minimal(base_size = 14, base_family = 'serif') +
      xlab("Total N (mg/L)")+ ylab("Sp Cond (uS/cm)") + 
      ggtitle(paste("Total P at",mydf.prediction$TP))
    
    grid.arrange(p1, p2, ncol = 2)
    
  })
    
}