library(shiny)
library(tidyverse)
library(randomForest)
library(gridExtra)

# load rf models
load(file = 'data/hab.rf.wp.Rdata')
load(file = 'data/wq.rf.wp.Rdata')

# length values for sequence plot data
len.x<-20

# chem plot data
plot.dat.chem <- crossing(
  TN2=seq(from=0,to=1.5, length.out=len.x*2.5),
  TP=seq(from=0,to=1, length.out=len.x*2.5),
  Cond=seq(from=0,to=2000, length.out=len.x*2.5)
)

plot.dat.chem$pChem<-predict(wq.rf.wp, newdata=plot.dat.chem, type="prob")[,2]

# hab plot data
plot.dat.hab<-expand.grid(
  indexscore_cram=seq(from=24,to=100, length.out=len.x/2),
  PCT_SAFN=seq(from=0,to=100, length.out=len.x/2),
  H_AqHab=seq(from=0,to=2.5, length.out=len.x/2),
  H_SubNat=seq(from=0,to=2.5, length.out=len.x/2),
  Ev_FlowHab=seq(from=0,to=1, length.out=len.x/2),
  XCMG=seq(from=0,to=264, length.out=len.x/2)
)

plot.dat.hab$pHab<-predict(hab.rf.wp, newdata=plot.dat.hab, type="prob")[,2]

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
    
    # probability of low stress, chem, hab, and overall
    mydf.prediction$pChem<-predict(wq.rf.wp, newdata=mydf.prediction, type="prob")[,2]
    mydf.prediction$pHab<-predict(hab.rf.wp, newdata=mydf.prediction, type="prob")[,2]
    mydf.prediction$pChemHab<-mydf.prediction$pChem*mydf.prediction$pHab
    
    mydf.prediction <- mydf.prediction %>% 
      mutate(
        BiologicalCondition = ifelse(CSCI>=0.79 & ASCI>=60,"Healthy",
                                     ifelse(CSCI<0.79 & ASCI<60,"Impacted for BMI and algae",
                                            ifelse(CSCI<0.79 & ASCI>=60,"Impacted for BMI",
                                                   ifelse(CSCI>=0.79 & ASCI<60,"Impacted for algae", "XXXXX"
                                                   )))
                                     ),
        WaterChemistryCondition = cut(pChem, 
                                      breaks = c(-Inf, 0.33, 0.67, Inf), 
                                      labels = c('Severe', 'Moderate', 'Low'),
                                      right = F
                                      ),
        HabitatCondition = cut(pHab, 
                               breaks = c(-Inf, 0.67, Inf), 
                               labels = c('Severe', 'Low'),
                               right = F
                               ),
        OverallStressCondition = cut(pChemHab, 
                                     breaks = c(-Inf, 0.33, 0.67, Inf), 
                                     labels = c('Severe', 'Moderate', 'Low'),
                                     right = F
                                     ),
        OverallStressCondition_detail = ifelse(pChemHab>=0.67,"Low stress",
                                                          ifelse(pChem<0.67 & pHab<0.67, "Stressed by chemistry and habitat degradation",
                                                                 ifelse(pChem<0.67 & pHab>=0.67, "Stressed by chemistry degradation",
                                                                        ifelse(pChem>=0.67 & pHab<0.67, "Stressed by habitat degradation",
                                                                               ifelse(pChem>=0.67 & pHab>=0.67, "Stressed by low levels of chemistry or habitat degradation",
                                                                                      "XXXXX"))))
                                               ),
        StreamHealthIndex = ifelse(BiologicalCondition=="Healthy" & OverallStressCondition=="Low","Healthy and unstressed",
                                              ifelse(BiologicalCondition=="Healthy" & OverallStressCondition!="Low","Healthy and resilient",
                                                     ifelse(BiologicalCondition!="Healthy" & OverallStressCondition=="Low","Impacted by unknown stress",
                                                            ifelse(BiologicalCondition!="Healthy" & OverallStressCondition!="Low","Impacted and stressed",
                                                                   "XXXXX")))
                                   )
      )
  
    return(mydf.prediction)
    
  })
  
  # text output
  output$overall <- renderUI(HTML(paste0('<h4><i>Overall: </i>', cats()$StreamHealthIndex, '</h4>')))
  output$biolcon <- renderUI(HTML(paste0('<h4><i>Biological condition: </i>', cats()$BiologicalCondition, '</h4>')))
  output$strscon <- renderUI(HTML(paste0('<h4><i>Stress condition: </i>', cats()$OverallStressCondition, '</h4>')))
  output$strsdet <- renderUI(HTML(paste0('<h4><i>Stress condition detail: </i>', cats()$OverallStressCondition_detail, '</h4>')))
  output$pchemhab <- renderUI(HTML(paste0('<h4><i>Prob. of low  overall stress: </i>', round(100 * cats()$pChemHab), '%</h4>')))
  output$pchem <- renderUI(HTML(paste0('<h4><i>Prob. of low chemistry stress: </i>', round(100 * cats()$pChem), '%</h4>')))
  output$phab <- renderUI(HTML(paste0('<h4><i>Prob. of low habitat stress: </i>', round(100 * cats()$pHab), '%</h4>')))
  
  # chemistry wq plots
  output$plochem <- renderPlot({
    
    mydf.prediction <- cats()
    
    # names of variables to plot
    plovrs <- c(input$chem1, input$chem2)
    
    # names of variables to hold constant
    convrs <- names(plot.dat.chem)[!names(plot.dat.chem) %in% c(plovrs, 'pChem')]
    
    # selected conditional variables
    selcon <- mydf.prediction[, convrs, drop = F] %>% 
      gather('var', 'val')

    # estimated conditional variables, closest values to sel
    selconclo <- plot.dat.chem[, convrs, drop = F] %>% 
      gather('var', 'val') %>% 
      left_join(selcon, by = 'var') %>% 
      mutate(minv = abs(val.x - val.y)) %>% 
      group_by(var) %>% 
      filter(minv == min(minv)) %>% 
      unique %>% 
      filter(!duplicated(var)) %>% 
      select(var, val.x) %>% 
      rename(val = val.x) %>% 
      mutate(val = round(val, 5)) %>% 
      unite('con', var, val, sep = ' == ') %>% 
      unlist %>% 
      paste(collapse = ' & ')
    
    # filtered chem data to plot
    toplo <- plot.dat.chem %>% 
      mutate_if(is.numeric, round, 5) %>% 
      filter_(selconclo)

    # actual selected values and prob est
    my.pred.plot<-mydf.prediction
    my.pred.plot$TN2<-pmin(max(plot.dat.chem$TN2),my.pred.plot$TN2)
    my.pred.plot$TP<-pmin(max(plot.dat.chem$TP),my.pred.plot$TP)
    my.pred.plot$Cond<-pmin(max(plot.dat.chem$Cond),my.pred.plot$Cond)
    
    # title
    titlvl <- selcon %>% 
      unite('con', var, val, sep = ' ') %>% 
      unlist %>% 
      paste(collapse = ', ')
    
    p1 <- ggplot(data = toplo, aes_string(x=plovrs[1], y=plovrs[2]))+
      geom_tile(aes(fill=pChem))+
      scale_fill_gradient2(low="#d7191c", mid="#ffffbf", high="#2c7bb6", midpoint=0.5)+
      geom_point(data=my.pred.plot, shape=21, size = 5, fill="white")+
      theme_minimal(base_size = 14, base_family = 'serif') +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      ggtitle(paste0('Constant values: ', titlvl))
    
    p1
    
  })
  
  # chemistry wq plots
  output$plohab <- renderPlot({
    
    mydf.prediction <- cats()
    
    # names of variables to plot
    plovrs <- c(input$hab1, input$hab2)
    
    # names of variables to hold constant
    convrs <- names(plot.dat.hab)[!names(plot.dat.hab) %in% c(plovrs, 'pHab')]
    
    # selected conditional variables
    selcon <- mydf.prediction[, convrs, drop = F] %>% 
      gather('var', 'val')
    
    # estimated conditional variables, closest values to sel
    selconclo <- plot.dat.hab[, convrs, drop = F] %>% 
      gather('var', 'val') %>% 
      left_join(selcon, by = 'var') %>% 
      mutate(minv = abs(val.x - val.y)) %>% 
      group_by(var) %>% 
      filter(minv == min(minv)) %>% 
      unique %>% 
      filter(!duplicated(var)) %>% 
      select(var, val.x) %>% 
      rename(val = val.x) %>% 
      mutate(val = round(val, 5)) %>% 
      unite('con', var, val, sep = ' == ') %>% 
      unlist %>% 
      paste(collapse = ' & ')
    
    # filtered hab data to plot
    toplo <- plot.dat.hab %>% 
      mutate_if(is.numeric, round, 5) %>% 
      filter_(selconclo)

    # actual selected values and prob est
    my.pred.plot<-mydf.prediction
    my.pred.plot$indexscore_cram<-pmin(max(plot.dat.hab$indexscore_cram),my.pred.plot$indexscore_cram)
    my.pred.plot$PCT_SAFN<-pmin(max(plot.dat.hab$PCT_SAFN),my.pred.plot$PCT_SAFN)
    my.pred.plot$H_AqHab<-pmin(max(plot.dat.hab$H_AqHab),my.pred.plot$H_AqHab)
    my.pred.plot$H_SubNat<-pmin(max(plot.dat.hab$H_SubNat),my.pred.plot$H_SubNat)
    my.pred.plot$Ev_FlowHab<-pmin(max(plot.dat.hab$Ev_FlowHab),my.pred.plot$Ev_FlowHab)
    my.pred.plot$XCMG<-pmin(max(plot.dat.hab$XCMG),my.pred.plot$XCMG)
    
    # title
    titlvl <- selcon %>% 
      unite('con', var, val, sep = ' ') %>% 
      unlist %>% 
      paste(collapse = ', ')
    
    p1 <- ggplot(data = toplo, aes_string(x=plovrs[1], y=plovrs[2]))+
      geom_tile(aes(fill=pHab))+
      scale_fill_gradient2(low="#d7191c", mid="#ffffbf", high="#2c7bb6", midpoint=0.5)+
      geom_point(data=my.pred.plot, shape=21, size = 5, fill="white")+
      theme_minimal(base_size = 14, base_family = 'serif') +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      ggtitle(paste0('Constant values: ', titlvl))
    
    p1
    
  })
    
}