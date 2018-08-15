# descriptor colors for wqi
getdsccol <- function(dscin){
  
  if(is.factor(dscin))
    stop('dscin must be character')
  
  # relative severity levels
  l1 <- 'Green'
  l2 <- 'LightGreen'
  l3 <- 'LightBlue'
  l4 <- 'RoyalBlue'
  
  #  color categories
  overallcol <- list(
    'Healthy and unstressed' = l1, 
    'Healthy and resilient' = l2,
    'Impacted by unknown stress' = l3,
    'Impacted and stressed' = l4
  )                                   
  biologscol <- list(
    'Healthy' = l1,
    'Impacted for BMI' = l2,
    'Impacted for algae' = l3,
    'Impacted for BMI and algae' = l4
  )
  strsovrcol <- list(
    'Low' = l1, 
    'Moderate' = l3,
    'Severe' = l4
  )
  strsdetcol <- list(
    'Low stress' = l1, 
    'Stressed by chemistry degradation' = l2,
    'Stressed by habitat degradation' = l3,
    'Stressed by chemistry and habitat degradation' = l4, 
    'Stressed by low levels of chemistry or habitat degradation' = l1
  )
  
  allcol <- c(overallcol, biologscol, strsovrcol, strsdetcol)
  
  out <- allcol[[dscin]]
  
  return(out)
  
}

######
# WQI output
#
# datin is a data frame of required inputs 
# datin <- data.frame(
#   CSCI = input$csci,
#   ASCI = input$asci,
#   TN2 = input$tn,
#   TP = input$tp,
#   Cond = input$cond,
#   indexscore_cram = input$cram,
#   PCT_SAFN = input$safn,
#   H_AqHab = input$aqhab,
#   H_SubNat = input$subnat,
#   Ev_FlowHab = input$flowhab,
#   XCMG = input$veg
# )
#
# requires random forest models to get probability output
wqi <- function(datin){
  
  # probability of low stress, chem, hab, and overall
  datin$pChem<-predict(wq.rf.wp, newdata=datin, type="prob")[,2]
  datin$pHab<-predict(hab.rf.wp, newdata=datin, type="prob")[,2]
  datin$pChemHab<-datin$pChem*datin$pHab
  
  out <- datin %>%
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
    ) %>% 
    mutate_if(is.factor, as.character)
  
  return(out)

}