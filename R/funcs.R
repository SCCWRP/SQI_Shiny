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
