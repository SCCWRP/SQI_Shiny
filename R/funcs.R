# descriptor colors for sqi
getdsccol <- function(dscin = NULL, palout = F, palfac = NULL){
  
  if(is.factor(dscin))
    stop('dscin must be character')

  # relative severity levels
  l1 <- '#008000' #'Green'
  l2 <- '#90EE90' #'LightGreen'
  l3 <- '#FFB6C1' #'LightPink'
  l4 <- '#DC143C' #'Crimson'
  
  # additional cols for specific sqi categories
  l5 <- '#4F94CD' #'steelblue3'
  l6 <- '#00C5CD' #'turquoise3'
  l7 <- '#8B0000' #'red4'
  
  if(palout) return(c(l1, l2, l3, l4))

  #  color categories
  StreamHealthIndex <- list(
    'Healthy and unstressed' = l1, 
    'Healthy and resilient' = l2,
    'Impacted by unknown stress' = l3,
    'Impacted and stressed' = l4
  )                                   
  BiologicalCondition <- list(
    'Healthy' = l1,
    'Impacted for BMI' = l5,
    'Impacted for algae' = l6,
    'Impacted for BMI and algae' = l4
  )
  OverallStressCondition <- list(
    'Low' = l1, 
    'Moderate' = l5,
    'Severe' = l4
  )
  OverallStressCondition_detail <- list(
    'Low stress' = l1, 
    'Stressed by chemistry degradation' = l5,
    'Stressed by habitat degradation' = l6,
    'Stressed by chemistry and habitat degradation' = l4, 
    'Stressed by low levels of chemistry or habitat degradation' = l7
  )

  # return individual palette with label is provided
  if(!is.null(palfac)){
    out <- get(palfac) %>% 
      unlist %>% 
      data.frame(nms = names(.), col = ., stringsAsFactors = F)
    return(out)
  }
  
  # get all
  allcol <- c(StreamHealthIndex, BiologicalCondition, OverallStressCondition, OverallStressCondition_detail)
  allcol <- unlist(allcol)
  
  # select colors
  out <- allcol[dscin]
  
  return(out)
  
}

# get relative distribution boxplots
#
# catslng observed data for selected site, all variables
# dstdat all observed data for sites in selected region
# selvr chr str of selected variables to plot
# collims limits defining categories for selected variables
# pal_exp color palette function for categories
#
dst_fun <- function(catslng, dstdat, selvr, collims, pal_exp){
  
  # make plots
  out <- dstdat %>% 
    filter(var %in% selvr) %>% 
    rename(
      distval = val
      ) %>% 
    mutate(
      var = factor(var, levels = selvr)
    ) %>% 
    arrange(var) %>% 
    group_by(var) %>% 
    nest %>% 
    left_join(collims, by = 'var') %>% 
    left_join(catslng, by = 'var') %>% 
    mutate(
      plos = purrr::pmap(list(as.character(var), data, lims, val), function(var, data, lims, val){
        
        # color limits
        toplo <- data 
        
        # setup limits labels and breaks for increasing/decreasing
        lbs <- c('lo', 'md', 'hi')
        brks <- c(-Inf, lims[2], lims[3], Inf)
        if(lims[1] > lims[2]){
          lbs <- rev(lbs)
          brks <- c(Inf, lims[2], lims[3], -Inf) 
        }
        valcat <- cut(val, breaks = brks, labels = lbs)
        
        # boxplot
        p <- ggplot(toplo, aes(x = factor(var), y = distval)) + 
          geom_boxplot(fill = scales::alpha('#99ccff', 0.3)) + 
          geom_hline(aes(yintercept = val), colour = pal_exp(valcat), size = 3, alpha = 0.7) +
          geom_hline(aes(yintercept = val), colour = pal_exp(valcat), size = 1, alpha = 1) +
          theme_bw() + 
          theme(
            axis.title = element_blank(), 
            legend.position = 'none'
          ) +
          xlab(var)
        
        if(var %in% c('Total nitrogen', 'Total phosphorus'))
          p <- p + scale_y_log10()
        
        return(p)
        
      })
    ) %>% 
    select(plos)
  
  return(out)
  
}