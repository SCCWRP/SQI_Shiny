# descriptor colors for wqi
getdsccol <- function(dscin = NULL, palout = F, palfac = NULL){
  
  if(is.factor(dscin))
    stop('dscin must be character')

  # relative severity levels
  l1 <- '#008000' #'Green'
  l2 <- '#90EE90' #'LightGreen'
  l3 <- '#FFB6C1' #'LightPink'
  l4 <- '#DC143C' #'Crimson'
  
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
    'Impacted for BMI' = l2,
    'Impacted for algae' = l3,
    'Impacted for BMI and algae' = l4
  )
  OverallStressCondition <- list(
    'Low' = l1, 
    'Moderate' = l3,
    'Severe' = l4
  )
  OverallStressCondition_detail <- list(
    'Low stress' = l1, 
    'Stressed by chemistry degradation' = l2,
    'Stressed by habitat degradation' = l3,
    'Stressed by chemistry and habitat degradation' = l4, 
    'Stressed by low levels of chemistry or habitat degradation' = l1
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

# plot stress surface
strs_surf_dev <- function(xvar, yvar, mod_in, mod = c('hab_mod', 'wq_mod'), title = TRUE, lenv = 200, opt_vrs = NULL, low = "#2c7bb6", mid = "#ffffbf", high = "#d7191c"){

  # get mod arg
  mod <- match.arg(mod)

  # hab and wq vars
  hab_vrs <- c('indexscore_cram', 'PCT_SAFN', 'H_AqHab', 'H_SubNat', 'Ev_FlowHab', 'XCMG')
  wq_vrs <- c('TN2', 'TP', 'Cond')

  # rng and avgs for habitat/wq variables
  # averages from calibration data, all stations/dates
  rng_vrs <- tibble::tibble(
    var = c(hab_vrs, wq_vrs),
    minv = c(24, 0, 0, 0, 0, 0, 0, 0, 0),
    avev = c(69.3, 38, 1.33, 1.3, 0.548, 108, 1.92, 0.232, 1615),
    maxv = c(100, 100, 2.5, 2.5, 1, 264, 1.5, 1, 2000),
    modv = c('hab_mod', 'hab_mod', 'hab_mod', 'hab_mod', 'hab_mod', 'hab_mod', 'wq_mod', 'wq_mod', 'wq_mod')
  ) %>%
    gather('rng', 'val', minv, avev, maxv)

  ## sanity checks
  # habitat
  if(mod == 'hab_mod'){

    # chk xvar and yvar are in hab_vrs
    chk <- any(!c(xvar, yvar) %in% hab_vrs)
    if(chk)
      stop('xvar and yvar must be one of ', paste(hab_vrs, collapse = ', '))

    # check the optional variables if provided
    if(!is.null(opt_vrs)){

      # check if names are right
      chk <- any(!names(opt_vrs) %in% hab_vrs)
      if(chk)
        stop('Names in opt_vrs must match those in ', paste(hab_vrs, collapse = ', '))

    }

    # water quality
  } else {

    # chk xvar and yvar are in wq_vrs
    chk <- any(!c(xvar, yvar) %in% wq_vrs)
    if(chk)
      stop('xvar and yvar must be one of ', paste(wq_vrs, collapse = ', '))

    # check the optional variables if provided
    if(!is.null(opt_vrs)){

      # check if names are right
      chk <- any(!names(opt_vrs) %in% wq_vrs)
      if(chk)
        stop('Names in opt_vrs must match those in ', paste(wq_vrs, collapse = ', '))

    }

  }

  # replace values in rng_vrs with those in opt_vrs
  # get probabilty from model for point
  if(!is.null(opt_vrs)){

    # opt_vrs in correct format
    opt_vrs <- opt_vrs %>%
      enframe('var', 'avev') %>%
      unnest() %>%
      gather('rng', 'val', avev)

    # join rng_vars with opt_vrs and replace
    rng_vrs <- rng_vrs %>%
      left_join(opt_vrs, by = c('var', 'rng')) %>%
      mutate(val = ifelse(is.na(val.y), val.x, val.y)) %>%
      dplyr::select(-val.x, -val.y)

    # data from opt_vrs to plot as single point
    # ceiling and floor by ranges in rng_vars
    toprd <- rng_vrs %>%
      spread(rng, val) %>%
      mutate(
        avev = pmin(avev, maxv),
        avev = pmax(avev, minv)
      ) %>%
      filter(var %in% opt_vrs$var) %>%
      dplyr::select(var, avev) %>%
      spread(var, avev)

  }

  # subset correct model
  rng_vrs <- rng_vrs %>%
    filter(modv %in% mod)

  # get xy vars to plot
  xy_vrs <- rng_vrs %>%
    filter(var %in% c(xvar, yvar)) %>%
    filter(!rng %in% 'avev') %>%
    group_by(var) %>%
    nest() %>%
    mutate(
      val = purrr::map(data, ~ seq(min(.$val), max(.$val), length.out = lenv))
    ) %>%
    dplyr::select(-data)

  # get constant vars
  cnt_vrs <- rng_vrs %>%
    filter(!var %in% c(xvar, yvar)) %>%
    filter(rng %in% 'avev') %>%
    dplyr::select(-modv, -rng)

  # combined data to pred
  prd_vrs <- rbind(xy_vrs, cnt_vrs) %>%
    deframe() %>%
    expand.grid

  # modelled response surface
  if(inherits(mod_in, 'gam'))
    rsp <- predict(mod_in, newdata = prd_vrs, type = "response")
  if(inherits(mod_in, 'randomForest'))
    rsp <- predict(mod_in, newdata = prd_vrs, type = "prob")[,2]
  rsp <- 1 - rsp

  # combined predictation data and response
  toplo <- prd_vrs %>%
    mutate(
      `Pr. stress` = rsp
    )

  # the plot
  p <- ggplot(toplo, aes_string(x = xvar, y = yvar)) +
    geom_tile(aes(fill = `Pr. stress`)) +
    # geom_contour(aes(z = `Pr. stress`), colour = 'black', linetype = 'dashed')  +
    scale_fill_gradient2(low = low, mid = mid, high = high, midpoint = 0.5) +
    theme_minimal(base_size = 14, base_family = 'serif') +
    theme(
      plot.title = element_text(size = 12),
      legend.position = 'top'
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    guides(fill = guide_colourbar(barheight = 0.5, barwidth = 10))

  # title
  if(title){

    titlvl <- cnt_vrs %>%
      mutate(val = round(val, 1)) %>%
      unite('con', var, val, sep = ' ') %>%
      unlist %>%
      paste(collapse = ', ') %>%
      paste0('Constant values:\n\t ', .)

    # add actual
    if(!is.null(opt_vrs)){

      selcvl <- opt_vrs %>%
        filter(var %in% c(xvar, yvar)) %>%
        mutate(val = round(val, 1)) %>%
        unite('con', var, val, sep = ' ') %>%
        dplyr::select(-rng) %>%
        unlist %>%
        paste(collapse = ', ') %>%
        paste0('Selected values:\n\t ', .)

      titlvl <- paste0(selcvl, '\n\n', titlvl)

      # add point with predicted value
      p <- p +
        geom_point(data = toprd, pch = 21, fill = 'white', size = 4)

    }

    # add title to plot
    p <- p + ggtitle(titlvl)

  }

  return(p)

}
