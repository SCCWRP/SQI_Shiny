# WQI analyses
library(tidyverse)
library(mgcv)

# lookup table of bio BCG class, corresponding score, and combined categorical score
xwalk <- read.csv('raw/scoring_xwalk.csv', stringsAsFactors = F)

# all chem, bio, hab data
# select variables of interest
# convert bio to BCG classes
# join with BCG, score, and combined score table
# convert combined score to "good", "bad"
alldat <- read.csv('raw/CombinedData_112417.csv', stringsAsFactors = F) %>% 
  select(MasterID, SampleDate2, Latitude, Longitude, SMCShed, SMC_LU, csci_mean, h20_mean, PCT_SAFN, H_AqHab, H_SubNat, Ev_FlowHab, XCMG, indexscore_cram, Cond, TN2, TP) %>% 
  na.omit %>% 
  mutate(
    CSCI_class = cut(csci_mean, breaks = c(-Inf, 0.3, 0.6, 0.77, 1, Inf), labels = c(6, 5, 4, 3, 2)), 
    CSCI_class = as.numeric(as.character(CSCI_class)),
    H20_class = cut(h20_mean, breaks = c(-Inf, 25, 60, 90, Inf), labels = c(5, 4, 3, 2)),
    H20_class = as.numeric(as.character(H20_class))
  ) %>% 
  left_join(xwalk, by = c('CSCI_class', 'H20_class')) %>% 
  select(-bmiscore, -algscore, -BugMidpoint, -AlgMidpoint, -Bio_WP) %>% 
  mutate(
    Bio_pf = ifelse(Bio_BPJ > 0, 1, 0)
  )

# get calibration/validation datasets
set.seed(500)
mydf.t<- alldat %>% group_by(Bio_pf)
my.sites <- unique(mydf.t[, c('MasterID', 'Bio_pf')])
sites.cal <- sample_frac(my.sites, 0.75, replace = F) %>% 
  group_by('Bio_pf')
mydf.t <- mydf.t %>% 
  mutate(
    SiteSet = ifelse(MasterID %in% sites.cal$MasterID, 'Cal', 'Val')
  ) %>% 
  select(MasterID, SampleDate2, SiteSet)
alldat <- alldat %>% 
  left_join(mydf.t, by = c('MasterID', 'SampleDate2', 'Bio_pf'))

# separate cal, val data
caldat <- alldat %>% 
  filter(SiteSet %in% 'Cal')
valdat <- alldat %>% 
  filter(SiteSet %in% 'Val')

# models
wq_gam <- gam(Bio_pf ~ s(TN2, bs = 'tp') + s(TP, bs = 'tp') + s(Cond, bs = 'tp'),
              family = binomial('logit'), data = caldat)

hab_gam <- gam(Bio_pf ~ s(indexscore_cram, bs = 'tp') + s(PCT_SAFN, bs = 'tp') + s(H_AqHab, bs = 'tp') + s(H_SubNat, bs = 'tp') + s(Ev_FlowHab, bs = 'tp') + s(XCMG, bs = 'tp'),
               family = binomial('logit'), data = caldat)

# # predictions
# caldat$pChem <- predict(wq_gam, type = "response")
# valdat$pChem <- predict(wq_gam, newdata = valdat, type = "response")
# caldat$pHab <- predict(hab_gam, type = "response")
# valdat$pHab <- predict(hab_gam, newdata = valdat, type = "response")
# caldat$pChemHab <- caldat$pChem * caldat$pHab
# valdat$pChemHab <- valdat$pChem * valdat$pHab

# length values for sequence plot data
lenv <- 50

yvar <- 'PCT_SAFN'
xvar <- 'indexscore_cram'

# mod
mod <- 'hab_gam'

# # averages for ranges
# tmp <- caldat %>% 
#   select(MasterID, SampleDate2, indexscore_cram, PCT_SAFN, H_AqHab, H_SubNat, Ev_FlowHab, XCMG, TN2, TP, Cond) %>% 
#   gather('var', 'val', -MasterID, -SampleDate2) %>% 
#   group_by(var) %>% 
#   summarise(val = mean(val, na.rm = T))

# rng and avgs for habitat/wq variables
# averages from calibration data, all stations/dates
rng_vrs <- tibble( 
  var = c('indexscore_cram', 'PCT_SAFN', 'H_AqHab', 'H_SubNat', 'Ev_FlowHab', 'XCMG', 'TN2', 'TP', 'Cond'),
  minv = c(24, 0, 0, 0, 0, 0, 0, 0, 0),
  avev = c(69.3, 38, 1.33, 1.3, 0.548, 108, 1.92, 0.232, 1615),
  maxv = c(100, 100, 2.5, 2.5, 1, 264, 1.5, 1, 2000),
  modv = c('hab_gam', 'hab_gam', 'hab_gam', 'hab_gam', 'hab_gam', 'hab_gam', 'wq_gam', 'wq_gam', 'wq_gam')
  ) %>% 
  gather('rng', 'val', minv, avev, maxv)

habvrs <- c('indexscore_cram', 'PCT_SAFN', 'H_AqHab', 'H_SubNat', 'Ev_FlowHab', 'XCMG')
wqvrs <- c('TN2', 'TP', 'Cond')

# subset correct model
rng_vrs <- rng_vrs %>% 
  filter(modv %in% mod)

# get xy vars to plot
xy_vrs <- rng_vrs %>% 
  filter(var %in% c(xvar, yvar)) %>% 
  filter(!rng %in% 'avev') %>% 
  group_by(var) %>% 
  nest %>% 
  mutate(
    val = map(data, ~ seq(min(.$val), max(.$val), length.out = lenv))
  ) %>% 
  select(-data)

# get constant vars
cnt_vrs <- rng_vrs %>% 
  filter(!var %in% c(xvar, yvar)) %>% 
  filter(rng %in% 'avev') %>% 
  select(-modv, -rng)

# combined data to pred
prd_vrs <- rbind(xy_vrs, cnt_vrs) %>% 
  deframe %>% 
  expand.grid

# modelled response
rsp <- paste0('predict(', mod, ', newdata = prd_vrs, type = "response")')
rsp <- eval(parse(text = rsp))

# combined predictation data and response
toplo <- prd_vrs %>% 
  mutate(
    rsp = rsp
  )

p1 <- ggplot(toplo, aes_string(x = xvar, y = yvar)) +
  geom_tile(aes(fill = rsp)) +
  scale_fill_gradient2(low = "#d7191c", mid = "#ffffbf", high = "#2c7bb6", midpoint = 0.5) +
  theme_minimal(base_size = 14, base_family = 'serif') +
  theme(
    plot.title = element_text(size = 12), 
    legend.position = 'top'
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

p1

# to do
# functionize 
# add title value with constant variables
# add check for xvar, yvar inputs based on model input
# add option for user input of constant variables
# most importantly, check model fitts and comparison with random forest
# 