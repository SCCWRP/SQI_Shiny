# create models of probability of stressed biological condition
# saved in sqi package repo

library(tidyverse)
library(mgcv)
library(randomForest)

# lookup table of bio BCG class, corresponding score, and combined categorical score
xwalk <- read.csv('raw/scoring_xwalk.csv', stringsAsFactors = F)

# all chem, bio, hab data
# select variables of interest
# convert bio to BCG classes
# join with BCG, score, and combined score table
# convert combined score to "good", "bad"
# bio_bf, 1 is "good" biology, 0 is "poor" biology
alldat <- read.csv('raw/CombinedData_112417.csv', stringsAsFactors = F) %>% 
  select(MasterID, SampleDate2, Latitude, Longitude, SMCShed, SMC_LU, csci_mean, h20_mean, PCT_SAFN, H_AqHab, H_SubNat, Ev_FlowHab, XCMG, indexscore_cram, Cond, TN2, TP) %>% 
  na.omit %>% 
  mutate(
    CSCI_class = cut(csci_mean, breaks = c(-Inf, 0.3, 0.6, 0.77, 1, Inf), labels = c(6, 5, 4, 3, 2)), 
    CSCI_class = as.numeric(as.character(CSCI_class)),
    ASCI_class = cut(h20_mean, breaks = c(-Inf, 25, 60, 90, Inf), labels = c(5, 4, 3, 2)),
    ASCI_class = as.numeric(as.character(ASCI_class))
  ) %>% 
  left_join(xwalk, by = c('CSCI_class', 'ASCI_class')) %>% 
  select(-bmiscore, -algscore, -Bio_WP) %>% 
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

# models, gam
wqgam <- gam(Bio_pf ~ s(TN2, bs = 'tp') + s(TP, bs = 'tp') + s(Cond, bs = 'tp'),
             family = binomial('logit'), data = caldat)

habgam <- gam(Bio_pf ~ s(indexscore_cram, bs = 'tp') + s(PCT_SAFN, bs = 'tp') + s(H_AqHab, bs = 'tp') + s(H_SubNat, bs = 'tp') + s(Ev_FlowHab, bs = 'tp') + s(XCMG, bs = 'tp'),
              family = binomial('logit'), data = caldat)

# # models, glm
# wqglm <- glm(Bio_pf ~ log10(1 + TN2) + log10(1 + TP) + Cond,
#              family = binomial('logit'), data = caldat)
#   
# habglm <- glm(Bio_pf ~ indexscore_cram + PCT_SAFN + H_AqHab + H_SubNat + Ev_FlowHab + XCMG,
#                family = binomial('logit'), data = caldat)
# 
# save(wqglm, file = '../SQI_doc/data/wqglm.RData', compress = 'xz')
# save(habglm, file = '../SQI_doc/data/habglm.RData', compress = 'xz')

# models, rf
set.seed(102)
wqrf <- randomForest(y = as.factor(caldat$Bio_pf),
                          x = caldat[, c('TN2', 'Cond', 'TP')], 
                          ntree = 1000, importance = T)

set.seed(104)
habrf <- randomForest(y = as.factor(caldat$Bio_pf), 
                           x = caldat[, c('indexscore_cram', 'PCT_SAFN', 'H_AqHab', 'H_SubNat', 'Ev_FlowHab', 'XCMG')], 
                           ntree = 1000, importance = T)

##
# save models
wqgam <- save(wqgam, file = '../SQI/data/wqgam.RData', compress = 'xz')
habgam <- save(habgam, file = '../SQI/data/habgam.RData', compress = 'xz')
wqrf <- save(wqrf, file = '../SQI/data/wqrf.RData', compress = 'xz')
habrf <- save(habrf, file = '../SQI/data/habrf.RData', compress = 'xz')


