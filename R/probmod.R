# WQI analyses
library(tidyverse)

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
wq_mod <- gam(Bio_pf ~ s(TN2, bs = 'tp') + s(TP, bs = 'tp') + s(Cond, bs = 'tp'),
              family = binomial('logit'), data = caldat)

hab_mod <- gam(Bio_pf ~ s(indexscore_cram, bs = 'tp') + s(PCT_SAFN, bs = 'tp') + s(H_AqHab, bs = 'tp') + s(H_SubNat, bs = 'tp') + s(Ev_FlowHab, bs = 'tp') + s(XCMG, bs = 'tp'),
               family = binomial('logit'), data = caldat)

# # predictions
# caldat$pChem <- predict(wq_gam, type = "response")
# valdat$pChem <- predict(wq_gam, newdata = valdat, type = "response")
# caldat$pHab <- predict(hab_gam, type = "response")
# valdat$pHab <- predict(hab_gam, newdata = valdat, type = "response")
# caldat$pChemHab <- caldat$pChem * caldat$pHab
# valdat$pChemHab <- valdat$pChem * valdat$pHab

# length values for sequence plot data
lenv <- 200

yvar <- 'TP'
xvar <- 'TN2'

# mod
mod <- 'wq_mod'

# opt_vrs <- list(
#   indexscore_cram = 100,
#   PCT_SAFN = 25,
#   H_AqHab = 0,
#   H_SubNat = 1.3,
#   Ev_FlowHab = 0.17,
#   XCMG = 100
# )
opt_vrs <- list(
  TN2 = 2.22,
  TP = 0.156,
  Cond = 1139
)

# # averages for ranges
# tmp <- caldat %>% 
#   select(MasterID, SampleDate2, indexscore_cram, PCT_SAFN, H_AqHab, H_SubNat, Ev_FlowHab, XCMG, TN2, TP, Cond) %>% 
#   gather('var', 'val', -MasterID, -SampleDate2) %>% 
#   group_by(var) %>% 
#   summarise(val = mean(val, na.rm = T))

strs_surf(xvar, yvar, mod, opt_vrs = opt_vrs)
