#WQI analyses
library(tidyverse)

setwd("L:/SMC WQI_RM/Data/RawData/WQICalcs_112017")

# lookup table of bio BCG class, corresponding score, and combined categorical score
xwalk <- read.csv('scoring_xwalk.csv', stringsAsFactors = F)

# all chem, bio, hab data
# select variables of interest
# convert bio to BCG classes
# join with BCG, score, and combined score table
# convert combined score to "good", "bad"
alldat <- read.csv('CombinedData_112417.csv', stringsAsFactors = F) %>% 
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

library(randomForest)
set.seed(101)
wq.rf.bpj<-randomForest(y=as.factor(rf.cal[,"Bio_BPJ_pf"]),
                        x=rf.cal[,c("TN2","Cond","TP")],
                        ntree=1000, importance=T)

set.seed(103)
hab.rf.bpj<-randomForest(y=as.factor(rf.cal[,"Bio_BPJ_pf"]),
                         x=rf.cal[,c("indexscore_cram","PCT_SAFN","H_AqHab","H_SubNat","Ev_FlowHab","XCMG")],
                         ntree=1000, importance=T)


#Predictions
# rf.cal$pChem_bpj<-    predict(wq.rf.bpj, type="prob")[,2]
# rf.val$pChem_bpj<-    predict(wq.rf.bpj, newdata=rf.val, type="prob")[,2]
rf.cal$pChem_wp<-    predict(wq.rf.wp, type="prob")[,2]
rf.val$pChem_wp<-    predict(wq.rf.wp, newdata=rf.val, type="prob")[,2]
# rf.cal$pHab_bpj<-    predict(hab.rf.bpj, type="prob")[,2]
# rf.val$pHab_bpj<-    predict(hab.rf.bpj, newdata=rf.val, type="prob")[,2]
rf.cal$pHab_wp<-    predict(hab.rf.wp, type="prob")[,2]
rf.val$pHab_wp<-    predict(hab.rf.wp, newdata=rf.val, type="prob")[,2]
# rf.cal$pChemHab_wp<-    predict(chemhab.rf.wp, type="prob")[,2]
# rf.val$pChemHab_wp<-    predict(chemhab.rf.wp, newdata=rf.val, type="prob")[,2]
rf.cal$pChemHab_wp<-    rf.cal$pChem_wp * rf.cal$pHab_wp
rf.val$pChemHab_wp<-    rf.val$pChem_wp * rf.val$pHab_wp

rf.calval<-rbind.fill(rf.cal, rf.val)
