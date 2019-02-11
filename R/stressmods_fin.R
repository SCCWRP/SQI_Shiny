# create models of probability of stressed biological condition
# saved in sqi package repo
library(tidyverse)
library(mgcv)
library(randomForest)
library(lubridate)
library(readxl)

# lookup table of bio BCG class, corresponding score, and combined categorical score
xwalk <- read.csv('raw/scoring_xwalk.csv', stringsAsFactors = F)

##
# all chem, bio, hab

# original file from rafi
orig <- read.csv('raw/CombinedData_112417.csv', stringsAsFactors = F) %>% 
  select(MasterID, SampleDate2, Latitude, Longitude, indexscore_cram, csci_mean, Cond, TN2, TP) %>% 
  mutate(
    date = mdy(SampleDate2),
    yr = year(date)
  ) %>% 
  dplyr::select(-SampleDate2, -date)

# getting asci
asci <- read.csv('raw/asci.scores.csv', stringsAsFactors = FALSE) %>% 
  dplyr::select(X, MMI.hybrid) %>% 
  rename(
    sampleid = X,
    asci_mean = MMI.hybrid
  ) %>% 
  mutate(
    MasterID = gsub('^(.*)_.*_.*$', '\\1', sampleid),
    date = gsub('^.*_(.*)_.*$', '\\1', sampleid), 
    smps = gsub('^.*_.*_(.*)$', '\\1', sampleid),
    date = mdy(date),
    yr = year(date)
  ) %>% 
  group_by(MasterID, yr) %>% 
  summarize(asci_mean = mean(asci_mean, na.rm = T))

# getting ipi scores, phab scores
ipiscr <- read.csv('raw/ipiscr.csv', stringsAsFactors = F) %>% 
  dplyr::select(StationCode, SampleDate, IPI, Ev_FlowHab_score, H_AqHab_score, H_SubNat_score, PCT_SAFN_score, XCMG_score) %>% 
  rename(
    MasterID = StationCode,
    date = SampleDate
  ) %>% 
  mutate(
    yr = mdy(date), 
    yr = year(yr)
  ) %>% 
  dplyr::select(-date) %>% 
  gather('var', 'val', -MasterID, -yr) %>% 
  mutate(
    var = gsub('\\_score$', '', var)
  ) %>% 
  spread(var, val)

# cram, pull out data from original file, import new, combine
crmold <- orig %>% 
  dplyr::select(MasterID, yr, indexscore_cram) %>% 
  na.omit
crmdat <- read_excel('raw/Copy of CRAM_SMC_20190122_for SWAMP_ACR_rdm.xlsx', sheet = 'Index and Attribute Scores') %>% 
  dplyr::select(StationCode, visitdate, indexscore) %>% 
  rename(
    MasterID = StationCode, 
    date = visitdate,
    indexscore_cram = indexscore
  ) %>% 
  mutate(
    yr = as.Date(date),
    yr = year(yr)
  ) %>% 
  bind_rows(crmold) %>% 
  group_by(MasterID, yr) %>% 
  summarize(indexscore_cram = mean(indexscore_cram, na.rm = T)) %>% 
  unique

# combine all
alldat <- orig %>%
  select(-indexscore_cram) %>%  # combined with crmdat above
  full_join(orig, asci, by = c('MasterID', 'yr')) %>%
  full_join(ipiscr, by = c('MasterID', 'yr')) %>%
  full_join(crmdat, by = c('MasterID', 'yr')) %>% 
  na.omit %>% 
  mutate(
    CSCI_class = cut(csci_mean, breaks = c(-Inf, 0.325, 0.625, 0.825, 1.025, Inf), labels = c(6, 5, 4, 3, 2)), 
    CSCI_class = as.numeric(as.character(CSCI_class)),
    ASCI_class = cut(asci_mean, breaks = c(-Inf, 0.3, 0.67, 0.97, 1.23, Inf), labels = c(6, 5, 4, 3, 2)),
    ASCI_class = as.numeric(as.character(ASCI_class))
  ) %>% 
  left_join(xwalk, by = c('CSCI_class', 'ASCI_class')) %>% 
  select(-bmiscore, -algscore, -Bio_WP) %>% 
  mutate(
    bio_fp = ifelse(Bio_BPJ <= 0, 1, 0)
  )

# get calibration/validation datasets
set.seed(500)
mydf.t<- alldat %>% group_by(bio_fp)
my.sites <- unique(mydf.t[, c('MasterID', 'bio_fp')])
sites.cal <- sample_frac(my.sites, 0.75, replace = F) %>% 
  group_by('bio_fp')
mydf.t <- mydf.t %>% 
  mutate(
    SiteSet = ifelse(MasterID %in% sites.cal$MasterID, 'Cal', 'Val')
  ) %>% 
  select(MasterID, date, SiteSet)
alldat <- alldat %>% 
  left_join(mydf.t, by = c('MasterID', 'date', 'bio_fp'))

# separate cal, val data
caldat <- alldat %>% 
  filter(SiteSet %in% 'Cal')
valdat <- alldat %>% 
  filter(SiteSet %in% 'Val')

# models, gam
wqgam <- gam(bio_fp ~ s(log10(1+TN2), bs = 'tp') + s(log10(1 + TP), bs = 'tp') + s(Cond, bs = 'tp'),
             family = binomial('logit'), data = caldat)

habgam <- gam(bio_fp ~ s(indexscore_cram, bs = 'tp') + s(IPI, bs = 'tp'),
              family = binomial('logit'), data = caldat)

# save models
save(wqgam, file = '../SQI/data/wqgam.RData', compress = 'xz')
save(habgam, file = '../SQI/data/habgam.RData', compress = 'xz')

# sample data for package
sampdat <- alldat %>% 
  select(MasterID, date, csci_mean, asci_mean,IPI, PCT_SAFN, H_AqHab, H_SubNat, Ev_FlowHab, XCMG, indexscore_cram, Cond, TN2, TP, SiteSet) %>% 
  rename(
    CSCI = csci_mean, 
    ASCI = asci_mean
  )
  
save(sampdat, file = '../SQI/data/sampdat.RData', compress = 'xz')
