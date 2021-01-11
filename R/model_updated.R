# SQI model recalibration script
# January 11, 2021
# Heili Lowman

# All of the below code has been taken from Marcus' original script found here: https://github.com/SCCWRP/SQI_Doc/blob/master/R/dat_proc.R

# Load packages
library(tidyverse)
library(sf)
library(randomForest)

# create sqi mods ---------------------------------------------------------

# Load in dataset created by SQL query on January 11, 2021.
# This dataset will be considered "static" and only run once for model creation.
# Note, the below code uses the SQL column names; the shiny app will rename them in its code
sqidat <- read_csv("https://smcchecker.sccwrp.org/smc/sqi_rawdata") %>% 
  
  # group by and retain ID columns
  group_by(masterid, yr, cnty, watershed, regional_board, latitude, longitude, comid) %>% 
  
  # calculate the means of all indices measure multiple times
  summarise(across(d_asci:ps, mean, na.rm= TRUE)) %>% 
  
  # ALWAYS ungroup after grouping
  ungroup() %>%
  
  # add CSCI classification column
  mutate(CSCI_rc = cut(csci, breaks = c(-Inf, 0.63, 0.79, 0.92, Inf), 
    labels = c('vla', 'la', 'pa', 'li')), 
    CSCI_rc = as.character(CSCI_rc)) %>%
  
  # add ASCI classification column
  mutate(ASCI_rc = cut(d_asci, breaks = c(-Inf, 0.70, 0.83, 0.93, Inf), 
    labels = c('vla', 'la', 'pa', 'li')),
    ASCI_rc = as.character(ASCI_rc)) %>%
  
  # add BPJ score column (based on Beck et al., 2019, Table 1)
  mutate(Bio_BPJ = case_when(d_asci >= 0.93 & csci >= 0.92 ~ 5,
    d_asci >= 0.93 & csci >= 0.79 & csci < 0.92 ~ 3,
    d_asci >= 0.93 & csci >= 0.63 & csci < 0.79 ~ -1,
    d_asci >= 0.93 & csci < 0.63 ~ -2,
    d_asci >= 0.83 & d_asci < 0.93 & csci >= 0.92 ~ 3,
    d_asci >= 0.83 & d_asci < 0.93 & csci >= 0.79 & csci < 0.92 ~ 2,
    d_asci >= 0.83 & d_asci < 0.93 & csci >= 0.63 & csci < 0.79 ~ -2,
    d_asci >= 0.83 & d_asci < 0.93 & csci < 0.63 ~ -4,
    d_asci >= 0.70 & d_asci < 0.83 & csci >= 0.92 ~ -1,
    d_asci >= 0.70 & d_asci < 0.83 & csci >= 0.79 & csci < 0.92 ~ -2,
    d_asci >= 0.70 & d_asci < 0.83 & csci >= 0.63 & csci < 0.79 ~ -3,
    d_asci >= 0.70 & d_asci < 0.83 & csci < 0.63 ~ -5,
    d_asci < 0.70 & csci >= 0.92 ~ -2,
    d_asci < 0.70 & csci >= 0.79 & csci < 0.92 ~ -4,
    d_asci < 0.70 & csci >= 0.63 & csci < 0.79 ~ -5,
    d_asci < 0.70 & csci < 0.63 ~ -6)) %>%
  
  # add additional bio score column.
  mutate(bio_fp = ifelse(Bio_BPJ < 0, 1, 0))

# get calibration/validation datasets
set.seed(500)

# want both passing and failing sites in calibration and validation datasets
mydf.t<- sqidat %>% group_by(bio_fp)

my.sites <- unique(mydf.t[, c('masterid', 'bio_fp')])

sites.cal <- sample_frac(my.sites, 0.75, replace = F) %>% 
  group_by('bio_fp')

mydf.t <- mydf.t %>% 
  mutate(SiteSet = ifelse(masterid %in% sites.cal$masterid, 'Cal', 'Val')) %>% 
  select(masterid, yr, SiteSet)

# adds SiteSet column to dataset
sqidat <- sqidat %>% 
  left_join(mydf.t, by = c('masterid', 'yr', 'bio_fp'))

# separate cal, val data
caldat <- sqidat %>% 
  filter(SiteSet %in% 'Cal')

valdat <- sqidat %>% 
  filter(SiteSet %in% 'Val')

# models, glm
# the following models use the same variables as those in Beck et al., 2019
# bio_fp is pass/fail
# 'logit' makes it logistic regression

# pChem
wqglm <- glm(bio_fp ~ log10(0.1 + tn) + log10(0.01 + tp) + conductivity,
  family = binomial('logit'), data = caldat)

# pHab
habglm <- glm(bio_fp ~ hy + pct_safn + xcmg, 
  family = binomial('logit'), data = caldat)

# save to project 
save(wqglm, file = 'data/wqglm2021.RData', compress = 'xz')
save(habglm, file = 'data/habglm2021.RData', compress = 'xz')

# End of script.