library(tidyverse)
library(readxl)
library(lubridate)
library(randomForest)

# import ------------------------------------------------------------------

fls <- list.files('raw/rawdig/',
                  recursive = T, full.names = T)

dat <- fls %>% 
  enframe %>% 
  group_by(value) %>% 
  mutate(
    rawdat = purrr::map(value, function(x){
      
      # import, wrangle data
      raw <- read_excel(x)
      
      return(raw)
      
    })
  ) %>% 
  select(-name) %>% 
  ungroup %>% 
  mutate(value = basename(value))

# masterid ----------------------------------------------------------------

mastid <- dat %>% 
  filter(grepl('^luStation', value)) %>% 
  pull(rawdat) %>% 
  .[[1]] %>% 
  select(StationID, MasterID) %>% 
  rename(StationCode = StationID)

# ASCI --------------------------------------------------------------------

ascidat <- dat %>% 
  filter(grepl('^asci', value)) %>% 
  pull(rawdat) %>% 
  .[[1]] %>% 
  filter(ind %in% 'MMI') %>% 
  filter(tax %in% 'Hybrid') %>% 
  rename(
    asci_mean = scr
  ) %>% 
  select(sampleid, asci_mean) %>% 
  mutate(
    StationCode = gsub('^(.*)_.*_.*$', '\\1', sampleid),
    date = gsub('^.*_(.*)_.*$', '\\1', sampleid), 
    smps = gsub('^.*_.*_(.*)$', '\\1', sampleid),
    date = mdy(date),
    yr = year(date),
    asci_mean = as.numeric(asci_mean)
  ) %>% 
  group_by(StationCode, yr) %>% 
  summarize(asci_mean = mean(asci_mean, na.rm = T)) %>% 
  gather('var', 'val', -StationCode, -yr)


# CSCI --------------------------------------------------------------------

cscidat <- dat %>% 
  filter(grepl('^CSCI', value)) %>% 
  pull(rawdat) %>% 
  .[[1]] %>% 
  select(StationCode, SampleYear, CSCI) %>% 
  rename(yr = SampleYear) %>% 
  group_by(StationCode, yr) %>% 
  summarize(csci_mean = mean(CSCI, na.rm = T)) %>% 
  gather('var', 'val', -StationCode, -yr)


# CRAM --------------------------------------------------------------------

cramdat <- dat %>% 
  filter(grepl('^CRAM', value)) %>% 
  pull(rawdat) %>% 
  .[[1]] %>% 
  select(StationCode, visitdate, indexscore, bs, blc, hy, ps) %>% 
  mutate(yr = year(visitdate)) %>% 
  group_by(StationCode, yr) %>% 
  summarize(
    indexscore_cram = mean(indexscore, na.rm = T),
    bs = mean(bs, na.rm = T),
    blc = mean(blc, na.rm = T),
    hy = mean(hy, na.rm = T),
    ps = mean(ps, na.rm = T)
  ) %>% 
  gather('var', 'val', -StationCode, -yr)


# IPI ---------------------------------------------------------------------

# new ipi data
ipifeb <- dat %>% 
  filter(grepl('^IPI\\s', value)) %>% 
  pull(rawdat) %>% 
  .[[1]] %>% 
  dplyr::select(StationCode, SampleDate, IPI, Ev_FlowHab_score, H_AqHab_score, H_SubNat_score, PCT_SAFN_score, XCMG_score) %>% 
  mutate(
    yr = year(SampleDate)
  ) %>% 
  dplyr::select(-SampleDate) %>% 
  gather('var', 'val', -StationCode, -yr) %>% 
  mutate(
    var = gsub('\\_score$', '', var)
  ) %>% 
  spread(var, val) %>% 
  gather('var', 'val', -StationCode, -yr)

# old ipi data
ipidec <- dat %>% 
  filter(grepl('^ipiscr', value)) %>% 
  pull(rawdat) %>% 
  .[[1]] %>% 
  dplyr::select(StationCode, SampleDate, IPI, Ev_FlowHab_score, H_AqHab_score, H_SubNat_score, PCT_SAFN_score, XCMG_score) %>% 
  mutate(
    yr = year(SampleDate)
  ) %>% 
  dplyr::select(-SampleDate) %>% 
  gather('var', 'val', -StationCode, -yr) %>% 
  mutate(
    var = gsub('\\_score$', '', var)
  ) 

# all ipi
ipidat <- rbind(ipifeb, ipidec) %>% 
  group_by(StationCode, yr, var) %>% 
  summarise(val = mean(val, na.rm = T))

# nutrients ---------------------------------------------------------------

cond <- dat %>% 
  filter(grepl('^SpecCond', value)) %>% 
  pull(rawdat) %>% 
  .[[1]] %>% 
  select(StationCode, SampleDate, Result) %>% 
  mutate(
    yr = year(SampleDate), 
    Result = as.numeric(Result)
  ) %>% 
  group_by(StationCode, yr) %>% 
  summarise(Cond = mean(Result, na.rm = T)) %>% 
  gather('var', 'val', -StationCode, -yr) %>% 
  na.omit

tntpsmc <- dat %>% 
  filter(grepl('^nutrient', value)) %>% 
  pull(rawdat) %>% 
  .[[1]] %>% 
  select(stationcode, sampledate, total_n_mgl, total_p_mgl) %>% 
  mutate(
    yr = year(sampledate)
  ) %>% 
  rename(
    StationCode = stationcode
  ) %>% 
  group_by(StationCode, yr) %>% 
  summarize(
    TN = mean(total_n_mgl, na.rm = T),
    TP = mean(total_p_mgl, na.rm = T)
  ) %>% 
  gather('var', 'val', -StationCode, -yr) %>% 
  na.omit

tntpabc <- dat %>% 
  filter(grepl('^Compiled', value)) %>% 
  pull(rawdat) %>% 
  .[[1]] %>% 
  select(StationCode, SampleDate, AnalyteName, Result) %>% 
  filter(AnalyteName %in% c('SpecificConductivity', 'Phosphorus as P', 'Total_N_calculated', 'Total_n_calculated', 'Total_P_reported', 'Total_N_partial', 'Total_N_Partial', 'Total_P_Partial')) %>% 
  mutate(
    AnalyteName = case_when(
      AnalyteName %in% 'SpecificConductivity' ~ 'Cond', 
      AnalyteName %in% c('Phosphorus as P', 'Total_P_reported', 'Total_P_partial') ~ 'TP', 
      AnalyteName %in% c('Total_N_calculated', 'Total_n_calculated', 'Total_N_partial', 'Total_N_Partial') ~ 'TN'
    ), 
    SampleDate = case_when( # ignore warnings here
      is.na(as.numeric(SampleDate)) ~ dmy(SampleDate), 
      !is.na(as.numeric(SampleDate)) ~ as.Date(as.numeric(SampleDate), origin = '1899-12-30')
    ), 
    yr = year(SampleDate)
  ) %>% 
  group_by(StationCode, yr, AnalyteName) %>% 
  summarise(Result = mean(Result, na.rm = T)) %>% 
  spread(AnalyteName, Result) %>% 
  gather('var', 'val', -StationCode, -yr) %>% 
  na.omit

# all nutrients
nutdat <- rbind(cond, tntpsmc, tntpabc) %>% 
  group_by(StationCode, yr, var) %>% 
  summarise(val = mean(val, na.rm = T))

# combine all -------------------------------------------------------------

alldat <- bind_rows(cscidat, ascidat, cramdat, ipidat, nutdat) %>%
  ungroup %>%
  left_join(mastid, by  = 'StationCode') %>%
  select(-StationCode) %>%
  na.omit %>% 
  group_by(MasterID, yr, var) %>%
  summarize(val = mean(val, na.rm = T)) %>%
  spread(var, val) %>% 
  na.omit

dim(alldat)

# combine all
alldatprp <- alldat %>%  
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

# models, glm
wqglm <- glm(Bio_pf ~ log10(1 + TN2) + log10(1 + TP) + Cond,
             family = binomial('logit'), data = caldat)

habglm <- glm(Bio_pf ~ indexscore_cram + PCT_SAFN + H_AqHab + H_SubNat + Ev_FlowHab + XCMG,
               family = binomial('logit'), data = caldat)

# save
save(wqglm, file = '../SQI_doc/data/wqglm.RData', compress = 'xz')
save(habglm, file = '../SQI_doc/data/habglm.RData', compress = 'xz')

# sample data for package
sampdat <- alldat %>% 
  select(MasterID, date, csci_mean, asci_mean,IPI, PCT_SAFN, H_AqHab, H_SubNat, Ev_FlowHab, XCMG, indexscore_cram, Cond, TN2, TP, SiteSet) %>% 
  rename(
    CSCI = csci_mean, 
    ASCI = asci_mean
  )
  
save(sampdat, file = '../SQI/data/sampdat.RData', compress = 'xz')
