library(tidyverse)
library(readxl)
library(lubridate)

fls <- list.files('L:/SMC Regional Monitoring_ES/SMC_RM/Misc/Find missing samples/Data for Water Quality Index_021219/',
                  recursive = T, full.names = T)

list(fls)

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

mastid <- dat %>% 
  filter(grepl('^luStation', value)) %>% 
  pull(rawdat) %>% 
  .[[1]] %>% 
  select(StationID, MasterID) %>% 
  rename(StationCode = StationID)
      
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

cscidat <- dat %>% 
  filter(grepl('^CSCI', value)) %>% 
  pull(rawdat) %>% 
  .[[1]] %>% 
  select(StationCode, SampleYear, CSCI) %>% 
  rename(yr = SampleYear) %>% 
  group_by(StationCode, yr) %>% 
  summarize(csci_mean = mean(CSCI, na.rm = T)) %>% 
  gather('var', 'val', -StationCode, -yr)
  
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

ipidat <- dat %>% 
  filter(grepl('^IPI', value)) %>% 
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
  gather('var', 'val', -StationCode, -yr)

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
  gather('var', 'val', -StationCode, -yr)

tntpabc <- dat %>% 
  filter(grepl('^Compiled', value)) %>% 
  pull(rawdat) %>% 
  .[[1]] %>% 
  select(StationCode, SampleDate, AnalyteName, Result) %>% 
  filter(AnalyteName %in% c('SpecificConductivity', 'Phosphorus as P', 'Total_N_calculated', 'Total_n_calculated', 'Total_P_reported')) %>% 
  mutate(
    AnalyteName = case_when(
      AnalyteName %in% 'SpecificConductivity' ~ 'Cond', 
      AnalyteName %in% c('Phosphorus as P', 'Total_P_reported') ~ 'TP', 
      AnalyteName %in% c('Total_N_calculated', 'Total_n_calculated') ~ 'TN'
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
  gather('var', 'val', -StationCode, -yr)

alldat <- bind_rows(ascidat, cond, cramdat, cscidat, ipidat, tntpabc, tntpsmc) %>% 
  ungroup %>% 
  left_join(mastid, by  = 'StationCode') %>% 
  select(-StationCode) %>% 
  na.omit %>% 
  group_by(MasterID, yr, var) %>% 
  summarize(val = mean(val, na.rm = T)) %>% 
  spread(var, val)
  

