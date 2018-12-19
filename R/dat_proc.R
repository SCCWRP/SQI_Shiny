library(dplyr)
library(tidyr)
library(sf)
library(SQI)

prj <- 4326 # geographic wgs84

######
# get all existing data 
# obs for repeat visits are averaged for each site

# input data and sqi scores as sf object
alldatavg <- read.csv('//172.16.1.5/Biology/SMC SQI_RM/Data/RawData/SMC_WQIapp_051618/WQI_App/alldata.csv', stringsAsFactors = F) %>% 
  mutate(date = as.Date(SampleDate2, format = '%m/%d/%Y')) %>% 
  select(MasterID, date, Latitude, Longitude, csci_mean, h20_mean, PCT_SAFN, H_AqHab, H_SubNat, Ev_FlowHab, XCMG, indexscore_cram, Cond, TN2, TP) %>% 
  rename(
    CSCI = csci_mean,
    ASCI = h20_mean
  ) %>% 
  gather('var', 'val', -MasterID, -date) %>%
  group_by(MasterID, var) %>% 
  summarise(val = mean(val, na.rm = T)) %>% 
  spread(var, val) %>% 
  sqi(wq_mod_in = wqgam, hab_mod_in = habgam) %>%
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = prj)

######
# get all existing data 
# same as above but repeats not averaged

# input data and sqi scores as sf object
alldat <- read.csv('//172.16.1.5/Biology/SMC SQI_RM/Data/RawData/SMC_WQIapp_051618/WQI_App/alldata.csv', stringsAsFactors = F) %>% 
  mutate(date = as.Date(SampleDate2, format = '%m/%d/%Y')) %>% 
  select(MasterID, date, Latitude, Longitude, csci_mean, h20_mean, PCT_SAFN, H_AqHab, H_SubNat, Ev_FlowHab, XCMG, indexscore_cram, Cond, TN2, TP) %>% 
  rename(
    CSCI = csci_mean,
    ASCI = h20_mean
  ) %>% 
  sqi(wq_mod_in = wqgam, hab_mod_in = habgam) %>%
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = prj)

######
# county and smc shed data data, intersected with sample data above

data(alldatavg)
data(alldat)

# county
cntys <- st_read('S:/Spatial_Data/CA_Counties/cnty24k97.shp') %>% 
  st_transform(crs = prj) %>% 
  .[alldatavg, ] %>% 
  select(NAME) %>% 
  rename(cnty = NAME) %>% 
  mutate_if(is.factor, as.character)

# sheds, as sf
sheds <- st_read('S:/Spatial_Data/SMCBasefiles/Boundaries/SMCSheds/SMCSheds2009/SMCSheds2009.shp') %>%
  st_as_sf %>% 
  st_transform(crs = prj) %>% 
  select(SMC_Name) %>% 
  mutate_if(is.factor, as.character)

# get intersection with sample data
alldat <- alldat %>% 
  st_intersection(cntys) %>% 
  st_intersection(sheds)
alldatavg <- alldatavg %>% 
  st_intersection(cntys) %>% 
  st_intersection(sheds)

save(cntys, file = 'data/cntys.RData', compress = 'xz')
save(sheds, file = 'data/sheds.RData', compress = 'xz')
save(alldat, file = 'data/alldat.RData', compress = 'xz')
save(alldatavg, file = 'data/alldatavg.RData', compress = 'xz')
