library(dplyr)
library(tidyr)
library(sf)
library(SQI)

prj <- 4326 # geographic wgs84

data(sqidat)

# spatial data ------------------------------------------------------------

# county and smc shed data data, intersected with sample data above

# county
cntys <- st_read('S:/Spatial_Data/CA_Counties/cnty24k97.shp') %>% 
  st_transform(crs = prj) %>% 
  .[sqidat, ] %>% 
  select(NAME) %>% 
  rename(cnty = NAME) %>% 
  mutate_if(is.factor, as.character)

# sheds, as sf
sheds <- st_read('S:/Spatial_Data/SMCBasefiles/Boundaries/SMCSheds/SMCSheds2009/SMCSheds2009.shp') %>%
  st_as_sf %>% 
  st_transform(crs = prj) %>% 
  select(SMC_Name) %>% 
  mutate_if(is.factor, as.character)

# regional water quality board boundaries
rwqbs <- st_read('S:/Spatial_Data/RWQCBdistricts/rwqcbnda.shp') %>%
  st_as_sf %>% 
  st_transform(crs = prj) %>% 
  select(RBNAME) %>% 
  filter(RBNAME %in% c('Los Angeles', 'San Diego', 'Santa Ana')) %>% 
  group_by(RBNAME) %>% 
  summarize() %>% 
  mutate_if(is.factor, as.character)

# get intersection with sample data
sqidat <- sqidat %>% 
  st_intersection(cntys) %>% 
  st_intersection(sheds) %>% 
  st_intersection(rwqbs)

save(cntys, file = 'data/cntys.RData', compress = 'xz')
save(sheds, file = 'data/sheds.RData', compress = 'xz')
save(rwqbs, file = 'data/rwqbs.RData', compress = 'xz')
save(sqidat, file = 'data/sqidat.RData', compress = 'xz')
