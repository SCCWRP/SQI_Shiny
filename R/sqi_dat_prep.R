# Script pulling the SQI dataset from the SMC and reformatting it to align with Shiny dashboard script
# generally have to rename columns and clean dataset a bit

library(tidyverse)

sqi_raw <- read_csv("https://smcchecker.sccwrp.org/smc/sqi_rawdata")

sqidat_fordash <- sqi_raw %>% 
  # only keep data
  # column renmaing to match with names in index.Rmd
  rename(MasterID = masterid, yr = year, ASCI = d_asci_max, CSCI = csci, 
  