library(leaflet)
library(shinyjs)
library(shinyBS)
library(shinyCustom)
library(shinydashboard)
library(shinyWidgets)
library(ShinyDash)
library(tidyverse)
library(mapview)
library(rvest)

# watersheds to select from data folder
shds <- list.files('data') %>%
  str_subset('Santa Ana\\.RData$') %>% 
  gsub('\\.RData$|^scrs_|^spat_', '', .) %>%
  unique

# column padding global
pad <- 'padding:0px;'

# Define UI for application
shinyUI(fluidPage(
  
  theme = 'styles.css',

  
))


