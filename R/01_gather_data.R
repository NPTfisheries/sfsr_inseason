# Purpose: Compile data for SF Salmon River Chinook salmon evaluation, calculate
# in-season escapement estimates.

# Authors: Mike Ackerman and Ryan N. Kinzer 
# 
# Created: July 17, 2023
# Modified:

library(tidyverse)
library(PITcleanr)

load("data/config.rda")

yrs <- 2010:2023

dart_obs_ls <- map(.x = yrs,
           .f = function(x){
    compressDART(species = 'Chinook',
                 loc = 'GRA',
                 spawn_year = x,
                 configuration = config)
    })

names(dart_obs_ls) <- yrs

# Extract the named data frame from the list of lists
compress_obs <- dart_obs_ls %>%
  map_dfr(. %>% pluck("compress_obs"), .id = 'spawn_year')

sfsr_tags <- compress_obs %>%
  filter(site_code %in% c('SFG', 'ESS', 'ZEN', 'KRS', 'SALSFW', 'STR')) %>%
  distinct(tag_code) %>%
  pull()

sfsr_obs <- compress_obs %>%
  filter(tag_code %in% sfsr_tags)

save(dart_obs_ls,
     file = "data/dart_obs_ls.rda")

save(sfsr_obs,
     file = "data/sfsr_obs.rda")