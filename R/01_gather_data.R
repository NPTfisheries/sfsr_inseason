# Purpose: Compile data for SF Salmon River Chinook salmon evaluation, calculate
# in-season escapement estimates.

# Authors: Mike Ackerman and Ryan N. Kinzer 
# 
# Created: July 17, 2023
# Modified:

# load necessary libraries
library(tidyverse)
library(PITcleanr)

# load configuration
load("data/config.rda")

# which spawn years to query
yrs <- 2010:2023

# create list of DART observations, by spawn year, compressed
dart_obs_ls <- map(.x = yrs,
                   .f = function(x){
                     compressDART(species = "Chinook",
                                  loc = "GRA",
                                  spawn_year = x,
                                  configuration = config)
    })

# assign names to dfs
names(dart_obs_ls) <- yrs

# extract the named data frame from the list of lists
compress_obs = dart_obs_ls %>%
  map_dfr(. %>%
            pluck("compress_obs"),
          .id = "spawn_year")

# tags observed in SFSR in SY2023
sfsr_sy23_tags = compress_obs %>%
  filter(spawn_year == 2023) %>%
  filter(node %in% c("SFG", "ESS", "ZEN", "KRS", "SALSFW", "STR")) %>%
  distinct(tag_code) %>%
  pull()

# write tag list
write.table(sfsr_sy23_tags,
            here("data/sfsr_sy23_tags.txt"),
            quote = F,
            row.names = F,
            col.names = F,
            sep = "\t")

# compressed observations for tags observed in SFSR
sfsr_sy23_obs = compress_obs %>%
  filter(tag_code %in% sfsr_sy23_tags)

# write out objects for analysis
save(dart_obs_ls,
     file = "data/dart_obs_ls.rda")
save(sfsr_sy23_obs,
     file = "data/sfsr_sy23_obs.rda")

