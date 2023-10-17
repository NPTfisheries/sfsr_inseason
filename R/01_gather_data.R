# Purpose: Compile data for SF Salmon River Chinook salmon evaluation, calculate
# in-season escapement estimates.

# Authors: Mike Ackerman and Ryan N. Kinzer 
# 
# Created: July 17, 2023
#   Modified: October 17, 2023

# load necessary libraries
library(tidyverse)
library(PITcleanr)
library(here)

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
                                  configuration = configuration)
    })

# assign names to dfs
names(dart_obs_ls) = yrs

# extract the named data frame from the list of lists
dart_obs = dart_obs_ls %>%
  map_dfr(. %>% 
            pluck("dart_obs") %>% 
            mutate(trans_status = as.character(trans_status)), .id = 'spawn_year')

# extract mark data
mark_data = dart_obs %>%
  #filter(event_type_name == 'Mark') %>% wasn't getting every tag for some reason
  select(tag_code, 
         file_id, 
         contains('mark_'), 
         contains('event_'), 
         contains('rel_'), 
         flags) %>%
  select(-event_type_name) %>%
  distinct(tag_code, 
           .keep_all = TRUE)

# compile compressed obs, SY2010 - 2023
compress_obs = dart_obs_ls %>%
  map_dfr(. %>% 
            pluck("compress_obs"), 
          .id = 'spawn_year') %>%
  left_join(mark_data)

# trim down to just tags observed within the sfsr, all years
sfsr_tags = compress_obs %>%
  filter(node %in% c("SFG", "ESS", "ZEN", "KRS", "STR", "SALSFW", "MCCA")) %>%
  mutate(node = recode(node,
                       MCCA = "STR",
                       SALSFW = "STR")) %>%
  distinct(tag_code) %>%
  pull()

# now get all of the observations for those tags observed within the sfsr 
# i.e., we don't just want the sfsr observations
sfsr_obs = compress_obs %>%
  filter(tag_code %in% sfsr_tags)

# # write tag list
# write.table(sfsr_sy23_tags,
#             here("data/sfsr_sy23_tags.txt"),
#             quote = F,
#             row.names = F,
#             col.names = F,
#             sep = "\t")

# compressed observations for tags observed in SFSR, SY2023
sfsr_sy23_obs = sfsr_obs %>%
  filter(spawn_year == 2023)

# write out objects for analysis
save(dart_obs_ls,
     file = "data/dart_obs_ls.rda")

save(sfsr_sy23_obs,
     file = "data/sfsr_sy23_obs.rda")

save(sfsr_obs,
     file = "data/sfsr_obs.rda")
