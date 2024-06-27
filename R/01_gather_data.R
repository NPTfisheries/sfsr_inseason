# Purpose: Compile data for SF Salmon River Chinook salmon to calculate
# in-season escapement estimates by origin, release group and year.
#
# Authors: Mike Ackerman and Ryan N. Kinzer 
# 
# Created: July 17, 2023
#   Modified: June 27, 2024

# clear environment
rm(list = ls())

# load necessary libraries
library(tidyverse)
#remotes::install_github("mackerman44/PITcleanr", ref = "npt_dev")
library(PITcleanr)
library(here)
library(janitor)

# load configuration file
load(here("data/configuration_files/site_config_LGR_20240304.rda"))
rm(flowlines, node_paths, parent_child, pc_nodes, sites_sf)
#load("data/config.rda")

# which spawn years to query
yrs <- 2010:2024

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
            mutate(trans_status = as.character(trans_status)), .id = "spawn_year")

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

# compile compressed obs, SY2010 - 2024
compress_obs = dart_obs_ls %>%
  map_dfr(. %>% 
            pluck("compress_obs"),
          .id = 'spawn_year') %>%
  left_join(mark_data)

# nodes of interest in south fork salmon river
sfsr_nodes = c("ESS_D", "ESS_U", "JOHNSC", "KNOXB", "KRS", "KRS_D", "KRS_U", "LAKEC", "MCCA", "SALSFW", "SFG", "STR", "ZEN_D", "ZEN_U")

# trim down to just tags observed within the sfsr, all years
sfsr_obs = compress_obs %>%
  group_by(tag_code) %>%
  filter(any(node %in% sfsr_nodes)) %>%
  ungroup() %>%
  mutate(node = recode(node,
                       MCCA = "STR",
                       SALSFW = "STR"))

# sfsr_tags = compress_obs %>%
#   filter(node %in% c("SFG", "ESS", "ZEN", "KRS", "STR", "SALSFW", "MCCA")) %>%
#   mutate(node = recode(node,
#                        MCCA = "STR",
#                        SALSFW = "STR")) %>%
#   distinct(tag_code) %>%
#   pull()

# now get all of the observations for those tags observed within the sfsr 
# i.e., we don't just want the sfsr observations
# sfsr_obs = compress_obs %>%
#   filter(tag_code %in% sfsr_tags)

# # write tag list
# write.table(sfsr_sy23_tags,
#             here("data/sfsr_sy23_tags.txt"),
#             quote = F,
#             row.names = F,
#             col.names = F,
#             sep = "\t")

# compressed observations for tags observed in SFSR, SY2024
# sfsr_sy23_obs = sfsr_obs %>%
#   filter(spawn_year == 2023)

# compressed observations for tags observed in SFSR, SY2024
# sfsr_sy24_obs = sfsr_obs %>%
#   filter(spawn_year == 2024)

# write out objects for analysis
save(dart_obs_ls,
     sfsr_obs,
     file = here("data/observations/sfsr_obs_20240627.rda"))

# END SCRIPT