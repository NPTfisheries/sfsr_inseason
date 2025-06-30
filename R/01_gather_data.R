# Purpose: Compile data for SF Salmon River Chinook salmon to calculate
# in-season escapement estimates by origin, release group and year.
#
# Authors: Mike Ackerman and Ryan N. Kinzer 
# 
# Created: July 17, 2023
#   Modified: June 30, 2025

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

# which spawn years to query
yrs = 2010:2025

# create list of DART observations, by spawn year, compressed
dart_obs_ls = map(.x = yrs,
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

# compile compressed obs and join the mark data
compress_obs = dart_obs_ls %>%
  map_dfr(. %>% 
            pluck("compress_obs"),
          .id = 'spawn_year') %>%
  left_join(mark_data)

# nodes of interest in south fork salmon river
sfsr_nodes = c("ESS_D", "ESS_U", "JOHNSC",       # East Fork South Fork Salmon
               "ZEN_D", "ZEN_U", "LAKEC",        # Secesh River
               "SFG", "KRS", "KRS_D", "KRS_U",   # South Fork Salmon Arrays
               "KNOXB", "MCCA", "SALSFW", "STR") # South Fork Salmon weir and MRR sites

# trim down to just tags observed within the sfsr, all years
sfsr_obs = compress_obs %>%
  group_by(tag_code) %>%
  filter(any(node %in% sfsr_nodes)) %>%
  ungroup() %>%
  # recode MCCA and SALSFW to be same as STR
  mutate(node = recode(node,
                       MCCA = "STR",
                       SALSFW = "STR"))

# write out objects for analysis
save(dart_obs_ls,
     sfsr_obs,
     file = paste0(here("data/observations/sfsr_obs_"), format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".rda"))

# END SCRIPT