# Purpose: Compile data for SF Salmon River Chinook salmon, SY2023, to harvest estimates relative to abundance
# estimates.
# 
# Authors: Mike Ackerman and Ryan N. Kinzer 
# 
# Created: July 17, 2023
# Modified:

# clear environment
rm(list = ls())

# load necessary packages
library(tidyverse)
library(PITcleanr)
library(here)
library(magrittr)

#---------------------
# gather PIT-tag data

# get site configuration info from PTAGIS
# config = buildConfig()
# save(config, file = here("data/config.rda"))
load(here("data/config.rda"))

# SF Salmon River nodes of interest
nodes_of_interest = c("SFG", "KRS", "KNOXB")
sf_salmon_config = config %>%
  filter(node %in% c("GRA", nodes_of_interest))

# get all observations from DART for adults at GRA and upstream (includes newly and previously tagged fish)
sy2023_chnk_obs = queryObsDART(species = "Chinook",
                               loc = "GRA",
                               spawn_year = 2023) %>%
  group_by(tag_id) %>%
  filter(any(obs_site %in% nodes_of_interest))

# unique tags from all tags observed at SFG and KRS
sy2023_chnk_tags = sy2023_chnk_obs %>%
  select(tag_id) %>%
  distinct()

# write to .txt for upload to PTAGIS
write.table(sy2023_chnk_tags,
            here("data/sf_salmon_sy2023_chnk_tag_list.txt"),
            quote = F,
            row.names = F,
            col.names = F,
            sep = "\t")

# now query CTHs for sf_tags in PTAGIS

# read in CTH file
sy2023_chnk_cth = readCTH("data/sf_salmon_chnk_sy2023_cth.csv")

# compress observations
sy2023_chnk_comp_cth = compress(cth_file = sy2023_chnk_cth,
                                file_type = "PTAGIS",   
                                max_minutes = NA,
                                configuration = sf_salmon_config,
                                units = "days",
                                ignore_event_vs_release = TRUE)

# isolate only SF Salmon CTH observations
parent_child = tribble(~"parent", ~"child",
                       "GRA", "SFG",
                       "SFG", "KRS")

# add nodes i.e., arrays
pc_nodes = addParentChildNodes(parent_child, config)

# add directionality and indicate whether each detection should be kept
sy2023_chnk_comp_cth = filterDetections(compress_obs = sy2023_chnk_comp_cth,
                                        parent_child = pc_nodes,
                                        max_obs_date = NULL)

# write out objects for analysis
save(sy2023_chnk_tags,
     sy2023_chnk_obs,
     sy2023_chnk_cth,
     sy2023_chnk_comp_cth,
     file = here("data/sy2023_sf_salmon_chnk_data.rda"))

### END SCRIPT
