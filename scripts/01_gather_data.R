# Purpose: Retrieve data for SF Salmon River Chinook salmon to calculate
#   in-season escapement estimates by origin, release group and year.
#
# Authors: Mike Ackerman and Ryan N. Kinzer 
# 
# Created: July 17, 2023
#   Last Modified: June 5, 2026

# clear environment
rm(list = ls())

# load necessary libraries
library(tidyverse)
library(lubridate)
library(here)
#remotes::install_github("mackerman44/PITcleanr", ref = "npt_dev")
library(PITcleanr)

#---------
# metadata
update_dttm  = now(tzone = "America/Boise")
current_year = year(update_dttm) 

#----------------
# set directories
dart_obs_dir = here("data/dart_obs")
comp_obs_dir = here("data/comp_obs")
out_dir      = here("data")

walk(
  c(dart_obs_dir, comp_obs_dir, out_dir),
  dir.create,
  recursive    = TRUE,
  showWarnings = FALSE
)

#-------------------
# site configuration
load(here("data/configuration_files/site_config_LGR_20260116.rda"))
rm(crb_sites_sf, flowlines, parent_child, sr_site_pops)

#-----------------------------------
# all spawn years desired to include
sy_all = tibble(
  species  = "Chinook",
  spawn_yr = 2010:current_year
) %>%
  mutate(
    spc_code = "chnk",
    dart_file_name = paste0(spc_code, "_sy", spawn_yr, "_dart_obs.rds"),
    comp_file_name = paste0(spc_code, "_sy", spawn_yr, "_comp_obs.rds"),
    dart_file_path = file.path(dart_obs_dir, dart_file_name),
    comp_file_path = file.path(comp_obs_dir, comp_file_name)
  )

#------------------------
# choose years to refresh

# Option 1: refresh all years
# spawn_yrs_to_retrieve = 2010:current_year

# Option 2: refresh current year only
spawn_yrs_to_retrieve = current_year

sy_retrieve = sy_all %>%
  filter(spawn_yr %in% spawn_yrs_to_retrieve)

#---------------------------------------------
# nodes of interest in South Fork Salmon River
sfsr_nodes = c(
  "ESS_D", "ESS_U", "JOHNSC",        # East Fork South Fork Salmon
  "ZEN_D", "ZEN_U", "LAKEC",         # Secesh River
  "SFG", "KRS", "KRS_D", "KRS_U",    # South Fork Salmon Arrays
  "KNOXB", "MCCA", "SALSFW", "STR"   # South Fork Salmon weir and MRR sites
)

#----------------
# helper function
retrieve_sfsr_chinook_sy = function(yr,
                                    dart_file_path,
                                    comp_file_path,
                                    nodes  = sfsr_nodes,
                                    config = configuration) {
  
  message("Retrieving and compressing SF Salmon Chinook observations, spawn year ", yr)
  
  dart_out = compressDART(
    species       = "Chinook",
    loc           = "GRA",
    spawn_year    = yr,
    configuration = config
  )
  
  # annual raw DART observations
  dart_obs = dart_out$dart_obs %>%
    mutate(
      species      = "Chinook",
      spawn_year   = yr,
      trans_status = as.character(trans_status)
    )
  
  saveRDS(dart_obs, dart_file_path)
  
  # annual mark data
  mark_df = dart_obs %>%
    select(
      tag_code,
      file_id,
      contains("mark_"),
      contains("event_"),
      contains("rel_"),
      length,
      flags
    ) %>%
    select(-any_of("event_type_name")) %>%
    distinct(tag_code, .keep_all = TRUE)
  
  # annual compressed observations, trimmed to SFSR tags
  comp_obs = dart_out$compress_obs %>%
    mutate(
      species  = "Chinook",
      spawn_yr = yr
    ) %>%
    left_join(mark_df, by = "tag_code") %>%
    group_by(tag_code) %>%
    filter(any(node %in% nodes)) %>%
    ungroup() %>%
    mutate(
      node = recode(
        node,
        MCCA   = "STR",
        SALSFW = "STR"
      )
    )
  
  saveRDS(comp_obs, comp_file_path)
  
  message("Saved DART observations: ", basename(dart_file_path))
  message("Saved compressed observations: ", basename(comp_file_path))
  
  invisible(comp_obs)
  
} # end retrieve_sfsr_chinook_sy()

#------------------------
# retrieve selected years
pwalk(
  sy_retrieve,
  \(species,
    spawn_yr,
    spc_code,
    dart_file_name,
    comp_file_name,
    dart_file_path,
    comp_file_path) {
    
    retrieve_sfsr_chinook_sy(
      yr             = spawn_yr,
      dart_file_path = dart_file_path,
      comp_file_path = comp_file_path
    )
  }
)

#-------------------------
# compile all annual files
# dart_obs_df = list.files(
#   dart_obs_dir,
#   pattern = "^chnk_sy\\d{4}_dart_obs\\.rds$",
#   full.names = TRUE
# ) %>%
#   map(readRDS) %>%
#   bind_rows()

sfsr_comp_obs_df = list.files(
  comp_obs_dir,
  pattern = "^chnk_sy\\d{4}_comp_obs\\.rds$",
  full.names = TRUE
) %>%
  map(readRDS) %>%
  bind_rows()

#----------------------
# save compiled objects
save(
  update_dttm,
  #dart_obs_df,
  sfsr_comp_obs_df,
  file = file.path(
    out_dir, 
    paste0("obs_archive/sfsr_chnk_compiled_obs_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".rda") # for archiving
  )
)

# stable/latest file
save(
  update_dttm,
  #dart_obs_df,
  sfsr_comp_obs_df,
  file = file.path(out_dir, "sfsr_chnk_compiled_obs_latest.rda")
)

### END SCRIPT

# load configuration file
# load("data/configuration_files/site_config_LGR_20260116.rda")
# rm(crb_sites_sf, flowlines, parent_child, sr_site_pops)
# 
# # which spawn year(s) to query
# yrs = 2010:2026
# 
# # create list of DART observations, by spawn year, compressed
# dart_obs_ls = map(.x = yrs,
#                   .f = function(x){
#                     compressDART(species = "Chinook",
#                                  loc = "GRA",
#                                  spawn_year = x,
#                                  configuration = configuration)
#                     })
# 
# # assign names to dfs
# names(dart_obs_ls) = yrs
# 
# # extract the named data frame from the list of lists
# dart_obs_df = dart_obs_ls %>%
#   map_dfr(. %>%
#             pluck("dart_obs") %>%
#             mutate(trans_status = as.character(trans_status)), .id = "spawn_year")
# 
# # extract mark data
# mark_df = dart_obs_df %>%
#   #filter(event_type_name == 'Mark') %>% wasn't getting every tag for some reason
#   select(tag_code,
#          file_id,
#          contains('mark_'),
#          contains('event_'),
#          contains('rel_'),
#          length,
#          flags) %>%
#   select(-event_type_name) %>%
#   distinct(tag_code,
#            .keep_all = TRUE)
# 
# # compile compressed obs and join the mark data
# compress_df = dart_obs_ls %>%
#   map_dfr(. %>% 
#             pluck("compress_obs"),
#           .id = 'spawn_year') %>%
#   left_join(mark_df)
# 
# # nodes of interest in south fork salmon river
# sfsr_nodes = c("ESS_D", "ESS_U", "JOHNSC",       # East Fork South Fork Salmon
#                "ZEN_D", "ZEN_U", "LAKEC",        # Secesh River
#                "SFG", "KRS", "KRS_D", "KRS_U",   # South Fork Salmon Arrays
#                "KNOXB", "MCCA", "SALSFW", "STR") # South Fork Salmon weir and MRR sites
# 
# # trim down to just tags observed within the sfsr, all years
# sfsr_compress_df = compress_df %>%
#   group_by(tag_code) %>%
#   filter(any(node %in% sfsr_nodes)) %>%
#   ungroup() %>%
#   # recode MCCA and SALSFW to be same as STR
#   mutate(node = recode(node,
#                        MCCA = "STR",
#                        SALSFW = "STR"))
# 
# # write out objects for analysis
# save(dart_obs_ls,
#      sfsr_compress_df,
#      file = paste0("data/observations/sfsr_obs_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".rda"))
# 
# # END SCRIPT