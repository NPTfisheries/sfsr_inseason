# Purpose: Summarize tags by location, release group and year, and expand based on
# tagging rates
#
# Authors: Mike Ackerman and Ryan N. Kinzer 
# 
# Created: July 17, 2023
#   Modified: June 13, 2025

# clear environment
rm(list = ls())

# load necessary libraries
library(tidyverse)
library(here)
library(readxl)
library(janitor)

# get the date-time from the most recent dart observations
dt_tm = list.files(path = here("data/observations/"),
                   recursive = F,
                   full.names = F) %>%
  sort() %>%
  .[length(.)] %>%
  sub("^[^_]*_[^_]*_([^\\.]+)\\.rda$", "\\1", .)

# load observation data
load(paste0(here("data/observations/sfsr_obs_"), dt_tm, ".rda"))

# load lgr tag expansion data, the preferred option
lgr_tag_exp = read_excel(path = here("data/mark_rates/2025 PIT_Tag_Analysis_LowerGranite_20250611.xlsx"),
                         sheet = "PIT Data") %>%
  clean_names() %>%
  select(tag,
         release_site_code,
         brood_year,
         ocean_age,
         sby_c,
         exp_rate = expansion)

# load generic mark rates from bon, the backup option
bon_exp_rates = read_excel(path = here("data/mark_rates/2024 PIT_Tag_Analysis_Bonneville.xlsx"),
                        sheet = "Historic Juv Rel Numbers") %>%
  clean_names() %>%
  filter(str_detect(hatchery, "McCall")) %>%
  mutate(hatchery = case_when(
    hatchery == "McCall (Int)" ~ "McCall - Integrated",
    hatchery == "McCall (Seg)"  ~ "McCall - Segregated",
    TRUE ~ hatchery
  )) %>%
  mutate(ral_mark_rate = pit_release_ral / hatch_release,
         ral_exp_rate = 1 / ral_mark_rate) %>%
  rename(rel_group = hatchery,
         rel_year = migr_year)

# set some parameters
yr = 2025

# filter to year of interest
sfsr_obs_yr = sfsr_obs %>%
  filter(spawn_year == yr)

# newly tagged fish observed at KRS
# lgrldr = sfsr_obs_yr %>%
#   filter(mark_site %in% c('LGRLDR', 'MCCA'),
#          rel_site %in% c('LGRLDR', 'KNOXB'),
#          node == "KRS",
#          mark_rear_type_name == "W") %>%
#   distinct(tag_code, 
#            .keep_all = TRUE)
# 
# # histogram of mark dates for fish observed at KRS
# lgrldr %>%
#   ggplot(aes(x=mark_date)) +
#   geom_histogram()

# summarize tags by release group, release year, and site code (node)
tag_df = sfsr_obs_yr %>%
  select(spawn_year,
         tag_code,
         node,
         event_type_name,
         min_det,
         mark_site,
         mark_date,
         mark_rear_type_name,
         rel_site,
         rel_date,
         flags) %>%
  mutate(rel_year = year(rel_date)) %>%
  # for any tag that was observed at a node multiple times, just keep the first detection
  group_by(tag_code, node) %>%
  filter(min_det == min(min_det)) %>%
  ungroup() %>%
  # keep just detections within mainsten SF Salmon River
  filter(node %in% c("SFG", "KRS_D", "KRS_U", "STR")) %>%
  # join tag-specific lgr tag detection rates
  left_join(lgr_tag_exp, by = c("tag_code" = "tag")) %>%
  group_by(mark_site,
           mark_rear_type_name,
           rel_site,
           rel_year,
           ocean_age,
           sby_c,
           exp_rate,
           flags,
           node) %>%
  summarise(n = n_distinct(tag_code),
            .groups = "drop") %>%
  # only interested in tagged fish at LGR or hatchery release sites in SF Salmon
  filter(rel_site %in% c("LGRLDR", "KNOXB", "SALTRPT", "SFSRKT")) %>%
  mutate(rel_group = case_when(
    rel_site == "KNOXB" & str_detect(flags, "AI")     ~ "McCall - Integrated",
    rel_site == "KNOXB" & str_detect(flags, "AD")     ~ "McCall - Segregated",
    rel_site == "LGRLDR" & mark_rear_type_name == "W" ~ "LGR - NOR",
    rel_site == "LGRLDR" & str_detect(flags, "CW")    ~ "LGR - HOR",
    rel_site == "KNOXB"  & mark_rear_type_name == "W" ~ "KNOXB - NOR",
    rel_site == "SFSRKT" & mark_rear_type_name == "W" ~ "SFSRKT - NOR",
    TRUE ~ NA
  )) %>%
  group_by(rel_group,
           rel_year,
           ocean_age,
           sby_c,
           exp_rate,
           node) %>%
  summarize(n_tags = sum(n),
            .groups = "drop") %>%
  arrange(rel_group,
          rel_year,
          node) %>%
  # join bon group mark rates (back-up)
  left_join(bon_exp_rates %>%
              select(rel_group,
                     rel_year,
                     ral_exp_rate),
            by = c("rel_group", "rel_year"))

# perform tag expansions
lgr_trap_rate = 0.20
tag_exp = tag_df %>%
  mutate(exp_rate = case_when(
    str_detect(rel_group, "LGR - NOR") ~ 1 / lgr_trap_rate, # the LGR sample rate
    is.na(exp_rate) ~ ral_exp_rate,                         # if not lgr expansion rate, use bon rate
    TRUE ~ exp_rate
  )) %>%
  mutate(n_tags_exp = round(n_tags * exp_rate)) %>%
  group_by(rel_group,
           rel_year,
           ocean_age,
           node) %>%
  summarise(n_tags = sum(n_tags, na.rm = T),
            n_tags_exp = sum(n_tags_exp, na.rm = T),
            .groups = "drop")

#---------------------------------
# calculate detection probabilities
library(PITcleanr)
load(here("data/configuration_files/site_config_LGR_20240304.rda"))

# calculate node detection efficiencies for all spawn years
node_eff = sfsr_obs %>%
  group_by(spawn_year) %>%
  do(estNodeEff(capHist_proc = ., 
                node_order = node_paths)) %>%
  ungroup() %>%
  filter(node %in% c("SFG", "KRS_D", "KRS_U", "STR"))

# expand estimates by site detection probabilities
exp_summ = tag_exp %>%
  filter(str_detect(rel_group, "McCall") | str_detect(rel_group, "LGR")) %>%
  filter(rel_group != "LGR - HOR") %>%
  left_join(node_eff %>%
              filter(spawn_year == yr) %>%
              select(node, 
                     eff_est, 
                     eff_se)) %>%
  mutate(tot_est = round(n_tags_exp / eff_est)) %>%
  filter(node != "KRS_U") %>%
  # parse by adults vs. jacks
  mutate(age = case_when(
    str_detect(rel_group, "LGR")        ~ "All",
    ocean_age == "2" | ocean_age == "3" ~ "Adults",
    ocean_age == "1"                    ~ "Jacks",
    TRUE ~ NA
  )) %>%
  group_by(rel_group,
           age,
           node,
           eff_est,
           eff_se) %>%
  summarize(n_tags = sum(n_tags),
            n_tags_exp = sum(n_tags_exp),
            tot_est = sum(tot_est),
            .groups = "drop") %>%
  select(rel_group,
         age,
         node,
         n_tags,
         n_tags_exp,
         eff_est,
         eff_se,
         tot_est) %>%
  arrange(rel_group,
          desc(node),
          age)

# write to file
library(writexl)
write_xlsx(list(tag_exp_rel_group = tag_exp,
                exp_summary = exp_summ),
           path = paste0(here("output"), "/sfsr_inseason_ests_sy24_", dt_tm, ".xlsx"))

# END SCRIPT

# OLD CODE FOR CALCULATING DETECTION PROBS
# convert observations into capture histories
# sf_ch = sfsr_obs %>%
#   filter(node %in% sf_nodes) %>%
#   mutate(node = factor(node,
#                        levels = sf_nodes)) %>%
#   select(tag_code, spawn_year, node) %>%
#   distinct() %>%
#   mutate(seen = 1) %>%
#   pivot_wider(names_from = node,
#               values_from = seen,
#               values_fill = 0,
#               names_sort = T,
#               names_expand = T) %>%
#   mutate(SF_weir = if_else(SALSFW == 1 | STR == 1, 1, 0)) %>%
#   select(-SALSFW, -STR)
# 
# # SFG & KRS detection probs by spawn year
# sf_p_x_sy = sf_ch %>%
#   mutate(pass_SFG = if_else(SF_weir == 1 | KRS == 1, T, F)) %>%
#   select(spawn_year, pass_SFG, SFG) %>%
#   filter(pass_SFG == T) %>%
#   group_by(spawn_year) %>%
#   summarise(n_tags = n(),
#             p = sum(SFG) / n(),
#             .groups = "drop") %>%
#   mutate(site = "SFG") %>%
#   bind_rows(sf_ch %>%
#               mutate(pass_KRS = if_else(SF_weir == 1, T, F)) %>%
#               select(spawn_year, pass_KRS, KRS) %>%
#               filter(pass_KRS == T) %>%
#               group_by(spawn_year)%>%
#               summarise(n_tags = n(),
#                         p = sum(KRS) / n()) %>%
#               mutate(site = "KRS")) %>%
#   arrange(spawn_year, site)
# 
# # SFG & KRS detection probs
# sf_p = sf_p_x_sy %>%
#   group_by(site) %>%
#   summarise(n_tags = sum(n_tags),
#             p = mean(p))
