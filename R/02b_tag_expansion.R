# Purpose: Summarize tags by location, release group and year, and expand based on
# tagging rates
#
# Authors: Mike Ackerman and Ryan N. Kinzer 
# 
# Created: July 17, 2023
#   Modified: June 27, 2024

# clear environment
rm(list = ls())

# load necessary libraries
library(tidyverse)
library(here)

# load data
load(here("data/sfsr_obs_20240627.rda"))
idfg_tag_exp = read_csv(file = here("data/idfg_tag_expansions.csv"))

# recode some nodes in sfsr_obs 
sfsr_obs = sfsr_obs %>%
  mutate(node = case_when(
    node %in% c("ESSA0", "ESSB0") ~ "ESS",
    node %in% c("ZENA0", "ZENB0") ~ "ZEN",
    node %in% c("KRSA0", "KRSB0") ~ "KRS",
    TRUE ~ node
  ))

# set some parameters
yr = 2024

# filter to year of interest
sfsr_obs_yr = sfsr_obs %>%
  filter(spawn_year == yr)

# newly tagged fish observed at KRS
lgrldr = sfsr_obs_yr %>%
  filter(mark_site %in% c('LGRLDR', 'MCCA'),
         rel_site %in% c('LGRLDR', 'KNOXB'),
         node == "KRS",
         mark_rear_type_name == "W") %>%
  distinct(tag_code, 
           .keep_all = TRUE)

# histogram of mark dates for fish observed at KRS
lgrldr %>%
  ggplot(aes(x=mark_date)) +
  geom_histogram()

# summarize tags by release group, release year, and site code (node)
tag_df = sfsr_obs_yr %>%
  select(tag_code,
         node,
         event_date_time_value,
         mark_rear_type_name,
         mark_site,
         mark_date,
         rel_site,
         rel_date,
         flags) %>%
  mutate(rel_year = year(rel_date)) %>%
  filter(node %in% c("SFG", "KRS", "SALSFW", "STR")) %>%
  left_join(idfg_tag_exp %>%
              select(Tag, SbyC, Expansion), 
            by = c("tag_code" = "Tag")) %>%
  group_by(mark_site,
           rel_site,
           mark_rear_type_name,
           rel_year,
           SbyC,
           Expansion,
           flags,
           node) %>%
  summarise(n = n_distinct(tag_code),
            .groups = "drop") %>%
  filter(rel_site %in% c("LGRLDR", "KNOXB")) %>%
  filter(!(rel_site == "LGRLDR" & mark_rear_type_name == "H")) %>%
  mutate(rel_group = case_when(
    rel_site == "KNOXB" & str_detect(flags, "AI") ~ "McCall - Integrated",
    rel_site == "KNOXB" & str_detect(flags, "AD") ~ "McCall - Segregated",
    rel_site == "LGRLDR" & mark_rear_type_name == "W" ~ "LGR - NOR",
    rel_site == "KNOXB" & mark_rear_type_name == "W"  ~ "KNOXB - NOR",
    TRUE ~ NA
  )) %>%
  #ungroup() %>%
  group_by(rel_group, 
           rel_year,
           SbyC,
           Expansion,
           node) %>%
  summarise(n_tags = sum(n),
            .groups = "drop") %>%
  arrange(rel_group, 
          rel_year, 
          node)

# # create expansion table
# exp_tbl = tibble(
#   rel_group = c("McCall - Integrated", "McCall - Integrated", 
#                 "McCall - Segregated", "McCall - Segregated",
#                 "LGR - NOR"),
#   rel_year = c(2021, 2022,
#                2021, 2022,
#                2023),
#   tag_expansion = c(7, 8,
#                     67, 13,
#                     1/0.18))  

tag_exp = tag_df %>%
  rename(tag_expansion = Expansion) %>%
  mutate(tag_expansion = case_when(
    rel_group == "LGR - NOR" ~ 1 / 0.20,
    TRUE ~ tag_expansion
  )) %>%
  mutate(est = round(n_tags * tag_expansion))
  
# expand n tags by expansion rate
# tag_exp = left_join(tag_df,
#                     exp_tbl) %>%
#   mutate(est = round(n_tags * tag_expansion))

# expansion summary
exp_df = tag_exp %>%
  group_by(rel_group, node) %>%
  summarise(n_tags = sum(n_tags, na.rm = T),
            n_tags_exp = sum(est, na.rm = T),
            .groups = "drop")

#---------------------------------
# calculate detection probabilities

# load data
#load("data/sfsr_obs.rda")

# nodes of interest for det probs
sf_nodes = c("SFG", "KRS", "SALSFW", "STR")

# convert observations into capture histories
sf_ch = sfsr_obs %>%
  filter(node %in% sf_nodes) %>%
  mutate(node = factor(node,
                       levels = sf_nodes)) %>%
  select(tag_code, spawn_year, node) %>%
  distinct() %>%
  mutate(seen = 1) %>%
  pivot_wider(names_from = node,
              values_from = seen,
              values_fill = 0,
              names_sort = T,
              names_expand = T) %>%
  mutate(SF_weir = if_else(SALSFW == 1 | STR == 1, 1, 0)) %>%
  select(-SALSFW, -STR)

# SFG & KRS detection probs by spawn year
sf_p_x_sy = sf_ch %>%
  mutate(pass_SFG = if_else(SF_weir == 1 | KRS == 1, T, F)) %>%
  select(spawn_year, pass_SFG, SFG) %>%
  filter(pass_SFG == T) %>%
  group_by(spawn_year) %>%
  summarise(n_tags = n(),
            p = sum(SFG) / n(),
            .groups = "drop") %>%
  mutate(site = "SFG") %>%
  bind_rows(sf_ch %>%
              mutate(pass_KRS = if_else(SF_weir == 1, T, F)) %>%
              select(spawn_year, pass_KRS, KRS) %>%
              filter(pass_KRS == T) %>%
              group_by(spawn_year)%>%
              summarise(n_tags = n(),
                        p = sum(KRS) / n()) %>%
              mutate(site = "KRS")) %>%
  arrange(spawn_year, site)

# SFG & KRS detection probs
sf_p = sf_p_x_sy %>%
  group_by(site) %>%
  summarise(n_tags = sum(n_tags),
            p = mean(p))

# expand estimates by site detection probabilities
exp_df_2 = exp_df %>%
  left_join(sf_p %>%
              select(site, p),
            by = c("node" = "site")) %>%
  mutate(tot_est = round(n_tags_exp / p, 0))

# write to .rda
write_csv(exp_df_2,
          file = here("data/sfsr_expansion_sy2024.csv"))
#save(tag_exp, exp_df, exp_df_2, file = './data/expansion.rda')

# END SCRIPT