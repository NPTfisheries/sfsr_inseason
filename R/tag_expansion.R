# Purpose: Summarize tags by location, release group and year, and expand based on
# tagging rates
#
# Authors: Mike Ackerman and Ryan N. Kinzer 
# 
# Created: July 17, 2023
# Modified:

# load necessary libraries
library(tidyverse)

# load data
load("data/sfsr_sy23_obs.rda")

tag_summ = sfsr_sy23_obs %>%
  select(tag_code,
         site_code,
         node,
         event_date_time_value,
         mark_rear_type_name,
         mark_site,
         mark_date,
         rel_site,
         rel_date,
         flags) %>%
  mutate(rel_year = year(rel_date)) %>%
  filter(site_code %in% c("SFG", "KRS", "SALSFW", "STR")) %>%
  group_by(mark_site,
           rel_site,
           mark_rear_type_name,
           rel_year,
           flags,
           site_code) %>%
  count() %>%
  filter(rel_site %in% c("LGRLDR", "KNOXB")) %>%
  mutate(rel_group = case_when(
    rel_site == "KNOXB" & str_detect(flags, "AI") ~ "McCall - Integrated",
    rel_site == "KNOXB" & str_detect(flags, "AD") ~ "McCall - Segregated",
    rel_site == "LGRLDR" & mark_rear_type_name == "W" ~ "LGR - NOR",
    rel_site == "LGRLDR" & mark_rear_type_name == "W" ~ "Unknown",
    TRUE ~ NA
  ))

exp_tbl = tibble(
  rel_group = c("McCall - Integrated", 
                "McCall - Integrated", 
                "McCall - Segregated",
                "McCall - Segregated",
                "LGR - NOR"),
  rel_year = c(2021,
               2022,
               2021,
               2022,
               2023),
  tag_expansion = c(7,
                    8,
                    67,
                    13,
                    1/0.18))  

tag_exp <- left_join(tag_summ,
                      exp_tbl) %>%
  mutate(est = n*tag_expansion)

exp_summ <- tag_exp %>%
  group_by(site_code, rel_group) %>%
  summarise(tot_est = sum(est, na.rm=TRUE))

# END SCRIPT