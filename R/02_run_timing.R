# Purpose: Calculate run-timing curves for SFSR fish to arrays.

# Authors: Mike Ackerman and Ryan N. Kinzer 
# 
# Created: July 17, 2023
# Modified:


library(tidyverse)

load("data/sfsr_obs.rda")

sfsr_fish <- sfsr_obs %>%
  filter(mark_site %in% c('LGRLDR', 'MCCA')) %>%
  filter(rel_site %in% c('LGRLDR', 'KNOXB')) %>%
  mutate(julian = lubridate::mdy(paste0(format(min_det, format = '%m-%d'), '-2020'))) %>%
  mutate(group = case_when(
    mark_site == 'MCCA' & grepl('AD', flags) ~ 'Segregated',
    mark_site == 'MCCA' & grepl('CW', flags) ~ 'Integregated',
    mark_site == 'MCCA' & grepl('AI', flags) ~ 'Integregated',
    mark_site == 'LGRLDR' & mark_rear_type_name != 'H' ~ 'Natural',
    TRUE ~ 'other'
  ))
  
obs <- sfsr_fish %>%
  filter(group != 'other') %>%
  filter(site_code %in% c('SFG', 'KRS')) %>%
  group_by(site_code, tag_code) %>%
  slice_min(min_det)

obs %>%
  filter(site_code == 'KRS') %>%
  group_by(as.character(ju))
  ggplot(aes(x = ))


# check run-timing by group - not a big difference
obs %>%
  filter(site_code == "KRS") %>%
  ggplot(aes(x = julian, color = spawn_year)) +
  stat_ecdf() #+
  #facet_grid(spawn_year~site_code)


obs_sum <- obs %>%
  #filter(spawn_year != 2023) %>%
  group_by(site_code, spawn_year) %>%
  summarise(n_tags = n_distinct(tag_code),
            q01 = quantile(julian, probs = .01, type = 1),
            q10 = quantile(julian, probs = .1, type = 1),
            q25 = quantile(julian, probs = .25, type = 1),            
            q50 = quantile(julian, probs = .5, type = 1),
            q75 = quantile(julian, probs = .75, type = 1),          
            q90 = quantile(julian, probs = .9, type = 1),
            q99 = quantile(julian, probs = .99, type = 1)) %>%
  pivot_longer(contains('q'), names_to = 'percentile', values_to = 'date') %>%
  mutate(julian = yday(date)) %>%
  ungroup()


obs_sum %>%
  ggplot(aes(x = spawn_year, y = date)) +
  geom_point() +
  coord_flip() +
  facet_grid(percentile~site_code)

tmp <- obs_sum %>%
  group_by(site_code, percentile) %>%
  summarise(mu = julian,
            stdev = sd(julian))





