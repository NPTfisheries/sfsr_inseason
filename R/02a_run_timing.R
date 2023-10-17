# Purpose: Calculate run-timing curves for SFSR fish to arrays.

# Authors: Mike Ackerman and Ryan N. Kinzer 
# 
# Created: July 17, 2023
#   Modified: October 17, 2023

# load necessary libaries
library(tidyverse)

# load tags observed in SFSR, all years
load("data/sfsr_obs.rda")

sfsr_fish <- sfsr_obs %>%
  filter(mark_site %in% c('LGRLDR', 'MCCA')) %>%
  filter(rel_site %in% c('LGRLDR', 'KNOXB')) %>%
  mutate(julian = lubridate::mdy(paste0(format(min_det, format = '%m-%d'), '-2020'))) %>%
  mutate(group = case_when(
    mark_site == 'MCCA' & grepl('AD', flags) ~ 'Segregated',
    mark_site == 'MCCA' & grepl('CW||AI', flags) ~ 'Integregated',
    mark_site == 'LGRLDR' & mark_rear_type_name != 'H' ~ 'Natural',
    TRUE ~ NA
  ))
  
obs <- sfsr_fish %>%
  filter(group != 'other') %>%
  filter(node %in% c('SFG', 'KRS')) %>%
  group_by(node, tag_code) %>%
  slice_min(min_det)

# check run-timing by group - not a big difference
obs_sum <- obs %>%
  group_by(node, spawn_year, julian) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  complete(node, spawn_year, julian, fill = list(n=0)) %>%
  group_by(node, spawn_year) %>%
  mutate(csum = cumsum(n),
         cdf = csum/sum(n)) %>%
  ungroup()

avg_run <- obs_sum %>%
  filter(spawn_year != 2023) %>%
  group_by(node, julian) %>%
  summarise(cdf = mean(cdf))
  
fig_run <- ggplot(data = NULL, aes(x = julian, y = cdf)) +
    geom_line(data = obs_sum %>% filter(spawn_year != 2023), aes(group = spawn_year), colour = 'grey') +
    geom_line(data = avg_run, colour = 'navy') +
    geom_line(data = obs_sum %>% filter(spawn_year == 2023), colour = 'limegreen') +
    scale_x_date(date_breaks = '2 weeks', date_labels = "%b-%d") +
    facet_wrap(~fct_rev(node), ncol = 1) +
  theme_bw() +
  labs(x = 'Date',
       y = 'CDF')
       # subtitle = 'Run-timing of SFSR hatchery (segregated and integrated) and natural origin returns across PIT-arrays. The grey lines indicate individual spawn year returns for \n 2010-2022, and the dark blue line shows the average cumulative proportion of returns for each day. ')

fig_run
ggsave(here("/figures/run_timing.png"), fig_run, width = 5, height = 5)

obs_run <- obs %>%
  #filter(spawn_year != 2023) %>%
  group_by(node, spawn_year) %>%
  summarise(n_tags = n_distinct(tag_code),
            p01 = quantile(julian, probs = .01, type = 1),
            p10 = quantile(julian, probs = .1, type = 1),
            p25 = quantile(julian, probs = .25, type = 1),            
            p50 = quantile(julian, probs = .5, type = 1),
            p75 = quantile(julian, probs = .75, type = 1),          
            p90 = quantile(julian, probs = .9, type = 1),
            p99 = quantile(julian, probs = .99, type = 1)) %>%
  pivot_longer(p01:p99, names_to = 'percentile', values_to = 'date') %>%
  mutate(julian = yday(date)) %>%
  ungroup()


# Figure out the missed period with AR model
obs_23 <- obs %>%
  filter(spawn_year == 2023) %>%
  group_by(node, group, julian) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  complete(node, group, julian) %>%
  group_by(node, group) %>%
  mutate(tmp = zoo::na.approx(n))

obs_23 %>%
  ggplot(aes(x = julian, y = n)) +
  geom_point() +
  geom_smooth() +
  facet_grid(group ~ fct_rev(node)) +
  theme_bw() +
  labs(x = 'Date',
       y = 'Daily Count')
  
obs_23

obs_ts <- ts(obs_23$n[obs_23$node == 'KRS'])
int_ts <- zoo::na.approx(obs_ts)
plot(int_ts)
plot(diff(int_ts))
acf(diff(int_ts)) #no autocorrelation



