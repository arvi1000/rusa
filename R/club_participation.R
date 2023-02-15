library(data.table)
library(tidyverse)
library(lubridate)

events <- fread('data/events.csv')
event_details <- fread('data/event_details.csv')
event_results <- fread('data/event_results.csv')

event_details <- event_details %>% rename(host_club = club)

events <- events %>% mutate(rtid_char = rtid, rtid = as.numeric(rtid))

# UNIQUE FINISHERS ----
club_summary <- event_results %>% 
  merge(event_details, by='eid', all.x=T) %>%
  group_by(club = `club / acp code`, event_year = year(date)) %>%
  summarise(unique_riders = length(unique(rusa)),
            brevet_finishers = n())

top_clubs <- club_summary %>%
  filter(event_year >= '2017') %>%
  group_by(club) %>%
  summarise(unique_riders = sum(unique_riders)) %>%
  arrange(-unique_riders)

plot_data <- club_summary %>% filter(club %in% top_clubs$club[1:10]) 
  
plot_data %>%
  ggplot(aes(x=event_year, y=unique_riders, group=club, color=club)) +
  geom_line() +
  ggrepel::geom_text_repel(
    data = filter(plot_data, event_year == max(plot_data$event_year)),
    aes(label= club), 
    hjust=1, xlim = 2021.2) +
  #scale_x_continuous(breaks = c(2017:2021, rep(NA, 3))) +
  xlim(2017, 2024) +
  theme_light() +
  theme(legend.position = 'none')
                          
# where's davis? ----
event_results %>% 
  merge(event_details, by='eid', all.x=T) %>%
  mutate(own_rider = host_club == `club / acp code`) %>%
  group_by(host_club) %>%
  summarise(finishers = n(), home_finishers = sum(own_rider), pct_home = home_finishers/finishers) %>%
  ggplot(aes(x=finishers, y=pct_home, color=grepl('Davis', host_club))) +
  geom_point(size=4) +
  labs(title='Total Brevet Finishers by Hosting Club, 2009-2021 vs. percent of finishers listing host club as their club')

# uniques vs total ----
club_summary %>%
  filter(event_year == 2021,
         club %in% top_clubs$club[1:20]) %>%
  mutate(short_name = club %>% 
           gsub(' +(Randonneurs|Cycling|Club) +', '', .) %>%
           gsub('/.*', '', .)) %>%
  ggplot(aes(x=unique_riders, y = brevet_finishers/unique_riders)) +
  geom_point(size=4) +
  ggrepel::geom_text_repel(aes(label = short_name)) +
  labs(y='Avg brevets finished per finisher',
       title = 'Top clubs by unique finishers, 2021') +
  theme_light()

# DNF

evnt_dat <- merge(
  select(events, c('eid', 'region', 'date', 'rtid', 'route')),
  select(event_details, c('eid', 'distance', 'finishers', 'dnf')),
  by='eid', all.x=T) %>%
  mutate(starters = finishers + dnf, 
         dnf_rate = dnf/starters,
         date = ymd(date),
         year = year(date)) %>%
  filter(starters > 0)

dnf_plot <- evnt_dat %>%
  mutate(nominal_dist = factor(floor(distance/100)*100)) %>%
  group_by(rtid, route, distance, region, nominal_dist) %>%
  summarise(runnings = n(),
            mean_starters = mean(starters),
            mean_unweighted_dnf_rate = mean(dnf_rate))

dnf_plot$nominal_dist <- 
  dnf_plot$nominal_dist %>% 
  fct_relevel('100') %>%
  fct_lump(5, other_level = '1000+')

dnf_plot %>%
  filter(runnings>=3, mean_starters >=10) %>%
  ggplot(aes(x=mean_starters, y=mean_unweighted_dnf_rate, 
             size=runnings)) +
  geom_point(alpha=.5) +
  facet_wrap(~nominal_dist) +
  theme_light() +
  theme(panel.grid.minor = element_blank()) +
  labs(title='RUSA Events 2009-2021: DNF rate vs event size, by nominal km distance',
       subtitle = 'Only events run 3+ times with 10+ mean starters')

# for jesse
top_regions <- event_details %>%
  group_by(region) %>%
  summarise(sz=sum(finishers)) %>%
  arrange(-sz) %>%
  ungroup

keep_regions <- top_regions$region[c(1,7,9,10)]

dnf_plot %>%
  mutate(region_label = factor(region, top_regions$region),
         region_label = fct_other(region_label, keep = keep_regions)) %>%
  filter(runnings>=3, mean_starters >=10) %>% 
  ggplot(aes(x=mean_starters, y=mean_unweighted_dnf_rate, 
             size=runnings)) +
  geom_point(aes(alpha=region_label, color=region_label)) +
  facet_wrap(~nominal_dist) +
  theme_light() +
  theme(panel.grid.minor = element_blank()) +
  scale_color_manual(values = c("#424395", "#5EC2DA", "#EBC915", "#EB549A",'#000000')) +
  scale_alpha_manual(values = c(.8, .8, .8, .8, .2)) +
  labs(title='RUSA Events 2009-2021: DNF rate vs event size, by nominal km distance',
       subtitle = 'Only events run 3+ times with 10+ mean starters')

# DNFiest routes
dnf_plot %>%
  filter(runnings>=3, mean_starters >=10) %>%
  arrange(-mean_unweighted_dnf_rate) %>%
  ungroup() %>%
  select(-rtid, -distance, -nominal_dist)

# DNFiest major routes  
dnf_plot %>%
  filter(runnings>=5, mean_starters >=25) %>%
  arrange(-mean_unweighted_dnf_rate) %>%
  ungroup() %>%
  select(-rtid, -distance, -nominal_dist)
