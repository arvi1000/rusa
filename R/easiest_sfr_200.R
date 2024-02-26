library(data.table)
library(tidyverse)
library(lubridate)

events <- fread('data/events.csv')
event_details <- fread('data/event_details.csv')
event_results <- fread('data/event_results.csv')

sfr_200 <- 
  events %>%
  filter(grepl('San Francisco', region) & distance >= 200 & distance < 300)

sfr_200 <- merge(
  sfr_200, 
  event_details %>% select(eid, finishers, dnf),
  by='eid')

summary_dat <-
  merge(sfr_200, event_results, by='eid') %>%
  mutate(total_hours = hours + minutes/60) %>%
  group_by(route) %>%
  summarise(times_held = length(unique(date)),
            finishers = sum(finishers),
            dnf = sum(dnf),
            median_finish_hours = quantile(total_hours, .5)) %>%
  mutate(dnf_pct = dnf / (dnf+finishers)) %>%
  arrange(-times_held)
  
summary_dat %>%
  ggplot(aes(x=median_finish_hours, y=dnf_pct)) +
  geom_point(shape=21, color='red') +
  ggrepel::geom_text_repel(aes(label=gsub('2[0-9]*(k|K|km)?', '', route))) +
  theme_light()

summary_dat %>% 
  arrange(median_finish_hours) %>%
  mutate(dnf_pct = scales::percent(dnf_pct, 0.1)) %>%
  select(route, times_held, dnf_pct, median_finish_hours) %>%
  print(n=Inf)
