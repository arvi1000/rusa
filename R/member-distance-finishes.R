library(data.table)
library(tidyverse)
library(lubridate)

events <- fread('data/events.csv')
event_details <- fread('data/event_details.csv')
event_results <- fread('data/event_results.csv')

summary_dat <- events %>%
  merge(event_details %>% select(eid, finishers, dnf), by='eid') %>%
  merge(event_results, by='eid') %>%
  mutate(total_hours = hours + minutes/60)

table(summary_dat$type)

summary_dat %>%
  filter(type == 'ACPB') %>%
  group_by(event_year = year(date), norm_distance = floor(distance/100)*100) %>%
  summarise(unique_rusa_finishers = length(unique(rusa)))  %>%
  ggplot(aes(x=factor(norm_distance), y=unique_rusa_finishers, fill=factor(event_year))) +
  geom_col(position = 'dodge') +
  labs(fill='Year', x='Distance', y='RUSA Members',
       title ='Brevet Completion: Unique Finishers by Distance by Year') +
  theme_light()
