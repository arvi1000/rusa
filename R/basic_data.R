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
