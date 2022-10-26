library(tidyverse)
library(lubridate)
library(glue)

event_details <- read.csv(
  'data/event_details.csv', 
  colClasses=c('character', 'character', 'character', 'numeric',
               'character',  'numeric', 'numeric', 'character'))
event_details <- event_details %>% mutate(date = ymd(date))
event_details <- event_details %>%
  mutate(distance=trunc(distance/100)*100,
       starters = dnf+finishers,
       dnf_rate = dnf/starters)

yr_range = event_details$date %>% year %>% range %>% paste(collapse='-')

# by region ----
dnf_by_region <-
  event_details %>%
  group_by(region) %>%
  summarise(finishers=sum(finishers, na.rm=T),
            dnf=sum(dnf, na.rm=T),
            starters = finishers+dnf,
            dnf_rate = dnf/starters)
dnf_by_region %>%
  filter(starters>10) %>%
  ggplot(aes(x=starters, y=dnf_rate)) +
  #geom_point(alpha=0.5, size=4) +
  geom_text(aes(label = gsub('\\:.*', '', region))) +
  #geom_smooth() +
  scale_y_continuous(labels = scales::percent) +
  ggtitle('Total Event Starters vs DNF rate by RUSA region',
          subtitle = yr_range) +
  theme_light()

# by month ----
event_details %>%
  group_by(month = month(date, label=T)) %>%
  summarise(finishers=sum(finishers, na.rm=T),
            dnf=sum(dnf, na.rm=T),
            starters = finishers+dnf,
            dnf_rate = dnf/starters) %>%
  #filter(state %in% c('CA', 'TX', 'FL', 'WA', 'NJ')) %>%
  ggplot(aes(x=month, y=dnf_rate)) +
  geom_col() +
  ggtitle('DNF rate by month, all regions',
          subtitle = yr_range) +
  theme_light()

# by distance ----
event_details %>%
  group_by(distance=trunc(distance/100)*100) %>%
  summarise(dnf = sum(dnf, na.rm = T),
            finishers = sum(finishers, na.rm=T),
            starters = dnf+finishers,
            dnf_rate = dnf/starters) %>%
  ggplot(aes(x=factor(distance), y=dnf_rate)) +
  geom_col() +
  theme_light() +
  labs(title=glue('DNF rate by event distance, {yr_range}'),
       subtitle = '(rounded to 100km)')

event_details %>%
  filter(starters>=10) %>%
  ggplot(aes(x=factor(distance), y=dnf_rate)) +
  geom_boxplot(outlier.shape = NA) +
  theme_light()   

event_details %>%
  filter(starters>=10) %>%
  ggplot(aes(x=starters, y=dnf_rate)) +
  geom_jitter(shape=21, alpha=0.5, aes(size=starters)) +
  labs(title=glue('DNF rate by event distance, {yr_range}'))+
  #facet_wrap(~distance) +
  theme_light() +
  theme(panel.grid.minor = element_blank())


# the worst events. what happened on that Alaska 100k pop in Feb 2013?
event_details %>%
  arrange(-dnf_rate, -starters) %>%
  head

event_details %>%
  filter(starters>0 & dnf !=-1) %>%
  filter(distance <= 600) %>%
  ggplot(aes(distance * starters, dnf_rate)) +
  geom_point()

event_details %>%
  filter(starters>0 & dnf !=-1 & 
           distance * starters > 50000)
  