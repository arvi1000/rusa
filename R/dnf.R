library(tidyverse)
library(lubridate)

event_details <- 
  list.files('data', pattern = 'event_details', full.names = T) %>%
  lapply(function(x) {
    read.csv(x, colClasses=c('character', 'character', 'character',
                             'numeric', 'character',  'numeric', 'numeric',
                             'character'))
    }) %>%
  bind_rows
event_details <- event_details %>% mutate(date = ymd(date))

event_details %>%
  group_by(year=year(date)) %>%
  summarise(finishers=sum(finishers, na.rm=T),
            dnf=sum(dnf, na.rm=T),
            starters = finishers+dnf,
            dnf_rate = dnf/starters)

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
  geom_smooth() +
  scale_y_continuous(labels = scales::percent) +
  ggtitle('Total Event Starters vs DNF rate by RUSA region, 2016-2021') +
  theme_light()


event_details %>%
  group_by(state=gsub('\\:.*', '', region), 
           month = month(date, label=T)) %>%
  summarise(finishers=sum(finishers, na.rm=T),
            dnf=sum(dnf, na.rm=T),
            starters = finishers+dnf,
            dnf_rate = dnf/starters) %>%
  filter(state %in% c('CA', 'TX', 'FL', 'WA', 'NJ')) %>%
  ggplot(aes(x=month, y=dnf_rate, group=state, color=state)) +
  geom_line() +
  theme_light()

event_details %>%
  group_by(month = month(date, label=T)) %>%
  summarise(finishers=sum(finishers, na.rm=T),
            dnf=sum(dnf, na.rm=T),
            starters = finishers+dnf,
            dnf_rate = dnf/starters) %>%
  #filter(state %in% c('CA', 'TX', 'FL', 'WA', 'NJ')) %>%
  ggplot(aes(x=month, y=dnf_rate)) +
  geom_col() +
  theme_light()


event_details %>%
  group_by(distance) %>%
  summarise(finishers=sum(finishers, na.rm=T),
            dnf=sum(dnf, na.rm=T),
            starters = finishers+dnf,
            dnf_rate = dnf/starters) %>%
  #filter(state %in% c('CA', 'TX', 'FL', 'WA', 'NJ')) %>%
  ggplot(aes(x=distance, y=dnf_rate, group=distance)) +
  geom_boxplot() +
  theme_light()

event_details %>%
  mutate(distance=trunc(distance/100)*100,
         starters = dnf+finishers,
         dnf_rate = dnf/starters) %>%
  filter(starters>=10) %>%
  #filter(distance==600) %>%
  #arrange(-dnf_rate)
  ggplot(aes(x=starters, y=dnf_rate)) +
  geom_jitter(alpha=.5, size=3, aes(color=factor(distance))) +
  ggtitle('Size and dnf rate, 2016-2021') +
  #facet_wrap(~starters > 10, scales = 'free_x') +
  # ggplot(aes(x=factor(distance), y=dnf_rate)) +
  # geom_boxplot(outlier.color = NA) +
  theme_light() 

event_details %>%
  group_by(distance=trunc(distance/100)*100) %>%
  summarise(dnf = sum(dnf, na.rm = T),
            finishers = sum(finishers, na.rm=T),
            starters = dnf+finishers,
            dnf_rate = dnf/starters) %>%
  ggplot(aes(x=factor(distance), y=dnf_rate)) +
  geom_col() +
  theme_light() +
  labs(title='Unweighted DNF rate by event distance, 2016-2021',
       subtitle = ' (rounded to 100km)')

  


