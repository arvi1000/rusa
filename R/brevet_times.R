library(tidyverse)
library(lubridate)
library(glue)

results <- read_csv('data/event_results.csv')
events <- read_csv('data/events.csv')
events[c(3506,7136),]

dat <- 
  merge(results, 
        events %>%
          mutate(rtid = as.numeric(rtid)) %>%
          select(eid, date, route, distance, region, rtid),
        by='eid', all.x = T)
dat <- dat %>%
  mutate(mins = hours*60+minutes,
       dist=100*trunc(distance/100), year=year(date),
       hrs=mins/60)
yr_range <- range(dat$year, na.rm=T) %>% paste(collapse = '-')

dat %>%
  filter(year(date) == 2018) %>%
  group_by(rtid, date, region, distance, route) %>%
  tally() %>%
  arrange(-n) %>%
  print(n=50)

# over time
dat %>%
  filter(dist<=600) %>%
  group_by(dist, year) %>%
  summarise(q25 = quantile(mins, .25),
            q50 = quantile(mins, .5),
            q75 = quantile(mins, .75))  %>%
  pivot_longer(cols=starts_with('q')) %>%
  ggplot(aes(x=year, y=value, group=name, color=name)) +
  geom_line() +
  facet_wrap(~dist, ncol=1, scales='free_y')


dat %>%
  filter(dist<=1200) %>%
  group_by(dist) %>%
  summarise(q10 = quantile(hrs, .1),
            q50 = quantile(hrs, .5),
            q90 = quantile(hrs, .9)) %>%
  pivot_longer(cols=starts_with('q')) %>%
  ggplot(aes(x=factor(dist), y=value, group=name, color=name)) +
  geom_point() + geom_line() +
  #geom_hline(yintercept = c(13.5, 27, 40), color='grey') +
  scale_y_continuous(breaks = seq(0, 90, 12)) +
  theme_light() +
  labs(title='Finish Times by Event Distance: Median, 10th ptile (fast), 90th ptile (slow)',
       subtitle = glue('RUSA events {yr_range}'),
       color = 'Percentile Finish',
       y='hours', x='event distance')