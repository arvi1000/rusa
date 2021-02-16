library(httr)
library(rvest)
library(glue)
library(lubridate)
library(ggplot2)
library(dplyr)

# func def -----
get_results <- function(rtid) {
  
  # SCRAPE ----
  # get events for a given route ('rtid')
  route_url <- function(rtid) {
    glue("https://rusa.org/cgi-bin/resultsearch_PF.pl?",
         "regid=&date=&type=&dist=&",
         "rtid={rtid}",
         "&esortby=cert&",
         "collapse=1", rtid=rtid)
  }
  route_results <- 
    route_url(rtid) %>%
    read_html
  dat <- route_results %>%
    html_nodes('table') %>%
    html_table %>%
    .[[1]]
  
  # CLEAN -----
  # remove the 'x nonmember(s) also finished this event' rows
  bad_rows <- grepl('also finished this event$', dat[[1]])
  dat <- dat[!bad_rows,]
  
  # find 'event divider rows' -- the remaining ones where every column is the same
  all_same <- function(x) all(x==x[1])
  event_rows <- apply(dat, 1, all_same)
  
  # add event date as own field:
  # 1) pull the dates
  extracted_dates <- 
    dat[event_rows, 1] %>% 
    stringr::str_extract('[0-9]{4}/[0-9]{2}/[0-9]{2}') %>%
    ymd
  class(extracted_dates)
  # 2) add empty date column
  dat[['event_date']] <- as.Date(NA)
  
  # 3) fill in values at each position where a new event starts
  dat[event_rows, 'event_date'] <- extracted_dates
  
  # 4) lastly, carry those dates foreward over missing values, filling every row
  dat[['event_date']] <- zoo::na.locf(dat[['event_date']])
  
  # now we can drop these 'event divider' rows
  dat <- dat[!event_rows,]
  
  # type conversion
  dat$Time <- hm(dat$Time)
  dat$time_hours <- as.numeric(dat$Time)/60/60
  
  # append rtid as attribute
  attr(dat, 'rtid') <- rtid
  
  return(dat)
}
summary_plot <- function(dat, route_name=NULL) {
  
  route_choice <- list(rtid = attributes(dat)[['rtid']],
                       name = route_name)

  # summary data
  summ_dat <- dat %>%
    group_by(event_date) %>%
    summarise(med_time = median(time_hours),
              finishers = n())
  
  # quantiles
  q_tiles <- quantile(dat$time_hours, c(.05, .25, .5, .75, .95)) %>%
    (function(x) {
      times <- paste0(floor(x), 'h', 
                      round((x %% 1)*60), 'm')
      labels <- paste0(names(x), ':')
      paste(labels, times)
    }) %>%
    paste(collapse=', ')
  
  quantile(dat$time_hours, c(.05, .25, .5, .75, .95)) %>%
    (function(x) {
      paste0(floor(x), 'h', 
             round((x %% 1)*60), 'm') %>%
        setNames(names(x))
    })
  
  # facet plot
  ggplot(dat, aes(time_hours)) +
    geom_histogram(binwidth = 0.25) +
    geom_vline(data = summ_dat,
               aes(xintercept = med_time, color='Median Time'),
               linetype='solid') +
    facet_wrap(~event_date) +
    theme_light() +
    theme(panel.grid.minor = element_blank(),
          legend.position = 'bottom') + #c(.88,.12)) +
    labs(title='Distribution of Finish Times',
         subtitle = paste('RUSA brevet route', 
                          route_choice$rtid, route_choice$name),
           y='Finisher count', color=NULL)
         #,       caption=q_tiles)
}

# pull data, plot ----
# list(
#   rtid=809, name="SFR Two Rock Valley Ford"
#   #rtid=1158, name='SFR Del Puerto Canyon
#   ) %>% 
#   (function(x) x$rtid %>% get_results %>% summary_plot(x$name))


# compare 300s
h300 <- get_results(214)
old_caz <- get_results(958)
avg_times <- function(dat) {
  dat %>%
  group_by(`RUSA#`) %>%
  mutate(mins = period_to_seconds(Time)/60) %>%
  summarise(finish = mean(mins))
}

comparison <- merge(avg_times(old_caz), avg_times(h300), by = 'RUSA#') %>%
  mutate(old_caz = finish.x/60, h300=finish.y/60)
comparison %>%
  ggplot(aes(x=h300, y=old_caz)) +
  geom_point() +
  coord_equal() +
  scale_x_continuous(breaks = 10:21) +
  scale_y_continuous(breaks = 10:21) +
  theme_light() +
  labs(title = glue("Finish in time in hours for riders who've done",
                    "\nboth old caz and healdsburg 300"),
       subtitle = 'Multiple finish times averaged')
  
