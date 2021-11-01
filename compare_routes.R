# compare 300s ----
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

