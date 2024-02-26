# prepare data ----
source('R/basic_data.R')

summary_dat <- summary_dat %>% mutate(event_year = year(date))
kr <- summary_dat %>% filter(grepl('King Ridge', route))
hop <- summary_dat %>% filter(grepl('Hopland', route))

col_subset <- function(x) x %>% select(event_year, date, route, rusa, total_hours)
both_routes <- merge(
  col_subset(hop),  col_subset(kr), 
  by=c('event_year', 'rusa'), all = T, 
  suffixes = c('_hop', '_kr'))


# MEGA PLOT ----
ggplot(both_routes,
       aes(x=total_hours_hop, y=total_hours_kr)) +
  geom_jitter(alpha=.8, width = .2, height=.2) +
  geom_smooth(method='lm', color='black') +
  geom_jitter(data=filter(both_routes, is.na(total_hours_hop)),
              aes(x=31, color='no hopland'), alpha=.5)+
  geom_jitter(data=filter(both_routes, is.na(total_hours_kr)),
              aes(y=31, color='no king ridge'), alpha=.5)+
  xlim(15,32) +ylim(15,32) +
  theme_light() +
  labs(title = 'Two 400ks King Ridge vs Hopland',
       subtitle = 'Black points show times for riders doing both in same year')

# lm ----
fit <- 
  both_routes %>%
  filter(!is.na(total_hours_hop) & !is.na(total_hours_kr)) %>%
  lm(data=.,
     total_hours_kr ~ total_hours_hop)
summary(fit)

predict.lm(fit, 
           data.frame(total_hours_hop = c(20,20.5,21,21.5)), 
           interval = 'confidence',
           level = .95)

# better plots ----
library(patchwork)
library(glue)

scatter_points_n <- both_routes %>%
  filter(!is.na(total_hours_hop) & !is.na(total_hours_kr)) %>%
  nrow
hop_points_n <- sum(!is.na(both_routes$total_hours_hop))
kr_points_n <- sum(!is.na(both_routes$total_hours_kr))

p1 <- ggplot(both_routes,
       aes(x=total_hours_hop, y=total_hours_kr)) +
  geom_jitter(size =4, alpha=.8, width = .05, height=.05) +
  geom_smooth(method='lm', color='black')+
                #, formula = y ~ x + 0) +
  xlim(15,29) +ylim(15,29) +
  theme_light() +
  labs(title = 'Two 400ks King Ridge vs Hopland',
       subtitle = glue('Riders finishing both in the same year (n={scatter_points_n})'))

p2 <-
  ggplot(both_routes,
       aes(x=total_hours_hop, color=!is.na(total_hours_kr))) +
  geom_density() +
  labs(color='Also did King Ridge that year', 
       subtitle=glue('Distibution of all Hopland finish times (n={hop_points_n})')) +
  xlim(13,27) + ylim(0, .25) +
  theme_light() +
  theme(legend.position = 'bottom',
        panel.grid.minor = element_blank())

p3 <-
  ggplot(both_routes,
       aes(x=total_hours_kr, color=!is.na(total_hours_hop))) +
  geom_density() +
  labs(color='Also did Hopland that year', 
       subtitle=glue('Distibution of all King Ridge finish times (n={kr_points_n})')) +
  xlim(13,27) + ylim(0, .25) +
  theme_light() +
  theme(legend.position = 'bottom',
        panel.grid.minor = element_blank())

p1 / (p2 + p3) + plot_layout(heights = c(2,1))
