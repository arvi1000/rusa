input_data <- list(
  list(rtid=809, name="SFR Two Rock Valley Ford"),
  list(rtid=1158, name='SFR Del Puerto Canyon'),
  list(rtid=303, name='Westfield MA 400k')
)

# plot one event
dat <- get_results(rtid = input_data[[3]]$rtid) 
summary_plot(dat, route_name = input_data[[3]]$name,
             hour_bindwidth = 1)

