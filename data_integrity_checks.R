library(clipr)

# there are dupe'd fields btw events and event details. do they match?
e_er <- merge(events, event_details, by='eid', all.x = T)
# 3.5% of data is NA in event_details
sapply(e_er, function(x) sum(is.na(x)) / length(x))

# no mismatches outside of NA
e_er %>%
  summarise(n(),
            reg_pct =  sum(region.x != region.y, na.rm = T),
            club_pct = sum(club.x != club.y, na.rm = T),
            type_pct = sum(type.x != type.y, na.rm = T),
            dist_pct = sum(distance.x != distance.y, na.rm = T),
            date_pct = sum(date.x != date.y, na.rm = T))

# clubs and regions
events %>%
  group_by(club) %>%
  summarise(n=n()) %>%
  arrange(-n) %>%
  print(n=Inf)
events %>%
  group_by(region) %>%
  summarise(n=n(), clubs = length(unique(club))) %>%
  arrange(-n) %>%
  print(n=Inf)

# riders -- names are not unique!
event_results %>%
  group_by(rusa) %>%
  summarise(names = length(unique(name))) %>%
  ungroup() %>%
  group_by(names) %>%
  tally()

rusa_dupes <- event_results %>%
  group_by(rusa) %>%
  summarise(names = length(unique(name))) %>%
  filter(names>1)

event_results %>%
  filter(rusa %in% rusa_dupes$rusa) %>%
  arrange(rusa) %>% View

# some people have racked up >1 club affiliation
event_results %>%
  group_by(rusa) %>%
  summarise(clubs = length(unique(`club / acp code`))) %>%
  ungroup() %>%
  group_by(clubs) %>%
  tally() 

# routes ----
events %>%
  group_by(rtid) %>%
  summarise(routes = length(unique(route))) %>%
  ungroup() %>%
  group_by(routes) %>%
  tally() 

events %>%
  filter(rtid %in% (
    events %>%
      group_by(rtid) %>%
      summarise(routes = length(unique(route))) %>%
      filter(routes > 1) %>%
      .$rtid
  ))

# DNF ----
event_details %>%
  filter(finishers > 0 | dnf > 0) %>%
  group_by(yr = year(date)) %>%
  summarise(zero_dnf  = sum(dnf==0, na.rm = T),
            na_dnf    = sum(is.na(dnf)),
            neg_1_dnf = sum(dnf==-1, na.rm = T),
            some_dnf  = sum(dnf >0, na.rm = T) )


