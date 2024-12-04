####**************************
# Code Description ####
#*Goal: Imputed NYS Dataset 
#*Created 4/21/2023
####**************************
####*
####*
####*

library(mice)

locality <- st_read(here("shapes","e_locality")) %>% 
  janitor::clean_names() %>% 
  rename(dps_id = prime_dps) %>%
  select(dps_id, geometry) 

nyspo <- read_fst(here("data","nyspo","nys_power_outages.fst"))

nyspo_day <- nyspo %>% 
  dplyr::select(dps_id, datetime,customers_out,customers) %>% 
  mutate(date = lubridate::as_date(datetime, tz = "EST"),
         hour = hour(datetime)) %>%
  group_by(dps_id) %>% 
  complete(date = seq.Date(min(as_date("2017-01-01")), as_date(max("2020-12-31")), by = "day")) %>% 
  ungroup()

nyspo_day <- nyspo_day %>% 
  group_by(dps_id, date) %>% 
  complete(hour = seq.int(from = 0, to = 23, by = 1)) 

average_customers <- nyspo_day %>% 
  group_by(dps_id) %>% 
  summarize(average_customers = mean(customers, na.rm = T))

nyspo_complete <- nyspo_day %>% 
  left_join(average_customers, by ="dps_id") %>% 
  mutate(customers = ifelse(is.na(customers), average_customers, customers)) %>% 
  dplyr::select(-datetime)

nyspo_complete_i <- mice(nyspo_complete, maxit=5)

nyspo_complete_i <- complete(nyspo_complete_i) 


nyspo_complete_i <- read_fst(here("data","nyspo","imputed_nyspo.fst"))

nyspo <- nyspo_complete_i %>% 
  group_by(dps_id, date, hour) %>% 
  filter(row_number() == 1) %>% 
  ungroup()

nyspo %>% 
  write_fst(here("data","nyspo","imputed_nyspo.fst"))
