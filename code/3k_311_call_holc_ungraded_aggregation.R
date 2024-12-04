###########***********************
#### Code Description ####
# Author: Alex
# Date: 4/24/23
# Updated: 10/5/24
# Goal: 311 HOLC Analysis
####**********************

three_11 <- read_fst(here("data","311","three_one_one.fst"))

outages <- three_11 %>% 
  filter(descriptor == "POWER OUTAGE") %>% 
  filter(!is.na(longitude) | !is.na(latitude)) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs=4326) %>% 
  st_transform(., crs = 3748)  %>% 
  filter(year(created_date) > 2016 & year(created_date) < 2020) %>% 
  mutate(season = ifelse(month(created_date) <5 | month(created_date) > 9, "cool", "warm"))

nyc <- read_sf(here("shapes","holc_ungraded"))

holc_hh <- read_csv(here("data","census","holc_income_w_households_ungraded.csv")) %>% 
  st_drop_geometry() %>% 
  rename(hh = amrie001) %>% 
  dplyr::select(unique_id,hh)

nyc_holc_maps <- st_read(here("shapes", "holc_ungraded")) %>% 
  janitor::clean_names() %>% 
  st_transform(., crs = 3748) 

st_crs(nyc_holc_maps) == st_crs(outages)

# HOLC Map 

nyc_holc_maps <- st_transform(nyc_holc_maps, crs = 4326)
outages <- st_transform(outages, crs = 4326) 

data_sf_summary <- nyc_holc_maps %>% 
  mutate(outages = lengths(st_intersects(., outages))) %>% 
  st_centroid() %>% 
  st_transform(., crs = 4326) %>%
  mutate(unique_id = "U") %>% 
  left_join(holc_hh, by = "unique_id") %>% 
  mutate(outage_rate = (outages/(hh*3))*1000) %>% # Adjusts for yearly (2017-2019) per 1000 households 
  st_drop_geometry()

print(data_sf_summary$outage_rate)

# Outage rate: 0.2813454