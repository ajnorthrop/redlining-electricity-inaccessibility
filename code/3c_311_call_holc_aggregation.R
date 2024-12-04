###########***********************
#### Code Description ####
# Author: Alex
# Date: 4/24/23
# Updated: 1/1/24
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

nyc <- read_sf(here("shapes","holc_aggregated"))

holc_hh <- read_csv(here("data","census","holc_income_w_households.csv")) %>% 
  st_drop_geometry() %>% 
  rename(hh = amrie001) %>% 
  dplyr::select(unique_id,hh)

included <- c("NYBronx1938", "NYBrooklyn1938", "NYManhattan1937",
              "NYQueens1938", "NYStatenIsland1940")

nyc_holc_maps <- st_read(here("shapes", "holc_aggregated")) %>% 
  janitor::clean_names() %>% 
  st_transform(., crs = 3748) %>% 
  filter(source %in% included) %>% 
  filter(holc_grade != "E") %>% 
  dplyr::select(-name, -holc_id)

st_crs(nyc_holc_maps) == st_crs(outages)

# HOLC Map 

nyc_holc_maps <- st_transform(nyc_holc_maps, crs = 4326)
outages <- st_transform(outages, crs = 4326) 

data_sf_summary <- nyc_holc_maps %>% 
  mutate(outages = lengths(st_intersects(., outages))) %>% 
  st_centroid() %>% 
  st_transform(., crs = 4326) %>%
  left_join(holc_hh, by = "unique_id") %>% 
  mutate(outage_rate = (outages/(hh*3))*1000) %>% # Adjusts for yearly (2017-2019) per 1000 households 
  filter(!is.na(outage_rate)) %>% 
  dplyr::select(unique_id, holc_grade, hh,outages, outage_rate) %>% 
  st_drop_geometry()

# Split up into warm/cool seasons, too 

outages_cool <- outages %>% filter(season == "cool")
outages_warm <- outages %>% filter(season == "warm")

data_sf_summary_cool <- nyc_holc_maps %>% 
  mutate(outages = lengths(st_intersects(., outages_cool))) %>% 
  st_centroid() %>% 
  st_transform(., crs = 4326) %>%
  left_join(holc_hh, by = "unique_id") %>% 
  mutate(outage_rate = (outages/(hh*1.75))*1000) %>% # Adjusts for yearly (2017-2019) per 1000 households per season 
  filter(!is.na(outage_rate)) %>% 
  st_drop_geometry() %>% 
  mutate(season = "cool") %>% 
  dplyr::select(unique_id, holc_grade, hh,outages, outage_rate, season)

data_sf_summary_warm <- nyc_holc_maps %>% 
  mutate(outages = lengths(st_intersects(., outages_warm))) %>% 
  st_centroid() %>% 
  st_transform(., crs = 4326) %>%
  left_join(holc_hh, by = "unique_id") %>% 
  mutate(outage_rate = (outages/(hh*1.25))*1000) %>% 
  filter(!is.na(outage_rate)) %>% 
  st_drop_geometry() %>% 
  mutate(season = "warm") %>% 
  dplyr::select(unique_id, holc_grade, hh,outages, outage_rate, season)
 
# season_311 <- rbind(data_sf_summary_cool,data_sf_summary_warm) 

# Save files 

data_sf_summary %>% 
  st_drop_geometry() %>% 
  write_csv(here("data","311","holc_311_aggregated.csv"))

data_sf_summary_cool %>% 
  st_drop_geometry() %>% 
  write_csv(here("data","311","holc_311_aggregated_cool.csv"))

data_sf_summary_warm %>% 
  st_drop_geometry() %>% 
  write_csv(here("data","311","holc_311_aggregated_warm.csv"))