###########***********************
#### Code Description ####
# Author: Alex
# Date: 7/23/23
# Updated: 1/1/24
# Goal: 311 to Census Tracts (2020)
####**********************

## Read the data 

three_11 <- read_fst(here("data","311","three_one_one.fst"))

outages <- three_11 %>% 
  filter(descriptor == "POWER OUTAGE") %>% 
  filter(!is.na(longitude) | !is.na(latitude)) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs=4326) %>% 
  st_transform(., crs = 3748) %>%
  filter(year(created_date) > 2016 & year(created_date) < 2020) %>% 
  mutate(season = ifelse(month(created_date) <5 | month(created_date) > 9, "cool", "warm"))

hh <- read_csv(here("data","census","nhgis0039_ds249_20205_tract.csv")) %>% 
  janitor::clean_names() %>% 
  filter(statea == "36") %>% 
  filter(countya == "061" |
           countya == "081" |
           countya == "085" |
           countya == "047" |
           countya == "005") %>% 
  mutate(geoid = paste0(statea,countya,tracta)) %>%
  select(geoid, amqse001)

census_tracts <- read_sf(here("shapes","nhgis_cts_2020")) %>% 
  janitor::clean_names() %>% 
  filter(statefp == "36") %>% 
  filter(countyfp == "061" |
           countyfp == "081" |
           countyfp == "085" |
           countyfp == "047" |
           countyfp == "005") %>% 
  select(geoid, geometry, countyfp) %>%
  st_transform(., crs = 3748) %>% 
  left_join(hh, by = c("geoid")) 

census_tracts <- census_tracts %>% 
  rename(hh = amqse001) 

st_crs(census_tracts) == st_crs(outages)

# # Check outages 
# 
# ggplot(outages) +
#   geom_sf(aes(geometry = geometry), alpha = 0.1, color = "black") 
# 
# # Check CT map 
# 
# ggplot(census_tracts) +
#   geom_sf(aes(geometry = geometry), alpha = 0.1, color = "black") 

# Create an aggregate of 311 outage calls per CT 

# Census Tract Yearly

data_sf_summary <- census_tracts %>% 
  mutate(outages = lengths(st_intersects(., outages))) %>% 
  select(geoid, geometry, outages, hh) %>% 
  mutate(outage_rate = (outages/(hh*3) * 1000))

# Census Tract Seasonal 

outages_cool <- outages %>% 
  filter(season == "cool")

outages_warm <- outages %>% 
  filter(season == "warm")

data_sf_summary_cool <- census_tracts %>% 
  mutate(outages = lengths(st_intersects(., outages_cool))) %>% 
  select(geoid, geometry, outages, hh) %>% 
  mutate(outage_rate = (outages/(hh*1.75) * 1000)) %>% # Rate for cool season
  mutate(season = "cool")

data_sf_summary_warm <- census_tracts %>% 
  mutate(outages = lengths(st_intersects(., outages_warm))) %>% 
  select(geoid, geometry, outages, hh) %>% 
  mutate(outage_rate = (outages/(hh*1.25) * 1000)) %>% # Rate for warm season
  mutate(season = "warm")

data_sf_summary_szn <- rbind(data_sf_summary_cool, data_sf_summary_warm) %>% 
  st_drop_geometry()

# Will add in the ICE data in later stages of the data processing 

# ct_ice <- read_csv(here("data","ice","race_and_income","final","ct_race_income_nyc.csv")) %>% 
#   janitor::clean_names() %>%
#   mutate(geoid = str_sub(geoid, 8)) %>% 
#   mutate(geoid = as.double(geoid)) %>%
#   mutate(ice_percentile = percent_rank(ct_ice_race_income),
#          ice_quartile = case_when(ice_percentile < .25 ~ "Quartile 1 (Least Privileged)",
#                                   ice_percentile < .5 & ice_percentile >= .25 ~ "Quartile 2",
#                                   ice_percentile < .75 & ice_percentile >= .5 ~ "Quartile 3",
#                                   ice_percentile >= .75 ~ "Quartile 4 (Most Privileged)")) %>% 
#   mutate(geoid = as.character(geoid))

# data_sf_summary <- data_sf_summary %>% 
#   left_join(ct_ice, by ="geoid") %>% 
#   filter(!is.na(ct_ice_race_income))
# 
# data_sf_summary_szn <- data_sf_summary_szn %>% 
#   left_join(ct_ice, by ="geoid") %>% 
#   filter(!is.na(ct_ice_race_income))

# Clean the data frames 

data_sf_summary <- data_sf_summary %>% 
  filter(!is.nan(outage_rate)) %>% 
  select(geoid, outages, hh, outage_rate) %>% 
  st_drop_geometry()

data_sf_summary_cool <- data_sf_summary_cool %>% 
  filter(!is.nan(outage_rate)) %>% 
  select(geoid, outages, hh, outage_rate, season) %>% 
  st_drop_geometry()

data_sf_summary_warm <- data_sf_summary_warm %>% 
  filter(!is.nan(outage_rate)) %>% 
  select(geoid, outages, hh, outage_rate, season) %>% 
  st_drop_geometry()

# Save the data 

write_csv(data_sf_summary, here("data","311","ct_311_aggregated.csv"))
write_csv(data_sf_summary_warm, here("data","311","ct_311_aggregated_warm.csv"))
write_csv(data_sf_summary_cool, here("data","311","ct_311_aggregated_cool.csv"))

