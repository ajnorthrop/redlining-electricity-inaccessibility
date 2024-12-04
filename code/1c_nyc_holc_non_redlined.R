####**************************
# Code Description ####
#*Goal: Aggregate NYC HOLC Map
#*Created 11/6/2022
#*Last Edit: 12/22/2023
####**************************

sf_use_s2(FALSE)

### Redlining ###

# HOLC Maps and get relevant geometries 

holc_combined <- read_sf(here("shapes","holc_aggregated", "holc_aggregated.shp")) %>% 
  st_transform(., crs = 3748) %>% 
  select(holc_grade, unique_id, geometry, source) %>%
  filter(source == "NYBronx1938" | source == "NYBrooklyn1938" | source == "NYManhattan1937" | source == "NYQueens1938" | source == "NYStatenIsland1940") %>% 
  select(holc_grade, geometry) %>% st_union() %>% st_as_sf() %>%  mutate(grade = "y")

# We restrict the ungraded areas for areas captured by the ICE metric in NYC 

ice <- read_csv(paste0(here("data", "ice", "race_and_income", "final", "ct_race_income_nyc.csv"))) 

ct <- st_read(here("shapes", "nhgis_cts_2020")) %>% 
  janitor::clean_names() %>% 
  filter(countyfp %in% c("005", "047", "061","081", "085")) %>% 
  filter(statefp == "36") %>% 
  mutate(geoid = as.double(geoid)) %>% 
  st_transform(., crs = 3748) %>% 
  dplyr::select(geoid,geometry, countyfp) %>% 
  left_join(ice, by = "geoid") %>% 
  filter(!is.na(ice)) %>% 
  st_union() %>% st_as_sf() %>%  mutate(grade = "n")

# Create a "cut out" of 

combined <- rbind(holc_combined, ct) 

non_overlaps <- combined %>% st_intersection() %>% st_as_sf() %>% rename(geometry = x) %>% 
  filter(n.overlaps == 1) %>% filter(grade == "n") %>% select(grade, geometry)

st_write(non_overlaps, here("shapes","holc_ungraded", "holc_ungraded.shp"))