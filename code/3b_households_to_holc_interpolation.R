###########***********************
#### Code Description ####
# Author: Alex
# Date: 4/24/23
# Updated: 1/1/24
# Goal: 311 HOLC Analysis
####**********************

library(tigris)

# Load HOLC Boundaries 

nyc_holc <- read_sf(here("shapes","holc_aggregated"))

# Load geographic boundaries 

bg <- block_groups(state = "New York", year = 2020) %>% 
  janitor::clean_names() %>% 
  dplyr::select(geoid, geometry)

# Load Income Data (NHGIS)

nys_bg <- read_csv(here("data","census","nhgis0034_ds249_20205_blck_grp.csv")) %>% 
  janitor::clean_names() %>% 
  dplyr::select(amrie001, amr8e001, geoid)  %>% 
  mutate(income = amrie001*amr8e001) %>% 
  na.omit() %>% 
  mutate(geoid = substring(geoid, 8)) %>% 
  left_join(bg, by = "geoid") %>% 
  st_as_sf()

nyc_holc <- st_transform(nyc_holc, crs = 3748)
nys_bg <- st_transform(nys_bg, crs = 3748)
st_crs(nyc_holc) == st_crs(nys_bg) # TRUE 

# Interpolate 

step_1 <- aw_interpolate(nyc_holc, unique_id, nys_bg, geoid, weight = "sum", extensive = "income") %>% 
  dplyr::select(holc_grade,unique_id,income,geometry) %>% 
  na.omit()

step_2 <- aw_interpolate(nyc_holc, unique_id, nys_bg, geoid, weight = "sum", extensive = "amrie001") %>% 
  dplyr::select(unique_id,amrie001) %>% 
  st_drop_geometry() %>% 
  na.omit()

holc_income <- step_1 %>% 
  left_join(step_2, by = "unique_id") %>% 
  mutate(income = income/amrie001) %>% 
  na.omit() %>% 
  filter(holc_grade != "E")

holc_income %>% 
  write_csv(here("data","census","holc_income_w_households.csv"))
