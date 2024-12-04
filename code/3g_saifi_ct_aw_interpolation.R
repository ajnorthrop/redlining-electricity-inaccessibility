####***********************
#### Code Description ####
# Author: Alex
# Date: 6/29/22
# Last Update: 2/24/24
# Goal: SAIFI CT Interpolation
####**********************

# Load packages (read_libraries)
# Load specific package for this process 
# Documentation: https://cran.r-project.org/web/packages/areal/vignettes/areal.html

rm(list=ls())
library(here)
source(paste0(here("code","0_setup", "0_read_libraries.R")))

# Step 1: Load data required-------------------------------------------------------------------------

# Energy use data 

nyspo_saifi <- read_csv(here("data","nyspo","saifi_17_19.csv"))

nyspo_saifi_seasonal <- read_csv(here("data","nyspo","saifi_17_19_szn.csv"))

# ICE 

ice <- read_csv(paste0(here("data", "ice", "race_and_income", "final", "ct_race_income_nyc.csv"))) # 2217

# POL Shapefile (source)
customers <- nyspo_saifi %>% 
  dplyr::select(dps_id, customers)

locality <- st_read(here("shapes","e_locality")) %>% 
  janitor::clean_names() %>% 
  rename(dps_id = prime_dps) %>%
  st_transform(., crs = 3748) %>% 
  dplyr::select(dps_id, geometry) 

# Census tract shapefile (target)

ny_census_tracts <- read_sf(here("shapes","nhgis_cts_2020")) %>% 
  janitor::clean_names() %>% 
  filter(statefp == "36") %>% 
  filter(countyfp == "061" |
           countyfp == "081" |
           countyfp == "085" |
           countyfp == "047" |
           countyfp == "005") %>% 
  select(geoid, geometry, countyfp) %>%
  st_transform(., crs = 3748) %>% 
  mutate(geoid = as.double(geoid)) 

# Step 2: Attach locality shapefile to data-------------------------------------------------------------------------

pol_saifi <- locality %>%
  left_join(nyspo_saifi, by = "dps_id") %>% na.omit()

pol_saifi_warm <- locality %>%
  left_join(nyspo_saifi_seasonal, by = "dps_id") %>%
  filter(season == "warm") %>% na.omit()

pol_saifi_cool <- locality %>%
  left_join(nyspo_saifi_seasonal, by = "dps_id") %>% 
  filter(season == "cool") %>% na.omit()

# Step 3: Perform areal interpolation-------------------------------------------------------------------------

# All Season 

saifi_all <- areal::aw_interpolate(ny_census_tracts,
                                   tid = geoid, 
                                   source = pol_saifi,
                                   sid = dps_id,
                                   output = "tibble",
                                   weight = "sum",
                                   intensive = "saifi")

customers_all <-  areal::aw_interpolate(ny_census_tracts,
                                        tid = geoid, 
                                        source = pol_saifi,
                                        sid = dps_id,
                                        output = "tibble",
                                        weight = "sum",
                                        extensive = "customers")


interupptions_all <-  areal::aw_interpolate(ny_census_tracts,
                                        tid = geoid, 
                                        source = pol_saifi,
                                        sid = dps_id,
                                        output = "tibble",
                                        weight = "sum",
                                        extensive = "cust_interruptions")

# Cool Season

saifi_cool <- areal::aw_interpolate(ny_census_tracts,
                                   tid = geoid, 
                                   source = pol_saifi_cool,
                                   sid = dps_id,
                                   output = "tibble",
                                   weight = "sum",
                                   intensive = "saifi")

customers_cool <-  areal::aw_interpolate(ny_census_tracts,
                                        tid = geoid, 
                                        source = pol_saifi_cool,
                                        sid = dps_id,
                                        output = "tibble",
                                        weight = "sum",
                                        extensive = "customers")


interupptions_cool <-  areal::aw_interpolate(ny_census_tracts,
                                            tid = geoid, 
                                            source = pol_saifi_cool,
                                            sid = dps_id,
                                            output = "tibble",
                                            weight = "sum",
                                            extensive = "cust_interruptions")

# Warm Season 

saifi_warm <- areal::aw_interpolate(ny_census_tracts,
                                    tid = geoid, 
                                    source = pol_saifi_warm,
                                    sid = dps_id,
                                    output = "tibble",
                                    weight = "sum",
                                    intensive = "saifi")

customers_warm <-  areal::aw_interpolate(ny_census_tracts,
                                         tid = geoid, 
                                         source = pol_saifi_warm,
                                         sid = dps_id,
                                         output = "tibble",
                                         weight = "sum",
                                         extensive = "customers")


interupptions_warm <-  areal::aw_interpolate(ny_census_tracts,
                                             tid = geoid, 
                                             source = pol_saifi_warm,
                                             sid = dps_id,
                                             output = "tibble",
                                             weight = "sum",
                                             extensive = "cust_interruptions")

# Step 4: Unify in a df-------------------------------------------------------------------------

saifi_ct <- saifi_all %>% 
  left_join(customers_all, by = c("geoid", "countyfp")) %>% 
  left_join(interupptions_all, by = c("geoid", "countyfp")) %>% 
  left_join(ice, by = c("geoid")) %>% na.omit() %>% select(1:5) # ensures only data with ICE are included 

saifi_ct_cool <- saifi_cool %>% 
  left_join(customers_cool, by = c("geoid", "countyfp")) %>% 
  left_join(interupptions_cool, by = c("geoid", "countyfp")) %>% 
  left_join(ice, by = c("geoid")) %>% na.omit() %>% select(1:5) # ensures only data with ICE are included 

saifi_ct_warm <- saifi_warm %>% 
  left_join(customers_warm, by = c("geoid", "countyfp")) %>% 
  left_join(interupptions_warm, by = c("geoid", "countyfp")) %>% 
  left_join(ice, by = c("geoid")) %>% na.omit() %>% select(1:5) # ensures only data with ICE are included 


# Step 5: Save data-------------------------------------------------------------------------

write_csv(saifi_ct, here("data","nyspo","ct_saifi_aggregated.csv"))

write_csv(saifi_ct_cool, here("data","nyspo","ct_saifi_cool.csv"))

write_csv(saifi_ct_warm, here("data","nyspo","ct_saifi_warm.csv"))
