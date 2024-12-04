####***********************
#### Code Description ####
# Author: Alex
# Date: 6/29/22
# Last Update: 10/5/24
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

# POL Shapefile (source)

customers <- nyspo_saifi %>% 
  dplyr::select(dps_id, customers)

locality <- st_read(here("shapes","e_locality")) %>% 
  janitor::clean_names() %>% 
  rename(dps_id = prime_dps) %>%
  st_transform(., crs = 3748) %>% 
  dplyr::select(dps_id, geometry) 

# HOLC  shapefile (target)

# Included HOLC maps

nyc_holc_maps <- st_read(here("shapes", "holc_ungraded")) %>% 
  janitor::clean_names() %>% 
  st_transform(., crs = 3748) %>% mutate(unique_id = "U")

# Step 2: Attach locality shapefile to data-------------------------------------------------------------------------

pol_saifi <- locality %>%
  left_join(nyspo_saifi, by = "dps_id") %>% na.omit()

# Step 3: Perform areal interpolation-------------------------------------------------------------------------

# All Season 

saifi_all <- areal::aw_interpolate(nyc_holc_maps,
                                   tid = unique_id, 
                                   source = pol_saifi,
                                   sid = dps_id,
                                   output = "tibble",
                                   weight = "sum",
                                   intensive = "saifi")

customers_all <-  areal::aw_interpolate(nyc_holc_maps,
                                        tid = unique_id, 
                                        source = pol_saifi,
                                        sid = dps_id,
                                        output = "tibble",
                                        weight = "sum",
                                        extensive = "customers")


interupptions_all <-  areal::aw_interpolate(nyc_holc_maps,
                                            tid = unique_id, 
                                            source = pol_saifi,
                                            sid = dps_id,
                                            output = "tibble",
                                            weight = "sum",
                                            extensive = "cust_interruptions")

# Step 4: Unify in a df-------------------------------------------------------------------------

saifi_holc <- saifi_all %>% 
  left_join(customers_all, by = c("unique_id","grade")) 


# Step 5: Capture data-------------------------------------------------------------------------

print(saifi_holc$saifi)