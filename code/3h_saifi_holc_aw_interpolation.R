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
included <- c("NYBronx1938", "NYBrooklyn1938", "NYManhattan1937",
              "NYQueens1938", "NYStatenIsland1940")

nyc_holc_maps <- st_read(here("shapes", "holc_aggregated")) %>% 
  janitor::clean_names() %>% 
  st_transform(., crs = 3748) %>% 
  filter(source %in% included) %>% 
  filter(holc_grade != "E") %>% 
  dplyr::select(-name, -holc_id)

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

# Cool Season

saifi_cool <- areal::aw_interpolate(nyc_holc_maps,
                                    tid = unique_id, 
                                    source = pol_saifi_cool,
                                    sid = dps_id,
                                    output = "tibble",
                                    weight = "sum",
                                    intensive = "saifi")

customers_cool <-  areal::aw_interpolate(nyc_holc_maps,
                                         tid = unique_id, 
                                         source = pol_saifi_cool,
                                         sid = dps_id,
                                         output = "tibble",
                                         weight = "sum",
                                         extensive = "customers")


interupptions_cool <-  areal::aw_interpolate(nyc_holc_maps,
                                             tid = unique_id, 
                                             source = pol_saifi_cool,
                                             sid = dps_id,
                                             output = "tibble",
                                             weight = "sum",
                                             extensive = "cust_interruptions")

# Warm Season 

saifi_warm <- areal::aw_interpolate(nyc_holc_maps,
                                    tid = unique_id, 
                                    source = pol_saifi_warm,
                                    sid = dps_id,
                                    output = "tibble",
                                    weight = "sum",
                                    intensive = "saifi")

customers_warm <-  areal::aw_interpolate(nyc_holc_maps,
                                         tid = unique_id,  
                                         source = pol_saifi_warm,
                                         sid = dps_id,
                                         output = "tibble",
                                         weight = "sum",
                                         extensive = "customers")


interupptions_warm <-  areal::aw_interpolate(nyc_holc_maps,
                                             tid = unique_id, 
                                             source = pol_saifi_warm,
                                             sid = dps_id,
                                             output = "tibble",
                                             weight = "sum",
                                             extensive = "cust_interruptions")

# Step 4: Unify in a df-------------------------------------------------------------------------

saifi_holc <- saifi_all %>% 
  left_join(customers_all, by = c("holc_grade", "source","unique_id")) %>% 
  left_join(interupptions_all, c("holc_grade", "source","unique_id"))

saifi_holc_cool <- saifi_cool %>% 
  left_join(customers_cool, by = c("holc_grade", "source","unique_id")) %>% 
  left_join(interupptions_cool, by = c("holc_grade", "source","unique_id"))

saifi_holc_warm <- saifi_warm %>% 
  left_join(customers_warm, by = c("holc_grade", "source","unique_id")) %>% 
  left_join(interupptions_warm, by = c("holc_grade", "source","unique_id")) 


# Step 5: Save data-------------------------------------------------------------------------

write_csv(saifi_holc, here("data","nyspo","holc_saifi_aggregated.csv"))

write_csv(saifi_holc_cool, here("data","nyspo","holc_saifi_cool.csv"))

write_csv(saifi_holc_warm, here("data","nyspo","holc_saifi_warm.csv"))
