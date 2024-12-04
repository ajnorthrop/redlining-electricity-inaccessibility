###########***********************
#### Code Description ####
# Author: Alex
# Date: 4/2/24
# Last Updated: 4/5/24
# Goal: 311 HOLC Analysis
####**********************

# Load packages 
rm(list=ls())
library(here)
source(paste0(here("code","0_setup", "0_read_libraries.R")))

# Step 1: Load data required-------------------------------------------------------------------------

ice <- read_csv(paste0(here("data", "ice", "race_and_income", "final", "ct_race_income_nyc.csv"))) # 2217

# Load HOLC households (see 3b)

hh <- read_csv(here("data","census","holc_income_w_households.csv")) %>% 
  dplyr::select(unique_id, amrie001) %>% 
  rename(hh = amrie001)

# Load HOLC Boundaries 

included <- c("NYBronx1938", "NYBrooklyn1938", "NYManhattan1937",
              "NYQueens1938", "NYStatenIsland1940")

nyc_holc <- read_sf(here("shapes","holc_aggregated")) %>% 
  st_transform(., crs = 3748) %>% 
  mutate(holc_area = st_area(geometry)) %>% 
  left_join(hh, by = "unique_id") %>% 
  filter(source %in% included)

# Load CT Boundaries 

ny_census_tracts <- read_sf(here("shapes","nhgis_cts_2020")) %>% 
  janitor::clean_names() %>% 
  filter(statefp == "36") %>% 
  filter(countyfp == "061" |
           countyfp == "081" |
           countyfp == "085" |
           countyfp == "047" |
           countyfp == "005") %>% 
  dplyr::select(geoid, geometry, countyfp) %>%
  st_transform(., crs = 3748) %>% 
  mutate(geoid = as.double(geoid)) %>% 
  mutate(ct_area = st_area(geometry)) 

# Step 2: Interpolate-------------------------------------------------------------------------

intersection <- aw_intersect(nyc_holc, ny_census_tracts, area_var) 

total <- aw_total(intersection, nyc_holc, unique_id, area_var, total_var, type = "extensive", weight = "sum")

weight <- aw_weight(total, area_var, total_var, area_weight)

calculated <- aw_calculate(weight, hh, area_weight, hh_holc_ct)

# Step 3: Aggregate-------------------------------------------------------------------------

ct_holc <- calculated %>%
  st_drop_geometry() %>%
  group_by(holc_grade, geoid) %>% 
  summarize(hh_holc_ct = sum(hh_holc_ct)) %>%
  mutate(col_a = ifelse(holc_grade == "A", hh_holc_ct, 0),
         col_b = ifelse(holc_grade == "B", hh_holc_ct, 0),
         col_c = ifelse(holc_grade == "C", hh_holc_ct, 0),
         col_d = ifelse(holc_grade == "D", hh_holc_ct, 0)) %>% 
  group_by(geoid) %>% 
  summarize(col_a = sum(col_a),
            col_b = sum(col_b),
            col_c = sum(col_c),
            col_d = sum(col_d)) %>%
  mutate(total = col_a + col_b + col_c + col_d) %>%
  mutate(priv = (col_a + col_b)/total,
         non_priv = (col_c + col_d)/total) %>%
  mutate(holc = ifelse(priv > 0.5, "AB", "CD")) %>% 
  dplyr::select(geoid, holc)

# Step 4: Add quartiles -------------------------------------------------------------------------

cc <- ice %>% 
  left_join(ct_holc, by = "geoid") %>% 
  dplyr::select(geoid,holc,ice_quartile) %>% 
  mutate(cc = ifelse(holc == "AB" & (ice_quartile == "Q4" | ice_quartile == "Q3"), "High-High", NA),
         cc = ifelse(holc == "CD" & (ice_quartile == "Q1" | ice_quartile == "Q2"), "Low-Low", cc),
         cc = ifelse(holc == "AB" & (ice_quartile == "Q1" | ice_quartile == "Q2"), "High-Low", cc),
         cc = ifelse(holc == "CD" & (ice_quartile == "Q4" | ice_quartile == "Q3"), "Low-High", cc)) %>% 
  dplyr::select(geoid, cc)

# Step 5: Save -------------------------------------------------------------------------

cc <- cc %>% 
  filter(!is.na(cc)) 

cc$cc <- factor(cc$cc, levels = c("High-High", "Low-High", "High-Low", "Low-Low"))

cc %>%
  write_csv(here("data","ice","concordance","concordance_dta.csv"))
