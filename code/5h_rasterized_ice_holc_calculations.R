####***********************
#### Code Description ####
# Author: Alex
# Created: 8/19/23
# Last Update: 2/25/24 
# Goal: Place Outage Information in Standardized Grids 
####**********************

# Load packages 
rm(list=ls())
library(here)
library(raster)
library(fasterize)
library(terra)
library(ggsankey)
source(paste0(here("code","0_setup", "0_read_libraries.R")))

# Step 1: Load data required-------------------------------------------------------------------------
# Load ICE - CT 

nyc_df <- read_csv(here("data","raster","nyc_raster_data.csv")) 

# Get units by HOLC grade 

holc <- nyc_df %>% 
  group_by(holc_grade) %>% 
  summarize(count_sum = n()) 

holc

# Get units by ICE quartile 

ice <- nyc_df %>% 
  group_by(ice_quantile) %>% 
  summarize(count = n())

ice

# Get units by HOLC grade and ICE quartile 

holc_ice <- nyc_df %>% 
  group_by(holc_grade, ice_quantile) %>% 
  summarize(count = n())

holc_ice

# Get overall percentages 

total <- holc %>% summarize(total = sum(count_sum))

(10000*total)/783249480 # units (.1 km^2) converted to meters^2 divided by total land area in m^2

# Attach sum to full df 

combined <- holc_ice %>% 
  left_join(holc, by = "holc_grade") %>% 
  mutate(percentage = ((count/count_sum) * 100)) %>% 
  mutate(percentage_all = (count/total$total) * 100) %>% 
  mutate(percentage = round(percentage, 1),
         percentage_all = round(percentage_all, 1)) 

total_used <- combined %>% filter(holc_grade != "None") 

sum(total_used$count)

print(combined)

write.table(combined, file = here("tables","raster_holc_ice_table.txt"), sep = ",", quote = F, row.names = T)

# Spearman correlation 

pre_cor <- nyc_df %>% 
  filter(holc_grade != "None") %>% 
  dplyr::select(holc_grade, ice_quantile) %>% 
  mutate(holc_grade = case_when(holc_grade == "A" ~ 1,
                                holc_grade == "B" ~ 2,
                                holc_grade == "C" ~ 3,
                                holc_grade == "D" ~ 4),
         ice_quantile = case_when(ice_quantile == "Quartile 1 (Least Privileged)" ~ 4,
                                  ice_quantile == "Quartile 2" ~ 3,
                                  ice_quantile == "Quartile 3" ~ 2,
                                  ice_quantile == "Quartile 4 (Most Privileged)" ~ 1))

# Get correlation coefficients 

cor(pre_cor$holc_grade,pre_cor$ice_quantile, method="spearman")




