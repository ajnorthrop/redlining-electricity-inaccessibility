####***********************
#### Code Description ####
# Author: Alex
# Date: 2/24/24
# Last Update: 2/24/24
# Goal: Apply filters to the data
####**********************

# Load packages 
rm(list=ls())
library(here)
source(paste0(here("code","0_setup", "0_read_libraries.R")))

# Step 1: Load data required, set filter, and save-------------------------------------------------------------------------

# Sets the filter cutoff value for all dfs 
cutoff <- 30

### Load CT Data 

## 311 

ct_311_aggregated <- read_csv(here("data","311","ct_311_aggregated.csv")) %>% 
  filter(hh > cutoff) %>% 
  write_csv(., here("data","311","final","ct_311_final.csv"))

ct_311_cool <- read_csv(here("data","311","ct_311_aggregated_cool.csv")) %>% 
  filter(hh > cutoff) %>% 
  write_csv(., here("data","311","final","ct_311_cool_final.csv"))

ct_311_warm <- read_csv(here("data","311","ct_311_aggregated_warm.csv")) %>% 
  filter(hh > cutoff) %>%
  write_csv(., here("data","311","final","ct_311_warm_final.csv"))

## Energy

ct_energy_aggregated <- read_csv(here("data","energy","ct_energy_use_aggregated.csv")) %>% 
  filter(number_of_accounts > cutoff) %>%
  write_csv(., here("data","energy","final","ct_energy_final.csv"))

ct_energy_cool <- read_csv(here("data","energy","ct_energy_use_cool.csv")) %>% 
  filter(number_of_accounts > cutoff) %>%
  write_csv(., here("data","energy","final","ct_energy_cool_final.csv"))

ct_energy_warm <- read_csv(here("data","energy","ct_energy_use_warm.csv")) %>% 
  filter(number_of_accounts > cutoff) %>%
  write_csv(., here("data","energy","final","ct_energy_warm_final.csv"))

## SAIFI

ct_saifi_aggregated <- read_csv(here("data","nyspo","ct_saifi_aggregated.csv")) %>% 
  filter(customers > cutoff) %>%
  write_csv(., here("data","nyspo","final","ct_saifi_final.csv"))

ct_saifi_cool <- read_csv(here("data","nyspo","ct_saifi_cool.csv")) %>% 
  filter(customers > cutoff) %>%
  write_csv(., here("data","nyspo","final","ct_saifi_cool_final.csv"))
  
ct_saifi_warm <- read_csv(here("data","nyspo","ct_saifi_warm.csv")) %>% 
  filter(customers > cutoff) %>%
  write_csv(., here("data","nyspo","final","ct_saifi_warm_final.csv"))

### Load HOLC Data 

## 311 

holc_311_aggregated <- read_csv(here("data","311","holc_311_aggregated.csv")) %>% 
  filter(hh > cutoff) %>%
  write_csv(., here("data","311","final","holc_311_final.csv"))

holc_311_cool <- read_csv(here("data","311","holc_311_aggregated_cool.csv")) %>% 
  filter(hh > cutoff) %>%
  write_csv(., here("data","311","final","holc_311_cool_final.csv"))

holc_311_warm <- read_csv(here("data","311","holc_311_aggregated_warm.csv")) %>% 
  filter(hh > cutoff) %>%
  write_csv(., here("data","311","final","holc_311_warm_final.csv"))

## Energy

holc_energy_aggregated <- read_csv(here("data","energy","holc_energy_use_aggregated.csv")) %>% 
  filter(number_of_accounts > cutoff) %>%
  write_csv(., here("data","energy","final","holc_energy_final.csv"))

holc_energy_cool <- read_csv(here("data","energy","holc_energy_use_cool.csv")) %>% 
  filter(number_of_accounts > cutoff) %>%
  write_csv(., here("data","energy","final","holc_energy_cool_final.csv"))

holc_energy_warm <- read_csv(here("data","energy","holc_energy_use_warm.csv")) %>% 
  filter(number_of_accounts > cutoff) %>%
  write_csv(., here("data","energy","final","holc_energy_warm_final.csv"))

## SAIFI

holc_saifi_aggregated <- read_csv(here("data","nyspo","holc_saifi_aggregated.csv")) %>% 
  filter(customers > cutoff) %>%
  write_csv(., here("data","nyspo","final","holc_saifi_final.csv"))

holc_saifi_cool <- read_csv(here("data","nyspo","holc_saifi_cool.csv")) %>% 
  filter(customers > cutoff) %>%
  write_csv(., here("data","nyspo","final","holc_saifi_cool_final.csv"))

holc_saifi_warm <- read_csv(here("data","nyspo","holc_saifi_warm.csv")) %>% 
  filter(customers > cutoff) %>%
  write_csv(., here("data","nyspo","final","holc_saifi_warm_final.csv"))

### For inclusion numbers 

n_distinct(ct_311_aggregated$geoid)
n_distinct(ct_energy_aggregated$geoid)
n_distinct(ct_saifi_aggregated$geoid)

n_distinct(holc_311_aggregated$unique_id)
n_distinct(holc_energy_aggregated$unique_id)
n_distinct(holc_saifi_aggregated$unique_id)

holc_saifi_aggregated %>% group_by(holc_grade) %>% summarize(count = n())


