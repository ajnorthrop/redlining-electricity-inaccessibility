####***********************
#### Code Description ####
# Author: Vivian & Alex
# Date: 4/18/23
# Goal: Impute the energy use data
####**********************

# Load data ---------------------------------------------------------------
library(mice)
library(splines)
library(mgcv)
library(VIM)
library(here)
library(dplyr)
library(lubridate)
set.seed(421)

################ RESIDENTIAL 

# filter to nyc electriticy residential customers
zip_energy_use <- read.csv(here("data", "energy","raw", "Utility_Energy_Registry_Monthly_ZIP_Code_Energy_Use__Beginning_2016.csv")) %>% 
  janitor::clean_names() %>% 
  filter(data_class == "electricity",
         data_field_display_name == "Residential Consumption (R)",
         utility_display_name == "Consolidated Edison",
         year != 2016)

length(unique(zip_energy_use$zip_code)) #211 distinct zips

# Eval missingness by zip ---------------------------------------------------------------

# how many data points should there be?
# 12 months, 3 years, 211 zips

nrow(zip_energy_use) == 12*3*211 #should be 7596 entries but no

# expand data so every zip has every month-year entry
zip_energy_use_full <- zip_energy_use %>% 
  group_by(zip_code) %>% 
  mutate(Date = as.Date(paste0(year, "-", month, "-01"))) %>% 
  complete(Date = seq.Date(min(as.Date("2017-01-01")), as.Date(max("2019-12-01")), by = "month")) %>% 
  mutate(month = month(Date),
         year = year(Date)) %>% 
  group_by(zip_code, month, year) %>% 
  filter(row_number() == 1)

zip_energy_use_full <- zip_energy_use_full %>% 
  mutate(number_of_accounts = ifelse(is.na(number_of_accounts), 0, number_of_accounts)) 

# Perform imputation 

zip_energy_use_i <- mice(zip_energy_use_full, maxit=100)

zip_energy_use_i <- complete(zip_energy_use_i) 

# Ensure 0s for the two questionable zip codes with 0 customers 

zip_energy_use_i <- zip_energy_use_i %>% 
  mutate(value = case_when(zip_code == 10580 ~ 0,
                             zip_code == 11697 ~ 0,
                             TRUE ~ value)) %>% 
  mutate(data_field_display_name = ifelse(is.na(data_field_display_name), "Residential Consumption (R)", data_field_display_name))


zip_energy_use_i %>% 
  write_csv(here("data","energy","zip_code_imputed_residential_energy_use.csv"))

#### TOTAL 

# filter to nyc electriticy total customers
zip_energy_use <- read.csv(here("data","energy","raw", "Utility_Energy_Registry_Monthly_ZIP_Code_Energy_Use__Beginning_2016.csv")) %>% 
  janitor::clean_names() %>% 
  filter(data_class == "electricity",
         data_field_display_name == "Total Consumption (T)",
         utility_display_name == "Consolidated Edison",
         year != 2016)

# expand data so every zip has every month-year entry
zip_energy_use_full <- zip_energy_use %>% 
  group_by(zip_code) %>% 
  mutate(Date = as.Date(paste0(year, "-", month, "-01"))) %>% 
  complete(Date = seq.Date(min(as.Date("2017-01-01")), as.Date(max("2019-12-01")), by = "month")) %>% 
  mutate(month = month(Date),
         year = year(Date)) %>% 
  group_by(zip_code, month, year) %>% 
  filter(row_number() == 1)

zip_energy_use_full <- zip_energy_use_full %>% 
  mutate(value = ifelse(value == -999, NA, value))

# Note: for Zip Codes 10580 and 11697, data shows 0 customers and 0 enery use 
# These are the only two zips with missing accounts (this happens with total consumption, too)
# thus, we can safely assume that # of accounts for these NA values would be 0 

zip_energy_use_full <- zip_energy_use_full %>% 
  mutate(number_of_accounts = ifelse(is.na(number_of_accounts), 0, number_of_accounts)) 

# Perform imputation 

zip_energy_use_i <- mice(zip_energy_use_full, maxit=100)

zip_energy_use_i <- complete(zip_energy_use_i) 

# Ensure 0s for the two questionable zip codes with 0 customers 

zip_energy_use_i <- zip_energy_use_i %>% 
  mutate(value = case_when(zip_code == 10580 ~ 0,
                           zip_code == 11697 ~ 0,
                           TRUE ~ value)) %>% 
  mutate(data_field_display_name = ifelse(is.na(data_field_display_name), "Total Consumption (T)", data_field_display_name))


zip_energy_use_i %>% 
  write_csv(here("data","energy","zip_code_imputed_total_energy_use.csv"))

#### BUSINESS 

# filter to nyc electriticy total customers
zip_energy_use <- read.csv(here("data", "energy","raw","Utility_Energy_Registry_Monthly_ZIP_Code_Energy_Use__Beginning_2016.csv")) %>% 
  janitor::clean_names() %>% 
  filter(data_class == "electricity",
         data_field_display_name == "Business Consumption (SC+O)",
         utility_display_name == "Consolidated Edison",
         year != 2016)

length(unique(zip_energy_use$zip_code)) #211 distinct zips

nrow(zip_energy_use) == 12*3*211 #should be 7596 entries but no


# expand data so every zip has every month-year entry
zip_energy_use_full <- zip_energy_use %>% 
  group_by(zip_code) %>% 
  mutate(Date = as.Date(paste0(year, "-", month, "-01"))) %>% 
  complete(Date = seq.Date(min(as.Date("2017-01-01")), as.Date(max("2019-12-01")), by = "month")) %>% 
  mutate(month = month(Date),
         year = year(Date)) %>% 
  group_by(zip_code, month, year) %>% 
  filter(row_number() == 1)

nrow(zip_energy_use_full) == 12*3*211 #full data

zip_energy_use_full <- zip_energy_use_full %>% 
  mutate(value = ifelse(value == -999, NA, value))

# Note: for Zip Codes 10580 and 11697, data shows 0 customers and 0 enery use 
# These are the only two zips with missing accounts (this happens with total consumption, too)
# thus, we can safely assume that # of accounts for these NA values would be 0 

zip_energy_use_full <- zip_energy_use_full %>% 
  mutate(number_of_accounts = ifelse(is.na(number_of_accounts), 0, number_of_accounts)) 

# Perform imputation 

zip_energy_use_i <- mice(zip_energy_use_full, maxit=100)

zip_energy_use_i <- complete(zip_energy_use_i) 

# Ensure 0s for the two questionable zip codes with 0 customers 

zip_energy_use_i <- zip_energy_use_i %>% 
  mutate(value = case_when(zip_code == 10580 ~ 0,
                           zip_code == 11697 ~ 0,
                           TRUE ~ value)) %>% 
  mutate(data_field_display_name = ifelse(is.na(data_field_display_name), "Business Consumption (SC+O)", data_field_display_name))

zip_energy_use_i %>% 
  write_csv(here("data","energy","zip_code_imputed_business_energy_use.csv"))

#### Look at these datasets 

residential <- read_csv(here("data","energy","zip_code_imputed_residential_energy_use.csv"))
total <- read_csv(here("data","energy","zip_code_imputed_total_energy_use.csv"))
business <- read_csv(here("data","energy","zip_code_imputed_business_energy_use.csv"))

all <- rbind(residential,total, business)

all %>% 
write_csv(here("data","energy","zip_code_imputed_all_energy_use.csv"))
