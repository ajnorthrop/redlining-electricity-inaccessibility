# Code description --------------------------------------------------------
# Author: Vivian
# Date: 7/31/23
# Last Updated: 4/5/24
# Goal: Calculate ice metrics for NYC census tracts
# Methods inspired by Nancy Krieger here: https://ajph.aphapublications.org/doi/full/10.2105/AJPH.2015.302955
# Data obtained from NHGIS: http://doi.org/10.18128/D050.V18.0
# We define ICE using the following:
# ICE race alone - disadvantaged group = non-white groups; advantaged group = white
# ICE racialized economic segregation (race + income) - XXX; YYY

library(tidyverse)

# ICE - race alone ----------------------------------------------------------
# read in data, keeping only variables and observations of interest
# AMPWE001 = total
# AMPWE002 = white alone
# AMPWE0XX = all others
ct_race_alone <- read_csv(paste0(here("data", "census","nhgis0027_ds249_20205_tract.csv"))) %>% 
  filter(STATEA == "36",
         COUNTYA %in% c("005", "047", "061","081", "085")) %>% 
  dplyr::select("GISJOIN", "STATEA", "COUNTYA", starts_with("AMPWE0")) %>% 
  filter(rowSums(dplyr::select(., starts_with("AMPWE"))) > 30) # remove rows where all population is less than 30 (still keep 99.5%)

# generate ice metric
# we use estimates so total of individual race categories can exceed estimated total category
ct_race_alone <- ct_race_alone %>% 
  rename(n_total = AMPWE001,
         n_white = AMPWE002) %>% 
  rowwise() %>% 
  mutate(n_non_white = sum(AMPWE003, AMPWE004, AMPWE005, AMPWE006, AMPWE007, AMPWE008)) %>% 
  mutate(ct_ice_race_only = (n_white - n_non_white)/(n_total)) 

write_csv(ct_race_alone, paste0(here("data", "ice", "race_alone", "final", "ct_race_alone.csv")))

# ICE - race + income -----------------------------------------------------------
# census tract income, limited to NYC
medhhinc_nyc <- read_csv(paste0(here("data", "census", "nhgis0030_ds249_20205_tract.csv"))) %>% 
  filter(STATEA == "36",
         COUNTYA %in% c("005", "047", "061","081", "085")) %>% 
  dplyr::select("GISJOIN", "STATEA", "COUNTYA", "AMR8E001") %>% 
  rename(medhhinc = AMR8E001) %>% 
  filter(!is.na(medhhinc))

quantile(medhhinc_nyc$medhhinc, probs = 0.2) # 45,545
quantile(medhhinc_nyc$medhhinc, probs = 0.8) # 97,847

# race and income data
# 20th percentile would be closest to anything below 44,999 for NYC
# 80th percentile would be closest to anything above 99,999 for NYC
ct_race_income_nyc <- read_csv(paste0(here("data", "census", "nhgis0031_ds250_20205_tract.csv"))) %>% 
  filter(STATEA == "36",
         COUNTYA %in% c("005", "047", "061","081", "085")) %>% 
  rename_with(~ str_replace(., "AM8XE", "est_white"), contains("AM8XE")) %>%
  rename_with(~ str_replace(., "AM8YE", "est_black"), contains("AM8YE")) %>% 
  rename_with(~ str_replace(., "AM8ZE", "est_native"), contains("AM8ZE")) %>%
  rename_with(~ str_replace(., "AM80E", "est_asian"), contains("AM80E")) %>% 
  rename_with(~ str_replace(., "AM81E", "est_pi"), contains("AM81E")) %>%
  rename_with(~ str_replace(., "AM82E", "est_other"), contains("AM82E")) %>% 
  rename_with(~ str_replace(., "AM83E", "est_gt2"), contains("AM83E")) %>%
  rename_with(~ str_replace(., "AM84E", "est_nhwhite"), contains("AM84E")) %>% 
  dplyr::select(GEOID, STATEA, COUNTYA, starts_with("est"), -starts_with("AM8")) %>% 
  rowwise() %>% 
  mutate(est_hwhite001 = est_white001 - est_nhwhite001, # Create a hispanic-white category
         est_hwhite002 = est_white002 - est_nhwhite002,
         est_hwhite003 = est_white003 - est_nhwhite003,
         est_hwhite004 = est_white004 - est_nhwhite004,
         est_hwhite005 = est_white005 - est_nhwhite005,
         est_hwhite006 = est_white006 - est_nhwhite006,
         est_hwhite007 = est_white007 - est_nhwhite007,
         est_hwhite008 = est_white008 - est_nhwhite008,
         est_hwhite009 = est_white009 - est_nhwhite009,
         est_hwhite010 = est_white010 - est_nhwhite010,
         est_hwhite011 = est_white011 - est_nhwhite011,
         est_hwhite012 = est_white012 - est_nhwhite012,
         est_hwhite013 = est_white013 - est_nhwhite013,
         est_hwhite014 = est_white014 - est_nhwhite014,
         est_hwhite015 = est_white015 - est_nhwhite015,
         est_hwhite016 = est_white016 - est_nhwhite016,
         est_hwhite017 = est_white017 - est_nhwhite017) %>%
  mutate(tot_hh = est_white001 + est_black001 + est_native001 + est_asian001 + est_pi001 + est_other001 + est_gt2001,
         white_inc_gt_80_pctl = est_nhwhite014 + est_nhwhite015 + est_nhwhite016 + est_nhwhite017,
         non_white_inc_lt_20_pctl = (est_black002 + est_black003 + est_black004 + est_black005 + est_black006 + est_black007 + est_black008 + est_black009) +
           (est_native002 + est_native003 + est_native004 + est_native005 + est_native006 + est_native007 + est_native008 + est_native009) +
           (est_asian002 + est_asian003 + est_asian004 + est_asian005 + est_asian006 + est_asian007 + est_asian008 + est_asian009) +
           (est_pi002 + est_pi003 + est_pi004 + est_pi005 + est_pi006 + est_pi007 + est_pi008 + est_pi009) +
           (est_other002 + est_other003 + est_other004 + est_other005 + est_other006 + est_other007 + est_other008 + est_other009) +
           (est_gt2002 + est_gt2003 + est_gt2004 + est_gt2005 + est_gt2006 + est_gt2007 + est_gt2008 + est_gt2009) +
           (est_hwhite002 + est_hwhite003 + est_hwhite004 + est_hwhite005 + est_hwhite006 + est_hwhite007 + est_hwhite008 + est_hwhite009)
  ) 

nrow(ct_race_income_nyc) #2,327 CTs in NYC 
sum(ct_race_income_nyc$tot_hh) # 3,191,691 households in NYC

ct_race_income_nyc <- ct_race_income_nyc %>% 
  filter(tot_hh > 30) %>% # Uses the same filter that is later applied (see file: "4a_apply_filters.R")
  ungroup() 

nrow(ct_race_income_nyc) #2,218 CTs in NYC are included 
sum(ct_race_income_nyc$tot_hh) # 3,191,608 households in NYC that are included 

# generate ice metric
ct_race_income_nyc <- ct_race_income_nyc %>% 
  janitor::clean_names() %>% 
  dplyr::select(geoid, tot_hh, white_inc_gt_80_pctl, non_white_inc_lt_20_pctl) %>% 
  mutate(ice = (white_inc_gt_80_pctl - non_white_inc_lt_20_pctl)/(tot_hh)) 

# Create cutoffs for quartiles 

quant <- quantile(ct_race_income_nyc$ice, c(.25, .5, .75, 1))

# Apply the cutoffs 

ice_intermediate <- ct_race_income_nyc %>%
  mutate(ice_quartile = case_when(ice < quant[1] ~ "Q1", # Least privileged 
                                  ice < quant[2] & ice >= quant[1] ~ "Q2",
                                  ice < quant[3] & ice >= quant[2] ~ "Q3",
                                  ice >= quant[3] ~ "Q4")) # most privileged

# check that quartiles are properly computed 
# 
# ice_final %>% group_by(ice_quartile) %>% summarize(count = n())

# Clean the geoid column

ice_final <- ice_intermediate %>% 
  mutate(geoid = as.character(geoid)) %>% 
  mutate(geoid = str_sub(geoid, 8)) %>% 
  mutate(geoid = as.double(geoid))

# Save the file 

write_csv(ice_final, paste0(here("data", "ice", "race_and_income", "final", "ct_race_income_nyc.csv")))

# Also save the intermediate file of income for later analyses 

medhhinc_nyc <- medhhinc_nyc  %>% 
  janitor::clean_names() %>% 
  rename(geoid = gisjoin) %>%
  mutate(geoid = as.character(geoid)) %>% 
  mutate(geoid = str_sub(geoid, 2)) %>% 
  mutate(geoid = as.double(geoid)) %>% 
  select(geoid, medhhinc)

write_csv(medhhinc_nyc, paste0(here("data", "ice", "income_alone", "ct_income_nyc_only.csv")))




write_csv(ct_race_income_nyc, paste0(here("data", "ice", "race_and_income", "final", "ct_race_income_nyc.csv")))