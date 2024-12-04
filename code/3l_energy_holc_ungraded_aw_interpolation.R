####***********************
#### Code Description ####
# Author: Alex Northrop
# Date: 2/23/24
# Updated: 2/23/24
# Goal: Energy use data - Areally interpolate from zip code to HOLC boundaries
# Source = zip energy use
# Target = HOLC Boundaries
####**********************

# Load packages (read_libraries)
# Load specific package for this process 
# Documentation: https://cran.r-project.org/web/packages/areal/vignettes/areal.html

rm(list=ls())
library(tigris)
source(paste0(here("code","0_setup", "0_read_libraries.R")))


# Step 1: Load & clean data required-------------------------------------------------------------------------

# Zip code to ZCTA crosswalk (from HUD USPS)

zip_xwalk <- read_xlsx(here("data", "census", "ZIPCodetoZCTACrosswalk2019.xlsx")) %>% 
  janitor::clean_names() %>% 
  select(zip_code, zcta) %>% 
  mutate(zip_code = as.double(zip_code))

#energy use data #
#--- note - use the imputed data 

zip_energy_use <- read_csv(here("data", "energy", "zip_code_imputed_all_energy_use.csv")) %>% 
  janitor::clean_names() %>% 
  filter(year != 2016,
         data_class == "electricity",
         value != -999,
         str_detect(data_field_display_name, "Consumption"),
         str_detect(data_field_display_name, "Residential"),
         utility_display_name == "Consolidated Edison")
length(unique(zip_energy_use$zip_code)) #209 zips

years <- c(2017:2019)
zip_energy_use <- data.frame()
for (year in years){
  
  print(year)
  
  yrly_zip_energy_use <- read_csv(here("data", "energy", "zip_code_imputed_all_energy_use.csv")) %>% 
    janitor::clean_names() %>% 
    filter(year != 2016,
           data_class == "electricity",
           value != -999,
           str_detect(data_field_display_name, "Consumption"),
           str_detect(data_field_display_name, "Residential"),
           utility_display_name == "Consolidated Edison")
  
  zip_energy_use <- rbind(zip_energy_use, yrly_zip_energy_use) %>% 
    filter(number_of_accounts > 1)
  
}

# Replace the zip_code of the energy use to the corresponding ZCTA 

zip_energy_use <- left_join(zip_energy_use, zip_xwalk, by="zip_code") %>% 
  select(-zip_code) %>% 
  mutate(zcta = as.character(zcta))

# energy use metrics for residential use (seasonal)

zip_energy_use_residential_seasonal <- zip_energy_use %>% 
  filter(str_detect(data_field_display_name, "Residential")) %>% 
  mutate(season = case_when(month %in% c(10,11,12,1,2,3,4) ~ "cool",
                            month %in% c(5,6,7,8,9) ~ "warm")) %>%   
  group_by(zcta, season) %>% 
  summarize(value = mean(value),
            number_of_accounts = mean(number_of_accounts)) %>% 
  ungroup()

# energy use metrics for residential use (yearly)

zip_energy_use_residential <- zip_energy_use %>% 
  filter(str_detect(data_field_display_name, "Residential")) %>% 
  group_by(zcta) %>% 
  summarize(value = mean(value),
            number_of_accounts = mean(number_of_accounts)) %>% 
  ungroup()

# NYC Energy Use Zip Shape File #

zcta_shapefile <- zctas(state = "New York", year = 2010) %>% 
  janitor::clean_names() %>% 
  dplyr::select(zcta5ce10, geometry) %>%
  st_transform(., crs = 3748) 

nyc_zctas <- read_csv(here("data", "census", "modzcta.csv")) %>% 
  janitor::clean_names() %>%
  select(zcta)

# Split for unique nyc zctas 

# Split the vector

nyc_zctas <- str_split(nyc_zctas$zcta, ",", simplify = TRUE) 

# Replace all spaces

nyc_zctas <- str_replace_all(nyc_zctas, fixed(" "), "")

# Remove the empty strings

nyc_zctas <- nyc_zctas %>% unique() %>% stri_remove_empty()

length(nyc_zctas) # 215


# These are the NYC ZCTAs 

zcta_shapefile <- zcta_shapefile %>% 
  filter(zcta5ce10 %in% nyc_zctas) %>% 
  rename(zcta = zcta5ce10)

# now these are what zip look like

ggplot(zcta_shapefile) + 
  geom_sf() +
  theme_map() +
  ggtitle("NYC ZCTA boundaries")

# Load the HOLC Maps 

nyc_holc_maps <- st_read(here("shapes", "holc_ungraded")) %>% 
  janitor::clean_names() %>% 
  st_transform(., crs = 3748) %>% mutate(unique_id = "U")

# Step 2: Attach ZCTA shapefile to data-------------------------------------------------------------------------

zip_energy_use_residential <- zip_energy_use_residential %>% 
  left_join(zcta_shapefile, by ="zcta") %>%
  st_as_sf()

zip_energy_use_residential_cool <- zip_energy_use_residential_seasonal %>% 
  filter(season == "cool") %>%
  left_join(zcta_shapefile, by ="zcta") %>%
  st_as_sf()

zip_energy_use_residential_warm <- zip_energy_use_residential_seasonal %>% 
  filter(season == "warm") %>%
  left_join(zcta_shapefile, by ="zcta") %>%
  st_as_sf()

# Step 3: Perform areal interpolation-------------------------------------------------------------------------

# All Season 

value_all <- areal::aw_interpolate(nyc_holc_maps,
                                   tid = unique_id, 
                                   source = zip_energy_use_residential,
                                   sid = zcta,
                                   output = "tibble",
                                   weight = "sum",
                                   extensive = "value")

account_all <- areal::aw_interpolate(nyc_holc_maps,
                                     tid = unique_id, 
                                     source = zip_energy_use_residential,
                                     sid = zcta,
                                     output = "tibble",
                                     weight = "sum",
                                     extensive = "number_of_accounts")

# Cool Season


value_cool <- areal::aw_interpolate(nyc_holc_maps,
                                    tid = unique_id, 
                                    source = zip_energy_use_residential_cool,
                                    sid = zcta,
                                    output = "tibble",
                                    weight = "sum",
                                    extensive = "value")

account_cool <- areal::aw_interpolate(nyc_holc_maps,
                                      tid = unique_id, 
                                      source = zip_energy_use_residential_cool,
                                      sid = zcta,
                                      output = "tibble",
                                      weight = "sum",
                                      extensive = "number_of_accounts")
# Warm Season 

value_warm <- areal::aw_interpolate(nyc_holc_maps,
                                    tid = unique_id, 
                                    source = zip_energy_use_residential_warm,
                                    sid = zcta,
                                    output = "tibble",
                                    weight = "sum",
                                    extensive = "value")


account_warm <- areal::aw_interpolate(nyc_holc_maps,
                                      tid = unique_id, 
                                      source = zip_energy_use_residential_warm,
                                      sid = zcta,
                                      output = "tibble",
                                      weight = "sum",
                                      extensive = "number_of_accounts")

# Step 4: Unify in a df-------------------------------------------------------------------------

energy_use_holc <- value_all %>% 
  left_join(account_all, by = c("unique_id","grade")) %>% 
  mutate(energy_per_account = value/number_of_accounts) 

energy_use_holc_cool <- value_cool %>% 
  left_join(account_cool, by = c("unique_id","grade")) %>% 
  mutate(energy_per_account = value/number_of_accounts) 

energy_use_holc_warm <- value_warm %>% 
  left_join(account_warm, by = c("unique_id","grade")) %>% 
  mutate(energy_per_account = value/number_of_accounts) 

# Step 5: Report data-------------------------------------------------------------------------

print(energy_use_holc$energy_per_account * 3600)