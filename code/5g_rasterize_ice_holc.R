####***********************
#### Code Description ####
# Author: Alex
# Date: 8/19/2023
# Last Update: 2/25/24
# Goal: Rasterize ICE and HOLC data 
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
# Load Packages 

sf_use_s2(FALSE)

# Load ICE - CT 

ice <- read_csv(paste0(here("data", "ice", "race_and_income", "final", "ct_race_income_nyc.csv"))) # 2217

nyc_ice <- st_read(here("shapes", "nhgis_cts_2020")) %>% 
  janitor::clean_names() %>% 
  filter(countyfp %in% c("005", "047", "061","081", "085")) %>% 
  filter(statefp == "36") %>% 
  mutate(geoid = as.double(geoid)) %>% 
  st_transform(., crs = 3748) %>% 
  dplyr::select(geoid,geometry) %>% 
  left_join(ice, by = "geoid") %>% 
  filter(!is.na(ice)) 

land_captured <- st_read(here("shapes", "nhgis_cts_2020")) %>% 
  janitor::clean_names() %>% 
  filter(countyfp %in% c("005", "047", "061","081", "085")) %>% 
  filter(statefp == "36") %>% 
  mutate(geoid = as.double(geoid)) %>% 
  st_transform(., crs = 3748) %>% 
  dplyr::select(geoid,geometry)

sum(st_area(land_captured$geometry)) / sum(st_area(nyc_ice$geometry))

# Load HOLC Grade/Boundaries 

redlining_grades <- read_sf(here("shapes","holc_aggregated")) %>% 
  filter(source == "NYBronx1938" | 
           source == "NYStatenIsland1940" | 
           source == "NYQueens1938" | 
           source == "NYManhattan1937" | 
           source == "NYBrooklyn1938") %>% 
  dplyr::select(unique_id,holc_grade,geometry) %>% 
  mutate(holc_number = case_when(holc_grade == "A" ~ 1,
                                 holc_grade == "B" ~ 2,
                                 holc_grade == "C" ~ 3,
                                 holc_grade == "D" ~ 4)) %>% 
  st_transform(., crs = 3748) 

# Step 2: Rasterize and create plot-------------------------------------------------------------------------

# Rasterize and plot 

blank <- raster(nyc_ice, res = 100)
r <- fasterize(nyc_ice, blank, field = "ice", fun="last") # Edit for sensitivity analysis 

# Check ICE 

plot(r)

r2 <- fasterize(redlining_grades, blank, field = "holc_number", fun="last") # Edit for sensitivity analysis 


# Check HOLC
plot(r2)

# Combine/stack the plots 

nyc <- stack(r, r2) 

nyc_polygon <- rasterToPolygons(nyc, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=FALSE)

ice_vector <- nyc_polygon$layer.1
holc_vector <- nyc_polygon$layer.2

# Get quartiles of ICE metrics 

ice_quant <- as.data.frame(ice_vector) %>% 
  filter(!is.na(ice_vector)) %>% 
  mutate(ice_vector = ice_vector)
         
quant <- quantile(ice_quant$ice_vector, c(.25, .5, .75, 1))

# Make the final data frame

nyc_df <- data.frame(ice_vector, holc_vector) %>% 
  filter(!is.na(ice_vector)) %>% 
  mutate(holc_grade = case_when(holc_vector == 1 ~ "A",
                                 holc_vector == 2 ~ "B",
                                 holc_vector == 3 ~ "C",
                                 holc_vector == 4 ~ "D",
                                 is.na(holc_vector) == TRUE ~ "None")) %>% 
  mutate(ice = ice_vector) %>% 
  dplyr::select(ice, holc_grade) %>% 
  mutate(ice_quantile = case_when(ice < quant[1] ~ "Quartile 1 (Least Privileged)",
                                  ice < quant[2] & ice >= quant[1] ~ "Quartile 2",
                                  ice < quant[3] & ice >= quant[2] ~ "Quartile 3",
                                  ice >= quant[3] ~ "Quartile 4 (Most Privileged)")) %>% 
  mutate(combined = paste0(holc_grade, "-",ice_quantile))

write_csv(nyc_df, here("data","raster","nyc_raster_data.csv"))

# Make long 

sankey_df <- nyc_df %>% 
  filter(holc_grade != "None") %>% 
  make_long(holc_grade, ice_quantile)

sankey_df$node <- factor(sankey_df$node,levels = c("D","C","B","A",
                                                   "Quartile 1 (Least Privileged)",
                                                   "Quartile 2",
                                                   "Quartile 3",
                                                   "Quartile 4 (Most Privileged)"))

sankey_df$next_node <- factor(sankey_df$next_node,levels = c("Quartile 1 (Least Privileged)",
                                                             "Quartile 2",
                                                             "Quartile 3",
                                                             "Quartile 4 (Most Privileged)"))

# Make the final plot 

ggplot(sankey_df, aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               fill = factor(node),
               label = node)) +
  geom_sankey(flow.alpha = .4) +
  scale_fill_manual(values = c("#a95861","#c6bd63","#53818c","#537f4d",
                                        "#a95861","#c6bd63","#53818c","#537f4d")) + 
  theme_sankey(base_size = 16) + 
  geom_sankey_label(size = 4,
                    angle = 90,
                    color = "white") + 
  theme_classic() +
  ggtitle("HOLC to ICE Quartiles") + 
  theme(panel.background = element_rect(fill = NA), 
   #     panel.border = element_rect(fill = NA),
   #     axis.text.x = element_blank(),
        axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),   
        axis.title.x = element_blank(),   
        legend.position = "none"
  ) +
  labs(fill = "") 

# Step 3: Save output-------------------------------------------------------------------------

ggsave(here("figures","nyc_raster_sankey.png"), width = 10, height = 10, units = "in")
