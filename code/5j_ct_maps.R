###########***********************
#### Code Description ####
# Author: Alex
# Date Created: 4/24/23
# Updated: 6/10/24
# Goal: CT Bivariate Maps 
####**********************

# Load packages 
rm(list=ls())
library(here)
library(biscale)
library(nngeo) # to remove holes from SP objects 
source(paste0(here("code","0_setup", "0_read_libraries.R")))

# Step 1: Load data required-------------------------------------------------------------------------

## ct 

ice <- read_csv(paste0(here("data", "ice", "race_and_income", "final", "ct_race_income_nyc.csv"))) # 2217

ct <- st_read(here("shapes", "nhgis_cts_2020")) %>% 
  janitor::clean_names() %>% 
  filter(countyfp %in% c("005", "047", "061","081", "085")) %>% 
  filter(statefp == "36") %>% 
  mutate(geoid = as.double(geoid)) %>% 
  st_transform(., crs = 3748) %>% 
  dplyr::select(geoid,geometry, countyfp) %>% 
  left_join(ice, by = "geoid") %>% 
  filter(!is.na(ice)) 

# Create unique blocks for each NYC borough 

ct_bronx <- ct %>% filter(countyfp == "005") %>% st_union() %>% nngeo::st_remove_holes() %>% 
  st_as_sf() %>% mutate(county = "Bronx") %>% rename(geometry = x)
ct_brooklyn <- ct %>% filter(countyfp == "047") %>% st_make_valid() %>% st_union() %>% nngeo::st_remove_holes() %>% 
  st_as_sf() %>% mutate(county = "Brooklyn") %>% rename(geometry = x)
ct_manhattan <- ct %>% filter(countyfp == "061") %>% st_make_valid() %>% st_union() %>% nngeo::st_remove_holes() %>% 
  st_as_sf() %>% mutate(county = "Manhattan") %>% rename(geometry = x)
ct_queens <- ct %>% filter(countyfp == "081") %>% st_make_valid() %>% st_union() %>% nngeo::st_remove_holes() %>% 
  st_as_sf() %>% mutate(county = "Queens") %>% rename(geometry = x)
ct_staten <- ct %>% filter(countyfp == "085") %>% st_make_valid() %>% st_union() %>% nngeo::st_remove_holes() %>%
  st_as_sf() %>% mutate(county = "Staten Island") %>% rename(geometry = x)

sf_centroids_staten <- ct_staten %>% 
  st_point_on_surface() %>% 
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2])

sf_centroids_queens <- ct_queens %>% 
  st_point_on_surface() %>% 
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2])

sf_centroids_bronx <- ct_bronx %>% 
  st_point_on_surface() %>% 
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2])

sf_centroids_man <- ct_manhattan %>% 
  st_point_on_surface() %>% 
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2])

sf_centroids_bklyn <- ct_brooklyn %>% 
  st_point_on_surface() %>% 
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2])

## 311 

ct_311_aggregated <- read_csv(here("data","311","final","ct_311_final.csv")) %>%
  left_join(ct, by = c("geoid")) %>% 
  mutate(ice_quartile = factor(ice_quartile, levels = c("Q4","Q3","Q2","Q1"))) %>% 
  dplyr::select(geoid,outage_rate,ice_quartile,geometry) %>% 
  na.omit()

## Energy

ct_energy_aggregated <- read_csv(here("data","energy","final","ct_energy_final.csv")) %>%
  left_join(ct, by = c("geoid")) %>% 
  mutate(ice_quartile = factor(ice_quartile, levels = c("Q4","Q3","Q2","Q1"))) %>% 
  dplyr::select(geoid,energy_per_account,ice_quartile,geometry) %>% 
  na.omit()


## SAIFI

ct_saifi_aggregated <- read_csv(here("data","nyspo","final","ct_saifi_final.csv")) %>%
  left_join(ct, by = c("geoid")) %>% 
  mutate(ice_quartile = factor(ice_quartile, levels = c("Q4","Q3","Q2","Q1"))) %>% 
  dplyr::select(geoid,saifi,ice_quartile,geometry) %>% 
  na.omit()


# Load Colors 

rl <- c(
  "1-1" = "#fef9c8",
  "2-1" = "#dcf3d4",
  "3-1" = "#bcedee",
  "4-1" = "#ea96fe",
  "1-2" = "#fef392", 
  "2-2" = "#b9e7aa", 
  "3-2" = "#79dbde", 
  "4-2" = "#d42dfc", 
  "1-3" = "#fded5b", 
  "2-3" = "#97dc7f", 
  "3-3" = "#36c9cd", 
  "4-3" = "#9902bd", 
  "1-4" = "#fde725",
  "2-4" = "#74D055",
  "3-4" = "#238a8d",
  "4-4" = "#440154")

ct_colors <- c("#440154FF", "#238A8DFF", "#74D055FF","#FDE725FF")

# Get NYC ct boundaries  

# Make data compatible 

# Step 2: Map!-------------------------------------------------------------------------
  

## General map

map_ice <- ggplot(ct) + 
  geom_sf(aes(geometry = geometry, fill = ice_quartile), alpha = .8, lwd =.1,color = "grey20", show.legend = T) + 
  scale_fill_manual(values = ct_colors, name = "ICE Quartile") +
  geom_sf(ct_bronx, mapping = aes(geometry = geometry), alpha = 0, lwd =.8, color = "black") + 
  geom_sf(ct_brooklyn, mapping = aes(geometry = geometry), alpha = 0, lwd =.8, color = "black") +
  geom_sf(ct_manhattan, mapping = aes(geometry = geometry), alpha = 0, lwd =.8, color = "black") +
  geom_sf(ct_queens, mapping = aes(geometry = geometry), alpha = 0, lwd =.8, color = "black") +
  geom_sf(ct_staten, mapping = aes(geometry = geometry), alpha = 0, lwd =.8, color = "black") +
  geom_label(data = sf_centroids_bklyn, aes(label = county, y = lat, x = lon), nudge_y = -10500, nudge_x = -3000, alpha = .7) + 
  geom_label(data = sf_centroids_man, aes(label = county, y = lat, x = lon), nudge_x = -5600, alpha = .7) + 
  geom_label(data = sf_centroids_queens, aes(label = county, y = lat, x = lon), nudge_x = 4000,nudge_y = 8000, alpha = .7) + 
  geom_label(data = sf_centroids_bronx, aes(label = county, y = lat, x = lon), nudge_x = 6500, nudge_y = -2500, alpha = .7) + 
  geom_label(data = sf_centroids_staten, aes(label = county, y = lat, x = lon), nudge_x = 0, nudge_y = -7500, alpha = .7) + 
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) + 
  labs(title = "")

map_ice
ggsave(here("figures","ice_general.png"), map_ice, width = 10, height = 10, units = "in", dpi = 300)

## 

data_po <- bi_class(ct_311_aggregated, x = ice_quartile, y = outage_rate, style = "quantile", dim = 4)

map_po <- ggplot(data_po) + 
  geom_sf(aes(geometry = geometry, fill = bi_class), alpha = .8, lwd =.1,color = "grey20", inherit.aes = FALSE, show.legend = F) + 
  bi_scale_fill(pal = rl, dim = 4) +
  bi_theme() + 
  geom_sf(ct_bronx, mapping = aes(geometry = geometry), alpha = 0, lwd =.8, color = "black") + 
  geom_sf(ct_brooklyn, mapping = aes(geometry = geometry), alpha = 0, lwd =.8, color = "black") +
  geom_sf(ct_manhattan, mapping = aes(geometry = geometry), alpha = 0, lwd =.8, color = "black") +
  geom_sf(ct_queens, mapping = aes(geometry = geometry), alpha = 0, lwd =.8, color = "black") +
  geom_sf(ct_staten, mapping = aes(geometry = geometry), alpha = 0, lwd =.8, color = "black") +
  geom_label(data = sf_centroids_bklyn, aes(label = county, y = lat, x = lon), nudge_y = -10500, nudge_x = -3000, alpha = .7) + 
  geom_label(data = sf_centroids_man, aes(label = county, y = lat, x = lon), nudge_x = -5600, alpha = .7) + 
  geom_label(data = sf_centroids_queens, aes(label = county, y = lat, x = lon), nudge_x = 4000,nudge_y = 8000, alpha = .7) + 
  geom_label(data = sf_centroids_bronx, aes(label = county, y = lat, x = lon), nudge_x = 6500, nudge_y = -2500, alpha = .7) + 
  geom_label(data = sf_centroids_staten, aes(label = county, y = lat, x = lon), nudge_x = 0, nudge_y = -7500, alpha = .7) + 
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) + 
  labs(title = "")

legend_311 <- bi_legend(pal = rl,
                    dim = 4,
                    xlab = "Lower ICE Quartile ",
                    ylab = "Higher Call Rate ",
                    size = 16)


finalPlot_po <- ggdraw() +
  draw_plot(map_po, 0, 0, 1, 1) # +
  # draw_plot(legend, 0, .4, 0.2, 0.2)

finalPlot_po

### Energy Use 

data_eu <- bi_class(ct_energy_aggregated, x = ice_quartile, y = energy_per_account, style = "quantile", dim = 4)

# Reverse to keep scale consistent with other measures 

data_eu <- data_eu %>% 
  mutate(bi_class = case_when(bi_class == "1-1" ~ "1-4",
                              bi_class == "2-1" ~ "2-4",
                              bi_class == "3-1" ~ "3-4",
                              bi_class == "4-1" ~ "4-4",
                              bi_class == "1-2" ~ "1-3",
                              bi_class == "2-2" ~ "2-3",
                              bi_class == "3-2" ~ "3-3",
                              bi_class == "4-2" ~ "4-3",
                              bi_class == "1-3" ~ "1-2",
                              bi_class == "2-3" ~ "2-2",
                              bi_class == "3-3" ~ "3-2",
                              bi_class == "4-3" ~ "4-2",
                              bi_class == "1-4" ~ "1-1",
                              bi_class == "2-4" ~ "2-1",
                              bi_class == "3-4" ~ "3-1",
                              bi_class == "4-4" ~ "4-1")) 

map_eu <- ggplot(data_eu) + 
  geom_sf(aes(geometry = geometry, fill = bi_class), alpha = .8, lwd =.1,color = "grey20", inherit.aes = FALSE, show.legend = F) + 
  bi_scale_fill(pal = rl, dim = 4) +
  bi_theme() + 
  geom_sf(ct_bronx, mapping = aes(geometry = geometry), alpha = 0, lwd =.8, color = "black") + 
  geom_sf(ct_brooklyn, mapping = aes(geometry = geometry), alpha = 0, lwd =.8, color = "black") +
  geom_sf(ct_manhattan, mapping = aes(geometry = geometry), alpha = 0, lwd =.8, color = "black") +
  geom_sf(ct_queens, mapping = aes(geometry = geometry), alpha = 0, lwd =.8, color = "black") +
  geom_sf(ct_staten, mapping = aes(geometry = geometry), alpha = 0, lwd =.8, color = "black") +
  geom_label(data = sf_centroids_bklyn, aes(label = county, y = lat, x = lon), nudge_y = -10500, nudge_x = -3000, alpha = .7) + 
  geom_label(data = sf_centroids_man, aes(label = county, y = lat, x = lon), nudge_x = -5600, alpha = .7) + 
  geom_label(data = sf_centroids_queens, aes(label = county, y = lat, x = lon), nudge_x = 4000,nudge_y = 8000, alpha = .7) + 
  geom_label(data = sf_centroids_bronx, aes(label = county, y = lat, x = lon), nudge_x = 6500, nudge_y = -2500, alpha = .7) + 
  geom_label(data = sf_centroids_staten, aes(label = county, y = lat, x = lon), nudge_x = 0, nudge_y = -7500, alpha = .7) + 
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) + 
  labs(title = "")

legend_energy <- bi_legend(pal = rl,
                    dim = 4,
                    xlab = "Lower ICE Quartile ",
                    ylab = "Lower REU  ",
                    size = 16)


finalPlot_eu <- ggdraw() +
   draw_plot(map_eu, 0, 0, 1, 1) # +
  # draw_plot(legend, 0, .4, 0.2, 0.2)

finalPlot_eu

### 311 

data_sf_bv <- bi_class(ct_saifi_aggregated, x = ice_quartile, y = saifi, style = "quantile", dim = 4)

map_saifi <- ggplot(data_sf_bv) +
  geom_sf(aes(geometry = geometry, fill = bi_class), alpha = .8, lwd =.1,color = "grey20", inherit.aes = FALSE, show.legend = F) + 
  bi_scale_fill(pal = rl, dim = 4) +
  bi_theme() + 
  geom_sf(ct_bronx, mapping = aes(geometry = geometry), alpha = 0, lwd =.8, color = "black") + 
  geom_sf(ct_brooklyn, mapping = aes(geometry = geometry), alpha = 0, lwd =.8, color = "black") +
  geom_sf(ct_manhattan, mapping = aes(geometry = geometry), alpha = 0, lwd =.8, color = "black") +
  geom_sf(ct_queens, mapping = aes(geometry = geometry), alpha = 0, lwd =.8, color = "black") +
  geom_sf(ct_staten, mapping = aes(geometry = geometry), alpha = 0, lwd =.8, color = "black") +
  geom_label(data = sf_centroids_bklyn, aes(label = county, y = lat, x = lon), nudge_y = -10500, nudge_x = -3000, alpha = .7) + 
  geom_label(data = sf_centroids_man, aes(label = county, y = lat, x = lon), nudge_x = -5600, alpha = .7) + 
  geom_label(data = sf_centroids_queens, aes(label = county, y = lat, x = lon), nudge_x = 4000,nudge_y = 8000, alpha = .7) + 
  geom_label(data = sf_centroids_bronx, aes(label = county, y = lat, x = lon), nudge_x = 6500, nudge_y = -2500, alpha = .7) + 
  geom_label(data = sf_centroids_staten, aes(label = county, y = lat, x = lon), nudge_x = 0, nudge_y = -7500, alpha = .7) + 
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) + 
  labs(title = "")

legend_saifi <- bi_legend(pal = rl,
                    dim = 4,
                    xlab = "Lower ICE Quartile ",
                    ylab = "Higher SAIFI ",
                    size = 16)

finalPlot_saifi <- ggdraw() +
  draw_plot(map_saifi, 0, 0, 1, 1) # +
  # draw_plot(legend, 0, .4, 0.2, 0.2)

finalPlot_saifi

# Step 3: Save files-------------------------------------------------------------------------

finalPlot_po

ggsave(here("figures","ice_figure_calls.png"), finalPlot_po, width = 10, height = 10, units = "in", dpi = 300)
ggsave(here("figures","ice_figure_calls_legend.png"), legend_311, width = 3, height = 3, units = "in", dpi = 300)


finalPlot_eu

ggsave(here("figures","ice_figure_energy.png"), finalPlot_eu, width = 10, height = 10, units = "in", dpi = 300)
ggsave(here("figures","ice_figure_energy_legend.png"), legend_energy, width = 3, height = 3, units = "in", dpi = 300)


finalPlot_saifi

ggsave(here("figures","ice_figure_saifi.png"), finalPlot_saifi, width = 10, height = 10, units = "in", dpi = 300)
ggsave(here("figures","ice_figure_saifi_legend.png"), legend_saifi, width = 3, height = 3, units = "in", dpi = 300)


