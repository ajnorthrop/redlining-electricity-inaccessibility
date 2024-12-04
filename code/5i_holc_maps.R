###########***********************
#### Code Description ####
# Author: Alex
# Date Created: 4/24/23
# Updated: 2/25/24
# Goal: HOLC Bivariate Maps 
####**********************

# Load packages 
rm(list=ls())
library(here)
library(biscale)
library(nngeo)
source(paste0(here("code","0_setup", "0_read_libraries.R")))

# Step 1: Load data required-------------------------------------------------------------------------

## HOLC 

included <- c("NYBronx1938", "NYBrooklyn1938", "NYManhattan1937",
              "NYQueens1938", "NYStatenIsland1940")

holc <- read_sf(here("shapes","holc_aggregated")) %>% 
  filter(holc_grade != "E") %>% dplyr::select(-geometry) %>% 
  filter(source %in% included) %>% 
  dplyr::select(unique_id,holc_grade,geometry, source) %>% 
  st_transform(., crs = 3748) %>% 
  mutate(county = case_when(source == "NYBronx1938" ~ "Bronx",
                            source == "NYBrooklyn1938" ~ "Brooklyn",
                            source == "NYManhattan1937" ~ "Manhattan",
                            source == "NYQueens1938" ~ "Queens",
                            source == "NYStatenIsland1940" ~ "Staten Island"))

# Census tract boundaries for counties 

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

# # Create unique blocks for each NYC borough 
# 
# holc_bronx <- holc %>% filter(county == "Bronx") %>% st_union() %>% nngeo::st_remove_holes() %>% 
#   st_as_sf() %>% mutate(county = "Bronx") %>% rename(geometry = x)
# holc_brooklyn <- holc %>% filter(county == "Brooklyn") %>% st_make_valid() %>% st_union() %>% nngeo::st_remove_holes() %>% 
#   st_as_sf() %>% mutate(county = "Brooklyn") %>% rename(geometry = x)
# holc_manhattan <- holc %>% filter(county == "Manhattan") %>% st_make_valid() %>% st_union() %>% nngeo::st_remove_holes() %>% 
#   st_as_sf() %>% mutate(county = "Manhattan") %>% rename(geometry = x)
# holc_queens <- holc %>% filter(county == "Queens") %>% st_make_valid() %>% st_union() %>% nngeo::st_remove_holes() %>% 
#   st_as_sf() %>% mutate(county = "Queens") %>% rename(geometry = x)
# holc_staten <- holc %>% filter(county == "Staten Island") %>% st_make_valid() %>% st_union() %>% nngeo::st_remove_holes() %>%
#   st_as_sf() %>% mutate(county = "Staten Island") %>% rename(geometry = x)

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

holc_311_aggregated <- read_csv(here("data","311","final","holc_311_final.csv")) %>%
  left_join(holc, by = c("unique_id","holc_grade")) %>% 
  mutate(holc_grade = as.factor(holc_grade))

## Energy

holc_energy_aggregated <- read_csv(here("data","energy","final","holc_energy_final.csv")) %>%
  left_join(holc, by = c("unique_id","holc_grade")) %>% 
  mutate(holc_grade = as.factor(holc_grade))


## SAIFI

holc_saifi_aggregated <- read_csv(here("data","nyspo","final","holc_saifi_final.csv")) %>%
  left_join(holc, by = c("unique_id","holc_grade")) %>% 
  mutate(holc_grade = as.factor(holc_grade))


# Load Colors 

rl <- c(
  "1-1" = "#bdd5ba",
  "2-1" = "#d1e0e4",
  "3-1" = "#f3f1de",
  "4-1" = "#dab7bc",
  "1-2" = "#8eb788", 
  "2-2" = "#9abbc4", 
  "3-2" = "#dad49a", 
  "4-2" = "#bf828a", 
  "1-3" = "#537f4d", 
  "2-3" = "#53818d", 
  "3-3" = "#c6bd63", 
  "4-3" = "#a95862", 
  "1-4" = "#2a4228",
  "2-4" = "#35525a",
  "3-4" = "#6f6929",
  "4-4" = "#6d383e")

holc_colors <- c("#537f4d", "#53818c", "#c6bd63","#a95861")

# Get NYC HOLC boundaries  

# Make data compatible 

# Step 2: Map!-------------------------------------------------------------------------

map_holc <- ggplot(holc) + 
  geom_sf(ct_bronx, mapping = aes(geometry = geometry), alpha = 0.4, lwd =.5, color = "black", fill = "tan") + 
  geom_sf(ct_brooklyn, mapping = aes(geometry = geometry), alpha = 0.4, lwd =.5, color = "black", fill = "tan") +
  geom_sf(ct_manhattan, mapping = aes(geometry = geometry), alpha = 0.4, lwd =.5, color = "black", fill = "tan") +
  geom_sf(ct_queens, mapping = aes(geometry = geometry), alpha = 0.4, lwd =.5, color = "black", fill = "tan") +
  geom_sf(ct_staten, mapping = aes(geometry = geometry), alpha = 0.4, lwd =.5, color = "black", fill = "tan") +
  geom_sf(aes(geometry = geometry, fill = holc_grade), alpha = .8, lwd =.1, color = "grey20", show.legend = T) + 
  scale_fill_manual(values = holc_colors, name = "Holc Grade") +
  geom_label(data = sf_centroids_bklyn, aes(label = county, y = lat, x = lon), nudge_y = -10500, nudge_x = -3000, alpha = .7) + 
  geom_label(data = sf_centroids_man, aes(label = county, y = lat, x = lon), nudge_x = -5600, alpha = .7) + 
  geom_label(data = sf_centroids_queens, aes(label = county, y = lat, x = lon), nudge_x = 5600,nudge_y = 3750, alpha = .7) + 
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

map_holc
ggsave(here("figures","holc_general.png"), map_holc, width = 10, height = 10, units = "in", dpi = 300)

  
data_po <- bi_class(holc_311_aggregated, x = holc_grade, y = outage_rate, style = "quantile", dim = 4)

map_po <- ggplot(data_po) + 
  geom_sf(ct_bronx, mapping = aes(geometry = geometry), alpha = 0.4, lwd =.5, color = "black", fill = "tan") + 
  geom_sf(ct_brooklyn, mapping = aes(geometry = geometry), alpha = 0.4, lwd =.5, color = "black", fill = "tan") +
  geom_sf(ct_manhattan, mapping = aes(geometry = geometry), alpha = 0.4, lwd =.5, color = "black", fill = "tan") +
  geom_sf(ct_queens, mapping = aes(geometry = geometry), alpha = 0.4, lwd =.5, color = "black", fill = "tan") +
  geom_sf(ct_staten, mapping = aes(geometry = geometry), alpha = 0.4, lwd =.5, color = "black", fill = "tan") +
  geom_sf(aes(geometry = geometry, fill = bi_class), alpha = .8, lwd =.1, color = "black",inherit.aes = FALSE, show.legend = F) + 
  bi_scale_fill(pal = rl, dim = 4) +
  bi_theme() + 
  geom_label(data = sf_centroids_bklyn, aes(label = county, y = lat, x = lon), nudge_y = -10500, nudge_x = -3000, alpha = .7) + 
  geom_label(data = sf_centroids_man, aes(label = county, y = lat, x = lon), nudge_x = -5600, alpha = .7) + 
  geom_label(data = sf_centroids_queens, aes(label = county, y = lat, x = lon), nudge_x = 5600,nudge_y = 3750, alpha = .7) + 
  geom_label(data = sf_centroids_bronx, aes(label = county, y = lat, x = lon), nudge_x = 6800, nudge_y = -2900, alpha = .7) + 
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

legend <- bi_legend(pal = rl,
                    dim = 4,
                    xlab = "Lower HOLC Grade ",
                    ylab = "Higher Call Rate ",
                    size = 16)


finalPlot_po <- ggdraw() +
  draw_plot(map_po, 0, 0, 1, 1) # +
  # draw_plot(legend, 0, .4, 0.2, 0.2)

finalPlot_po

ggsave(here("figures","redlining_figure_calls.png"), finalPlot_po, width = 10, height = 10, units = "in", dpi = 300)

ggsave(here("figures","redlining_figure_calls_legend.png"), legend, width = 3, height = 3, units = "in", dpi = 300)


### Energy Use 

data_eu <- bi_class(holc_energy_aggregated, x = holc_grade, y = energy_per_account, style = "quantile", dim = 4)

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
  geom_sf(ct_bronx, mapping = aes(geometry = geometry), alpha = 0.4, lwd =.5, color = "black", fill = "tan") + 
  geom_sf(ct_brooklyn, mapping = aes(geometry = geometry), alpha = 0.4, lwd =.5, color = "black", fill = "tan") +
  geom_sf(ct_manhattan, mapping = aes(geometry = geometry), alpha = 0.4, lwd =.5, color = "black", fill = "tan") +
  geom_sf(ct_queens, mapping = aes(geometry = geometry), alpha = 0.4, lwd =.5, color = "black", fill = "tan") +
  geom_sf(ct_staten, mapping = aes(geometry = geometry), alpha = 0.4, lwd =.5, color = "black", fill = "tan") +
  geom_sf(data = data_eu, aes(geometry = geometry, fill = bi_class), alpha = .8, lwd =.1, color = "black",inherit.aes = FALSE, show.legend = F) + 
  bi_scale_fill(pal = rl, dim = 4) +
  bi_theme() + 
  geom_label(data = sf_centroids_bklyn, aes(label = county, y = lat, x = lon), nudge_y = -10500, nudge_x = -3000, alpha = .7) + 
  geom_label(data = sf_centroids_man, aes(label = county, y = lat, x = lon), nudge_x = -5600, alpha = .7) + 
  geom_label(data = sf_centroids_queens, aes(label = county, y = lat, x = lon), nudge_x = 5600,nudge_y = 3750, alpha = .7) + 
  geom_label(data = sf_centroids_bronx, aes(label = county, y = lat, x = lon), nudge_x = 6800, nudge_y = -2900, alpha = .7) + 
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

legend <- bi_legend(pal = rl,
                    dim = 4,
                    xlab = "Lower HOLC Grade ",
                    ylab = "Lower REU ",
                    size = 16)


finalPlot_eu <- ggdraw() +
  draw_plot(map_eu, 0, 0, 1, 1) # +
  # draw_plot(legend, 0, .4, 0.2, 0.2)

finalPlot_eu

ggsave(here("figures","redlining_figure_energy.png"), finalPlot_eu, width = 10, height = 10, units = "in", dpi = 300)
ggsave(here("figures","redlining_figure_energy_legend.png"), legend, width = 3, height = 3, units = "in", dpi = 300)

### 311 

data_sf_bv <- bi_class(holc_saifi_aggregated, x = holc_grade, y = saifi, style = "quantile", dim = 4)

map_saifi <- ggplot(data_sf_bv) +
  geom_sf(ct_bronx, mapping = aes(geometry = geometry), alpha = 0.4, lwd =.5, color = "black", fill = "tan") + 
  geom_sf(ct_brooklyn, mapping = aes(geometry = geometry), alpha = 0.4, lwd =.5, color = "black", fill = "tan") +
  geom_sf(ct_manhattan, mapping = aes(geometry = geometry), alpha = 0.4, lwd =.5, color = "black", fill = "tan") +
  geom_sf(ct_queens, mapping = aes(geometry = geometry), alpha = 0.4, lwd =.5, color = "black", fill = "tan") +
  geom_sf(ct_staten, mapping = aes(geometry = geometry), alpha = 0.4, lwd =.5, color = "black", fill = "tan") +
  geom_sf(data = data_sf_bv, aes(geometry = geometry, fill = bi_class), alpha = .8, lwd =.1, inherit.aes = FALSE, color = "black", show.legend = F) + 
  bi_scale_fill(pal = rl, dim = 4) +
  bi_theme() + 
  geom_label(data = sf_centroids_bklyn, aes(label = county, y = lat, x = lon), nudge_y = -10500, nudge_x = -3000, alpha = .7) + 
  geom_label(data = sf_centroids_man, aes(label = county, y = lat, x = lon), nudge_x = -5600, alpha = .7) + 
  geom_label(data = sf_centroids_queens, aes(label = county, y = lat, x = lon), nudge_x = 5600,nudge_y = 3750, alpha = .7) + 
  geom_label(data = sf_centroids_bronx, aes(label = county, y = lat, x = lon), nudge_x = 6800, nudge_y = -2900, alpha = .7) + 
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

legend <- bi_legend(pal = rl,
                    dim = 4,
                    xlab = "Lower HOLC Grade ",
                    ylab = "Higher SAIFI ",
                    size = 16)

finalPlot_saifi <- ggdraw() +
  draw_plot(map_saifi, 0, 0, 1, 1) # +
  # draw_plot(legend, 0, .4, 0.2, 0.2)

finalPlot_saifi

ggsave(here("figures","redlining_figure_saifi.png"), finalPlot_saifi, width = 10, height = 10, units = "in", dpi = 300)
ggsave(here("figures","redlining_figure_saifi_legend.png"), legend, width = 3, height = 3, units = "in", dpi = 300)



