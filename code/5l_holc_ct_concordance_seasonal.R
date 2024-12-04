####***********************
#### Code Description ####
# Author: Alex
# Date: 2/23/24
# Last Update: 4/3/24
# Goal: Seasonal CT Analyses - Concordance
####**********************

# Load packages 
rm(list=ls())
library(here)
source(paste0(here("code","0_setup", "0_read_libraries.R")))

# Step 1: Load data required-------------------------------------------------------------------------

### Load CT Data 

# ICE Data 

ice_final <- read_csv(paste0(here("data", "ice", "race_and_income", "final", "ct_race_income_nyc.csv"))) # 2217
cc_final <- read_csv(paste0(here("data", "ice", "concordance", "concordance_dta.csv"))) # 2217

## 311 

ct_311_cool <- read_csv(here("data","311","final","ct_311_cool_final.csv")) %>%
  mutate(outage_rate = ifelse(outage_rate == 0, 0.0001, outage_rate)) 


ct_311_warm <- read_csv(here("data","311","final","ct_311_warm_final.csv")) %>%
  mutate(outage_rate = ifelse(outage_rate == 0, 0.0001, outage_rate)) 

ct_311_aggregated <- rbind(ct_311_cool,ct_311_warm) %>%
  mutate(outage_rate = ifelse(outage_rate == 0, 0.0001, outage_rate))

## Energy

ct_energy_cool <- read_csv(here("data","energy","final","ct_energy_cool_final.csv")) %>% 
  mutate(season = "cool") %>%
  mutate(energy_per_account = ifelse(energy_per_account == 0, 0.0001, energy_per_account))

ct_energy_warm <- read_csv(here("data","energy","final","ct_energy_warm_final.csv")) %>% 
  mutate(season = "warm") %>%
  mutate(energy_per_account = ifelse(energy_per_account == 0, 0.0001, energy_per_account))

ct_energy_aggregated <- rbind(ct_energy_cool,ct_energy_warm) %>% 
  mutate(energy_per_account = ifelse(energy_per_account == 0, 0.0001, energy_per_account))

## SAIFI

ct_saifi_cool <- read_csv(here("data","nyspo","final","ct_saifi_cool_final.csv")) %>% 
  mutate(season = "cool") %>%
  mutate(saifi = ifelse(saifi == 0, 0.0001, saifi))

ct_saifi_warm <- read_csv(here("data","nyspo","final","ct_saifi_warm_final.csv")) %>% 
  mutate(season = "warm") %>%
  mutate(saifi = ifelse(saifi == 0, 0.0001, saifi))

ct_saifi_aggregated <- rbind(ct_saifi_cool,ct_saifi_warm) %>%
  mutate(saifi = ifelse(saifi == 0, 0.0001, saifi))

color_pal <- c("#eb8055b2", "#13306db2")


# Step 2: 311 Analyses (General)-------------------------------------------------------------------------

# Add ice quartile

ct_311_aggregated <- ct_311_aggregated %>% # 2,216
  left_join(ice_final, by = "geoid") %>% 
  left_join(cc_final, by = "geoid") %>% 
  filter(!is.na(tot_hh)) %>% 
  filter(!is.na(cc)) %>% 
  filter(cc != "High-Low") %>%
  filter(cc != "Low-High")

# Check distribution (not normal)

qqnorm(ct_311_aggregated$outage_rate, pch = 1, frame = FALSE) 

qqline(ct_311_aggregated$outage_rate, col = "steelblue", lwd = 2)

# Get summary statistics

ct_mean <- ct_311_aggregated %>% 
  group_by(cc, season) %>% 
  summarise(outage_rate = mean(outage_rate))

# Show distribution (and save for future plot)

a <- ggplot(ct_311_aggregated, aes(x=cc, y=outage_rate, fill=cc)) + 
  geom_violin(draw_quantiles = c(0.5)) + 
  scale_fill_manual(values=color_pal, guide = "none") + 
  theme_classic() +
  scale_y_log10() + 
  scale_x_discrete(limits=rev) + 
  xlab(label = "HOLC-ICE Pairs") + 
  ylab(label = "311 Outage Calls per 1000 Household-Years") + 
  ggtitle("A.") +
  theme(plot.title = element_text(face="bold", size = 15)) + 
  coord_flip() + 
  facet_wrap(~season, nrow = 2, labeller = labeller(season = c(`cool` = "Cool Season", "warm" = "Warm Season")))

a

# Summary data for chart 

round(summary(ct_311_aggregated$outage_rate),2)

# Add for future table 

call_ct <- as.vector(c(round(summary(ct_311_aggregated$outage_rate),2)))

# Outages by quartile 

ct_311_aggregated %>% 
  st_drop_geometry() %>% 
  group_by(cc, season) %>% 
  summarize(outages = sum(outages)) %>% 
  mutate()

# Summary of outage-rate by quartile 

ct_311_aggregated_cool <- ct_311_aggregated %>% 
  filter(season == "cool")

ct_311_aggregated_warm <- ct_311_aggregated %>% 
  filter(season == "warm")

# 

calls <- do.call(rbind , by(ct_311_aggregated$outage_rate, ct_311_aggregated$cc ,summary)) 

round(calls, 2)

# Cool weather 

calls <- do.call(rbind , by(ct_311_aggregated_cool$outage_rate, ct_311_aggregated_cool$cc ,summary)) 

call_ct_ice_cool <- round(calls, 2)

write.table(call_ct_ice_cool, file = here("tables","call_ct_ice_cool_holc.txt"), sep = ",", quote = F, row.names = T)

# Warm weather 

calls <- do.call(rbind , by(ct_311_aggregated_warm$outage_rate, ct_311_aggregated_warm$cc ,summary)) 

call_ct_ice_warm <- round(calls, 2)

write.table(call_ct_ice_warm, file = here("tables","call_ct_ice_warm_holc.txt"), sep = ",", quote = F, row.names = T)

# Statistical tests 

kruskal.test(outage_rate ~ season, data = ct_311_aggregated)

pairwise.wilcox.test(ct_311_aggregated$outage_rate, ct_311_aggregated$season,   p.adjust.method = "BH")


# Step 3: Energy Analyses (General)-------------------------------------------------------------------------

# Add ice quartile

ct_energy_aggregated <- ct_energy_aggregated %>% 
  left_join(ice_final, by = "geoid") %>% 
  left_join(cc_final, by = "geoid") %>%
  filter(!is.na(tot_hh)) %>%
  filter(!is.na(cc)) %>% 
  filter(cc != "High-Low") %>%
  filter(cc != "Low-High")

# Check distribution (close to normal)

qqnorm(ct_energy_aggregated$energy_per_account, pch = 1, frame = FALSE) 

qqline(ct_energy_aggregated$energy_per_account, col = "steelblue", lwd = 2)

# Get summary statistics

ct_mean <- ct_energy_aggregated %>% 
  group_by(cc, season) %>% 
  summarise(energy_per_account = mean(energy_per_account))

ct_mean

# Show distribution (and save for future plot)

b <- ggplot(ct_energy_aggregated, aes(x=cc, y=energy_per_account, fill=cc)) + 
  geom_violin(draw_quantiles = c(0.5)) + 
  scale_fill_manual(values=color_pal, guide = "none") + 
  theme_classic() +
  scale_y_log10() + 
  scale_x_discrete(limits=rev) + 
  xlab(label = "HOLC-ICE Pairs") + 
  ylab(label = "Average Monthly Energy Use per Account (MWH)") + 
  ggtitle("B.") +
  theme(plot.title = element_text(face="bold", size = 15)) +
  coord_flip() +
  facet_wrap(~season, nrow = 2, labeller = labeller(season = c(`cool` = "Cool Season", "warm" = "Warm Season")))


b

# Summary of energy use by quartile and season disaggregated 

ct_energy_aggregated_cool <- ct_energy_aggregated %>% 
  filter(season == "cool")

ct_energy_aggregated_warm <- ct_energy_aggregated %>% 
  filter(season == "warm")

# 

energy <- do.call(rbind , by(ct_energy_aggregated$energy_per_account, ct_energy_aggregated$cc ,summary)) 

round(energy, 2)

# Cool weather 

calls <- do.call(rbind , by(ct_energy_aggregated_cool$energy_per_account, ct_energy_aggregated_cool$cc ,summary)) 

call_ct_ice_cool <- round(calls, 2)

write.table(call_ct_ice_cool, file = here("tables","energy_ct_ice_cool_holc.txt"), sep = ",", quote = F, row.names = T)

# Warm weather 

calls <- do.call(rbind , by(ct_energy_aggregated_warm$energy_per_account, ct_energy_aggregated_warm$cc ,summary)) 

call_ct_ice_warm <- round(calls, 2)

write.table(call_ct_ice_warm, file = here("tables","energy_ct_ice_warm_holc.txt"), sep = ",", quote = F, row.names = T)

# Statistical tests 

kruskal.test(energy_per_account ~ season, data = ct_energy_aggregated)

pairwise.wilcox.test(ct_energy_aggregated$energy_per_account, ct_energy_aggregated$cc,   p.adjust.method = "BH")

# Step 4: SAIFI Analyses (General)-------------------------------------------------------------------------

# Add ice quartile

ct_saifi_aggregated <- ct_saifi_aggregated %>% 
  left_join(ice_final, by = "geoid") %>% 
  left_join(cc_final, by = "geoid") %>%
  filter(!is.na(tot_hh)) %>%
  filter(!is.na(cc)) %>% 
  filter(cc != "High-Low") %>%
  filter(cc != "Low-High")

# Check distribution (not normal)

qqnorm(ct_saifi_aggregated$saifi, pch = 1, frame = FALSE) 

qqline(ct_saifi_aggregated$saifi, col = "steelblue", lwd = 2)

# Get summary statistics

ct_mean <- ct_saifi_aggregated %>% 
  group_by(cc, season) %>% 
  summarise(saifi = mean(saifi))

ct_mean

# Show distribution (and save for future plot)

c <- ggplot(ct_saifi_aggregated, aes(x=cc, y=saifi, fill=cc)) + 
  geom_violin(draw_quantiles = c(0.5)) + 
  scale_fill_manual(values=color_pal, guide = "none") + 
  theme_classic() +
  scale_y_log10() + 
  scale_x_discrete(limits=rev) + 
  xlab(label = "HOLC-ICE Pairs") + 
  ylab(label = "Average SAIFI (2017-2019)") + 
  ggtitle("C.") +
  theme(plot.title = element_text(face="bold", size = 15)) + 
  coord_flip() + 
  facet_wrap(~season, nrow = 2, labeller = labeller(season = c(`cool` = "Cool Season", "warm" = "Warm Season")))


c

# Summary of SAIFI by quartile  and season disaggregated

ct_saifi_aggregated_cool <- ct_saifi_aggregated %>% 
  filter(season == "cool")

ct_saifi_aggregated_warm <- ct_saifi_aggregated %>% 
  filter(season == "warm")

# 

saifi <- do.call(rbind , by(ct_saifi_aggregated$saifi, ct_saifi_aggregated$season ,summary)) 

round(saifi, 2)

# Cool weather 

calls <- do.call(rbind , by(ct_saifi_aggregated_cool$saifi, ct_saifi_aggregated_cool$cc ,summary)) 

call_ct_ice_cool <- round(calls, 2)

write.table(call_ct_ice_cool, file = here("tables","saifi_ct_ice_cool_holc.txt"), sep = ",", quote = F, row.names = T)

# Warm weather 

calls <- do.call(rbind , by(ct_saifi_aggregated_warm$saifi, ct_saifi_aggregated_warm$cc ,summary)) 

call_ct_ice_warm <- round(calls, 2)

write.table(call_ct_ice_warm, file = here("tables","saifi_ct_ice_warm_holc.txt"), sep = ",", quote = F, row.names = T)

# Statistical tests 

kruskal.test(saifi ~ cc, data = ct_saifi_aggregated)

pairwise.wilcox.test(ct_saifi_aggregated$saifi, ct_saifi_aggregated$cc,   p.adjust.method = "BH")

# Step 5: Combine Plots-------------------------------------------------------------------------

# First, plot the data and save 

a / b / c

ggsave(filename = here("figures", "ct_seasonal_boxplot_holc.png"), plot = a / b / c, width = 10, height = 20, units = "in", dpi = 300)



#### TEST 

g_ridges <- 
  ggplot(ct_311_aggregated, aes(outage_rate, fct_rev(cc), color = cc, fill = cc)) + 
  scale_color_manual(values = color_pal, guide = "none") +
  scale_fill_manual(values = color_pal, guide = "none") +
  ggridges::stat_density_ridges(bandwidth = .1,
    quantile_lines = TRUE, quantiles = 2, 
    color = "black", alpha = .) +
  scale_x_log10(limits = c(0.0001, max(ct_311_aggregated$outage_rate))) +
  theme_classic() + 
  facet_wrap(~season, nrow = 2, labeller = labeller(season = c(`cool` = "Cool Season", "warm" = "Warm Season")))




ggplot(ct_311_aggregated, aes(cc, outage_rate)) +
  scale_color_viridis_d(
    option = "mako", name = "Level:", direction = -1, 
    begin = .15, end = .9
  ) +
  guides(
    color = guide_legend(reverse = TRUE, title.position = "top")
  ) +
  theme(
    legend.position = c(.75, .95), legend.direction = "horizontal",
    legend.title = element_text(face = "bold", size = 22, hjust = .5)
  ) +
  theme_classic() + 
  ggdist::stat_interval(.width = c(.25, .5, .75), size = 12) +
  stat_summary(
    geom = "point", fun = median,
    color = "white", size = 6, shape = 1, stroke = 1.6
  ) + 
  ggdist::stat_halfeye(
    limits = c(0, p_limits = (.75)),
    adjust = .33, ## bandwidth
    width = .7, fill = "grey85",
    interval_colour = NA, point_colour = "black",
    shape = 23, stroke = 1.5, point_size = 5, point_fill = "white",
    position = position_nudge(x=0),
    aes(thickness = stat(f*n))
  ) +
  coord_flip() + 
  xlab(label = "HOLC-ICE Pairs") +
  ylab(label = "311 Outage Calls per 1000 Household-Years") 


ggplot(ct_311_aggregated, aes(x=cc, y=outage_rate, fill=cc)) + 
  geom_violin(draw_quantiles = c(0.5)) + 
  scale_fill_manual(values=color_pal, guide = "none") + 
  theme_classic() +
  scale_y_log10() + 
  scale_x_discrete(limits=rev) + 
  xlab(label = "HOLC-ICE Pairs") + 
  ylab(label = "311 Outage Calls per 1000 Household-Years") + 
  ggtitle("A.") +
  theme(plot.title = element_text(face="bold", size = 15)) + 
  coord_flip() + 
  facet_wrap(~season, nrow = 2, labeller = labeller(season = c(`cool` = "Cool Season", "warm" = "Warm Season")))

a