####***********************
#### Code Description ####
# Author: Alex
# Date: 2/23/24
# Last Update: 2/23/24
# Goal: Seasonal CT Analyses 
####**********************

# Load packages 
rm(list=ls())
library(here)
source(paste0(here("code","0_setup", "0_read_libraries.R")))

# Step 1: Load data required-------------------------------------------------------------------------

### Load CT Data 

# ICE Data 

ice_final <- read_csv(paste0(here("data", "ice", "race_and_income", "final", "ct_race_income_nyc.csv"))) # 2217
income <- read_csv(paste0(here("data", "ice", "income_alone", "ct_income_nyc_only.csv"))) 

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
  mutate(energy_per_account = 3600*energy_per_account) %>% 
  mutate(energy_per_account = ifelse(energy_per_account == 0, 0.0001, energy_per_account))

ct_energy_warm <- read_csv(here("data","energy","final","ct_energy_warm_final.csv")) %>% 
  mutate(season = "warm") %>%
  mutate(energy_per_account = 3600*energy_per_account) %>% 
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

col_pal <- rev(c("#440154FF", "#238A8DFF", "#74D055FF","#FDE725FF"))


# Step 2: 311 Analyses (General)-------------------------------------------------------------------------

# Add ice quartile

ct_311_aggregated <- ct_311_aggregated %>% # 2,216
  left_join(ice_final, by = "geoid") %>% 
  filter(!is.na(tot_hh))

# Check distribution (not normal)

qqnorm(ct_311_aggregated$outage_rate, pch = 1, frame = FALSE) 

qqline(ct_311_aggregated$outage_rate, col = "steelblue", lwd = 2)

# Get summary statistics

ct_mean <- ct_311_aggregated %>% 
  group_by(ice_quartile, season) %>% 
  summarise(outage_rate = mean(outage_rate))

# Show distribution (and save for future plot)

a <- ggplot(ct_311_aggregated, aes(x=ice_quartile, y=outage_rate, fill=ice_quartile)) + 
  geom_boxplot() + 
  scale_fill_manual(values=col_pal, guide = "none") + 
  theme_classic() +
  scale_y_log10() + 
  scale_x_discrete(limits=rev) + 
  xlab(label = "ICE Quartile") + 
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
  group_by(ice_quartile, season) %>% 
  summarize(outages = sum(outages)) %>% 
  mutate()

# Summary of outage-rate by quartile 

ct_311_aggregated_cool <- ct_311_aggregated %>% 
  filter(season == "cool")

ct_311_aggregated_warm <- ct_311_aggregated %>% 
  filter(season == "warm")

# 

calls <- do.call(rbind , by(ct_311_aggregated$outage_rate, ct_311_aggregated$season ,summary)) 

round(calls, 2)

# Cool weather 

calls <- do.call(rbind , by(ct_311_aggregated_cool$outage_rate, ct_311_aggregated_cool$ice_quartile ,summary)) 

call_ct_ice_cool <- round(calls, 2)

write.table(call_ct_ice_cool, file = here("tables","call_ct_ice_cool.txt"), sep = ",", quote = F, row.names = T)

# Warm weather 

calls <- do.call(rbind , by(ct_311_aggregated_warm$outage_rate, ct_311_aggregated_warm$ice_quartile ,summary)) 

call_ct_ice_warm <- round(calls, 2)

write.table(call_ct_ice_warm, file = here("tables","call_ct_ice_warm.txt"), sep = ",", quote = F, row.names = T)

# Statistical tests 

kruskal.test(outage_rate ~ season, data = ct_311_aggregated)

pairwise.wilcox.test(ct_311_aggregated$outage_rate, ct_311_aggregated$season,   p.adjust.method = "BH")


# Step 3: Energy Analyses (General)-------------------------------------------------------------------------

# Add ice quartile

ct_energy_aggregated <- ct_energy_aggregated %>% 
  left_join(ice_final, by = "geoid") %>% 
  filter(!is.na(tot_hh))

# Check distribution (close to normal)

qqnorm(ct_energy_aggregated$energy_per_account, pch = 1, frame = FALSE) 

qqline(ct_energy_aggregated$energy_per_account, col = "steelblue", lwd = 2)

# Get summary statistics

ct_mean <- ct_energy_aggregated %>% 
  group_by(ice_quartile, season) %>% 
  summarise(energy_per_account = mean(energy_per_account))

ct_mean

# Show distribution (and save for future plot)

b <- ggplot(ct_energy_aggregated, aes(x=ice_quartile, y=energy_per_account, fill=ice_quartile)) + 
  geom_boxplot() + 
  scale_fill_manual(values=col_pal, guide = "none") + 
  theme_classic() +
  scale_y_log10() + 
  scale_x_discrete(limits=rev) + 
  xlab(label = "ICE Quartile") + 
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

energy <- do.call(rbind , by(ct_energy_aggregated$energy_per_account, ct_energy_aggregated$season ,summary)) 

round(energy, 2)

# Cool weather 

calls <- do.call(rbind , by(ct_energy_aggregated_cool$energy_per_account, ct_energy_aggregated_cool$ice_quartile ,summary)) 

call_ct_ice_cool <- round(calls, 0)

write.table(call_ct_ice_cool, file = here("tables","energy_ct_ice_cool.txt"), sep = ",", quote = F, row.names = T)

# Warm weather 

calls <- do.call(rbind , by(ct_energy_aggregated_warm$energy_per_account, ct_energy_aggregated_warm$ice_quartile ,summary)) 

call_ct_ice_warm <- round(calls, 0)

write.table(call_ct_ice_warm, file = here("tables","energy_ct_ice_warm.txt"), sep = ",", quote = F, row.names = T)

# Statistical tests 

kruskal.test(energy_per_account ~ season, data = ct_energy_aggregated)

pairwise.wilcox.test(ct_energy_aggregated$energy_per_account, ct_energy_aggregated$season,   p.adjust.method = "BH")
pairwise.wilcox.test(ct_energy_aggregated_cool$energy_per_account, ct_energy_aggregated_cool$ice_quartile,   p.adjust.method = "BH")
pairwise.wilcox.test(ct_energy_aggregated_warm$energy_per_account, ct_energy_aggregated_warm$ice_quartile,   p.adjust.method = "BH")

# Step 4: SAIFI Analyses (General)-------------------------------------------------------------------------

# Add ice quartile

ct_saifi_aggregated <- ct_saifi_aggregated %>% 
  left_join(ice_final, by = "geoid") %>% 
  filter(!is.na(tot_hh))

# Check distribution (not normal)

qqnorm(ct_saifi_aggregated$saifi, pch = 1, frame = FALSE) 

qqline(ct_saifi_aggregated$saifi, col = "steelblue", lwd = 2)

# Get summary statistics

ct_mean <- ct_saifi_aggregated %>% 
  group_by(ice_quartile, season) %>% 
  summarise(saifi = mean(saifi))

ct_mean

# Show distribution (and save for future plot)

c <- ggplot(ct_saifi_aggregated, aes(x=ice_quartile, y=saifi, fill=ice_quartile)) + 
  geom_boxplot() + 
  scale_fill_manual(values=col_pal, guide = "none") + 
  theme_classic() +
  scale_y_log10() + 
  scale_x_discrete(limits=rev) + 
  xlab(label = "ICE Quartile") + 
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

calls <- do.call(rbind , by(ct_saifi_aggregated_cool$saifi, ct_saifi_aggregated_cool$ice_quartile ,summary)) 

call_ct_ice_cool <- round(calls, 2)

write.table(call_ct_ice_cool, file = here("tables","saifi_ct_ice_cool.txt"), sep = ",", quote = F, row.names = T)

# Warm weather 

calls <- do.call(rbind , by(ct_saifi_aggregated_warm$saifi, ct_saifi_aggregated_warm$ice_quartile ,summary)) 

call_ct_ice_warm <- round(calls, 2)

write.table(call_ct_ice_warm, file = here("tables","saifi_ct_ice_warm.txt"), sep = ",", quote = F, row.names = T)

# Statistical tests 
# 
# kruskal.test(saifi ~ season, data = ct_saifi_aggregated)
# 
# pairwise.wilcox.test(ct_saifi_aggregated_warm$saifi, ct_saifi_aggregated_warm$ice_quartile,   p.adjust.method = "BH")
# pairwise.wilcox.test(ct_saifi_aggregated_cool$saifi, ct_saifi_aggregated_cool$ice_quartile,   p.adjust.method = "BH")
# pairwise.wilcox.test(ct_saifi_aggregated$saifi, ct_saifi_aggregated$ice_quartile,   p.adjust.method = "BH")

# Step 5: Combine Plots-------------------------------------------------------------------------

# First, plot the data and save 

a / c / b

ggsave(filename = here("figures", "ct_seasonal_boxplot.png"), plot = a / b / c, width = 10, height = 20, units = "in", dpi = 300)

cbind(data.frame(labs),data.frame(call_ct), data.frame(energy_ct), data.frame(saifi_ct))