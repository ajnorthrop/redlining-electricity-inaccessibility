####***********************
#### Code Description ####
# Author: Alex
# Date: 2/23/24
# Last Update: 2/25/24
# Goal: Seasonal HOLC Analyses 
####**********************

# Load packages 
rm(list=ls())
library(here)
source(paste0(here("code","0_setup", "0_read_libraries.R")))

# Step 1: Load data required-------------------------------------------------------------------------

### Load holc Data 

## 311 

holc_311_cool <- read_csv(here("data","311","final","holc_311_cool_final.csv")) %>%
  mutate(outage_rate = ifelse(outage_rate == 0, 0.0001, outage_rate)) 

holc_311_warm <- read_csv(here("data","311","final","holc_311_warm_final.csv")) %>% 
  mutate(outage_rate = ifelse(outage_rate == 0, 0.0001, outage_rate)) 

holc_311_aggregated <- rbind(holc_311_cool,holc_311_warm) %>% 
  mutate(holc_grade = factor(holc_grade, levels = c("D", "C", "B", "A"))) %>% 
  mutate(outage_rate = ifelse(outage_rate == 0, 0.0001, outage_rate)) 

## Energy

holc_energy_cool <- read_csv(here("data","energy","final","holc_energy_cool_final.csv")) %>% 
  mutate(season = "cool") %>%
  mutate(energy_per_account = 3600 * energy_per_account) %>% # converts from MWH to MJ
  mutate(energy_per_account = ifelse(energy_per_account == 0, 0.0001, energy_per_account))

holc_energy_warm <- read_csv(here("data","energy","final","holc_energy_warm_final.csv")) %>% 
  mutate(season = "warm") %>%
  mutate(energy_per_account = 3600 * energy_per_account) %>% # converts from MWH to MJ
  mutate(energy_per_account = ifelse(energy_per_account == 0, 0.0001, energy_per_account))

holc_energy_aggregated <- rbind(holc_energy_cool,holc_energy_warm) %>% 
  mutate(holc_grade = factor(holc_grade, levels = c("D", "C", "B", "A"))) %>% 
  mutate(energy_per_account = ifelse(energy_per_account == 0, 0.0001, energy_per_account))

## SAIFI

holc_saifi_cool <- read_csv(here("data","nyspo","final","holc_saifi_cool_final.csv")) %>% 
  mutate(season = "cool") %>%
  mutate(saifi = ifelse(saifi == 0, 0.0001, saifi))

holc_saifi_warm <- read_csv(here("data","nyspo","final","holc_saifi_warm_final.csv")) %>% 
  mutate(season = "warm") %>% 
  mutate(saifi = ifelse(saifi == 0, 0.0001, saifi))

holc_saifi_aggregated <- rbind(holc_saifi_cool,holc_saifi_warm) %>% 
  mutate(holc_grade = factor(holc_grade, levels = c("D", "C", "B", "A"))) %>% 
  mutate(saifi = ifelse(saifi == 0, 0.0001, saifi))

# Step 2: 311 Analyses (General)-------------------------------------------------------------------------

# Check distribution (not normal)

qqnorm(holc_311_aggregated$outage_rate, pch = 1, frame = FALSE) 

qqline(holc_311_aggregated$outage_rate, col = "steelblue", lwd = 2)

# Get summary statistics

holc_mean <- holc_311_aggregated %>% 
  group_by(holc_grade, season) %>% 
  summarise(outage_rate = mean(outage_rate))

holc_mean

# Show distribution (and save for future plot)

a <- ggplot(holc_311_aggregated, aes(x=holc_grade, y=outage_rate, fill=holc_grade)) + 
  geom_boxplot() + 
  scale_fill_manual(values=rev(c("#537f4d", "#53818c", "#c6bd63","#a95861")), guide = "none") + 
  theme_classic() +
  scale_y_log10() + 
  scale_x_discrete(limits=rev) + 
  xlab(label = "HOLC Grade") + 
  ylab(label = "311 Outage Calls per 1000 Household-Years") + 
  ggtitle("A.") +
  theme(plot.title = element_text(face="bold", size = 15)) + 
  coord_flip() + 
  facet_wrap(~season, nrow = 2, labeller = labeller(season = c(`cool` = "Cool Season", "warm" = "Warm Season")))

a

# Summary data for chart 

round(summary(holc_311_aggregated$outage_rate),2)

# Add for future table 

call_holc <- as.vector(c(round(summary(holc_311_aggregated$outage_rate),2)))

# Outages by quartile 

holc_311_aggregated %>% 
  st_drop_geometry() %>% 
  group_by(holc_grade, season) %>% 
  summarize(outages = sum(outages)) %>% 
  mutate()

# Summary of outage-rate by quartile 

holc_311_aggregated_cool <- holc_311_aggregated %>% 
  filter(season == "cool")

holc_311_aggregated_warm <- holc_311_aggregated %>% 
  filter(season == "warm")

# 

calls <- do.call(rbind , by(holc_311_aggregated$outage_rate, holc_311_aggregated$season ,summary)) 

round(calls, 2)

# Cool weather 

calls <- do.call(rbind , by(holc_311_aggregated_cool$outage_rate, holc_311_aggregated_cool$holc_grade ,summary)) 

call_holc_szn_cool <- round(calls, 2)

write.table(call_holc_szn_cool, file = here("tables","call_holc_szn_cool.txt"), sep = ",", quote = F, row.names = T)

# Warm weather 

calls <- do.call(rbind , by(holc_311_aggregated_warm$outage_rate, holc_311_aggregated_warm$holc_grade ,summary)) 

call_holc_szn_warm <- round(calls, 2)

write.table(call_holc_szn_warm, file = here("tables","call_holc_szn_warm.txt"), sep = ",", quote = F, row.names = T)

# Seasonal 

do.call(rbind , by(holc_311_aggregated$outage_rate, holc_311_aggregated$season ,summary)) 

# Statistical tests (not statistically significant)

# Warm 

outage_control_data_warm <- holc_311_aggregated_warm %>%
  select(holc_grade, outage_rate) %>%
  filter(holc_grade =="A")

holc_311_aggregated_warm %>%
  filter(holc_grade != "A") %>%
  group_by(holc_grade) %>%
  summarise(p_value = wilcox.test(outage_control_data_warm$outage_rate, outage_rate, exact = FALSE)$p.value)


# Cool 

outage_control_data_cool <- holc_311_aggregated_cool %>%
  select(holc_grade, outage_rate) %>%
  filter(holc_grade =="A")

holc_311_aggregated_cool %>%
  filter(holc_grade != "A") %>%
  group_by(holc_grade) %>%
  summarise(p_value = wilcox.test(outage_control_data_cool$outage_rate, outage_rate, exact = FALSE)$p.value)


# Step 3: Energy Analyses (General)-------------------------------------------------------------------------

# Check distribution (close to normal)

qqnorm(holc_energy_aggregated$energy_per_account, pch = 1, frame = FALSE) 

qqline(holc_energy_aggregated$energy_per_account, col = "steelblue", lwd = 2)

# Get summary statistics

holc_mean <- holc_energy_aggregated %>% 
  group_by(holc_grade, season) %>% 
  summarise(energy_per_account = mean(energy_per_account))

holc_mean

# Show distribution (and save for future plot)

b <- ggplot(holc_energy_aggregated, aes(x=holc_grade, y=energy_per_account, fill=holc_grade)) + 
  geom_boxplot() + 
  scale_fill_manual(values=rev(c("#537f4d", "#53818c", "#c6bd63","#a95861")), guide = "none") + 
  theme_classic() +
  scale_y_log10() + 
  scale_x_discrete(limits=rev) + 
  xlab(label = "HOLC Grade") + 
  ylab(label = "Average Monthly Energy Use per Account (MJ)") + 
  ggtitle("B.") +
  theme(plot.title = element_text(face="bold", size = 15)) +
  coord_flip() +
  facet_wrap(~season, nrow = 2, labeller = labeller(season = c(`cool` = "Cool Season", "warm" = "Warm Season")))


b

# Summary of energy use by quartile and season disaggregated 

holc_energy_aggregated_cool <- holc_energy_aggregated %>% 
  filter(season == "cool")

holc_energy_aggregated_warm <- holc_energy_aggregated %>% 
  filter(season == "warm")

# 

energy <- do.call(rbind , by(holc_energy_aggregated$energy_per_account, holc_energy_aggregated$season ,summary)) 

round(energy, 0)

# Cool weather 

calls <- do.call(rbind , by(holc_energy_aggregated_cool$energy_per_account, holc_energy_aggregated_cool$holc_grade ,summary)) 

call_holc_szn_cool <- round(calls, 0)

write.table(call_holc_szn_cool, file = here("tables","energy_holc_szn_cool.txt"), sep = ",", quote = F, row.names = T)

# Warm weather 

calls <- do.call(rbind , by(holc_energy_aggregated_warm$energy_per_account, holc_energy_aggregated_warm$holc_grade ,summary)) 

call_holc_warm <- round(calls, 0)

write.table(call_holc_warm, file = here("tables","energy_holc_warm.txt"), sep = ",", quote = F, row.names = T)

kruskal.test(energy_per_account ~ holc_grade, data = holc_energy_aggregated_warm)

calls <- do.call(rbind , by(holc_energy_aggregated_warm$energy_per_account, holc_energy_aggregated_warm$holc_grade ,summary)) 

# Statistical tests 

# Seasonal

calls <- do.call(rbind , by(holc_energy_aggregated$energy_per_account, holc_energy_aggregated$season ,summary)) 

call_holc_seasonal <- round(calls, 2)

call_holc_seasonal

kruskal.test(energy_per_account ~ season, data = holc_energy_aggregated)
kruskal.test(energy_per_account ~ holc_grade, data = holc_energy_aggregated_warm)

pairwise.wilcox.test(holc_energy_aggregated$energy_per_account, holc_energy_aggregated$season,   p.adjust.method = "BH")

# Step 4: SAIFI Analyses (General)-------------------------------------------------------------------------

# Check distribution (not normal)

qqnorm(holc_saifi_aggregated$saifi, pch = 1, frame = FALSE) 

qqline(holc_saifi_aggregated$saifi, col = "steelblue", lwd = 2)

# Get summary statistics

holc_mean <- holc_saifi_aggregated %>% 
  group_by(holc_grade, season) %>% 
  summarise(saifi = mean(saifi))

holc_mean

# Show distribution (and save for future plot)

c <- ggplot(holc_saifi_aggregated, aes(x=holc_grade, y=saifi, fill=holc_grade)) + 
  geom_boxplot() + 
  scale_fill_manual(values=rev(c("#537f4d", "#53818c", "#c6bd63","#a95861")), guide = "none") + 
  theme_classic() +
  scale_y_log10() + 
  scale_x_discrete(limits=rev) + 
  xlab(label = "HOLC Grade") + 
  ylab(label = "Average SAIFI (2017-2019)") + 
  ggtitle("C.") +
  theme(plot.title = element_text(face="bold", size = 15)) + 
  coord_flip() + 
  facet_wrap(~season, nrow = 2, labeller = labeller(season = c(`cool` = "Cool Season", "warm" = "Warm Season")))


c

# Summary of SAIFI by quartile  and season disaggregated

holc_saifi_aggregated_cool <- holc_saifi_aggregated %>% 
  filter(season == "cool")

holc_saifi_aggregated_warm <- holc_saifi_aggregated %>% 
  filter(season == "warm")

# 

saifi <- do.call(rbind , by(holc_saifi_aggregated$saifi, holc_saifi_aggregated$season ,summary)) 

round(saifi, 2)

# Cool weather 

calls <- do.call(rbind , by(holc_saifi_aggregated_cool$saifi, holc_saifi_aggregated_cool$holc_grade ,summary)) 

call_holc_szn_cool <- round(calls, 2)

write.table(call_holc_szn_cool, file = here("tables","saifi_holc_szn_cool.txt"), sep = ",", quote = F, row.names = T)

# Warm weather 

calls <- do.call(rbind , by(holc_saifi_aggregated_warm$saifi, holc_saifi_aggregated_warm$holc_grade ,summary)) 

call_holc_szn_warm <- round(calls, 2)

write.table(call_holc_szn_warm, file = here("tables","saifi_holc_szn_warm.txt"), sep = ",", quote = F, row.names = T)

# Statistical tests seasonal 

calls <- do.call(rbind , by(holc_saifi_aggregated$saifi, holc_saifi_aggregated$season ,summary)) 

call_holc_szn <- round(calls, 2)

call_holc_szn

kruskal.test(saifi ~ season, data = holc_saifi_aggregated)

pairwise.wilcox.test(holc_saifi_aggregated$saifi, holc_saifi_aggregated$season,   p.adjust.method = "BH")

# Step 5: Combine Plots-------------------------------------------------------------------------

# First, plot the data and save 

a / c / b

ggsave(filename = here("figures", "holc_seasonal_boxplot.png"), plot = a / b / c, width = 10, height = 20, units = "in", dpi = 300)


