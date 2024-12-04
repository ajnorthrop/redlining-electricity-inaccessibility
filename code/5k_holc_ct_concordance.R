####***********************
#### Code Description ####
# Author: Alex
# Date: 2/23/24
# Last Update: 6/27/24
# Goal: CT Concordance Analyses 
####**********************

# Load packages 
rm(list=ls())
library(here)
source(paste0(here("code","0_setup", "0_read_libraries.R")))

# Step 1: Load data required-------------------------------------------------------------------------

### Load ICE Data

ice_final <- read_csv(paste0(here("data", "ice", "race_and_income", "final", "ct_race_income_nyc.csv"))) # 2217

cc_final <- read_csv(paste0(here("data", "ice", "concordance", "concordance_dta.csv"))) # 2217

### Load CT Data 

## 311 

ct_311_aggregated <- read_csv(here("data","311","final","ct_311_final.csv")) %>% # 2217
  mutate(outage_rate = ifelse(outage_rate == 0, 0.0001, outage_rate)) 

## Energy

ct_energy_aggregated <- read_csv(here("data","energy","final","ct_energy_final.csv")) %>% #2287
  mutate(energy_per_account = 3600 * energy_per_account) %>% # converts from MWH to MJ
  mutate(energy_per_account = ifelse(energy_per_account == 0, 0.0001, energy_per_account))

## SAIFI

ct_saifi_aggregated <- read_csv(here("data","nyspo","final","ct_saifi_final.csv")) %>% # 2313 
  mutate(saifi = ifelse(saifi == 0, 0.0001, saifi))

# Colors for plots 

color_pal <- c("#eb8055b2", "#13306db2")

# Step 2: 311 Analyses (General)-------------------------------------------------------------------------

# Add CC designation

ct_311_aggregated <- ct_311_aggregated %>% # 2,216
  left_join(ice_final, by = "geoid") %>% 
  left_join(cc_final, by = "geoid") %>% 
  filter(!is.na(tot_hh)) %>%
  filter(!is.na(cc)) %>% 
  filter(cc != "High-Low") %>% 
  filter(cc != "Low-High") %>% 
  mutate(cc = ifelse(cc == "High-High", "Consistently Advantaged", "Consistently Disadvantaged"))

write_csv(ct_311_aggregated, here("data","311","final","both_311_final.csv"))
  
# Check distribution (not normal)

qqnorm(ct_311_aggregated$outage_rate, pch = 1, frame = FALSE) 

qqline(ct_311_aggregated$outage_rate, col = "steelblue", lwd = 2)

# Get summary statistics

ct_mean <- ct_311_aggregated %>% 
  group_by(cc) %>% 
  summarise(outage_rate = mean(outage_rate))

# Show distribution (and save for future plot)

a <- ggplot(ct_311_aggregated, aes(x=cc, y=outage_rate, fill=cc)) + 
  geom_boxplot() + 
  scale_fill_manual(values=(color_pal), guide = "none") + 
  theme_classic() +
  scale_y_log10() + 
  scale_x_discrete(limits=rev) + 
  xlab(label = "HOLC-ICE Pairs") + 
  ylab(label = "311 Outage Calls per 1000 Household-Years") + 
  ggtitle("A.") +
  theme(plot.title = element_text(face="bold", size = 15)) + 
  coord_flip()

a

# Summary data for chart 

round(summary(ct_311_aggregated$outage_rate),2)

# Add for future table 

call_ct <- as.vector(c(round(summary(ct_311_aggregated$outage_rate),2)))

# Outages by quartile 

ct_311_aggregated %>% 
  st_drop_geometry() %>% 
  group_by(cc) %>% 
  summarize(outages = sum(outages)) %>% 
  mutate()

# Summary of outage-rate by quartile 

calls <- do.call(rbind , by(ct_311_aggregated$outage_rate, ct_311_aggregated$cc, summary)) 

call_ct_ice <- round(calls, 2)

write.table(call_ct_ice, file = here("tables","call_ct_ice_holc.txt"), sep = ",", quote = F, row.names = T)

# Statistical tests 

outage_control_data <- ct_311_aggregated %>%
  dplyr::select(ice_quartile,outage_rate)%>%
  filter(ice_quartile =="Q4")

ct_311_aggregated %>%
  filter(ice_quartile !="Q4") %>% 
  group_by(ice_quartile) %>%
  summarise(p_value = wilcox.test(outage_control_data$outage_rate, outage_rate, exact = FALSE)$p.value)


# Step 3: Energy Analyses (General)-------------------------------------------------------------------------

# Add ice quartile

ct_energy_aggregated <- ct_energy_aggregated %>% 
  left_join(ice_final, by = "geoid") %>% 
  left_join(cc_final, by = "geoid") %>% 
  filter(!is.na(tot_hh)) %>% 
  filter(!is.na(cc)) %>% 
  filter(cc != "High-Low") %>% 
  filter(cc != "Low-High") %>% 
  mutate(cc = ifelse(cc == "High-High", "Consistently Advantaged", "Consistently Disadvantaged"))

write_csv(ct_energy_aggregated, here("data","energy","final","both_energy_final.csv"))


# Check distribution (close to normal)

qqnorm(ct_energy_aggregated$energy_per_account, pch = 1, frame = FALSE) 

qqline(ct_energy_aggregated$energy_per_account, col = "steelblue", lwd = 2)

# Get summary statistics

ct_mean <- ct_energy_aggregated %>% 
  group_by(cc) %>% 
  summarise(energy_per_account = mean(energy_per_account))

ct_mean

# Show distribution (and save for future plot)

b <- ggplot(ct_energy_aggregated, aes(x=cc, y=energy_per_account, fill=cc)) + 
  geom_boxplot() + 
  scale_fill_manual(values= (color_pal), guide = "none") + 
  theme_classic() +
  scale_y_log10() + 
  scale_x_discrete(limits=rev) + 
  xlab(label = "HOLC-ICE Pairs") + 
  ylab(label = "Average Monthly Energy Use per Account (MWH)") + 
  ggtitle("B.") +
  theme(plot.title = element_text(face="bold", size = 15)) +
  coord_flip()

b

# Summary data for chart 

round(summary(ct_energy_aggregated$energy_per_account),0)

# Add for future table 

energy_ct <- as.vector(c(round(summary(ct_energy_aggregated$energy_per_account),0)))

# Summary of outage-rate by quartile 

energy <- do.call(rbind , by(ct_energy_aggregated$energy_per_account, ct_energy_aggregated$cc, summary)) 

energy_ct_ice <- round(energy, 0)

write.table(energy_ct_ice, file = here("tables","energy_ct_ice_holc.txt"), sep = ",", quote = F, row.names = T)


# Statistical tests 

energy_control_data <- ct_energy_aggregated %>%
  dplyr::select(ice_quartile,energy_per_account) %>%
  filter(ice_quartile =="Q4")

ct_energy_aggregated %>%
  filter(ice_quartile !="Q4") %>% 
  group_by(ice_quartile) %>%
  summarise(p_value = wilcox.test(energy_control_data$energy_per_account, energy_per_account, exact = FALSE)$p.value)


# Step 4: SAIFI Analyses (General)-------------------------------------------------------------------------

# Add ice quartile

ct_saifi_aggregated <- ct_saifi_aggregated %>% 
  left_join(ice_final, by = "geoid") %>% 
  left_join(cc_final, by = "geoid") %>%
  filter(!is.na(tot_hh)) %>% 
  filter(!is.na(cc)) %>% 
  filter(cc != "High-Low") %>% 
  filter(cc != "Low-High") %>% 
  mutate(cc = ifelse(cc == "High-High", "Consistently Advantaged", "Consistently Disadvantaged"))

write_csv(ct_saifi_aggregated, here("data","nyspo","final","both_saifi_final.csv"))


# Check distribution (not normal)

qqnorm(ct_saifi_aggregated$saifi, pch = 1, frame = FALSE) 

qqline(ct_saifi_aggregated$saifi, col = "steelblue", lwd = 2)

# Get summary statistics

ct_mean <- ct_saifi_aggregated %>% 
  group_by(cc) %>% 
  summarise(saifi = mean(saifi))

ct_mean

# Show distribution (and save for future plot)

c <- ggplot(ct_saifi_aggregated, aes(x=cc, y=saifi, fill=cc)) + 
  geom_boxplot() + 
  scale_fill_manual(values=(color_pal), guide = "none") + 
  theme_classic() +
  scale_y_log10() + 
  scale_x_discrete(limits=rev) + 
  xlab(label = "HOLC-ICE Pairs") + 
  ylab(label = "Average SAIFI (2017-2019)") + 
  ggtitle("C.") +
  theme(plot.title = element_text(face="bold", size = 15)) + 
  coord_flip()

c

# Summary data for chart 

round(summary(ct_saifi_aggregated$saifi),2)

# Add for future table 

saifi_ct <- as.vector(c(round(summary(ct_saifi_aggregated$saifi),2)))

# Summary of outage-rate by quartile 

saifi <- do.call(rbind , by(ct_saifi_aggregated$saifi, ct_saifi_aggregated$cc, summary)) 

saifi_ct_ice <- round(saifi, 2)

write.table(saifi_ct_ice, file = here("tables","saifi_ct_ice_holc.txt"), sep = ",", quote = F, row.names = T)

# Statistical tests 

saifi_control_data <- ct_saifi_aggregated %>%
  dplyr::select(ice_quartile,saifi) %>%
  filter(ice_quartile =="Q4")

ct_saifi_aggregated %>%
  filter(ice_quartile !="Q4") %>% 
  group_by(ice_quartile) %>%
  summarise(p_value = wilcox.test(saifi_control_data$saifi, saifi, exact = FALSE)$p.value)

# Step 5: Combine Plots-------------------------------------------------------------------------

# First, plot the data and save 

a / b / c

ggsave(filename = here("figures", "ct_holc_boxplot.png"), plot = a / b / c, width = 10, height = 10, units = "in", dpi = 300)
