####***********************
#### Code Description ####
# Author: Alex
# Date: 2/23/24
# Last Update: 2/23/24
# Goal: CT Analyses 
####**********************

# Load packages 
rm(list=ls())
library(here)
source(paste0(here("code","0_setup", "0_read_libraries.R")))
options(scipen=999)

# Step 1: Load data required-------------------------------------------------------------------------

### Load ICE Data

ice_final <- read_csv(paste0(here("data", "ice", "race_and_income", "final", "ct_race_income_nyc.csv"))) # 2217

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

col_pal <- c("#440154FF", "#238A8DFF", "#74D055FF","#FDE725FF")

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
  group_by(ice_quartile) %>% 
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
  coord_flip()

a

# Summary data for chart 

round(summary(ct_311_aggregated$outage_rate),2)

# Add for future table 

call_ct <- as.vector(c(round(summary(ct_311_aggregated$outage_rate),2)))

# Outages by quartile 

ct_311_aggregated %>% 
  st_drop_geometry() %>% 
  group_by(ice_quartile) %>% 
  summarize(outages = sum(outages)) %>% 
  mutate()

# Summary of outage-rate by quartile 

calls <- do.call(rbind , by(ct_311_aggregated$outage_rate, ct_311_aggregated$ice_quartile, summary)) 

call_ct_ice <- round(calls, 2)

write.table(call_ct_ice, file = here("tables","call_ct_ice.txt"), sep = ",", quote = F, row.names = T)

# Statistical tests 

outage_control_data <- ct_311_aggregated %>%
  select(ice_quartile,outage_rate)%>%
  filter(ice_quartile =="Q4")

ct_311_aggregated %>%
  filter(ice_quartile !="Q4") %>% 
  group_by(ice_quartile) %>%
  summarise(p_value = wilcox.test(outage_control_data$outage_rate, outage_rate, exact = FALSE)$p.value)


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
  group_by(ice_quartile) %>% 
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
  coord_flip()

b

# Summary data for chart 

round(summary(ct_energy_aggregated$energy_per_account),2)

# Add for future table 

energy_ct <- as.vector(c(round(summary(ct_energy_aggregated$energy_per_account),2)))

# Summary of outage-rate by quartile 

energy <- do.call(rbind , by(ct_energy_aggregated$energy_per_account, ct_energy_aggregated$ice_quartile, summary)) 

energy_ct_ice <- round(energy, 0)

write.table(energy_ct_ice, file = here("tables","energy_ct_ice.txt"), sep = ",", quote = F, row.names = T)


# Statistical tests 

energy_control_data <- ct_energy_aggregated %>%
  select(ice_quartile,energy_per_account) %>%
  filter(ice_quartile =="Q4")

ct_energy_aggregated %>%
  filter(ice_quartile !="Q4") %>% 
  group_by(ice_quartile) %>%
  summarise(p_value = format(wilcox.test(energy_control_data$energy_per_account, energy_per_account, exact = FALSE)$p.value, 
                             scientific = FALSE))

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
  group_by(ice_quartile) %>% 
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
  coord_flip()

c

# Summary data for chart 

round(summary(ct_saifi_aggregated$saifi),2)

# Add for future table 

saifi_ct <- as.vector(c(round(summary(ct_saifi_aggregated$saifi),2)))

# Summary of outage-rate by quartile 

saifi <- do.call(rbind , by(ct_saifi_aggregated$saifi, ct_saifi_aggregated$ice_quartile, summary)) 

saifi_ct_ice <- round(saifi, 2)

write.table(saifi_ct_ice, file = here("tables","saifi_ct_ice.txt"), sep = ",", quote = F, row.names = T)

# Statistical tests 

saifi_control_data <- ct_saifi_aggregated %>%
  select(ice_quartile,saifi) %>%
  filter(ice_quartile =="Q4")

ct_saifi_aggregated %>%
  filter(ice_quartile !="Q4") %>% 
  group_by(ice_quartile) %>%
  summarise(p_value = wilcox.test(saifi_control_data$saifi, saifi, exact = FALSE)$p.value)

# Step 5: Combine Plots-------------------------------------------------------------------------

# First, plot the data and save 

a / c / b

ggsave(filename = here("figures", "ct_general_boxplot.png"), plot = a / b / c, width = 10, height = 10, units = "in", dpi = 300)

# Then, get the output in a combined format for Table X 

labs <- c("min","q1","med","mean","q3","max")

# Data for table 
cbind(data.frame(labs),data.frame(call_ct), data.frame(energy_ct), data.frame(saifi_ct))

