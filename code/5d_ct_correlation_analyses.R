####***********************
#### Code Description ####
# Author: Alex
# Date: 2/23/24
# Last Update: 2/25/24
# Goal: ICE Correlation Analyses 
####**********************

# Load packages 
rm(list=ls())
library(here)
library(ggcorrplot)
source(paste0(here("code","0_setup", "0_read_libraries.R")))

# Step 1: Load data required-------------------------------------------------------------------------

### Load ICE data

### Load ct Data 

## 311 

ct_311_aggregated <- read_csv(here("data","311","final","ct_311_final.csv")) %>%
  select(geoid, outage_rate)

## Energy

ct_energy_aggregated <- read_csv(here("data","energy","final","ct_energy_final.csv")) %>%
  select(geoid, energy_per_account)

## SAIFI

ct_saifi_aggregated <- read_csv(here("data","nyspo","final","ct_saifi_final.csv")) %>%
  select(geoid, saifi)

# Step 2: Combine into a common df-------------------------------------------------------------------

ct_combined <- ct_311_aggregated %>% 
  left_join(ct_energy_aggregated, by = "geoid") %>% 
  left_join(ct_saifi_aggregated, by = "geoid") %>% 
  na.omit() %>% 
  select(-geoid)

# Step 2: Get correlation-------------------------------------------------------------------------

### Load data 

# Correlation

ct_combined <- ct_combined %>% 
  rename("Outage Rate (311 Calls)" = outage_rate,
    "Residential Energy Use" = energy_per_account,
    "SAIFI" = "saifi"
  ) 

correlation_matrix <- round(cor(ct_combined, method = "spearman"), 2)
correlation_p <- cor_pmat(ct_combined, method = "spearman")

ggcorrplot(correlation_matrix, hc.order =TRUE, 
           type ="lower", lab =T, p.mat=correlation_p, 
           ggtheme = ggplot2::theme_classic,
           insig = "blank",
           outline.color = "black",
           title = "Correlation Matrix") + 
  theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust=0.5),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y =element_blank(),
        axis.line.x =element_blank(),
        plot.title=element_text(face="bold"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth =1)) 

# Step 4: Save-------------------------------------------------------------------------


ggsave(here("figures","ct_correlation_matrix.png"), width = 10, height = 10, units = "in", dpi = 300) 
