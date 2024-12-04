####***********************
#### Code Description ####
# Author: Alex
# Date: 2/23/24
# Last Update: 2/25/24
# Goal: HOLC Analyses 
####**********************

# Load packages 
rm(list=ls())
library(here)
library(ggcorrplot)
source(paste0(here("code","0_setup", "0_read_libraries.R")))

# Step 1: Load data required-------------------------------------------------------------------------

### Load HOLC Data 

## 311 

holc_311_aggregated <- read_csv(here("data","311","final","holc_311_final.csv")) %>%
  select(unique_id, outage_rate)

## Energy

holc_energy_aggregated <- read_csv(here("data","energy","final","holc_energy_final.csv")) %>%
  select(unique_id, energy_per_account)

## SAIFI

holc_saifi_aggregated <- read_csv(here("data","nyspo","final","holc_saifi_final.csv")) %>%
  select(unique_id, saifi)

# Step 2: Combine into a common df-------------------------------------------------------------------

holc_combined <- holc_311_aggregated %>% 
  left_join(holc_energy_aggregated, by = "unique_id") %>% 
  left_join(holc_saifi_aggregated, by = "unique_id") %>% 
  select(-unique_id) %>% 
  na.omit() # Remove NAs

# Step 2: Get correlation-------------------------------------------------------------------------

### Load data 

# Correlation

holc_combined <- holc_combined %>% 
  rename("Outage Rate (311 Calls)" = outage_rate,
    "Residential Energy Use" = energy_per_account,
    "SAIFI" = "saifi"
  ) 

correlation_matrix <- round(cor(holc_combined, method = "spearman"), 2)
correlation_p <- round(cor_pmat(holc_combined, method = "spearman"), 2)

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
        panel.border = element_rect(colour = "black", fill=NA, size=1)) 

# Step 4: Save-------------------------------------------------------------------------


ggsave(here("figures","holc_correlation_matrix.png"), width = 10, height = 10, units = "in", dpi = 300) 
