####***********************
#### Code Description ####
# Author: Alex
# Date: 4/15/24
# Last Update: 4/15/24
# Goal: Create ridgeline plots for CT and HOLC 
####**********************


# Load packages 
rm(list=ls())
library(here)
source(paste0(here("code","0_setup", "0_read_libraries.R")))

# Step 1: Load data required-------------------------------------------------------------------------

### Load HOLC Data 
# NB: 0s are replaced with 0.0001 to avoid log(0) errors

## 311 

holc_311_aggregated <- read_csv(here("data","311","final","holc_311_final.csv")) %>%
  mutate(holc_grade = factor(holc_grade, levels = c("D", "C", "B", "A"))) %>% 
  mutate(outage_rate = ifelse(outage_rate == 0, 0.0001, outage_rate))

holc_311_aggregated$holc_grade <- factor(holc_311_aggregated$holc_grade, levels = c("A", "B", "C", "D"))

holc_311_warm <- read_csv(here("data","311","final","holc_311_warm_final.csv")) %>% 
  mutate(outage_rate = ifelse(outage_rate == 0, 0.0001, outage_rate)) 

holc_311_cool <- read_csv(here("data","311","final","holc_311_cool_final.csv")) %>%
  mutate(outage_rate = ifelse(outage_rate == 0, 0.0001, outage_rate))

holc_311_seasonal <- rbind(holc_311_cool, holc_311_warm)

holc_311_seasonal$holc_grade <- factor(holc_311_seasonal$holc_grade, levels = c("A", "B", "C", "D"))


## Energy

holc_energy_aggregated <- read_csv(here("data","energy","final","holc_energy_final.csv")) %>%
  mutate(holc_grade = factor(holc_grade, levels = c("D", "C", "B", "A"))) %>% 
  mutate(energy_per_account = 3600 * energy_per_account) %>% # converts from MWH to MJ
  mutate(energy_per_account = ifelse(energy_per_account == 0, 0.0001, energy_per_account))

holc_energy_aggregated$holc_grade <- factor(holc_energy_aggregated$holc_grade, levels = c("A", "B", "C", "D"))

holc_energy_cool <- read_csv(here("data","energy","final","holc_energy_cool_final.csv")) %>% 
  mutate(season = "cool") %>%
  mutate(energy_per_account = 3600 * energy_per_account) %>% # converts from MWH to MJ
  mutate(energy_per_account = ifelse(energy_per_account == 0, 0.0001, energy_per_account))

holc_energy_warm <- read_csv(here("data","energy","final","holc_energy_warm_final.csv")) %>% 
  mutate(season = "warm") %>%
  mutate(energy_per_account = 3600 * energy_per_account) %>% # converts from MWH to MJ
  mutate(energy_per_account = ifelse(energy_per_account == 0, 0.0001, energy_per_account))

holc_energy_seasonal <- rbind(holc_energy_cool, holc_energy_warm)

holc_energy_seasonal$holc_grade <- factor(holc_energy_seasonal$holc_grade, levels = c("A", "B", "C", "D"))


## SAIFI

holc_saifi_aggregated <- read_csv(here("data","nyspo","final","holc_saifi_final.csv")) %>%
  mutate(holc_grade = factor(holc_grade, levels = c("D", "C", "B", "A"))) %>% 
  mutate(saifi = ifelse(saifi == 0, 0.0001, saifi))

holc_saifi_aggregated$holc_grade <- factor(holc_saifi_aggregated$holc_grade, levels = c("A", "B", "C", "D"))

holc_saifi_cool <- read_csv(here("data","nyspo","final","holc_saifi_cool_final.csv")) %>% 
  mutate(season = "cool") %>%
  mutate(saifi = ifelse(saifi == 0, 0.0001, saifi))

holc_saifi_warm <- read_csv(here("data","nyspo","final","holc_saifi_warm_final.csv")) %>% 
  mutate(season = "warm") %>% 
  mutate(saifi = ifelse(saifi == 0, 0.0001, saifi))

holc_saifi_seasonal <- rbind(holc_saifi_cool, holc_saifi_warm)

holc_saifi_seasonal$holc_grade <- factor(holc_saifi_seasonal$holc_grade, levels = c("A", "B", "C", "D"))


### Load ICE Data

ice_final <- read_csv(paste0(here("data", "ice", "race_and_income", "final", "ct_race_income_nyc.csv"))) # 2217

### Load CT Data 

## 311 

ct_311_aggregated <- read_csv(here("data","311","final","ct_311_final.csv")) %>% # 2217
  mutate(outage_rate = ifelse(outage_rate == 0, 0.0001, outage_rate)) %>% 
  left_join(ice_final, by = "geoid") %>% 
  filter(!is.na(tot_hh))

ct_311_aggregated$ice_quartile <- factor(ct_311_aggregated$ice_quartile, levels = c("Q4", "Q3", "Q2", "Q1"))

ct_311_cool <- read_csv(here("data","311","final","ct_311_cool_final.csv")) %>%
  mutate(outage_rate = ifelse(outage_rate == 0, 0.0001, outage_rate)) %>% 
  left_join(ice_final, by = "geoid") %>% 
  filter(!is.na(tot_hh))

ct_311_warm <- read_csv(here("data","311","final","ct_311_warm_final.csv")) %>%
  mutate(outage_rate = ifelse(outage_rate == 0, 0.0001, outage_rate)) %>% 
  left_join(ice_final, by = "geoid") %>% 
  filter(!is.na(tot_hh))

ct_311_seasonal <- rbind(ct_311_cool, ct_311_warm)

ct_311_seasonal$ice_quartile <- factor(ct_311_seasonal$ice_quartile, levels = c("Q4", "Q3", "Q2", "Q1"))


## Energy

ct_energy_aggregated <- read_csv(here("data","energy","final","ct_energy_final.csv")) %>% #2287
  mutate(energy_per_account = ifelse(energy_per_account == 0, 0.0001, energy_per_account)) %>% 
  mutate(energy_per_account = 3600 * energy_per_account) %>% # converts from MWH to MJ
  left_join(ice_final, by = "geoid") %>% 
  filter(!is.na(tot_hh))

ct_energy_aggregated$ice_quartile <- factor(ct_energy_aggregated$ice_quartile, levels = c("Q4", "Q3", "Q2", "Q1"))

ct_energy_cool <- read_csv(here("data","energy","final","ct_energy_cool_final.csv")) %>% 
  mutate(season = "cool") %>%
  mutate(energy_per_account = 3600 * energy_per_account) %>% # converts from MWH to MJ
  mutate(energy_per_account = ifelse(energy_per_account == 0, 0.0001, energy_per_account)) %>% 
  left_join(ice_final, by = "geoid") %>% 
  filter(!is.na(tot_hh))

ct_energy_warm <- read_csv(here("data","energy","final","ct_energy_warm_final.csv")) %>% 
  mutate(season = "warm") %>%
  mutate(energy_per_account = 3600 * energy_per_account) %>% # converts from MWH to MJ
  mutate(energy_per_account = ifelse(energy_per_account == 0, 0.0001, energy_per_account)) %>% 
  left_join(ice_final, by = "geoid") %>% 
  filter(!is.na(tot_hh))

ct_energy_seasonal <- rbind(ct_energy_cool, ct_energy_warm)

ct_energy_seasonal$ice_quartile <- factor(ct_energy_seasonal$ice_quartile, levels = c("Q4", "Q3", "Q2", "Q1"))

## SAIFI

ct_saifi_aggregated <- read_csv(here("data","nyspo","final","ct_saifi_final.csv")) %>% # 2313 
  mutate(saifi = ifelse(saifi == 0, 0.0001, saifi)) %>% 
  left_join(ice_final, by = "geoid") %>% 
  filter(!is.na(tot_hh))

ct_saifi_aggregated$ice_quartile <- factor(ct_saifi_aggregated$ice_quartile, levels = c("Q4", "Q3", "Q2", "Q1"))

ct_saifi_warm <- read_csv(here("data","nyspo","final","ct_saifi_warm_final.csv")) %>% 
  mutate(season = "warm") %>%
  mutate(saifi = ifelse(saifi == 0, 0.0001, saifi)) %>% 
  left_join(ice_final, by = "geoid") %>% 
  filter(!is.na(tot_hh))

ct_saifi_cool <- read_csv(here("data","nyspo","final","ct_saifi_cool_final.csv")) %>% 
  mutate(season = "cool") %>%
  mutate(saifi = ifelse(saifi == 0, 0.0001, saifi)) %>%
  left_join(ice_final, by = "geoid") %>% 
  filter(!is.na(tot_hh))

ct_saifi_seasonal <- rbind(ct_saifi_warm, ct_saifi_cool)

ct_saifi_seasonal$ice_quartile <- factor(ct_saifi_seasonal$ice_quartile, levels = c("Q4", "Q3", "Q2", "Q1"))

# Combined

both_311 <- read_csv(here("data","311","final","both_311_final.csv")) %>% 
  mutate(outage_rate = ifelse(outage_rate == 0, 0.0001, outage_rate)) %>% mutate(cc = ifelse(cc == "Consistently Disadvantaged", "Persistently Disadvantaged", cc))

both_energy <- read_csv(here("data","energy","final","both_energy_final.csv")) %>%
  mutate(energy_per_account = ifelse(energy_per_account == 0, 0.0001, energy_per_account)) %>% mutate(cc = ifelse(cc == "Consistently Disadvantaged", "Persistently Disadvantaged", cc))

both_saifi <- read_csv(here("data","nyspo","final","both_saifi_final.csv")) %>%
  mutate(saifi = ifelse(saifi == 0, 0.0001, saifi)) %>% mutate(cc = ifelse(cc == "Consistently Disadvantaged", "Persistently Disadvantaged", cc))

# Obtain color palettes 

col_holc <- c("#537f4d", "#53818c", "#c6bd63","#a95861")

col_ct <- rev(c("#440154FF", "#238A8DFF", "#74D055FF","#FDE725FF"))

col_both <- c("#eb8055b2", "#13306db2")


# Step 2: Create ridgeline functions-------------------------------------------------------------------------

ridges <- function(data, string, metric, strat, color_pal, xlab, type) {
  
  graph <- data %>%
    ggplot(aes({{metric}}, color = {{strat}}, fill = {{strat}}, y ={{strat}})) + 
    scale_color_manual(values = color_pal, guide = "none") +
    scale_fill_manual(values = color_pal, guide = "none") +
    ggridges::stat_density_ridges(bandwidth = .1,
                                  quantile_lines = TRUE, quantiles = 2, 
                                  color = "black", alpha = .8) +
    scale_y_discrete(expand = c(0,0)) +
    xlab(xlab) + 
    ylab(type) + 
    scale_x_log10() +
    theme_classic() + 
    theme(axis.text.y = element_text(size = 6),
          axis.ticks.y = element_blank(),
          axis.title.y = element_text(size = 7),
          axis.text.x = element_text(size = 6),
          axis.title.x = element_text(size = 7),
          strip.text = element_text(size = 7),
          legend.position = "none") 
  
    ggsave(here("figures","ridge_plots",paste0("ridgeplot_",string,".png")))
    
    return(graph)
}

ridges_seasonal <- function(data,string, metric, strat, color_pal, xlab, type) {
  
  graph <- data %>%
  ggplot(aes({{metric}}, color = {{strat}}, fill = {{strat}}, y ={{strat}})) + 
  scale_color_manual(values = color_pal, guide = "none") +
  scale_fill_manual(values = color_pal, guide = "none") +
  ggridges::stat_density_ridges(bandwidth = .1,
                                quantile_lines = TRUE, quantiles = 2, 
                                color = "black", alpha = .8) +
  scale_y_discrete(expand = c(0,0)) +
  xlab(xlab) + 
  ylab(type) + 
  scale_x_log10() +
  theme_classic() + 
  theme(axis.text.y = element_text(size = 6),
        axis.ticks.y = element_blank(),
        axis.title.y = element_text(size = 7),
        axis.text.x = element_text(size = 6),
        axis.title.x = element_text(size = 7),
        strip.text = element_text(size = 6),
        legend.position = "none") +
  facet_wrap(~season, nrow = 2, 
             labeller = labeller(season = c(`cool` = "Cool Season", "warm" = "Warm Season"))) 
  
    ggsave(here("figures","ridge_plots",paste0("seasonal_ridgeplot_",string,".png")))
    
    return(graph)
}

# Step 3: Run functions -------------------------------------------------------------------------

holc_311 <- ridges(holc_311_aggregated,"holc_311", outage_rate, holc_grade, 
                   col_holc, paste0("Calls per 1,000 Household-Years (More Outages ","\U2192", ")"), "HOLC Grade")

holc_saifi <- ridges(holc_saifi_aggregated,"holc_saifi", saifi, holc_grade, 
                     col_holc, paste0("SAIFI (Unreliability ","\U2192", ")"), "HOLC Grade")

holc_energy <- ridges(holc_energy_aggregated, "holc_energy", energy_per_account, holc_grade,
                      col_holc, paste0("Average MJ per Account, Monthly (More Energy Use ","\U2192", ")"), "HOLC Grade")

ct_311 <- ridges(ct_311_aggregated, "ct_311", outage_rate, ice_quartile, col_ct, paste0("Calls per 1,000 Household-Years (More Outages ","\U2192", ")"), 
                 paste0("ICE Quartile \n(", "Disadvantage ", "\U2192", ")"))
ct_saifi <- ridges(ct_saifi_seasonal, "ct_saifi", saifi, ice_quartile, col_ct, paste0("SAIFI (Unreliability ","\U2192", ")"), 
                   paste0("ICE Quartile \n(", "Disadvantage ", "\U2192", ")"))
ct_energy <- ridges(ct_energy_seasonal, "ct_energy", energy_per_account, ice_quartile, col_ct, paste0("Average MJ per Account, Monthly (More Energy Use ","\U2192", ")"), 
                    paste0("ICE Quartile \n(", "Disadvantage ", "\U2192", ")"))

both_311 <- ridges(both_311, "both_311", outage_rate, cc, col_both, paste0("Calls per 1,000 Household-Years (More Outages ","\U2192", ")"), "") 
both_saifi <- ridges(both_saifi, "both_saifi", saifi, cc, col_both, paste0("SAIFI (Unreliability ","\U2192", ")"), "")
both_energy <- ridges(both_energy, "both_energy", energy_per_account, cc, col_both, paste0("Average MJ per Account, Monthly (More Energy Use ","\U2192", ")"), "")

holc_311_s <- ridges_seasonal(holc_311_seasonal, "holc_311", outage_rate, holc_grade, col_holc, paste0("Calls per 1,000 Household-Years (More Outages ","\U2192", ")"), "HOLC Grade")
holc_saifi_s <- ridges_seasonal(holc_saifi_seasonal, "holc_saifi",saifi, holc_grade, col_holc, paste0("SAIFI (Unreliability ","\U2192", ")"), "HOLC Grade")
holc_energy_s <- ridges_seasonal(holc_energy_seasonal, "holc_energy",energy_per_account, holc_grade, col_holc, paste0("Average MJ per Account, Monthly (More Energy Use ","\U2192", ")"), "HOLC Grade")

ct_311_s <- ridges_seasonal(ct_311_seasonal, "ct_311", outage_rate, ice_quartile, col_ct, paste0("Calls per 1,000 Household-Years (More Outages ","\U2192", ")"), 
                            paste0("ICE Quartile \n(", "Disadvantage ", "\U2192", ")"))
ct_saifi_s <- ridges_seasonal(ct_saifi_seasonal, "ct_saifi", saifi, ice_quartile, col_ct, paste0("SAIFI (Unreliability ","\U2192", ")"), 
                              paste0("ICE Quartile \n(", "Disadvantaged ", "\U2192", ")"))
ct_energy_s <- ridges_seasonal(ct_energy_seasonal, "ct_energy", energy_per_account, ice_quartile, col_ct, paste0("Average MJ per Account, Monthly (More Energy Use ","\U2192", ")"), 
                               paste0("ICE Quartile \n(", "Disadvantaged ", "\U2192", ")"))


# Step 4: Save aggregate graphs -------------------------------------------------------------------------
# Author note: For the combined graphs, they are saved in the a .png file and then converted to .pdf using Preview on Mac 
# This was done due to the driver producing an error when generating unicode arrows that are used in the figures 

holc_311 <- holc_311 + ggtitle("A") + theme(plot.title = element_text(face = "bold", size = 7)) 
holc_saifi <- holc_saifi + ggtitle("B") + theme(plot.title = element_text(face = "bold", size = 7)) 
holc_energy <-  holc_energy + ggtitle("C") + theme(plot.title = element_text(face = "bold", size = 7))
holc_simple <- holc_311 / holc_saifi / holc_energy

ggsave(here("figures","ridge_plots","holc_combined.png"), holc_simple,
              width = 178, height = 166, dpi = 300, units = "mm", device='png')

ct_311 <- ct_311 + ggtitle("A") + theme(plot.title = element_text(face = "bold", size = 7))
ct_saifi <- ct_saifi + ggtitle("B") + theme(plot.title = element_text(face = "bold", size = 7)) 
ct_energy <- ct_energy + ggtitle("C") + theme(plot.title = element_text(face = "bold", size = 7))

ct_simple <- ct_311 / ct_saifi / ct_energy
ct_simple

ggsave(here("figures","ridge_plots","ct_combined.png"), ct_simple,
       width = 178, height = 166, dpi = 300, units = "mm", device='png')

holc_311_s <- holc_311_s + ggtitle("A") + theme(plot.title = element_text(face = "bold", size = 7))
holc_saifi_s <- holc_saifi_s + ggtitle("B") + theme(plot.title = element_text(face = "bold", size = 7))
holc_energy_s <- holc_energy_s + ggtitle("C") + theme(plot.title = element_text(face = "bold", size = 7))

holc_s <- holc_311_s / holc_saifi_s / holc_energy_s
holc_s

ggsave(here("figures","ridge_plots","holc_combined_seasonal.png"), holc_s,
       width = 178, height = 166, dpi = 300, units = "mm", device='png')

# ggsave(here("figures","ridge_plots","holc_combined_seasonal.png"), holc_s)

ct_311_s <- ct_311_s + ggtitle("A") + theme(plot.title = element_text(face = "bold", size = 7)) 
ct_saifi_s <- ct_saifi_s + ggtitle("B") + theme(plot.title = element_text(face = "bold", size = 7))
ct_energy_s <- ct_energy_s + ggtitle("C") + theme(plot.title = element_text(face = "bold", size = 7))

ct_s <- ct_311_s / ct_saifi_s / ct_energy_s
ct_s

ggsave(here("figures","ridge_plots","ct_combined_seasonal.png"), ct_s,
       width = 178, height = 166, dpi = 300, units = "mm", device='png')

# ggsave(here("figures","ridge_plots","ct_combined_seasonal.png"), ct_s)

both_311 <- both_311 + ggtitle("A") + theme(plot.title = element_text(face = "bold", size = 7),
                                            axis.text.y = element_text(vjust = -2))
both_saifi <- both_saifi + ggtitle("B") + theme(plot.title = element_text(face = "bold", size = 7),
                                                axis.text.y = element_text(vjust = -2))
both_energy <- both_energy + ggtitle("C") + theme(plot.title = element_text(face = "bold", size = 7),
                                                  axis.text.y = element_text(vjust = -2))

both <- both_311 / both_saifi / both_energy
both

ggsave(here("figures","ridge_plots","both_combined.png"), both,
       width = 178, height = 166, dpi = 300, units = "mm", device='png')






