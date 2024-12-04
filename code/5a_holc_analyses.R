####***********************
#### Code Description ####
# Author: Alex
# Date: 2/23/24
# Last Update: 6/27/24
# Goal: HOLC Analyses 
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

## Energy

holc_energy_aggregated <- read_csv(here("data","energy","final","holc_energy_final.csv")) %>%
  mutate(holc_grade = factor(holc_grade, levels = c("D", "C", "B", "A"))) %>% 
  mutate(energy_per_account = 3600 * energy_per_account) %>% # converts from MWH to MJ
  mutate(energy_per_account = ifelse(energy_per_account == 0, 0.0001, energy_per_account))

## SAIFI

holc_saifi_aggregated <- read_csv(here("data","nyspo","final","holc_saifi_final.csv")) %>%
  mutate(holc_grade = factor(holc_grade, levels = c("D", "C", "B", "A"))) %>% 
  mutate(saifi = ifelse(saifi == 0, 0.0001, saifi))

## HOLC 

included <- c("NYBronx1938", "NYBrooklyn1938", "NYManhattan1937",
              "NYQueens1938", "NYStatenIsland1940")

holc <- read_sf(here("shapes","holc_aggregated")) %>% 
  as.data.frame() %>% st_drop_geometry() %>% 
  filter(holc_grade != "E") %>% select(-geometry) %>% 
  filter(source %in% included) 

# Summary of HOLC grades

total_holc <- nrow(holc)
holc %>% group_by(holc_grade) %>% summarize(count = n())  %>% mutate(percent = 100*(count/total_holc))

# Step 2: 311 Analysis (General)-------------------------------------------------------------------------

# Check distribution (not normal)

qqnorm(holc_311_aggregated$outage_rate, pch = 1, frame = FALSE) 

qqline(holc_311_aggregated$outage_rate, col = "steelblue", lwd = 2)

# Get summary statistics

holc_mean <- holc_311_aggregated %>% 
  group_by(holc_grade) %>% 
  summarise(outage_rate = mean(outage_rate))

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
  coord_flip()

a

# Summary data for chart 

round(summary(holc_311_aggregated$outage_rate),2)

# Add for future table 

call_holc_sv <- as.vector(c(round(summary(holc_311_aggregated$outage_rate),2)))

# Outages by quartile 

holc_311_aggregated %>% 
  st_drop_geometry() %>% 
  group_by(holc_grade) %>% 
  summarize(outages = sum(outages)) %>% 
  mutate()

# Summary of outage-rate by quartile 

calls <- do.call(rbind , by(holc_311_aggregated$outage_rate, holc_311_aggregated$holc_grade, summary)) 

call_holc <- round(calls, 2)

call_holc

write.table(call_holc, file = here("tables","call_holc_ice.txt"), sep = ",", quote = F, row.names = T)

# Statistical tests 

outage_control_data <- holc_311_aggregated %>%
  select(holc_grade,outage_rate)%>%
  filter(holc_grade =="A")

holc_311_aggregated %>%
  filter(holc_grade != "A") %>%
  group_by(holc_grade) %>%
  summarise(p_value = wilcox.test(outage_control_data$outage_rate, outage_rate, exact = FALSE)$p.value)


# Step 3: Energy Analyses (General)-------------------------------------------------------------------------

# Check distribution (close to normal)

qqnorm(holc_energy_aggregated$energy_per_account, pch = 1, frame = FALSE) 

qqline(holc_energy_aggregated$energy_per_account, col = "steelblue", lwd = 2)

# Get summary statistics

holc_mean <- holc_energy_aggregated %>% 
  group_by(holc_grade) %>% 
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
  coord_flip()

b

# Summary data for chart 

round(summary(holc_energy_aggregated$energy_per_account),2)

# Add for future table 

energy_holc_sv <- as.vector(c(round(summary(holc_energy_aggregated$energy_per_account),2)))

# Summary of outage-rate by quartile 

energy <- do.call(rbind , by(holc_energy_aggregated$energy_per_account, holc_energy_aggregated$holc_grade, summary)) 

energy_holc <- round(energy, 0)

write.table(energy_holc, file = here("tables","energy_holc.txt"), sep = ",", quote = F, row.names = T)


# Statistical tests 

energy_control_data <- holc_energy_aggregated %>%
  select(holc_grade,energy_per_account) %>%
  filter(holc_grade =="A")

holc_energy_aggregated %>%
  filter(holc_grade != "A") %>%
  group_by(holc_grade) %>%
  summarise(p_value = wilcox.test(energy_control_data$energy_per_account, energy_per_account, exact = FALSE)$p.value)

# Step 4: SAIFI Analyses (General)-------------------------------------------------------------------------

# Check distribution (not normal)

qqnorm(holc_saifi_aggregated$saifi, pch = 1, frame = FALSE) 

qqline(holc_saifi_aggregated$saifi, col = "steelblue", lwd = 2)

# Get summary statistics

holc_mean <- holc_saifi_aggregated %>% 
  group_by(holc_grade) %>% 
  summarise(saifi = mean(saifi))

holc_saifi_aggregated %>% group_by(source) %>% summarize(saifi_mean = mean(saifi),
                                                         saifi_median = median(saifi))

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
  coord_flip()

c

# Summary data for chart 

round(summary(holc_saifi_aggregated$saifi),2)

# Add for future table 

saifi_holc_sv <- as.vector(c(round(summary(holc_saifi_aggregated$saifi),2)))

# Summary of outage-rate by quartile 

saifi <- do.call(rbind , by(holc_saifi_aggregated$saifi, holc_saifi_aggregated$holc_grade, summary)) 

saifi_holc <- round(saifi, 2)

write.table(saifi_holc, file = here("tables","saifi_holc.txt"), sep = ",", quote = F, row.names = T)

# Statistical tests 

saifi_control_data <- holc_saifi_aggregated %>%
  select(holc_grade,saifi) %>%
  filter(holc_grade =="A")

holc_saifi_aggregated %>%
  filter(holc_grade != "A") %>%
  group_by(holc_grade) %>%
  summarise(p_value = wilcox.test(saifi_control_data$saifi, saifi, exact = FALSE)$p.value)

# Step 5: Combine Plots-------------------------------------------------------------------------

# First, plot the data and save 

a / c / b

ggsave(filename = here("figures", "holc_general_boxplot.png"), plot = a / b / c, width = 10, height = 10, units = "in", dpi = 300)

# Then, get the output in a combined format for Table X 

labs <- c("min","q1","med","mean","q3","max")

# Data for table 
cbind(data.frame(labs),data.frame(call_holc_sv), data.frame(energy_holc_sv), data.frame(saifi_holc_sv))

