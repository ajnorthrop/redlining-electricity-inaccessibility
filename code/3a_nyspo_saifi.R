####**************************
# Code Description ####
#*Goal: Create SAIFI Scores: NYS
#*Created 11/16/2022
#*Updated: 1/19/2023
####**************************

nyspo <- read_fst(here("data","nyspo","imputed_nyspo.fst"))

# SAIDI = System Average Interruption Duration Index. 
# It is the minutes of non-momentary electric interruptions, per year, the average customer experienced.
# SAIFI = System Average Interruption Frequency Index. 
# It is the number of non-momentary electric interruptions, per year, the average customer experienced.

#########################################
# Goal 1: Establish POL-level SAIFI     #
######################################### 

# Creates a PO for every instance in which a pod has at least one customer out 

nyspo_saifi <- nyspo %>% 
  filter(year(date) != 2020) %>% 
  group_by(year = year(date),dps_id) %>% 
  summarise(tcout = sum(customers_out),
            cust = mean(customers)) %>% 
  ungroup() %>% 
  group_by(dps_id) %>% 
  summarize(cust_years = sum(cust),
            customers = mean(cust),
            cust_interruptions = sum(tcout)) %>% 
  mutate(saifi = cust_interruptions/cust_years)

summary(nyspo_saifi$saifi)

nyspo_saifi %>% 
  write_csv(here("data","nyspo","saifi_17_19.csv"))


