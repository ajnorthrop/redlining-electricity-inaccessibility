####**************************
# Code Description ####
#*Goal: Aggregate NYC HOLC Map
#*Created 11/6/2022
#*Last Edit: 10/4/2024, updated comments
####**************************

sf_use_s2(FALSE)

### Redlining ###

# HOLC Maps and get relevant geometries 

dir_list <- list.dirs(here("shapes","holc_maps")) 

holc_combined <- do.call(sf:::rbind.sf, sapply(dir_list[2:18], st_read, simplify = FALSE))

holc_combined$source <- row.names(holc_combined)

rownames(holc_combined) <- NULL

holc_combined$source <- sapply(strsplit(holc_combined$source, "/\\s*"), tail, 1)
holc_combined$source <- gsub("\\..*","",holc_combined$source)

holc_combined <- holc_combined %>% 
  mutate(unique_id = row_number())

# Write shape file

st_write(holc_combined, here("shapes","holc_aggregated", "holc_aggregated.shp"))