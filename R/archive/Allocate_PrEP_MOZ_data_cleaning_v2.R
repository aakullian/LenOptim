# Functions to take a dataframe and allocate Len according to the population at risk size,
# the previous coverage of oral prep, and incidence rate. It constrains the number of units
# of Len to a total defined.

#sets the directory to where this script is stored#
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(tidyr)
library(dplyr)
library(stringr)
library(readxl)
library(readr)
library(janitor)
library(DT)
library(sf)
library(ggplot2)
library(scales)
library(patchwork)
library(conflicted)
library(stringi)
library(fuzzyjoin)
library(purrr)
library(data.table)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

############################################################################
# Load data files
############################################################################

## 1. Load naomi-based analysis output
load("Len_optim_data_MOZ_F1524.RData")

district_sf <- naomi_ssa_shp_m %>%
  filter(iso3 == "MOZ") %>%
  dplyr::select(iso3, area_id, area_name,geometry) %>%
  distinct()

# 1. Load the datasets
facility_df <- read_csv("facility_data_MOZ_ec_v3_fix.csv", locale = locale(encoding = "ISO-8859-1"))
length(unique(facility_df$facility_name))
district_df <- fread("MOZ_district_df_UTF8_BOM.csv", encoding = "UTF-8")

facility_df_district_names = facility_df %>%
  select(area_id, district, province) %>%
  distinct(.keep_all = TRUE)

age_groups = c("15-24", "25-34", "35-49", "50+")

# 2. Create a facility dataframe that is a place-holder for analysis of just districts
facility_df_place_holder <- data.frame(naomi_ssa_shp_m) %>%
  select(area_id, sex, age_group_label, pop_at_risk) %>%
  filter(sex != "both", age_group_label %in% c("15-24", "25-34", "35-49", "50+")) %>%
  mutate(
    sex_abbrev = ifelse(sex == "female", "f", "m"),
    age_group_label = gsub("-", " - ", age_group_label),
    new_col = paste0(sex_abbrev, "_", age_group_label)
  ) %>%
  select(area_id, new_col, pop_at_risk) %>%  # <-- Only keep columns needed for pivoting
  pivot_wider(names_from = new_col, values_from = pop_at_risk) %>%
  mutate(total_initiations = 0, facility_name = paste("district_",area_id,sep=""), latitude=NA, longitude=NA) %>%
  left_join(facility_df_district_names, by="area_id")

#filter(!is.na(Longitude),!is.na(Latitude)) %>%
#mutate(Latitude=Latitude*-1)

  
############################################################################
# Clean datasets
############################################################################


facility_df <- facility_df %>%
    mutate(
      latitude = as.numeric(latitude),
      longitude = as.numeric(longitude)
    ) 
  #filter(!is.na(Longitude),!is.na(Latitude)) %>%
  #mutate(Latitude=Latitude*-1)
  
head(facility_df[c("latitude", "longitude")])


#Create incidence_df from the risk distribution district-level data

# incidence_df <- risk_dist_targeting_fine_scale %>%
#   mutate(quantile_target_factor = paste(quant_target-0.125, "-",quant_target+0.125),
#          inc_mult_group = cut(inc_mult, c(0,0.5,1,2,Inf), right=F),
#          age_group_label = recode(age_group_label, "50+"="50-99")) %>%
#   filter(quant_target %in% c(0.125,0.375,0.625,0.875), sex!="both",age_group_label!="15-49", age_group_label!="15-24")

incidence_df <- risk_dist_targeting_fine_scale %>%
  mutate(quantile_target_factor = paste(quant_target-0.125, "-",quant_target+0.125),
         inc_mult_group = cut(inc_mult, c(0,0.5,1,2,Inf), right=F),
         age_group_label = recode(age_group_label, "50+"="50-99")) %>%
  filter(quant_target %in% c(0.125,0.375,0.625,0.875))

# write.csv(facility_df_raw, "facility_df.csv", row.names = FALSE, na = "")
# write.csv(incidence_df_raw, "incidence_df.csv", row.names = FALSE, na = "")
# write.csv(prep_uptake_df_raw, "prep_uptake_df.csv", row.names = FALSE, na = "")

#rm(list = setdiff(ls(), c("facility_df_raw", "incidence_df_raw", "prep_uptake_df_raw", "facility_coords_df", "district_sf")))

facility_coords_df <- facility_df %>%
  select(facility_name, latitude, longitude, district, area_id) %>%
  filter(!is.na(longitude),!is.na(latitude)) 

#facility_coords_df <- st_as_sf(facility_coords_df, coords = c("Longitude", "Latitude"), crs = 4326)

head(facility_df)

#summary of new infections and pop at risk by district
district_new_infections = data.frame(incidence_df) %>% select(area_id, sex, age_group_label, total_infected_subsample, pop_subsample) %>%
  group_by(area_id) %>%
  summarize(new_infections = sum(total_infected_subsample), pop_at_risk = sum(pop_subsample))

sum(district_new_infections$new_infections)



