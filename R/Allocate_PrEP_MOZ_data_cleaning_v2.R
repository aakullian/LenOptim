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

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

############################################################################
# Load data files
############################################################################

## 1. Load naomi-based analysis output
load("Len_optim_data_MOZ.RData")

district_sf <- naomi_ssa_shp_m_MOZ %>%
  filter(iso3 == "MOZ") %>%
  dplyr::select(iso3, area_id, area_name,geometry) %>%
  distinct()

# 1. Load the datasets
facility_df <- read_csv("facility_data_MOZ_ec_v3_fix.csv", locale = locale(encoding = "ISO-8859-1"))
district_df <- fread("MOZ_district_df_UTF8_BOM.csv", encoding = "UTF-8")


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

incidence_df <- risk_dist_targeting_fine_scale %>%
  mutate(quantile_target_factor = paste(quant_target-0.125, "-",quant_target+0.125),
         inc_mult_group = cut(inc_mult, c(0,0.5,1,2,Inf), right=F),
         age_group_label = recode(age_group_label, "50+"="50-99")) %>%
  filter(quant_target %in% c(0.125,0.375,0.625,0.875), sex!="both",age_group_label!="15-49", age_group_label!="15-24")

# write.csv(facility_df_raw, "facility_df.csv", row.names = FALSE, na = "")
# write.csv(incidence_df_raw, "incidence_df.csv", row.names = FALSE, na = "")
# write.csv(prep_uptake_df_raw, "prep_uptake_df.csv", row.names = FALSE, na = "")

#rm(list = setdiff(ls(), c("facility_df_raw", "incidence_df_raw", "prep_uptake_df_raw", "facility_coords_df", "district_sf")))

facility_coords_df <- facility_df %>%
  select(facility_name, latitude, longitude, district, area_id) %>%
  filter(!is.na(longitude),!is.na(latitude)) 

#facility_coords_df <- st_as_sf(facility_coords_df, coords = c("Longitude", "Latitude"), crs = 4326)

head(facility_df)

