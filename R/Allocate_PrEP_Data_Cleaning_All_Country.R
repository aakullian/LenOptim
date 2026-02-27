# Functions to take a dataframe and allocate Len according to the population at risk size,
# the previous coverage of oral prep, and incidence rate. It constrains the number of units
# of Len to a total defined.

#sets the directory to where this script is stored#
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# -------------------------------
# Load required libraries
# -------------------------------

# List of required packages
required_packages <- c(
  "tidyr", "dplyr", "stringr", "readxl", "readr", "janitor",
  "DT", "sf", "ggplot2", "scales", "patchwork", "conflicted",
  "stringi", "fuzzyjoin", "purrr", "data.table","kableExtra"
)

# Install any packages that are missing
installed <- required_packages %in% installed.packages()[,"Package"]
if (any(!installed)) {
  install.packages(required_packages[!installed])
}

# Load packages
lapply(required_packages, library, character.only = TRUE)

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
library(kableExtra)
library(viridis)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

############################################################################
# Load data files
############################################################################

## 1. Load naomi-based analysis output
file_to_load <- paste0("Len_optim_data_", country_iso, "_", risk_groups, "_risk_groups.RData")
load(file_to_load)
obj_name <- paste0("naomi_risk_dist_targeting_", country_iso, "_", risk_groups, "_risk_groups")
data <- get(obj_name)
rm(list=obj_name)

## 2. Load naomi shapefile
district_sf <- naomi_ssa_shp_m %>%
  filter(iso3 == country_iso) %>%
  dplyr::select(iso3, area_id, area_name,geometry) %>%
  distinct()

#filter(!is.na(Longitude),!is.na(Latitude)) %>%
#mutate(Latitude=Latitude*-1)

############################################################################
# Clean datasets
############################################################################
# 2. Create a "facility" dataframe that is a place-holder for analysis of just districts
facility_df <- data.frame(naomi_ssa_shp_m) %>%
  filter(iso3==country_iso, sex != "both", age_group_label %in% c("15-24", "25-34", "35-49", "50+")) %>%
  select(area_id, area_name, sex, age_group_label, pop_at_risk) %>%
  mutate(
    province = NA,
    district = area_name,
    sex_abbrev = ifelse(sex == "female", "f", "m"),
    age_group_label = gsub("-", " - ", age_group_label),
    new_col = paste0(sex_abbrev, "_", age_group_label)
  ) %>%
  select(province, area_id, district, new_col, pop_at_risk) %>%  # <-- Only keep columns needed for pivoting
  pivot_wider(names_from = new_col, values_from = pop_at_risk) %>%
  mutate(total_initiations = 0, facility_name = paste("district_",area_id,sep=""), latitude=NA, longitude=NA) 

facility_coords_df <- facility_df %>%
  select(facility_name, latitude, longitude, district, area_id)
#filter(!is.na(longitude),!is.na(latitude)) 

#Create incidence_df from the risk distribution district-level data
n_quants = length(unique(data$quant_target))
incidence_df <- data %>%
  mutate(quantile_target_factor = paste(quant_target-1/n_quants, "-",quant_target),
         inc_mult_group = cut(inc_mult, c(0,0.5,1,2,Inf), right=F),
         age_group_label = recode(age_group_label, "50+"="50-99"))

rm(naomi_ssa_shp_m, data)

#summary of new infections and pop at risk by district
district_new_infections = data.frame(incidence_df) %>% select(area_id, sex, age_group_label, total_infected_subsample, pop_subsample) %>%
  group_by(area_id) %>%
  summarize(new_infections = sum(total_infected_subsample), pop_at_risk = sum(pop_subsample))




