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

if(country_iso == "KEN" & risk_groups == 4){
  load("Len_optim_data_KEN_4_risk_groups.RData")  # loads naomi_ssa_shp_m, prep_uptake_fine_scale, risk_dist_targeting_fine_scale
  data = naomi_risk_dist_targeting_KEN_4_risk_groups
  rm(naomi_risk_dist_targeting_KEN_4_risk_groups)
}

if(country_iso == "KEN" & risk_groups == 8){
  load("Len_optim_data_KEN_8_risk_groups.RData")  # loads naomi_ssa_shp_m, prep_uptake_fine_scale, risk_dist_targeting_fine_scale
  data = naomi_risk_dist_targeting_8_risk_groups
  rm(naomi_risk_dist_targeting_KEN_8_risk_groups)
}

if(country_iso == "MWI"){
  load("Len_optim_data_MWI_8_risk_groups.RData")  # loads naomi_ssa_shp_m, prep_uptake_fine_scale, risk_dist_targeting_fine_scale
  data = naomi_risk_dist_targeting_MWI_8_risk_groups
  rm(naomi_risk_dist_targeting_MWI_8_risk_groups)
}

if(country_iso == "ZAF"){
  load("Len_optim_data_ZAF_8_risk_groups.RData")  # loads naomi_ssa_shp_m, prep_uptake_fine_scale, risk_dist_targeting_fine_scale
  data = naomi_risk_dist_targeting_ZAF_8_risk_groups
  rm(naomi_risk_dist_targeting_ZAF_8_risk_groups)
}

if(country_iso == "MOZ"){
  load("Len_optim_data_MOZ_8_risk_groups.RData")  # loads naomi_ssa_shp_m, prep_uptake_fine_scale, risk_dist_targeting_fine_scale
  data = naomi_risk_dist_targeting_MOZ_8_risk_groups
  rm(naomi_risk_dist_targeting_MOZ_8_risk_groups)
}

district_sf <- naomi_ssa_shp_m %>%
  filter(iso3 == country_iso) %>%
  dplyr::select(iso3, area_id, area_name,geometry) %>%
  distinct()

#filter(!is.na(Longitude),!is.na(Latitude)) %>%
#mutate(Latitude=Latitude*-1)

############################################################################
# Clean datasets
############################################################################
# 2. Create a facility dataframe that is a place-holder for analysis of just districts
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




