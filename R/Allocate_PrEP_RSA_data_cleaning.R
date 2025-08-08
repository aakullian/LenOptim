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

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

############################################################################
# Load data files
############################################################################

## Load naomi-based analysis output
load("Len_optim_data_RSA.RData")

district_sf <- naomi_ssa_shp_m %>%
  filter(iso3 == "ZAF") %>%
  dplyr::select(iso3, area_id, area_name,geometry)

rm(list = setdiff(ls(), c("naomi_ssa_shp_m", "district_sf","risk_dist_targeting_fine_scale")))

# Read the facility-level CSV provided by Hasina
df <- read_delim("LEN Quantification Perf Review 24.06.2025_With Geo-Coordinates 24.06.2024.csv",
                 delim = ",",
                 locale = locale(encoding = "Latin1"),
                 guess_max = 5000)  # ensures full type guessing

# Load district names
district_names <- read.csv('District Names.csv') %>% #names to merge facility to NAOMI datasets
  dplyr::select(area_id,District) %>%
  rename(district=District)

# Load the oral prep disagg data (use the correct sheet name)
df_raw <- read_excel("Disaggregated PrEP Initiations April 2020 to March 2025.xlsx", sheet = "Sheet2")

############################################################################
# Clean datasets
############################################################################

# Clean column names
df <- df %>%
  janitor::clean_names()

# Function to clean numeric strings
safe_numeric <- function(x) {
  x <- as.character(x)
  x <- iconv(x, to = "UTF-8", sub = "byte")     # fix encoding
  x <- gsub(",", "", x)                         # remove commas
  x <- ifelse(grepl("%", x), 
              as.numeric(gsub("%", "", x)) / 100, 
              x)                                # convert percentages
  suppressWarnings(as.numeric(x))               # convert to numeric
}

# Identify character columns that are likely numeric
char_cols <- df %>% dplyr::select(where(is.character))
likely_numeric <- sapply(char_cols, function(col) {
  sum(grepl("[0-9]", col)) / length(col) > 0.5  # more than half look numeric
})

# Apply numeric cleaning only to those
df[ , names(likely_numeric[likely_numeric])] <- 
  lapply(df[ , names(likely_numeric[likely_numeric])], safe_numeric)

# Check result
ncol(df)

df <- df %>%
  left_join(district_names, join_by(district)) %>%
  mutate(
    Latitude = as.numeric(latitude),
    Longitude = as.numeric(longitude)
  ) %>%
  filter(!is.na(Longitude),!is.na(Latitude)) %>%
  mutate(Latitude=Latitude*-1)

head(df[c("Latitude", "Longitude")])

facility_df_mapping <- df %>%
  dplyr::select(province, district, sub_district, correct_facility_name, Latitude, Longitude)

facility_coords_df <- facility_df_mapping %>%
  rename(
    facility_name = correct_facility_name,
    latitude = Latitude,
    longitude = Longitude
  ) %>%
  select(facility_name, latitude, longitude)

facility_df_raw <- df

facility_coords_df <- facility_coords_df %>%
  left_join(facility_df_raw %>% rename(facility_name = correct_facility_name) %>% select(facility_name, area_id), by = "facility_name")

# ggplot(facility_df_mapping) +
#   geom_sf() +
#   theme_minimal() +
#   ggtitle("Facility Locations in South Africa")

#Read in PrEP uptake by age and gender provided by Tumisho on 25June2025

# Step 2: Keep only relevant columns (facility name + disaggregated initiations)
df_clean <- df_raw %>%
  select(Facility, matches("^F \\d{2}-\\d{2}|^M \\d{2}-\\d{2}"))

# Step 3: Rename columns to a clean format (e.g., F 15-19 b female_15-19)
colnames(df_clean) <- colnames(df_clean) %>%
  str_replace("^F ", "female_") %>%
  str_replace("^M ", "male_") %>%
  str_replace(" ", "")  # remove any remaining spaces

# Step 4: Convert to long format
df_long <- df_clean %>%
  pivot_longer(
    cols = -Facility,
    names_to = "stratum",
    values_to = "prep_initiations"
  ) %>%
  mutate(
    sex = str_extract(stratum, "^(female|male)"),
    age_group_label = str_extract(stratum, "\\d{2}-\\d{2}"),
    prep_initiations = as.numeric(prep_initiations)
  ) %>%
  select(Facility, sex, age_group_label, prep_initiations) %>%
  filter(!is.na(prep_initiations), prep_initiations > 0)

prep_uptake_df_raw <- df_long

#Create incidence_df from the risk distribution district-level data

incidence_df_raw <- risk_dist_targeting_fine_scale %>%
  mutate(quantile_target_factor = paste(quant_target-0.125, "-",quant_target+0.125),
         inc_mult_group = cut(inc_mult, c(0,0.5,1,2,Inf), right=F),
         age_group_label = recode(age_group_label, "50+"="50-99")) %>%
  filter(quant_target %in% c(0.125,0.375,0.625,0.875), sex!="both",age_group_label!="15-49", age_group_label!="15-24")

write.csv(facility_df_raw, "facility_df.csv", row.names = FALSE, na = "")
write.csv(incidence_df_raw, "incidence_df.csv", row.names = FALSE, na = "")
write.csv(prep_uptake_df_raw, "prep_uptake_df.csv", row.names = FALSE, na = "")

rm(list = setdiff(ls(), c("facility_df_raw", "incidence_df_raw", "prep_uptake_df_raw", "facility_coords_df", "district_sf")))

############################################################################
# Set datasets
############################################################################

facility_df = facility_df_raw 
length(unique(facility_df$correct_facility_name)) #3,464 facilities
#nrow(facility_df %>% filter(total_initiations_fy_24_25 > 100)) #919 facilities
#prep_uptake_df = prep_uptake_df_raw
incidence_df = incidence_df_raw