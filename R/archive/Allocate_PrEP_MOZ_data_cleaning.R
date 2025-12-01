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
facility_df <- read_csv("facility_data_MOZ_ec_v2.csv", locale = locale(encoding = "ISO-8859-1"))
district_df = as.data.frame(district_sf)[,c(1:3)]

# 1. Write CSV to a UTF-8 string
csv_content <- capture.output(write.csv(district_df, row.names = FALSE, file = ""))

# 2. Combine lines into a single UTF-8 string
csv_utf8 <- paste(csv_content, collapse = "\r\n")

# 3. Encode as bytes with BOM prefix
csv_bytes <- c(charToRaw("\xEF\xBB\xBF"), charToRaw(csv_utf8))  # BOM + UTF-8 content

# 4. Write to file
output_path <- "MOZ_district_df_UTF8_BOM.csv"
con <- file(output_path, open = "wb")  # binary write mode
writeBin(csv_bytes, con)
close(con)

############################################################################

# 2. Normalize names
normalize <- function(x) {
  stri_trans_general(tolower(trimws(x)), "Latin-ASCII")
}

facility_df <- facility_df %>%
  mutate(
    district_facility = district,
    district_lower = normalize(district_facility),
    district_mapped = case_when(
      stri_detect_fixed(district_facility, "\u00e7") ~ "Manhi\u00e7a",
      district_lower == "manica" ~ "Manhica",
      TRUE ~ str_to_title(district_facility)
    ),
    district_mapped_lower = normalize(district_mapped)
  )

district_df <- district_df %>%
  mutate(area_name_lower = normalize(area_name))

# 3. Exact match
exact_match_df <- facility_df %>%
  left_join(district_df, by = c("district_mapped_lower" = "area_name_lower"))

# 4. Get unmatched for further processing
unmatched_df <- exact_match_df %>%
  filter(is.na(area_id)) %>%
  distinct(district_facility)

# Re-create normalized unmatched and reference columns
unmatched_df <- unmatched_df %>%
  mutate(district_norm = normalize(district_facility))

district_ref <- district_df %>%
  mutate(area_name_norm = normalize(area_name)) %>%
  select(area_name, area_name_norm)

# Suggest best match with score
suggestions <- unmatched_df %>%
  rowwise() %>%
  mutate(
    suggestion = {
      dist_vec <- stringdist::stringdist(district_norm, district_ref$area_name_norm, method = "lv")
      best_i <- which.min(dist_vec)
      district_ref$area_name[best_i]
    },
    similarity = {
      best_i <- which.min(stringdist::stringdist(district_norm, district_ref$area_name_norm, method = "lv"))
      best_dist <- stringdist::stringdist(district_norm, district_ref$area_name_norm[best_i], method = "lv")
      max_len <- max(nchar(district_norm), nchar(district_ref$area_name_norm[best_i]), 1)
      1 - (best_dist / max_len)
    }
  ) %>%
  ungroup()

# 6. Accept high-confidence matches (≥ 0.88) + manual additions
approved_matches <- suggestions %>%
  filter(similarity >= 0.88 |
           district_facility %in% c("Cidade de Inhambane", "Cidade de Quelimane")) %>%
  select(district_facility, district_mapped_final = suggestion)

# 7. Apply manual mapping to facility_df
facility_df <- facility_df %>%
  left_join(approved_matches, by = "district_facility") %>%
  mutate(
    district_mapped_final = coalesce(district_mapped_final, district_mapped)  # fallback to prior logic
  )

# 8. Final join to reference table
final_facility_df <- facility_df %>%
  left_join(district_df, by = c("district_mapped_final" = "area_name"))

# 9. Check for unmatched
unmatched_final <- final_facility_df %>%
  filter(is.na(area_id)) %>%
  distinct(district_facility)

# ✅ Final outputs
print("✅ Final joined facility data:")
print(final_facility_df %>% select(district_facility, district_mapped_final, area_name, area_id))

print("⚠️ Remaining unmatched facility districts:")
print(unmatched_final)



############################################################################
# Clean datasets
############################################################################

# Clean column names
df <- df %>%
  janitor::clean_names()

df <- df %>%
  left_join(as.data.frame(district_sf), join_by(district)) %>%
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