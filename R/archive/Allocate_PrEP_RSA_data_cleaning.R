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
library(fuzzyjoin)
library(ggrepel)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

############################################################################
# Load data files
############################################################################

## Load naomi-based analysis output
load("Len_optim_data_ZAF_extra_ages.RData")

district_sf <- naomi_ssa_shp_m %>%
  filter(iso3 == "ZAF") %>%
  dplyr::select(iso3, area_id, area_name,geometry) %>%
  distinct()

rm(list = setdiff(ls(), c("naomi_ssa_shp_m", "district_sf","risk_dist_targeting_fine_scale","risk_dist_targeting_fine_scale_FULL_PERCENTILES")))

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

# Load NDOH Len 1st phase allocation plan - this is from a presentation called "Lenacapavir Scale-up Updated 12 August 2025 rev" located in the R folder of LenOptim
ndoh_len_plan = read.csv("RSA_OUTPUT/Slide12_Target_Population_Lenacapavir_Corrected.csv")
sum(ndoh_len_plan$Total.Len.Initiations)

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

# Step 3: Rename columns to a clean format (e.g., F 15-19 b female_15-19)
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

incidence_df <- risk_dist_targeting_fine_scale %>%
  mutate(quantile_target_factor = paste(quant_target-0.125, "-",quant_target+0.125),
         inc_mult_group = cut(inc_mult, c(0,0.5,1,2,Inf), right=F),
         age_group_label = recode(age_group_label, "50+"="50-99")) %>%
  filter(quant_target %in% c(0.125,0.375,0.625,0.875), sex!="both",age_group_label!="15-49", age_group_label!="15-24")

incidence_df_quant <- risk_dist_targeting_fine_scale_FULL_PERCENTILES %>%
    mutate(quantile_target_factor = paste(quant_target-1/n_quantiles, "-",quant_target),
           inc_mult_group = cut(inc_mult, c(0,0.5,1,2,5,Inf), right=F),
           age_group_label = recode(age_group_label, "50+"="50-99")) %>%
    filter(sex!="both",age_group_label!="15-49", age_group_label!="15-24")

save(incidence_df_quant, file="incidence_df_quantiles.RData")

# Create RSA KP dataset and merge with gen pop dataset
incidence_df_kp <- data.frame(naomi_ssa_shp_m) %>%
  filter(sex %in% c("both","female"),age_group_label=="15-49") %>%
  select(area_id,sex,age_group_label,incidence,pop_at_risk) %>%
  mutate(group = paste0(sex, "_", age_group_label), incidence=incidence*1000) %>%
  select(area_id, group, incidence) %>%
  pivot_wider(
    names_from = group,
    values_from = incidence
  ) %>%
  mutate(SW_inc = 2*6*`female_15-49`, ANC_inc=1.5*1.35*`female_15-49`, GBMSM_inc = 2*3.75*`both_15-49`, TGW_inc = 2*5.625*`both_15-49`)

df_long_inc_kp <- incidence_df_kp %>%  # replace with actual variable name
  pivot_longer(
    cols = ends_with("_inc"),
    names_to = "sex",
    names_pattern = "(.*)_inc",
    values_to = "inc_in_sample"
  ) %>%
  select(area_id, sex, inc_in_sample) %>%
  mutate(
    sex = toupper(sex),  # optional: make labels match format like "SW", "ANC"
    age_group_label = "15-49",
    iso3 = "ZAF",        # fill in if needed
    inc_district = NA,
    pop_district = NA,
    total_infections_district_age_sex = NA,
    pop_subsample = NA,
    total_infected_subsample = NA,
    infections_averted = NA,
    quantile_target_factor = "0 - 1"
  ) %>%
  select(
    sex, age_group_label, iso3, area_id, inc_district, pop_district,
    total_infections_district_age_sex, pop_subsample, total_infected_subsample,
    inc_in_sample, infections_averted, quantile_target_factor
  )

incidence_df_kp <- bind_rows(incidence_df, df_long_inc_kp)

#summary of new infections and pop at risk by district
district_new_infections = data.frame(naomi_ssa_shp_m) %>% select(area_id, sex, age_group_label, incidence, pop_at_risk) %>%
  filter(age_group_label %in% c("15-19", "20-24", "25-34", "35-49","50+"), sex!="both") %>%
  mutate(new_infections = incidence * pop_at_risk) %>%
  group_by(area_id) %>%
  summarize(new_infections = sum(new_infections), pop_at_risk = sum(pop_at_risk))

# write.csv(facility_df_raw, "facility_df.csv", row.names = FALSE, na = "")
# write.csv(incidence_df_raw, "incidence_df.csv", row.names = FALSE, na = "")
# write.csv(prep_uptake_df_raw, "prep_uptake_df.csv", row.names = FALSE, na = "")

#rm(list = setdiff(ls(), c("ndoh_len_plan",  "naomi_ssa_shp_m", "facility_df_raw", "incidence_df", "incidence_df_kp","prep_uptake_df_raw", "facility_coords_df", "district_sf", "district_new_infections")))

############################################################################
# Set datasets
############################################################################

facility_df = facility_df_raw 
length(unique(facility_df$correct_facility_name)) #3,464 facilities

facility_df_district_names = facility_df %>%
  select(area_id, district, province) %>%
  distinct(.keep_all = TRUE) %>%
  mutate(district = str_remove_all(district, regex("(?i)\\b(Municipality|Metropolitan|District)\\b"))) %>%
  mutate(district = str_squish(district))  # removes extra spaces

age_groups = c("15-19", "20-24", "25-34", "35-49","50+")

# 2. Create a facility dataframe that is a place-holder for analysis with only districts (no facilities included)
facility_df_place_holder <- data.frame(naomi_ssa_shp_m) %>%
  select(area_id, sex, age_group_label, pop_at_risk) %>%
  filter(sex != "both", age_group_label %in% c("15-19", "20-24", "25-34", "35-49","50+")) %>%
  mutate(
    sex_abbrev = ifelse(sex == "female", "f", "m"),
    age_group_label = gsub("-", " - ", age_group_label),
    new_col = paste0(sex_abbrev, "_", age_group_label)
  ) %>%
  select(area_id, new_col, pop_at_risk) %>%  # <-- Only keep columns needed for pivoting
  pivot_wider(names_from = new_col, values_from = pop_at_risk) %>%
  mutate(total_initiations = 0, facility_name = paste("district_",area_id,sep=""), latitude=NA, longitude=NA) %>%
  left_join(facility_df_district_names, by="area_id")

ndoh_len_plan_merged <- stringdist_join(ndoh_len_plan, facility_df_district_names,
                                by = c("District" = "district"),
                                mode = "left",  # or "inner", "right"
                                method = "jw",  # Jaro-Winkler similarity
                                max_dist = 0.3,  # adjust based on your tolerance
                                distance_col = "dist") %>%
  select(District, district, dist, everything()) %>%
  group_by(District) %>%
  slice_min(order_by = dist, n = 1) %>%
  ungroup() %>%
  mutate(district = if_else(District == "Ugu", "kz Ugu", district),
         area_id = if_else(District == "Ugu", "ZAF_2_DC21", area_id),
         province = if_else(District == "Ugu", "kz KwaZulu-Natal Province", province)) %>%
  select(-c(dist, Province, District, province, district)) %>%
  left_join(facility_df_place_holder, by="area_id") %>%
  select(district, area_id, everything())

ndoh_len_plan_investigate = ndoh_len_plan_merged %>%
  mutate(sum_Len = rowSums(across(c(
    X15..Len.Initiations,
    ANC.Len.Initiations,
    SW.Len.Initiations,
    GBMSM.Len.Initiations,
    TG.Len.Initiations
  )), na.rm = TRUE)) %>%
  mutate(across(
    c(X15..Len.Initiations, ANC.Len.Initiations, SW.Len.Initiations,
      GBMSM.Len.Initiations, TG.Len.Initiations),
    ~ .x / sum_Len,
    .names = "{.col}_prop"
  )) %>%
  left_join(naomi_ssa_shp_m %>% filter(age_group_label=="15-49", sex=="female") %>% select(area_id, incidence) %>% mutate(incidence_1549 = incidence) %>% select(-incidence), by="area_id")

library(forcats)

df <- ndoh_len_plan_investigate %>%
  mutate(district = fct_reorder(district, -X15..Len.Initiations_prop))

df_long <- df %>%
  pivot_longer(
    cols = c(X15..Len.Initiations_prop, ANC.Len.Initiations_prop, SW.Len.Initiations_prop,
             GBMSM.Len.Initiations_prop, TG.Len.Initiations_prop),
    names_to = "Initiation_Type",
    values_to = "Count"
  )

# Step 2: Plot
ndoh_len_plan_plot = ggplot(df_long, aes(x = district, y = Count, fill = Initiation_Type)) +
  geom_col(position = "stack") +
  coord_flip() +
  labs(
    title = "Distribution of Lenacapavir Initiations by District (Ordered by % X15+)",
    x = "District",
    y = "Proportion of Total Initiations",
    fill = "Initiation Type"
  ) +
  theme_minimal() +
  ggtitle("NDoH Proposed District-level allocation of Lenacapavir initiations by risk group") +
  scale_fill_manual(
    name = "Population",
    values = c(
      "SW.Len.Initiations_prop"    = "#D55E00",  # Female sex workers
      "GBMSM.Len.Initiations_prop" = "#0072B2",  # Gay and bi-sexual men who have sex with men
      "TG.Len.Initiations_prop"    = "#CC79A7",  # Transgender women
      "ANC.Len.Initiations_prop"   = "#009E73",  # Ante-natal care
      "X15..Len.Initiations_prop"  = "#F0E442"   # General population
    ),
    labels = c(
      "SW.Len.Initiations_prop"    = "Female sex workers",
      "GBMSM.Len.Initiations_prop" = "Gay and bi-sexual men who have sex with men",
      "TG.Len.Initiations_prop"    = "Transgender women",
      "ANC.Len.Initiations_prop"   = "Antenatal care population",
      "X15..Len.Initiations_prop"  = "General population"
    )
  )

ndoh_len_plan_plot_scatter = ggplot(df, aes(x = incidence_1524*1000, y = X15..Len.Initiations_prop)) +
  geom_point(color = "steelblue", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "darkred", linetype = "dashed") +
  geom_text_repel(aes(label = district), size = 3, max.overlaps = 50) +
  geom_hline(yintercept = 0, color = "black") +
  geom_vline(xintercept = 0, color = "black") +
  labs(
    title = "Proportion of 15+ Len Initiations vs. HIV Incidence by District",
    x = "HIV Incidence (per 1000 py)",
    y = "Proportion 15+ initiations"
  ) +
  theme_minimal()

#nrow(facility_df %>% filter(total_initiations_fy_24_25 > 100)) #919 facilities
#prep_uptake_df = prep_uptake_df_raw

save(ndoh_len_plan_plot, ndoh_len_plan_plot_scatter, file = "RSA_OUTPUT/extra_plots.RData")



