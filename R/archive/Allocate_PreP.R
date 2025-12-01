# Function to take a dataframe and allocate Len according to the population at risk size,
# the previous coverage of oral prep, and incidence rate. It constrains the number of units
# of Len to a total defined.

#sets the directory to where this script is stored#
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(readr)
library(janitor)
library(DT)
library(sf)

## Facility analysis
load("Len_optim_data_FULL_COUNTRY_LIST.RData")

# Read the CSV
df <- read_delim("LEN Quantification Perf Review 24.06.2025_With Geo-Coordinates 24.06.2024.csv",
                 delim = ",",
                 locale = locale(encoding = "Latin1"),
                 guess_max = 5000)  # ensures full type guessing

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
char_cols <- df %>% select(where(is.character))
likely_numeric <- sapply(char_cols, function(col) {
  sum(grepl("[0-9]", col)) / length(col) > 0.5  # more than half look numeric
})

# Apply numeric cleaning only to those
df[ , names(likely_numeric[likely_numeric])] <- 
  lapply(df[ , names(likely_numeric[likely_numeric])], safe_numeric)

# Check result
ncol(df)

district_names <- read.csv('District Names.csv') %>% #names to merge facility to NAOMI datasets
  select(area_id,District) %>%
  rename(district=District)

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
  select(province, district, sub_district, correct_facility_name, Latitude, Longitude)

ggplot(facility_df_mapping) +
  geom_sf() +
  theme_minimal() +
  ggtitle("Facility Locations in South Africa")

incidence_df <- risk_dist_targeting_fine_scale %>%
  mutate(quantile_target_factor = paste(quant_target-0.125, "-",quant_target+0.125),
       inc_mult_group = cut(inc_mult, c(0,0.5,1,2,Inf), right=F),
       age_group_label = recode(age_group_label, "50+"="50-99")) %>%
  filter(quant_target %in% c(0.125,0.375,0.625,0.875), sex!="both",age_group_label!="15-49", age_group_label!="15-24")

write.csv(facility_df, "facility_df.csv", row.names = FALSE, na = "")
write.csv(incidence_df, "incidence_df.csv", row.names = FALSE, na = "")

facility_df <- df 

allocate_prep_by_risk <- function(facility_df, incidence_df,
                                  total_units = 1e6,
                                  efficacy = 0.95,
                                  cost_per_unit = 130,
                                  dalys_per_infection = 20,
                                  demand_multiplier = 2) {
  library(dplyr)
  library(tidyr)
  library(stringr)
  
  # Step 1: Minimal facility fields
  facility_df <- facility_df %>%
    select(
      area_id,
      correct_facility_name,
      district,
      total_initiations,
      starts_with("population_f_"),
      starts_with("population_m_")
    ) %>%
    rename(
      facility_name = correct_facility_name,
      District = district,
      Total.Initiations = total_initiations
    )
  
  # Step 2: Pivot population
  pop_cols <- names(facility_df)[grepl("^population_[fm]_", names(facility_df))]
  
  df_long <- facility_df %>%
    pivot_longer(cols = all_of(pop_cols), names_to = "group", values_to = "population") %>%
    mutate(
      sex = case_when(
        grepl("population_f_", group) ~ "female",
        grepl("population_m_", group) ~ "male",
        TRUE ~ NA_character_
      ),
      age_group_label = case_when(
        grepl("15_19", group) ~ "15-19",
        grepl("20_24", group) ~ "20-24",
        grepl("25_34", group) ~ "25-34",
        grepl("35_49", group) ~ "35-49",
        grepl("50", group) ~ "50-99",
        grepl("15$", group) ~ "15-99",
        TRUE ~ NA_character_
      ),
      population = as.numeric(population)
    ) %>%
    group_by(area_id, facility_name) %>%
    mutate(
      total_population = sum(population, na.rm = TRUE),
      prob_prep_use_overall = ifelse(total_population > 0,
                                     first(Total.Initiations) / total_population,
                                     0)
    ) %>%
    ungroup()
  
  # Step 3: Cross join with risk strata
  quantiles <- unique(as.character(incidence_df$quantile_target_factor))
  
  df_expanded <- df_long %>%
    crossing(quantile_target_factor = quantiles) %>%
    mutate(population = population / 4)
  
  # Step 4: Merge incidence
  incidence_df <- incidence_df %>%
    mutate(
      sex = tolower(sex),
      age_group_label = trimws(age_group_label),
      quantile_target_factor = as.character(quantile_target_factor)
    )
  
  df_merged <- df_expanded %>%
    left_join(
      incidence_df %>%
        select(area_id, sex, age_group_label, inc_in_sample, quantile_target_factor),
      by = c("area_id", "sex", "age_group_label", "quantile_target_factor")
    ) %>%
    filter(!is.na(inc_in_sample), !is.na(population))
  
  # Step 5: Compute district incidence
  district_incidence <- incidence_df %>%
    group_by(area_id) %>%
    summarise(
      district_incidence = weighted.mean(inc_in_sample, pop_subsample, na.rm = TRUE),
      .groups = "drop"
    )
  
  df_merged <- df_merged %>%
    left_join(district_incidence, by = "area_id") %>%
    mutate(
      raw_weight = inc_in_sample / district_incidence,
      clipped_weight = pmin(pmax(raw_weight, 0.1), 2)
    )
  
  # Step 6: Compute lambda & prob_prep_use (capped & scaled by demand)
  df_result <- df_merged %>%
    group_by(area_id, facility_name) %>%
    group_modify(~{
      total_pop <- sum(.x$population, na.rm = TRUE)
      prob_overall <- .x$prob_prep_use_overall[1]
      denom <- sum(.x$clipped_weight * .x$population, na.rm = TRUE)
      if (denom == 0) return(.x)
      lambda <- prob_overall * total_pop / denom
      .x$lambda_factor <- lambda
      .x$prob_prep_use_raw <- lambda * .x$clipped_weight
      .x$prob_prep_use <- pmin(1, .x$prob_prep_use_raw * demand_multiplier)
      .x
    }) %>%
    ungroup()
  
  # Step 7: Compute units_needed with floor & minimum of 1
  df_result <- df_result %>%
    mutate(
      units_needed = ifelse(
        population > 0 & prob_prep_use > 0,
        pmax(1, floor(population * prob_prep_use)),
        0
      ),
      allocated_units = 0
    ) %>%
    arrange(desc(units_needed))
  
  # Step 8: Estimate potential infections averted & cost-effectiveness
  df_result <- df_result %>%
    mutate(
      infections_potential = units_needed * inc_in_sample / 1000 * efficacy,
      cost_total = units_needed * cost_per_unit,
      cost_per_daly = ifelse(infections_potential > 0,
                             cost_total / (infections_potential * dalys_per_infection),
                             Inf)
    ) %>%
    # Step 9: Only allow allocation where cost per DALY ≤ $500
    mutate(
      units_needed = ifelse(cost_per_daly <= 500, units_needed, 0)
    )
  
  # Step 10: Allocation loop
  remaining <- suppressWarnings(as.numeric(total_units))
  if (is.null(remaining) || is.na(remaining) || !is.finite(remaining)) remaining <- 0
  
  for (i in seq_len(nrow(df_result))) {
    if (!is.finite(remaining) || remaining <= 0) break
    alloc <- min(df_result$units_needed[i], remaining)
    df_result$allocated_units[i] <- alloc
    remaining <- remaining - alloc
  }
  
  # Step 11: Final infections averted (based on actual allocation)
  df_result <- df_result %>%
    mutate(
      infections_averted = allocated_units * inc_in_sample / 1000 * efficacy
    )
  
  return(df_result)
}

result <- allocate_prep_by_risk(
  facility_df = facility_df,
  incidence_df = incidence_df,
  total_units = 1e6,
  efficacy = 0.95,
  cost_per_unit = 130,
  dalys_per_infection = 20,
  demand_multiplier = 10
)

head(result)


summarize_prep_allocation <- function(result,
                                      cost_per_unit = 130,
                                      dalys_per_infection = 20) {
  library(dplyr)
  library(tidyr)
  
  # ---- 1. Recode risk strata for display ----
  result <- result %>%
    mutate(`Risk quartile` = recode(quantile_target_factor,
                                    "0 - 0.25" = "<25%",
                                    "0.25 - 0.5" = "25-50%",
                                    "0.5 - 0.75" = "50-75%",
                                    "0.75 - 1" = "75-100%"
    ))
  
  # ---- 2. Summary Report ----
  summary_report <- result %>%
    summarise(
      total_allocated_units = sum(allocated_units, na.rm = TRUE),
      total_infections_averted = sum(infections_averted, na.rm = TRUE),
      total_dalys_averted = total_infections_averted * dalys_per_infection,
      total_cost = total_allocated_units * cost_per_unit,
      cost_per_infection_averted = total_cost / total_infections_averted,
      cost_per_daly_averted = total_cost / total_dalys_averted,
      n_facilities_allocated = n_distinct(facility_name[allocated_units > 0]),
      total_targeted_individuals = total_allocated_units
    )
  
  # ---- 3. Stratified Summary: Age × Sex × Risk ----
  stratified_summary <- result %>%
    filter(allocated_units > 0) %>%
    group_by(age_group_label, sex, `Risk quartile`) %>%
    summarise(
      targeted_individuals = sum(allocated_units, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(sex, age_group_label, `Risk quartile`)
  
  # ---- 4. Facility Allocation (Long) ----
  facility_age_gender_allocation <- result %>%
    filter(allocated_units > 0) %>%
    group_by(facility_name, area_id, District, age_group_label, sex) %>%
    summarise(
      allocated_units = sum(allocated_units, na.rm = TRUE),
      .groups = "drop"
    )
  
  # ---- 5. Facility Allocation (Wide with Total Column) ----
  # Specify desired age group column order
  age_order <- c("15-19", "20-24", "25-34", "35-49")
  
  # Reorder columns manually after pivot
  facility_age_gender_allocation_wide <- facility_age_gender_allocation %>%
    pivot_wider(
      names_from = age_group_label,
      values_from = allocated_units,
      values_fill = 0
    ) %>%
    mutate(
      total_allocated = rowSums(across(where(is.numeric))),
      sex = factor(sex, levels = c("male", "female"))
    ) %>%
    relocate(any_of(age_order), .after = sex) %>%
    arrange(facility_name, sex)
  # ---- Return All ----
  return(list(
    summary_report = summary_report,
    stratified_summary = stratified_summary,
    facility_age_gender_allocation = facility_age_gender_allocation,
    facility_age_gender_allocation_wide = facility_age_gender_allocation_wide
  ))
}


report_output <- summarize_prep_allocation(result)

report_output$summary_report
#report_output$stratified_summary
report_output$facility_age_gender_allocation_wide

library(knitr)
library(kableExtra)

report_output$summary_report %>%
  kable(digits = 0, format = "html", caption = "Summary of PrEP Allocation") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

report_output$facility_age_gender_allocation_wide %>%
  slice_head(n = 20) %>%
  kable(digits = 0, format = "html", caption = "Facility Allocation by Age and Sex (Top 10)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

# Merge in coordinates from original facility_df
map_df <- result %>%
  left_join(facility_df_mapping %>%
              rename(facility_name = correct_facility_name),
            by = "facility_name") %>%
  mutate(allocated_facility = ifelse(allocated_units>0, 1, 0))

map_df <- st_as_sf(map_df, coords = c("Longitude", "Latitude"), crs = 4326) 

# Basic map of allocated facilities
library(ggplot2)
ggplot(map_df, aes(y = Latitude, x = Longitude)) +
  geom_point(aes(color = factor(allocated_facility))) +
  #scale_size_continuous(name = "Allocated PrEP units") +
  #scale_color_viridis_c(option = "C", name = "Allocated PrEP units") +
  theme_minimal() +
  labs(title = "Facilities Receiving PrEP Allocation",
       x = "Longitude", y = "Latitude")

map_df <- result %>%
  group_by(facility_name) %>%
  summarise(allocated_units = sum(allocated_units, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    facility_df_mapping %>%
      select(facility_name = correct_facility_name, Latitude, Longitude),
    by = "facility_name"
  ) %>%
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  rename(latitude=Latitude, longitude=Longitude)  %>%
  mutate(
    allocated_flag = ifelse(allocated_units > 0, "Yes", "No")
  )

map_sf <- st_as_sf(
  map_df,
  coords = c("longitude", "latitude"),
  crs = 4326  # WGS84
)

# Step 2: Plot map
ggplot() +
  geom_sf(data = africa_adm0_cropped_subs, color = "black", fill = NA, linewidth = 0.8) +
  geom_sf(data = map_sf, aes(color = allocated_flag), size = 1.5, alpha = 0.9) +
  scale_color_manual(values = c("No" = "grey60", "Yes" = "red")) +
  theme_minimal() +
  labs(
    title = "Facilities Receiving PrEP Allocation",
    color = "Allocated PrEP?"
  )

#Interogate facility allocation
missed_opportunities <- result %>%
  group_by(facility_name, area_id) %>%
  summarise(
    cost_effective_units = sum(units_needed[cost_per_daly <= 500], na.rm = TRUE),
    allocated_units = sum(allocated_units, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(cost_effective_units > 0 & allocated_units == 0)
