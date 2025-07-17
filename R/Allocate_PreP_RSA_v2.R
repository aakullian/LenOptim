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

############################################################################3
# Load data files
############################################################################3

## Load naomi-based analysis output
load("Len_optim_data_FULL_COUNTRY_LIST.RData")

district_sf <- naomi_ssa_shp_m %>%
  filter(iso3 == "ZAF") %>%
  select(iso3, area_id, area_name,geometry)

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

############################################################################3
# Clean datasets
############################################################################3

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

######

facility_df = facility_df_raw 
length(unique(facility_df$correct_facility_name)) #3,464 facilities
nrow(facility_df %>% filter(total_initiations_fy_24_25 > 100)) #919 facilities
#prep_uptake_df = prep_uptake_df_raw
incidence_df = incidence_df_raw


allocate_prep_by_risk_with_stratified_prob <- function(facility_df,
                                                       incidence_df,
                                                       budget,
                                                       cost_per_unit = 130,
                                                       min_total_initiations = 100,
                                                       #prep_uptake_df = NULL,
                                                       efficacy = 0.95,
                                                       dalys_per_infection = 20,
                                                       coverage_mult=1) {
  library(dplyr)
  library(tidyr)
  library(stringr)
  
  total_units <- floor(budget / cost_per_unit)
  
  # Step 1: Filter and reshape facility data
  facility_df <- facility_df %>%
    dplyr::select(
      area_id,
      district,
      correct_facility_name,
      total_initiations = total_initiations_fy_24_25,
      starts_with("population_f_"),
      starts_with("population_m_")
    ) %>%
    rename(facility_name = correct_facility_name) %>%
    filter(total_initiations > min_total_initiations)
  
  pop_cols <- names(facility_df)[grepl("^population_[fm]_", names(facility_df))]
  
  df_long <- facility_df %>%
    pivot_longer(cols = all_of(pop_cols), names_to = "group", values_to = "catchment_population_group") %>%
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
        grepl("50", group)    ~ "50-99",
        grepl("15$", group)   ~ "15-99",
        TRUE ~ NA_character_
      ),
      catchment_population_group = as.numeric(catchment_population_group)
    ) %>%
    filter(!is.na(sex), !is.na(age_group_label)) %>%
    group_by(area_id, facility_name) %>%
    mutate(total_catchment_population = sum(catchment_population_group, na.rm = TRUE)) %>%
    ungroup()
  
  # Step 2: Cross with risk quantiles
  quantiles <- unique(as.character(incidence_df$quantile_target_factor))
  df_expanded <- df_long %>%
    crossing(quantile_target_factor = quantiles) %>%
    mutate(catchment_population_risk_strata = catchment_population_group / 4)
  
  # Step 3: Merge incidence
  incidence_df <- incidence_df %>%
    mutate(
      sex = tolower(sex),
      age_group_label = trimws(age_group_label),
      quantile_target_factor = as.character(quantile_target_factor)
    )
  
  df_merged <- df_expanded %>%
    left_join(
      incidence_df %>% select(area_id, sex, age_group_label, inc_in_sample, quantile_target_factor, inc_district),
      by = c("area_id", "sex", "age_group_label", "quantile_target_factor")
    ) %>%
    mutate(inc_in_sample_cat = round(inc_in_sample, 0)) %>%
    filter(!is.na(inc_in_sample), !is.na(catchment_population_group)) %>%
    mutate(allocated_units = 0)
  
  # --- Method A: Ranked population strategy ---
    df_result <- df_merged %>%
      arrange(desc(inc_in_sample), desc(total_initiations), facility_name, sex, age_group_label) %>%
      mutate(allocated_units = 0)
    
    remaining_units <- total_units
    
    df_result <- df_result %>%
          mutate(priority = row_number()) %>%
          mutate(cum_pop = cumsum(catchment_population_risk_strata)) %>%
          mutate(units_needed = ceiling(coverage_mult * catchment_population_risk_strata))
        
    # Allocate PrEP
      df_result <- df_result %>%
        mutate(units_needed = ifelse(is.na(units_needed), 0, units_needed)) %>%
        mutate(
          cum_units = cumsum(units_needed),
          units_this_round = ifelse(
            units_needed > 0,
            pmin(units_needed, pmax(0, remaining_units - dplyr::lag(cum_units, default = 0))),
            0
          ),
          allocated_units = allocated_units + units_this_round
        ) 
        #%>% select(-units_needed, -cum_units, -units_this_round)
      
      remaining_units <- total_units - sum(df_result$allocated_units, na.rm = TRUE)
      #if (remaining_units <= 0) break

  
  # Final: estimate infections averted
  df_result <- df_result %>%
    mutate(infections_averted = allocated_units * inc_in_sample / 1000 * efficacy)
  
  return(df_result)
}


summarize_allocation_scenarios <- function(results_list,
                                           incidence_df,
                                           budgets,
                                           cost_per_units,
                                           coverage_mults,
                                           scenario_labels = NULL,
                                           dalys_per_infection = 20,
                                           efficacy = 0.95) {
  library(dplyr)
  library(tibble)
  library(tidyr)
  
  if (is.null(scenario_labels)) {
    scenario_labels <- paste0("Scenario_", seq_along(results_list))
  }
  
  # Internal function: create facility summary by age/sex
  create_facility_summary_by_age_sex <- function(result_df, age_order = c("15-19", "20-24", "25-34", "35-49", "50-99")) {
    result_df %>%
      group_by(facility_name, sex, age_group_label) %>%
      summarise(
        catchment_population = sum(catchment_population_risk_strata, na.rm = TRUE),
        prep_units = sum(allocated_units, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        prep_coverage = round((prep_units / catchment_population)*100, 1),
        age_group_label = factor(age_group_label, levels = age_order)
      ) %>%
      arrange(facility_name, sex, age_group_label)
  }
  
  summaries <- lapply(seq_along(results_list), function(i) {
    result <- results_list[[i]]
    cost <- cost_per_units[[i]]
    budget <- budgets[[i]]
    coverage_mult <- coverage_mults[[i]]
    
    expected_infections <- sum(incidence_df$inc_district * incidence_df$pop_subsample / 1000, na.rm = TRUE)
    total_population <- sum(incidence_df$pop_subsample, na.rm = TRUE)
    
    total_allocated_units <- sum(result$allocated_units, na.rm = TRUE)
    infections_averted <- sum(result$allocated_units * result$inc_in_sample / 1000 * efficacy, na.rm = TRUE)
    prep_coverage <- (total_allocated_units / total_population)*100
    total_cost <- total_allocated_units * cost
    cost_per_infection_averted <- total_cost / infections_averted
    total_dalys_averted <- infections_averted * dalys_per_infection
    cost_per_daly_averted <- total_cost / total_dalys_averted
    percent_reduction <- (infections_averted / expected_infections)*100
    number_needed_to_treat <- cost_per_infection_averted / cost
    facilities_allocated <- length(unique(result$facility_name[result$allocated_units > 0]))
    
    avg_incidence_allocated <- weighted.mean(result$inc_in_sample[result$allocated_units > 0],
                                             w = result$allocated_units[result$allocated_units > 0], na.rm = TRUE)
    avg_incidence_population <- weighted.mean(incidence_df$inc_district,
                                              w = incidence_df$pop_district, na.rm = TRUE)
    incidence_targeting_ratio <- avg_incidence_allocated / avg_incidence_population
    
    by_age_sex <- result %>%
      filter(allocated_units > 0) %>%
      group_by(sex, age_group_label) %>%
      summarise(units = sum(allocated_units, na.rm = TRUE), .groups = "drop") %>%
      mutate(percent = round(100 * units / total_allocated_units, 1)) %>%
      arrange(sex, age_group_label)
    
    breakdown_label <- paste(by_age_sex$sex, by_age_sex$age_group_label, "=", by_age_sex$percent, "%", collapse = "; ")
    
    list(
      summary = tibble(
        scenario = scenario_labels[[i]],
        budget = budget,
        cost_per_unit = cost,
        coverage_mult = coverage_mult,
        total_allocated_units = total_allocated_units,
        expected_infections_no_prep = expected_infections,
        infections_averted = infections_averted,
        percent_reduction_in_incidence = percent_reduction,
        prep_coverage_percent = prep_coverage,
        total_dalys_averted = total_dalys_averted,
        total_cost = total_cost,
        cost_per_infection_averted = cost_per_infection_averted,
        cost_per_daly_averted = cost_per_daly_averted,
        number_needed_to_treat = number_needed_to_treat,
        facilities_with_allocation = facilities_allocated,
        avg_incidence_allocated = avg_incidence_allocated,
        avg_incidence_population = avg_incidence_population,
        incidence_targeting_ratio = incidence_targeting_ratio,
        allocation_by_age_sex = breakdown_label
      ),
      facility_summary = create_facility_summary_by_age_sex(result)
    )
  })
  
  summary_df <- bind_rows(lapply(summaries, `[[`, "summary"))
  facility_tables <- lapply(summaries, `[[`, "facility_summary")
  
  return(list(
    summary = summary_df,
    facility_summaries = facility_tables
  ))
}


## Run models ##

run_cost_and_demand_scenarios <- function(cost_per_unit_vec,
                                          budget_vec,
                                          coverage_mult_vec,
                                          facility_df,
                                          incidence_df,
                                          #prep_uptake_df,
                                          scenario_labels = NULL,
                                          efficacy = 0.95,
                                          dalys_per_infection = 20) {
  library(dplyr)
  library(tidyr)
  
  # Create full grid of scenarios
  scenario_grid <- crossing(
    budget = budget_vec,
    cost_per_unit = cost_per_unit_vec,
    coverage_mult = coverage_mult_vec
  ) %>%
    mutate(
      scenario_id = row_number(),
      scenario_label = if (!is.null(scenario_labels)) scenario_labels else
        paste0("Budget $", budget / 1e6, "M @ $", cost_per_unit, " (", coverage_mult * 100, "%)")
    )
  
  # Run allocation for each combination
  results_list <- lapply(seq_len(nrow(scenario_grid)), function(i) {
    total_units <- floor(scenario_grid$budget[i] / scenario_grid$cost_per_unit[i])
    
    allocate_prep_by_risk_with_stratified_prob(
      facility_df = facility_df,
      incidence_df = incidence_df,
      #prep_uptake_df = prep_uptake_df,
      cost_per_unit = scenario_grid$cost_per_unit[i],
      budget = scenario_grid$budget[i],
      coverage_mult = scenario_grid$coverage_mult[i],
      efficacy = efficacy
    )
  })
  
  summary_df <- summarize_allocation_scenarios(
    results_list = results_list,
    incidence_df = incidence_df,
    budgets = scenario_grid$budget,
    cost_per_units = scenario_grid$cost_per_unit,
    coverage_mults = scenario_grid$coverage_mult,
    scenario_labels = scenario_grid$scenario_label,
    dalys_per_infection = dalys_per_infection,
    efficacy = efficacy
  )
  
  # summary_df$summary <- summary_df$summary %>%
  #   left_join(scenario_grid %>% select(scenario_label, coverage_mult), by = "scenario_label")
  
  return(list(
    summary = summary_df$summary,  # not summary_result itself
    facility_summaries = summary_df$facility_summaries,
    results_list = results_list,
    scenario_grid = scenario_grid
  ))
}

# Select and summarize results by budget and cost
select_scenario_result <- function(output, budget_value, cost_value, coverage_mult_value){
  # Find the matching index
  idx <- which(output$scenario_grid$budget == budget_value &
                 output$scenario_grid$cost_per_unit == cost_value &
                    output$scenario_grid$coverage_mult == coverage_mult_value)
  
  if (length(idx) != 1) {
    stop("Scenario not uniquely identified.")
  }
  
  list(
    result_df = output$results_list[[idx]],
    summary_row = output$summary[idx, ],
    index = idx
  )
}

map_prep_allocation_scenario <- function(results_list,
                                         scenario_grid,
                                         index,
                                         facility_coords,
                                         district_shapefile,
                                         pop_column = "catchment_population_risk_strata") {
  library(dplyr)
  library(ggplot2)
  library(sf)
  library(viridis)
  
  result_df <- results_list[[index]]
  cost <- scenario_grid$cost_per_unit[index]
  budget <- scenario_grid$budget[index]
  label <- paste0("")
  
  # 1. Facility-level allocation
  facility_map_df <- facility_coords %>%
    left_join(
      result_df %>%
        group_by(facility_name, area_id) %>%
        summarise(allocated = sum(allocated_units, na.rm = TRUE) > 0, .groups = "drop"),
      by = c("facility_name", "area_id")
    ) %>%
    mutate(allocated = ifelse(is.na(allocated), FALSE, allocated))
  
  p_facilities <- ggplot(facility_map_df, aes(x = longitude, y = latitude)) +
    geom_sf(data = district_shapefile, fill = NA, color = "black", inherit.aes = FALSE) +
    geom_point(aes(color = allocated), size = 1, alpha = 0.3) +
    scale_color_manual(values = c("blue", "red"), labels = c("No", "Yes")) +
    coord_sf() +
    labs(
      title = "Facilities Receiving PrEP Allocation",
      color = "Allocated PrEP"
    ) +
    theme_minimal()
  
  # 2. % of facilities with PrEP by district
  district_facility_summary <- facility_map_df %>%
    group_by(area_id) %>%
    summarise(
      n_facilities = n(),
      n_allocated = sum(allocated),
      percent_allocated = 100 * n_allocated / n_facilities,
      .groups = "drop"
    ) %>%
    mutate(allocated_yn = ifelse(percent_allocated==0,0,1))
  
  district_map1 <- left_join(district_shapefile, district_facility_summary, by = "area_id")
  
  p_district_facilities <- ggplot(district_map1) +
    geom_sf(aes(fill = as.factor(allocated_yn)), color = "white") +
    scale_fill_manual(values=c("blue","red"),labels=c("No","Yes"),  name = "Districts with PrEP allocation", na.value = "grey90") +
    labs(
      title = "% Facilities Allocated PrEP"
    ) +
    theme_minimal()
  
  # 3. % of population covered by PrEP by district
  district_pop_summary <- result_df %>%
    group_by(area_id) %>%
    summarise(
      pop_total = sum(.data[[pop_column]], na.rm = TRUE),
      units_allocated = sum(allocated_units, na.rm = TRUE),
      percent_pop_covered = 100 * units_allocated / pop_total,
      .groups = "drop"
    ) %>%
    mutate(percent_pop_covered=ifelse(percent_pop_covered==0,NA,percent_pop_covered))
   
  district_map2 <- left_join(district_shapefile, district_pop_summary, by = "area_id")
  
  p_district_pop <- ggplot(district_map2) +
    geom_sf(aes(fill = percent_pop_covered), color = "white") +
    scale_fill_viridis_c(name = "% Catchment Pop Covered", na.value = "grey90") +
    labs(
      title = "% Catchment Population Covered by PrEP"
    ) +
    theme_minimal()
  
  return(list(
    facility_point_map = p_facilities,
    district_facility_choropleth = p_district_facilities,
    district_population_choropleth = p_district_pop
  ))
}


format_prep_allocation_maps <- function(maps, result_df, label_text = NULL) {
  library(dplyr)
  library(patchwork)
  library(ggplot2)
  
  # Compute annotation stats
  facility_stats <- facility_coords_df %>%
    left_join(
      result_df %>%
        group_by(facility_name) %>%
        summarise(allocated = sum(allocated_units, na.rm = TRUE) > 0),
      by = "facility_name"
    ) %>%
    mutate(allocated = ifelse(is.na(allocated), FALSE, allocated))
  
  n_allocated <- sum(facility_stats$allocated)
  n_total <- nrow(facility_stats)
  pct_allocated <- round(100 * n_allocated / n_total, 1)
  total_units <- sum(result_df$allocated_units, na.rm = TRUE)
  
  footer_text <- paste0(
    "Facilities with PrEP: ", n_allocated, " of ", n_total, " (", pct_allocated, "%)   |   ",
    "Total PrEP Units Allocated: ", format(total_units, big.mark = ",")
  )
  
  # Base map theme
  base_theme <- theme_void(base_size = 15) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b = 8)),
      legend.position = "bottom",
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 12),
      plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
    )
  
  # Individual maps with titles
  p1 <- maps$facility_point_map +
    base_theme +
    labs(title = "PrEP Allocation: Facilities")
  
  p2 <- maps$district_facility_choropleth +
    base_theme +
    labs(title = "Districs with PrEP Allocation")
  
  p3 <- maps$district_population_choropleth +
    base_theme +
    labs(title = "% Population Covered")
  
  # Combine plots in a row with unified header and footer
  combined_plot <- (p1 | p2 | p3) +
    plot_layout(ncol = 3, widths = c(1, 1, 1), guides = "keep") +
    plot_annotation(
      title = label_text,
      caption = footer_text,
      theme = theme(
        plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        plot.caption = element_text(hjust = 0.5, size = 15, face = "italic")
      )
    )
  
  return(combined_plot)
}

  
generate_prep_allocation_outputs <- function(facility_df,
                                               incidence_df,
                                               facility_coords_df,
                                               district_sf,
                                               budget_vec,
                                               cost_per_unit_vec,
                                               coverage_mult_vec,        # ??? NEW
                                               selected_budget,
                                               selected_cost,
                                               selected_coverage_mult,   # ??? NEW
                                               efficacy = 0.95,
                                               dalys_per_infection = 20) {
  # 1. Run allocation scenarios
  output <- run_cost_and_demand_scenarios(
    cost_per_unit_vec = cost_per_unit_vec,
    budget_vec = budget_vec,
    facility_df = facility_df,
    incidence_df = incidence_df,
    efficacy = efficacy,
    dalys_per_infection = dalys_per_infection,
    coverage_mult_vec = coverage_mult_vec
  )
  
  # 2. Select scenario
  selected <- select_scenario_result(output, budget_value = selected_budget, cost_value = selected_cost, coverage_mult_value = selected_coverage_mult)
  result_df <- selected$result_df
  idx <- selected$index
  
  # 3. Generate maps
  maps <- map_prep_allocation_scenario(
    results_list = output$results_list,
    scenario_grid = output$scenario_grid,
    index = idx,
    facility_coords = facility_coords_df,
    district_shapefile = district_sf
  )
  
  # 4. Format map layout
  label_text <- paste0("Scenario: $", selected_cost, "/unit, $", format(selected_budget, big.mark = ","), " budget")
  formatted_map <- format_prep_allocation_maps(
    maps = maps,
    result_df = result_df,
    label_text = label_text
  )
  
  # 5. Pull summary table
  summary_row <- output$summary
  facility_summary <- output$facility_summaries[[idx]]
  
  # 6. Return everything
  return(list(
    formatted_map = formatted_map,
    summary_table = summary_row,
    facility_summary = facility_summary,
    result_df = result_df
  ))
}



