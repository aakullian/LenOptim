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

# Data from Updated_Parameters_Mozambique LEN modeling 2025ks br.xlsx
# pregnant	2,111,754 (2,338,568)	432,627 (487,742)	77,913 (80,075)	SISMA (National HMIS) Jan-Dec 2024 Note: This data reflects the number of pregnant women who attended at least one antenatal care (ANC) clinic visit; it does not capture those who never visited a facility during pregnancy. These figures align with subnational UNAIDS estimates. Estimates in red: according to the 2021–2022 PHIA, 88.7% of pregnant women in Zambézia attended at least one ANC visit, 97.3% did so in Gaza, and 90.3% nationally. To adjust for non-attendance rates, the estimated number of pregnant women would be approximately 2,338,568 nationally, 487,742 in Zambezia, and 80,075 in Gaza.

# Number of pregnant HIV negative women
pbfw_data <- facility_df_place_holder %>%
  select(province,`f_15 - 24`, `f_25 - 34`) %>%
  filter(province %in% c("GAZA","ZAMBEZIA")) %>%
  mutate(n_hiv_neg = `f_15 - 24` + `f_25 - 34`) %>% 
  #group_by(province) %>%
  summarise(pop_hiv_neg_1534 = sum(n_hiv_neg)) %>% 
  mutate(province = "Gaza + Zambezia") %>% 
  mutate(
    n_preg_women = case_when(
      province == "Gaza + Zambezia" ~ 80075 + 487742,
      TRUE ~ NA_real_
    ),
    n_births = case_when(
      province == "Gaza + Zambezia" ~ 73899 + 574548,
      TRUE ~ NA_real_
    ),
    n_hivpos_preg_women = case_when(
      province == "Gaza + Zambezia" ~ 11960 + 26862,
      TRUE ~ NA_real_
    ),
    n_hivneg_preg_women = n_preg_women - n_hivpos_preg_women,
    births_per_preg_women = n_births / n_preg_women,
    hivneg_preg_women_per_woman = n_hivneg_preg_women / pop_hiv_neg_1534,
    n_births_to_hivneg_preg_women = births_per_preg_women * n_hivneg_preg_women)

facility_df = facility_df_place_holder 
facility_coords_df <- facility_df_place_holder %>%
  select(facility_name, latitude, longitude, district, area_id)
  #filter(!is.na(longitude),!is.na(latitude)) 

allocate_prep_by_risk_with_stratified_prob <- function(facility_df,
                                                       incidence_df,
                                                       budget,
                                                       cost_per_unit = 130,
                                                       #prep_uptake_df = NULL,
                                                       efficacy = 0.95,
                                                       dalys_per_infection = 20) {
  library(dplyr)
  library(tidyr)
  library(stringr)
  
  total_units <- floor(budget / cost_per_unit)
  
  # Step 1: Filter and reshape facility data
  facility_df <- facility_df %>%
    dplyr::select(
      area_id,
      district,
      province,
      facility_name,
      total_initiations,
      starts_with("f_"),
      starts_with("m_")
    ) 
  
  pop_cols <- names(facility_df)[grepl("^[fm]_", names(facility_df))]
  
  df_long <- facility_df %>%
    pivot_longer(cols = all_of(pop_cols), names_to = "group", values_to = "catchment_population_group") %>%
    mutate(
      sex = case_when(
        grepl("f_", group) ~ "female",
        grepl("m_", group) ~ "male",
        TRUE ~ NA_character_
      ),
      age_group_label = case_when(
        grepl("15 - 24", group) ~ "15-24",
        grepl("25 - 34", group) ~ "25-34",
        grepl("35 - 49", group) ~ "35-49",
        grepl("50", group)    ~ "50-99",
        TRUE ~ NA_character_
      ),
      catchment_population_group = as.numeric(catchment_population_group)
    ) %>%
    filter(!is.na(sex), !is.na(age_group_label)) %>%
    group_by(province, area_id, district,facility_name) %>%
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
  
  # df_merged <- df_expanded %>%
  #   left_join(
  #     incidence_df %>% select(area_id, sex, age_group_label, inc_in_sample, quantile_target_factor, inc_district),
  #     by = c("area_id", "sex", "age_group_label", "quantile_target_factor")
  #   ) %>%
  #   mutate(inc_in_sample_cat = round(inc_in_sample, 0)) %>%
  #   filter(!is.na(inc_in_sample), !is.na(catchment_population_group)) %>%
  #   mutate(allocated_units = 0) %>%
  #   mutate(
  #     hiv_neg_preg_women = dplyr::case_when(
  #       sex == "female" & age_group_label %in% c("15-24","25-34") ~
  #         catchment_population_risk_strata * pbfw_data$hivneg_preg_women_per_woman,
  #       TRUE ~ NA_real_
  #     )
  #   )
  
  df_merged <- df_expanded %>%
    left_join(
      incidence_df %>%
        select(area_id, sex, age_group_label, inc_in_sample, quantile_target_factor, inc_district),
      by = c("area_id", "sex", "age_group_label", "quantile_target_factor")
    ) %>%
    mutate(inc_in_sample_cat = round(inc_in_sample, 0)) %>%
    filter(!is.na(inc_in_sample), !is.na(catchment_population_group)) %>%
    mutate(allocated_units = 0) %>%
    # 1) raw counts for target ages (female 15–24 / 25–34)
    mutate(
      hiv_neg_preg_women_raw = case_when(
        sex == "female" & age_group_label %in% c("15-24","25-34") ~
          catchment_population_risk_strata * pbfw_data$hivneg_preg_women_per_woman,
        TRUE ~ NA_real_
      ),
      # 2) weights: 2 for 15–24, 1 for 25–34 (this can be changed to make the ratio of pregnancy rates different)
      age_weight = case_when(
        sex == "female" & age_group_label == "15-24" ~ pregnancy_rate_ratio,
        sex == "female" & age_group_label == "25-34" ~ 1,
        TRUE ~ NA_real_
      )
    ) %>%
    # 3) reallocate within each district × risk group by weights
    group_by(area_id, quantile_target_factor) %>%     # <-- change risk_group to your column name if different
    mutate(
      pool = if (all(is.na(hiv_neg_preg_women_raw))) NA_real_
      else sum(hiv_neg_preg_women_raw, na.rm = TRUE),
      wsum = sum(age_weight[!is.na(hiv_neg_preg_women_raw)], na.rm = TRUE),
      hiv_neg_preg_women = case_when(
        !is.na(age_weight) & !is.na(pool) & wsum > 0 ~ pool * age_weight / wsum,
        TRUE ~ NA_real_
      )
    ) %>%
    ungroup() %>%
    select(-hiv_neg_preg_women_raw, -age_weight, -pool, -wsum) %>%
    mutate(
      hiv_neg_non_preg_women = catchment_population_risk_strata - hiv_neg_preg_women,
      p_preg = hiv_neg_preg_women / catchment_population_risk_strata,
      inc_nonpreg = inc_in_sample / (1 + p_preg),
      inc_preg = preg_multiplier * inc_in_sample / (1 + p_preg)
    )
  
  # --- Method A: Ranked population strategy ---
    df_result <- df_merged %>%
      arrange(desc(inc_nonpreg), desc(total_initiations), facility_name, sex, age_group_label) %>%
      mutate(allocated_units = 0)
    
    remaining_units <- total_units
    
    df_result <- df_result %>%
          mutate(priority = row_number()) %>%
          mutate(cum_pop = cumsum(hiv_neg_non_preg_women)) %>%
          mutate(units_needed = ceiling(coverage_mult * hiv_neg_non_preg_women),
                 skip = ifelse(age_group_label %in% age_group_allocation_selection & sex %in% sex_allocation_selection, 0,1))
    
    # Allocate PrEP
      df_result <- df_result %>%
        mutate(units_needed = ifelse(is.na(units_needed) | skip==1, 0, units_needed)) %>%
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
  # This includes both the infections averted in the pregnant woman and infant
  # infant infections averted is assumed to be 12% of maternal infections averted based on MTCT rates without PrEP
  df_result <- df_result %>%
    mutate(infections_averted = allocated_units * inc_nonpreg / 1000 * efficacy)
  
  return(df_result)
}

summarize_allocation_scenarios <- function(results_list,
                                           incidence_df,
                                           budgets,
                                           cost_per_units,
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
  create_facility_summary_by_age_sex <- function(result_df, age_order = age_groups) {
  #create_facility_summary_by_age_sex <- function(result_df, age_order = c("15-19", "20-24", "25-34", "35-49", "50-99")) {  
    result_df %>%
      group_by(facility_name, sex, age_group_label) %>%
      summarise(
        catchment_population = sum(catchment_population_risk_strata, na.rm = TRUE),
        prep_units = sum(allocated_units, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        prep_coverage = round(prep_units / catchment_population, 3),
        age_group_label = factor(age_group_label, levels = age_order)
      ) %>%
      arrange(facility_name, sex, age_group_label)
  }
  
  summaries <- lapply(seq_along(results_list), function(i) {
    result <- results_list[[i]]
    cost <- cost_per_units[[i]]
    budget <- budgets[[i]]
    
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
        total_allocated_units = total_allocated_units,
        expected_infections_no_prep = expected_infections,
        infections_averted = infections_averted,
        percent_reduction_in_incidence = percent_reduction,
        prep_coverage = prep_coverage,
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
    cost_per_unit = cost_per_unit_vec
  ) %>%
    mutate(
      scenario_id = row_number(),
      scenario_label = if (!is.null(scenario_labels)) scenario_labels else
        paste0("Budget $", budget / 1e6, "M @ $", cost_per_unit)
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
      efficacy = efficacy
    )
  })
  
  summary_df <- summarize_allocation_scenarios(
    results_list = results_list,
    incidence_df = incidence_df,
    budgets = scenario_grid$budget,
    cost_per_units = scenario_grid$cost_per_unit,
    scenario_labels = scenario_grid$scenario_label,
    dalys_per_infection = dalys_per_infection,
    efficacy = efficacy
  )
  
  return(list(
    summary = summary_df$summary,  # not summary_result itself
    facility_summaries = summary_df$facility_summaries,
    results_list = results_list,
    scenario_grid = scenario_grid
  ))
}

# ## Run scenarios ##
# output <- run_cost_and_demand_scenarios(
#   cost_per_unit_vec = c(60, 100, 120),
#   budget_vec = c(29e6),
#   facility_df = facility_df,
#   incidence_df = incidence_df
# )

# View(output$facility_summaries[[1]])
# View(output$summary)
# 
# facility_120_summary <- (output$facility_summaries[[2]]) %>%
#   filter(age_group_label!="50-99")
# View(facility_120_summary)

# Select and summarize results by budget and cost
select_scenario_result <- function(output, budget_value, cost_value) {
  # Find the matching index
  idx <- which(output$scenario_grid$budget == budget_value &
                 output$scenario_grid$cost_per_unit == cost_value)
  
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
  
  length(unique(result_df$facility_name))
  length(unique(facility_coords$facility_name))
  length(unique(result_df$facility_name[result_df$units_this_round>0]))
  
  facility_summary <- result_df %>%
    group_by(facility_name, area_id) %>%
    summarise(units_prep = sum(allocated_units, na.rm=T),
              pop_at_risk = sum(catchment_population_risk_strata),
      allocated = sum(allocated_units, na.rm = TRUE) > 0, .groups = "drop")
  
  # 1. Facility-level allocation
  facility_map_df <- facility_coords %>%
    left_join(facility_summary,
      by = c("facility_name", "area_id")
    ) 
    #mutate(allocated = ifelse(is.na(allocated), FALSE, allocated))
  
  length(unique(facility_map_df$facility_name[facility_map_df$allocated=="TRUE"]))
  unique(facility_map_df$district[facility_map_df$allocated=="TRUE"])
  
  # Create a new column with labels for legend
  facility_map_df <- facility_map_df %>%
    mutate(prep_allocated_label = ifelse(allocated == "TRUE", "Yes", "No"))
  
  # 2. % of facilities with PrEP by district
  district_facility_summary <- facility_map_df %>%
    group_by(area_id, district) %>%
    summarise(
      n_facilities = n(),
      n_allocated = sum(allocated),
      percent_allocated = 100 * n_allocated / n_facilities,
      prep_coverage = 100 *(units_prep / pop_at_risk),
      .groups = "drop"
    ) %>%
    mutate(allocated_yn = ifelse(percent_allocated==0,0,1))
  

  district_map1 <- left_join(district_shapefile, district_facility_summary, by = "area_id") 
  
  library(ggrepel)

# Subset districts that are marked as allocated
district_labels <- district_map1 %>%
  filter(allocated_yn == 1)

# Create plot
p_district_facilities <- ggplot(district_map1) +
  geom_sf(aes(fill = as.factor(allocated_yn)), color = "white") +
  scale_fill_manual(
    values = c("grey", "red"),
    labels = c("No", "Yes"),
    name = "Districts with PrEP allocation",
    na.value = "grey90"
  ) +
  ggrepel::geom_text_repel(
    data = district_labels,
    aes(
      geometry = geometry,
      label = district
    ),
    stat = "sf_coordinates",
    size = 2.8,                    # smaller text
    segment.color = "black",
    segment.size = 0.4,
    box.padding = 0.6,            # more space between label and district
    min.segment.length = 0.3,     # longer lines
    seed = 123,
    show.legend = FALSE,
    max.overlaps = 20
  ) +
  labs(
    title = "% Facilities Allocated PrEP"
  ) +
  theme_minimal()
  
# p_district_prep_coverage <- ggplot(district_map1 %>% mutate(across(where(is.numeric), ~ na_if(., 0)))) +
#   geom_sf(aes(fill = prep_coverage), color = "white") +
#   scale_fill_viridis_c(name = "Infections averted (%)", na.value = "grey90") +
#   labs(
#     title = "PrEP coverage (%)"
#   ) +
#   guides(fill = guide_colorbar(direction = "horizontal", barwidth = 15)) +
#   theme_minimal() +
#   theme(
#     legend.position = "bottom",
#     legend.key.height = unit(1.5, "cm")
#   )

  
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
  
  district_map2 <- district_map2 %>%
    mutate(centroid = st_centroid(geometry)) %>%
    mutate(
      lon = st_coordinates(centroid)[,1],
      lat = st_coordinates(centroid)[,2]
    )
  
  p_district_pop <- ggplot(district_map2) +
    geom_sf(aes(fill = percent_pop_covered), color = "white") +
    scale_fill_viridis_c(name = "% Catchment Pop Covered", na.value = "grey90", limits=c(0,7)) +
    labs(
      title = "% Catchment Population Covered by PrEP"
    ) +
    guides(fill = guide_colorbar(direction = "horizontal", barwidth = 15)) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.key.height = unit(1.5, "cm")
    ) +
    ggrepel::geom_text_repel(
      data = district_map2 %>% filter(percent_pop_covered >0 ),
      aes(
        geometry = geometry,
        label = sprintf("%.1f", percent_pop_covered)
      ),
      stat = "sf_coordinates",
      size = 2.8,                    # smaller text
      segment.color = "black",
      segment.size = 0.4,
      box.padding = 0.6,            # more space between label and district
      min.segment.length = 0.3,     # longer lines
      seed = 123,
      show.legend = FALSE,
      max.overlaps = 20
    ) 
    # geom_text(
    #   data = district_map2 %>% filter(percent_pop_covered >0 ),  # adjust threshold as needed
    #   #data = district_map2 %>% filter(area_name == "KaMpfumu"),  # adjust threshold as needed
    #   aes(x = lon, y = lat, label = paste(round(percent_pop_covered, 1),"%")),
    #   size = 3, color = "black"
    # )
  
  district_inc_reduction <- result_df %>%
    #filter(age_group_label != "50-99") %>%
    group_by(area_id) %>%
    summarise(
      infections_averted=sum(infections_averted, na.rm = TRUE),
      units_allocated = sum(allocated_units, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(infections_averted > 0) %>%
    left_join(
      district_new_infections,
      by = c("area_id")
    ) %>%
    mutate(inc_reduction = infections_averted / new_infections)
  
  district_map3 <- left_join(district_shapefile, district_inc_reduction, by = "area_id") %>%
    mutate(infections_averted = na_if(infections_averted , 0),
           units_allocated  = na_if(units_allocated  , 0),
           inc_reduction = infections_averted / new_infections * 100)
  
  district_map3 <- district_map3 %>%
    mutate(centroid = st_centroid(geometry)) %>%
    mutate(
      lon = st_coordinates(centroid)[,1],
      lat = st_coordinates(centroid)[,2]
    )
  
  p_district_inf_averted <- ggplot(district_map3) +
    geom_sf(aes(fill = inc_reduction), color = "white") +
    scale_fill_viridis_c(name = "Infections averted (%)", na.value = "grey90", limits=c(0,26)) +
    labs(
      title = "Reduction in incidence by district (%)"
    ) +
    guides(fill = guide_colorbar(direction = "horizontal", barwidth = 15)) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.key.height = unit(1.5, "cm")
    ) +
    ggrepel::geom_text_repel(
      data = district_map3 %>% filter(inc_reduction >0 ),
      aes(
        geometry = geometry,
        label = sprintf("%.1f", inc_reduction)
      ),
      stat = "sf_coordinates",
      size = 2.8,                    # smaller text
      segment.color = "black",
      segment.size = 0.4,
      box.padding = 0.6,            # more space between label and district
      min.segment.length = 0.3,     # longer lines
      seed = 123,
      show.legend = FALSE,
      max.overlaps = 20
    ) 
    # geom_text(
    #   data = district_map3 %>% filter(inc_reduction >0 ),  # adjust threshold as needed
    #   #data = district_map3 %>% filter(area_name == "KaMpfumu"),  # adjust threshold as needed
    #   aes(x = lon, y = lat, label = paste(round(inc_reduction, 1),"%")),
    #   size = 3, color = "black"
    # )
  
  return(list(
    #facility_point_map = p_facilities,
    district_facility_choropleth = p_district_facilities,
    district_population_choropleth = p_district_pop,
    district_infections_averted = p_district_inf_averted
  ))
}

format_prep_allocation_maps <- function(maps, result_df, incidence_df, efficacy, label_text = NULL) {
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
  total_pop <- sum(incidence_df$pop_subsample, na.rm = TRUE)
  prep_coverage_pct <- round(100 * total_units / total_pop, 1)
  expected_infections <- sum(incidence_df$inc_district * incidence_df$pop_subsample / 1000, na.rm = TRUE)
  infections_averted <- sum(result_df$allocated_units * result_df$inc_in_sample / 1000 * efficacy, na.rm = TRUE)
  percent_infections_averted <- round(100 * infections_averted / expected_infections, 1)
  
  footer_text <- paste0(
    "Districts with PrEP: ", n_allocated, " of ", n_total, " (", pct_allocated, "%)   |   ",
    #"Total PrEP Units Allocated: ", format(total_units, big.mark = ","), "   |   ",
    "PrEP Coverage: ", prep_coverage_pct, "%   |   ",
    "% Infections Averted: ", percent_infections_averted, "%"
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
  # p1 <- maps$facility_point_map +
  #   base_theme +
  #   labs(title = "PrEP Allocation: Facilities")
  
  p2 <- maps$district_facility_choropleth +
    base_theme +
    labs(title = "Districs with PrEP Allocation")
  
  p3 <- maps$district_population_choropleth +
    base_theme +
    labs(title = "% Population Covered")
  
  p4 <- maps$district_infections_averted +
    base_theme +
    labs(title = "% of infections averted")
  
  # Combine plots in a row with unified header and footer
  combined_plot <- (p2 | p3 | p4) +
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

# format_prep_allocation_maps <- function(maps, result_df, label_text = NULL) {
#   library(dplyr)
#   library(patchwork)
#   library(ggplot2)
#   
#   # Compute annotation stats
#   facility_stats <- result_df %>%
#     group_by(facility_name) %>%
#     summarise(allocated = sum(allocated_units, na.rm = TRUE) > 0, .groups = "drop")
#   
#   n_allocated <- sum(facility_stats$allocated)
#   n_total <- nrow(facility_stats)
#   pct_allocated <- round(100 * n_allocated / n_total, 1)
#   total_units <- sum(result_df$allocated_units, na.rm = TRUE)
#   
#   footer_text <- paste0(
#     "Facilities with PrEP: ", n_allocated, " of ", n_total, " (", pct_allocated, "%)   |   ",
#     "Total PrEP Units Allocated: ", format(total_units, big.mark = ",")
#   )
#   
#   # Base map theme
#   base_theme <- theme_void(base_size = 15) +
#     theme(
#       plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b = 8)),
#       legend.position = "bottom",
#       legend.title = element_text(size = 13),
#       legend.text = element_text(size = 12),
#       plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
#     )
#   
#   # Individual maps with titles
#   p1 <- maps$facility_point_map +
#     base_theme +
#     labs(title = "PrEP Allocation: Facilities")
#   
#   p2 <- maps$district_facility_choropleth +
#     base_theme +
#     labs(title = "Districs with PrEP Allocation")
#   
#   p3 <- maps$district_population_choropleth +
#     base_theme +
#     labs(title = "% Population Covered")
#   
#   # Combine plots in a row with unified header and footer
#   combined_plot <- (p1 | p2 | p3) +
#     plot_layout(ncol = 3, widths = c(1, 1, 1), guides = "keep") +
#     plot_annotation(
#       title = label_text,
#       caption = footer_text,
#       theme = theme(
#         plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
#         plot.caption = element_text(hjust = 0.5, size = 15, face = "italic")
#       )
#     )
#   
#   return(combined_plot)
# }

# selected_100 <- select_scenario_result(output, budget_value = 29e6, cost_value = 100)
# result_100 <- selected_100$result_df 
# selected_120 <- select_scenario_result(output, budget_value = 29e6, cost_value = 120)
# result_120 <- selected_120$result_df 
# 
# #create maps
# maps_100 <- map_prep_allocation_scenario(
#   results_list = output$results_list,
#   scenario_grid = output$scenario_grid,
#   index = selected_100$index,
#   facility_coords = facility_coords_df,
#   district_shapefile = district_sf
# )
# 
# maps_120 <- map_prep_allocation_scenario(
#   results_list = output$results_list,
#   scenario_grid = output$scenario_grid,
#   index = selected_120$index,
#   facility_coords = facility_coords_df,
#   district_shapefile = district_sf
# )
# 
# maps_100$facility_point_map
# maps_100$district_facility_choropleth
# maps_100$district_population_choropleth
# 
# formatted_maps_100 <- format_prep_allocation_maps(
#   maps = maps_100,
#   result_df = result_100,
#   label_text = "Scenario: $100/unit, $29M budget"
# )
# 
# formatted_maps_120 <- format_prep_allocation_maps(
#   maps = maps_120,
#   result_df = result_120,
#   label_text = "Scenario: $120/unit, $29M budget"
# )

create_summary_by_province_district_age_sex <- function(result_df) {
  total_units <- sum(result_df$allocated_units, na.rm = TRUE)
  
  # Create combined key like "male_15-19"
  result_df <- result_df %>%
    mutate(sex_age = paste0(tolower(sex), "_", age_group_label))
  
  # Summarize units
  units_summary <- result_df %>%
    group_by(province, district, sex_age) %>%
    summarise(units = sum(allocated_units, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(
      names_from = sex_age,
      values_from = units,
      names_glue = "{sex_age}",
      values_fill = 0
    )
  
  # Summarize percent of total
  pct_summary <- result_df %>%
    group_by(province, district, sex_age) %>%
    summarise(pct = round(100 * sum(allocated_units, na.rm = TRUE) / total_units, 2), .groups = "drop") %>%
    pivot_wider(
      names_from = sex_age,
      values_from = pct,
      names_glue = "{sex_age}_pct",
      values_fill = 0
    )
  
  # Interleave units and pct columns for clarity
  # combined_summary <- left_join(units_summary, pct_summary, by = c("province", "district")) %>%
  #   relocate(ends_with("_pct"), .after = ends_with(c("_15-19", "_20-24", "_25-49", "_50-99"))) %>%
  #   arrange(province, district)
  
  combined_summary <- left_join(units_summary, pct_summary, by = c("province", "district")) %>%
    relocate(ends_with("_pct"), .after = ends_with(c("_15-24", "_25-34", "_35-49"))) %>%
    arrange(province, district)
  
  # Province-level summary
  province_summary <- result_df %>%
    group_by(province) %>%
    summarise(prep_units_allocated = sum(allocated_units, na.rm = TRUE), .groups = "drop") %>%
    mutate(percent_of_total = round(100 * prep_units_allocated / total_units, 2)) %>%
    arrange(province)
  
  return(list(
    by_province_district_age_sex = combined_summary,
    by_province = province_summary
  ))
}


generate_prep_allocation_outputs <- function(facility_df,
                                             incidence_df,
                                             facility_coords_df,
                                             district_sf,
                                             budget_vec,
                                             cost_per_unit_vec,
                                             selected_budget,
                                             selected_cost,
                                             efficacy = 0.95,
                                             dalys_per_infection = 20) {
  # 1. Run allocation scenarios
  output <- run_cost_and_demand_scenarios(
    cost_per_unit_vec = cost_per_unit_vec,
    budget_vec = budget_vec,
    facility_df = facility_df,
    incidence_df = incidence_df,
    efficacy = efficacy,
    dalys_per_infection = dalys_per_infection
  )
  
  # 2. Select scenario
  selected <- select_scenario_result(output, budget_value = selected_budget, cost_value = selected_cost)
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
  
  # # 4. Format map layout
  # label_text <- paste0("Scenario: $", selected_cost, "/unit, $", format(selected_budget, big.mark = ","), " budget")
  # formatted_map <- format_prep_allocation_maps(
  #   maps = maps,
  #   result_df = result_df,
  #   label_text = label_text
  # )
  # 
  # 4. Format map layout
  label_text <- paste0("Scenario: General population women (25-34 years)  |  ", selected_budget/selected_cost," Len courses  |  ", "Uptake: ", (coverage_mult)*100," %")
  formatted_map <- format_prep_allocation_maps(
    maps = maps,
    result_df = result_df,
    incidence_df = incidence_df,
    label_text = label_text,
    efficacy = efficacy
  )
  
  # 5. Pull summary table
  summary_row <- output$summary[idx, ]
  facility_summary <- output$facility_summaries[[idx]]
  
  summaries <- create_summary_by_province_district_age_sex(result_df)
  
  # 6. Return everything
  return(list(
    formatted_map = formatted_map,
    summary_table = summary_row,
    facility_summary = facility_summary,
    result_df = result_df,
    by_prov_dist_age_sex = summaries$by_province_district_age_sex,
    by_province = summaries$by_province
  ))
}



