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

############################################################################
#Run allocation functions
############################################################################
allocate_prep_by_risk_with_stratified_prob <- function(facility_df,
                                                       incidence_df,
                                                       budget,
                                                       cost_per_unit = 130,
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
    filter(total_initiations > total_initiations_value)
  
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
  
  # p_facilities <- ggplot(facility_map_df, aes(x = longitude, y = latitude)) +
  #   geom_sf(data = district_shapefile, fill = NA, color = "black", inherit.aes = FALSE) +
  #   geom_point(aes(color = allocated), size = 1, alpha = 0.3) +
  #   scale_color_manual(values = c("blue", "red"), labels = c("No", "Yes")) +
  #   coord_sf() +
  #   labs(
  #     title = "Facilities Receiving PrEP Allocation",
  #     color = "Allocated PrEP"
  #   ) +
  #   theme_minimal()
  
  # Create a new column with labels for legend
  facility_map_df <- facility_map_df %>%
    mutate(prep_allocated_label = ifelse(allocated == "TRUE", "Yes", "No"))
  
  p_facilities <- ggplot() +
    geom_sf(data = district_shapefile, fill = NA, color = "black") +
    
    # Plot facilities not allocated
    geom_point(data = facility_map_df %>% filter(allocated == "FALSE"),
               aes(x = longitude, y = latitude, color = prep_allocated_label),
               size = 2, alpha = 0.4) +
    
    # Plot facilities that were allocated
    geom_point(data = facility_map_df %>% filter(allocated == "TRUE"),
               aes(x = longitude, y = latitude, color = prep_allocated_label),
               size = 2, alpha = 0.6) +
    
    # Manually define colors and labels
    scale_color_manual(
      name = "Allocated PrEP",
      values = c("Yes" = "red", "No" = "grey"),
      breaks = c("Yes", "No")
    ) +
    
    coord_sf() +
    labs(title = "Facilities Receiving PrEP Allocation") +
    theme_minimal()
  
  # p_facilities <- ggplot(facility_map_df %>% filter(allocated=="TRUE"), aes(x = longitude, y = latitude)) + #NOTE MANY FACILITIES DONT HAVE LAT/LON AND WILL NOT BE MAPPED
  #   geom_sf(data = district_shapefile, fill = NA, color = "black", inherit.aes = FALSE) +
  #   #geom_point(aes(color = allocated), size = 2, alpha=0.5) +
  #   #scale_color_manual(values = c("gray", "red"), labels = c("No", "Yes")) +
  #   geom_point(data=facility_map_df %>% filter(allocated=="FALSE"), color="grey", size = 2, alpha=0.4) +
  #   geom_point(color="red", size = 2, alpha=0.6) +
  #   #scale_color_manual(values = c("gray", "red"), labels = c("No", "Yes")) +
  #   coord_sf() +
  #   labs(
  #     title = "Facilities Receiving PrEP Allocation",
  #     color = "Allocated PrEP"
  #   ) +
  #   theme_minimal()
  # 
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
  
  # p_district_facilities <- ggplot(district_map1) +
  #   geom_sf(aes(fill = as.factor(allocated_yn)), color = "white") +
  #   scale_fill_manual(values=c("blue","red"),labels=c("No","Yes"),  name = "Districts with PrEP allocation", na.value = "grey90") +
  #   labs(
  #     title = "% Facilities Allocated PrEP"
  #   ) +
  #   theme_minimal()
  
  # Subset districts that are marked as allocated
  district_labels <- district_map1 %>%
    filter(allocated_yn == 1)
  
  # Create plot
  p_district_facilities <- ggplot(district_map1) +
    geom_sf(aes(fill = as.factor(allocated_yn)), color = "white") +
    scale_fill_manual(
      values = c("grey90", "red"),
      labels = c("No", "Yes"),
      name = "Districts with PrEP allocation",
      na.value = "grey90"
    ) +
    # ggrepel::geom_text_repel(
    #   data = district_labels,
    #   aes(
    #     geometry = geometry,
    #     label = district
    #   ),
    #   stat = "sf_coordinates",
    #   size = 2.8,                    # smaller text
    #   segment.color = "black",
    #   segment.size = 0.4,
    #   box.padding = 0.6,            # more space between label and district
    #   min.segment.length = 0.3,     # longer lines
    #   seed = 123,
    #   show.legend = FALSE,
    #   max.overlaps = 20
    # ) +
    labs(
      title = "% Facilities Allocated PrEP"
    ) +
    theme_minimal()
  
  # 3. % of population covered by PrEP by district
  # district_new_infections = data.frame(naomi_ssa_shp_m) %>% select(area_id, sex, age_group_label, incidence, pop_at_risk) %>%
  #   filter(age_group_label %in% c("15-19", "15-24", "25-34", "35-49")) %>%
  #   mutate(new_infections = incidence * pop_at_risk) %>%
  #   group_by(area_id) %>%
  #   summarize(new_infections = sum(new_infections), pop_at_risk = sum(pop_at_risk))
  
  district_inc_reduction <- result_df %>%
    #filter(age_group_label != "50-99") %>%
    group_by(area_id) %>%
    summarise(
      infections_averted=sum(infections_averted, na.rm = TRUE),
      units_allocated = sum(allocated_units, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(
      district_new_infections,
      by = c("area_id")
    )
  
  district_map2 <- left_join(district_shapefile, district_inc_reduction, by = "area_id") %>%
    mutate(infections_averted = na_if(infections_averted , 0),
           units_allocated  = na_if(units_allocated  , 0))
  
  # district_pop_summary <- result_df %>%
  #   group_by(area_id) %>%
  #   summarise(
  #     pop_total = sum(.data[[pop_column]], na.rm = TRUE),
  #     units_allocated = sum(allocated_units, na.rm = TRUE),
  #     percent_pop_covered = 100 * units_allocated / pop_total,
  #     .groups = "drop"
  #   ) %>%
  #   mutate(percent_pop_covered=ifelse(percent_pop_covered==0,NA,percent_pop_covered))
  #  
  # district_map2 <- left_join(district_shapefile, district_pop_summary, by = "area_id")
  
  p_district_pop <- ggplot(district_map2) +
    geom_sf(aes(fill = (units_allocated / pop_at_risk) * 100 ), color = "white") +
    scale_fill_viridis_c(name = "% Len coverage", na.value = "grey90", limits=c(0,7)) +
    labs(
      title = "% District Population At Risk Covered by PrEP"
    ) +
    guides(fill = guide_colorbar(direction = "horizontal", barwidth = 15)) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.key.height = unit(1.5, "cm")
    )
  
  p_district_pop <- ggplot(district_map2) +
    geom_sf(aes(fill = (units_allocated / pop_at_risk) * 100), color = "white") +
    scale_fill_viridis_c(name = "% Len coverage", na.value = "grey90", limits=c(0,17)) +
    labs(
      title = "District-level Len coverage (HIV negative adults age 15+)"
    ) +
    guides(fill = guide_colorbar(direction = "horizontal", barwidth = 15)) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.key.height = unit(1.5, "cm")
    ) +
    ggrepel::geom_text_repel(
      data = district_map2 %>% filter((units_allocated / pop_at_risk) * 100 >0 ),
      aes(
        geometry = geometry,
        label = sprintf("%.1f", (units_allocated / pop_at_risk) * 100)
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
  
  #4 % reduction in incidence by district (add facility #'s infections averted by number of infections in district)
  # district_new_infections = data.frame(naomi_ssa_shp_m) %>% select(area_id, sex, age_group_label, incidence, pop_at_risk) %>%
  #   filter(age_group_label %in% c("15-19", "15-24", "25-34", "35-49")) %>%
  #   mutate(new_infections = incidence * pop_at_risk) %>%
  #   group_by(area_id) %>%
  #   summarize(new_infections = sum(new_infections))
  # 
  # district_inc_reduction <- result_df %>%
  #   filter(age_group_label != "50-99") %>%
  #   group_by(area_id) %>%
  #   summarise(
  #     infections_averted=sum(infections_averted),
  #     .groups = "drop"
  #   ) %>%
  #   left_join(
  #     district_new_infections,
  #     by = c("area_id")
  #   )
  # 
  # district_map3 <- left_join(district_shapefile, district_inc_reduction, by = "area_id")
  
  # p_district_inf_averted <- ggplot(district_map2) +
  #   geom_sf(aes(fill = (infections_averted / new_infections)*100), color = "white") +
  #   scale_fill_viridis_c(name = "Infections averted (%)", na.value = "grey90", limits=c(0,26)) +
  #   labs(
  #     title = "Reduction in incidence by district (%)"
  #   ) +
  #   guides(fill = guide_colorbar(direction = "horizontal", barwidth = 15)) +
  #   theme_minimal() +
  #   theme(
  #     legend.position = "bottom",
  #     legend.key.height = unit(1.5, "cm")
  #   )
  
  p_district_inf_averted <- ggplot(district_map2) +
    geom_sf(aes(fill = (infections_averted / new_infections)*100), color = "white") +
    scale_fill_viridis_c(name = "Infections averted (%)", na.value = "grey90", limits=c(0,55)) +
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
      data = district_map2 %>% filter(infections_averted / new_infections*100 >0),
      aes(
        geometry = geometry,
        label = sprintf("%.1f", infections_averted / new_infections*100)
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
  
  p_district_efficiency <- ggplot(district_map2) +
    geom_sf(aes(fill = (infections_averted / new_infections) / (units_allocated / pop_at_risk) ), color = "white") +
    scale_fill_viridis_c(name = "Infections averted per unit PrEP", na.value = "grey90") +
    labs(
      title = "Efficiency"
    ) +
    guides(fill = guide_colorbar(direction = "horizontal", barwidth = 15)) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.key.height = unit(1.5, "cm")
    )
  
  return(list(
    facility_point_map = p_facilities,
    district_facility_choropleth = p_district_facilities,
    district_population_choropleth = p_district_pop,
    district_infections_averted = p_district_inf_averted,
    district_prep_efficiency = p_district_efficiency
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
  total_pop <- sum(district_new_infections$pop_at_risk, na.rm = TRUE)
  prep_coverage_pct <- round(100 * total_units / total_pop, 1)
  expected_infections <- sum(district_new_infections$new_infections, na.rm = TRUE)
  infections_averted <- sum(result_df$allocated_units * result_df$inc_in_sample / 1000 * efficacy, na.rm = TRUE)
  percent_infections_averted <- round(100 * infections_averted / expected_infections, 1)
  
  footer_text <- paste0(
    "Facilities with PrEP: ", n_allocated, " of ", n_total, " (", pct_allocated, "%)   |   ",
    "Total PrEP Units Allocated: ", format(total_units, big.mark = ","), "   |   ",
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
  p1 <- maps$facility_point_map +
    base_theme +
    labs(title = "PrEP Allocation: Facilities")
  
  p2 <- maps$district_facility_choropleth +
    base_theme +
    labs(title = "Districs with PrEP Allocation")
  
  p3 <- maps$district_population_choropleth +
    base_theme +
    labs(title = "% Population Covered")
  
  p4 <- maps$district_infections_averted +
    base_theme +
    labs(title = "% Reduction in Incidence")
  
  p5 <- maps$district_prep_efficiency +
    base_theme +
    labs(title = "PrEP efficiency")
  
  # Combine plots in a row with unified header and footer
  combined_plot <- (p1 | p3 | p4 ) +
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
                                               coverage_mult_vec,      
                                               selected_budget,
                                               selected_cost,
                                               selected_coverage_mult,  
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
  label_text <- paste0("Scenario: $", selected_cost, "/unit, $", format(selected_budget/1000000, big.mark = ","),"M",  " budget",", " ,"min facility capacity: " ,total_initiations_value, " initiations")
  #label_text <- paste0("Scenario: ", selected_budget/selected_cost," Len person-years,", " max PrEP coverage by district/age/sex strata: ", (coverage_mult*0.25)*100,"%")
  formatted_map <- format_prep_allocation_maps(
    maps = maps,
    result_df = result_df,
    incidence_df = incidence_df,
    label_text = label_text,
    efficacy = efficacy
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



