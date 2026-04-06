# model_functions.R
# Core allocation functions for LenOptim Shiny dashboard.
# Refactored from Allocate_PreP_All_Country_Model.R to eliminate global variable dependencies.
# All formerly-global variables are now explicit function parameters.

# Disable scientific notation globally
options(scipen = 999)

allocate_prep_by_risk_with_stratified_prob <- function(facility_df,
                                                       incidence_df,
                                                       budget,
                                                       cost_per_unit = 130,
                                                       efficacy = 0.95,
                                                       dalys_per_infection = 20,
                                                       coverage_mult = 1.0,
                                                       age_group_allocation_selection = c("15-24", "25-34", "35-49"),
                                                       sex_allocation_selection = c("male", "female"),
                                                       min_risk_quantile = 0) {

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
    group_by(province, area_id, district, facility_name) %>%
    mutate(total_catchment_population = sum(catchment_population_group, na.rm = TRUE)) %>%
    ungroup()

  # Step 2: Cross with risk quantiles
  quantiles <- unique(as.character(incidence_df$quantile_target_factor))
  df_expanded <- df_long %>%
    crossing(quantile_target_factor = quantiles) %>%
    mutate(catchment_population_risk_strata = catchment_population_group / length(quantiles))

  # Step 3: Merge incidence
  incidence_df <- incidence_df %>%
    mutate(
      sex = tolower(sex),
      age_group_label = trimws(age_group_label),
      quantile_target_factor = as.character(quantile_target_factor)
    )

  df_merged <- df_expanded %>%
    left_join(
      incidence_df %>% dplyr::select(area_id, sex, age_group_label, inc_in_sample, quantile_target_factor, inc_district, quant_target),
      by = c("area_id", "sex", "age_group_label", "quantile_target_factor")
    ) %>%
    mutate(inc_in_sample_cat = round(inc_in_sample, 0)) %>%
    filter(!is.na(inc_in_sample), !is.na(catchment_population_group)) %>%
    mutate(allocated_units = 0)

  # --- Ranked population strategy ---
  df_result <- df_merged %>%
    arrange(desc(inc_in_sample), desc(total_initiations), facility_name, sex, age_group_label) %>%
    mutate(allocated_units = 0)

  remaining_units <- total_units

  df_result <- df_result %>%
    mutate(priority = row_number()) %>%
    mutate(cum_pop = cumsum(catchment_population_risk_strata)) %>%
    mutate(units_needed = ceiling(coverage_mult * catchment_population_risk_strata),
           skip = ifelse(age_group_label %in% age_group_allocation_selection &
                         sex %in% sex_allocation_selection &
                         quant_target > min_risk_quantile, 0, 1))

  # Allocate PrEP
  df_result <- df_result %>%
    mutate(units_needed = ifelse(is.na(units_needed) | skip == 1, 0, units_needed)) %>%
    mutate(
      cum_units = cumsum(units_needed),
      units_this_round = ifelse(
        units_needed > 0,
        pmin(units_needed, pmax(0, remaining_units - dplyr::lag(cum_units, default = 0))),
        0
      ),
      allocated_units = allocated_units + units_this_round
    )

  remaining_units <- total_units - sum(df_result$allocated_units, na.rm = TRUE)

  # Final: estimate infections averted
  df_result <- df_result %>%
    mutate(infections_averted = allocated_units * inc_in_sample / 1000 * efficacy)

  return(df_result)
}


summarize_allocation_scenarios <- function(results_list,
                                           incidence_df,
                                           budgets,
                                           cost_per_units,
                                           scenario_labels = NULL,
                                           dalys_per_infection = 20,
                                           efficacy = 0.95,
                                           age_group_allocation_selection = c("15-24", "25-34", "35-49")) {

  if (is.null(scenario_labels)) {
    scenario_labels <- paste0("Scenario_", seq_along(results_list))
  }

  # Internal function: create facility summary by age/sex
  create_facility_summary_by_age_sex <- function(result_df, age_order = age_group_allocation_selection) {
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
    prep_coverage <- (total_allocated_units / total_population) * 100
    total_cost <- total_allocated_units * cost
    cost_per_infection_averted <- total_cost / infections_averted
    total_dalys_averted <- infections_averted * dalys_per_infection
    cost_per_daly_averted <- total_cost / total_dalys_averted
    percent_reduction <- (infections_averted / expected_infections) * 100
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


run_cost_and_demand_scenarios <- function(cost_per_unit_vec,
                                          budget_vec,
                                          facility_df,
                                          incidence_df,
                                          scenario_labels = NULL,
                                          efficacy = 0.95,
                                          dalys_per_infection = 20,
                                          coverage_mult = 1.0,
                                          age_group_allocation_selection = c("15-24", "25-34", "35-49"),
                                          sex_allocation_selection = c("male", "female"),
                                          risk_groups = 4,
                                          min_risk_quantile = 0) {

  # Create full grid of scenarios
  scenario_grid <- crossing(
    budget = budget_vec,
    cost_per_unit = cost_per_unit_vec
  ) %>%
    mutate(
      scenario_id = row_number(),
      scenario_label = if (!is.null(scenario_labels)) scenario_labels else
        paste0(risk_groups, " risk groups (quantiles) ", "Budget $", format(budget, big.mark = ","), " @ $", cost_per_unit, "/course")
    )

  # Run allocation for each combination
  results_list <- lapply(seq_len(nrow(scenario_grid)), function(i) {
    allocate_prep_by_risk_with_stratified_prob(
      facility_df = facility_df,
      incidence_df = incidence_df,
      cost_per_unit = scenario_grid$cost_per_unit[i],
      budget = scenario_grid$budget[i],
      efficacy = efficacy,
      coverage_mult = coverage_mult,
      age_group_allocation_selection = age_group_allocation_selection,
      sex_allocation_selection = sex_allocation_selection,
      min_risk_quantile = min_risk_quantile
    )
  })

  summary_df <- summarize_allocation_scenarios(
    results_list = results_list,
    incidence_df = incidence_df,
    budgets = scenario_grid$budget,
    cost_per_units = scenario_grid$cost_per_unit,
    scenario_labels = scenario_grid$scenario_label,
    dalys_per_infection = dalys_per_infection,
    efficacy = efficacy,
    age_group_allocation_selection = age_group_allocation_selection
  )

  return(list(
    summary = summary_df$summary,
    facility_summaries = summary_df$facility_summaries,
    results_list = results_list,
    scenario_grid = scenario_grid
  ))
}


select_scenario_result <- function(output, budget_value, cost_value) {
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
                                         district_new_infections,
                                         pop_column = "catchment_population_risk_strata") {

  result_df <- results_list[[index]]
  cost <- scenario_grid$cost_per_unit[index]
  budget <- scenario_grid$budget[index]

  facility_summary <- result_df %>%
    group_by(facility_name, area_id) %>%
    summarise(units_prep = sum(allocated_units, na.rm = TRUE),
              pop_at_risk = sum(catchment_population_risk_strata),
              allocated = sum(allocated_units, na.rm = TRUE) > 0, .groups = "drop")

  # 1. Facility-level allocation
  facility_map_df <- facility_coords %>%
    left_join(facility_summary,
              by = c("facility_name", "area_id")) %>%
    mutate(
      allocated = ifelse(is.na(allocated), FALSE, allocated),
      units_prep = ifelse(is.na(units_prep), 0, units_prep),
      pop_at_risk = ifelse(is.na(pop_at_risk), 0, pop_at_risk),
      prep_allocated_label = ifelse(allocated, "Yes", "No")
    )

  # 2. % of facilities with PrEP by district
  district_facility_summary <- facility_map_df %>%
    group_by(area_id, district) %>%
    summarise(
      n_facilities = n(),
      n_allocated = sum(allocated, na.rm = TRUE),
      percent_allocated = 100 * n_allocated / n_facilities,
      prep_coverage = ifelse(sum(pop_at_risk, na.rm = TRUE) > 0,
                             100 * sum(units_prep, na.rm = TRUE) / sum(pop_at_risk, na.rm = TRUE), 0),
      .groups = "drop"
    ) %>%
    mutate(allocated_yn = ifelse(percent_allocated == 0, 0, 1))

  district_map1 <- left_join(district_shapefile, district_facility_summary, by = "area_id") %>%
    mutate(allocated_yn = ifelse(is.na(allocated_yn), 0, allocated_yn),
           allocation_label = factor(ifelse(allocated_yn == 1, "Yes", "No"), levels = c("No", "Yes")))

  # Subset districts that are marked as allocated
  district_labels <- district_map1 %>%
    filter(allocated_yn == 1)

  # Create plot -- use numeric fill with manual breaks to guarantee both legend entries
  p_district_facilities <- ggplot(district_map1) +
    geom_sf(aes(fill = factor(allocated_yn, levels = c(0, 1))), color = "white") +
    scale_fill_manual(
      values = c("0" = "grey80", "1" = "red"),
      labels = c("0" = "No", "1" = "Yes"),
      breaks = c("0", "1"),
      name = "Allocated",
      drop = FALSE,
      guide = guide_legend(override.aes = list(color = "grey40"))
    ) +
    ggrepel::geom_text_repel(
      data = district_labels,
      aes(geometry = geometry, label = district),
      stat = "sf_coordinates",
      size = 2.8,
      segment.color = "black",
      segment.size = 0.4,
      box.padding = 0.6,
      min.segment.length = 0.3,
      seed = 123,
      show.legend = FALSE,
      max.overlaps = 20
    ) +
    labs(title = "Districts with PrEP Allocation") +
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
    mutate(percent_pop_covered = ifelse(percent_pop_covered == 0, NA, percent_pop_covered))

  district_map2 <- left_join(district_shapefile, district_pop_summary, by = "area_id")

  district_map2 <- district_map2 %>%
    mutate(centroid = st_centroid(geometry)) %>%
    mutate(
      lon = st_coordinates(centroid)[, 1],
      lat = st_coordinates(centroid)[, 2]
    )

  p_district_pop <- ggplot(district_map2) +
    geom_sf(aes(fill = percent_pop_covered), color = "white") +
    scale_fill_viridis_c(name = "%", na.value = "grey90",
                         limits = c(0, max(district_map2$percent_pop_covered, na.rm = TRUE) + 1)) +
    labs(title = "% Catchment Population Covered by PrEP") +
    guides(fill = guide_colorbar(direction = "horizontal", barwidth = 10, barheight = 0.5)) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    ggrepel::geom_text_repel(
      data = district_map2 %>% filter(percent_pop_covered > 0),
      aes(geometry = geometry, label = sprintf("%.1f", percent_pop_covered)),
      stat = "sf_coordinates",
      size = 2.8,
      segment.color = "black",
      segment.size = 0.4,
      box.padding = 0.6,
      min.segment.length = 0.3,
      seed = 123,
      show.legend = FALSE,
      max.overlaps = 20
    )

  # 4. Incidence reduction by district
  district_inc_reduction <- result_df %>%
    group_by(area_id) %>%
    summarise(
      infections_averted = sum(infections_averted, na.rm = TRUE),
      units_allocated = sum(allocated_units, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(infections_averted > 0) %>%
    left_join(district_new_infections, by = c("area_id")) %>%
    mutate(inc_reduction = infections_averted / new_infections)

  district_map3 <- left_join(district_shapefile, district_inc_reduction, by = "area_id") %>%
    mutate(infections_averted = na_if(infections_averted, 0),
           units_allocated = na_if(units_allocated, 0),
           inc_reduction = infections_averted / new_infections * 100)

  district_map3 <- district_map3 %>%
    mutate(centroid = st_centroid(geometry)) %>%
    mutate(
      lon = st_coordinates(centroid)[, 1],
      lat = st_coordinates(centroid)[, 2]
    )

  p_district_inf_averted <- ggplot(district_map3) +
    geom_sf(aes(fill = inc_reduction), color = "white") +
    scale_fill_viridis_c(name = "%", na.value = "grey90",
                         limits = c(0, max(district_map3$inc_reduction, na.rm = TRUE) + 1)) +
    labs(title = "Reduction in incidence by district (%)") +
    guides(fill = guide_colorbar(direction = "horizontal", barwidth = 10, barheight = 0.5)) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    ggrepel::geom_text_repel(
      data = district_map3 %>% filter(inc_reduction > 0),
      aes(geometry = geometry, label = sprintf("%.1f", inc_reduction)),
      stat = "sf_coordinates",
      size = 2.8,
      segment.color = "black",
      segment.size = 0.4,
      box.padding = 0.6,
      min.segment.length = 0.3,
      seed = 123,
      show.legend = FALSE,
      max.overlaps = 20
    )

  return(list(
    district_facility_choropleth = p_district_facilities,
    district_population_choropleth = p_district_pop,
    district_infections_averted = p_district_inf_averted
  ))
}


format_prep_allocation_maps <- function(maps, result_df, incidence_df, efficacy,
                                        facility_coords_df,
                                        label_text = NULL) {

  # Compute annotation stats
  facility_stats <- facility_coords_df %>%
    left_join(
      result_df %>%
        group_by(facility_name) %>%
        summarise(allocated = sum(allocated_units, na.rm = TRUE) > 0, .groups = "drop"),
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
    "PrEP Coverage: ", prep_coverage_pct, "%   |   ",
    "% Infections Averted: ", percent_infections_averted, "%"
  )

  # Base map theme
  base_theme <- theme_void(base_size = 13) +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5, margin = margin(b = 6)),
      legend.position = "bottom",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      legend.key.height = unit(0.4, "cm"),
      legend.key.width = unit(0.8, "cm"),
      legend.margin = margin(t = 0, b = 0),
      legend.box.margin = margin(t = -5, b = -5),
      plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
    )

  p2 <- maps$district_facility_choropleth +
    base_theme +
    labs(title = "Districts with PrEP Allocation")

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


create_summary_by_province_district_age_sex <- function(result_df,
                                                        age_group_allocation_selection = c("15-24", "25-34", "35-49")) {
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


compute_dose_response_curve <- function(facility_df,
                                        incidence_df,
                                        efficacy = 0.95,
                                        coverage_mult = 1.0,
                                        age_group_allocation_selection = c("15-24", "25-34", "35-49"),
                                        sex_allocation_selection = c("male", "female"),
                                        min_risk_quantile = 0) {
  # Run a single unlimited-budget allocation to get the full ranked list,
  # then compute cumulative courses vs cumulative infections averted.

  # Use a very large budget so everything gets allocated
  max_budget <- 1e12

  result_df <- allocate_prep_by_risk_with_stratified_prob(
    facility_df = facility_df,
    incidence_df = incidence_df,
    budget = max_budget,
    cost_per_unit = 1,
    efficacy = efficacy,
    coverage_mult = coverage_mult,
    age_group_allocation_selection = age_group_allocation_selection,
    sex_allocation_selection = sex_allocation_selection,
    min_risk_quantile = min_risk_quantile
  )

  # Total expected infections across all strata

  total_expected_infections <- sum(incidence_df$inc_district * incidence_df$pop_subsample / 1000, na.rm = TRUE)

  # Walk down the ranked allocation (already sorted by descending incidence)
  # and compute cumulative courses and infections averted
  curve_df <- result_df %>%
    filter(allocated_units > 0) %>%
    arrange(desc(inc_in_sample)) %>%
    mutate(
      infections_averted_row = allocated_units * inc_in_sample / 1000 * efficacy,
      cum_courses = cumsum(allocated_units),
      cum_infections_averted = cumsum(infections_averted_row),
      pct_reduction = 100 * cum_infections_averted / total_expected_infections
    ) %>%
    dplyr::select(cum_courses, cum_infections_averted, pct_reduction, inc_in_sample,
           area_id, sex, age_group_label)

  return(list(
    curve = curve_df,
    total_expected_infections = total_expected_infections,
    max_courses = max(curve_df$cum_courses),
    max_reduction = max(curve_df$pct_reduction)
  ))
}


generate_prep_allocation_outputs <- function(facility_df,
                                             incidence_df,
                                             facility_coords_df,
                                             district_sf,
                                             district_new_infections,
                                             budget_vec,
                                             cost_per_unit_vec,
                                             selected_budget,
                                             selected_cost,
                                             efficacy = 0.95,
                                             dalys_per_infection = 20,
                                             coverage_mult = 1.0,
                                             age_group_allocation_selection = c("15-24", "25-34", "35-49"),
                                             sex_allocation_selection = c("male", "female"),
                                             risk_groups = 4,
                                             min_risk_quantile = 0) {
  # 1. Run allocation scenarios
  output <- run_cost_and_demand_scenarios(
    cost_per_unit_vec = cost_per_unit_vec,
    budget_vec = budget_vec,
    facility_df = facility_df,
    incidence_df = incidence_df,
    efficacy = efficacy,
    dalys_per_infection = dalys_per_infection,
    coverage_mult = coverage_mult,
    age_group_allocation_selection = age_group_allocation_selection,
    sex_allocation_selection = sex_allocation_selection,
    risk_groups = risk_groups,
    min_risk_quantile = min_risk_quantile
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
    district_shapefile = district_sf,
    district_new_infections = district_new_infections
  )

  # 4. Format map layout -- use actual allocated units, not requested
  actual_allocated <- sum(result_df$allocated_units, na.rm = TRUE)
  label_text <- paste0(
    "Scenario: ", format(actual_allocated, big.mark = ","), " Len courses allocated, ",
    risk_groups, " risk groups (quantiles), ",
    "Coverage cap: ", (coverage_mult) * 100, "%",
    if (min_risk_quantile > 0) paste0(", Targeting top ", round((1 - min_risk_quantile) * 100), "% risk") else ""
  )
  formatted_map <- format_prep_allocation_maps(
    maps = maps,
    result_df = result_df,
    incidence_df = incidence_df,
    label_text = label_text,
    efficacy = efficacy,
    facility_coords_df = facility_coords_df
  )

  # 5. Pull summary table
  summary_row <- output$summary[idx, ]
  facility_summary <- output$facility_summaries[[idx]]

  summaries <- create_summary_by_province_district_age_sex(
    result_df,
    age_group_allocation_selection = age_group_allocation_selection
  )

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
