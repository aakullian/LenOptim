# data_loader.R
# Loads pre-computed Stage 1 .RData files and reshapes them into model-ready dataframes.
# Refactored from Allocate_PrEP_Data_Cleaning_All_Country.R to eliminate global variable dependencies.

load_country_data <- function(country_iso, risk_groups, data_dir) {
  # Load the pre-computed .RData file for the given country and risk group setting.
  #

  # Args:
  #   country_iso: ISO3 country code (e.g., "ZMB")
  #   risk_groups: Number of risk groups (1, 4, or 8)
  #   data_dir: Path to directory containing .RData files
  #
  # Returns:
  #   Named list with: facility_df, incidence_df, facility_coords_df, district_sf, district_new_infections
  #   Or list(error = "message") if file not found.

  file_to_load <- file.path(data_dir, paste0("Len_optim_data_", country_iso, "_", risk_groups, "_risk_groups.RData"))

  if (!file.exists(file_to_load)) {
    return(list(error = paste0(
      "Pre-computed data not available for ", country_iso,
      " with ", risk_groups, " risk group(s). ",
      "Please run generate_all_data.R first."
    )))
  }

  # Load into a temporary environment to avoid polluting global env
  tmp_env <- new.env(parent = emptyenv())
  load(file_to_load, envir = tmp_env)

  # Extract the two key objects
  naomi_ssa_shp_m <- tmp_env$naomi_ssa_shp_m
  obj_name <- paste0("naomi_risk_dist_targeting_", country_iso, "_", risk_groups, "_risk_groups")
  data <- tmp_env[[obj_name]]

  # Build district shapefile
  district_sf <- naomi_ssa_shp_m %>%
    filter(iso3 == country_iso) %>%
    dplyr::select(iso3, area_id, area_name, geometry) %>%
    distinct()

  # Build facility dataframe (district-level placeholder)
  facility_df <- data.frame(naomi_ssa_shp_m) %>%
    filter(iso3 == country_iso, sex != "both",
           age_group_label %in% c("15-24", "25-34", "35-49", "50+")) %>%
    dplyr::select(area_id, area_name, sex, age_group_label, pop_at_risk) %>%
    mutate(
      province = NA,
      district = area_name,
      sex_abbrev = ifelse(sex == "female", "f", "m"),
      age_group_label = gsub("-", " - ", age_group_label),
      new_col = paste0(sex_abbrev, "_", age_group_label)
    ) %>%
    dplyr::select(province, area_id, district, new_col, pop_at_risk) %>%
    pivot_wider(names_from = new_col, values_from = pop_at_risk) %>%
    mutate(total_initiations = 0,
           facility_name = paste("district_", area_id, sep = ""),
           latitude = NA, longitude = NA)

  # Build facility coordinates dataframe
  facility_coords_df <- facility_df %>%
    dplyr::select(facility_name, latitude, longitude, district, area_id)

  # Build incidence dataframe from risk distribution data
  n_quants <- length(unique(data$quant_target))
  incidence_df <- data %>%
    mutate(
      quantile_target_factor = paste(quant_target - 1 / n_quants, "-", quant_target),
      inc_mult_group = cut(inc_mult, c(0, 0.5, 1, 2, Inf), right = FALSE),
      age_group_label = recode(age_group_label, "50+" = "50-99")
    )

  # Summary of new infections and pop at risk by district
  district_new_infections <- data.frame(incidence_df) %>%
    dplyr::select(area_id, sex, age_group_label, total_infected_subsample, pop_subsample) %>%
    group_by(area_id) %>%
    summarize(
      new_infections = sum(total_infected_subsample),
      pop_at_risk = sum(pop_subsample),
      .groups = "drop"
    )

  return(list(
    facility_df = facility_df,
    incidence_df = incidence_df,
    facility_coords_df = facility_coords_df,
    district_sf = district_sf,
    district_new_infections = district_new_infections
  ))
}
