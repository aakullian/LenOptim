#Run scenario with NDOH prioritization (restrcted to 15-24 F for general allocation) and including KP scenario. 

#sets the directory to where this script is stored (skipped when knitting / non-interactive)#
if (interactive() && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}

#cleaning code (run for all scenarios)
source("00_data_cleaning.R", echo=F)

#run all functions
source("01_model_ndoh_kp.R", echo=F) #District simulations (no facility data)

# --- Harmonise volume with Lise (Thembisa) Phase 1: 500,000 person-years ---
# The published NDOH plan totals ~448k PY; scale its per-group initiation counts
# proportionally so total allocation = 500k, preserving the plan's population mix.
TARGET_PY  <- 500000
EFFICACY   <- 0.99   # Lise used 99% (PURPOSE trials); previously 0.95
scale_cols <- intersect(c("GenPop_initiations","KP_initiations","ANC_initiations",
                          "SW.Len.Initiations","GBMSM.Len.Initiations","TG.Len.Initiations",
                          "Total_initiations","Total.Len.Initiations"), names(facility_df))
.sf <- TARGET_PY / sum(facility_df$Total.Len.Initiations, na.rm = TRUE)
facility_df <- facility_df %>% dplyr::mutate(dplyr::across(dplyr::all_of(scale_cols), ~ .x * .sf))

# Define all combinations of parameters
param_grid <- expand.grid(
  age_group_allocation_selection = list(
    #c("15-19", "20-24"),
    #c("15-19", "20-24", "25-34"),
    c("15-19", "20-24", "25-34", "35-49")
  ),
  sex_allocation_selection = list(
    #c("female")
    c("female", "male")
  ),
  kp_selection = c(1),   # KP scenario only; NDOH general-population run dropped
  stringsAsFactors = FALSE
)

# Run all scenarios
for (i in seq_len(nrow(param_grid))) {
  age_group_allocation_selection <- param_grid$age_group_allocation_selection[[i]]
  sex_allocation_selection <- param_grid$sex_allocation_selection[[i]]
  kp_selection <- param_grid$kp_selection[i]
  
  # Construct tag for filename
  age_tag <- if (identical(age_group_allocation_selection, c("15-19", "20-24"))) "15-24" else
    if (identical(age_group_allocation_selection, c("15-19", "20-24", "25-34"))) "15-34" else "15-49"
  sex_tag <- if (setequal(sex_allocation_selection, c("female", "male"))) "MF" else "F"
  kp_tag  <- if (kp_selection == 1) "KP" else "GP"
  
  scenario_tag <- paste0(kp_tag, "_", sex_tag, age_tag)
  
  # Set static inputs
  min_total_initiations <- 0
  units <- TARGET_PY
  coverage_mult <- 1

  # Run model
  outputs <- generate_prep_allocation_outputs(
    facility_df = facility_df,
    incidence_df = incidence_df,
    facility_coords_df = facility_coords_df,
    district_sf = district_sf,
    efficacy = EFFICACY,
    budget_vec = units * 100,
    cost_per_unit_vec = c(100),
    selected_budget = units * 100,
    selected_cost = 100
  )
  
  # Save outputs with informative name
  save(outputs, ndoh_len_plan_plot, ndoh_len_plan_plot_scatter,
       file = paste0("output/Len_optim_RSA_output_NDOH_prioritization_", scenario_tag, ".RData"))

  ggsave(paste0("output/Len_optim_RSA_output_NDOH_prioritization_", scenario_tag, ".jpg"),
         outputs$formatted_map, width = 20, height = 8, dpi = 300)
}
