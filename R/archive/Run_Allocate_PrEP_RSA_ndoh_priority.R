#Run scenario with NDOH prioritization (restrcted to 15-24 F for general allocation) and including KP scenario. 

#sets the directory to where this script is stored#
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#cleaning code (run for all scenarios)
source("Allocate_PrEP_RSA_data_cleaning.R", echo=F)

#run all functions
source("Allocate_PreP_RSA_v2_ndoh_priority_KP.R", echo=F) #District simulations (no facility data)

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
  kp_selection = c(1,0),
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
  units <- sum(facility_df$Total.Len.Initiations)
  coverage_mult <- 1
  
  # Run model
  outputs <- generate_prep_allocation_outputs(
    facility_df = facility_df,
    incidence_df = incidence_df,
    facility_coords_df = facility_coords_df,
    district_sf = district_sf,
    efficacy = 0.95,
    budget_vec = units * 100,
    cost_per_unit_vec = c(100),
    selected_budget = units * 100,
    selected_cost = 100
  )
  
  # Save outputs with informative name
  save(outputs, ndoh_len_plan_plot, ndoh_len_plan_plot_scatter,
       file = paste0("RSA_OUTPUT/Len_optim_RSA_output_NDOH_prioritization_", scenario_tag, ".RData"))
  
  ggsave(paste0("RSA_OUTPUT/Len_optim_RSA_output_NDOH_prioritization_", scenario_tag, ".jpg"),
         outputs$formatted_map, width = 20, height = 8, dpi = 300)
}
