#sets the directory to where this script is stored#
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#clean inputs
source("Allocate_PrEP_RSA_data_cleaning.R", echo=F)

#Set parameters and model groups
#quant_scenario_yn = 1
units = 448278

# Define all combinations of parameters
param_grid <- expand.grid(
  age_group_allocation_selection = list(
    #c("15-19", "20-24"),
    #c("15-19", "20-24", "25-34"),
    c("15-19", "20-24", "25-34", "35-49")
  ),
  sex_allocation_selection = list(
    #c("female"),
    c("female", "male")
  ), 
  quant_scenario = c(2,4,20), #set to 4 for default (e.g upper 25th percentile is the default uptake group)
  stringsAsFactors = FALSE
)

# # Define all combinations of parameters
# param_grid <- expand.grid(
#   age_group_allocation_selection = list(
#     c("15-19", "20-24","25-34","35-49")
#   ),
#   sex_allocation_selection = list(
#     c("female","male")
#   ),
#   quant_scenario = c(4),
#   stringsAsFactors = FALSE
# )

# Run all scenarios
#source("Allocate_PreP_RSA_v2_district_priority.R", echo=F) #District simulations (no facility data)
source("Allocate_PreP_RSA_v2_district_priority.R", echo=F) #District simulations (running different degrees of geographic versus demographic targeting)

for (i in seq_len(nrow(param_grid))) {
  age_group_allocation_selection <- param_grid$age_group_allocation_selection[[i]]
  sex_allocation_selection <- param_grid$sex_allocation_selection[[i]]
  quant_scenario_selection <- param_grid$quant_scenario[[i]]
  
  # Construct tag for filename
  age_tag <- if (identical(age_group_allocation_selection, c("15-19", "20-24"))) "15-24" else
    if (identical(age_group_allocation_selection, c("15-19", "20-24", "25-34"))) "15-34" else "15-49"
  sex_tag <- if (setequal(sex_allocation_selection, c("female", "male"))) "MF" else "F"
  quant_tag <- paste("quantile_n_",quant_scenario_selection,sep="")
  
  scenario_tag <- paste0("_", sex_tag, age_tag, quant_tag)
  
  # Set static inputs
  min_total_initiations <- 0
  coverage_mult <- 1
  units = units #Total courses of Len (person-years)
  #units = 448278 #Total courses of Len (person-years)
  
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
  save(outputs,
       file = paste0("RSA_OUTPUT/Len_optim_RSA_output_DISTRICT_prioritization_", scenario_tag, ".RData"))
  
  ggsave(paste0("RSA_OUTPUT/Len_optim_RSA_output_DISTRICT_prioritization_", scenario_tag, ".jpg"),
         outputs$formatted_map, width = 20, height = 8, dpi = 300)
  
  c(age_group_allocation_selection, sex_allocation_selection,quant_scenario_selection)
}
