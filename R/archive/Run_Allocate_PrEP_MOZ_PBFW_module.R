# --- working dir & params (as you had) ---
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#Spreadsheet used to derive the vertical transmission rate multiplier
# HIV prevalence (%)	Incidence (per 100 PY)	Exposure duration (y)	Overall VT (%)	% of VT from INCIDENT	N_prev (mothers)	N_inc (mothers)	Total exposed	Total infant VT (count)	Implied r_incident (%)	Implied r_prevalent (%)	VALID r_incident?	ratio
# 20	2	1	15	20	200	16	216	32.4	40.5	13	YES	2.7
# 20	2	1	15	30	200	16	216	32.4	60.7	11.3	YES	4.046666667
# 20	2	1	15	40	200	16	216	32.4	81	9.7	YES	5.4
# 20	2	1	15	50	200	16	216	32.4	101.2	8.1	NO	6.746666667

scenarios_to_run <- c("PBFW","NON_PBFW")  # you can keep this, but not required
min_total_initiations <- 0
units <- 34000 + 12500
age_group_allocation_selection <- c("15-24","25-34")
sex_allocation_selection <- c("female")
pregnancy_rate_ratio <- 2
preg_multiplier <- 2
vert_transmission_rate_multiplier <- 4.046666667  # from spreadsheet above
vertical_transmission_rate <- 0.162

# shared data
source("Allocate_PrEP_MOZ_data_cleaning_v2.R", echo = FALSE)

coverage_mult_vec <- c(0.25, 0.5, 0.75, 1)

for (coverage_mult in coverage_mult_vec) {
  
  ## ---------- NON-PBFW ----------
  source("Allocate_PreP_MOZ_v2_district_priority_non_PBFW.R", echo = FALSE)  # defines generate_prep_allocation_outputs for NON-PBFW
  outputs <- generate_prep_allocation_outputs(
    facility_df        = facility_df,
    incidence_df       = incidence_df,
    facility_coords_df = facility_coords_df,
    district_sf        = district_sf,
    efficacy           = 0.95,
    budget_vec         = units * 100,
    cost_per_unit_vec  = c(100),
    selected_budget    = units * 100,
    selected_cost      = 100
  )
  
  # ---- your "middle code" that must run before PBFW ----
  # (use `outputs` here exactly as your original code expected)
  # Example (keep whatever you had):
  # outputs$formatted_map
  # outputs$summary_table
  # facility_summary <- ... (your joins/renames)
  # assign(paste0("map_", coverage_mult*100, "pct"), outputs$formatted_map)
  # assign(paste0("summarytable", coverage_mult*100, "pct"), outputs$summary_table)
  
  save(outputs,
       file = sprintf("MOZ_OUTPUT/Len_optim_MOZ_output_NON_PBFW_prioritization_%d.RData",
                      as.integer(coverage_mult * 100)))
  
  ## ---------- PBFW ----------
  source("Allocate_PreP_MOZ_v2_district_priority_PBFW.R", echo = FALSE)       # overwrites generate_prep_allocation_outputs for PBFW
  outputs <- generate_prep_allocation_outputs(
    facility_df        = facility_df,
    incidence_df       = incidence_df,
    facility_coords_df = facility_coords_df,
    district_sf        = district_sf,
    efficacy           = 0.95,
    budget_vec         = units * 100,
    cost_per_unit_vec  = c(100),
    selected_budget    = units * 100,
    selected_cost      = 100
  )
  
  save(outputs,
       file = sprintf("MOZ_OUTPUT/Len_optim_MOZ_output_PBFW_prioritization_%d.RData",
                      as.integer(coverage_mult * 100)))
}

