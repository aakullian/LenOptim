#sets the directory to where this script is stored#
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source("Allocate_PrEP_RSA_data_cleaning.R", echo=F)
source("Allocate_PreP_RSA_v2.R", echo=F)

total_initiations_value = 240 #Restrict to facilities with a minimum number of historical PrEP initiations

outputs <- generate_prep_allocation_outputs(
  facility_df = facility_df,
  incidence_df = incidence_df,
  facility_coords_df = facility_coords_df,
  district_sf = district_sf,
  efficacy = 0.95,
  budget_vec = c(29e6),
  cost_per_unit_vec = c(100),
  coverage_mult_vec = c(1),
  selected_budget = 29e6,
  selected_cost = 100,
  selected_coverage_mult = 1
)

outputs$formatted_map         # view or save the map
outputs$summary_table         # one-row scenario summary
outputs$facility_summary <-
  outputs$facility_summary %>%
  filter(age_group_label!="50-99") # stratified facility table
outputs$result_df             # raw allocation output

write.csv(outputs$result_df, "result_df_min100inits_$100.csv", row.names = FALSE, na = "")
write.csv(outputs$summary_table, "summary_table_min100inits_$100.csv", row.names = FALSE, na = "")
write.csv(outputs$facility_summary, "facility_summary_table__min100inits_$100.csv", row.names = FALSE, na = "")

ggsave("formatted_maps__min100inits_$100.jpg", outputs$formatted_map, width = 20, height = 8, dpi = 300)

