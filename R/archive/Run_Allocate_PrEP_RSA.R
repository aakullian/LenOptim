#sets the directory to where this script is stored#
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source("Allocate_PrEP_RSA_data_cleaning.R", echo=F)
source("Allocate_PreP_RSA_v3.R", echo=F) #facility simulations

total_initiations_value = 0 #Restrict to facilities with a minimum number of historical PrEP initiations
#prep_units = 1500000 #Total courses of Len (person-years)
#budget = 100*prep_units
coverage_mult = 1

outputs <- generate_prep_allocation_outputs(
  facility_df = facility_df,
  incidence_df = incidence_df,
  facility_coords_df = facility_coords_df,
  district_sf = district_sf,
  efficacy = 0.95,
  budget_vec = c(29000000,29000000*2),
  cost_per_unit_vec = c(100,60,40),
  coverage_mult_vec = c(coverage_mult),
  selected_budget = 29000000,
  selected_cost = 60,
  selected_coverage_mult = coverage_mult
)

outputs$formatted_map         # view or save the map
outputs$summary_table         # one-row scenario summary
outputs$facility_summary <-
  outputs$facility_summary %>%
  filter(age_group_label!="50-99") # stratified facility table
outputs$result_df             # raw allocation output

## Save R data file ##
save(outputs, file = "Len_optim_RSA_output_FacilityAnalysis.RData")

write.csv(outputs$result_df, "result_df_min100inits_$100.csv", row.names = FALSE, na = "")
write.csv(outputs$summary_table, "summary_table_min100inits_$100.csv", row.names = FALSE, na = "")
write.csv(outputs$facility_summary, "facility_summary_table__min100inits_$100.csv", row.names = FALSE, na = "")

ggsave("RSA_OUTPUT/formatted_maps_Facility_483000.jpg", outputs$formatted_map, width = 20, height = 8, dpi = 300)

