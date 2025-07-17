source("Allocate_PreP_RSA_v2.R", echo=F)

outputs <- generate_prep_allocation_outputs(
  facility_df = facility_df,
  incidence_df = incidence_df,
  facility_coords_df = facility_coords_df,
  district_sf = district_sf,
  budget_vec = c(29e6),
  cost_per_unit_vec = c(60, 100, 120),
  coverage_mult_vec = c(1, 0.9, 0.8, 0.7,0.6,0.5),
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

write.csv(outputs$summary_table, "summary_table.csv", row.names = FALSE, na = "")
write.csv(outputs$summary_table, "summary_table.csv", row.names = FALSE, na = "")
write.csv(outputs$facility_summary, "facility_summary_table_120.csv", row.names = FALSE, na = "")
ggsave("formatted_maps_120.jpg", outputs$formatted_map, width = 20, height = 8, dpi = 300)

