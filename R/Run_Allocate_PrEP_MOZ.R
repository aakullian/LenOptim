#sets the directory to where this script is stored#
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source("Allocate_PrEP_MOZ_data_cleaning_v2.R", echo=F) #Run data cleaning code

source("Allocate_PreP_MOZ_v1.R", echo=F) #Load model functions

#Set parameters and run model
min_total_initiations = 0 #Restrict to facilities with a minimum number of historical PrEP initiations
coverage_mult = 1 #The largest fraction of the target population that can receive Len.  So if set to 0.5, half of the catchment population at a facility is allocated Len.  This spreads Len availability to more facilities.
units = 34000 #Total courses of Len (person-years)

outputs <- generate_prep_allocation_outputs(
  facility_df = facility_df, #facility data
  incidence_df = incidence_df, #naomi incidence data
  facility_coords_df = facility_coords_df, #facility lat/lon
  district_sf = district_sf, #districts shapefile
  efficacy = 0.95, #len efficacy
  budget_vec = units*100, #budget at a certain price (set at $100/py)
  cost_per_unit_vec = c(100), #Vector to run scenarios under different Len prices
  selected_budget = units*100, #total budget
  selected_cost = 100, #cost selected for output from vost vector
)

#View outputs
outputs$formatted_map         # view or save the map
outputs$summary_table         # one-row scenario summary
outputs$facility_summary <- outputs$facility_summary %>% filter(age_group_label!="50-99", prep_units>0) # stratified facility table
outputs$result_df             # raw allocation output
outputs$by_province  #district/age/sex allocation of prep
outputs$by_prov_dist_age_sex <- outputs$by_prov_dist_age_sex[,c(1:5,7:9)]

#Write outputs
write.csv(outputs$result_df, "MOZ_OUTPUT/result_df.csv", row.names = FALSE, na = "")
write.csv(outputs$summary_table, "MOZ_OUTPUT/summary_table_34000PY.csv", row.names = FALSE, na = "")
write.csv(outputs$facility_summary, "MOZ_OUTPUT/facility_summary_table_34000PY.csv", row.names = FALSE, na = "")
write.csv(outputs$by_province, "MOZ_OUTPUT/prep_by_prov_34000PY.csv", row.names = FALSE, na = "")
write.csv(outputs$by_prov_dist_age_sex %>% filter(age_group_label!="50-99"), "MOZ_OUTPUT/prep_by_dist_age_sex_34000PY.csv", row.names = FALSE, na = "")
ggsave("MOZ_OUTPUT/formatted_maps_340000PY.jpg", outputs$formatted_map, width = 20, height = 8, dpi = 300)


