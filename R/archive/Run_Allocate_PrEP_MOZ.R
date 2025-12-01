#sets the directory to where this script is stored#
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#Set parameters and run model
scenarios_to_run = c("PBFW","NON_PBFW") #options are "NON_PBFW", "PBFW" 
min_total_initiations = 0 #Restrict to facilities with a minimum number of historical PrEP initiations
units = 34000+12500 #Total courses of Len (person-years) + added PEPFAR allocation
age_group_allocation_selection = c("15-24","25-34") #Restrict eligible groups to 15-24 female
sex_allocation_selection = c("female")
pregnancy_rate_ratio = 2 #relative ratio of pregnancies per pop in 15-24 versus 25-34 age group
preg_multiplier = 2 #relative risk of HIV in pregnancy (versus non-pregnant women), the weighted average will = overall incidence in the strata
vert_transmission_rate_multiplier = 3 #relative ratio of vertical transmission risk in incident HIV versus prevalent HIV
                                      #must not exceed 1/0.12 (so the overall vertical transmission rate does not exceed 100%)

#Load data cleaning code
source("Allocate_PrEP_MOZ_data_cleaning_v2.R", echo=F) 

#Non-PBFW
source("Allocate_PreP_MOZ_v2_district_priority_non_PBFW.R", echo=F) #Load model functions
#PBFW
source("Allocate_PreP_MOZ_v2_district_priority_PBFW.R", echo=F) #Load model functions for Pregnant women (includes estimate of vertical transmission)

for(i in c(0.25,0.5,0.75,1)){
  
  coverage_mult = i  #The largest fraction of the target population that can receive Len.  So if set to 0.5, half of the catchment population at a facility is allocated Len.  This spreads Len availability to more facilities.

  outputs <- generate_prep_allocation_outputs(
    facility_df = facility_df, #facility data
    incidence_df = incidence_df, #naomi incidence data
    facility_coords_df = facility_coords_df, #facility lat/lon
    district_sf = district_sf, #districts shapefile
    efficacy = 0.95, #len efficacy
    budget_vec = units*100, #budget at a certain price (set at $100/py)
    cost_per_unit_vec = c(100), #Vector to run scenarios under different Len prices
    selected_budget = units*100, #total budget
    selected_cost = 100 #cost selected for output from vost vector
    #coverage_mult_vec = c(1),
    #selected_coverage_mult = 1
  )
  
  #View outputs
  outputs$formatted_map         # view or save the map
  outputs$summary_table         # one-row scenario summary
  facility_summary <- outputs$facility_summary %>% 
    filter(prep_units>0) %>% 
    rename(area_id = facility_name) %>%
    mutate(area_id = sub("district_", "", area_id))  %>%
    left_join(district_df, by="area_id") %>%
    rename(district = area_name) %>%
    select(-c(area_id, iso3)) %>%
    select(district, everything())
  
  # stratified facility table 
  outputs$result_df             # raw allocation output
  outputs$by_province  #district/age/sex allocation of prep
  by_prov_dist_age_sex <- outputs$by_prov_dist_age_sex %>% filter(rowSums(across(where(is.numeric))) != 0) %>% mutate(Total = rowSums(across(where(is.numeric))),
                                                                                                                      Percent = round(Total/sum(Total) * 100,1)) 
  name= paste("map_", coverage_mult*100,"pct", sep="") 
  assign(name, outputs$formatted_map)
  
  name= paste("summarytable", coverage_mult*100,"pct", sep="") 
  assign(name,   outputs$summary_table)
  
  #Write outputs Gen Pop
  save(outputs, file=paste("MOZ_OUTPUT/Len_optim_MOZ_output_PBFW_prioritization_", coverage_mult*100,".RData", sep="")) 
  #save(outputs, file=paste("MOZ_OUTPUT/Len_optim_MOZ_output_NON_PBFW_prioritization_", coverage_mult*100,".RData", sep=""))  
}


