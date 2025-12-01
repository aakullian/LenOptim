############################################################################################################
# Runs Len allocation model:
# 1. Sets parameters
# 2. Loads data cleaning and model code
# 3. Runs model
############################################################################################################

#sets the directory to where this script is stored#
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#Set parameters and run model
country_iso = "KEN" #options are ZAF","SWZ","LSO","MOZ",'ZWE',"BWA","MWI","ZMB","KEN","TZA","UGA"
min_total_initiations = 0 #Set to zero to allow all facilities to be eligible for Len.  Set higher to restrict to facilities with at least that many initiations in the last year.
units = 100000 #Total courses of Len (person-years). Set here if volumn is known and set below if budget and price / course is known
cost_per_unit = 100 #Cost per course of Len in USD
budget = units * cost_per_unit #Total budget for Len. 
age_group_allocation_selection = c("15-24","25-34","35-49") #Restrict eligible groups by age
sex_allocation_selection = c("female","male") #Restrict eligible groups by sex
coverage_mult = 0.5  #The largest fraction of the target population that can receive Len.  So if set to 0.5, half of the population at risk at a facility or district is allocated Len.  This spreads Len availability to more facilities.
efficacy = 0.95 #len efficacy

#Load data cleaning code
source("Allocate_PrEP_Data_Cleaning_All_Country.R", echo=F) 
#load model code
source("Allocate_PreP_All_Country_Model.R", echo=F) #Load model functions

#Run model:
outputs <- generate_prep_allocation_outputs(
    facility_df = facility_df, #facility data
    incidence_df = incidence_df, #naomi incidence data
    facility_coords_df = facility_coords_df, #facility lat/lon
    district_sf = district_sf, #districts shapefile
    efficacy = 0.95, #len efficacy
    budget_vec = budget, #Vector to run scenarios under different budgets. Leave as is if units defined above
    cost_per_unit_vec = cost_per_unit, #Vector to run scenarios under different Len prices. Leave as is if units defined above
    selected_budget = budget, #total budget. Leave as is if units defined above
    selected_cost = cost_per_unit #cost selected for output from cost vector. Leave as is if units defined above
  )
  
#View outputs
outputs$formatted_map         # view or save the map
outputs$summary_table         # one-row scenario summary
outputs$result_df             # raw allocation output
by_district_age_sex <- outputs$by_prov_dist_age_sex %>% 
  filter(rowSums(across(where(is.numeric))) != 0) %>% 
  mutate(Total = rowSums(across(where(is.numeric))),Percent = round(Total/sum(Total) * 100,1)) 
name= paste("map_", coverage_mult*100,"pct", sep="") 
assign(name, outputs$formatted_map)
name= paste("summarytable", coverage_mult*100,"pct", sep="") 
assign(name,   outputs$summary_table)
  
#Write outputs
save(outputs, file=paste("LenOutputData",".RData", sep="")) 


