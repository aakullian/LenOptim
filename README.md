# LenOptim


# 📚 LenOptim R Function Documentation

This document provides descriptions for all major functions used in the LenOptim PrEP allocation pipeline. 

**Primary R scripts:**
1. NAOMI_config_Allocate_PrEP_RSA.R - creates incidence_df (the incidence input file from NAOMI data)
2. Allocate_PrEP_RSA_v2 - generates a PrEP allocation strategy using incidence_df and facility_df (facilty name, lat/lon, district, previous PrEP capacity, catchment populations by age and gender)
3. Run_Allocate_PrEP_RSA - runs Allocate_PrEP_RSA

---

## 1. `generate_prep_allocation_outputs()`

**Description:**  
Runs the full PrEP allocation pipeline and returns formatted map outputs, summary table, and result dataframe for a selected scenario.

**Arguments:**
- `facility_df`: DataFrame with facility-level population and initiation data
- `incidence_df`: DataFrame with incidence by district, age, sex, risk group
- `facility_coords_df`: Coordinates for mapping each facility
- `district_sf`: District shapefile (sf object)
- `budget_vec`: Vector of budgets to simulate
- `cost_per_unit_vec`: Vector of unit costs to simulate
- `coverage_mult_vec`: Vector of the proportion of a age/gender/risk/facility sub-population to allocate PrEP (e.g. 1, 0.8, etc.)
- `selected_budget`: Budget to plot
- `selected_cost`: Unit cost to plot
- `selected_coverage_mult`: Coverage multiplier for selected scenario
- `efficacy`: Efficacy of PrEP
- `dalys_per_infection`: DALYs per HIV infection

**Returns:**
- `formatted_map`: Patchwork of 3 ggplots
- `summary_table`: One-row summary of selected scenario
- `facility_summary`: Stratified allocation table showing number of PrEP units allocated per facility by age and gendder
- `result_df`: Row-level allocation results for one scenario

---

## 2. `run_cost_and_demand_scenarios()`

**Description:**  
Runs allocation model across combinations of cost, budget, and coverage.

**Returns:**
- `summary`: Scenario-level metrics
- `facility_summaries`: List of facility-level summaries
- `results_list`: List of allocation dataframes
- `scenario_grid`: Scenario parameters

---

## 3. `allocate_prep_by_risk_with_stratified_prob()`

**Description:**  
Allocates PrEP to facility-age-sex-risk strata, prioritizing highest incidence within budget.

**Key Logic:**
- Filters facilities by `min_total_initiations`
- Crosses population with risk strata
- Joins with incidence estimates
- Ranks strata by `inc_in_sample` and `total_initiations`
- Allocates `coverage_mult * population` until units run out

---

## 4. `summarize_allocation_scenarios()`

**Description:**  
Summarizes each scenario with:
- Total units
- PrEP coverage
- Infections averted
- Cost per infection averted
- Incidence targeting ratio

**Returns:**
- `summary`: Table of scenario-level stats
- `facility_summaries`: List of facility-level outputs

---

## 5. `select_scenario_result()`

**Description:**  
Retrieves a single scenario by budget, cost, and coverage multiplier.

**Returns:**
- `result_df`
- `summary_row`
- `index` (scenario index in results list)

---

## 6. `map_prep_allocation_scenario()`

**Description:**  
Creates 3 maps from selected allocation results:
1. Facility point map (Yes/No allocation)
2. % facilities allocated per district
3. % of catchment population covered

**Returns:** List of ggplots

---

## 7. `format_prep_allocation_maps()`

**Description:**  
Combines maps and adds footer text with:
- Facility coverage
- PrEP coverage
- Infections averted

Use in publication figures or reports.

---
