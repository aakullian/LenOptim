# =============================================================================
# Volume sensitivity: sweep the total number of Lenacapavir person-years at a
# fixed targeting intensity (universal delivery, quant 4; general population
# MF 15-49) to show how the average incidence of those reached, and therefore
# the marginal impact per person-year, declines as volume grows and allocation
# is forced into progressively lower-incidence districts/strata.
#
# Complements Jamieson et al. Phase 2 (large-scale roll-out): our static model
# cannot capture continuation duration or secondary transmission over 20 years,
# but it does capture the saturation of high-incidence targets as volume scales.
#
# Writes output/volume_sensitivity.RData (data.frame `volume_sensitivity`).
# =============================================================================

#sets the directory to where this script is stored (skipped when knitting / non-interactive)#
if (interactive() && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}

source("00_data_cleaning.R", echo = FALSE)
source("02_model_district_priority.R", echo = FALSE)

library(dplyr); library(purrr)

# Fixed targeting for the sweep: universal delivery (quant 4), general pop MF 15-49
age_group_allocation_selection <- c("15-19", "20-24", "25-34", "35-49")
sex_allocation_selection       <- c("female", "male")
quant_scenario_selection       <- 4
coverage_mult                  <- 1
min_total_initiations          <- 0

# Person-year volumes to sweep (0.5M = the Global Fund donation; up to Phase-2 scale)
volume_vec <- c(250000, 500000, 1000000, 2000000, 3000000, 5000000)

volume_sensitivity <- map_dfr(volume_vec, function(units) {
  out <- run_cost_and_demand_scenarios(         # summary path only (no map rendering)
    cost_per_unit_vec = 100,
    budget_vec        = units * 100,
    facility_df       = facility_df,
    incidence_df      = incidence_df,
    efficacy          = 0.99)
  s <- out$summary[1, ]
  tibble(
    volume_requested   = units,
    py_allocated       = s$total_allocated_units,
    infections_averted = s$infections_averted,
    avg_incidence      = s$avg_incidence_allocated,
    targeting_ratio    = s$incidence_targeting_ratio,
    districts          = s$facilities_with_allocation,
    pct_reduction      = s$percent_reduction_in_incidence)
}) %>%
  mutate(per_100k = infections_averted / py_allocated * 1e5)   # marginal efficiency

save(volume_sensitivity, file = "output/volume_sensitivity.RData")
