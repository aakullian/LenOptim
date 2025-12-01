
# LenOptim Modular Functions for Naomi Analysis
# Author: ChatGPT Refactor

library(tidyverse)
library(sf)

# ----------------------------
# 1. Load all required data
# ----------------------------
load_naomi_data <- function(path_adm0, path_adm0_data, path_adm2_data, path_shp) {
  list(
    adm0 = readRDS(path_adm0_data),
    adm2 = readRDS(path_adm2_data),
    shp = read_sf(path_shp),
    africa_adm0 = read_sf(path_adm0)
  )
}

# ----------------------------
# 2. Prepare national-level ADM2 estimates
# ----------------------------
prep_naomi_adm2 <- function(naomi_data, iso3, sex_groups, age_groups,
                            indicators = c("incidence", "prevalence", "population")) {
  naomi_data$adm2 %>%
    filter(
      iso3 %in% iso3,
      sex %in% sex_groups,
      age_group_label %in% age_groups,
      area_level == 2,
      indicator %in% indicators
    ) %>%
    group_by(sex, age_group_label, area_id, indicator) %>%
    arrange(calendar_quarter) %>%
    mutate(calendar_n = row_number()) %>%
    filter(calendar_n == max(calendar_n)) %>%
    pivot_wider(names_from = indicator, values_from = mean)
}

# ----------------------------
# 3. Simulate PrEP allocation by incidence threshold
# ----------------------------
simulate_allocation_by_threshold <- function(naomi_df, units_prep, inc_seq,
                                             time_horizon = 4, efficacy = 0.95,
                                             duration = 1, prep_cost = 100,
                                             tx_cost = 10000, dalys = 20, cdt = 500) {
  results <- list()
  for (i in inc_seq) {
    for (j in units_prep) {
      sim <- naomi_df %>%
        filter(incidence > i, sex != "both", age_group_label != "15-49") %>%
        mutate(
          pop_at_risk = population * (1 - prevalence),
          prop_at_risk = pop_at_risk / sum(pop_at_risk),
          units_allocated = j * prop_at_risk,
          infections_averted = units_allocated / (1 / (efficacy * incidence * duration))
        ) %>%
        summarise(
          incidence_threshold = i,
          units_prep = j,
          total_units = sum(units_allocated),
          infections_averted = sum(infections_averted),
          avg_incidence = weighted.mean(incidence, w = pop_at_risk),
          nnt = 1 / (efficacy * avg_incidence * duration),
          cost_per_daly = (nnt * prep_cost - tx_cost) / dalys,
          pt = (tx_cost + dalys * cdt) / nnt
        )
      results <- append(results, list(sim))
    }
  }
  bind_rows(results)
}

# ----------------------------
# 4. Simulate risk-based targeting using quantile thresholds
# ----------------------------
simulate_risk_targeting <- function(inc, shape, quant_target, samplesize = 100000,
                                    coverage = 1, efficacy = 0.95, duration = 1,
                                    prep_cost = 130, tx_cost = 10000, daly = 20, cdt = 500) {
  x <- rgamma(n = samplesize, shape = shape, scale = inc / shape)
  x <- round(x * 1000, 2)
  pop <- tibble(x = x) %>%
    mutate(
      infected = rbinom(n(), 1, prob = x / 1000),
      quantile = rank(x) / n()
    ) %>%
    filter(quantile > quant_target) %>%
    summarise(
      inc_in_sample = mean(x),
      infections_averted = sum(infected) * efficacy,
      nnt = 1 / (efficacy * inc_in_sample / 1000 * duration),
      cost_per_daly = (nnt * prep_cost - tx_cost) / daly,
      pt = (tx_cost + daly * cdt) / nnt
    )
}
