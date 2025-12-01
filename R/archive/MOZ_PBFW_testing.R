library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
library(purrr)
library(here)
library(kableExtra)
library(stringr)
library(purrr)
library(here)
library(gt)

# Optionally confirm the root:
here::here()  # prints the project root path

# Set working directory to a folder relative to the root
setwd(here("R/MOZ_OUTPUT/Distirct_allocation_PBFW_With_addl_PEPFAR_alloc"))  # change folder name as needed

# List all RData files matching a naming pattern
files <- list.files(pattern = "^Len_optim_MOZ_output_.*\\.RData$")

# Optionally extract scenario names from filenames
scenario_labels <- gsub("\\.RData", "", basename(files))

# Load each RData into a separate environment
envs <- lapply(files, function(f) {
  e <- new.env()
  load(f, envir = e)
  e
})

scenario_tables <- map2(envs, scenario_labels, ~{
  .x$outputs$summary_table %>%
    mutate(scenario = .y)
})

scenario_maps <- map2(envs, scenario_labels, ~{
  .x$outputs$formatted_map
})

scenario_district_allocation <- map2(envs, scenario_labels, ~{
  .x$outputs$by_prov_dist_age_sex %>%
    mutate(scenario = .y)
})

combined_summary <- bind_rows(scenario_tables)

x <- "Len_optim_MOZ_output_PBFW_prioritization_10.3"
num <- as.numeric(str_extract(x, "\\d+\\.\\d+"))
num

combined_summary_table <- combined_summary %>%
  select(-c(budget, cost_per_unit, total_cost))  %>%
  select(scenario, allocation_by_age_sex, everything())  %>%
  mutate(
    `coverage district/age/gender (%)` = as.numeric(str_extract(scenario, "\\d+\\.\\d+")),
  ) %>%
  relocate(`coverage district/age/gender (%)`, .after = 1) %>%  # move 'version' after the first column
  rename(
    `scenario_id` = scenario,
    #`Budget (USD)` = budget,
    #`Cost per Unit` = cost_per_unit,
    #`Coverage Multiplier` = coverage_mult,
    `Total Units Allocated` = total_allocated_units,
    `Expected Infections (No PrEP)` = expected_infections_no_prep,
    `Infections Averted` = infections_averted,
    `Incidence Reduction (%)` = percent_reduction_in_incidence,
    `PrEP Coverage (%)` = prep_coverage,
    `Total DALYs Averted` = total_dalys_averted,
    #`Total Cost (USD)` = total_cost,
    `Cost per Infection Averted` = cost_per_infection_averted,
    `Cost per DALY Averted` = cost_per_daly_averted,
    `Number Needed to Treat (NNT)` = number_needed_to_treat,
    `Districts Allocated` = facilities_with_allocation,
    `Avg Incidence (Allocated)` = avg_incidence_allocated,
    `Avg Incidence (Population)` = avg_incidence_population,
    `Targeting Ratio` = incidence_targeting_ratio
    #`Allocation by Age/Sex/KP` = allocation_by_age_sex
  )  %>%
  mutate(
    # Strategy description
    scenario_description = case_when(
      str_detect(scenario_id, "PBFW") ~ "Restrict to PBFW only",
      str_detect(scenario_id, "GENPOP") ~ "General population",
      TRUE ~ "Uncategorized Strategy"
    )
  ) %>%
  relocate(scenario_description, .after = scenario_id) %>%
  select(-scenario_id) %>%
  mutate(
    `Expected Infections (No PrEP)` = formatC(`Expected Infections (No PrEP)`, format = "f", digits = 0),
    `Infections Averted` = formatC(`Infections Averted`, format = "f", digits = 0),
    `Incidence Reduction (%)` = formatC(`Incidence Reduction (%)`, format = "f", digits = 1),
    `PrEP Coverage (%)` = formatC(`PrEP Coverage (%)`, format = "f", digits = 1),
    `Total DALYs Averted` = formatC(`Total DALYs Averted`, format = "f", digits = 0),
    `Cost per Infection Averted` = paste0("$", formatC(`Cost per Infection Averted`, format = "f", digits = 0, big.mark = ",")),
    `Cost per DALY Averted` = paste0("$", formatC(`Cost per DALY Averted`, format = "f", digits = 0, big.mark = ",")),
    `Number Needed to Treat (NNT)` = formatC(`Number Needed to Treat (NNT)`, format = "f", digits = 0),
    `Avg Incidence (Allocated)` = formatC(`Avg Incidence (Allocated)`, format = "f", digits = 1),
    `Avg Incidence (Population)` = formatC(`Avg Incidence (Population)`, format = "f", digits = 1),
    `Targeting Ratio` = formatC(`Targeting Ratio`, format = "f", digits = 1) 
  ) %>%
  arrange(scenario_description,`coverage district/age/gender (%)`) 


combined_summary_district <- bind_rows(scenario_district_allocation) 

# 1. Keep all non-numeric columns + numeric columns that are not all zeros
non_zero_cols <- names(combined_summary_district)[map_lgl(combined_summary_district, ~ !is.numeric(.x) || !all(.x %in% c(0, NA)))]

combined_summary_district = combined_summary_district  %>%
  
  select(all_of(non_zero_cols)) %>%
  
  # 2. Rename KP columns (and their percent versions)
  rename_with(~ str_replace_all(.x, c(
    "anc_15-49" = "Antenatal care",
    "gbmsm_15-49" = "Men who have sex with men",
    "sw_15-49" = "Female sex workers",
    "tgw_15-49" = "Transgender women",
    "anc_15-49_pct" = "Antenatal care pct",
    "gbmsm_15-49_pct" = "Men who have sex with men pct",
    "sw_15-49_pct" = "Female sex workers pct",
    "tgw_15-49_pct" = "Transgender women pct"
  ))) %>%
  
  # 3. Remove underscores from all column names
  rename_with(~ gsub("_", " ", .x)) %>%
  
  # 4. Move `scenario` column to the front
  relocate(scenario) %>%
  
  # 5. Reorder: each percent column comes right after its count column
  {
    cols <- names(.)
    pct_cols <- cols[grepl(" pct$", cols)]
    base_cols <- cols[!cols %in% pct_cols & cols != "scenario"]
    ordered_cols <- c("scenario")
    
    for (base in base_cols) {
      ordered_cols <- c(ordered_cols, base)
      pct_match <- paste0(base, " pct")
      if (pct_match %in% pct_cols) {
        ordered_cols <- c(ordered_cols, pct_match)
      }
    }
    
    select(., all_of(ordered_cols))
  } %>%
  
  # 6. Add (count) and (percent) labels to numeric columns
  rename_with(~ if_else(
    str_detect(.x, " pct$"),
    paste0(str_remove(.x, " pct$"), " (percent)"),
    paste0(.x, " (count)")
  ),
  .cols = where(is.numeric))

