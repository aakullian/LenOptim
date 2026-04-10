# LenOptim: Lenacapavir PrEP Allocation Optimization

## Project Goal

Optimize the sub-national (district-level) allocation of Lenacapavir (Len) for HIV prevention (PrEP) across countries in sub-Saharan Africa. The model determines which districts, age groups, sex groups, and risk strata should receive a constrained supply of Len to maximize infections averted, given a fixed budget or number of courses.

## Overview

Lenacapavir is a long-acting injectable HIV prevention drug. Because supply is limited and costly, it must be allocated strategically. This project uses UNAIDS Naomi model estimates of district-level HIV incidence, prevalence, and population size to simulate within-district heterogeneity in risk (via a gamma distribution) and then allocates Len to the highest-risk population strata first, across all districts and demographic groups.

## Pipeline

The model runs in two stages:

### Stage 1: Data Preparation (`R/NAOMI_config_Allocate_PrEP_allcountry_v3.R`)

Generates a country-specific `.RData` file containing district-level risk distributions.

**What it does:**
1. Loads Africa shapefiles and UNAIDS Naomi subnational HIV estimates (`naomi3_2024_07_01.rds`).
2. Joins spatial and indicator data, filters to selected country/age/sex groups.
3. Computes derived metrics per district: incidence, population at risk, NNT, cost per DALY averted, price threshold, infections averted.
4. Simulates within-district risk heterogeneity by sampling individual-level incidence from a gamma distribution (parameterized by district mean incidence), then stratifying the population into N risk quantiles (e.g., 4 or 8).
5. Saves an `.RData` file (e.g., `Len_optim_data_ZMB_8_risk_groups.RData`) containing the risk-stratified dataframe and spatial data.

**Key parameters:**
- `iso3_group` -- country ISO3 code (e.g., "ZMB", "ZAF", "MWI")
- `sex_groups` -- sexes to include ("female", "male", "both")
- `age_groups` -- age bands (e.g., "15-24", "25-34", "35-49")
- `n_risk_groups` -- number of within-district risk quantiles (4 or 8)
- `e` -- efficacy (default 0.95)
- `px` -- cost per course ($55)
- `tx` -- lifetime treatment cost ($5000)
- `daly` -- DALYs per infection (10)
- `cdt` -- cost per DALY averted threshold ($500)

### Stage 2: Allocation Model (`R/Run_Allocate_PrEP_All_Country_Model.R`)

Runs the optimization model using the Stage 1 output.

**What it does:**
1. Sets scenario parameters (country, risk groups, age/sex eligibility, coverage cap, total Len courses, cost, efficacy).
2. Sources `R/Allocate_PrEP_Data_Cleaning_All_Country.R` to load and reshape the Stage 1 `.RData` into facility/district and incidence dataframes.
3. Sources `R/Allocate_PreP_All_Country_Model.R` which defines the core allocation and output functions.
4. Calls `generate_prep_allocation_outputs()` to run allocation, generate maps, and produce summary tables.

**Core allocation logic** (`allocate_prep_by_risk_with_stratified_prob`):
- Expands each district's population across age/sex groups and risk quantiles.
- Merges with risk-quantile-specific incidence rates from Stage 1.
- Ranks all population strata (across all districts) by descending incidence.
- Greedily allocates Len courses top-down until the budget is exhausted, capping each stratum at `coverage_mult` fraction of its population.

**Key parameters:**
- `country_iso` -- must match the Stage 1 output
- `risk_groups` -- 4 or 8 (must match Stage 1)
- `age_group_allocation_selection` -- age groups eligible for allocation
- `sex_allocation_selection` -- sexes eligible for allocation
- `coverage_mult` -- max fraction of target population receiving Len (e.g., 0.75)
- `units` -- total Len courses available
- `cost_per_unit` -- cost per course ($55)
- `budget` -- derived as `units * cost_per_unit`
- `efficacy` -- Len efficacy (0.95)

**Outputs:**
- Choropleth maps: districts receiving Len, % population covered, % incidence reduction
- Summary table: total units allocated, infections averted, PrEP coverage, cost-effectiveness metrics
- Detailed allocation by district/age/sex
- `.RData` and `.csv` output files

## Supported Countries

ZAF, SWZ, LSO, MOZ, ZWE, BWA, MWI, ZMB, KEN, TZA, UGA

## Data Dependencies

- `NAOMI/Africa_admin0/afr_g2014_2013_0.shp` -- Africa admin0 shapefile
- `NAOMI/Combined Subnational dataset/naomi3_2024_07_01.rds` -- UNAIDS Naomi model estimates
- `NAOMI/Combined Subnational dataset/2024_ssa_lowest_level_35.geojson` -- Naomi district shapefile

## Key R Scripts

| Script | Role |
|--------|------|
| `R/NAOMI_config_Allocate_PrEP_allcountry_v3.R` | Stage 1: data prep and risk distribution generation |
| `R/Run_Allocate_PrEP_All_Country_Model.R` | Stage 2: entry point to run the allocation model |
| `R/Allocate_PrEP_Data_Cleaning_All_Country.R` | Loads Stage 1 output and reshapes into model inputs |
| `R/Allocate_PreP_All_Country_Model.R` | Core allocation functions, mapping, and summary generation |
