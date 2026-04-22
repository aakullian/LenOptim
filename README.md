# LenOptim -- Lenacapavir PrEP Allocation Optimizer

An interactive Shiny dashboard for optimizing sub-national allocation of Lenacapavir (long-acting injectable PrEP) across 11 countries in sub-Saharan Africa.

Given a fixed supply of Len courses, the model determines which districts, age groups, sex groups, and risk strata should receive allocation to maximize HIV infections averted.

## Running Locally

After cloning, from the repo root:

```bash
Rscript run.R
```

The bootstrap script installs any missing R packages from CRAN and launches the dashboard in your browser. Requires R >= 4.2. On Linux, `sf` additionally needs system libraries (GDAL, PROJ, GEOS, UDUNITS) — see the error message the script prints if they're missing.

Alternatively, open `run.R` in RStudio and click **Source**, or run `shiny::runApp("R/shiny_app", launch.browser = TRUE)` directly if dependencies are already installed.

### Data files

The app needs 33 pre-computed `.RData` files (~592 MB total) in `R/shiny_app/data/`. `run.R` will download and extract them automatically from a [GitHub Release](https://github.com/aakullian/LenOptim/releases/tag/data-v1) on first run. To regenerate from scratch, use `R/generate_all_data.R` (requires UNAIDS Naomi source data).

## Features

- **Allocation Maps** -- Choropleth maps showing districts receiving Len, population coverage, and incidence reduction
- **Summary Table** -- Infections averted, cost-effectiveness, NNT, PrEP coverage, and targeting ratio
- **District Detail** -- Sortable, searchable allocation by district/age/sex (downloadable to CSV/Excel)
- **Volume Finder** -- Interactive dose-response curve: how many Len courses are needed to achieve any target incidence reduction
- **Scenario Comparison** -- Save multiple model runs and compare side by side

## How the Model Works

1. District-level HIV incidence, prevalence, and population estimates are drawn from the [UNAIDS Naomi model](https://naomi-spectrum.unaids.org/) (2024 estimates).
2. Within each district, individual-level risk heterogeneity is simulated using a gamma distribution, then stratified into risk quantiles (1, 4, or 8 groups).
3. All population strata across all districts are ranked by descending incidence.
4. Len courses are allocated top-down to the highest-risk strata first, until the supply is exhausted or the coverage cap is reached.

## Repository Structure

```
R/shiny_app/
  global.R              # Libraries, constants, data file scanning
  ui.R                  # Dashboard layout and inputs
  server.R              # Reactive logic, model execution, rendering
  model_functions.R     # Core allocation, mapping, and summary functions
  data_loader.R         # Loads pre-computed country data files
  data/                 # Pre-computed .RData files (not tracked in git)
context.md              # Detailed project documentation
```

## Pre-computed Data

The dashboard uses pre-computed `.RData` files containing district-level risk distributions for each country and risk group setting (33 files: 11 countries x 3 risk groups). These files are stored in `R/shiny_app/data/` but are not tracked in git due to their size.

To generate the data files, run `R/generate_all_data.R` with access to the NAOMI source data.

## Supported Countries

Botswana, Eswatini, Kenya, Lesotho, Malawi, Mozambique, South Africa, Tanzania, Uganda, Zambia, Zimbabwe

## Requirements

- R >= 4.5.0
- Key packages: shiny, dplyr, tidyr, sf, ggplot2, patchwork, viridis, ggrepel, DT, scales, conflicted, purrr, stringr

## Reference

Akullian A, Imai-Eaton JW, Sharma M, Subedar H, O'Brien M, Garnett G. *Health impact and cost-effectiveness of geographically prioritized long-acting PrEP delivery in southern and eastern Africa.* medRxiv 2026. [doi:10.1101/2026.01.01.345396v1](https://www.medrxiv.org/content/10.1101/2026.01.01.345396v1)
