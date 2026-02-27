# LenOptim – All-Country Lenacapavir Allocation Model

This repository contains the core R scripts required to run country-level Lenacapavir (long-acting PrEP) allocation and prioritization models.

The model estimates infections averted, DALYs averted, number needed to treat (NNT), cost per DALY averted, and cost-effectiveness under different geographic and demographic prioritization strategies.

This README is intended for users who wish to run the model for their own country or update assumptions.

---

## Repository Structure

All core scripts are located in: R/

The key files are:

- **Allocate_PrEP_Data_Cleaning_All_Country.R**  
  Prepares and cleans input datasets used in the model.

- **Allocate_PreP_All_Country_Model.R**  
  Core allocation model. Implements geographic, age, and risk-group prioritization logic.

- **NAOMI_config_Allocate_PrEP_allcountry_v3.R**  
  Configuration file where model parameters and assumptions are defined (costs, DALYs, coverage assumptions, etc.).

- **Run_Allocate_PrEP_All_Country_Model.R**  
  Main execution script. Sources all required files and runs the model workflow.

---

## Model Overview

The model:

- Uses district-level HIV incidence estimates
- Allocates Lenacapavir under constrained budgets
- Prioritizes by geography, age, and risk strata
- Estimates:
  - HIV infections averted
  - DALYs averted
  - Number needed to treat (NNT)
  - Cost per DALY averted
  - Cost-effectiveness thresholds

---

## How to Run the Model

### Step 1 – Set Working Directory

Open R or RStudio and set your working directory to the repository root.

Example:

```r
setwd("path/to/LenOptim")
