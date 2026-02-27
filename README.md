# LenOptim – All-Country Lenacapavir Allocation Model

This repository contains the core R scripts required to run country-level Lenacapavir (long-acting PrEP) allocation and prioritization models.

The model estimates infections averted, DALYs averted, number needed to treat (NNT), cost per DALY averted, and cost-effectiveness under different geographic and demographic prioritization strategies.

This README is intended for users who wish to run the model for their own country or update assumptions.
=======
This document provides descriptions for all major functions used in the LenOptim PrEP allocation pipeline. 

**Primary R scripts:**
Within R folder:
1. NAOMI_config_Allocate_PrEP_allcountry_v3.R - creates incidence_df (the incidence input file from NAOMI data)
2. Allocate_PrEP_Data_Cleaning_All_Country.R - generates a PrEP allocation strategy using incidence_df and facility_df (facilty name, lat/lon, district, previous PrEP capacity, catchment populations by age and gender)
3. Run_Allocate_PrEP_All_Country - runs Allocate_PrEP_model
>>>>>>> 28e22c4 (Update core country model scripts and README)

---

## Repository Structure

All core scripts are located in:

```
R/
```

The key files are:

* **Allocate_PrEP_Data_Cleaning_All_Country.R**
  Prepares and cleans input datasets used in the model.

* **Allocate_PreP_All_Country_Model.R**
  Core allocation model. Implements geographic, age, and risk-group prioritization logic.

* **NAOMI_config_Allocate_PrEP_allcountry_v3.R**
  Configuration file where model parameters and assumptions are defined (costs, DALYs, coverage assumptions, etc.).

* **Run_Allocate_PrEP_All_Country_Model.R**
  Main execution script. Sources all required files and runs the model workflow.

---

## Model Overview

The model:

* Uses district-level HIV incidence estimates
* Allocates Lenacapavir under constrained budgets
* Prioritizes by geography, age, and risk strata
* Estimates:

  * HIV infections averted
  * DALYs averted
  * Number needed to treat (NNT)
  * Cost per DALY averted
  * Cost-effectiveness thresholds

---

## How to Run the Model

### Step 1 – Set Working Directory

Open R or RStudio and set your working directory to the repository root.

Example:

```r
setwd("path/to/LenOptim")
```

---

### Step 2 – Update Model Assumptions (Optional)

Edit:

```
R/NAOMI_config_Allocate_PrEP_allcountry_v3.R
```

Here you can modify:

* Lenacapavir cost per person per year
* Budget levels
* DALYs per HIV infection
* Treatment cost assumptions
* Risk group stratification settings
* Coverage constraints

---

### Step 3 – Run the Model

Execute:

```r
source("R/Run_Allocate_PrEP_All_Country_Model.R")
```

This script:

1. Loads required libraries
2. Sources configuration and cleaning scripts
3. Runs the allocation model
4. Produces summary outputs

---

## Required Inputs

Users must provide:

* District-level HIV incidence estimates
* Population data (age/sex stratified as required)
* Cost assumptions
* Risk-group structure (if applicable)

Input formatting must match the structure expected in the data cleaning script.

---

## Customizing for a New Country

To run the model for a new country:

1. Prepare incidence and population inputs in the required format.
2. Update configuration parameters in:

   ```
   NAOMI_config_Allocate_PrEP_allcountry_v3.R
   ```
3. Ensure district identifiers align with input datasets.
4. Run the model via:

   ```
   Run_Allocate_PrEP_All_Country_Model.R
   ```

---

## R Version and Packages

Recommended R version: ≥ 4.2

Commonly used packages may include:

* dplyr
* data.table
* ggplot2
* sf
* scales

Install packages if needed:

```r
install.packages(c("dplyr","data.table","ggplot2","sf","scales"))
```

---

## Notes on Outputs

Model outputs typically include:

* District-level allocation summaries
* Impact estimates
* Cost-effectiveness metrics
* Coverage summaries

Outputs can be further analyzed or visualized depending on user needs.
