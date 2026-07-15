# Modeling the impact of Lenacapavir allocation strategies in South Africa

Self-contained, versioned snapshot (2026-07-13) of the analysis and report:
**"Modeling the impact of Lenacapavir allocation strategies in South Africa."**

This folder reproduces the original report from raw inputs. It was assembled from the
verified script chain in `R/archive/` (data cleaning + two allocation engines + two
runners + report Rmd), cleaned up so it runs end-to-end from this one folder.

## Scenarios

The report compares three allocation strategies (5 model runs total):

| # | Scenario | Engine | Runner | `.RData` produced |
|---|----------|--------|--------|-------------------|
| 1 | NDOH, Key Populations + General Population | `01_model_ndoh_kp.R` (`kp_selection = 1`) | `run_ndoh.R` | `..._NDOH_prioritization_KP_MF15-49.RData` |
| 2 | NDOH, General Population only | `01_model_ndoh_kp.R` (`kp_selection = 0`) | `run_ndoh.R` | `..._NDOH_prioritization_GP_MF15-49.RData` |
| 3 | District prioritization (3 risk-targeting levels) | `02_model_district_priority.R` | `run_district.R` | `..._DISTRICT_prioritization__MF15-49quantile_n_{2,4,20}.RData` |

`quant_scenario = c(2, 4, 20)` in `run_district.R` controls how concentrated uptake is in
the highest-risk stratum (weak → strong geographic/risk targeting).

## Run order

Open this folder in RStudio (the scripts use `setwd(dirname(rstudioapi::...))`, so an
interactive RStudio session is assumed).

**Stage 1 (optional, run once):** `stage1_prepare_naomi_data.R` regenerates
`data/Len_optim_data_ZAF_extra_ages.RData` from the raw Naomi inputs under
`<repo root>/NAOMI/`. It is slow and only needed if those inputs change — the
committed `.RData` is already in `data/`, so you can normally skip straight to the
runners below. (See the reproducibility note under "Data inputs".)

**One-knit workflow (default):** just knit `Len_RSA_report.Rmd`. Its setup chunk sources
`run_ndoh.R`, `run_district.R`, and `run_volume_sensitivity.R` (which in turn source the
data-cleaning + allocation engines), regenerates the scenario `.RData` files in `output/`,
then builds the tables, figures and maps. The model runs take a few minutes each knit. To knit quickly against
existing outputs when only editing text, set `run_models <- FALSE` in the setup chunk.

You can still run the pipeline manually instead (e.g. for debugging): **Source**
`run_ndoh.R`, then `run_district.R`, then knit with `run_models <- FALSE`. Each runner
re-sources `00_data_cleaning.R`, so they are independent.

The scripts' `setwd(...)` is guarded to run only in an interactive RStudio session, so it
is skipped during knitting (knitr already sets the working directory to this folder).

## Files

| File | Role |
|------|------|
| `stage1_prepare_naomi_data.R` | **Upstream (run once):** builds `data/Len_optim_data_ZAF_extra_ages.RData` from raw Naomi inputs — joins Naomi estimates and simulates within-district risk quantiles via a gamma distribution (now seeded) |
| `00_data_cleaning.R` | Loads NAOMI + facility + NDOH inputs; builds `facility_df_place_holder`, `incidence_df`, `incidence_df_quant`, `incidence_df_kp`, `district_new_infections`, `ndoh_len_plan_merged`, `district_sf`; writes `output/extra_plots.RData` |
| `01_model_ndoh_kp.R` | NDOH engine: allocates to general pop (women 2:1 over men) + optionally key populations, per the NDOH 25-district plan volumes |
| `02_model_district_priority.R` | District engine: ranks all districts/strata by incidence and allocates top-down |
| `run_ndoh.R` | Runs the NDOH key-population scenario |
| `run_district.R` | Runs the district-prioritization scenarios (quant 2/4/20) |
| `run_volume_sensitivity.R` | Sweeps total person-years (0.25M–5M) at fixed targeting (universal delivery) → `output/volume_sensitivity.RData` for the volume-sensitivity figure |
| `Len_RSA_report.Rmd` + `styles.css` | The report |
| `data/` | All raw inputs (copied in — see below) |
| `output/` | Model `.RData`, maps, and intermediate saves (created at run time) |

## Key parameters / assumptions (as in the original report)

- **Total volume:** 500,000 person-years of Lenacapavir, harmonised with Jamieson et al. (Thembisa) Phase 1. District runs set `units = 500000`; the NDOH runner scales the published plan's per-group counts (~448k) up to 500k, preserving the population mix.
- **Cost per course:** $100; **efficacy:** 0.99 (PURPOSE-trial estimate, matching Jamieson et al.; was 0.95); **coverage_mult:** 1.
- The original report was 448k person-years at 95% efficacy; this version is re-harmonised to 500k / 99% for comparability, so its absolute numbers differ from the archived report (~1.16× higher).
- **General-population split:** women prioritized 2:1 over men (`01_model_ndoh_kp.R`).
- **KP incidence** (`00_data_cleaning.R`, derived from NAOMI female 15-49 incidence):
  SW = 2 x 6 x inc; ANC = 1.5 x 1.35 x inc; GBMSM = 2 x 3.75 x inc; TGW = 2 x 5.625 x inc.
- **District targeting:** uptake concentrated in the top incidence quantile (`0.75-1`).

## Data inputs (`data/`)

| File | Source |
|------|--------|
| `Len_optim_data_ZAF_extra_ages.RData` | NAOMI-based district risk distributions (`naomi_ssa_shp_m`, `risk_dist_targeting_fine_scale`, `..._FULL_PERCENTILES`). Generated by `stage1_prepare_naomi_data.R`. **Reproducibility:** the committed file behind the original report was produced *without* a random seed; `stage1_*` now sets `set.seed(12345)`, so re-running produces a fresh (slightly different, but reproducible-going-forward) file. Keep the committed copy to match the original report exactly. |
| `LEN Quantification Perf Review 24.06.2025_With Geo-Coordinates 24.06.2024.csv` | Facility list + coordinates (Hasina) |
| `District Names.csv` | District ↔ NAOMI `area_id` crosswalk |
| `Disaggregated PrEP Initiations April 2020 to March 2025.xlsx` | Oral PrEP initiations by age/sex (Tumisho) |
| `Slide12_Target_Population_Lenacapavir_Corrected.csv` | NDOH 1st-phase Len allocation plan by district & risk group ("Lenacapavir Scale-up Updated 12 August 2025 rev") |

## Provenance & cleanup notes

- Assembled from `R/archive/`: `Allocate_PrEP_RSA_data_cleaning.R` (full 344-line version recovered from git commit `d165116`), `Allocate_PreP_RSA_v2_ndoh_priority_KP.R`, `Allocate_PreP_RSA_v2_district_priority.R`, `Run_Allocate_PrEP_RSA_ndoh_priority.R`, `Run_Allocate_PrEP_RSA_District_Allocation.R`, and `Len_RSA_report.Rmd`.
- Cleanup applied: consolidated into one folder with `data/` + `output/`; repointed all read/write paths; removed a stray `browser()` in the NDOH engine that halted execution. **Allocation logic is otherwise unchanged**, so results should match the original report (that is the sanity check).
- The standalone `Allocate_PreP_RSA_v2_ndoh_priority.R` (older, women-15-24-only) was **not** used by the report and is intentionally excluded — the NDOH general-population scenario comes from `01_model_ndoh_kp.R` with `kp_selection = 0`.
