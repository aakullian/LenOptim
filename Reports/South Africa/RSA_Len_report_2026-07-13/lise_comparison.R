# =============================================================================
# Comparison of our 1-year, static, primary-infection allocation model against
# Lise Jamieson et al. (Thembisa v4.8) Phase-1 LEN donation optimisation.
#
# Lise's Phase 1: ~500,000 person-years of LEN over 2026-2027, 99% efficacy,
# reported as HIV infections averted over 2026-2030 INCLUDING onward (secondary)
# transmission. Ours is 1-year PRIMARY infections averted, no transmission
# dynamics. The two are therefore NOT directly comparable on absolute numbers.
#
# Approach (agreed): keep our clean 1-yr primary estimate; report the implied
# amplification ratio (Lise 5-yr incl. secondary / ours 1-yr primary); and check
# that the RANKING of allocation strategies agrees. Run both Lise's optimum
# splits and the NDOH-plan split, side by side.
#
# NOTE: the preprint does NOT report per-group HIV incidence (Thembisa derives it
# endogenously and time-varying), so we CANNOT reconcile absolute incidence. We
# therefore de-emphasise absolute infections and the amplification ratio, and lean
# on two incidence-magnitude-INDEPENDENT comparisons:
#   (1) RANK of each allocation strategy in both models (Spearman), and
#   (2) each strategy's impact NORMALISED to its own model's optimum (=100%),
#       i.e. do the two models agree on the *shape* of the impact-vs-allocation
#       profile. Agreement here corroborates the targeting conclusion regardless
#       of any incidence-level mismatch.
#
# Because our infections_averted is exactly linear:
#     infections = person_years x (incidence/1000) x efficacy
# we extract the effective per-population incidence our model assigned (from the
# committed NDOH-KP run) and then compute infections averted for ANY allocation
# split analytically at the harmonised inputs below. This isolates the single
# biggest driver -- the incidence we assign each population -- for sanity-checking
# against Thembisa.
#
# Run in RStudio after run_ndoh.R has produced the KP output .RData.
# =============================================================================

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
library(dplyr)
library(tidyr)

# ---- Harmonised inputs to match Lise's Phase 1 -----------------------------
PY_TOTAL <- 500000   # person-years of LEN (Lise: ~500k over 2026-27)
EFFICACY <- 0.99     # Lise used 99% (PURPOSE); ours used 95%

# ---- 1. Load our committed NDOH-KP allocation output -----------------------
load("output/Len_optim_RSA_output_NDOH_prioritization_KP_MF15-49.RData")  # -> outputs
result_df <- outputs$result_df

# ---- 2. Effective incidence (per 1000 py) our model assigned per population -
# Weighted by allocated person-years, so this is the incidence of those actually
# reached. Independent of the efficacy / volume used in the original run.
eff_inc <- function(df) {
  df <- df %>% filter(allocated_units > 0, !is.na(inc_in_sample))
  if (nrow(df) == 0) return(NA_real_)
  weighted.mean(df$inc_in_sample, w = df$allocated_units)
}

is_agyw <- function(df) df %>% filter(sex == "female",
                                      age_group_label %in% c("15-19", "20-24"))

inc <- c(
  FSW  = eff_inc(result_df %>% filter(sex == "SW")),
  MSM  = eff_inc(result_df %>% filter(sex == "GBMSM")),
  PBFW = eff_inc(result_df %>% filter(sex == "ANC")),
  AGYW = eff_inc(is_agyw(result_df))
)

cat("\n==== Our effective incidence per population (per 1000 py) ====\n")
print(round(inc, 1))
cat("(Sanity-check these against Thembisa's assumed incidence by group.)\n")

# ---- 3. Split calculator: 1-yr primary infections averted ------------------
# shares: named numeric over the populations in `inc`, expressed as fractions
#         (or percentages -- normalised below) of PY_TOTAL.
avert_1yr <- function(shares, inc_vec = inc, PY = PY_TOTAL, eff = EFFICACY) {
  shares <- shares[names(inc_vec)]
  shares[is.na(shares)] <- 0
  shares <- shares / sum(shares)            # normalise to 1
  sum(shares * inc_vec / 1000, na.rm = TRUE) * PY * eff
}

# ---- 4. Lise's Phase-1 results (slides 16-18): FSW / MSM / AGYW / PBFW % ----
# infections = Lise's 5-yr (2026-2030) HIV infections averted, incl. secondary.
lise <- tribble(
  ~label,                 ~FSW, ~MSM, ~AGYW, ~PBFW, ~lise_5yr,
  "Optimum (55 PBFW)",      18,   26,    0,    55,   20554,
  "Alt 30 MSM",             14,   30,    0,    55,   20554,
  "AGYW 5%",                18,   26,    5,    50,   20260,
  "AGYW 10%",               18,   26,   10,    45,   20000,
  "AGYW 30% (mod)",         18,   26,   30,    25,   18756,
  "AGYW 75%",                0,    0,   75,    25,    6749,
  "AGYW 100%",               0,    0,  100,     0,    5003
)

lise_cmp <- lise %>%
  rowwise() %>%
  mutate(
    ours_1yr_primary = avert_1yr(c(FSW = FSW, MSM = MSM, AGYW = AGYW, PBFW = PBFW))
  ) %>%
  ungroup() %>%
  mutate(
    amplification = lise_5yr / ours_1yr_primary,
    rank_ours = rank(-ours_1yr_primary),
    rank_lise = rank(-lise_5yr),
    # incidence-magnitude-independent: each model normalised to its own optimum
    ours_rel = round(100 * ours_1yr_primary / max(ours_1yr_primary), 1),
    lise_rel = round(100 * lise_5yr        / max(lise_5yr),        1)
  )

# ---- 5. NDOH-plan split (as our model actually allocated it) ---------------
# Uses ALL populations our model reaches (incl. gen-pop 25-49, men, TGW), so this
# reflects the real planned rollout rather than Lise's 4-population optimum.
ndoh_shares <- result_df %>%
  filter(allocated_units > 0) %>%
  mutate(grp = case_when(
    sex == "SW"    ~ "FSW",
    sex == "GBMSM" ~ "MSM",
    sex == "ANC"   ~ "PBFW",
    sex == "TGW"   ~ "TGW",
    sex == "female" & age_group_label %in% c("15-19","20-24") ~ "AGYW",
    sex == "female" ~ "gen_F_25_49",
    sex == "male"   ~ "gen_M",
    TRUE ~ "other"
  )) %>%
  group_by(grp) %>%
  summarise(py = sum(allocated_units), inc = weighted.mean(inc_in_sample, allocated_units),
            .groups = "drop") %>%
  mutate(share = py / sum(py))

ndoh_1yr <- sum(ndoh_shares$share * ndoh_shares$inc / 1000) * PY_TOTAL * EFFICACY

cat("\n==== NDOH-plan allocation shares (our model) ====\n")
print(ndoh_shares %>% mutate(share_pct = round(100 * share, 1),
                             inc = round(inc, 1)) %>% select(grp, py, inc, share_pct))

# ---- 6. Side-by-side output ------------------------------------------------
cat("\n==== Allocation splits: ours (1-yr primary) vs Lise (5-yr incl. secondary) ====\n")
cat("ours_rel / lise_rel = impact normalised to each model's own optimum (=100).\n")
cat("These two columns are the incidence-independent comparison; read them first.\n\n")
print(as.data.frame(lise_cmp %>%
  mutate(ours_1yr_primary = round(ours_1yr_primary),
         amplification = round(amplification, 2)) %>%
  select(label, FSW, MSM, AGYW, PBFW,
         ours_rel, lise_rel, rank_ours, rank_lise,
         ours_1yr_primary, lise_5yr, amplification)))

cat(sprintf(
  "\nNDOH plan (our full rollout, %d PY @ %.0f%% eff): %s infections averted (1-yr primary)\n",
  PY_TOTAL, 100 * EFFICACY, format(round(ndoh_1yr), big.mark = ",")))

cat(sprintf(
  "\nRanking agreement (Spearman, ours vs Lise across optimum splits): %.2f\n",
  suppressWarnings(cor(lise_cmp$ours_1yr_primary, lise_cmp$lise_5yr, method = "spearman"))))

# ---- 7. Save ---------------------------------------------------------------
write.csv(lise_cmp, "output/lise_comparison_table.csv", row.names = FALSE)
cat("\nSaved output/lise_comparison_table.csv\n")
