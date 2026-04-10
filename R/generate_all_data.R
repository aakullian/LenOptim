############################################################################################################
# Batch script to pre-compute Stage 1 .RData files for all countries and risk group settings.
# Generates files for the Shiny dashboard (R/shiny_app/data/).
#
# Usage: Run this script from RStudio or Rscript. It will generate 33 .RData files
#        (11 countries x 3 risk group settings: 1, 4, 8).
#
# Prerequisites: NAOMI data files must be present at ../NAOMI/ relative to R/ directory.
############################################################################################################

library(raster)
library(ggplot2)
library(sf)
library(tidyverse)
library(classInt)
library(metR)
library(ggrepel)
library(dplyr)

# Remove sci notation
options(scipen = 100, digits = 4)

# Set working directory to where this script lives
script_dir <- if (interactive() && requireNamespace("rstudioapi", quietly = TRUE)) {
  dirname(rstudioapi::getSourceEditorContext()$path)
} else {
  # When run via Rscript, use the script's own directory
  this_file <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", this_file, value = TRUE)
  if (length(file_arg) > 0) {
    dirname(normalizePath(sub("--file=", "", file_arg)))
  } else {
    getwd()
  }
}
setwd(script_dir)

output_dir <- file.path(script_dir, "shiny_app", "data")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

############################################################################################################
# Fixed parameters (same as NAOMI_config_Allocate_PrEP_allcountry_v3.R)
############################################################################################################
sex_groups <- c("female", "male")
age_groups <- c("15-24", "25-34", "35-49")
indicators <- c("HIV incidence", "HIV prevalence", "Population", "infections")
e <- 0.95
d <- 1
px <- 55
tx <- 5000
daly <- 10
cdt <- 500
f <- 1

############################################################################################################
# Load shared data (once)
############################################################################################################
cat("Loading Africa shapefiles...\n")
africa_adm0 <- read_sf('../NAOMI/Africa_admin0/afr_g2014_2013_0.shp')

bbox_coords <- c(-20, -40, 60, 40)
bbox_polygon <- st_as_sf(st_sfc(st_polygon(list(matrix(c(
  bbox_coords[1], bbox_coords[2],
  bbox_coords[3], bbox_coords[2],
  bbox_coords[3], bbox_coords[4],
  bbox_coords[1], bbox_coords[4],
  bbox_coords[1], bbox_coords[2]
), ncol = 2, byrow = TRUE))), crs = 4326))
africa_adm0_cropped <- sf::st_intersection(bbox_polygon, africa_adm0)

cat("Loading NAOMI indicators and shapefile...\n")
naomi_indicators <- readRDS('../NAOMI/Combined Subnational dataset/naomi3_2024_07_01.rds')
naomi_shp <- read_sf('../NAOMI/Combined Subnational dataset/2024_ssa_lowest_level_35.geojson')

############################################################################################################
# Define country and risk group combinations to generate
############################################################################################################
all_countries <- c("ZAF", "SWZ", "LSO", "MOZ", "ZWE", "BWA", "MWI", "ZMB", "KEN", "TZA", "UGA")
all_risk_groups <- c(1, 4, 8)

############################################################################################################
# Loop over all combinations
############################################################################################################
for (iso3_group in all_countries) {
  for (n_risk_groups in all_risk_groups) {

    output_file <- file.path(output_dir, paste0("Len_optim_data_", iso3_group, "_", n_risk_groups, "_risk_groups.RData"))

    # Skip if already exists
    if (file.exists(output_file)) {
      cat("SKIP:", output_file, "(already exists)\n")
      next
    }

    cat("\n===========================================================\n")
    cat("Generating:", iso3_group, "with", n_risk_groups, "risk groups\n")
    cat("===========================================================\n")

    # Check if country exists in NAOMI data
    naomi_country_check <- naomi_shp %>% filter(iso3 == iso3_group)
    if (nrow(naomi_country_check) == 0) {
      cat("WARNING: No NAOMI data for", iso3_group, "- skipping\n")
      next
    }

    # Join and subset
    naomi_ssa_shp_m <- naomi_shp %>%
      filter(iso3 %in% iso3_group) %>%
      inner_join(naomi_indicators, by = join_by(area_id), keep = NULL) %>%
      filter(age_group_label %in% age_groups, sex %in% sex_groups, indicator_label %in% indicators) %>%
      rename_at(vars(ends_with(".x")), ~str_replace(., "\\..$", "")) %>%
      select_at(vars(-ends_with(".y"), -c(indicator, lower, upper))) %>%
      dplyr::select(-age_group) %>%
      pivot_wider(names_from = c(indicator_label, age_group_label), values_from = c(mean)) %>%
      pivot_longer(
        cols = matches("Population|prevalence|incidence"),
        names_to = c("indicator_label", "age_group_label"),
        names_sep = "_",
        values_to = "mean"
      ) %>%
      pivot_wider(names_from = c(indicator_label), values_from = c(mean)) %>%
      mutate(
        incidence = `HIV incidence`, prevalence = `HIV prevalence`,
        pop_at_risk = Population * (1 - prevalence),
        inc_cat = cut(incidence * 1000, breaks = c(0, 1, 2, 3, 4, 5, 10, 15, 20),
                      labels = c("<1", "1-1.9", "2-2.9", "3-3.9", "4-4.9", "5-5.9", "10-14.9", "15-19.9"), right = F),
        nnt = 1 / (e * incidence * d),
        nnt_cat = cut(nnt, breaks = c(0, 100, 200, 500, 1000, Inf),
                      labels = c("<100", "100-199", "200-499", "500-999", "1000+"), right = F),
        cdaverted = (nnt * px - tx) / daly,
        cdaverted_cat = cut(cdaverted, breaks = c(-Inf, 0, 500, 1000, 5000, 10000, Inf),
                            labels = c("<0", "0-499", "500-999", "1000-4999", "5000-9999", "10000+"), right = F),
        ci = nnt * px - tx,
        pt = ((tx + daly * cdt) / nnt),
        pt_cat = cut(pt, breaks = c(0, 2.5, 10, 50, 100, 200, 500),
                     labels = c("<2.5", "<10", "<50", "<100", "<200", "<300"), right = F),
        infections_expected = incidence * pop_at_risk,
        infections_averted = incidence * f * e * pop_at_risk,
        infections_averted_cat = cut(infections_averted, breaks = c(0, 10, 50, 100, 1000, Inf),
                                     labels = c("<10", "10-50", "50-100", "100-1000", ">1000"), right = F),
        incidence_reduction = infections_averted / (incidence * pop_at_risk)
      )

    naomi_ssa_shp_m_df <- data_frame(naomi_ssa_shp_m)

    # Create incidence list for risk distribution sampling
    inc_list <- naomi_ssa_shp_m_df %>%
      dplyr::select(area_id, iso3, incidence, pop_at_risk, age_group_label, sex) %>%
      arrange(incidence)

    efficacy <- e

    # Risk distribution sampling loop
    n <- 1
    for (i in seq(1, nrow(inc_list))) {
      for (shape in c(1)) {
        for (quant in seq(1 / n_risk_groups, 1, 1 / n_risk_groups)) {
          inc_list_sub <- inc_list[i, ]
          samplesize <- inc_list_sub$pop_at_risk
          if (is.na(samplesize) | samplesize == 0) {
            next
          }
          inc <- inc_list_sub$incidence
          area_id <- inc_list_sub$area_id
          sex <- inc_list_sub$sex
          age_group_label <- inc_list_sub$age_group_label

          x <- rgamma(n = samplesize, shape = shape, scale = inc / shape)
          x <- round(x * 1000, 2)
          pop <- as.data.frame(x)
          pop$infected <- ifelse(runif(nrow(pop), min = 0, max = 1000) > pop$x, 0, 1)
          total_infections <- sum(pop$infected)
          total_pop <- length(x)
          pop$quantile <- rank(pop$x) / nrow(pop)
          sub_pop <- pop %>%
            filter(quantile > quant - 1 / n_risk_groups, quantile < quant) %>%
            summarise(
              sex = sex,
              age_group_label = age_group_label,
              iso3 = iso3_group,
              area_id = area_id,
              inc_district = inc * 1000,
              pop_district = total_pop,
              total_infections_district_age_sex = total_infections,
              pop_subsample = n(),
              total_infected_subsample = sum(infected),
              inc_in_sample = (total_infected_subsample / pop_subsample) * 1000,
              infections_averted = total_infected_subsample * efficacy,
              sensitivity = total_infected_subsample / total_infections,
              nnt = 1 / (e * (inc_in_sample / 1000) * d),
              cdaverted = (nnt * 130 - tx) / daly,
              pt = (tx + daly * cdt) / nnt,
              shape = shape,
              quant_target = quant
            )
          df1 <- sub_pop
          if (n == 1) { df2 <- df1 } else { df2 <- rbind(df1, df2) }
          n <- 2
          cat(paste("  working on inc=", round(inc, 4), "shape=", shape,
                    "quantile=", quant, "sex=", sex, "age group=", age_group_label, "\n"))
        }
      }
    }

    df2$inc_mult <- df2$inc_in_sample / df2$inc_district
    name <- paste0("naomi_risk_dist_targeting_", iso3_group, "_", n_risk_groups, "_risk_groups")
    assign(name, df2)

    # Save .RData file with the two key objects
    save(naomi_ssa_shp_m, list = name, africa_adm0_cropped, file = output_file)
    cat("SAVED:", output_file, "\n")

    # Clean up loop variables
    rm(df2, naomi_ssa_shp_m, naomi_ssa_shp_m_df, inc_list)
    rm(list = name)
  }
}

cat("\n\nDone! Check", output_dir, "for generated files.\n")
