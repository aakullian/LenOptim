#!/usr/bin/env Rscript
# Bootstrap + launcher for the LenOptim Shiny app.
# Usage from the repo root:
#   Rscript run.R
# or open in RStudio and click Source.

required_packages <- c(
  "conflicted", "shiny", "tidyr", "dplyr", "stringr", "sf",
  "ggplot2", "scales", "patchwork", "viridis", "ggrepel",
  "DT", "purrr", "htmltools"
)

min_r_version <- "4.2.0"
if (getRversion() < min_r_version) {
  stop(
    "This app requires R >= ", min_r_version,
    ". You have R ", getRversion(), ".",
    "\nInstall a newer R from https://cran.r-project.org/ and try again."
  )
}

missing <- required_packages[!vapply(required_packages, requireNamespace,
                                     logical(1), quietly = TRUE)]

if (length(missing) > 0) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")

  still_missing <- missing[!vapply(missing, requireNamespace,
                                   logical(1), quietly = TRUE)]
  if (length(still_missing) > 0) {
    stop(
      "Failed to install: ", paste(still_missing, collapse = ", "),
      "\nOn Linux, `sf` needs system libraries (GDAL, PROJ, GEOS). Install them first:",
      "\n  Ubuntu/Debian: sudo apt install libgdal-dev libproj-dev libgeos-dev libudunits2-dev",
      "\n  macOS:         brew install gdal proj geos udunits",
      "\nOn Windows, CRAN binaries include these -- rerun after checking your internet connection."
    )
  }
}

app_dir <- "R/shiny_app"
data_dir <- file.path(app_dir, "data")
data_files <- list.files(data_dir, pattern = "^Len_optim_data_.*\\.RData$",
                         full.names = FALSE)

if (length(data_files) == 0) {
  stop(
    "No model data files found in ", data_dir, ".",
    "\nThe app needs 33 pre-computed `.RData` files (one per country x risk-groups combo).",
    "\nThese are not tracked in git due to size (~592 MB total).",
    "\nContact the repo maintainer for a copy, or regenerate them with R/generate_all_data.R",
    "\n(requires access to the UNAIDS Naomi source data)."
  )
}

message("Starting LenOptim dashboard... (", length(data_files), " data files loaded)")
shiny::runApp(app_dir, launch.browser = TRUE)
