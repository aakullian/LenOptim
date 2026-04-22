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
expected_n_files <- 33
data_url <- "https://github.com/aakullian/LenOptim/releases/download/data-v1/lenoptim-data-v1.zip"

count_data_files <- function() {
  length(list.files(data_dir, pattern = "^Len_optim_data_.*\\.RData$"))
}

if (count_data_files() < expected_n_files) {
  message(
    "Data directory is missing files (found ", count_data_files(),
    " of ", expected_n_files, ").\nDownloading ~592 MB from GitHub release..."
  )
  dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
  zip_path <- tempfile(fileext = ".zip")
  ok <- tryCatch({
    old_timeout <- getOption("timeout")
    options(timeout = 1800)
    on.exit(options(timeout = old_timeout), add = TRUE)
    utils::download.file(data_url, zip_path, mode = "wb", quiet = FALSE)
    utils::unzip(zip_path, exdir = data_dir)
    TRUE
  }, error = function(e) {
    message("Download or extraction failed: ", conditionMessage(e))
    FALSE
  })
  if (file.exists(zip_path)) file.remove(zip_path)

  if (!ok || count_data_files() < expected_n_files) {
    stop(
      "Could not obtain the data files automatically.",
      "\nTry downloading manually from ", data_url,
      "\nand unzipping into ", normalizePath(data_dir, mustWork = FALSE), "."
    )
  }
  message("Downloaded and extracted ", count_data_files(), " data files.")
}

message("Starting LenOptim dashboard... (", length(data_files), " data files loaded)")
shiny::runApp(app_dir, launch.browser = TRUE)
