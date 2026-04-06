# global.R
# Loaded once when the Shiny app starts.
# Sources model functions, loads libraries, defines constants.

# -------------------------------
# Load required libraries
# -------------------------------
library(conflicted)
library(shiny)
library(tidyr)
library(dplyr)
library(stringr)
library(sf)
library(ggplot2)
library(scales)
library(patchwork)
library(viridis)
library(ggrepel)
library(DT)
library(purrr)

# Resolve conflicts
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("dataTableOutput", "DT")
conflict_prefer("renderDataTable", "DT")

# -------------------------------
# Source model code
# -------------------------------
source("model_functions.R")
source("data_loader.R")

# -------------------------------
# Constants
# -------------------------------
SUPPORTED_COUNTRIES <- c(
  "Botswana" = "BWA",
  "Eswatini" = "SWZ",
  "Kenya" = "KEN",
  "Lesotho" = "LSO",
  "Malawi" = "MWI",
  "Mozambique" = "MOZ",
  "South Africa" = "ZAF",
  "Tanzania" = "TZA",
  "Uganda" = "UGA",
  "Zambia" = "ZMB",
  "Zimbabwe" = "ZWE"
)

RISK_GROUP_OPTIONS <- c("1" = 1, "4" = 4, "8" = 8)
AGE_GROUP_OPTIONS <- c("15-24", "25-34", "35-49")
SEX_OPTIONS <- c("female", "male")
# Resolve data directory relative to this script's location (works regardless of working directory)
APP_DIR <- if (nzchar(Sys.getenv("SHINY_APP_DIR", ""))) {
  Sys.getenv("SHINY_APP_DIR")
} else {
  # When Shiny runs, getwd() is typically the app directory
  getwd()
}
DATA_DIR <- file.path(APP_DIR, "data")

# Debug: print to console so you can verify the path
message("App directory: ", APP_DIR)
message("Data directory: ", DATA_DIR)
message("Data dir exists: ", dir.exists(DATA_DIR))
message("Files found: ", length(list.files(DATA_DIR, pattern = "\\.RData$")))

# Scan which data files are available
available_data <- list.files(DATA_DIR, pattern = "Len_optim_data_.*\\.RData$")
AVAILABLE_COMBOS <- data.frame(
  file = available_data,
  stringsAsFactors = FALSE
) %>%
  mutate(
    country = str_extract(file, "(?<=Len_optim_data_)[A-Z]{3}"),
    risk_groups = as.integer(str_extract(file, "(?<=_)\\d+(?=_risk_groups)"))
  )
