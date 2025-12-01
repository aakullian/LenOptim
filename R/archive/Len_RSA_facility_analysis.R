## Facility-level analysis of South Africa Len allocation  ##
## Adam Akullian, Hasina Subedar, June 11, 2025

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

#sets the directory to where this script is stored#
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

facilitydata <- read.csv('../LEN Quantification Perf Review 02.06.2025.csv') #facility data on PrEP allocation and targeting
geocodedata <- read.csv("../LEN Quantification Perf Review 02.06.2025_With Geo-Coordinates.csv") #geog coordinates of all facilities

names(facilitydata)
names(geocodedata)

facilitydata_geocoded <- facilitydata %>%
  left_join(geocodedata, join_by(Correct.Facility.Name==Facility.Name))

facilitydata_geocoded$Latitude
facilitydata_geocoded$Longitude

## Save select dataframes ##
#rm(naomi_indicators_adm0, naomi_indicators, df1, df2, africa_adm0, bbox_polygon, naomi_shp)

## Save R data file ##
save.image(file = "")