library(raster)
library(ggplot2)
library(dplyr)
library(sf)

#sets the directory to where this script is stored#
setwd(dirname(rstudioapi::getSourceEditorContext()$path))



#Bring in UNAIDS Naomi model estimates
naomi_full <- read_sf('../NAOMI/Combined Subnational dataset/2024_ssa_lowest_level_35.geojson')
table(naomi_full$region)
naomi_ssa <- subset(naomi_full, region!="LAC")

ggplot(naomi_ssa) +
  geom_sf(fill = "#69b3a2", color = "white") +
  theme_void()


#Cost effectiveness calculations


#PLOTTING#

data=subset(africa_admin2_ihme, year==2018 & measure=="HIV Incidence" & metric=="Rate" & is.na(mean)!=1)

summary(data$mean/100)
data_inc <- data %>%
  mutate(inc_cat_p100=cut(mean/100, breaks=c(-Inf, 0.1, 0.5, 1,5,10,20,Inf)))
summary(data_inc$inc_cat_p100)
hist(data$mean/100)

ggplot(data_inc) +
  #geom_spatraster(data = r) +
  geom_sf(mapping = aes(fill=inc_cat_p100),color=NA) +
  scale_fill_brewer(palette = "OrRd") +
  labs(fill="Incidence rate (per 1,000 py)")







landscan_global_df <- landscan_global %>%
  as.data.frame(xy = TRUE) %>%
  rename(count = landscan.global.2023.colorized_1)

head(landscan_global_df)

ggplot(landscan_global_df) +  
  geom_tile(aes(fill=factor(count),alpha=0.8)) + 
  #geom_polygon(data=OR, aes(x=long, y=lat, group=group), 
  #             fill=NA,color="grey50", size=1)+
  coord_equal()


g_tmax_map <- ggplot(data = landscan_global_df) +
    geom_raster(aes(x = x, y = y)) +
    scale_fill_viridis_c() +
    theme_void() +
    theme(
      legend.position = "bottom"

#BRING IN DATA, CLIP TO GEOGRAPHY, AND MERGE IN IHME ESTIMATES#

#Bring in population dentisy
landscan_global <- raster("../landscan-global-2023-assets/landscan-global-2023-colorized.tif")

#Read in Africa shapefiles and clip pop density raster (landscan_global) to Africa extent
africa_adm0 <- read_sf('../Africa_admin0/afr_g2014_2013_0.shp')
plot(st_geometry(africa_adm0))
landscan_global_t <- terra::rast(landscan_global)
r <- raster::mask(raster::crop(landscan_global_t, africa_adm0), africa_adm0)
plot(r)
plot(st_geometry(africa_adm0), add=T)

#Bring in Africa Admin2 shapefile
global_adm2 = st_read("../gadm_410.gpkg")
head(global_adm2)
africa_admin2 <- subset(global_adm2, CONTINENT=="Africa") %>%
  dplyr::select(COUNTRY,NAME_2)

#Bring in IHME HIV incidence data by Admin2 and merge with shapefile
imhe_inc_mort_adm2 <- read.csv('../IHME_SSA_HIV_INC_MORT_2000_2018_INC/IHME_SSA_HIV_INC_MORT_2000_2018_INCIDENCE_ADMIN_2_Y2021M01D26.csv')
head(imhe_inc_mort_adm2)
africa_admin2_ihme <- merge(africa_admin2, imhe_inc_mort_adm2, by.x="NAME_2", by.y="ADM2_NAME")
table(africa_admin2_ihme$year)
