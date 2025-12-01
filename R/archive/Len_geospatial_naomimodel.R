library(raster)
library(ggplot2)
library(dplyr)
library(sf)
library(tidyverse)
library(classInt)

# Remove sci notation
options(scipen = 100, digits = 4)

#sets the directory to where this script is stored#
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#Bring in Africa countries shp
africa_adm0 <- read_sf('../Africa_admin0/afr_g2014_2013_0.shp') #shapefile

#Bring in UNAIDS Naomi model estimates
naomi_indicators <- readRDS('../NAOMI/Combined Subnational dataset/naomi3_2024_07_01.rds')
names(naomi_indicators)
table(naomi_indicators$area_id)
table(naomi_indicators$indicator_label)
table(naomi_indicators$indicator)
table(naomi_indicators$region)
table(naomi_indicators$sex)
table(naomi_indicators$age_group_label)

naomi_shp <- read_sf('../NAOMI/Combined Subnational dataset/2024_ssa_lowest_level_35.geojson') #shapefile
names(naomi_shp)
table(naomi_shp$area_id)
table(naomi_shp$region)
table(naomi_shp$country)
table(naomi_shp$iso3)

#Set subset varibales and set paramters for new vars
regions=c("ESA") #ESA, LAC, WCA
countries <- c("South Africa","Eswatini","Lesotho","Mozambique",'Zimbabwe',"Botswana","Malawi","Zambia")
iso3_group <- c("ZAF","SWZ","LSO","MOZ",'ZWE',"BWA","MWI","ZMB")
iso3_group <- c("ZAF","SWZ","LSO")
sex_groups = c("female") # "male","female","both"
age_groups <- c("15-19","20-24","25-29","30-34","35-39","40-44","45-49") # "15-19","20-24","25-29","30-34","35-39","40-44","45-49"
age_groups <- c("15-24","25-34","25-49") # "15-19","20-24","25-29","30-34","35-39","40-44","45-49"
age_groups <- c("15-49") # "15-19","20-24","25-29","30-34","35-39","40-44","45-49"

indicators <- c("HIV incidence","HIV prevalence","Population") #
e <- 0.95 #efficacy of prevention
d <- 1 #duration of protection in years
px=200 #cost per person per year of prevention 
tx=10000 #cost over lifetime of treatment
daly=20 #DALYs associated with an HIV infection
cdt=500 #cost per DALY averted threshold
f=1 #effective coverage

#join and subset
naomi_ssa_shp_m <- naomi_shp %>%
  filter(iso3 %in% iso3_group) %>%
  inner_join(naomi_indicators, by=join_by(area_id), keep=NULL) %>%
  filter(age_group_label %in% age_groups, sex %in% sex_groups, indicator_label %in% indicators) %>%
  rename_at(vars(ends_with(".x")),~str_replace(., "\\..$","")) %>% 
  select_at(vars(-ends_with(".y"),-c(indicator,lower,upper))) %>%
  pivot_wider(names_from = indicator_label, values_from = c(mean)) %>%
  mutate(nnt=1/(e*`HIV incidence`*d),nnt_cat=cut(nnt,breaks=c(0,100,200,500,1000,Inf), labels=c("<100","100-200","200-500","500-1000",">1000"), right=F), #number needed to treat
         cd=(nnt*px-tx)/daly, #cost effectiveness ($/Daly)
         #bi=, #budget impact
         pt=(tx+daly*cdt)/nnt, pt_cat=cut(pt,breaks=c(0,10,50,100,200,Inf), labels=c("<$10","$10-50","$50-100","$100-200",">$200"), right=F), #price threshold
         impact=`HIV incidence`*f*e*(Population*(1-`HIV prevalence`)), impact_cat=cut(impact,breaks=c(0,10,50,100,1000,Inf), labels=c("<10","10-50","50-100","100-1000",">1000"), right=F) #impact (infections averted)
         )  
#  pivot_longer(cols=c(!area_id:calendar_quarter), names_to="indicator_label", values_to="mean") %>%

# get quantile breaks. Add .00001 offset to catch the lowest value
#breaks_pt <- classIntervals(c(min(naomi_ssa_shp_m$pt), naomi_ssa_shp_m$pt)-0.001, n = 4, style = "quantile")

#subset Africa adm0 shp
africa_adm0_subs <- subset(africa_adm0, ISO3 %in% iso3_group)

sf_use_s2(FALSE)
naomi_shp_dislv <- naomi_ssa_shp_m %>%
  group_by(country) %>%
  summarize(
    geometry = sf::st_union(geometry), # or SHAPE in case of data loaded from GDB
    across(everything(), first),
    .groups = "drop"
  ) 
                           
#Price threshold
naomi_ssa_shp_m %>% 
  ggplot() + 
  geom_sf(fill = "grey70", color = "black") +
  geom_sf(aes(fill=pt_cat)) +
  scale_fill_brewer(palette = "RdYlGn", na.value = "grey90",na.translate = FALSE) +
  facet_grid(sex~age_group_label) +
  labs(fill="") +
  theme_void() +
  theme(strip.background = element_blank(), legend.position = "bottom",panel.border = element_rect(color = "black", fill = NA, linewidth = 1), ) 
  #guides(fill = guide_colorbar(barwidth = 20, barheight = 0.5))

#NNT
naomi_ssa_shp_m %>% 
  ggplot() + 
  #geom_sf(fill = "grey70", color = "black") +
  geom_sf(aes(fill=nnt_cat)) +
  scale_fill_brewer(palette = "Spectral", direction=1, na.value = "grey90",na.translate = FALSE) +
  facet_grid(sex~age_group_label) +
  labs(fill="") +
  theme(strip.background = element_blank(), legend.position = "bottom",panel.border = element_rect(color = "black", fill = NA, linewidth = 1), ) 
#guides(fill = guide_colorbar(barwidth = 20, barheight = 0.5))

#impact
naomi_ssa_shp_m %>% 
  ggplot() + 
  geom_sf(fill = "grey70", color = "black") +
  geom_sf(aes(fill=impact_cat)) +
  geom_sf(data=naomi_shp_dislv, color="black", fill=NA, linewidth=0.8) +
  scale_fill_brewer(palette = "Spectral", direction=-1, na.value = "grey90",na.translate = FALSE) +
  facet_grid(sex~age_group_label) +
  theme(strip.background = element_blank(), legend.position = "bottom",panel.border = element_rect(color = "black", fill = NA, linewidth = 1), ) 
#guides(fill = guide_colorbar(barwidth = 20, barheight = 0.5))
  
summary(naomi_ssa_shp_m$mean)
table(naomi_ssa_shp_m$country)
table(naomi_ssa_shp_m$indicator_label)
table(naomi_ssa_shp_m$age_group_label)
table(naomi_ssa_shp_m$sex)

#Plot output
# my_breaks <- c(0,1,2,3,5,10,20)
# test <- naomi_ssa_shp_m %>%
#   group_by(indicator_label) %>%
#   mutate(quantiles = quantile(mean, 0.25))

data=subset(naomi_ssa_shp_m, indicator_label=="pt")
summary(data$mean)

ggplot(data) +
  geom_sf(aes(fill=mean*1000))+
  #scale_color_distiller(palette = "Spectral")+
  #scale_fill_viridis_c(option = "D",guide = guide_colorbar(direction="horizontal")) +
  scale_fill_gradientn(colours = colorspace::diverge_hcl(3),
                       guide = guide_colorbar(direction="horizontal")) +
  facet_wrap(indicator_label~sex~age_group_label) +
  labs(fill="") +
  theme_void() +
  theme(strip.background = element_blank(), legend.position = "bottom",panel.border = element_rect(color = "black", fill = NA, linewidth = 1), ) +
  guides(fill = guide_colorbar(barwidth = 20, barheight = 0.5))

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

