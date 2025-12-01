library(raster)
library(ggplot2)
library(sf)
library(tidyverse)
library(classInt)
library(metR)
library(ggrepel)
library(dplyr)

#R_LIBS_scales#R_LIBS_SITE="C:\\Users\\adamak\\AppData\\Local\\Programs\\R\\R-4.4.3\\Library"

# Remove sci notation
options(scipen = 100, digits = 4)

#sets the directory to where this script is stored#
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#Bring in Africa countries shp and clip to extent desired
africa_adm0 <- read_sf('../Africa_admin0/afr_g2014_2013_0.shp') #shapefile

#coordinates (ensure closed polygon)
bbox_coords <- c(-20, -40, 60, 40)

# Create a polygon from the bounding box
bbox_polygon <- st_as_sf(st_sfc(st_polygon(list(matrix(c(
  bbox_coords[1], bbox_coords[2],  
  bbox_coords[3], bbox_coords[2],
  bbox_coords[3], bbox_coords[4],  
  bbox_coords[1], bbox_coords[4],
  bbox_coords[1], bbox_coords[2]   
), ncol = 2, byrow = TRUE))), crs = 4326))

st_crs(bbox_polygon)==st_crs(africa_adm0)

africa_adm0_cropped <- sf::st_intersection(bbox_polygon,africa_adm0)
africa_adm0_cropped %>% 
  ggplot() + 
  geom_sf(fill = "grey70", color = "black")

#Bring in UNAIDS Naomi model estimates
## Data downloaded from: https://naomiestimates.blob.core.windows.net/downloads/naomi-estimates.zip

naomi_indicators <- readRDS('../NAOMI/Combined Subnational dataset/naomi3_2024_07_01.rds')
naomi_indicators_adm0 <- readRDS('../NAOMI/Combined Subnational dataset/naomi1_2024_07_01.rds')
naomi_shp <- read_sf('../NAOMI/Combined Subnational dataset/2024_ssa_lowest_level_35.geojson') #shapefile

#Set subset varibales and set paramters for new vars
regions=c("ESA") #ESA, LAC, WCA
#countries <- c("South Africa","Eswatini","Lesotho","Mozambique",'Zimbabwe',"Botswana","Malawi","Zambia")
#iso3_group <- c("ZAF","SWZ","LSO","MOZ",'ZWE',"BWA","MWI","ZMB","KEN","TZA","UGA")
iso3_group <- c("MOZ")
sex_groups = c("female","male") # "male","female","both"
age_groups <- c("15-24","25-34","35-49","50+") # "15-19","20-24","25-29","30-34","35-39","40-44","45-49"

#Arguments
indicators <- c("HIV incidence","HIV prevalence","Population","infections") #
e <- 0.95 #efficacy of prevention
d <- 1 #duration of protection in years
px=130 #cost per 1 year course (two 6-monthly doses)
tx=10000 #cost over lifetime of treatment
daly=20 #DALYs associated with an HIV infection
cdt=500 #cost per DALY averted threshold
f=0.25 #effective coverage

#join and subset 
naomi_ssa_shp_m <- naomi_shp %>%
  filter(iso3 %in% iso3_group) %>%
  inner_join(naomi_indicators, by=join_by(area_id), keep=NULL) %>%
  filter(age_group_label %in% age_groups, sex %in% sex_groups, indicator_label %in% indicators) %>%
  rename_at(vars(ends_with(".x")),~str_replace(., "\\..$","")) %>% 
  select_at(vars(-ends_with(".y"),-c(indicator,lower,upper))) %>%
  dplyr::select(-age_group) %>%
  pivot_wider(names_from = c(indicator_label,age_group_label), values_from = c(mean)) %>%
  pivot_longer(
    cols = matches("population|prevalence|incidence", ignore.case = TRUE),
    names_to = c("indicator_label", "age_group_label"),
    names_sep = "_",
    values_to = "mean"
  ) %>%
  #pivot_longer(`Population_50+`:`HIV incidence_20-24`, names_sep = "_", names_to = c("indicator_label","age_group_label"), values_to = "mean") %>%
  pivot_wider(names_from = c(indicator_label), values_from = c(mean)) %>%
  mutate(incidence=`HIV incidence`,prevalence=`HIV prevalence`,
       pop_at_risk = Population*(1-prevalence),
       inc_cat=cut(incidence*1000,breaks=c(0, 1, 2,3,4,5,10,15,20), labels=c("<1","1-1.9","2-2.9","3-3.9","4-4.9","5-5.9","10-14.9","15-19.9"), right=F), #impact (infections averted)
       nnt=1/(e*incidence*d),nnt_cat=cut(nnt,breaks=c(0,100,200,500,1000,Inf), labels=c("<100","100-199","200-499","500-999","1000+"), right=F), #number needed to treat
       cd=(nnt*px-tx)/daly, cd_cat=cut(cd,breaks=c(-Inf,0,500,1000,5000,10000,Inf), labels=c("<0","0-499","500-999","1000-4999","5000-9999","10000+"), right=F), #cost effectiveness ($/Daly)
       ci=nnt*px-tx,
       #bi=, #budget impact
       pt=((tx+daly*cdt)/nnt), pt_cat=cut(pt,breaks=c(0,2.5,10,50,100,200,500),  labels=c("<2.5","<10","<50","<100","<200","<300"), right=F), #price threshold
       infections_averted=incidence*f*e*pop_at_risk, infections_averted_cat=cut(infections_averted,breaks=c(0,10,50,100,1000,Inf), labels=c("<10","10-50","50-100","100-1000",">1000"), right=F), #impact (infections averted)
       incidence_reduction=infections_averted/(incidence*(pop_at_risk))
) %>%
  select(-c(`Population`, `HIV prevalence`, `HIV incidence`))
  
naomi_ssa_shp_m_df <- data_frame(naomi_ssa_shp_m)         
inc_list <- naomi_ssa_shp_m_df %>%
  dplyr::select(area_id,iso3,incidence, pop_at_risk, age_group_label, sex)%>%
  arrange(incidence)

#simulate incidence by district/age/sex across empirical risk groups estimated from a gamma distribution of continual risk
efficacy=0.95
n=1
for (i in seq(1,nrow(inc_list),)){
  for(shape in c(1)){
      inc_list_sub <- inc_list[i,]
      samplesize=inc_list_sub$pop_at_risk
      if(is.na(samplesize==T)){
        next  # skip 3rd iteration and go to next iteration
      } 
      inc=inc_list_sub$incidence
      iso3=inc_list_sub$iso3
      area_id=inc_list_sub$area_id
      sex=inc_list_sub$sex
      age_group_label=inc_list_sub$age_group_label
      x <- rgamma(n=samplesize, shape=shape, scale=inc/shape) 
      x <- round(x*1000,2)
      pop = as.data.frame(x)
      pop$infected = ifelse(runif(nrow(pop), min=0, max=1000) > pop$x,0,1)
      total_infections = sum(pop$infected)
      total_pop = length(x)
      pop$quantile = rank(pop$x)/nrow(pop)
      for(quant in seq(0.125,0.875,0.25)){
      sub_pop <- pop %>%
        filter(quantile>quant-0.125, quantile < quant+0.125) %>% 
        summarise(
          sex=sex,
          age_group_label=age_group_label,
          iso3=iso3,
          area_id=area_id,
          inc_district=inc*1000, #incidence per thousand in the total population
          pop_district=total_pop,
          total_infections_district_age_sex=total_infections,
          pop_subsample=n(),
          total_infected_subsample=sum(infected), 
          inc_in_sample = (total_infected_subsample / pop_subsample)*1000,
          infections_averted=total_infected_subsample*efficacy,
          #infections_averted_pct=infections_averted/total_infections,
          sensitivity=total_infected_subsample/total_infections, 
          nnt=1/(e*(inc_in_sample/1000)*d),
          cdaverted=(nnt*130-tx)/daly, #based on $130/py
          pt=(tx+daly*cdt)/nnt,
          shape=shape, 
          quant_target=quant)
      df1=sub_pop
      if(n==1){df2<-df1}else(df2<-rbind(df1,df2))
      n=2
      print(paste("working on inc=",inc,"shape=" ,shape,"quantile=",quant,"sex=",sex,"age group=",age_group_label))
    }
  }
}


risk_dist_targeting_fine_scale = df2
risk_dist_targeting_fine_scale$inc_mult = risk_dist_targeting_fine_scale$inc_in_sample/risk_dist_targeting_fine_scale$inc_district #column of the relative difference in incidence between the targeted pop and the general pop = risk_dist_targeting_fine_scale %>%

## Save select dataframes ##
save(naomi_ssa_shp_m, risk_dist_targeting_fine_scale, file = "Len_optim_data_MOZ_F1524.RData")

