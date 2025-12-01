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
africa_adm0 <- read_sf('../NAOMI/Africa_admin0/afr_g2014_2013_0.shp') #shapefile

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

africa_adm0_cropped <- sf::st_intersection(bbox_polygon,africa_adm0)
africa_adm0_cropped %>% 
  ggplot() + 
  geom_sf(fill = "grey70", color = "black")

#Bring in UNAIDS Naomi model estimates
## Data downloaded from: https://naomiestimates.blob.core.windows.net/downloads/naomi-estimates.zip

naomi_indicators <- readRDS('../NAOMI/Combined Subnational dataset/naomi3_2024_07_01.rds')
names(naomi_indicators)
table(naomi_indicators$area_id)
table(naomi_indicators$indicator_label)
table(naomi_indicators$indicator)
table(naomi_indicators$region)
table(naomi_indicators$sex)
table(naomi_indicators$age_group_label)
table(naomi_indicators$calendar_quarter)

# naomi_indicators_adm0 <- readRDS('../NAOMI/Combined Subnational dataset/naomi1_2024_07_01.rds')
# names(naomi_indicators_adm0)
# table(naomi_indicators_adm0$iso3)
# table(naomi_indicators_adm0$area_level_label,naomi_indicators_adm0$area_level)
# table(naomi_indicators_adm0$calendar_quarter)
# table(naomi_indicators_adm0$calendar_quarter,naomi_indicators_adm0$area_level)
# table(naomi_indicators_adm0$age_group_label)
# table(naomi_indicators_adm0$sex)
# table(naomi_indicators_adm0$indicator_label)

naomi_shp <- read_sf('../NAOMI/Combined Subnational dataset/2024_ssa_lowest_level_35.geojson') #shapefile
names(naomi_shp)
table(naomi_shp$area_id)
table(naomi_shp$region)
table(naomi_shp$country)
table(naomi_shp$iso3)
table(naomi_shp$a)


#Set subset varibales and set paramters for new vars
regions=c("ESA") #ESA, LAC, WCA
#countries <- c("South Africa","Eswatini","Lesotho","Mozambique",'Zimbabwe',"Botswana","Malawi","Zambia")
iso3_group <- c("ZAF","SWZ","LSO","MOZ",'ZWE',"BWA","MWI","ZMB","KEN","TZA","UGA")
#iso3_group <- c("ZAF")
sex_groups = c("female","male","both") # "male","female","both"
age_groups <- c("15-19","15-24","20-24","25-34","35-49","15-49","50+") # "15-19","20-24","25-29","30-34","35-39","40-44","45-49"
#age_groups <- c("15-24","25-34","35-49","1") # "15-19","20-24","25-29","30-34","35-39","40-44","45-49"
#age_groups <- c("15-49") # "15-19","20-24","25-29","30-34","35-39","40-44","45-49"

# #Country trends in incidence
# names(naomi_indicators_adm0)
# table(naomi_indicators_adm0$country)
# time_plot <- naomi_indicators_adm0 %>%
#   filter(country %in% c("South Africa"), sex=="female", age_group_label%in%c("15-24"), area_level==1,
#          indicator %in% c("incidence")) %>%
#   ggplot() + 
#   geom_line(aes(x = calendar_quarter, y = mean*1000, color=area_name, group=area_name),size=1) +
#   scale_linetype_manual(values=c(1,2)) +
#   labs(x = "Districts (%)", y = "incidence per 1000 py") +
#   facet_grid(indicator~country, scales="free") +
#   theme_bw(base_size = 16) +
#   theme(strip.background = element_blank(), 
#         legend.position = "bottom",
#         panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
#         legend.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
# 
# table(naomi_indicators_adm0$calendar_quarter)
# table(naomi_indicators_adm0$age_group_label)
# 
# naomi_indicators_rsa <- naomi_indicators_adm0 %>%
#   filter(iso3 %in% c("ZAF"), sex%in%sex_groups, age_group_label%in%age_groups, area_level==2,
#          indicator %in% c("incidence","prevalence","population")) %>%
#   group_by(sex, age_group_label, area_id,indicator) %>%
#   arrange(sex, age_group_label, area_id,indicator,calendar_quarter) %>%
#   mutate(calendar_n = row_number()) %>%
#   filter(calendar_n==max(calendar_n)) %>%
#   dplyr::select(-indicator_label) %>%
#   pivot_wider(names_from = indicator, values_from = c(mean,se,median, mode, upper, lower))

#Arguments: We varied the DALYs associated with an HIV infection from 15 to 40 and lifetime treatment costs from USD 5,000 to USD 15,000
indicators <- c("HIV incidence","HIV prevalence","Population","infections") #
e <- 0.95 #efficacy of prevention
d <- 1 #duration of protection in years
px=130 #cost per 1 year course (two 6-monthly doses)
tx=5000 #cost over lifetime of treatment (run using a lower value of $5000 in sensitivity analysis)
daly=15 #DALYs associated with an HIV infection (run using a lower value of 15 in sensitivity analysis)
cdt=500 #cost per DALY averted threshold
f=1 #effective coverage

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
    cols = matches("Population|prevalence|incidence"),
    names_to = c("indicator_label", "age_group_label"),
    names_sep = "_",
    values_to = "mean"
  ) %>%
  pivot_wider(names_from = c(indicator_label), values_from = c(mean)) %>%
  mutate(incidence=`HIV incidence`,prevalence=`HIV prevalence`,
       pop_at_risk = Population*(1-prevalence),
       inc_cat=cut(incidence*1000,breaks=c(0, 1, 2,3,4,5,10,15,20), labels=c("<1","1-1.9","2-2.9","3-3.9","4-4.9","5-5.9","10-14.9","15-19.9"), right=F), #impact (infections averted)
       nnt=1/(e*incidence*d),nnt_cat=cut(nnt,breaks=c(0,100,200,500,1000,Inf), labels=c("<100","100-199","200-499","500-999","1000+"), right=F), #number needed to treat
       cd=(nnt*px-tx)/daly, cd_cat=cut(cd,breaks=c(-Inf,0,500,1000,5000,10000,Inf), labels=c("<0","0-499","500-999","1000-4999","5000-9999","10000+"), right=F), #cost effectiveness ($/Daly)
       ci=nnt*px-tx,
       #bi=, #budget impact
       pt=((tx+daly*cdt)/nnt), pt_cat=cut(pt,breaks=c(0,2.5,10,50,100,200,500),  labels=c("<2.5","<10","<50","<100","<200","<300"), right=F), #price threshold
       infections_expected=incidence*pop_at_risk,
       infections_averted=incidence*f*e*pop_at_risk, infections_averted_cat=cut(infections_averted,breaks=c(0,10,50,100,1000,Inf), labels=c("<10","10-50","50-100","100-1000",">1000"), right=F), #impact (infections averted)
       incidence_reduction=infections_averted/(incidence*(pop_at_risk)) #this will be the same for every district bc same coverage set
)  
  
naomi_ssa_shp_m_df <- tibble(naomi_ssa_shp_m)   

#Estimate total infections by country in both men and women 15-49 (testing)
naomi_total_1549_by_country <- as.data.frame(naomi_ssa_shp_m) %>%
  filter(iso3 %in% iso3_group, age_group_label =="15-49", sex == "both") %>%
  group_by(iso3) %>%
  summarise(total_inf_1549_bothsexes_country = sum(infections_expected),
            total_popatrisk_1549_bothsexes_country = sum(pop_at_risk))
      
## Excercise to understand cost efficiency of targeting by incidence ##

#generate 1,000 random values that follow gamma distribution
samplesize = 100000
mean <- 0.01 #mean incidence in pop
mean*samplesize #expected number of infections
shape <- 1 #shape parameter for the gamma distributed transmission probabilities
scale <- mean/shape #scale parameter for the gamma distributed transmission probabilities
x <- rgamma(n=samplesize, shape=shape, scale=scale)
hist(x,freq=F)
tmp <- hist(x, yaxt='n',ylab='Percent')
tmp2 <- pretty( tmp$counts/sum(tmp$counts)*100 )
axis(2, at=tmp2*sum(tmp$counts)/100, labels=tmp2)
x <- round(x*1000,2)
quantiles <- quantile(x, probs = seq(0.25, 0.75, 0.25), na.rm = FALSE)
quantiles
summary(x)

#sample top quartile by risk
pop = as.data.frame(x) 
#%>%
#  filter(x>quantiles[3])
nrow(pop)
coverage = 1
pop$infected = ifelse(runif(nrow(pop)*coverage, min=0, max=1000) > pop$x,0,1)
table(pop$infected)
summary(pop$x)

inf_quantiles = pop %>%
  mutate(quantile = rank(pop$x)/nrow(pop)) %>%
  arrange(-quantile) %>%
  mutate(cum_pop = row_number(),
         cum_pop_pct = 1-quantile,
         cum_infections = cumsum(infected), 
         sensitivity = cum_infections/sum(infected))
inf_quantiles

min(inf_quantiles$sensitivity[inf_quantiles$cum_pop_pct>0.50])
min(inf_quantiles$cum_pop_pct[inf_quantiles$sensitivity>0.50]) #half of all infections could be averted by targeting 19.8% of the pop with the highest risk

## Quartile risk distribution sampling with age/gender ##

naomi_ssa_shp_m_df <- data_frame(naomi_ssa_shp_m)
head(naomi_ssa_shp_m_df)
inc_list <- naomi_ssa_shp_m_df %>%
  filter(age_group_label %in% c("15-19","20-24","15-24","25-34","35-49") & sex!="both" | (age_group_label=="15-49" & sex=="both")) %>%
  dplyr::select(area_id,iso3,incidence, pop_at_risk, age_group_label, sex)%>%
  arrange(incidence)

efficacy=0.95

n=1
for (i in seq(1,nrow(inc_list),)){
  for(shape in c(1)){
    for(quant in seq(0.125,0.875,0.125)){
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
        #x <- rnbinom(samplesize, mu = inc*1000, size = shape)
        x <- rgamma(n=samplesize, shape=shape, scale=inc/shape) 
        x <- round(x*1000,2)
        pop = as.data.frame(x)
        pop$infected = ifelse(runif(nrow(pop), min=0, max=1000) > pop$x,0,1)
        total_infections = sum(pop$infected)
        total_pop = length(x)
        pop$quantile = rank(pop$x)/nrow(pop)
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
summary(risk_dist_targeting_fine_scale$inc_mult)
names(risk_dist_targeting_fine_scale)

## Save select dataframes ##
rm(naomi_indicators_adm0, naomi_indicators, df1, df2, africa_adm0, bbox_polygon, naomi_shp)

## Save R data file ##
save.image(file = "Len_optim_data_FULL_COUNTRY_LIST_v3_low_assumption_sensitivity_analysis.RData")
