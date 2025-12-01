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

############################################################################################################
# Set parameters
############################################################################################################

#Set subset varibales and set paramters for new vars
regions=c("ESA") #ESA, LAC, WCA
#countries <- c("South Africa","Eswatini","Lesotho","Mozambique",'Zimbabwe',"Botswana","Malawi","Zambia")
iso3_group <- c("KEN") # options: "ZAF","SWZ","LSO","MOZ",'ZWE',"BWA","MWI","ZMB","KEN","TZA","UGA"
sex_groups = c("female","male") # options: "male","female","both"
age_groups <- c("15-24","25-34","35-49") #options: "15-19","15-24","15-49","20-24","25-29","25-34","30-34","35-39","35-49","40-44","45-49","50+"

#Arguments:
indicators <- c("HIV incidence","HIV prevalence","Population","infections") #
e <- 0.95 #efficacy of prevention
d <- 1 #duration of protection in years
px=130 #cost per 1 year course (two 6-monthly doses)
tx=10000 #cost over lifetime of treatment (run using a lower value of $5000 in sensitivity analysis)
daly=20 #DALYs associated with an HIV infection (run using a lower value of 15 in sensitivity analysis)
cdt=500 #cost per DALY averted threshold
f=1 #effective coverage (proportion of pop at risk who would take up PrEP)

############################################################################################################
# Load data
############################################################################################################
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
naomi_indicators <- readRDS('../NAOMI/Combined Subnational dataset/naomi3_2024_07_01.rds')
naomi_shp <- read_sf('../NAOMI/Combined Subnational dataset/2024_ssa_lowest_level_35.geojson') #shapefile

#####################################################################################################
#join and subset 
#####################################################################################################

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
      
## Exercise to understand cost efficiency of targeting by incidence ##

#generate 1,000 random values that follow gamma distribution
samplesize = 100000
mean <- 0.01 #mean incidence in pop (per person-year)
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

#Create dataframe to run over all risk strata
inc_list <- naomi_ssa_shp_m_df %>%
  dplyr::select(area_id,iso3,incidence, pop_at_risk, age_group_label, sex)%>%
  arrange(incidence)

efficacy=e #set efficacy to e above
n_risk_groups = 8 #number of risk groups to sample (quantiles)

n=1
for (i in seq(1,nrow(inc_list),)){
  for(shape in c(1)){
    for(quant in seq(1/n_risk_groups,1,1/n_risk_groups)){ #quantiles to sample around
        inc_list_sub <- inc_list[i,]
        samplesize=inc_list_sub$pop_at_risk
        if(is.na(samplesize==T)){
          next  
        } 
        inc=inc_list_sub$incidence
        iso3=inc_list_sub$iso3
        area_id=inc_list_sub$area_id
        sex=inc_list_sub$sex
        age_group_label=inc_list_sub$age_group_label
        #x <- rnbinom(samplesize, mu = inc*1000, size = shape) #if using a negative binomial distribution
        x <- rgamma(n=samplesize, shape=shape, scale=inc/shape) #gamma distribution
        x <- round(x*1000,2)
        pop = as.data.frame(x) #create pop dataframe
        pop$infected = ifelse(runif(nrow(pop), min=0, max=1000) > pop$x,0,1) #determine infections based on risk distribution
        total_infections = sum(pop$infected)
        total_pop = length(x)
        pop$quantile = rank(pop$x)/nrow(pop)
        sub_pop <- pop %>% #filter to the target quantile and then stitch together
        filter(quantile>quant-1/n_risk_groups, quantile < quant) %>% #targeting each of the n_risk_groups quantiles
        summarise(
          sex=sex,
          age_group_label=age_group_label,
          iso3=iso3,
          area_id=area_id,
          inc_district=inc*1000, #incidence per thousand in the total population
          pop_district=total_pop,
          total_infections_district_age_sex=total_infections, #total infections in the age/sex/district strata
          pop_subsample=n(), #population in the targeted subsample
          total_infected_subsample=sum(infected), #infections in the targeted subsample
          inc_in_sample = (total_infected_subsample / pop_subsample)*1000, #incidence per thousand in the targeted subsample
          infections_averted=total_infected_subsample*efficacy, #infections averted in the targeted subsample
          sensitivity=total_infected_subsample/total_infections, 
          nnt=1/(e*(inc_in_sample/1000)*d), #number needed to treat in the targeted subsample
          cdaverted=(nnt*130-tx)/daly, #cost per DALY averted based on $130/py cost of PrEP
          pt=(tx+daly*cdt)/nnt, #price threshold (max price at which Len is cost-effective)
          shape=shape, #shape parameter used
          quant_target=quant) #quantile targeted
      df1=sub_pop
      if(n==1){df2<-df1}else(df2<-rbind(df1,df2))
      n=2
      print(paste("working on inc=",inc,"shape=" ,shape,"quantile=",quant,"sex=",sex,"age group=",age_group_label))
        }
      }
    }

df2$inc_mult = df2$inc_in_sample/df2$inc_district #column of the relative difference in incidence between the targeted pop and the general pop = risk_dist_targeting_fine_scale %>%
name <- paste0("naomi_risk_dist_targeting_",iso3_group,"_",n_risk_groups,"_risk_groups")
assign(name, df2) #create final dataframe

## Save select dataframes ##
all_objects = ls()
objects_to_keep <- c("naomi_ssa_shp_m", name,"africa_adm0_cropped", "risk_dist", "iso3_group", "n_risk_groups")
# Identify objects to remove (those not in objects_to_keep)
objects_to_remove <- setdiff(all_objects, objects_to_keep)
# Remove the identified objects
rm(list = objects_to_remove)

## Save R data file ##
#save.image(file = "Len_optim_data_FULL_COUNTRY_LIST_v3.RData")
save.image(file = paste0("Len_optim_data_",iso3_group,"_", n_risk_groups,"_risk_groups.RData"))
