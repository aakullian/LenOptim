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
names(naomi_indicators)
table(naomi_indicators$area_id)
table(naomi_indicators$indicator_label)
table(naomi_indicators$indicator)
table(naomi_indicators$region)
table(naomi_indicators$sex)
table(naomi_indicators$age_group_label)
table(naomi_indicators$calendar_quarter)

naomi_indicators_adm0 <- readRDS('../NAOMI/Combined Subnational dataset/naomi1_2024_07_01.rds')
names(naomi_indicators_adm0)
table(naomi_indicators_adm0$iso3)
table(naomi_indicators_adm0$area_level_label,naomi_indicators_adm0$area_level)
table(naomi_indicators_adm0$calendar_quarter)
table(naomi_indicators_adm0$calendar_quarter,naomi_indicators_adm0$area_level)
table(naomi_indicators_adm0$age_group_label)
table(naomi_indicators_adm0$sex)
table(naomi_indicators_adm0$indicator_label)

naomi_shp <- read_sf('../NAOMI/Combined Subnational dataset/2024_ssa_lowest_level_35.geojson') #shapefile
names(naomi_shp)
table(naomi_shp$area_id)
table(naomi_shp$region)
table(naomi_shp$country)
table(naomi_shp$iso3)

#Set subset varibales and set paramters for new vars
regions=c("ESA") #ESA, LAC, WCA
#countries <- c("South Africa","Eswatini","Lesotho","Mozambique",'Zimbabwe',"Botswana","Malawi","Zambia")
#iso3_group <- c("ZAF","SWZ","LSO","MOZ",'ZWE',"BWA","MWI","ZMB","KEN","TZA","UGA")
iso3_group <- c("ZAF")
sex_groups = c("female","male","both") # "male","female","both"
#age_groups <- c("15-19","20-24","25-29","30-34","35-39","40-44","45-49","15-24","25-34","35-49","15-49") # "15-19","20-24","25-29","30-34","35-39","40-44","45-49"
age_groups <- c("15-24","25-34","35-49","15-49") # "15-19","20-24","25-29","30-34","35-39","40-44","45-49"
#age_groups <- c("15-49") # "15-19","20-24","25-29","30-34","35-39","40-44","45-49"

#Country trends in incidence
names(naomi_indicators_adm0)
table(naomi_indicators_adm0$country)
time_plot <- naomi_indicators_adm0 %>%
  filter(country %in% c("South Africa"), sex=="female", age_group_label%in%c("15-24"), area_level==1,
         indicator %in% c("incidence")) %>%
  ggplot() + 
  geom_line(aes(x = calendar_quarter, y = mean*1000, color=area_name, group=area_name),size=1) +
  scale_linetype_manual(values=c(1,2)) +
  labs(x = "Districts (%)", y = "incidence per 1000 py") +
  facet_grid(indicator~country, scales="free") +
  theme_bw(base_size = 16) +
  theme(strip.background = element_blank(), 
        legend.position = "bottom",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
        legend.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

table(naomi_indicators_adm0$calendar_quarter)
table(naomi_indicators_adm0$age_group_label)

naomi_indicators_rsa <- naomi_indicators_adm0 %>%
  filter(iso3 %in% c("ZAF"), sex%in%sex_groups, age_group_label%in%age_groups, area_level==2,
         indicator %in% c("incidence","prevalence","population")) %>%
  group_by(sex, age_group_label, area_id,indicator) %>%
  arrange(sex, age_group_label, area_id,indicator,calendar_quarter) %>%
  mutate(calendar_n = row_number()) %>%
  filter(calendar_n==max(calendar_n)) %>%
  dplyr::select(-indicator_label) %>%
  pivot_wider(names_from = indicator, values_from = c(mean,se,median, mode, upper, lower))

indicators <- c("HIV incidence","HIV prevalence","Population") #
e <- 0.95 #efficacy of prevention
d <- 1 #duration of protection in years
px=200 #cost per 1 year course (two 6-monthly doses)
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
  #labels=c("<100","100-200","200-500","500-1000",">1000")
  #labels=c("2.5", "10","50","100","200","200+"),
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
         )  

district_count <- nrow(naomi_ssa_shp_m)
summary(naomi_ssa_shp_m$cd)

## Estimate the number of infections averted and % reduction in infections for 2.5 million doses allocated in different ways

#Estimate total infections by country in both men and women 15-49
naomi_total_1549_by_country <- naomi_shp %>%
  filter(iso3 %in% iso3_group) %>%
  inner_join(naomi_indicators, by=join_by(area_id), keep=NULL) %>%
  filter(age_group_label =="15-49", sex == "both", indicator_label %in% indicators) %>%
  rename_at(vars(ends_with(".x")),~str_replace(., "\\..$","")) %>% 
  select_at(vars(-ends_with(".y"),-c(indicator,lower,upper))) %>%
  pivot_wider(names_from = indicator_label, values_from = c(mean)) %>%
  mutate(incidence=`HIV incidence`,prevalence=`HIV prevalence`)
naomi_total_1549_by_country <- as.data.frame(naomi_total_1549_by_country) %>%
  group_by(iso3) %>%
  summarise(total_inf = sum( (Population*(1-prevalence))*incidence ),
            total_pop_at_risk = sum(Population*(1-prevalence)))

naomi_total_1549_3countries <- naomi_total_1549_by_country %>%
  filter(iso3 %in% c("ZAF","MWI","ZWE")) %>%
  summarise(total_inf = sum(total_inf),
            total_pop_at_risk = sum(total_pop_at_risk))

total_infections_1549 <- naomi_total_1549_3countries$total_inf
total_popatrisk_1549 <- naomi_total_1549_3countries$total_pop_at_risk

naomi_total_1549_11countries <- naomi_total_1549_by_country %>%
  summarise(total_inf = sum(total_inf),
            total_pop_at_risk = sum(total_pop_at_risk))

total_infections_1549_11countries <- naomi_total_1549_11countries$total_inf
total_popatrisk_1549_11countries <- naomi_total_1549_11countries$total_pop_at_risk

# Exercise: allocated 1m, 2m, 10m py Len by district-level incidence and age/gender to compare to IPM results
# Use most current year incidence and assume stable incidence over time.
# allocate Len to districts based on incidence thresholds

summary(naomi_indicators_rsa$mean_incidence)
age_groups <- c("15-24","25-34","35-49","15-49") # "15-19","20-24","25-29","30-34","35-39","40-44","45-49"
units_prep=c(1*10^6,2*10^6,10*10^6) #number of courses of PrEP to allocate
time_horizon = 4 #time horizon of scale-up in years
#c=c(0.1,0.2,0.3,0.4) #scale-up % over 4 years
efficacy=0.95*0.9
d=1
px=100 #price per person per year
tx=10000 #lifetime tx cost
summary(naomi_indicators_rsa$mean_incidence)
inc_seq = seq(0,0.015,0.001)
  
total_infections_rsa_1549 <- naomi_indicators_rsa %>%
  mutate(incidence=mean_incidence, prevalence=mean_prevalence, population=mean_population,pop_at_risk = population*(1-prevalence)) %>%
  filter(age_group_label == "15-49", sex=="both") %>% #select
  mutate(count=sum(incidence*pop_at_risk))
total_infections_rsa_1549 = sum(total_infections_rsa_1549$count)

table(naomi_indicators_rsa$age_group_label)  
table(naomi_indicators_rsa$sex)  
  
#loop over all incidence thresholds
n=1
for(i in inc_seq){
  for(j in units_prep){
    df1 <- filter(naomi_indicators_rsa, sex!="both", age_group_label!="15-49") %>%
      dplyr::mutate(incidence=mean_incidence, prevalence=mean_prevalence, population=mean_population,pop_at_risk = population*(1-prevalence)) %>%
      dplyr::filter(incidence> i) %>% #select all districts with incidence greater than a threshold
      group_by(country) %>%
      dplyr::mutate(prop_at_risk=pop_at_risk/sum(pop_at_risk), #get proportion of at risk population in district i of the total at risk population
                    new_infections=pop_at_risk*time_horizon*incidence, #new_infections over time horizon
           units_prep=j*prop_at_risk, #allocate Len proportionate to size of pop at risk
           infections_averted=units_prep/(1/(efficacy*incidence*d))) %>% #estimate number of infections averted by district
      group_by(country) %>%
      dplyr::summarize(pop_at_risk=sum(pop_at_risk)*time_horizon, #get total population at risk over 4 years
              incidence=sum(incidence*prop_at_risk), #assumes same incidence rate over 4 years
              nnt=1/(incidence*efficacy*d), #assumes same nnt over 4 years
              new_infections=sum(new_infections), #gets total infections over 4 years
              units_prep=sum(units_prep),
              pt=((tx+daly*cdt)/nnt), #assumes same pt over 4 years
              cd=(nnt*px-tx)/daly,
              total_infections_averted=sum(infections_averted), #get total infections averted
              infections_averted_pct=(total_infections_averted/(total_infections_rsa_1549*time_horizon))*100, #get total infections averted among all adults 15-49
              district_count=n(), .groups='drop') %>% 
    mutate(incidence_threshold=i, 
           efficacy=efficacy, 
           total_infections_1549=total_infections_rsa_1549*time_horizon) 
    #filter(units_prep <= total_pop_at_risk)  
    if(n==1){df2<-df1}else(df2<-rbind(df1,df2))
  n=2
    }
}

allocate_len <-df2 %>% arrange(incidence_threshold)
#df2$district_count_pct = (df2$district_count/district_count)*100

## Analysis of cost of targeting on cost effectiveness ##

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

#How much more efficient do you have to be (beyond random allocation) to be cost effective given incidence and heterogeneity? In a
#low incidence high heterogeneity setting you have to be 

#generate a paramter sweep of incidence and heterogeneity
samplesize = 100000
e=0.95 #efficacy
d=1 #duration
tx=10000
cdt=500
daly=20
px_cost = 200 #cost per person per year
mean <- 0.01 #mean incidence in pop
mean*samplesize #expected number of infections
shape <- 1 #shape parameter for the gamma distributed transmission probabilities
scale <- mean/shape #scale parameter for the gamma distributed transmission probabilities
x <- rgamma(n=samplesize, shape=shape, scale=scale)
hist(x,freq=F)
x <- round(x*1000,2)
tmp <- hist(x, yaxt='n',ylab='Percent')
tmp2 <- pretty( tmp$counts/sum(tmp$counts)*100 )
axis(2, at=tmp2*sum(tmp$counts)/100, labels=tmp2)

size=1 #parameters for nbinom
prob=0.02 #parameters for nbinom
y <- rnbinom(samplesize, mu = 10, size = 10)
hist(y,freq=F)
#y <- round(y*1000,2)
tmp <- hist(y, yaxt='n',ylab='Percent')
tmp2 <- pretty( tmp$counts/sum(tmp$counts)*100 )
axis(2, at=tmp2*sum(tmp$counts)/100, labels=tmp2)
quantiles <- quantile(y, probs = c(0.1,0.2,0.25, 0.5, 0.75, 0.8,0.9), na.rm = FALSE)
quantiles
summary(x)

#Sweep over heterogeneity and targeting (percentile risk threshold for PrEP offer)

inc=0.01
coverage=1
shape=1
quant_target=0.2

n=1
for(shape in c(0.5,1,10)){
#x <- rgamma(n=samplesize, shape=shape, scale=inc/shape) 
x <- rnbinom(samplesize, mu = inc*1000, size = shape)
x <- round(x,2)
x <- as.data.frame(x) %>%
  mutate(shape=shape)
df1=x
if(n==1){df2<-df1}else(df2<-rbind(df1,df2))
n=2
}
riskhist <- df2
table(riskhist$shape)
tapply(riskhist$x, riskhist$shape, summary)

n=1
for (inc in c(0.001,0.005,0.01,0.015,0.02)){
  for(shape in c(0.5,1,10,100)){
    for(quant_target in seq(0,0.95,0.05)){
      #x <- rgamma(n=samplesize, shape=shape, scale=inc/shape) 
      x <- rnbinom(samplesize, mu = inc*1000, size = shape)
      #x <- round(x*1000,2)
      pop = as.data.frame(x)
      pop$cost_target = x/mean(x) #cost is proportional to the mean incidence 
      pop$infected = ifelse(runif(nrow(pop), min=0, max=1000) > pop$x,0,1)
      total_infections = sum(pop$infected)
      pop$quantile = rank(pop$x)/nrow(pop)
      pop <- pop %>%
        filter(quantile>quant_target) %>% #target prep via a quantile threshold of risk (e.g., <0.1 would target the upper 10% of the risk distribution)
        sample_frac(coverage) %>% #percent of population in risk group sampled (set to 1 for select all)
        summarise(
               inc_full_pop=inc*1000, #incidence per thousand in the total population
               total_infected_in_sample=sum(infected), 
               total_infections=total_infections,
               inf_averted_pct=total_infected_in_sample/total_infections,
               population=n(),
               cost_per_inf_averted = (population*px_cost)/total_infected_in_sample,
               sensitivity=total_infected_in_sample/total_infections, 
               inc_in_sample = (total_infected_in_sample / population)*1000,
               nnt=1/(e*(inc_in_sample/1000)*d),
               cost=mean(cost_target),
               pt=(tx+daly*cdt)/nnt,
               shape=shape, quant_target=quant_target)
      df1=pop
      if(n==1){df2<-df1}else(df2<-rbind(df1,df2))
      n=2
      print(paste("working on inc=",inc,"shape=" ,shape,"quantile=",quant_target))
    }
  }
}

risk_dist = df2
names(risk_dist)

hist(x)

# Sample from the incidence by district map according to segment of the risk population.  
# This will give a range of incidence rates, price thresholds, NNT by district, Which
# can then be ordered and plotted to see cumulative infections averted for each country
naomi_ssa_shp_m_df <- data_frame(naomi_ssa_shp_m)
head(naomi_ssa_shp_m_df)
inc_list <- naomi_ssa_shp_m_df %>%
  filter(age_group_label=="15-24", sex=="female") %>%
  dplyr::select(area_id,iso3,incidence, pop_at_risk)%>%
  arrange(incidence)

n=1
for (i in seq(1,nrow(inc_list),)){
  for(shape in c(1)){
    for(quant in c(0,0.25,0.5,0.75)){
      samplesize=inc_list$pop_at_risk[i]
      inc=inc_list$incidence[i]
      iso3=inc_list$iso3[i]
      area_id=inc_list$area_id[i]
      #x <- rnbinom(samplesize, mu = inc*1000, size = shape)
      x <- rgamma(n=samplesize, shape=shape, scale=inc/shape) 
      x <- round(x*1000,2)
      pop = as.data.frame(x)
      pop$infected = ifelse(runif(nrow(pop), min=0, max=1000) > pop$x,0,1)
      total_infections = sum(pop$infected)
      total_pop = length(x)
      pop$quantile = rank(pop$x)/nrow(pop)
      #pop$sampled_p = rescale(x^n/mean(x)) #probability of being sampled 
      sub_pop <- pop %>%
        filter(quantile>quant, quantile <quant+0.25) %>% #percent of population in risk group sampled (set to 1 for select all)
        summarise(
          iso3=iso3,
          area_id=area_id,
          inc_district=inc*1000, #incidence per thousand in the total population
          pop_district=total_pop,
          total_infections_district=total_infections,
          pop_subsample=n(),
          total_infected_subsample=sum(infected), 
          inc_in_sample = (total_infected_subsample / pop_subsample)*1000,
          infections_averted=total_infected_subsample*efficacy,
          #infections_averted_pct=infections_averted/total_infections,
          sensitivity=total_infected_subsample/total_infections, 
          nnt=1/(e*(inc_in_sample/1000)*d),
          cdaverted=(nnt*px-tx)/daly, #based on $100/py
          pt=(tx+daly*cdt)/nnt,
          shape=shape, 
          quant_target=paste("[",quant,"-", quant+0.25,"]"))
      df1=sub_pop
      if(n==1){df2<-df1}else(df2<-rbind(df1,df2))
      n=2
      print(paste("working on inc=",inc,"shape=" ,shape,"quantile=",quant, "-",quant+0.25))
    }
  }
}

risk_dist_targeting = df2
names(risk_dist_targeting)

# cost effectiveness by different costs associated with targeting



## Cost effectiveness and impact by targeting geographically / demographically under different risk heterogeneity assumptions



## Save select dataframes ##
rm(naomi_indicators_adm0, naomi_indicators, df1, df2, africa_adm0, bbox_polygon, naomi_shp)

## Save R data file ##
save.image(file = "Len_optim_data_RSA.RData")



