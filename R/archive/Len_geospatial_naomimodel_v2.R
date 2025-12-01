library(raster)
library(ggplot2)
library(dplyr)
library(sf)
library(tidyverse)
library(classInt)
library(metR)
library(ggrepel)

R_LIBS_SITE="C:\\Users\\adamak\\AppData\\Local\\Programs\\R\\R-4.4.3\\Library"

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
table(naomi_indicators_adm0$area_level)
table(naomi_indicators_adm0$calendar_quarter)
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
countries <- c("South Africa","Eswatini","Lesotho","Mozambique",'Zimbabwe',"Botswana","Malawi","Zambia")
iso3_group <- c("ZAF","SWZ","LSO","MOZ",'ZWE',"BWA","MWI","ZMB","KEN","TZA","UGA")
#iso3_group <- c("ZAF","MWI","ZWE")
sex_groups = c("female","male","both") # "male","female","both"
#age_groups <- c("15-19","20-24","25-29","30-34","35-39","40-44","45-49") # "15-19","20-24","25-29","30-34","35-39","40-44","45-49"
age_groups <- c("15-24","25-34","35-49","15-49") # "15-19","20-24","25-29","30-34","35-39","40-44","45-49"
#age_groups <- c("15-49") # "15-19","20-24","25-29","30-34","35-39","40-44","45-49"

indicators <- c("HIV incidence","HIV prevalence","Population") #
e <- 0.95 #efficacy of prevention
d <- 1 #duration of protection in years
px=200 #cost per 1 year course (two 6-monthly doses)
tx=10000 #cost over lifetime of treatment
daly=20 #DALYs associated with an HIV infection
cdt=500 #cost per DALY averted threshold
f=1 #effective coverage

summary(naomi_ssa_shp_m$`HIV incidence`)
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

#First estimate total infections across all countries in men and women 15-49
naomi_total_1549 <- naomi_shp %>%
  filter(iso3 %in% iso3_group) %>%
  inner_join(naomi_indicators, by=join_by(area_id), keep=NULL) %>%
  filter(age_group_label =="15-49", sex == "both", indicator_label %in% indicators) %>%
  rename_at(vars(ends_with(".x")),~str_replace(., "\\..$","")) %>% 
  select_at(vars(-ends_with(".y"),-c(indicator,lower,upper))) %>%
  pivot_wider(names_from = indicator_label, values_from = c(mean)) %>%
  mutate(incidence=`HIV incidence`,prevalence=`HIV prevalence`) %>%
  summarise(total_inf = sum( (Population*(1-prevalence))*incidence ),
            total_pop_at_risk = sum(Population*(1-prevalence)))

total_infections_1549 <- naomi_total_1549$total_inf
total_pop_at_risk_1549 <- naomi_total_1549$total_pop_at_risk

#Estimate total infections by country in both men and women 15-49
naomi_total_1549_by_country <- naomi_shp %>%
  filter(iso3 %in% iso3_group) %>%
  inner_join(naomi_indicators, by=join_by(area_id), keep=NULL) %>%
  filter(age_group_label =="15-49", sex == "both", indicator_label %in% indicators) %>%
  rename_at(vars(ends_with(".x")),~str_replace(., "\\..$","")) %>% 
  select_at(vars(-ends_with(".y"),-c(indicator,lower,upper))) %>%
  pivot_wider(names_from = indicator_label, values_from = c(mean)) %>%
  mutate(incidence=`HIV incidence`,prevalence=`HIV prevalence`) %>%
  group_by(iso3) %>%
  summarise(total_inf = sum( (Population*(1-prevalence))*incidence ),
            total_pop_at_risk = sum(Population*(1-prevalence)))

#total_infections_1549 <- naomi_total_1549_by_country$total_inf
#total_pop_at_risk_1549 <- naomi_total_1549$total_pop_at_risk

#next allocate 2.5 million courses to districts based on incidence thresholds
summary(naomi_ssa_shp_m$incidence)
p=2.5*10^6 #number of courses of PrEP to allocate
i=0.001
naomi_ssa_shp_m_df <- as.data.frame(naomi_ssa_shp_m)
inc_seq = seq(min(naomi_ssa_shp_m_df$incidence),max(naomi_ssa_shp_m_df$incidence), max(naomi_ssa_shp_m_df$incidence/10))
inc_seq = seq(0,10/1000,1/2000)
effic_seq = seq(0,1,0.05)
#loop over all incidence thresholds
n=1
for(i in inc_seq){
  for(j in effic_seq){
    df1 <- naomi_ssa_shp_m_df %>%
    filter(incidence> i) %>% #select all districts with incidence greater than a threshold
    group_by(sex, age_group_label) %>%
    mutate(prop_at_risk=pop_at_risk/sum(pop_at_risk), #get proportion of at risk population in district i of the total at risk population
           units_prep=p*prop_at_risk, #allocate 2.5 million courses proportionate to size of pop at risk
           infections_averted=units_prep/(1/(j*incidence*d))) %>% #estimate number of infections averted by district
    summarise(total_pop_at_risk=sum(pop_at_risk), #get total population at risk 
              incidence=sum(incidence*prop_at_risk),
              nnt=1/(sum(prop_at_risk*incidence)*j*d),
              total_infections=sum(pop_at_risk*incidence),
              units_prep=sum(units_prep),
              pt=((tx+daly*cdt)/nnt),
              total_infections_averted=sum(infections_averted), #get total infections averted
              infections_averted_pct=(sum(total_infections_averted)/total_infections_1549)*100, #get total infections averted among all adults 15-49
              district_count=n()) %>% 
    mutate(incidence_threshold=i, efficacy=j) 
    #filter(units_prep <= total_pop_at_risk)  
    if(n==1){df2<-df1}else(df2<-rbind(df1,df2))
  n=2
  }
}

df2<-df2 %>% arrange(sex,age_group_label,incidence_threshold)
#df2$district_count_pct = (df2$district_count/district_count)*100

breaks_z=seq(0,20,5)
summary(df2$pt)
df2 %>% filter(units_prep <= total_pop_at_risk, age_group_label=="15-24", sex=="female") %>%
  ggplot() +
  geom_raster(aes(x=incidence_threshold*1000, y=efficacy, fill=infections_averted_pct))+ 
  scale_fill_gradientn(name="% of all infections averted (15-49)",
                       colours=c("darkblue","white","red"),breaks=seq(0,20,5)) +
  stat_contour(aes(y=efficacy, x=incidence_threshold*1000, z=infections_averted_pct),  
               color="black", size=0.5, linetype=1, binwidth=2.5) +
  geom_text_contour(aes(y=efficacy, x=incidence_threshold*1000, z=infections_averted_pct), breaks=seq(0,15,1), size=4, rotate=F) +
  #facet_grid(age_group_label ~ sex) +
  theme(legend.position="bottom") +
  xlab("Incidence treshold (per 1000 py)")+ylab("Efficacy")+
  scale_x_continuous(expand=c(0,0), breaks=inc_seq*1000, trans = "reverse") +
  scale_y_continuous(expand=c(0,0), breaks=effic_seq) +
  guides(fill = guide_legend(keywidth = 2, keyheight = 1, nrow=1)) +
  theme(legend.position="bottom") +
  theme(strip.background = element_rect(colour="black", fill="white"))

breaks_z=seq(0,20,5)
summary(df2$incidence_threshold)
df2 %>% filter(units_prep <= total_pop_at_risk, age_group_label=="15-24", sex=="female") %>%
  ggplot() +
  geom_raster(aes(x=incidence_threshold*1000, y=efficacy, fill=pt))+ 
  scale_fill_gradientn(name="% of all infections averted (15-49)",
                       colours=c("darkblue","white","red"),breaks=seq(0,250,50)) +
  stat_contour(aes(y=efficacy, x=incidence_threshold*1000, z=pt),  
               color="black", size=0.5, linetype=1, binwidth=50) +
  geom_text_contour(aes(y=efficacy, x=incidence_threshold*1000, z=pt),breaks=c(10,50,200), size=7, rotate=F) +
  facet_grid(age_group_label ~ sex) +
  theme(legend.position="bottom") +
  xlab("Incidence treshold (per 1000 py)")+ylab("Efficacy")+
  scale_x_continuous(expand=c(0,0), breaks=inc_seq*1000, trans = "reverse") +
  scale_y_continuous(expand=c(0,0), breaks=effic_seq) +
  guides(fill = guide_legend(keywidth = 2, keyheight = 1, nrow=1)) +
  theme(legend.position="bottom") +
  theme(strip.background = element_rect(colour="black", fill="white"))

summary(df2$incidence_threshold)
summary(df2$efficacy)
names(df2)
plot1 <- df2 %>% filter(efficacy=="0.95", sex=="female", age_group_label!="15-49") %>%
  ggplot(aes(x = incidence_threshold*1000, y = infections_averted_pct, color=age_group_label, linetype=as.factor(efficacy), label=district_count)) + 
  geom_point(size=2,alpha=0.5) +
  #geom_point(data = df2 %>% filter(efficacy=="0.95", sex=="female", age_group_label=="15-49"), color="black") +
  #geom_line(data = df2 %>% filter(efficacy=="0.95", sex=="female", age_group_label=="15-49"), color="black", linetype=2) +
  geom_line(size=1,alpha=0.5) +
  geom_text(hjust=0, vjust=1) +
  #geom_smooth(method="loess", se=F) +
  #scale_x_continuous(breaks=seq(0,300,50),limits=c(300,0)) +
  scale_x_continuous(expand=c(0,0), breaks=inc_seq*1000, trans = "reverse") +
  scale_y_continuous(breaks=seq(0,25,5),limits=c(0,25)) +
  labs(x = "Incidence threshold (per 1000py)", y = "Infections averted (% of total 15-49)") +
  facet_grid(~sex) +
  theme_bw(base_size=16)+  theme(legend.title=element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),strip.background = element_blank(), legend.position = "bottom",panel.border = element_rect(color = "black", fill = NA, linewidth = 1), ) 
plot1

## Histograms ##
age_group_selection = "15-24"
sex_selection="female"
summary(naomi_ssa_shp_m$incidence)

plot1a <- naomi_ssa_shp_m %>% 
  filter(sex==sex_selection, age_group_label==age_group_selection) %>%
  ggplot() + 
  #geom_bar(aes(x=incidence*1000, y=nnt, color=iso3), stat="identity") +
  geom_point(aes(x=incidence*1000, y=nnt, color=iso3, size=infections_averted/1000),alpha=0.2) +
  geom_point(aes(x=incidence*1000, y=nnt,color=iso3),  size=1) +
  scale_size_continuous(range = c(0, 20)) +
  #facet_grid(sex~age_group_label, switch="y") +
  #ggtitle(paste0(toTitleCase(sex_selection)," ",age_group_selection))+
  scale_y_continuous(breaks=seq(0,10000,100), limits=c(0,2200), name="NNT", sec.axis=sec_axis(~(.*px-tx)/daly, name="Cost per DALY averted ($)"))+
  labs(x="Incidence/1000py", y="NNT", size="infections averted (1,000s)", col="country") +
  geom_hline(yintercept = 20000/200, linetype=2, size=1)+
  theme_bw(base_size = 16) +
  scale_x_continuous(expand=c(0,0), trans = "reverse", breaks=seq(0,15,5),limits=c(16,0)) +
  guides(colour = guide_legend(override.aes = list(size=10))) +
  theme(strip.background = element_blank(), legend.position="bottom", legend.direction = "horizontal",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.box="vertical",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
plot1a

plot1a <- naomi_ssa_shp_m %>% 
  filter(sex==sex_selection) %>%
  ggplot() + 
  #geom_bar(aes(x=incidence*1000, y=nnt, color=iso3), stat="identity") +
  geom_point(aes(x=incidence*1000, y=nnt, color=iso3, size=infections_averted/1000),alpha=0.5) +
  geom_point(aes(x=incidence*1000, y=nnt,color=iso3),  size=2, alpha=0.5) +
  scale_size_continuous(range = c(0, 20)) +
  facet_grid(sex~age_group_label, switch="y") +
  #ggtitle(paste0(toTitleCase(sex_selection)," ",age_group_selection))+
  scale_y_continuous(name="NNT", sec.axis=sec_axis(~(.*px-tx)/daly, name="Cost per DALY averted"))+
  labs(x="Incidence/1000py", y="NNT", size="infections averted (1,000s)", col="country") +
  geom_hline(yintercept = 20000/200, linetype=2)+
  theme_bw(base_size = 24) +
  guides(colour = guide_legend(override.aes = list(size=10))) +
  theme(strip.background = element_blank(), legend.position=c(0.8,0.8),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))
plot1a

grid.arrange(plot1a, plot1b)

## Mapping ##

#subset Africa adm0 shp
africa_adm0_cropped_subs <- subset(africa_adm0_cropped, ISO3 %in% iso3_group)

#Incidence map
library(viridis)  
age_group_selection = "15-24"
sex_selection="female"
plot2a <- naomi_ssa_shp_m %>% 
  filter(age_group_label==age_group_selection & sex==sex_selection) %>%
  ggplot() + 
  geom_sf(color = "black") +
  geom_sf(data=africa_adm0_cropped_subs, color="black", fill="grey90", linewidth=0.8) +
  geom_sf(aes(fill=inc_cat),show.legend=T) +
  geom_sf(data=africa_adm0_cropped_subs, color="black", fill=NA, linewidth=0.8) +
  viridis::scale_fill_viridis(option="G", discrete=T, na.value = "grey90",drop=FALSE, direction=-1) +
  facet_grid(sex~age_group_label, switch="y") +
  labs(fill="Incidence (per 1000py)") +
  ggtitle(paste0(toTitleCase(sex_selection)," ",age_group_selection))+
  theme_void(base_size = 16) +
  theme(strip.background = element_blank(),
        legend.position=c(0.3,0.8),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        strip.text.y = element_blank(),
        strip.text.x = element_blank()) 
plot2a

plot2a_select <- naomi_ssa_shp_m %>% 
  filter(age_group_label==age_group_selection & sex==sex_selection) %>%
  ggplot() + 
  geom_sf(color = "black") +
  geom_sf(data=africa_adm0_cropped_subs, color="black", fill="grey90", linewidth=0.8) +
  geom_sf(aes(fill=inc_cat),show.legend=T) +
  geom_sf(data=naomi_ssa_shp_m %>% 
            filter(age_group_label==age_group_selection & sex==sex_selection & cd<500),
          fill=NA, color="red",lwd=1,show.legend=F) +
  geom_sf(data=africa_adm0_cropped_subs, color="black", fill=NA, linewidth=0.8) +
  viridis::scale_fill_viridis(option="G", discrete=T, na.value = "grey90",drop=FALSE, direction=-1) +
  facet_grid(sex~age_group_label, switch="y") +
  labs(fill="Incidence (per 1000py)") +
  ggtitle(paste0(toTitleCase(sex_selection)," ",age_group_selection))+
  theme_void(base_size = 24) +
  theme(strip.background = element_blank(),
        legend.position=c(0.35,0.8),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        strip.text.y = element_blank(),
        strip.text.x = element_blank()) 
plot2a_select

grid.arrange(plot2a, plot2a_select, nrow=1)

#NNT map
library(viridis)  
age_group_selection = "15-24"
sex_selection="female"
plot2b <- naomi_ssa_shp_m %>% 
  filter(age_group_label==age_group_selection & sex==sex_selection) %>%
  ggplot() + 
  geom_sf(color = "black") +
  geom_sf(data=africa_adm0_cropped_subs, color="black", fill="grey90", linewidth=0.8) +
  geom_sf(aes(fill=nnt_cat),show.legend=T) +
  geom_sf(data=africa_adm0_cropped_subs, color="black", fill=NA, linewidth=0.8) +
  viridis::scale_fill_viridis(option="G", discrete=T, na.value = "grey90",drop=FALSE, direction=-1) +
  facet_grid(sex~age_group_label, switch="y") +
  labs(fill="NNT") +
  ggtitle(paste0(toTitleCase(sex_selection)," ",age_group_selection))+
  theme_void(base_size = 24) +
  theme(strip.background = element_blank(),
        legend.position=c(0.2,0.87),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        strip.text.y = element_blank(),
        strip.text.x = element_blank()) 
plot2b

#Cost per DALY map
library(viridis)  
age_group_selection = "15-24"
sex_selection="female"
plot2c <- naomi_ssa_shp_m %>% 
  filter(age_group_label==age_group_selection & sex==sex_selection) %>%
  ggplot() + 
  geom_sf(color = "black") +
  geom_sf(data=africa_adm0_cropped_subs, color="black", fill="grey90", linewidth=0.8) +
  geom_sf(aes(fill=cd_cat),show.legend=T) +
  geom_sf(data=africa_adm0_cropped_subs, color="black", fill=NA, linewidth=0.8) +
  viridis::scale_fill_viridis(option="G", discrete=T, na.value = "grey90",drop=FALSE, direction=-1) +
  facet_grid(sex~age_group_label, switch="y") +
  labs(fill="Cost per DALY averted") +
  ggtitle(paste0(toTitleCase(sex_selection)," ",age_group_selection))+
  theme_void(base_size = 24) +
  theme(strip.background = element_blank(),
        legend.position=c(0.3,0.87),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        strip.text.y = element_blank(),
        strip.text.x = element_blank()) 
plot2c

#Infections averted
names(naomi_ssa_shp_m)
age_group_selection = "15-24"
sex_selection="female"
plot2d <- naomi_ssa_shp_m %>% 
  filter(age_group_label==age_group_selection & sex==sex_selection) %>%
  ggplot() + 
  geom_sf(color = "black") +
  geom_sf(data=africa_adm0_cropped_subs, color="black", fill="grey90", linewidth=0.8) +
  geom_sf(aes(fill=infections_averted_cat),show.legend=T) +
  geom_sf(data=africa_adm0_cropped_subs, color="black", fill=NA, linewidth=0.8) +
  viridis::scale_fill_viridis(option="G", discrete=T, na.value = "grey90",drop=FALSE, direction=-1) +
  facet_grid(sex~age_group_label, switch="y") +
  labs(fill="Infections averted") +
  ggtitle(paste0(toTitleCase(sex_selection)," ",age_group_selection))+
  theme_void(base_size = 24) +
  theme(strip.background = element_blank(),
        legend.position=c(0.25,0.87),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        strip.text.y = element_blank(),
        strip.text.x = element_blank()) 
plot2d

#Price threshold map
plot2e <- naomi_ssa_shp_m %>% 
  filter(age_group_label=="15-24" & sex=="female") %>%
  ggplot() + 
  geom_sf(color = "black") +
  geom_sf(data=africa_adm0_cropped_subs, color="black", fill="grey90", linewidth=0.8) +
  geom_sf(aes(fill=pt_cat),show.legend=T) +
  geom_sf(data=africa_adm0_cropped_subs, color="black", fill=NA, linewidth=0.8) +
  viridis::scale_fill_viridis(option="G", discrete=T, na.value = "grey90",drop=FALSE, direction=-1) +
  #facet_grid(sex~age_group_label) +
  labs(fill="Price treshold ($/yr)") +
  #guides(fill = guide_legend(title.position = "top", title.hjust=0.5,nrow=1)) +
  theme_void(base_size = 24) +
  theme(strip.background = element_blank(), legend.position=c(0.35,0.8),panel.border = element_rect(color = "black", fill = NA, linewidth = 1), ) 
plot2e


## Infections averted by incidence targeting ##

data=naomi_ssa_shp_m_df %>% filter(age_group_label=="15-24", sex=="female", pt>99) %>% summarize(inc_line=min(incidence)*1000) 
data2=naomi_ssa_shp_m_df %>% filter(age_group_label=="15-24", sex=="female", pt>199) %>% summarize(inc_line=min(incidence)*1000) 
data3=naomi_ssa_shp_m_df %>% filter(age_group_label=="15-24", sex=="female", pt>49) %>% summarize(inc_line=min(incidence)*1000) 

table(naomi_ssa_shp_m_df$age_group_label)
plot3a <- naomi_ssa_shp_m_df %>% 
  filter(age_group_label=="15-24", sex=="female") %>%
  group_by(sex,age_group_label) %>%
  arrange(-incidence) %>%
  mutate(
    cum_pop_at_risk_p = cumsum(pop_at_risk)/total_pop_at_risk_1549,
    cum_inf_averted_p = cumsum(infections_averted)/total_infections_1549) %>%
  select(incidence, cum_pop_at_risk_p, cum_inf_averted_p, sex, age_group_label) %>%
  pivot_longer(cols = cum_pop_at_risk_p:cum_inf_averted_p, names_to = "metric",values_to = "pct") %>%
  ggplot() + 
  geom_line(aes(x = incidence*1000, y = pct*100, linetype=metric, color=age_group_label),size=1) +
  scale_linetype_manual(labels=c("Infections averted","Population at risk"), values=c(1,2)) +
  scale_x_continuous(expand=c(0,0), trans = "reverse", breaks=seq(0,15,5),limits=c(16,0)) +
  scale_y_continuous(breaks=seq(0,32,5),limits=c(0,31)) +
  
  geom_vline(xintercept = data3$inc_line, linetype="dotted", color = "black", size=1) +
  #geom_vline(xintercept = data2$inc_line, linetype="dotted", color = "black", size=1) +
  labs(x = "Minimum incidence targeted (per 1000 py)", y = "% of all aged 15-49") +
  facet_grid(sex~age_group_label) +
  theme_bw(base_size = 24) +
  theme(strip.background = element_blank(), 
        legend.position = "bottom",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
        legend.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
plot3a

table(naomi_ssa_shp_m_df$age_group_label)
plot3b <- naomi_ssa_shp_m_df %>% 
  filter(age_group_label!="15-49", sex=="female") %>%
  group_by(sex,age_group_label) %>%
  arrange(-incidence) %>%
  mutate(
         cum_pop_at_risk_p = cumsum(pop_at_risk)/total_pop_at_risk_1549,
         cum_inf_averted_p = cumsum(infections_averted)/total_infections_1549) %>%
  select(incidence, cum_pop_at_risk_p, cum_inf_averted_p, sex, age_group_label) %>%
  pivot_longer(cols = cum_pop_at_risk_p:cum_inf_averted_p, names_to = "metric",values_to = "pct") %>%
  ggplot() + 
  geom_line(aes(x = incidence*1000, y = pct*100, linetype=metric, color=age_group_label),size=1) +
  scale_linetype_manual(labels=c("Infections averted","Population at risk"), values=c(1,2)) +
  scale_x_continuous(expand=c(0,0), trans = "reverse", breaks=seq(0,15,5),limits=c(16,0)) +
  scale_y_continuous(breaks=seq(0,32,5),limits=c(0,31)) +
  
  #geom_vline(xintercept = data$inc_line, linetype="dotted", color = "black", size=1) +
  #geom_vline(xintercept = data2$inc_line, linetype="dotted", color = "black", size=1) +
  labs(x = "Minimum incidence targeted (per 1000 py)", y = "% of all aged 15-49") +
  facet_grid(sex~age_group_label) +
  theme_bw(base_size = 24) +
  theme(strip.background = element_blank(), 
        legend.position = "bottom",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
        legend.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
plot3b

#incidence map with districts >$X price threshold

data=as.data.frame(naomi_ssa_shp_m %>% 
  filter(age_group_label==age_group_selection & sex==sex_selection & pt>99))
nrow(data)

plot3c <- naomi_ssa_shp_m %>% 
  filter(age_group_label==age_group_selection & sex==sex_selection) %>%
  ggplot() + 
  geom_sf(color = "black") +
  geom_sf(data=africa_adm0_cropped_subs, color="black", fill="grey90", linewidth=0.8) +
  geom_sf(aes(fill=inc_cat),show.legend=T) +
  geom_sf(data=naomi_ssa_shp_m %>% 
            filter(age_group_label==age_group_selection & sex==sex_selection & pt>49),
    fill=NA, color="red",lwd=0.5,show.legend=F) +
  geom_sf(data=africa_adm0_cropped_subs, color="black", fill=NA, linewidth=0.8) +
  viridis::scale_fill_viridis(option="G", discrete=T, na.value = "grey90",drop=FALSE, direction=-1) +
  facet_grid(sex~age_group_label, switch="y") +
  labs(fill="Incidence (per 1000py)") +
  geom_text(x=3, y=30, label="Scatter plot") +
  ggtitle(paste0(toTitleCase(sex_selection)," ",age_group_selection))+
  theme_void(base_size = 24) +
  theme(strip.background = element_blank(),
        legend.position=c(0.3,0.85),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        strip.text.y = element_blank(),
        strip.text.x = element_blank()) 
plot3c

grid.arrange(plot3c, plot3a,nrow=1)

#
plot3a <- naomi_ssa_shp_m %>% 
  #filter(age_group_label=="15-24" ) %>%
  group_by(sex,age_group_label) %>%
  arrange(nnt) %>%
  mutate(cum_pop = cumsum(pop_at_risk)/max(cumsum(pop_at_risk)),
         impact_pop = cumsum(infections_averted)/max(cumsum(infections_averted))) %>%
ggplot() + 
  geom_line(aes(x = nnt, y = cum_pop*100)) +
  geom_line(aes(x = nnt, y = impact_pop*100),linetype=2) +
  scale_x_continuous() +
  #scale_x_continuous(trans = "reverse", breaks=seq(0,300,50),limits=c(300,0)) +
  #labs(x = "Price threshold ($/yr)", y = "cumulative population (%)") +
  facet_grid(sex~age_group_label) +
  theme_bw(base_size = 24) +
  theme(strip.background = element_blank(), legend.position = "bottom",panel.border = element_rect(color = "black", fill = NA, linewidth = 1), ) 
plot3a

grid.arrange(plot1, plot2, ncol=2)

#by country

age_group_selection = "15-24"
plot3b <- as.data.frame(naomi_ssa_shp_m) %>% 
  filter(age_group_label=="15-24", sex=="female") %>%
  group_by(sex,age_group_label,country) %>%
  arrange(country,sex,age_group_label,-pt) %>%
  inner_join(naomi_total_1549_by_country, by = join_by(iso3)) %>%
  mutate(cum_pop = cumsum(pop_at_risk)/total_pop_at_risk,
       impact_pop = cumsum(infections_averted)/total_inf,
       label=ifelse(pt==min(pt),country,NA)) %>%
  ggplot(aes(x = pt, y = impact_pop*100,color=country, label=label)) + 
  geom_line(size=1) +
  geom_line(data=as.data.frame(naomi_ssa_shp_m) %>%
              filter(age_group_label=="15-24", sex=="female") %>%
              group_by(sex,age_group_label) %>%
              arrange(sex,age_group_label,-pt) %>%
              mutate(impact_pop = cumsum(infections_averted)/total_infections_1549,
                     label=ifelse(pt==min(pt),"all countries", NA)),
            aes(x=pt, y=impact_pop*100), color="black",linetype=2) +
  #ggtitle(paste0(toTitleCase(min(naomi_ssa_shp_m$sex))," ", age_group_selection))+
  scale_x_continuous(trans = "reverse") +
  #scale_x_continuous(breaks=seq(0,300,50),limits=c(0,300)) +
  labs(x = "Price threshold ($/person/yr)", y = "Infections averted (%)") +
  geom_text(vjust=-1) +
  #scale_color_viridis(discrete = TRUE, option = "D")+
  facet_grid(sex~age_group_label) +
  theme_bw(base_size = 16) +
  theme(legend.position="none",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        strip.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) 
plot3b

grid.arrange(plot3, plot3b, ncol=2)

#Impact by price threshold
summary(naomi_ssa_shp_m$pop_at_risk)
names(naomi_ssa_shp_m)
plot4 <- naomi_ssa_shp_m %>% 
  filter(age_group_label=="15-24" ) %>%
  group_by(sex,age_group_label) %>%
  arrange(pt) %>%
  mutate(impact_pop = cumsum(infections_averted)/max(cumsum(infections_averted))) %>%
ggplot(aes(x = pt, y = impact_pop*100)) + 
  geom_line() +
  labs(x = "Price threshold ($/yr)", y = "cumulative infections averted (%)") +
  facet_grid(sex~age_group_label) +
  theme_bw(base_size = 24) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

#NNT
naomi_ssa_shp_m %>% 
  ggplot() + 
  #geom_sf(fill = "grey70", color = "black") +
  geom_sf(aes(fill=nnt_cat)) +
  geom_sf(data=naomi_shp_dislv, color="black", fill=NA, linewidth=0.8) +
  geom_sf(data=naomi_shp_dislv, color="black", fill=NA, linewidth=0.8) +
  scale_fill_brewer(palette = "RdYlGn", direction=1, na.value = "grey90",na.translate = FALSE) +
  facet_grid(sex~age_group_label) +
  labs(fill="") +
  guides(fill = guide_legend(nrow = 1))+
  theme_void() +
  theme(strip.background = element_blank(), legend.position = "bottom",panel.border = element_rect(color = "black", fill = NA, linewidth = 1), ) 
#guides(fill = guide_colorbar(barwidth = 20, barheight = 0.5))

#impact
summary(naomi_ssa_shp_m$impact)
naomi_ssa_shp_m %>% 
  ggplot() + 
  geom_sf(fill = "grey70", color = "black") +
  geom_sf(aes(fill=impact_cat)) +
  geom_sf(data=naomi_shp_dislv, color="black", fill=NA, linewidth=0.8) +
  geom_sf(data=naomi_shp_dislv, color="black", fill=NA, linewidth=0.8) +
  scale_fill_brewer(palette = "RdYlGn", direction=-1, na.value = "grey90",na.translate = FALSE) +
  facet_grid(sex~age_group_label) +
  labs(fill="Infections averted") + 
  theme_void() +
  theme(strip.background = element_blank(), legend.position = "bottom",panel.border = element_rect(color = "black", fill = NA, linewidth = 1), ) 
#guides(fill = guide_colorbar(barwidth = 20, barheight = 0.5))

#Join naomi_ssa_shp_m_total with the subgroup specific metrics to get % reduction of all infections

#incidence reduction (%)
summary(naomi_ssa_shp_m$incidence_reduction)
naomi_ssa_shp_m %>% 
  ggplot() + 
  geom_sf(fill = "grey70", color = "black") +
  geom_sf(aes(fill=impact_cat)) +
  geom_sf(data=naomi_shp_dislv, color="black", fill=NA, linewidth=0.8) +
  geom_sf(data=naomi_shp_dislv, color="black", fill=NA, linewidth=0.8) +
  scale_fill_brewer(palette = "RdYlGn", direction=-1, na.value = "grey90",na.translate = FALSE) +
  facet_grid(sex~age_group_label) +
  labs(fill="Infections averted") + 
  theme_void() +
  theme(strip.background = element_blank(), legend.position = "bottom",panel.border = element_rect(color = "black", fill = NA, linewidth = 1), ) 
#guides(fill = guide_colorbar(barwidth = 20, barheight = 0.5))
  

