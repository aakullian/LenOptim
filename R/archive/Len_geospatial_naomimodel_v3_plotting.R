library(raster)
library(ggplot2)
library(dplyr)
library(sf)
library(tidyverse)
library(classInt)
library(metR)
library(ggrepel)
library(viridis)  

#R_LIBS_SITE="C:\\Users\\adamak\\AppData\\Local\\Programs\\R\\R-4.4.3\\Library"

# Remove sci notation
options(scipen = 100, digits = 4)

#sets the directory to where this script is stored#
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#load("Len_optim_data.RData")
load("Len_optim_data_FULL_COUNTRY_LIST.RData")

table(naomi_ssa_shp_m$iso3)

## Plotting ##

breaks_z=seq(0,20,5)
summary(allocate_2_5_m_doses$pt)

# plot_inf_averted_by_inc_and_eff_F1524 <- allocate_2_5_m_doses %>% filter(units_prep <= total_pop_at_risk, age_group_label=="15-24", sex=="female") %>%
#   ggplot() +
#   geom_raster(aes(x=incidence_threshold*1000, y=efficacy, fill=infections_averted_pct))+ 
#   scale_fill_gradientn(name="% of all infections averted (15-49)",
#                        colours=c("darkblue","white","red"),breaks=seq(0,20,5)) +
#   stat_contour(aes(y=efficacy, x=incidence_threshold*1000, z=infections_averted_pct),  
#                color="black", linewidth=0.5, linetype=1, binwidth=2.5) +
#   geom_text_contour(aes(y=efficacy, x=incidence_threshold*1000, z=infections_averted_pct), breaks=seq(0,15,1), size=4, rotate=F) +
#   #facet_grid(age_group_label ~ sex) +
#   theme(legend.position="bottom") +
#   xlab("Incidence treshold (per 1000 py)")+ylab("Efficacy")+
#   scale_x_continuous(expand=c(0,0), breaks=inc_seq*1000, trans = "reverse") +
#   scale_y_continuous(expand=c(0,0), breaks=effic_seq) +
#   guides(fill = guide_legend(keywidth = 2, keyheight = 1, nrow=1)) +
#   theme(legend.position="bottom") +
#   theme(strip.background = element_rect(colour="black", fill="white"))

plot_rsa_len_allocation <- allocate_len %>% filter() %>%
  ggplot(aes(x = incidence_threshold*1000, y = infections_averted_pct, color=factor(units_prep), group=units_prep, label=district_count)) + 
  geom_point(size=2,alpha=0.5) +
  #geom_point(data = allocate_2_5_m_doses %>% filter(efficacy=="0.95", sex=="female", age_group_label=="15-49"), color="black") +
  #geom_line(data = allocate_2_5_m_doses %>% filter(efficacy=="0.95", sex=="female", age_group_label=="15-49"), color="black", linetype=2) +
  geom_line(size=1,alpha=0.5) +
  geom_text(hjust=0, vjust=1) +
  #geom_smooth(method="loess", se=F) +
  #scale_x_continuous(breaks=seq(0,300,50),limits=c(300,0)) +
  scale_x_continuous(expand=c(0,0), breaks=inc_seq*1000, limits=c(0,14)) +
  scale_y_continuous(breaks=seq(0,25,5),limits=c(0,25)) +
  labs(x = "Min incidence targeted (per 1000py)", y = "Infections averted (% of total 15-49)") +
  #facet_grid(~sex) +
  theme_bw(base_size=16)+  theme(legend.title=element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),strip.background = element_blank(), legend.position = "bottom",panel.border = element_rect(color = "black", fill = NA, linewidth = 1), ) 

## Mapping ##

#subset Africa adm0 shp
africa_adm0_cropped_subs <- subset(africa_adm0_cropped, ISO3 %in% iso3_group)

#Incidence map
age_group_selection = "15-49"
sex_selection="both"
plot_map_inc_1549 <- naomi_ssa_shp_m %>% 
  filter(age_group_label==age_group_selection & sex==sex_selection) %>%
  ggplot() + 
  #geom_sf(color = "black") +
  #geom_sf(data=africa_adm0_cropped_subs, color="black", fill="grey90", linewidth=0.8) +
  geom_sf(aes(fill=inc_cat),show.legend=T) +
  #geom_sf(data=africa_adm0_cropped_subs, color="black", fill=NA, linewidth=0.8) +
  viridis::scale_fill_viridis(option="G", discrete=T, na.value = "grey90",drop=FALSE, direction=-1) +
  facet_grid(sex~age_group_label, switch="y") +
  labs(fill="Incidence (per 1000py)") +
  #ggtitle(paste0(toTitleCase(sex_selection)," ",age_group_selection))+
  theme_void(base_size = 16) +
  theme(strip.background = element_blank(),
        legend.position=c(0.25,0.85),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        strip.text.y = element_blank(),
        strip.text.x = element_blank()) 

#% of population by incidence threshold
table(naomi_ssa_shp_m$age_group_label)
plot_popatrisk_infections_rank_1549F <-
  as.data.frame(naomi_ssa_shp_m) %>% 
  filter(age_group_label%in%c("15-24","25-34","35-49"),sex!="both") %>%
  group_by(sex,age_group_label) %>%
  arrange(-incidence) %>%
  mutate(
    rank = row_number(),
    quantile = rank/max(rank),
    cum_pop_at_risk_p = cumsum(pop_at_risk)/sum(pop_at_risk),
    cum_inf_averted_p = cumsum(infections_averted)/sum(infections_averted)) %>%
  arrange(sex, rank) %>%
  dplyr::select(incidence, rank, quantile,cum_pop_at_risk_p, cum_inf_averted_p, sex, age_group_label) %>%
  #pivot_longer(cols = cum_pop_at_risk_p:cum_inf_averted_p, names_to = "metric",values_to = "pct") %>%
  ggplot() + 
  geom_line(aes(x = quantile, y = cum_inf_averted_p, color=age_group_label, linetype=sex),size=1) +
  scale_linetype_manual(values=c(1,2)) +
  #scale_x_continuous(expand=c(0,0), breaks=seq(0,15,5),limits=c()) +
  #scale_y_continuous(breaks=seq(0,32,5),limits=c(0,31)) +
  #geom_vline(xintercept = data3$inc_line, linetype="dotted", color = "black", size=1) +
  #geom_vline(xintercept = data2$inc_line, linetype="dotted", color = "black", size=1) +
  labs(x = "Districts (%)", y = "New infections (%)") +
  #facet_grid(~sex) +
  theme_bw(base_size = 16) +
  theme(strip.background = element_blank(), 
        legend.position = "bottom",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
        legend.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

#efficiency by incidence threshold
plot_efficience_by_min_inc_pt_1549 <-
  naomi_ssa_shp_m_df %>% 
  filter(age_group_label=="15-49",sex!="both") %>%
  group_by(sex,age_group_label) %>%
  arrange(-incidence) %>%
  mutate(cum_inf_averted_per_popatrisk = cumsum(infections_averted)/cumsum(pop_at_risk)) %>%
  dplyr::select(incidence, cum_inf_averted_per_popatrisk, sex, age_group_label) %>%
  pivot_longer(cols = cum_inf_averted_per_popatrisk, names_to = "metric",values_to = "value") %>%
  ggplot() + 
  geom_line(aes(x = incidence*1000, y = value*1000, linetype=metric, color=sex),size=1) +
  scale_linetype_manual(labels=c("Infections averted","Population at risk"), values=c(1,2)) +
  #scale_x_continuous(expand=c(0,0), breaks=seq(0,15,5),limits=c()) +
  scale_y_continuous(breaks=seq(0,15,5),limits=c(0,15)) +
  #geom_vline(xintercept = data3$inc_line, linetype="dotted", color = "black", size=1) +
  #geom_vline(xintercept = data2$inc_line, linetype="dotted", color = "black", size=1) +
  labs(x = "Minimum incidence targeted (per 1000 py)", y = "Infections/Population at risk") +
  #facet_grid(sex~age_group_label) +
  theme_bw(base_size = 24) +
  theme(strip.background = element_blank(), 
        legend.position = "bottom",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
        legend.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

plot_map_inc_select_cdlt500 <- naomi_ssa_shp_m %>% 
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
  #ggtitle(paste0(toTitleCase(sex_selection)," ",age_group_selection))+
  theme_void(base_size = 24) +
  theme(strip.background = element_blank(),
        legend.position=c(0.35,0.8),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        strip.text.y = element_blank(),
        strip.text.x = element_blank()) 

#inc map 15-24
age_group_selection = "15-24"
sex_selection="female"
plot_map_inc_F1524 <- naomi_ssa_shp_m %>% 
  filter(age_group_label==age_group_selection & sex==sex_selection) %>%
  ggplot() + 
  #geom_sf(color = "black") +
  #geom_sf(data=africa_adm0_cropped_subs, color="black", fill="grey90", linewidth=0.8) +
  geom_sf(aes(fill=inc_cat),show.legend=T) +
  #geom_sf(data=africa_adm0_cropped_subs, color="black", fill=NA, linewidth=0.8) +
  viridis::scale_fill_viridis(option="G", discrete=T, na.value = "grey90",drop=FALSE, direction=-1) +
  facet_grid(sex~age_group_label, switch="y") +
  labs(fill="Incidence (per 1,000 py)") +
  #ggtitle(paste0(toTitleCase(sex_selection)," ",age_group_selection))+
  theme_void(base_size = 24) +
  theme(strip.background = element_blank(),
        legend.position=c(0.38,0.82),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        strip.text.y = element_blank(),
        strip.text.x = element_blank()) 


#NNT map
age_group_selection = "15-24"
sex_selection="female"
plot_map_nnt_F1524 <- naomi_ssa_shp_m %>% 
  filter(age_group_label==age_group_selection & sex==sex_selection) %>%
  ggplot() + 
  #geom_sf(color = "black") +
  #geom_sf(data=africa_adm0_cropped_subs, color="black", fill="grey90", linewidth=0.8) +
  geom_sf(aes(fill=nnt_cat),show.legend=T) +
  #geom_sf(data=africa_adm0_cropped_subs, color="black", fill=NA, linewidth=0.8) +
  viridis::scale_fill_viridis(option="G", discrete=T, na.value = "grey90",drop=FALSE, direction=-1) +
  facet_grid(sex~age_group_label, switch="y") +
  labs(fill="NNT") +
  #ggtitle(paste0(toTitleCase(sex_selection)," ",age_group_selection))+
  theme_void(base_size = 24) +
  theme(strip.background = element_blank(),
        legend.position=c(0.18,0.86),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        strip.text.y = element_blank(),
        strip.text.x = element_blank()) 

#Cost per DALY map
age_group_selection = "15-24"
sex_selection="female"
plot_map_cd_F1524 <- naomi_ssa_shp_m %>% 
  filter(age_group_label==age_group_selection & sex==sex_selection) %>%
  ggplot() + 
  # geom_sf(color = "black") +
  # geom_sf(data=africa_adm0_cropped_subs, color="black", fill="grey90", linewidth=0.8) +
  geom_sf(aes(fill=cd_cat),show.legend=T) +
  #geom_sf(data=africa_adm0_cropped_subs, color="black", fill=NA, linewidth=0.8) +
  viridis::scale_fill_viridis(option="G", discrete=T, na.value = "grey90",drop=FALSE, direction=-1) +
  facet_grid(sex~age_group_label, switch="y") +
  labs(fill="Cost per DALY averted") +
  #ggtitle(paste0(toTitleCase(sex_selection)," ",age_group_selection))+
  theme_void(base_size = 24) +
  theme(strip.background = element_blank(),
        legend.position=c(0.3,0.87),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        strip.text.y = element_blank(),
        strip.text.x = element_blank()) 

#Infections averted
names(naomi_ssa_shp_m)
age_group_selection = "15-24"
sex_selection="female"
plot_map_infavert_F1524 <- naomi_ssa_shp_m %>% 
  filter(age_group_label==age_group_selection & sex==sex_selection) %>%
  ggplot() + 
  #geom_sf(color = "black") +
  #geom_sf(data=africa_adm0_cropped_subs, color="black", fill="grey90", linewidth=0.8) +
  geom_sf(aes(fill=infections_averted_cat),show.legend=T) +
  #geom_sf(data=africa_adm0_cropped_subs, color="black", fill=NA, linewidth=0.8) +
  viridis::scale_fill_viridis(option="G", discrete=T, na.value = "grey90",drop=FALSE, direction=-1) +
  facet_grid(sex~age_group_label, switch="y") +
  labs(fill="Max infections averted") +
  #ggtitle(paste0(toTitleCase(sex_selection)," ",age_group_selection))+
  theme_void(base_size = 24) +
  theme(strip.background = element_blank(),
        legend.position=c(0.33,0.87),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        strip.text.y = element_blank(),
        strip.text.x = element_blank()) 

#Price threshold map
plot_map_pt_F1524 <- naomi_ssa_shp_m %>% 
  filter(age_group_label=="15-24" & sex=="female") %>%
  ggplot() + 
  #geom_sf(color = "black") +
  #geom_sf(data=africa_adm0_cropped_subs, color="black", fill="grey90", linewidth=0.8) +
  geom_sf(aes(fill=pt_cat),show.legend=T) +
  #geom_sf(data=africa_adm0_cropped_subs, color="black", fill=NA, linewidth=0.8) +
  viridis::scale_fill_viridis(option="G", discrete=T, na.value = "grey90",drop=FALSE, direction=-1) +
  #facet_grid(sex~age_group_label) +
  labs(fill="Price treshold ($/yr)") +
  #guides(fill = guide_legend(title.position = "top", title.hjust=0.5,nrow=1)) +
  theme_void(base_size = 24) +
  theme(strip.background = element_blank(),         
        legend.position=c(0.33,0.85),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) 


## Histograms ##
age_group_selection = "15-24"
sex_selection="female"
summary(plot_nnt_dalyaverted_by_inc_1524F$incidence)

plot_nnt_dalyaverted_by_inc_1524F <- naomi_ssa_shp_m %>% 
  filter(sex==sex_selection, age_group_label==age_group_selection) %>%
  ggplot() + 
  #geom_bar(aes(x=incidence*1000, y=nnt, color=iso3), stat="identity") +
  geom_point(aes(x=incidence*1000, y=nnt, color=iso3, size=infections_averted/1000),alpha=0.1) +
  geom_point(aes(x=incidence*1000, y=nnt,color=iso3),  size=1.5) +
  scale_size_continuous(range = c(0, 20)) +
  #facet_grid(sex~age_group_label, switch="y") +
  #ggtitle(paste0(toTitleCase(sex_selection)," ",age_group_selection))+
  scale_y_continuous(breaks=seq(0,10000,100), limits=c(0,2200), name="NNT", sec.axis=sec_axis(~(.*px-tx)/daly, name="Cost per DALY averted ($)"))+
  labs(x="Incidence/1000py", y="NNT", size="infections averted (1,000s)", col="country") +
  geom_hline(yintercept = 20000/200, linetype=2, size=1)+
  theme_bw(base_size = 24) +
  scale_x_continuous(expand=c(0,0), breaks=seq(0,15,5),limits=c(0,16)) +
  guides(colour = guide_legend(override.aes = list(size=10))) +
  theme(strip.background = element_blank(), legend.position="bottom", legend.direction = "horizontal",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.box="vertical",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

## Infections averted by incidence targeting ##

data=naomi_ssa_shp_m_df %>% filter(age_group_label=="15-24", sex=="female", pt>99) %>% summarize(inc_line=min(incidence)*1000) 
data2=naomi_ssa_shp_m_df %>% filter(age_group_label=="15-24", sex=="female", pt>199) %>% summarize(inc_line=min(incidence)*1000) 
data3=naomi_ssa_shp_m_df %>% filter(age_group_label=="15-24", sex=="female", pt>49) %>% summarize(inc_line=min(incidence)*1000) 

table(naomi_ssa_shp_m_df$iso3)
plot_inf_averted_by_min_inc_pt_1524F <- naomi_ssa_shp_m_df %>% 
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
  #scale_y_continuous(breaks=seq(0,32,5),limits=c(0,31)) +
  
  geom_vline(xintercept = data3$inc_line, linetype="dotted", color = "black", size=1) +
  #geom_vline(xintercept = data2$inc_line, linetype="dotted", color = "black", size=1) +
  labs(x = "Minimum incidence targeted (per 1000 py)", y = "% of all aged 15-49") +
  facet_grid(sex~age_group_label) +
  theme_bw(base_size = 24) +
  theme(strip.background = element_blank(), 
        legend.position = "bottom",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
        legend.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

table(naomi_ssa_shp_m_df$age_group_label)
plot_inf_averted_by_min_inc_pt_allagesF <- naomi_ssa_shp_m_df %>% 
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

#incidence map with districts >$X price threshold

data=as.data.frame(naomi_ssa_shp_m %>% 
  filter(age_group_label==age_group_selection & sex==sex_selection & pt>200))
nrow(data)

plot_map_inc_pt200_1524F <- naomi_ssa_shp_m %>% 
  filter(age_group_label==age_group_selection & sex==sex_selection) %>%
  ggplot() + 
  #geom_sf(color = "black") +
  #geom_sf(data=africa_adm0_cropped_subs, color="black", fill="grey90", linewidth=0.8) +
  geom_sf(aes(fill=inc_cat),show.legend=T) +
  geom_sf(data=naomi_ssa_shp_m %>%  filter(age_group_label==age_group_selection & sex==sex_selection & pt>130),
    fill=NA, color="red",lwd=0.5,show.legend=F) +
  #geom_sf(data=africa_adm0_cropped_subs, color="black", fill=NA, linewidth=0.8) +
  viridis::scale_fill_viridis(option="G", discrete=T, na.value = "grey90",drop=FALSE, direction=-1) +
  facet_grid(sex~age_group_label, switch="y") +
  labs(fill="Incidence (per 1000py)") +
  geom_text(x=3, y=30, label="Scatter plot") +
  #ggtitle(paste0(toTitleCase(sex_selection)," ",age_group_selection))+
  theme_void(base_size = 16) +
  theme(strip.background = element_blank(),
        legend.position=c(0.30,0.80),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        strip.text.y = element_blank(),
        strip.text.x = element_blank()) 

#by country

age_group_selection = "15-24"
plot_inf_averted_by_pt_3countries <- as.data.frame(naomi_ssa_shp_m) %>% 
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
              mutate(impact_pop = cumsum(infections_averted)/total_infections_1549_11countries,
                     label=ifelse(pt==min(pt),"all countries", NA)),
            aes(x=pt, y=impact_pop*100), color="black",linetype=2) +
  scale_x_continuous(trans = "reverse") +
  #scale_x_continuous(breaks=seq(0,300,50),limits=c(0,300)) +
  labs(x = "Price threshold ($/person/yr)", y = "Max infections averted (%)") +
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


##Incidence reduction by price threshold ordered by decreasing incidence

##Incidence reduction by price threshold ordered by decreasing incidence
names(risk_dist_targeting)
table(risk_dist_targeting$quant_target)
plot_inf_averted_by_pt_3countries_targeting <- as.data.frame(risk_dist_targeting) %>% 
  inner_join(naomi_total_1549_by_country, by = join_by(iso3)) %>%
  arrange(quant_target,-inc_in_sample) %>%
  group_by(quant_target) %>%
  mutate(cum_pop = cumsum(pop_subsample)/sum(pop_district),
         impact_pop = cumsum(infections_averted)/sum(total_infections_district)) %>%
  ggplot(aes(x = cum_pop*100, y = impact_pop*100,color=quant_target,group=quant_target)) + 
  geom_line(size=1) +
  geom_abline(linetype=2)+
  scale_x_continuous(breaks=seq(0,60,5)) +
  scale_y_continuous(breaks=seq(0,60,5)) +
  labs(x = "Pop targeted (%)", y = "Infections averted (%)") +
  theme_bw(base_size = 16) +
  theme(legend.position="bottom",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        strip.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) 

plot_inf_averted_by_pt_3countries_targeting <- as.data.frame(risk_dist_targeting) %>% 
  group_by(iso3) %>%
  arrange(iso3,-pt) %>%
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
              mutate(impact_pop = cumsum(infections_averted)/total_infections_1549_11countries,
                     label=ifelse(pt==min(pt),"all countries", NA)),
            aes(x=pt, y=impact_pop*100), color="black",linetype=2) +
  scale_x_continuous(trans = "reverse") +
  #scale_x_continuous(breaks=seq(0,300,50),limits=c(0,300)) +
  labs(x = "Price threshold ($/person/yr)", y = "Max infections averted (%)") +
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

## Age distributions by district ##
table(naomi_ssa_shp_m$age_group_label)
table(naomi_ssa_shp_m$area_id)

sex_groups = c("female","male") # "male","female","both"
age_groups <- c("15-19","20-24","25-29","30-34","35-39","40-44","45-49") # "15-19","20-24","25-29","30-34","35-39","40-44","45-49"

plot_age_dist_facet_female <- naomi_ssa_shp_m %>% 
  filter(sex=="female", age_group_label %in% age_groups) %>%
  group_by(area_id, sex) %>%
  mutate(inc_rel_1519=incidence/incidence[age_group_label=="15-19"]) %>%
  ggplot(aes(x = age_group_label, y = inc_rel_1519, group=area_id, color=country)) + 
  geom_point(size=2) +
  geom_line(stat = "smooth", method = loess, span=0.6, size = 1) +
  labs(x = "Age", y = "Incidence relative to 15-19") +
  #scale_y_continuous(limits=c(0,3))+
  facet_wrap(~country, nrow=2, scales="free") +
  scale_color_viridis(discrete = TRUE, option = "D")+
  theme_bw(base_size = 16) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.background = element_blank(), 
        legend.position = "bottom",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
        legend.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

plot_age_dist_facet_male <- naomi_ssa_shp_m %>% 
  filter(sex=="male", age_group_label %in% age_groups) %>%
  group_by(area_id, sex) %>%
  mutate(inc_rel_1519=incidence/incidence[age_group_label=="15-19"]) %>%
  ggplot(aes(x = age_group_label, y = inc_rel_1519, group=area_id, color=country)) + 
  geom_point(size=2) +
  geom_line(stat = "smooth", method = loess, span=0.6, size = 1) +
  labs(x = "Age", y = "Incidence relative to 15-19") +
  #scale_y_continuous(limits=c(0,3))+
  facet_wrap(~country, nrow=2, scales="free") +
  scale_color_viridis(discrete = TRUE, option = "D")+
  theme_bw(base_size = 16) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.background = element_blank(), 
        legend.position = "bottom",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
        legend.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

plot_age_dist_female <- naomi_ssa_shp_m %>% 
  filter(sex=="female", age_group_label %in% age_groups) %>%
  group_by(area_id, sex) %>%
  mutate(inc_rel_1519=incidence/incidence[age_group_label=="15-19"]) %>%
  ggplot(aes(x = age_group_label, y = inc_rel_1519,color=country,group=area_id)) +
  geom_point(size=2,alpha=0.5) +
  geom_line(size=1,alpha=0.1) +
  #geom_line(aes(group=area_id), stat = "smooth", method = loess, span=0.6, size = 1,alpha=0.05) +
  #geom_line(aes(group=1), stat = "smooth", method = loess, span=0.01, size = 1,color="black",linetype=2) +
  #geom_line(aes(group=1)) +
  labs(x = "Age", y = "Incidence relative to 15-19") +
  scale_y_continuous(limits=c(0.25,2))+
  facet_wrap(~country) +
  #scale_color_viridis(discrete = TRUE, option = "D")+
  theme_bw(base_size = 16) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.background = element_blank(), 
        legend.position = "bottom",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
        legend.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())  

plot_age_dist_male <- naomi_ssa_shp_m %>% 
  filter(sex=="male", age_group_label %in% age_groups) %>%
  group_by(area_id, sex) %>%
  mutate(inc_rel_1519=incidence/incidence[age_group_label=="15-19"]) %>%
  ggplot(aes(x = age_group_label, y = inc_rel_1519,color=country)) +
  geom_point(size=2,alpha=0.5) +
  geom_line(aes(group=area_id), stat = "smooth", method = loess, span=0.6, size = 1,alpha=0.05) +
  geom_line(aes(group=1), stat = "smooth", method = loess, span=0.6, size = 2,color="black",linetype=2) +
  labs(x = "Age", y = "Incidence relative to 15-19") +
  #scale_y_continuous(limits=c(0,3))+
  facet_wrap(~country) +
  #scale_color_viridis(discrete = TRUE, option = "D")+
  theme_bw(base_size = 16) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.background = element_blank(), 
        legend.position = "bottom",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
        legend.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())  

plot_age_dist_both <- naomi_ssa_shp_m %>% 
  filter(sex!="both",age_group_label %in% age_groups) %>%
  group_by(area_id, sex) %>%
  mutate(inc_rel_1519=incidence/incidence[age_group_label=="15-19"]) %>%
  ggplot(aes(x = age_group_label, y = inc_rel_1519,color=country)) +
  geom_point(size=2,alpha=0.5) +
  geom_line(aes(group=area_id), stat = "smooth", method = loess, span=0.6, size = 1,alpha=0.05) +
  geom_line(aes(group=1), stat = "smooth", method = loess, span=0.6, size = 2,color="black",linetype=2) +
  labs(x = "Age", y = "Incidence relative to 15-19") +
  scale_y_continuous()+
  facet_grid(sex~country,scales="free") +
  #scale_color_viridis(discrete = TRUE, option = "D")+
  theme_bw(base_size = 16) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.background = element_blank(), 
        legend.position = "bottom",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1), 
        legend.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())  


## Risk heterogeneity ##

# Histogram with density plot
risk_hist <- ggplot(riskhist, aes(x = x,color = as.factor(shape),group=shape)) +
  #geom_histogram(aes(y = ..density..), position = "identity",
  #               colour="black", cex = 0.8) +
  geom_density(alpha=0.25,adjust=2,linewidth=2)+
  scale_x_continuous(limits=c(0,50), breaks=seq(0,50,10))+
  theme_bw(base_size = 16) +
  labs(color='Shape parameter') + xlab("risk (per 1000 py)") +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        strip.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) 

cum_inf_by_risk_dist <- filter(risk_dist) %>%
  pivot_longer(cols = total_infected_in_sample:pt, names_to = "metric",values_to = "value") %>%
  filter(shape<100, inc_full_pop==1, metric %in% c("inf_averted_pct")) %>%
  ggplot(aes(x=(quant_target)*100, y=value*100, group=shape ,color=factor(shape))) +
  geom_point()+ 
  #geom_line() +
  #geom_line(stat = "smooth", method = gam, size = 1) +
  geom_smooth(span=0.7,se=F,size=1.5)+
  #scale_color_gradientn(name="", colours=c("blue","white","red","darkred"),breaks=c(0.5,1,2,3,4,5),na.value="transparent") +
  #stat_contour(aes(y=1-artcov, x=prev, z=inc*100),color="black",breaks=c(0.1,1,2,4), size=0.8) +
  #geom_text_contour(aes(y=1-artcov, x=prev, z=inc*100),breaks=c(0.1,1,2,4), size=5, rotate=F) +
  theme(legend.position="bottom") +
  #facet_grid(metric~shape,scales="free")+
  #geom_point(data=StudyDataMaster, aes(x=pHIV*100, y=1-pART, color=Study)) +
  xlab("Percentile of the risk distribution targeted")+  ylab("cumulative infections (%)")+
  scale_x_continuous(expand=c(0,0), trans='reverse') +
  #scale_y_continuous(limits=c(0,500)) +
  theme_bw(base_size=16)+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),strip.background = element_blank(), 
        legend.position = "bottom",panel.border = element_rect(color = "black", fill = NA, linewidth = 1), ) +
  guides(color=guide_legend(nrow=1,byrow=TRUE, title="shape (risk heterogeneity)"))

inc_facets <- c("1"="1/1000py","5"="5/1000py","10"="10/1000py")

pt_by_risk_dist <- risk_dist %>%
  pivot_longer(cols = total_infected_in_sample:pt, names_to = "metric",values_to = "value") %>%
  filter(shape<100, inc_full_pop<15, metric %in% c("pt")) %>%
  ggplot(aes(x=quant_target*100, y=value, group=shape ,color=factor(shape))) +
  geom_point()+ 
  #geom_line() +
  #geom_line(stat = "smooth", method = gam, size = 1) +
  geom_smooth(span=1,se=F)+
  geom_hline(yintercept = 200, linetype=2) +
  #scale_color_gradientn(name="", colours=c("blue","white","red","darkred"),breaks=c(0.5,1,2,3,4,5),na.value="transparent") +
  #stat_contour(aes(y=1-artcov, x=prev, z=inc*100),color="black",breaks=c(0.1,1,2,4), size=0.8) +
  #geom_text_contour(aes(y=1-artcov, x=prev, z=inc*100),breaks=c(0.1,1,2,4), size=5, rotate=F) +
  theme(legend.position="bottom") +
  facet_grid(metric~inc_full_pop,scales="free", labeller=as_labeller(inc_facets))+
  #geom_point(data=StudyDataMaster, aes(x=pHIV*100, y=1-pART, color=Study)) +
  xlab("Percentile of the risk distribution targeted")+  ylab("price threshold ($ per py)")+
  scale_x_continuous(expand=c(0,0),trans="reverse") +
  #coord_cartesian(ylim=c(25,500))+
  scale_y_continuous(trans="log10",breaks=c(50,100,200,1000)) +
  theme_bw(base_size=16)+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),strip.background = element_blank(), 
        legend.position = "bottom",panel.border = element_rect(color = "black", fill = NA, linewidth = 1), ) +
  guides(color=guide_legend(nrow=1,byrow=TRUE, title="shape (risk heterogeneity)"))

ggarrange(risk_hist,cum_inf_by_risk_dist, pt_by_risk_dist,
          nrow = 1,
          labels = c("A", "B","C"),
          common.legend = T)

#Plot cost effectiveness by different risk targeting




## 




## Save R data file ##
rm(africa_adm0_cropped, africa_adm0_cropped_subs, data, data2, data3, allocate_2_5_m_doses, naomi_ssa_shp_m, naomi_ssa_shp_m_5yr, naomi_ssa_shp_m_df)
save.image(file = "plot_Len_optim_data.RData")
