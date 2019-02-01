#############################################
# Preliminary figures for whale futures paper
#############################################

# load packages
library(ggplot2)
library(dplyr)
library(readxl)
library(ggsci)
library(tidyverse)
library(mgcv)
library(lme4)


en_df <- read_csv("foragestats_combined_ko2.csv")

#creating new columns

en_df$feeding_rate = en_df$total_buzz_count/en_df$total_duration_h
en_df$en_h1 = en_df$Prey_E_kJ*en_df$feeding_rate
en_df$en_h2 = en_df$en_h1*2
en_df$en_h3 = en_df$en_h1*3
en_df$en_h4 = en_df$en_h1*4
en_df$en_h5 = en_df$en_h1*5
en_df$en_h6 = en_df$en_h1*6
en_df$en_h2 = en_df$en_h1*7
en_df$en_h3 = en_df$en_h1*8
en_df$en_h4 = en_df$en_h1*9
en_df$en_h5 = en_df$en_h1*10
en_df$en_h6 = en_df$en_h1*11
en_df$en_h6 = en_df$en_h1*12

# make the wide dataset long (i.e., tidy) 
en_df_tidy = gather(en_df, hour, en_per_hour, 42:47)

#turn certain values into numbers
en_df_tidy$hour = ifelse(en_df_tidy$hour == "en_h1", 1,
                         ifelse(en_df_tidy$hour == "en_h2", 2,
                                ifelse(en_df_tidy$hour == "en_h3", 3,
                                       ifelse(en_df_tidy$hour == "en_h4", 4,
                                              ifelse(en_df_tidy$hour == "en_h5", 5, 6)))))

en_df_tidy$corr_en_per_hour = en_df_tidy$en_per_hour/en_df_tidy$Body_mass_kg


en_df_tidy = spread(en_df_tidy, hour, en_h1_corr_mass:en_h6_corr_mass)

#
#to check if working
#
View(en_df_tidy[en_df_tidy$ID == "bb12_214a",])


# remove blank rows
en_df_tidy <- en_df_tidy[-which(is.na(en_df_tidy$en_per_hour)), ]


Sp_sum = en_df_tidy %>%
        group_by(Species, hour) %>%
        summarize(mean_corr_en_per_hour = mean(corr_en_per_hour, na.rm = TRUE))




# graph, energy in corrected for mass
plot_corr_en_per_h <- ggplot(en_df_tidy, aes(hour, log(corr_en_per_hour), color = Species)) +
  geom_point(alpha = 0.2) +
  geom_path(group=en_df_tidy$ID, alpha = 0.2) +
  geom_smooth(aes(group=en_df_tidy$Species)) + geom_smooth(color = "black", linetype="dashed") +
  theme_bw()
plot_corr_en_per_h

#get data ready to plot
Sp_sum = as.data.frame(Sp_sum)
Sp_sum$Species = as.factor(Sp_sum$Species)


###
# First plot working
###
plot_en_per_h_w_avg <- ggplot() +
  geom_point(data=en_df_tidy, aes(hour, log(corr_en_per_hour), color = Species), alpha = 0.2) +
  geom_path(data=en_df_tidy, aes(hour, log(corr_en_per_hour), color = Species), group=en_df_tidy$ID, alpha = 0.2) +
  geom_point(data = Sp_sum, aes(hour, log(mean_corr_en_per_hour), color = Species), size = 4) +
  geom_path(data = Sp_sum, aes(hour, log(mean_corr_en_per_hour), color = Species, group = Sp_sum$Species)) +
  geom_smooth(data=en_df_tidy, aes(hour, log(corr_en_per_hour)), color = "black", linetype="dashed") +
  #geom_path(data = en_df_tidy, aes(hour, log(corr_en_per_hour), color = "black", linetype="dashed")) +
  scale_fill_manual(values = c("Berardius_bairdii","Globicephala_macrorhynchus", "Globicephala_melas","Grampus_griseus", "Mesoplodon_densirostris",
                               "Orcinus_orca","Phocoena_phocoena", "Physeter_macrocephalus", "Ziphius_cavirostris"),                      
                     labels = c("Berardius bairdii","Globicephala macrorhynchus", "Globicephala melas","Grampus griseus", "Mesoplodon densirostris",
                                "Orcinus orca","Phocoena phocoena", "Physeter macrocephalus", "Ziphius cavirostris")) +
  theme_bw() +
  labs(x = "Time (hours)", y = "log[Energy gain corrected for body mass (kJ/kg)]") 
plot_en_per_h_w_avg

scale_fill_manual(name="My new legend", values=c("brown1","darkolivegreen4","burlywood3", labels=c("condition1", "condition2", "condition3")) +

# graph 
plot_en_per_h <- ggplot(en_df_tidy, aes(hour, log(en_per_hour/Body_mass_kg), color = Species)) +
  geom_point() +
  geom_path(group=en_df_tidy$ID) 
#geom_smooth(aes(group=en_df_tidy$Species), color = "black")
plot_en_per_h

a=en_df_tidy %>% as.data.frame()
plot_en_per_h <- ggplot() +
  geom_point(a, aes(hour, log(en_per_hour), color = Species)) +
  geom_line(group=en_df_tidy$ID)+
  geom_point(a,aes(hour,corr_en_per_hour),color="black")
plot_en_per_h



