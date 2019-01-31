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
en_df$en_h1_corr_mass = en_df$en_h1/en_df$Body_mass_kg
en_df$en_h2 = en_df$en_h1*2
en_df$en_h2_corr_mass = en_df$en_h2/en_df$Body_mass_kg
en_df$en_h3 = en_df$en_h1*3
en_df$en_h3_corr_mass = en_df$en_h3/en_df$Body_mass_kg
en_df$en_h4 = en_df$en_h1*4
en_df$en_h4_corr_mass = en_df$en_h4/en_df$Body_mass_kg
en_df$en_h5 = en_df$en_h1*5
en_df$en_h5_corr_mass = en_df$en_h5/en_df$Body_mass_kg
en_df$en_h6 = en_df$en_h1*6
en_df$en_h6_corr_mass = en_df$en_h6/en_df$Body_mass_kg

# make the wide dataset long (i.e., tidy) 
en_df_tidy = gather(en_df, hour, en_per_hour, c(42,44,46,48,50,52))
#en_df_tidy = gather(en_df_tidy, hour, corr_en_per_hour, c(42:47))

#to check if working
x <- en_df_tidy[en_df_tidy$ID == "bb12_214a",]

#turn certain values into numbers
en_df_tidy$hour = ifelse(en_df_tidy$hour == "en_h1", 1,
                    ifelse(en_df_tidy$hour == "en_h2", 2,
                           ifelse(en_df_tidy$hour == "en_h3", 3,
                                  ifelse(en_df_tidy$hour == "en_h4", 4,
                                         ifelse(en_df_tidy$hour == "en_h5", 5, 6)))))

# remove blank rows
en_df_tidy <- en_df_tidy[-which(is.na(en_df_tidy$en_per_hour)), ]



# graph 
plot_en_per_h <- ggplot(en_df_tidy, aes(hour, log(en_per_hour), color = Species)) +
  geom_point() +
  geom_path(group=en_df_tidy$ID) +
  geom_smooth(aes(group=en_df_tidy$Species), color = "black")
plot_en_per_h

a=en_df_tidy %>% as.data.frame()
plot_en_per_h <- ggplot() +
  geom_point(a, aes(hour, log(en_per_hour), color = Species)) +
  geom_line(group=en_df_tidy$ID)+
  geom_point(a,aes(hour,corr_en_per_hour),color="black")
plot_en_per_h


