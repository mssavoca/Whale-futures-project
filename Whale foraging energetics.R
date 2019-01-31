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
  geom_point() +
  geom_path(group=en_df_tidy$ID) 
#geom_smooth(aes(group=en_df_tidy$Species), color = "black")
plot_corr_en_per_h

Sp_sum %>% as.data.frame(Sp_sum)
plot_en_per_h_w_avg <- ggplot() +
  geom_point(en_df_tidy, aes(hour, log(corr_en_per_hour)), color = en_df_tidy$Species, alpha = 0.2) +
  geom_path(en_df_tidy, aes(hour, log(corr_en_per_hour)), group=en_df_tidy$ID, alpha = 0.2) +
  #geom_point(data = Sp_sum, aes(hour, mean_corr_en_per_hour, color = Species)) +
  #geom_path(data = Sp_sum, aes(hour, mean_corr_en_per_hour, group=en_df_tidy$ID))
plot_en_per_h_w_avg



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



