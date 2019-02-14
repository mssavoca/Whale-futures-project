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

# formula for standard error
SE = function(x){sd(x)/sqrt(sum(!is.na(x)))}

RorqualData <- read_csv("lunge_rates_from_Paolo.csv")
RorqualData$`deployment-time_h` <- (RorqualData$`deployment-time_secs`)/60/60

OdontoceteData <- read_csv("foragestats_combined_ko2.csv")

en_df <- merge(RorqualData, OdontoceteData, by = "ID", all.x = TRUE, all.y = TRUE)

# coalescing to merge two columns into one
en_df$TotalFeedingEvents = coalesce(en_df$total_lunges, en_df$total_buzz_count)
head(en_df$TotalFeedingEvents)

en_df$TotalTagTime_h = coalesce(en_df$`deployment-time_h`, en_df$total_duration_h)
head(en_df$TotalTagTime_h)

# could also try transmute(iris, sepal = Sepal.Length + Sepal. Width) to drop original columns


en_df <- filter(en_df, !Species %in% NA)  #removes rows with NA in Species column
en_df$Species <- gsub("_", " ", en_df$Species) #replaces underscore with space in the Species column

en_df$taxa <- gsub("M", "Mysticete", en_df$taxa)
en_df$taxa <- gsub("O", "Odontocete", en_df$taxa)

#creating new columns
en_df$feeding_rate = en_df$TotalFeedingEvents/en_df$TotalTagTime_h
en_df$en_h1 = en_df$Prey_E_kJ*en_df$feeding_rate
en_df$en_h2 = en_df$en_h1*2
en_df$en_h3 = en_df$en_h1*3
en_df$en_h4 = en_df$en_h1*4
en_df$en_h5 = en_df$en_h1*5
en_df$en_h6 = en_df$en_h1*6
# en_df$en_h2 = en_df$en_h1*7
# en_df$en_h3 = en_df$en_h1*8
# en_df$en_h4 = en_df$en_h1*9
# en_df$en_h5 = en_df$en_h1*10
# en_df$en_h6 = en_df$en_h1*11
# en_df$en_h6 = en_df$en_h1*12

# energy acquired in a day
en_df$en_day = en_df$en_h1*24
en_df$corr_en_day = en_df$en_day/en_df$Body_mass_kg

# mass of prey consumed in an hour / day
en_df$prey_wt_g_h1 = en_df$Prey_wt_g*en_df$feeding_rate
en_df$prey_wt_g_day = en_df$prey_wt_g_h1*24

# make the wide dataset long (i.e., tidy) 
en_df_tidy = gather(en_df, hour, en_per_hour, 53:58)

#turn certain values into numbers
en_df_tidy$hour = ifelse(en_df_tidy$hour == "en_h1", 1,
                         ifelse(en_df_tidy$hour == "en_h2", 2,
                                ifelse(en_df_tidy$hour == "en_h3", 3,
                                       ifelse(en_df_tidy$hour == "en_h4", 4,
                                              ifelse(en_df_tidy$hour == "en_h5", 5, 6)))))

en_df_tidy$corr_en_per_hour = en_df_tidy$en_per_hour/en_df_tidy$Body_mass_kg


#en_df_tidy = spread(en_df_tidy, hour, en_h1_corr_mass:en_h6_corr_mass)

#
#to check if working
#
View(en_df_tidy[en_df_tidy$ID == "bb12_214a",])


# remove blank rows
en_df_tidy <- en_df_tidy[-which(is.na(en_df_tidy$en_per_hour)), ]


# data summary filtering by lunge quality, removing any NA Species or NAs in feeding rate, filtering to tak
Sp_sum = en_df_tidy %>%
        drop_na(feeding_rate) %>% 
        filter(lunge_quality %in% c("ok", "good", NA, "good dives", "good_dives")) %>% 
        filter(sonar_exp %in% c("none", NA) & TotalTagTime_h > 23) %>%     #Filtering to only dives of more than 24 h
        group_by(taxa, Species) %>%
        dplyr::summarize(mean_en_per_hour = mean(en_per_hour),
          mean_corr_en_per_hour = mean(corr_en_per_hour),
          mean_prey_wt_g_per_hour = mean(prey_wt_g_h1), 
          mean_prey_wt_g_per_day = mean(prey_wt_g_day),
          SD_mean_prey_wt_g_per_day = sd(prey_wt_g_day),
          SE_mean_prey_wt_g_per_day = SE(prey_wt_g_day),
          mean_prey_wt_kg_3mo = mean((prey_wt_g_day/1000)*90),
          SE_prey_wt_kg_3mo = SE((prey_wt_g_day/1000)*90),
          mean_prey_wt_kg_6mo = mean((prey_wt_g_day/1000)*180),
          SE_prey_wt_kg_6mo = SE((prey_wt_g_day/1000)*180),
          mean_prey_wt_kg_9mo = mean((prey_wt_g_day/1000)*270),
          SE_prey_wt_kg_9mo = SE((prey_wt_g_day/1000)*270))
     
# make the wide dataset long (i.e., tidy) 
Sp_sum_tidy = Sp_sum %>%  
              gather(months_feeding, kg_consumed, c(9,11,13)) %>% 
  mutate(errBar = case_when(
    months_feeding == "mean_prey_wt_kg_3mo" ~ SE_prey_wt_kg_3mo,
    months_feeding == "mean_prey_wt_kg_6mo" ~ SE_prey_wt_kg_6mo,
    months_feeding == "mean_prey_wt_kg_9mo" ~ SE_prey_wt_kg_9mo
  ))


# Max's cool tidy code to look at feeding rates by rorqual species 
en_df_tidy %>% filter(TotalTagTime_h > 24) %>% group_by(species) %>% summarize(meanFeedRate = 24*mean(feeding_rate))
en_df_tidy %>% filter(TotalTagTime_h > 2) %>% ggplot(aes(x = 24*feeding_rate, color = species)) + geom_density()

ggplot(Sp_sum_tidy, aes(x = months_feeding, y = kg_consumed, fill = Species)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = kg_consumed - errBar, ymax = kg_consumed + errBar),
                stat = "identity") 

geom_errorbar(aes(ymin=kg_consumed-SE_prey_wt_kg_9mo, ymax=kg_consumed+SE_prey_wt_kg_9mo), 
              stat = "identity", position="dodge", color = "black")

Sp_sum_tidy$MonthsFeeding = ifelse(months_feeding$mean_prey_wt_kg_3mo & months_feeding$SE_prey_wt_kg_3mo, "3mo", 
                                   ifelse(months_feeding$mean_prey_wt_kg_6mo & months_feeding$SE_prey_wt_kg_6mo), "6mo", "9mo")

# looking at weighted means for NULL an BOUT fin and blue whale
fin_NULL <- weighted.mean(c(2740, 6000, 12900, 27840, 60000, 129240, 278520, 600000), c(1.9, 7.5, 12.1, 17.8, 22.7, 22.7, 12.8, 2.5))
fin_BOUT <- weighted.mean(c(6000, 12900, 27840, 60000, 129240, 278520, 600000), c(0.5, 3.7, 8.3, 13.9, 24.7, 30.9, 18))

blue_NULL <- weighted.mean(c(6160, 13400, 28810, 62176, 134000, 288636, 622028, 1340000), c(1.9, 7.4, 12.2, 17.3, 23, 22.8, 12.9, 2.5))
blue_BOUT <- weighted.mean(c(13400, 28810, 62176, 134000, 288636, 622028, 1340000), c(1, 3.3, 8.2, 17, 28.8, 29.9, 11.8))
                                                                                    
  
##########################
# Plot of energy in by time
##########################

plot_en_per_h_w_avg <- ggplot() +
  geom_point(data=en_df_tidy, aes(hour, log(corr_en_per_hour), color = Species, shape = Species), alpha = 0.2) +
  scale_shape_manual(name = "Species",                      
                     labels = c("Balaenoptera bonaerensis","Balaenoptera musculus","Balaenoptera physalus","Berardius bairdii",
                                "Globicephala macrorhynchus", "Globicephala melas","Grampus griseus", "Megaptera novaeangliae",
                                "Mesoplodon densirostris","Orcinus orca","Phocoena phocoena", "Physeter macrocephalus", "Ziphius cavirostris"),                     
                     values = c(0,1,2,3,4,5,6,7,8,9,10,12,13,14)) +
  #geom_path(data=en_df_tidy, aes(hour, log(corr_en_per_hour), color = Species), group=en_df_tidy$ID, alpha = 0.1) +
  geom_point(data = Sp_sum, aes(hour, log(mean_corr_en_per_hour), color = Species, shape = Species), size = 4) +
  geom_path(data = Sp_sum, aes(hour, log(mean_corr_en_per_hour), color = Species, group = Sp_sum$Species)) +
  geom_smooth(data=en_df_tidy, aes(hour, log(corr_en_per_hour)), color = "black", linetype="dashed") +
  scale_fill_manual(values = c("Berardius_bairdii","Globicephala_macrorhynchus", "Globicephala_melas","Grampus_griseus", "Mesoplodon_densirostris",
                               "Orcinus_orca","Phocoena_phocoena", "Physeter_macrocephalus", "Ziphius_cavirostris"),                      
                    labels = c("Berardius bairdii","Globicephala macrorhynchus", "Globicephala melas","Grampus griseus", "Mesoplodon densirostris",
                               "Orcinus orca","Phocoena phocoena", "Physeter macrocephalus", "Ziphius cavirostris")) +
  facet_grid(.~taxa) +
  theme_bw() +
  labs(x = "Time (hours)", y = "log[Energy gain corrected for body mass (kJ/kg)]") 
plot_en_per_h_w_avg


####################################################
# Plot of prey wt consumed by season
####################################################

Sp_sum_tidy <- group_by(Sp_sum_tidy, months_feeding, SE) 

View(filter(Sp_sum_tidy, Species =="Balaenoptera musculus"))

prey_wt_consumed_season <- ggplot(filter(Sp_sum_tidy,  Species %in% c("Balaenoptera musculus", "Balaenoptera physalus", "Megaptera novaeangliae")),
                            aes(x = months_feeding, y=kg_consumed, color = Species, fill = Species)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin=kg_consumed-SE_prey_wt_kg_9mo, ymax=kg_consumed+SE_prey_wt_kg_9mo), 
                stat = "identity", position="dodge", color = "black")

# aes(x = Sp_sum_tidy$SE),

prey_wt_consumed_season


################################################
# Scaling plot of energy in per day by body size
################################################

S1 <- ggplot(data=en_df_tidy, aes(x = log10(Body_mass_kg), y=log10(en_day), color = Species)) +
  geom_point(alpha = 0.5) +  
  geom_smooth(aes(group = taxa), method = lm) +
  geom_abline(intercept = 0, slope = 1, linetype ="dashed") + 

S1  
    
  theme_bw() + guides(size=FALSE, color=FALSE) + 
  ylim(1,7.25) + xlim(1,6.25) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold")) +
  labs(x = "log[Mass (kg)]", y = "log[Prey Energy (kJ)]")
fig_3a + scale_color_manual(values = cols)



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



