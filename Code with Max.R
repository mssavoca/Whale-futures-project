##################
# Coding with Max
##################

# load packages
library(ggplot2)
library(dplyr)
library(readxl)
library(ggsci)
library(tidyverse)

# # cool way to use ifesle
# d_full_NULL <- read_csv("Cetacea model output NULL_EXTANT.csv") %>% 
#   mutate(Species = ifelse(Species == "bonarensis", "bonaerensis", Species))


# formula for standard error
SE <- function(x){sd(x)/sqrt(sum(!is.na(x)))}

# load data
d_full_NULL <- read_csv("Cetacea model output NULL_EXTANT.csv") %>% 
  mutate(Species = recode(Species, bonarensis = "bonaerensis"))
d_full_BOUT <- read_csv("Cetacea model output BOUT_EXTANT.csv") %>% 
  mutate(Species = recode(Species, bonarensis = "bonaerensis"))
  
RorqualData <- read_csv("lunge_rates_from_Paolo.csv") %>% 
  mutate(`deployment-time_h` = `deployment-time_secs`/60/60)


# # sweet tidy code from Max
d_sum_NULL <- d_full_NULL %>%
  group_by(Genus, Species) %>%
  summarize(wgtMeanNULL = weighted.mean(`Prey W (g)`, Percent),
            medNULL = median(`Prey W (g)`, Percent))
d_sum_BOUT = d_full_BOUT %>% 
  group_by(Genus, Species) %>% 
  summarize(wgtMeanBOUT = weighted.mean(`Prey W (g)`, Percent), 
            medBOUT = median(`Prey W (g)`, Percent))

OdontoceteData <- read_csv("foragestats_combined_ko2.csv") %>% 
  separate(Species, into = c("Genus", "Species"), sep = "_") %>% 
  left_join(d_sum_NULL, by = c("Genus", "Species"))
##FIXME, I think this works though
OdontoceteData<- OdontoceteData %>% 
  left_join(d_sum_BOUT, by = c("Genus", "Species"))

# REMEMBER: OdontoceteData is all of the data we need. 
# Add rorqual data to odontocete data
cetacean_data <- left_join(OdontoceteData, RorqualData, by = "ID") %>%
  # feeding events are lunges, buzzes for rorquals, odontocetes
  mutate(TotalFeedingEvents = coalesce(total_lunges, total_buzz_count),
         TotalTagTime_h = coalesce(`deployment-time_h`, total_duration_h)) %>% 
  # drop unknown species
  drop_na(Species) %>% 
  mutate(taxa = recode(taxa, M = "Mysticete", O = "Odontocete"),
         # FEEDING RATE
         feeding_rate = TotalFeedingEvents / TotalTagTime_h)

# Gather scenarios
scenario_data <- cetacean_data %>% 
  filter(TotalTagTime_h > 23) %>% 
  select(ID, Species, feeding_rate, wgtMeanNULL, wgtMeanBOUT, medNULL, medBOUT) %>%
  gather(scenario, prey_wgt_g, wgtMeanNULL:medBOUT) %>% 
  mutate(hourly_prey_in_g = prey_wgt_g * feeding_rate,
         scenario_type = ifelse(scenario %in% c("wgtMeanNULL", "medNULL"), "NULL", "BOUT"),
         calc_type = ifelse(scenario %in% c("wgtMeanNULL", "wgtMeanBOUT"), "mean", "med")) %>%
  group_by(Species, scenario, scenario_type, calc_type) %>%
  summarize(mean_hourly_prey_in_g = mean(hourly_prey_in_g))
resolution <- 100
# Calculate feeding over time (for plotting)
max_months <- 6
max_hours <- 24
plot_data <- tibble(t = seq(0, 24 * 30 * max_months, length.out = resolution),
                    dummy = 1) %>%
  full_join(mutate(scenario_data, dummy = 1)) %>%
  select(-dummy) %>%
  filter(Species %in% c("musculus", "physalus", "novaeangliae")) %>%
  mutate(prey_consumed = mean_hourly_prey_in_g * t / 1e6)
# Plot scenarios
ggplot(plot_data,
       aes(x = t / 24 / 30, 
           y = prey_consumed, 
           linetype = calc_type, 
           color = scenario_type)) +
  geom_line() +
  theme_bw() +
  facet_grid(~Species) +
  labs(x = "Months", y = "Prey consumed (metric tons)") +
  geom_text(aes(y = prey_consumed - 3, 
                label = format(prey_consumed, digits = 0)),
            data = filter(plot_data, t == max(t)))







en_df <- merge(RorqualData, OdontoceteData, by = "ID", all.x = TRUE, all.y = TRUE)

# coalescing to merge two columns into one
en_df$TotalFeedingEvents = coalesce(en_df$total_lunges, en_df$total_buzz_count)
head(en_df$TotalFeedingEvents)

en_df$TotalTagTime_h = coalesce(en_df$`deployment-time_h`, en_df$total_duration_h)


en_df <- filter(en_df, !Species %in% NA)  #removes rows with NA in Species column
en_df$Species <- gsub("_", " ", en_df$Species) #replaces underscore with space in the Species column

en_df$taxa <- gsub("M", "Mysticete", en_df$taxa)
en_df$taxa <- gsub("O", "Odontocete", en_df$taxa)

#creating new columns
en_df$feeding_rate = en_df$TotalFeedingEvents/en_df$TotalTagTime_h

en_df$en_h1 = en_df$Prey_E_kJ*en_df$feeding_rate      # NEED TO CHANGE TO MEDIAN AND/OR WEIGHTED MEAN INSTEAD OF "Prey_E_kJ" to propogate new values through
en_df$en_h2 = en_df$en_h1*2
en_df$en_h3 = en_df$en_h1*3
en_df$en_h4 = en_df$en_h1*4
en_df$en_h5 = en_df$en_h1*5
en_df$en_h6 = en_df$en_h1*6




# energy acquired in a day
en_df$en_day = en_df$en_h1*24
en_df$corr_en_day = en_df$en_day/en_df$Body_mass_kg

# mass of prey consumed in an hour / day, using the point estimates Danuta gave me, the median and weighted means of the prey
en_df$prey_wt_g_h1 = en_df$Prey_wt_g*en_df$feeding_rate  # values from Danuta
en_df$prey_wt_g_day = en_df$prey_wt_g_h1*24              # values from Danuta
en_df$med_prey_wt_g_NULL_h1 = en_df$medNULL*en_df$feeding_rate
en_df$wgtMean_prey_wt_g_NULL_h1 = en_df$wgtMeanNULL*en_df$feeding_rate
en_df$med_prey_wt_g_BOUT_h1 = en_df$medBOUT*en_df$feeding_rate
en_df$wgtMean_prey_wt_g_BOUT_h1 = en_df$wgtMeanBOUT*en_df$feeding_rate

# make the wide dataset long (i.e., tidy) 
en_df_tidy = gather(en_df, hour, en_per_hour, 58:63)

#turn certain values into numbers
en_df_tidy$hour = ifelse(en_df_tidy$hour == "en_h1", 1,
                         ifelse(en_df_tidy$hour == "en_h2", 2,
                                ifelse(en_df_tidy$hour == "en_h3", 3,
                                       ifelse(en_df_tidy$hour == "en_h4", 4,
                                              ifelse(en_df_tidy$hour == "en_h5", 5, 6)))))

en_df_tidy$corr_en_per_hour = en_df_tidy$en_per_hour/en_df_tidy$Body_mass_kg    #Energy in per hour corrected for body mass

en_df_tidy <- en_df_tidy[-which(is.na(en_df_tidy$en_per_hour)), ]    # remove blank rows

# data summary filtering by lunge quality, removing any NA Species or NAs in feeding rate, filtering to tak
# HOT SLOPPY MESS. FIND BETTER WAY TO DO
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
                   SE_prey_wt_kg_9mo = SE((prey_wt_g_day/1000)*270),
                   med_prey_wt_g_NULL_per_hour  = mean(med_prey_wt_g_NULL_h1),
                   wgtMean_prey_wt_g_NULL_per_hour  = mean(wgtMean_prey_wt_g_NULL_h1),
                   med_prey_wt_g_BOUT_per_hour  = mean(med_prey_wt_g_BOUT_h1),
                   wgtMean_prey_wt_g_BOUT_per_hour  = mean(wgtMean_prey_wt_g_BOUT_h1),
                   Prey_wt_kg_3mo_NULLmed = mean((med_prey_wt_g_BOUT_h1/1000)*24*90),
                   Prey_wt_kg_6mo_NULLmed = mean((med_prey_wt_g_BOUT_h1/1000)*24*180),
                   Prey_wt_kg_9mo_NULLmed = mean((med_prey_wt_g_BOUT_h1/1000)*24*270),
                   Prey_wt_kg_3mo_NULLwgtMean = mean((wgtMean_prey_wt_g_NULL_h1/1000)*24*90),
                   Prey_wt_kg_6mo_NULLwgtMean = mean((wgtMean_prey_wt_g_NULL_h1/1000)*24*180),
                   Prey_wt_kg_9mo_NULLwgtMean = mean((wgtMean_prey_wt_g_NULL_h1/1000)*24*270))


# make the wide dataset long (i.e., tidy); CHANGE AS N ECESSARY FOR DIFFERENT PLOTS
Sp_sum_tidy = Sp_sum %>%  
  gather(months_feeding, kg_consumed, c(9,11,13)) %>% 
  mutate(errBar = case_when(
    months_feeding == "mean_prey_wt_kg_3mo" ~ SE_prey_wt_kg_3mo,
    months_feeding == "mean_prey_wt_kg_6mo" ~ SE_prey_wt_kg_6mo,
    months_feeding == "mean_prey_wt_kg_9mo" ~ SE_prey_wt_kg_9mo
  ))



####################################################
# Plot of prey wt consumed by season
####################################################

prey_wt_consumed_season <- ggplot(filter(Sp_sum_tidy,  Species %in% c("musculus", "physalus", "novaeangliae")),
                                  aes(x = months_feeding, y=kg_consumed, fill = Species)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = kg_consumed - errBar, ymax = kg_consumed + errBar),
                stat = "identity", position="dodge", color = "black")

prey_wt_consumed_season

