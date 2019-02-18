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
library(plyr)

# read in and clean data
Whale_POPs <- read_excel("Cetacean pollutant loads.xlsx")

Whale_POPs$'Average Pollutant load (ng/g lipid wt)' <- as.numeric(Whale_POPs$'Average Pollutant load (ng/g lipid wt)')
Whale_POPs$`Prey type` = as.factor(Whale_POPs$`Prey type`)
Whale_POPs$`Pollutant type` = as.factor(Whale_POPs$`Pollutant type`)
Whale_POPs = Whale_POPs %>% separate(Species,into = c("Genus","just Species"), sep=" ", remove = FALSE)


# Add column for ocean basin
Whale_POPs$Basin <- with(Whale_POPs, case_when(Region %in% c("Central North Pacific", "Northeast Pacific", "Northwest Pacific") ~ "North Pacific", 
                                               Region %in% c("Southeast Pacific", "Southwest Pacific") ~ "South Pacific", 
                                               Region %in% c("Northwest Atlantic", "Northeast Atlantic") ~ "North Atlantic", 
                                               Region == "Southwest Atlantic" ~ "South Atlantic", 
                                               Region == "Arctic Ocean" ~ "Arctic Ocean", Region == "Southern Ocean" ~ "Southern Ocean"))


#Graph of pollutant by region
p1 <- ggplot(data = filter(Whale_POPs, `Pollutant type`!= "Hg"), aes(Basin, log10(`Average Pollutant load (ng/g lipid wt)`))) +
  geom_boxplot(outlier.size = 0) +
  geom_jitter(aes(size = `Sample size`, color = Group), alpha = 0.3)+
  geom_hline(aes(yintercept=4), colour="#990000", linetype="dashed") +
  facet_grid(~`Pollutant type`) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p1

#Graph pollutant by prey
Whale_POPs$`Prey type` <- revalue(Whale_POPs$`Prey type`, c("Zooplankton"="Crustacean", "Krill"="Crustacean"))
Whale_POPs$`Prey type` <- fct_relevel(Whale_POPs$`Prey type`, "Top predator", "Fish", "Cephalopods", "Forage fish", "Crustacean")
# vline.dat <- data.frame(`Pollutant type`=levels(Whale_POPs$`Pollutant type`), vl=c(4,3.17,4))

p2 <- ggplot(data = filter(Whale_POPs, `Prey type` != "NA" & `Pollutant type` != "Hg"), aes(`Prey type`, log10(`Average Pollutant load (ng/g lipid wt)`))) +
  geom_boxplot(outlier.size = 0) + 
  geom_jitter(aes(size = `Sample size`, color = Group), alpha = 0.3) +
  geom_hline(aes(yintercept=4), colour="#990000", linetype="dashed") +
  facet_grid(~`Pollutant type`) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p2


# Pollutant by trophic level (values from Pauly et al. 1998)
p2_TL <- ggplot(data = filter(Whale_POPs, `Prey type` != "NA" & `Pollutant type` != "Hg"), aes(`Trophic level`, log10(`Average Pollutant load (ng/g lipid wt)`))) +
  geom_point(aes(size = `Sample size`, color = Group), alpha = 0.3) +
  geom_smooth(aes(weight = `Sample size`), method = "lm", color = "black") +
  facet_grid(~`Pollutant type`) +
  geom_hline(aes(yintercept=4), colour="#990000", linetype="dashed") +
  xlab("Trophic level") + ylab("log[Average pollutant load (ng/g lipid wt)")+
  theme_bw()
p2_TL

#pollutant by decade
p3 <- ggplot(data = Whale_POPs, aes(`Collection decade`, log10(`Average Pollutant load (ng/g lipid wt)`))) +
  geom_boxplot(outlier.size = 0) +
  geom_jitter(aes(size = `Sample size`, color = Group), alpha = 0.4) +
  geom_hline(aes(yintercept=4), colour="#990000", linetype="dashed") +
  facet_grid(~`Pollutant type`) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p3
