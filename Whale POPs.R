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


Whale_POPs <- read_excel("Cetacean pollutant loads.xlsx")

str(Whale_POPs)

Whale_POPs$'Average Pollutant load (ng/g lipid wt)' <- as.numeric(Whale_POPs$'Average Pollutant load (ng/g lipid wt)')
Whale_POPs$`Prey type` = as.factor(Whale_POPs$`Prey type`)
Whale_POPs = Whale_POPs %>% separate(Species,into = c("Genus","just Species"), sep=" ", remove = FALSE)

#pollutant by region
p1 <- ggplot(data = Whale_POPs, aes(Region, log10(`Average Pollutant load (ng/g lipid wt)`))) +
  geom_boxplot() +
  geom_jitter(size = `Sample size`)+
  geom_hline(aes(yintercept=4), colour="#990000", linetype="dashed") +
  facet_grid(~`Pollutant type`) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p1

#pollutant by prey
Whale_POPs$`Prey type` <- revalue(Whale_POPs$`Prey type`, c("Zooplankton"="Crustacean", "Krill"="Crustacean"))

Whale_POPs$`Prey type` <- fct_relevel(Whale_POPs$`Prey type`, "Top predator", "Fish", "Cephalopods", "Forage fish", "Crustacean")

p2 <- ggplot(data = filter(Whale_POPs, `Prey type` != "NA"), aes(`Prey type`, log10(`Average Pollutant load (ng/g lipid wt)`))) +
  geom_boxplot() + geom_jitter(data = filter(Whale_POPs, `Prey type` != "NA"), aes(size = `Sample size`, color = Species), alpha = 0.4) +
  geom_hline(aes(yintercept=4), colour="#990000", linetype="dashed") +
  facet_grid(~`Pollutant type`) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p2


Whale_POPs %>% 
    mutate(`Prey type` = ifelse(`Prey type` %in% c("Zooplankton", "Krill"), as.character(`Prey type`), "Crustacean")) %>% 

#pollutant by decade
p3 <- ggplot(data = Whale_POPs, aes(`Collection decade`, log(`Average Pollutant load (ng/g lipid wt)`))) +
  geom_boxplot() +
  geom_jitter() +
  facet_grid(Region~`Pollutant type`) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p3
