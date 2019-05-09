#############################################
# Preliminary figures for whale futures paper
#############################################

# load packages and functions ----
library(ggplot2)
library(dplyr)
library(readxl)
library(ggsci)
library(tidyverse)
library(mgcv)
library(lme4)
library(plyr)

abbr_binom = function(binom) {
  paste(str_sub(binom, 1, 1), 
        str_extract(binom, " .*"), 
        sep = ".")
}

# read in and clean data ----

Whale_POPs <- read_excel("Cetacean pollutant loads from LK.xlsx")

Whale_POPs$'Average Pollutant load (ng/g lipid wt)' <- as.numeric(Whale_POPs$'Average Pollutant load (ng/g lipid wt)')
Whale_POPs$`Prey type` <-  as.factor(Whale_POPs$`Prey type`)
Whale_POPs$`Pollutant type` <-  as.factor(Whale_POPs$`Pollutant type`)
Whale_POPs$`Sample size` <- as.numeric(Whale_POPs$`Sample size`)
Whale_POPs <-  Whale_POPs %>% separate(Species,into = c("Genus","Species"), sep=" ", remove = FALSE)


# Add column for ocean basin
Whale_POPs$Basin <- with(Whale_POPs, case_when(Region %in% c("Central North Pacific", "Northeast Pacific", "Northwest Pacific") ~ "North Pacific", 
                                               Region %in% c("Southeast Pacific", "Southwest Pacific") ~ "South Pacific", 
                                               Region %in% c("Northwest Atlantic", "Northeast Atlantic") ~ "North Atlantic", 
                                               Region == "Southwest Atlantic" ~ "South Atlantic", 
                                               Region == "Arctic Ocean" ~ "Arctic Ocean", 
                                               Region == "Southern Ocean" ~ "Southern Ocean"))


# Preliminary figures ----

#Graph of pollutant by region
p1 <- ggplot(data = filter(Whale_POPs, `Pollutant type`!= "Hg"), 
             aes(Basin, log10(`Average Pollutant load (ng/g lipid wt)`))) +
  geom_boxplot(outlier.size = 0) +
  geom_jitter(aes(size = `Sample size`, color = Group), alpha = 0.3) +
  geom_hline(aes(yintercept=4), colour="#990000", linetype="dashed") +
  facet_grid(~`Pollutant type`) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p1

<<<<<<< HEAD
#Graoh pollutant by prey
Whale_POPs %>% 
  mutate(`Prey type` = revalue(`Prey type`, c("Zooplankton"="Crustacean", "Krill"="Crustacean")),
  `Prey type` = fct_relevel(`Prey type`, "Top predator", "Fish", "Cephalopods", "Forage fish", "Crustacean"),
  harm_level = case_when(`Pollutant type` == "PCBs" ~ 4,
                         `Pollutant type` == "PBDEs" ~ 3,
                         `Pollutant type` == "DDTs" ~ 4))
  
=======
#Graph pollutant by prey
Whale_POPs$`Prey type` <- revalue(Whale_POPs$`Prey type`, c("Zooplankton"="Crustacean", "Krill"="Crustacean"))
Whale_POPs$`Prey type` <- fct_relevel(Whale_POPs$`Prey type`, "Top predator", "Fish", "Cephalopods", "Forage fish", "Crustacean")
>>>>>>> a068935aa1553cc1122617739395dde88cb1143b
# vline.dat <- data.frame(`Pollutant type`=levels(Whale_POPs$`Pollutant type`), vl=c(4,3.17,4))

a_harm <- tibble(type = c("DDTs", "PBDEs", "PCBs"), harm_val = c(4,3,4))

p2 <- ggplot(data = filter(Whale_POPs, `Prey type` != "NA" & Basin != "NA" & 
                             `Pollutant type` != "Hg"), 
             aes(`Prey type`, log10(`Average Pollutant load (ng/g lipid wt)`))) +
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(color = Group, size = `Sample size`, shape = Basin), alpha = 0.3) +
  geom_hline(data = a_harm, aes(yintercept=4), colour="#990000", linetype="dashed") +
  facet_grid(~`Pollutant type`) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p2


<<<<<<< HEAD
p2_OOEpres <- ggplot(data = filter(Whale_POPs, `Prey type` != "NA" & `Pollutant type` == "PBDEs"), 
                     aes(`Prey type`, log10(`Average Pollutant load (ng/g lipid wt)`))) +
  geom_boxplot(outlier.size = 0) + 
  geom_jitter(aes(size = `Sample size`, color = Group, shape = Basin), alpha = 0.75) +
  geom_hline(aes(yintercept=3), colour="#990000", linetype="dashed") +
  ggtitle("Meta-analysis of PBDEs in cetaceans around the world") +
  ylab("Pollutant load (log(ng/g lipid wt)") +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold"),
        plot.title = element_text(hjust = 0.5, size = 16), 
        strip.text.x = element_text(size = 14)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p2_OOEpres

=======
# Pollutant by trophic level (values from Pauly et al. 1998)
p2_TL <- ggplot(data = filter(Whale_POPs, `Prey type` != "NA" & `Pollutant type` != "Hg"), aes(`Trophic level`, log10(`Average Pollutant load (ng/g lipid wt)`))) +
  geom_point(aes(size = `Sample size`, color = Group), alpha = 0.3) +
  geom_smooth(aes(weight = `Sample size`), method = "lm", color = "black") +
  facet_grid(~`Pollutant type`) +
  geom_hline(aes(yintercept=4), colour="#990000", linetype="dashed") +
  xlab("Trophic level") + ylab("log[Average pollutant load (ng/g lipid wt)")+
  theme_bw()
p2_TL
>>>>>>> a068935aa1553cc1122617739395dde88cb1143b

#pollutant by decade
p3 <- ggplot(data = Whale_POPs, aes(`Collection decade`, log10(`Average Pollutant load (ng/g lipid wt)`))) +
  geom_boxplot(outlier.size = 0) +
  geom_jitter(aes(size = `Sample size`, color = Group), alpha = 0.4) +
  geom_hline(aes(yintercept=4), colour="#990000", linetype="dashed") +
  facet_grid(~`Pollutant type`) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p3
