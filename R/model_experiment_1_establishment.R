# Model for Experiment 1 for establishment ####



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Packages ###
library(tidyverse)
library(ggbeeswarm)

### Start ###
rm(list = ls())
setwd("Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_for_restoration/data/processed")

### Load data ###
establishment <- read_table2("data_processed_experiment_1_establishment.txt", col_names = T, na="na", col_types =
                       cols(
                         .default = col_double(),
                         name = col_factor()
                       )
)
environment <- read_table2("data_processed_experiment_1_environment.txt", col_names = T, na = "na", col_types = 
                             cols(
                               .default = col_double(),
                               plot = col_factor(),
                               block = col_factor(),
                               position = col_factor(),
                               brickType = col_factor(levels = c("Clean","Demolition")),
                               seedmix = col_factor(levels = c("Standard","Robust","Intermediate","Vigorous")),
                               brickRatio = col_factor(levels = c("5","30")),
                               acid = col_factor(levels = c("Control","Acid")),
                               f.watering = col_factor(levels = c("Dry", "Medium_dry", "Medium_moist","Moist"))
                             )        
)

environment$f.watering <- dplyr::recode(environment$f.watering,
                                        "Medium_dry" = "Medium dry", "Medium_moist" = "Medium moist")



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Establishment per plot ###
environment %>% 
  group_by(seedmix) %>% 
  summarise(mean.estRate = mean(estRate), sd.estRate = sd(estRate))
###Calculate SE
0.0723 / sqrt(32)
0.0983 / sqrt(32)
0.0890 / sqrt(32)
0.102 / sqrt(32)

### Establishment per species of designed seed mixtures ###
establishment %>% 
  group_by(poolD) %>% 
  summarise(mean.estRate = mean(estRate1D, na.rm = T), sd.estRate = sd(estRate1D, na.rm = T))
###Calculate SE
0.309 / sqrt(39)

###Eestablishment per species of standarad seed mixture ###
establishment %>% 
  group_by(poolS) %>% 
  summarise(mean.estRate = mean(estRate1S, na.rm = T), sd.estRate = sd(estRate1S, na.rm = T))
###Calculate SE
0.361 / sqrt(16)
