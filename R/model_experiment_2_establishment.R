# Model for Experiment 2 for establishment ####



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Packages ###
library(tidyverse)
library(ggbeeswarm)

### Start ###
rm(list = ls())
setwd("Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_for_restoration/data/processed")

### Load data ###
establishment <- read_table2("data_processed_experiment_2_establishment.txt", col_names = T, na="na", col_types =
                               cols(
                                 .default = col_double(),
                                 name = col_factor()
                               )
)
environment <- read_table2("data_processed_experiment_2_environment.txt", col_names = T, na = "na", col_types = 
                             cols(
                               .default = col_double(),
                               plot = col_factor(),
                               block = col_factor(),
                               position = col_factor(),
                               f.watering = col_factor(levels = c("Dry", "Medium_dry", "Medium_moist","Moist")),
                               seedmix = col_factor(levels = c("Standard","Robust","Intermediate","Vigorous")),
                               brickType = col_factor(levels = c("Demolition","Clean")),
                               brickRatio = col_factor(levels = c("30","5")),
                               acid = col_factor(levels = c("Acid","Control"))
                             )        
)

environment$f.watering <- dplyr::recode(environment$f.watering,
                                        "Medium_dry" = "Medium dry", "Medium_moist" = "Medium moist")


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Establishment per plot ###
environment %>% 
  group_by(seedmix) %>% 
  summarise(mean.estRate = mean(estRate), sd.estRate = sd(estRate))
###Calculate SE
0.0906 / sqrt(32)
0.112 / sqrt(32)

### Establishment per species of designed seed mixtures ###
establishment %>% 
  group_by(poolD) %>% 
  summarise(mean.estRate = mean(estRate2D, na.rm = T), sd.estRate = sd(estRate2D, na.rm = T))
###Calculate SE
0.305 / sqrt(39)
