# Model for Experiment 3 for establishment ####



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Packages ###
library(tidyverse)

### Start ###
rm(list = ls())
setwd("Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_for_restoration/data/processed")

### Load data ###
establishment <- read_table2("data_processed_experiment_3_establishment.txt", col_names = T, na="na", col_types =
                               cols(
                                 .default = col_double(),
                                 name = col_factor()
                               )
)
environment <- read_table2("data_processed_experiment_3_environment.txt", col_names = T, na = "na", col_types = 
                       cols(
                         .default = col_double(),
                         plot = col_factor(),
                         date = col_date(),
                         brickRatio = col_factor(),
                         texture = col_factor(),
                         compaction = col_factor(),
                         coal = col_factor()
                         )        
)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### Establishment per plot ###
summarise(environment, mean.estRate = mean(estRate), sd.estRate = sd(estRate))
###Calculate SE
0.114 / sqrt(72)

### Establishment per species ###
summarise(establishment, mean.estRate = mean(estRatio, na.rm = T), sd.estRate = sd(estRatio, na.rm = T))
0.292 / sqrt(39)
