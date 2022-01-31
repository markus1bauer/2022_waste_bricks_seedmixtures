# Brick-based substrates and designed seedmixtures
# Establishment of species of experiment 1 ####
# Markus Bauer
# 2022-01-24
# Citation: 
## Bauer M, Krause M, Heizinger V, Kollmann J (submitted) 
## Using waste bricks for recultivation: no negative effects of brick-augmented substrates with varying acid pre-treatment, soil type and moisture on contrasting seed mixtures
## Unpublished data.




#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Packages ###
library(here)
library(tidyverse)
library(ggbeeswarm)

### Start ###
rm(list = ls())
setwd(here("data/processed"))

### Load data ###
establishment <- read_table2("data_processed_experiment_1_establishment.txt", col_names = T, na = "na", col_types =
                       cols(
                         .default = "d",
                         name = "f"
                       ))
environment <- read_table2("data_processed_experiment_1_environment.txt", col_names = T, na = "na", col_types = 
                             cols(
                               .default = "d",
                               plot = "f",
                               block = "f",
                               position = "f",
                               brickType = col_factor(levels = c("Clean","Demolition")),
                               seedmix = col_factor(levels = c("Standard","Robust","Intermediate","Vigorous")),
                               brickRatio = col_factor(levels = c("5","30")),
                               acid = col_factor(levels = c("Control","Acid")),
                               f.watering = col_factor(levels = c("Dry", "Medium_dry", "Medium_moist","Moist"))
                             )) %>%
  mutate(f.watering = dplyr::recode(f.watering, "Medium_dry" = "Medium dry", "Medium_moist" = "Medium moist"))



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

### Establishment per species of standarad seed mixture ###
establishment %>% 
  group_by(poolS) %>% 
  summarise(mean.estRate = mean(estRate1S, na.rm = T), sd.estRate = sd(estRate1S, na.rm = T))
###Calculate SE
0.361 / sqrt(16)
