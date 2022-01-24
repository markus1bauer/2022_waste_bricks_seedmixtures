# Brick-based substrates and designed seedmixtures
# Establishment of species of experiment 3 ####
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

### Start ###
rm(list = ls())
setwd(here("data/processed"))

### Load data ###
establishment <- read_table("data_processed_experiment_3_establishment.txt", col_names = T, na = "na", col_types =
                               cols(
                                 .default = "d",
                                 name = "f"
                               ))
environment <- read_table("data_processed_experiment_3_environment.txt", col_names = T, na = "na", col_types = 
                       cols(
                         .default = "d",
                         plot = "f",
                         date = col_date(),
                         brickRatio = "f",
                         texture = "f",
                         compaction = "f",
                         coal = "f"
                         ))


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
