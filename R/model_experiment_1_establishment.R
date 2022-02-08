# Brick-based substrates and designed seedmixtures
# Establishment of species of experiment 1 ####
# Markus Bauer
# 2022-01-24



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
establishment <- read_csv("data_processed_experiment_1_2_3_traits.csv", col_names = T, na = "na", col_types =
                       cols(
                         .default = "d",
                         name = "f"
                       )) %>%
  select(name, estRate1Design, estRate1Standard)
environment <- read_csv("data_processed_experiment_1_environment.csv", col_names = T, na = "na", col_types = 
                             cols(
                               .default = "?"))



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics #############################################################################
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
  summarise(mean.estRate = mean(estRate1Design, na.rm = T), sd.estRate = sd(estRate1Design, na.rm = T))
###Calculate SE
0.309 / sqrt(39)

### Establishment per species of standarad seed mixture ###
establishment %>% 
  summarise(mean.estRate = mean(estRate1Standard, na.rm = T), sd.estRate = sd(estRate1Standard, na.rm = T))
###Calculate SE
0.361 / sqrt(16)
