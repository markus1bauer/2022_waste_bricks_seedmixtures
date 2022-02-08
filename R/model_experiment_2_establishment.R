# Brick-based substrates and designed seedmixtures
# Establishment of species of experiment 2 ####
# Markus Bauer
# 2022-01-24



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation #####################################################################################
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
  select(name, estRate2)
environment <- read_csv("data_processed_experiment_2_environment.csv", col_names = T, na = "na", col_types = 
                             cols(
                               .default = "?"
                               ))


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ######################################################################################
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
  summarise(mean.estRate = mean(estRate2, na.rm = T), sd.estRate = sd(estRate2, na.rm = T))
###Calculate SE
0.305 / sqrt(39)
