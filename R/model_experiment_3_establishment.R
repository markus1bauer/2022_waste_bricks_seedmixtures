# Brick-based substrates and designed seedmixtures
# Establishment of species of experiment 3 ####
# Markus Bauer
# 2022-01-24



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ##############################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### Packages ###
library(here)
library(tidyverse)

### Start ###
rm(list = ls())
setwd(here("data", "processed"))

### Load data ###
establishment <- read_csv("data_processed_experiment_1_2_3_traits.csv",
                          col_names = TRUE, na = "na", col_types =
                               cols(
                                 .default = "d",
                                 name = "f"
                               )) %>%
  select(name, estRate3)
environment <- read_csv("data_processed_experiment_3_environment.csv",
                        col_names = TRUE, na = "na", col_types =
                       cols(
                         .default = "?"
                         ))


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### Establishment per plot ###
summarise(environment, mean.estRate = mean(estRate), sd.estRate = sd(estRate))
###Calculate SE
0.114 / sqrt(72)

### Establishment per species ###
summarise(establishment,
          mean.estRate = mean(estRate3, na.rm = TRUE),
          sd.estRate = sd(estRate3, na.rm = TRUE))
0.292 / sqrt(39)
