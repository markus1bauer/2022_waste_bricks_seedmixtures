# Brick-based substrates and designed seedmixtures
# Establishment of species of experiment 2 ####
# Markus Bauer
# 2022-01-24
# Citation: 
## Bauer M, Krause M, Heizinger V, Kollmann J (submitted) 
## Using waste bricks for recultivation: no negative effects of brick-augmented substrates with varying acid pre-treatment, soil type and moisture on contrasting seed mixtures
## Unpublished data.



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
establishment <- read_table("data_processed_experiment_2_establishment.txt", col_names = T, na="na", col_types =
                               cols(
                                 .default = "d",
                                 name = "f"
                               )
)
environment <- read_table("data_processed_experiment_2_environment.txt", col_names = T, na = "na", col_types = 
                             cols(
                               .default = "d",
                               plot = "f",
                               block = "f",
                               position = "f",
                               f.watering = col_factor(levels = c("Dry", "Medium_dry", "Medium_moist","Moist")),
                               seedmix = col_factor(levels = c("Standard","Robust","Intermediate","Vigorous")),
                               brickType = col_factor(levels = c("Demolition","Clean")),
                               brickRatio = col_factor(levels = c("30","5")),
                               acid = col_factor(levels = c("Acid","Control"))
                             )) %>%
  mutate(f.watering = dplyr::recode(f.watering, "Medium_dry" = "Medium dry", "Medium_moist" = "Medium moist"))


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
