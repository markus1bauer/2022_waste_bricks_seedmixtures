# Brick-based substrates and designed seedmixtures
# Prepare environment data of experiment 1 and 2 ####
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

### Start ###
rm(list = ls())
setwd(here("data/raw"))
#library(installr);updateR(browse_news=F, install_R=T, copy_packages = T,copy_Rprofile.site = T,keep_old_packages = T, update_packages = T)

### Load data ###
environment12 <- read_csv("data_raw_experiment_1_2_environment.csv", col_names = T, na = "na", col_types = 
                            cols(
                              .default = "?",
                              plot = "f",
                              date = "D",
                              block = "f",
                              position = "f",
                              substrate = "f",
                              brickType = col_factor(levels = c("Clean", "Demolition")),
                              seedmix = col_factor(levels = c("Standard", "Robust", "Intermediate", "Vigorous")),
                              brickRatio = col_factor(levels = c("5", "30")),
                              acid = col_factor(levels = c("Control", "Acid"))
                            ))

environment3 <- read_csv("data_raw_experiment_3_environment.csv", col_names = T, na = "na", col_types =
                            cols(
                              .default = "?",
                              plot = "f",
                              date = "D",
                              substrate = "f",
                              brickRatio = col_factor(levels = c("5", "30")),
                              texture = col_factor(levels=c("Loam","Medium", "Sand")),
                              compaction = col_factor(levels=c("Control", "Compaction")),
                              coal = col_factor(levels=c("Control", "Coal")),
                              biomass = "d"
                            ))
  


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Create variables ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


environment12 <- environment12 %>% 
  mutate(conf.low = c(1:160),
         conf.high = c(1:160),
         biomass = grassMass + forbMass,
         grassRatio = grassMass / biomass,
         estRate = specPres / (specPres + specAbs),
         f.watering = factor(watering),
         f.watering = dplyr::recode(f.watering, "0.5" = "Dry", "1" = "Medium_dry", "2" = "Medium_moist", "3" = "Moist")
         ) %>%
  select(-date, -seedDensity, -substrate, -extraspecNo)
environment1 <- environment12 %>%
  filter(brickType == "Clean")
environment2 <- environment12 %>%
  filter(acid == "Acid" & seedmix == "Robust" | seedmix == "Vigorous" & acid == "Acid") %>%
  mutate(seedmix = factor(seedmix))
environment3 <- environment3 %>%
  mutate(conf.low = c(1:72),
         conf.high = c(1:72),
         estRate = specPres / (specPres + specAbs)
         ) %>%
  select(-date, -substrate)
  


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# C Export ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


write_csv(environment1, here("data/processed/data_processed_experiment_1_environment.csv"))
write_csv(environment2, here("data/processed/data_processed_experiment_2_environment.csv"))
write_csv(environment3, here("data/processed/data_processed_experiment_3_environment.csv"))
