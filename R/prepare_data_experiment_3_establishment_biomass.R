# Brick-based substrates and designed seedmixtures
# Script to prepare data of experiment 1 and 2 ####
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
seedmixtures <- read_table("data_raw_experiment_3_seed_mixtures.txt", col_names = T, na = "na", col_types =
                         cols(
                           name = "f",
                           presence = "d",
                           plot = "f"
                         )) %>%
  filter(name != "Ranunculus_bulbosus" & name != "Ranunculus_acris")
environment <- read_table("data_raw_experiment_3_environment.txt", col_names = T, na = "na", col_types =
                             cols(
                               plot = "f",
                               date = "D",
                               substrate = "f",
                               brickRatio = col_factor(levels = c("5", "30")),
                               texture = col_factor(levels=c("Loam","Medium", "Sand")),
                               compaction = col_factor(levels=c("Control", "Compaction")),
                               coal = col_factor(levels=c("Control", "Coal")),
                               biomass = "d",
                               estRate = "d"
                             )) %>%
### Create variables ###
  mutate(conf.low = c(1:72),
         conf.high = c(1:72)
         ) %>%
  select(-date, -substrate)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## 1 Establishment rate per plot ########################################################################

data <- seedmixtures %>% 
  group_by(plot) %>% 
  summarise(specRich = sum(presence))
environment <- left_join(environment, data) %>%
  mutate(estRate = specRich / 20)

## 2 Establishment rate of species #############################################################################################

establishment <- seedmixtures %>% 
  count(name, presence) %>%
  mutate(presence = factor(presence),
         presence = fct_recode(presence, pres = "1", abs = "0")) %>%
  tidyr::spread(presence, n) %>%
  mutate(pres = replace(pres, is.na(pres), 0),
         abs = replace(abs, is.na(abs), 0),
         estRatio =  pres / (abs + pres),
         rounded = round(estRatio,2)) %>%
  arrange(estRatio)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# C Export ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#write.table(environment, here("data/processed/data_processed_experiment_3_environment.txt"), sep = "\t", row.names = F, quote = F)
#write.table(establishment, here("data/processed/data_processed_experiment_3_establishment.txt"), sep = "\t", row.names = F, quote = F)
