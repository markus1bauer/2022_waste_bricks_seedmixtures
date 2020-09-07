# Script to prepare data of experiment 1 and 2 ###



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### Packages ###
library(tidyverse)

### Start ###
rm(list = ls())
setwd("Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_for_restoration/data/raw")
#library(installr);updateR(browse_news=F, install_R=T, copy_packages = T,copy_Rprofile.site = T,keep_old_packages = T, update_packages = T)

### Load data ###
seedmixtures <- read_table2("data_raw_experiment_3_seed_mixtures.txt", col_names = T, na="na", col_types =
                         cols(
                           name = col_factor(),
                           presence = col_double(),
                           plot = col_factor()
                         )
)
environment <- read_table2("data_raw_experiment_3_environment.txt", col_names = T, na="na", col_types =
                             cols(
                               plot = col_factor(),
                               date = col_date(),
                               substrate = col_factor(),
                               brickRatio = col_factor(levels = c("5","30")),
                               texture = col_factor(levels=c("Loam","Medium","Sand")),
                               compaction = col_factor(levels=c("Control","Compaction")),
                               coal = col_factor(levels=c("Control","Coal")),
                               biomass = col_double(),
                               estRate = col_double()
                             )
)

### Create variables ###
environment$conf.low <- c(1:72)
environment$conf.high <- c(1:72)
seedmixtures <- filter(seedmixtures, name != "Ranunculus_bulbosus" & name != "Ranunculus_acris")
environment <- select(environment, -date, -substrate)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## 1 Establishment rate per plot ########################################################################

data <- seedmixtures %>% 
  group_by(plot) %>% 
  summarise(specRich = sum(presence))
environment <- left_join(environment, data)
(environment <- mutate(environment, estRate = specRich / 20))

## 2 Establishment rate of species #############################################################################################

establishment <- seedmixtures %>% 
  count(name, presence)
establishment$presence <- factor(establishment$presence)
establishment$presence <- fct_recode(establishment$presence, pres = "1", abs = "0")
(establishment <- establishment %>% 
  tidyr::spread(presence, n) %>%
  mutate(pres = replace(pres, is.na(pres), 0)) %>%
  mutate(abs = replace(abs, is.na(abs), 0)) %>%
  mutate(estRatio =  pres / (abs + pres)) %>%
  mutate(rounded = round(estRatio,2)) %>%
  arrange(estRatio))



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# C Export ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#write.table(environment, "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_for_restoration/data/processed/data_processed_experiment_3_environment.txt", sep = "\t", row.names = F, quote = F)
#write.table(establishment, "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_for_restoration/data/processed/data_processed_experiment_3_establishment.txt", sep="\t", row.names = F, quote = F)
