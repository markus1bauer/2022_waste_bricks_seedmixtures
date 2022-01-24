# Brick-based substrates and designed seedmixtures
# Prepare data of experiment 1 and 2 for establishment ####
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
seedmixtures <- read_table("data_raw_experiment_1_2_seed_mixtures.txt", col_names = T, na = "na", col_types = 
                       cols(
                         .default = col_double(),
                         name = col_factor(),
                         plot = col_factor()
                         )        
)
environment <- read_table("data_raw_experiment_1_2_environment.txt", col_names = T, na = "na", col_types = 
                             cols(
                               .default = col_double(),
                               plot = col_factor(),
                               date = col_date(),
                               block = col_factor(),
                               position = col_factor(),
                               substrate = col_factor(),
                               brickType = col_factor(levels = c("Clean", "Demolition")),
                               seedmix = col_factor(levels = c("Standard", "Robust", "Intermediate", "Vigorous")),
                               brickRatio = col_factor(levels = c("5", "30")),
                               acid = col_factor(levels = c("Control", "Acid"))
                             )) %>%
  mutate(conf.low = c(1:160),
         conf.high = c(1:160),
         biomass = grassMass + forbMass,
         grassRatio = grassMass / biomass,
         f.watering = factor(watering),
         f.watering = dplyr::recode(f.watering, "0.5" = "Dry", "1" = "Medium_dry", "2" = "Medium_moist", "3" = "Moist")
         )
traits <- read_table("data_processed_experiment_1_2_3_traits.txt", col_names = T, na = "na", col_types =
                        cols(
                          .default = "d",
                          name = "f",
                          family = "f",
                          poolD = "f",
                          poolS = "f"
                        ))
### Create variables for environment data set ###

### Create environemnt data frames for experiment 1 and 2 ###
environment <- environment %>% 
  select(-date, -seedDensity, -substrate, -extraspecNo)
environment1 <- environment %>%
  filter(brickType == "Clean")
environment2 <- environment %>%
  filter(acid == "Acid" & seedmix == "Robust" | seedmix == "Vigorous" & acid == "Acid") %>%
  mutate(seedmix = factor(seedmix))
seedmixtures <- seedmixtures %>%
  filter(name != "Ranunculus_bulbosus" & name != "Ranunculus_acris")
seedmixtures1 <- left_join(environment1, seedmixtures)
seedmixtures2 <- left_join(environment2, seedmixtures)
traits <- select(traits, name, poolD, poolS)
rm(seedmixtures, environment)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## 1 Establishment rate per plot ########################################################################

### experiment 1 ###
temporary <- seedmixtures1 %>% 
  group_by(plot) %>% 
  summarise(specRich = sum(presence))
environment1 <- environment1 %>%
  left_join(temporary) %>%
  mutate(seeded = rep(rep(c(16,20), c(4,12)), 8),
         estRate = specRich / seeded
         )

### experiment 2 ###
temporary <- seedmixtures2 %>% 
  group_by(plot) %>% 
  summarise(specRich = sum(presence))
environment2 <- environment2 %>%
  left_join(temporary) %>%
  mutate(seeded = rep(20, 64),
         estRate = specRich / seeded
         )


## 2 Establishment rate of species #############################################################################

### Designed seed mixtures of experiment 1 ###
establishment1 <- seedmixtures1 %>% 
  filter(seedmix != "Standard") %>%
  count(name, presence) %>%
  mutate(presence = factor(presence),
         presence <- fct_recode(presence, pres1D = "1", abs1D = "0")
         ) %>% 
  tidyr::spread(presence, n) %>%
  mutate(abs1D = replace(abs1D, is.na(abs1D), 0),
         estRate1D =  pres1D / (abs1D + pres1D),
         rounded1D = round(estRate1D, 2)) %>%
  arrange(estRate1D) %>%
  left_join(traits, by = "name")

### Standard seed mixtures of experiment 1 ###
temporary <- seedmixtures1 %>% 
    filter(seedmix == "Standard") %>%
    count(name, presence) %>%
  mutate(presence = factor(presence),
         presence = fct_recode(presence, pres1S = "1", abs1S = "0")) %>%
  tidyr::spread(presence, n) %>%
  mutate(abs1S = replace(abs1S, is.na(abs1S), 0),
         estRate1S =  pres1S / (abs1S + pres1S),
         rounded1S = round(estRate1S, 2)
         ) %>%
  arrange(estRate1S)
(establishment1 <- left_join(establishment1, temporary))

### Designed seed mixtures of experiment 2 ###
establishment2 <- seedmixtures2 %>% 
    count(name, presence) %>%
  mutate(presence = factor(presence),
         presence <- fct_recode(presence, pres2D = "1", abs2D = "0")
         ) %>%
  tidyr::spread(presence, n) %>%
  mutate(abs2D = replace(abs2D, is.na(abs2D), 0),
         estRate2D =  pres2D / (abs2D + pres2D),
         rounded2D = round(estRate2D, 2)) %>%
  arrange(estRate2D) %>%
  left_join(traits, by = "name")



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# C Export ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#write.table(establishment1, here("data/processed/data_processed_experiment_1_establishment.txt"), sep = "\t", row.names = F, quote = F)
#write.table(establishment2, here("data/processed/data_processed_experiment_2_establishment.txt"), sep = "\t", row.names = F, quote = F)
#write.table(environment1, here("data/processed/data_processed_experiment_1_environment.txt"), sep = "\t", row.names = F, quote = F)
#write.table(environment2, here("data/processed/data_processed_experiment_2_environment.txt"), sep = "\t", row.names = F, quote = F)
