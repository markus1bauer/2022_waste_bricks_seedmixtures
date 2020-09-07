# Prepare data of experiment 1 and 2 for establishment ###



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
seedmixtures <- read_table2("data_raw_experiment_1_2_seed_mixtures.txt", col_names = T, na = "na", col_types = 
                       cols(
                         .default = col_double(),
                         name = col_factor(),
                         plot = col_factor()
                         )        
)
environment <- read_table2("data_raw_experiment_1_2_environment.txt", col_names = T, na = "na", col_types = 
                             cols(
                               .default = col_double(),
                               plot = col_factor(),
                               date = col_date(),
                               block = col_factor(),
                               position = col_factor(),
                               substrate = col_factor(),
                               brickType = col_factor(levels = c("Clean","Demolition")),
                               seedmix = col_factor(levels = c("Standard","Robust","Intermediate","Vigorous")),
                               brickRatio = col_factor(levels = c("5","30")),
                               acid = col_factor(levels = c("Control","Acid"))
                             )        
)
traits <- read_table2("Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_for_restoration/data/processed/data_processed_experiment_1_2_3_traits.txt", col_names = T, na = "na", col_types =
                        cols(
                          .default = col_double(),
                          name = col_factor(),
                          family = col_factor(),
                          poolD = col_factor(),
                          poolS = col_factor()
                        )
                      )
### Create variables for environment data set ###
environment$conf.low <- c(1:160);
environment$conf.high <- c(1:160)
environment <- mutate(environment, biomass = grassMass + forbMass)
environment$grassRatio <- environment$grassMass / environment$biomass
environment$f.watering <- factor(environment$watering)
environment$f.watering <- dplyr::recode(environment$f.watering,
                                        "0.5" = "Dry", "1" = "Medium_dry", "2" = "Medium_moist", "3" = "Moist")

### Create environemnt data frames for experiment 1 and 2 ###
environment <- select(environment, -date, -seedDensity, -substrate, -extraspecNo)
environment1 <- filter(environment, brickType == "Clean")
environment2 <- filter(environment, acid == "Acid" & seedmix == "Robust" | seedmix == "Vigorous" & acid == "Acid")
environment2$seedmix <- factor(environment2$seedmix)
seedmixtures <- filter(seedmixtures, name != "Ranunculus_bulbosus" & name != "Ranunculus_acris")
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
environment1 <- left_join(environment1, temporary)
environment1$seeded <- rep(rep(c(16,20), c(4,12)), 8)
(environment1 <- mutate(environment1, estRate = specRich / seeded))

### experiment 2 ###
temporary <- seedmixtures2 %>% 
  group_by(plot) %>% 
  summarise(specRich = sum(presence))
environment2 <- left_join(environment2, temporary)
environment2$seeded <- rep(20, 64)
(environment2 <- mutate(environment2, estRate = specRich / seeded))


## 2 Establishment rate of species #############################################################################

### Designed seed mixtures of experiment 1 ###
establishment1 <- seedmixtures1 %>% 
  filter(seedmix != "Standard") %>%
  count(name, presence) 
establishment1$presence <- factor(establishment1$presence)
establishment1$presence <- fct_recode(establishment1$presence, pres1D = "1", abs1D = "0")
(establishment1 <- establishment1 %>% 
  tidyr::spread(presence, n) %>%
  mutate(abs1D = replace(abs1D, is.na(abs1D), 0)) %>%
  mutate(estRate1D =  pres1D / (abs1D + pres1D)) %>%
  mutate(rounded1D = round(estRate1D, 2)) %>%
  arrange(estRate1D)
 )
establishment1 <- left_join(establishment1, traits, by = "name")

### Standard seed mixtures of experiment 1 ###
temporary <- seedmixtures1 %>% 
    filter(seedmix == "Standard") %>%
    count(name, presence)
temporary$presence <- factor(temporary$presence)
temporary$presence <- fct_recode(temporary$presence, pres1S = "1", abs1S = "0")
(temporary <- temporary %>%  
   tidyr::spread(presence, n) %>%
    mutate(abs1S = replace(abs1S, is.na(abs1S), 0)) %>%
    mutate(estRate1S =  pres1S / (abs1S + pres1S)) %>%
    mutate(rounded1S = round(estRate1S, 2)) %>%
    arrange(estRate1S)
  )
(establishment1 <- left_join(establishment1, temporary))

### Designed seed mixtures of experiment 2 ###
establishment2 <- seedmixtures2 %>% 
    count(name, presence)
establishment2$presence <- factor(establishment2$presence)
establishment2$presence <- fct_recode(establishment2$presence, pres2D = "1", abs2D = "0")
(establishment2 <- establishment2 %>% 
    tidyr::spread(presence, n) %>%
    mutate(abs2D = replace(abs2D, is.na(abs2D), 0)) %>%
    mutate(estRate2D =  pres2D / (abs2D + pres2D)) %>%
    mutate(rounded2D = round(estRate2D, 2)) %>%
    arrange(estRate2D)
  )
establishment2 <- left_join(establishment2, traits, by = "name")



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# C Export ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#write.table(establishment1, "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_for_restoration/data/processed/data_processed_experiment_1_establishment.txt", sep="\t", row.names = F, quote = F)
#write.table(establishment2, "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_for_restoration/data/processed/data_processed_experiment_2_establishment.txt", sep="\t", row.names = F, quote = F)
#write.table(environment1, "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_for_restoration/data/processed/data_processed_experiment_1_environment.txt", sep="\t", row.names = F, quote = F)
#write.table(environment2, "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_for_restoration/data/processed/data_processed_experiment_2_environment.txt", sep="\t", row.names = F, quote = F)
