# Brick-based substrates and designed seedmixtures
# Collect traits data ####
# Markus Bauer
# 2022-01-24



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ###############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### Packages ###
#library(installr);updateR(browse_news=F, install_R=T, copy_packages = T,copy_Rprofile.site = T,keep_old_packages = T, update_packages = T)
library(here)
library(tidyverse)
library(data.table)

### Start ###
rm(list = ls())
setwd(here("data", "raw"))

### Load data ###
seedmixtures12 <- read_csv("data_raw_experiment_1_2_seed_mixtures.csv",
                           col_names = TRUE, na = c("na", "NA", ""),
                           col_types =
                           cols(.default = "?")) %>%
  filter(name != "Ranunculus_bulbosus" & name != "Ranunculus_acris")

seedmixtures3 <- read_csv("data_raw_experiment_3_seed_mixtures.csv",
                          col_names = TRUE, na = c("na", "NA", ""),
                          col_types =
                           cols(.default = "?")) %>%
  filter(name != "Ranunculus_bulbosus" & name != "Ranunculus_acris")

environment12 <- read_csv("data_raw_experiment_1_2_environment.csv",
                          col_names = TRUE, na = c("na", "NA", ""),
                          col_types =
                          cols(.default = "?")) 

environment3 <- read_csv("data_raw_experiment_3_environment.csv",
                         col_names = TRUE, na = c("na", "NA", ""),
                         col_types =
                          cols(.default = "?")) 

traits <- read_csv("data_raw_experiment_1_2_3_traits.csv",
                   col_names = TRUE, na = c("na", "NA", ""),
                   col_types =
                        cols(
                          name = "f",
                          family = "f",
                          poolDesign = "f",
                          poolStandard = "f"
                          ))

#### Separate data sets ####
environment1 <- environment12 %>%
  filter(brickType == "Clean")
environment2 <- environment12 %>%
  filter(acid == "Acid" & seedmix == "Robust" |
           seedmix == "Vigorous" & acid == "Acid")
seedmixtures1 <- environment1 %>%
  select(plot, seedmix) %>%
  left_join(seedmixtures12, by = "plot")
seedmixtures2 <- environment2 %>%
  select(plot) %>%
  left_join(seedmixtures12, by = "plot")



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Include new data ##########################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## 2 Include traits ###########################################################

### a Specific leaf area ------------------------------------------------------
slaData <- fread("data_raw_traitbase_try_20190411_6084_sla.txt", 
                 header = TRUE, sep = "\t", dec = ".", quote = "",
                 data.table = TRUE) %>%
  rename(name = AccSpeciesName) %>%
  select(name, TraitID, StdValue) %>%
  filter(TraitID == 3115) %>%
  group_by(name) %>%
  summarise(value = median(StdValue, na.rm = TRUE)) %>%
  filter(!is.na(value)) %>%
  ungroup() %>%
  mutate(name = str_replace_all(name, " ", "_"))
#Include values in main data frame
traits <- traits %>%
  left_join(slaData, by = "name") %>%
  rename(slaTry = value)

### b Canopy height at maturity -----------------------------------------------
heightData <- fread("data_raw_traitbase_try_20190220_5762_height-seedmass.txt", 
                 header = TRUE, sep = "\t", dec = ".", quote = "",
                 data.table = TRUE) %>%
  rename(name = AccSpeciesName) %>%
  select(name, TraitID, TraitName, StdValue) %>%
  filter(TraitID == 3106) %>%
  group_by(name) %>%
  mutate(value = as.double(StdValue)) %>%
  summarise(value = median(value, na.rm = TRUE)) %>%
  filter(!is.na(value)) %>%
  ungroup() %>%
  mutate(name = str_replace_all(name, " ", "_"),
         name = str_replace(name,
                            "Centaurea_scabiosa_subsp._scabiosa",
                            "Centaurea_scabiosa"),
         name = str_replace(name,
                            "Cerastium_fontanum_subsp._vulgare",
                            "Cerastium_fontanum_ssp_vulgare"),
         name = str_replace(name,
                            "Dianthus_carthusianorum_subsp._carthusianorum",
                            "Dianthus_carthusianorum"),
         name = str_replace(name,
                            "Leucanthemum_ircutianum",
                            "Leucanthemum_vulgare"),
         name = str_replace(name,
                            "Pastinaca_sativa_subsp._sativa",
                            "Pastinaca_sativa")
         )
#Include values in main data frame
traits <- traits %>%
  left_join(heightData, by = "name") %>%
  rename(heightTry = value)

### c Seed mass --------------------------------------------------------------
seedmassData <- fread("data_raw_traitbase_try_20190220_5762_height-seedmass.txt", 
                    header = TRUE, sep = "\t", dec = ".", quote = "",
                    data.table = TRUE) %>%
  rename(name = AccSpeciesName) %>%
  select(name, TraitID, TraitName, StdValue) %>%
  filter(TraitID == 26) %>%
  group_by(name) %>%
  mutate(value = as.double(StdValue)) %>%
  summarise(value = median(value, na.rm = TRUE)) %>%
  filter(!is.na(value)) %>%
  ungroup() %>%
  mutate(name = str_replace_all(name, " ", "_"),
         name = str_replace(name,
                            "Centaurea_scabiosa_subsp._scabiosa",
                            "Centaurea_scabiosa"),
         name = str_replace(name,
                            "Cerastium_fontanum_subsp._vulgare",
                            "Cerastium_fontanum_ssp_vulgare"),
         name = str_replace(name,
                            "Dianthus_carthusianorum_subsp._carthusianorum",
                            "Dianthus_carthusianorum"),
         name = str_replace(name,
                            "Leucanthemum_ircutianum",
                            "Leucanthemum_vulgare"),
         name = str_replace(name,
                            "Pastinaca_sativa_subsp._sativa",
                            "Pastinaca_sativa")
  )
#Include values in main data frame
traits <- traits %>%
  left_join(seedmassData, by = "name") %>%
  rename(seedmassTry = value)

### d Check completeness of traits -------------------------------------------
test <- traits %>%
  filter(poolDesign == 1) %>%
  filter(name != "Ranunculus_spec")
test$name[which(!(test$name %in% slaData$name))]
(41 - length(which(!(test$name %in% slaData$name)))) / 41 #98%
test$name[which(!(test$name %in% heightData$name))]
(41 - length(which(!(test$name %in% heightData$name)))) / 41 #98%
test$name[which(!(test$name %in% seedmassData$name))]
(41 - length(which(!(test$name %in% seedmassData$name)))) / 41 #95%
rm(tryData, slaData, seedmassData, heightData, test)


## 3 Establishment rate of species ############################################

### a Designed seed mixes of experiment 1 -------------------------------------
data <- seedmixtures1 %>% 
  filter(seedmix != "Standard") %>%
  count(name, presence) %>%
  mutate(presence = factor(presence),
         presence = fct_recode(presence, pres1Design = "1", abs1Design = "0")
         ) %>% 
  tidyr::spread(presence, n) %>%
  mutate(abs1Design = replace(abs1Design, is.na(abs1Design), 0),
         estRate1Design =  pres1Design / (abs1Design + pres1Design),
         estRate1Design = round(estRate1Design, 2))
traits <- traits %>%
  left_join(data, by = "name")

### b Standard seed mixes of experiment 1 -------------------------------------
data <- seedmixtures1 %>% 
  filter(seedmix == "Standard") %>%
  count(name, presence) %>%
  mutate(presence = factor(presence),
         presence = fct_recode(presence,
                               pres1Standard = "1",
                               abs1Standard = "0")) %>%
  tidyr::spread(presence, n) %>%
  mutate(abs1Standard = replace(abs1Standard, is.na(abs1Standard), 0),
         estRate1Standard =  pres1Standard / (abs1Standard + pres1Standard),
         estRate1Standard = round(estRate1Standard, 2)
         )
traits <- traits %>%
  left_join(data, by = "name")

### c Experiment 2 -----------------------------------------------------------
data <- seedmixtures2 %>% 
  count(name, presence) %>%
  mutate(presence = factor(presence),
         presence = fct_recode(presence, pres2 = "1", abs2 = "0")
         ) %>%
  tidyr::spread(presence, n) %>%
  mutate(abs2 = replace(abs2, is.na(abs2), 0),
         estRate2 =  pres2 / (abs2 + pres2),
         estRate2 = round(estRate2, 2)
         )
traits <- traits %>%
  left_join(data, by = "name")

### d Experiment 3 -----------------------------------------------------------
data <- seedmixtures3 %>% 
  count(name, presence) %>%
  mutate(presence = factor(presence),
         presence = fct_recode(presence, pres3 = "1", abs3 = "0")) %>%
  tidyr::spread(presence, n) %>%
  mutate(pres3 = replace(pres3, is.na(pres3), 0),
         abs3 = replace(abs3, is.na(abs3), 0),
         estRate3 =  pres3 / (abs3 + pres3),
         estRate3 = round(estRate3, 2))
traits <- traits %>%
  left_join(data, by = "name")



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# C Export ###################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


write_csv(traits,
          here("data", "processed",
               "data_processed_experiment_1_2_3_traits.csv"))
