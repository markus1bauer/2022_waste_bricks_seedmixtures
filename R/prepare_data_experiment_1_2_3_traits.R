# Brick-based substrates and designed seedmixtures
# Collect traits data ####
# Markus Bauer
# 2022-01-24
# Citation: 
## Bauer M, Krause M, Heizinger V, Kollmann J (submitted) 
## Using waste bricks for recultivation: no negative effects of brick-augmented substrates with varying acid pre-treatment, soil type and moisture on contrasting seed mixtures
## Unpublished data.


### Packages ###
library(here)
library(tidyverse)
library(data.table)

### Start ###
rm(list = ls())
setwd(here("data/raw"))
#library(installr);updateR(browse_news=F, install_R=T, copy_packages = T,copy_Rprofile.site = T,keep_old_packages = T, update_packages = T)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

seedmixtures <- read_csv("data_raw_experiment_1_2_seed_mixtures.csv", col_names = T, na = c("na", "NA", ""), col_types = 
                           cols(.default = "?")) %>%
  filter(name != "Ranunculus_bulbosus" & name != "Ranunculus_acris")
seedmixtures <- read_csv("data_raw_experiment_3_seed_mixtures.csv", col_names = T, na = c("na", "NA", ""), col_types = 
                           cols(.default = "?")) %>%
  filter(name != "Ranunculus_bulbosus" & name != "Ranunculus_acris")

environment12 <- read_csv("data_raw_experiment_1_2_environment.csv", col_names = T, na = c("na", "NA", ""), col_types = 
                          cols(.default = "?")) 
environment3 <- read_csv("data_raw_experiment_3_environment.csv", col_names = T, na = c("na", "NA", ""), col_types = 
                          cols(.default = "?")) 

traits <- read_csv("data_raw_experiment_1_2_3_traits.csv", col_names = T, na = c("na", "NA", ""), col_types =
                        cols(
                          name = "f",
                          family = "f",
                          poolDesign = "f",
                          poolStandard = "f"
                          ))

#### Separate data sets ####
environment1 <- environment %>%
  filter(brickType == "Clean")
environment2 <- environment %>%
  filter(acid == "Acid" & seedmix == "Robust" | seedmix == "Vigorous" & acid == "Acid") %>%
  mutate(seedmix = factor(seedmix))
seedmixtures1 <- environment1 %>%
  left_join(seedmixtures, by = "plot")
seedmixtures2 <- environment2 %>%
  left_join(seedmixtures, by = "plot")



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Include new data ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## 2 Include traits #########################################################################################


### a Specific leaf area --------------------------------------------------------------------------------------

slaData <- fread("data_raw_traitbase_try_20190411_6084_sla.txt", 
                 header = T, sep = "\t", dec = ".", quote = "", data.table = T) %>%
  rename(name = AccSpeciesName) %>%
  select(name, TraitID, StdValue) %>%
  filter(TraitID == 3115) %>%
  group_by(name) %>%
  summarise(value = median(StdValue, na.rm = T)) %>%
  filter(!is.na(value)) %>%
  ungroup() %>%
  mutate(name = str_replace_all(name, " ", "_"))
#Include values in main data frame
traits <- traits %>%
  left_join(slaData, by = "name") %>%
  rename(slaTry = value)


### b Canopy height at maturity -----------------------------------------------------------------------------------

heightData <- fread("data_raw_traitbase_try_20190220_5762_height-seedmass.txt", 
                 header = T, sep = "\t", dec = ".", quote = "", data.table = T) %>%
  rename(name = AccSpeciesName) %>%
  select(name, TraitID, TraitName, StdValue) %>%
  filter(TraitID == 3106) %>%
  group_by(name) %>%
  mutate(value = as.double(StdValue)) %>%
  summarise(value = median(value, na.rm = T)) %>%
  filter(!is.na(value)) %>%
  ungroup() %>%
  mutate(name = str_replace_all(name, " ", "_"),
         name = str_replace(name, "Centaurea_scabiosa_subsp._scabiosa", "Centaurea_scabiosa"),
         name = str_replace(name, "Cerastium_fontanum_subsp._vulgare", "Cerastium_fontanum_ssp_vulgare"),
         name = str_replace(name, "Dianthus_carthusianorum_subsp._carthusianorum", "Dianthus_carthusianorum"),
         name = str_replace(name, "Leucanthemum_ircutianum"," Leucanthemum_vulgare"),
         name = str_replace(name, "Pastinaca_sativa_subsp._sativa", "Pastinaca_sativa")
         )
#Include values in main data frame
traits <- traits %>%
  left_join(heightData, by = "name") %>%
  rename(heightTry = value)

### c Seed mass -------------------------------------------------------------------------------------------------

seedmassData <- fread("data_raw_traitbase_try_20190220_5762_height-seedmass.txt", 
                    header = T, sep = "\t", dec = ".", quote = "", data.table = T) %>%
  rename(name = AccSpeciesName) %>%
  select(name, TraitID, TraitName, StdValue) %>%
  filter(TraitID == 26) %>%
  group_by(name) %>%
  mutate(value = as.double(StdValue)) %>%
  summarise(value = median(value, na.rm = T)) %>%
  filter(!is.na(value)) %>%
  ungroup() %>%
  mutate(name = str_replace_all(name, " ", "_"),
         name = str_replace(name, "Centaurea_scabiosa_subsp._scabiosa", "Centaurea_scabiosa"),
         name = str_replace(name, "Cerastium_fontanum_subsp._vulgare", "Cerastium_fontanum_ssp_vulgare"),
         name = str_replace(name, "Dianthus_carthusianorum_subsp._carthusianorum", "Dianthus_carthusianorum"),
         name = str_replace(name, "Leucanthemum_ircutianum"," Leucanthemum_vulgare"),
         name = str_replace(name, "Pastinaca_sativa_subsp._sativa", "Pastinaca_sativa")
  )
#Include values in main data frame
traits <- traits %>%
  left_join(seedmassData, by = "name") %>%
  rename(seedmassTry = value)


### d Check completeness of traits -------------------------------------------------------------------------------------------

test <- traits %>%
  filter(poolDesign == 1) %>%
  filter(name != "Ranunculus_spec")
test$name[which(!(test$name %in% slaData$name))]
(41-length(which(!(test$name %in% slaData$name))))/41 #100%
test$name[which(!(test$name %in% heightData$name))]
(41-length(which(!(test$name %in% heightData$name))))/41 #95%
test$name[which(!(test$name %in% seedmassData$name))]
(41-length(which(!(test$name %in% seedmassData$name))))/41 #93%
rm(tryData, slaData, seedmassData, heightData, test)
select(traits, name, sla, slaTry)
select(traits, name, height, heightTry)
select(traits, name, seedmass, seedmassTry)


## 3 Establishment rate per plot ########################################################################

### a Experiment 1 ---------------------------------------------------------------------------------------------------
temporary <- seedmixtures1 %>% 
  group_by(plot) %>% 
  summarise(specRich = sum(presence))
environment1 <- environment1 %>%
  left_join(temporary) %>%
  mutate(seeded = rep(rep(c(16,20), c(4,12)), 8),
         estRate = specRich / seeded
  )

### b Experiment 2 ---------------------------------------------------------------------------------------------------
temporary <- seedmixtures2 %>% 
  group_by(plot) %>% 
  summarise(specRich = sum(presence))
environment2 <- environment2 %>%
  left_join(temporary) %>%
  mutate(seeded = rep(20, 64),
         estRate = specRich / seeded
  )

### c Experiment 3 ---------------------------------------------------------------------------------------------------
data <- seedmixtures %>% 
  group_by(plot) %>% 
  summarise(specRich = sum(presence))
environment <- left_join(environment, data) %>%
  mutate(estRate = specRich / 20)


## 4 Establishment rate of species #############################################################################

### a Designed seed mixes of experiment 1 ---------------------------------------------------------------------------------------------------
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

### b Standard seed mixes of experiment 1 ---------------------------------------------------------------------------------------------------
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

### c Designed seed mixes of experiment 2 ---------------------------------------------------------------------------------------------------
establishment2 <- seedmixtures2 %>% 
  count(name, presence) %>%
  mutate(presence = factor(presence),
         presence <- fct_recode(presence, pres2D = "1", abs2D = "0")
  ) %>%
  tidyr::spread(presence, n) %>%
  mutate(abs2D = replace(abs2D, is.na(abs2D), 0),
         estRate2D =  pres2D / (abs2D + pres2D),
         rounded2D = round(estRate2D, 2)
  ) %>%
  arrange(estRate2D) %>%
  left_join(traits, by = "name")

### d Designed seed mixes of experiment 3 ---------------------------------------------------------------------------------------------------
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


write_csv(traits, here("data/processed/data_processed_experiment_1_2_3_traits.csv"))
