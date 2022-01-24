# Brick-based substrates and designed seedmixtures
# Collect traits data ####
# Markus Bauer
# 2022-01-24
# Citation: 
## Bauer M, Krause M, Heizinger V, Kollmann J (submitted) 
## Using waste bricks for recultivation: no negative effects of brick-augmented substrates with varying acid pre-treatment, soil type and moisture on contrasting seed mixtures
## Unpublished data.


### Packages ###
library(tidyverse)
library(data.table)

### Start ###
rm(list = ls())
setwd("Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_for_restoration/data/raw")
#library(installr);updateR(browse_news=F, install_R=T, copy_packages = T,copy_Rprofile.site = T,keep_old_packages = T, update_packages = T)

### 1 Load species list #####################################################################################

traits <- read_table2("data_raw_experiment_1_2_3_traits.txt", col_names = T, na = "na", col_types =
                        cols(
                          name = col_factor(),
                          family = col_factor(),
                          poolD = col_factor(),
                          poolS = col_factor()
                          )        
)


### 2 Specific leaf area #########################################################################################

slaData <- fread("data_raw_TRY_sla.txt", header = T, sep = "\t", dec = ".", quote = "", data.table = T)
slaData <- slaData %>%
  rename(name = AccSpeciesName) %>%
  select(name, ObservationID, TraitID, TraitName, StdValue, UnitName) %>%
  filter(TraitID == 3115) %>%
  group_by(name) %>%
  summarise(value = median(StdValue)) %>%
  filter(!is.na(value)) %>%
  select(name, value) %>%
  ungroup()
slaData$name <- gsub(" ","_",slaData$name)
slaData$name <- sub("Centaurea_scabiosa_subsp._scabiosa", "Centaurea_scabiosa", slaData$name)
slaData$name <- sub("Cerastium_fontanum_subsp._vulgare", "Cerastium_fontanum_ssp_vulgare", slaData$name)
slaData$name <- sub("Dianthus_carthusianorum_subsp._carthusianorum", "Dianthus_carthusianorum", slaData$name)
slaData$name <- sub("Leucanthemum_ircutianum", "Leucanthemum_vulgare", slaData$name)
slaData$name <- sub("Pastinaca_sativa_subsp._sativa", "Pastinaca_sativa", slaData$name)
#Include values in main data frame
traits <- left_join(traits, slaData, by = "name")
(traits <- traits %>%
  rename(slaTry = value) %>%
  select(name, family, n, f, r, sla, height, seedmass, slaTry, poolD, poolS))


### 3 Canopy height at maturity #########################################################################################

tryData <- fread("data_raw_TRY_height-seedmass.txt", header = T, sep = "\t", dec = ".", quote = "", data.table = T)
tryData$AccSpeciesName <- as.factor(tryData$AccSpeciesName)
tryData$TraitID <- as.factor(tryData$TraitID)
tryData$TraitName <- as.factor(tryData$TraitName)
tryData$StdValue <- as.numeric(tryData$StdValue)
tryData <- tryData %>%
  rename(name = AccSpeciesName) %>%
  select(name, ObservationID, TraitID, TraitName, StdValue, UnitName) %>%
  filter(TraitID == c(26, 3106)) %>%
  group_by(TraitName, name) %>%
  summarise(value = median(StdValue)) %>%
  filter(!is.na(value)) %>%
  select(name, TraitName, value) %>%
  ungroup()
tryData$name <- gsub(" ", "_", tryData$name)
tryData$name <- sub("Centaurea_scabiosa_subsp._scabiosa", "Centaurea_scabiosa", tryData$name)
tryData$name <- sub("Cerastium_fontanum_subsp._vulgare", "Cerastium_fontanum_ssp_vulgare", tryData$name)
tryData$name <- sub("Dianthus_carthusianorum_subsp._carthusianorum", "Dianthus_carthusianorum", tryData$name)
tryData$name <- sub("Leucanthemum_ircutianum"," Leucanthemum_vulgare", tryData$name)
tryData$name <- sub("Pastinaca_sativa_subsp._sativa", "Pastinaca_sativa", tryData$name)
heightData <- tryData %>%
  filter(TraitName == "Plant height vegetative") %>%
  select(name, value)
traits <- left_join(traits, heightData, by = "name")
(traits <- traits %>%
  rename(heightTry = value) %>%
  select(name, family, n, f, r, sla, height, seedmass, slaTry, heightTry, poolD, poolS))


### 4 Seed mass #########################################################################################

seedmassData <- tryData %>%
  filter(TraitName == "Seed dry mass") %>%
  select(name, value)
traits <- left_join(traits, seedmassData, by = "name")
(traits <- traits %>%
  rename(seedmassTry = value) %>%
  select(name, family, n, f, r, sla, height, seedmass, slaTry, heightTry, seedmassTry, poolD, poolS))


### 5 Check completeness of traits ###################################################################################################

test <- traits %>%
  filter(poolD == 1) %>%
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


### 6 Data export ###################################################################################################

#write.table(traits, "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_for_restoration/data/processed/data_processed_experiment_1_2_3_traits.txt", sep="\t", row.names = F, quote = F)
