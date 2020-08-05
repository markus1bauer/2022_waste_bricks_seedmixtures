### Script to prepare data of experiment 1 and 2 for establishment ###



### Packages ---------------------------------------------------------------------------------------------
library(tidyverse)

### Start----------------------------------------------------------------------------------------------
rm(list = ls())
setwd("Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_for_restoration/data/raw")

### Load data ----------------------------------------------------------------------------------------
estdata <- read_table2("data_raw_experiment_1_2_establishment.txt", col_names = T, na = "na", col_types = 
                       cols(
                         .default = col_double(),
                         name = col_factor(),
                         plot = col_factor()
                         )        
)
edata <- read_table2("data_raw_experiment_1_2_environment.txt", col_names = T, na = "na", col_types = 
                       cols(
                         .default = col_double(),
                         plot = col_factor(),
                         block = col_factor(),
                         position = col_factor(),
                         brickType = col_factor(levels = c("Clean","Demolition")),
                         seedmix = col_factor(levels = c("Standard","Robust","Intermediate","Vigorous")),
                         brickRatio = col_factor(levels = c("5","30")),
                         acid = col_factor(levels = c("Control","Acid"))
                       )        
)
sdata <- read_table2("data_raw_experiment_1_2_species.txt", col_names = T, na="na", col_types =
                       cols(
                         name = col_factor(),
                         descriptor = col_factor(),
                         nomenclature = col_factor(),
                         family = col_factor(),
                         abb = col_factor()
                       )
)


### Combine data frames and exclude Ranunculus ----------------------------------------------------------------------------------
estdata <- full_join(estdata, edata, by = "plot")
estdata <- select(data, name, plot, weight, weightRatio, presence, brickType, acid, seedmix)
sdata <- sdata %>%
  filter(name != "Ranunculus_acris" & name != "Ranunculus_bulbosus") %>%
  select(-(descriptor:abb))

### Create data frame for Experiment 1 ----------------------------------------------------------------
estdata1 <- filter(estdata, brickType == "Clean");

### Create data frame for Experiment 2 ----------------------------------------------------------------
estdata2 <- filter(estdata, acid == "Acid" & seedmix == "Robust" | seedmix == "Vigorous" & acid == "Acid")

### Save processed data-------------------------------------------------------------------------------
write.table(estdata1,"Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_for_restoration/data/processed/data_processed_experiment_1_establishment.txt", sep="\t", row.names=F)
write.table(estdata2,"Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_for_restoration/data/processed/data_processed_experiment_2_establishment.txt", sep="\t", row.names=F)
write.table(sdata,"Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_for_restoration/data/processed/data_processed_experiment_1_2_species.txt", sep="\t", row.names=F)
