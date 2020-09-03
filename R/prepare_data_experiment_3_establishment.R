### Script to prepare data of experiment 1 and 2 ###



### Packages ###

### Start ###
rm(list = ls())
setwd("Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_for_restoration/data/raw")

### Load data ###
estdata <- read_table2("data_raw_experiment_3_seed_mixtures.txt", col_names = T, na = "na", col_types = 
                         cols(
                           .default = col_double(),
                           name = col_factor(),
                           plot = col_factor()
                         )        
)
sdata <- read_table2("data_raw_experiment_3_traits.txt", col_names = T, na="na", col_types =
                       cols(
                         name = col_factor(),
                         descriptor = col_factor(),
                         nomenclature = col_factor(),
                         family = col_factor(),
                         abb = col_factor()
                         )
)

### Exclude Ranunculus ###
sdata <- sdata %>%
  filter(name != "Ranunculus_acris" & name != "Ranunculus_bulbosus") %>%
  select(-(descriptor:abb))

### Save processed data ###
write.table(estdata, "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_for_restoration/data/processed/data_processed_experiment_3_seed_mixtures.txt", sep = "\t", row.names = F)
write.table(sdata, "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_for_restoration/data/processed/data_processed_experiment_3_traits.txt", sep = "\t", row.names = F)
