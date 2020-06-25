### Script to prepare data of experiment 1 and 2 ###



### Packages ---------------------------------------------------------------------------------------------
library(tidyverse)

### Start----------------------------------------------------------------------------------------------
rm(list = ls())
setwd("Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_for_restoration/data/raw")

### Load data ----------------------------------------------------------------------------------------
edata <- read_table2("experiment_3_site_raw.txt", col_names = T, na="na", col_types =
                       cols(
                         plot = col_factor(),
                         brickRatio = col_factor(levels = c("5","30")),
                         texture = col_factor(levels=c("Loam","Medium","Sand")),
                         compaction = col_factor(levels=c("Control","Compaction")),
                         coal = col_factor(levels=c("Control","Coal")),
                         ph = col_double(),
                         biomass = col_double(),
                         estRate = col_double()
                       )
)

### Create variables ----------------------------------------------------------------------------------
edata$conf.low <- c(1:72);
edata$conf.high <- c(1:72)

### Save processed data-------------------------------------------------------------------------------
write.table(edata,"experiment_3_site_processed.txt", sep="\t", row.names=F)
