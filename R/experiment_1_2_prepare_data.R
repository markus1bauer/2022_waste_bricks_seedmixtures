### Script to prepare data of experiment 1 and 2 ###



### Packages ---------------------------------------------------------------------------------------------
library(tidyverse)

### Start----------------------------------------------------------------------------------------------
rm(list = ls())
setwd("Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/waste_bricks_for_restoration/data/raw")

### Load data ----------------------------------------------------------------------------------------
edata <- read_table2("experiment_1_2_site_raw.txt", col_names = T, na = "na", col_types = 
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

### Create variables ----------------------------------------------------------------------------------
edata$conf.low <- c(1:160);
edata$conf.high <- c(1:160)
edata <- mutate(edata, biomass = grassMass + forbMass)
edata$grassRatio <- edata$grassMass / edata$biomass
edata$f.watering <- factor(edata$watering)
edata$f.watering <- dplyr::recode(edata$f.watering,
                                  "0.5" = "Dry", "1" = "Medium_dry", "2" = "Medium_moist", "3" = "Moist")

### Create data frame for experiment 1 ----------------------------------------------------------------
edata1 <- filter(edata, brickType == "Clean");

### Create data frame for experiment 2 ----------------------------------------------------------------
edata2 <- filter(edata, acid == "Acid" & seedmix == "Robust" | seedmix == "Vigorous" & acid == "Acid")
edata2$seedmix <- factor(edata2$seedmix)

### Save processed data-------------------------------------------------------------------------------
#write.table(edata1,"experiment_1_site_processed.txt", sep="\t", row.names=F)
#write.table(edata2,"experiment_2_site_processed.txt", sep="\t", row.names=F)
