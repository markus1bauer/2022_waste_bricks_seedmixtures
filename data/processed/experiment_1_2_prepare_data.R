### Script to prepare data
#library(installr);updateR(browse_news=F, install_R=T, copy_packages = T,copy_Rprofile.site = T,keep_old_packages = T, update_packages = T)

### Packages -----------------------------------------------------------------------------------
library(tidyverse)

### Start-----------------------------------------------------------------------
rm(list = ls())
setwd("Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/waste_bricks_for_restoration/data/processed")

### Load data -------------------------------------------------------------------
edata <- read_table2("experiment_1_2_site_raw.txt", col_names = T, na = "na", col_types = 
                       cols(
                         .default = col_double(),
                         plot = col_factor(),
                         block = col_factor(),
                         brickType = col_factor(levels = c("Clean","Demolition")),
                         seedmix = col_factor(levels = c("Standard","Robust","Intermediate","Vigorous")),
                         brickRatio = col_factor(levels = c("5","30")),
                         acid = col_factor(levels = c("Control","Acid"))
                       )        
)

### Create variables -------------------------------------------------------------------
edata$conf.low <- c(1:160);
edata$conf.high <- c(1:160)
edata <- mutate(edata, biomass = grassMass + forbMass)
edata$grassRatio <- edata$grassMass / edata$biomass
edata$f.watering <- factor(edata$watering)

### Save processed data-------------------------------------------------------------------
#write.table(edata,"experiment_1_2_site_processed.txt",sep="\t",row.names=F)
