# Model for Experiment 3 for establishment ####



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#library(installr);updateR(browse_news=F, install_R=T, copy_packages = T,copy_Rprofile.site = T,keep_old_packages = T, update_packages = T)

### Packages ###
library(tidyverse)
library(ggplot2)
library(ggbeeswarm)

### Start ###
rm(list = ls())
setwd("Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_for_restoration/data/processed")

estdata <- read_table2("data_processed_experiment_3_estabilshment.txt", col_names = T, na="na", col_types =
                         cols(
                           name = col_factor(),
                           presence = col_double(),
                           plot = col_factor()
                         )
)
edata <- read_table2("data_processed_experiment_3_environment.txt", col_names = T, na = "na", col_types = 
                       cols(
                         .default = col_double(),
                         plot = col_factor(),
                         date = col_date(),
                         brickRatio = col_factor(),
                         texture = col_factor(),
                         compaction = col_factor(),
                         coal = col_factor()
                         )        
)
sdata <- read_table2("data_processed_experiment_3_species.txt", col_names = T, na = "na", col_types = 
                       cols(
                         .default = col_double(),
                         name = col_factor()
                       )        
)

## 1 Establishment rate per plot (in Excel file included) ########################################################################

#specRich <- sdata %>% group_by(plot) %>% summarise(n = sum(pres))
#edata$specRich <- specRich$n
#rm(specRich)
#edata <- mutate(edata, estRate = specRich / 20)
#edata$estRate
#write.table(edata,"just_for_copyR.txt",sep="\t",row.names=F)

## 2. Establishment rate of species (in Excel file included) #############################################################################################

#(est <- sdata %>% 
#  count(name, pres) %>% 
#  mutate(pres = fct_recode(pres, "pres" = "1", "abs" = "0")) %>%
#  tidyr::spread(pres, n) %>%
#  mutate(pres = replace(pres, is.na(pres), 0)) %>%
#  mutate(abs = replace(abs, is.na(abs), 0)) %>%
#  mutate(presRatio =  pres / (abs + pres)) %>%
#  mutate(rounded = round(presRatio,2)) %>%
#  arrange(presRatio))
#write.table(est,"just_for_copyR.txt",sep="\t",row.names=F)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


####a Data exploration####
edata %>% summarise(mean.estRate = mean(estRate), 
                    sd.estRate = sd(estRate))
sdata %>% summarise(mean.estRate = mean(presRatio, na.rm = T), 
                    sd.estRate = sd(presRatio, na.rm = T))
0.292 / sqrt(40)