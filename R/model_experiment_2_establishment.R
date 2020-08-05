# Model for Experiment 2 for establishment ####



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Packages ###
library(tidyverse)
library(ggplot2)
library(ggbeeswarm)

### Start ###
rm(list = ls())
setwd("Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_restoration/data/processed")

### Load data ###
### Load data ###
estdata <- read_table2("data_processed_experiment_1_establishment.txt", col_names = T, na="na", col_types =
                         cols(
                           name = col_factor(),
                           presence = col_double(),
                           plot = col_factor()
                         )
)
edata <- read_table2("data_processed_experiment_1_environment.txt", col_names = T, na = "na", col_types = 
                       cols(
                         .default = col_double(),
                         plot = col_factor(),
                         block = col_factor(),
                         position = col_factor(),
                         brickType = col_factor(),
                         seedmix = col_factor(),
                         brickRatio = col_factor(),
                         acid = col_factor(),
                         watering = col_factor(),
                         f.watering = col_factor()
                       )        
)
sdata <- read_table2("data_processed_experiment_1_2_species.txt", col_names = T, na = "na", col_types = 
                       cols(
                         name = col_factor(),
                         family = col_factor(),
                         poolD = col_factor(),
                         estRate1D = col_double(),
                         estRate2D = col_double(),
                         poolS = col_factor(),
                         estRate1S = col_double()
                       )        
)

## 1 Establishment rate per plot (in "data_raw_experiment_1_2.txt" included) ########################################################################

#specRich <- sdata %>% group_by(plot) %>% summarise(n = sum(pres))
#edata$specRich <- specRich$n
#rm(specRich)
#edata$seeded <- rep(rep(c(16,20), c(4,16)), 8)
#edata <- mutate(edata, estRate = specRich / seeded)
#write.table(edata,"just_for_copyR.txt",sep="\t",row.names=F)


## 2 Establishment rate of species (in "data_raw_experiment_1_2.txt" included) #############################################################################

#(est2D <- sdata2 %>% 
#    count(name, presence) %>% 
#    mutate(presence = replace(presence, presence == 1, "pres")) %>%
#    mutate(presence = replace(presence, presence == 0, "abs")) %>%
#    tidyr::spread(presence, n) %>%
#    mutate(abs = replace(abs, is.na(abs), 0)) %>%
#    mutate(presRatio =  pres / (abs + pres)) %>%
#    mutate(rounded = round(presRatio,2)) %>%
#    arrange(presRatio))
#write.table(est2D,"just_for_copy_2D.txt",sep="\t",row.names=F)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

####a Data exploration####
edata %>% group_by(seedmix) %>% summarise(mean.estRate = mean(estRate), 
                                           sd.estRate = sd(estRate))
sdata %>% group_by(poolD) %>% summarise(mean.estRate = mean(estRate2D, na.rm = T), 
                                        sd.estRate = sd(estRate2D, na.rm = T))
table(sdata$poolD)
0.341 / sqrt(40)

