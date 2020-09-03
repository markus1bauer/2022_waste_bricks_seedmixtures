# Model for Experiment 1 for establishment ####



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Packages ###
library(tidyverse)
library(ggbeeswarm)

### Start ###
rm(list = ls())
setwd("Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_for_restoration/data/processed")

### Load data ###
estdata <- read_table2("data_processed_experiment_1_seed_mixtures.txt", col_names = T, na="na", col_types =
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
sdata <- read_table2("data_processed_experiment_1_2_traits.txt", col_names = T, na = "na", col_types = 
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

#(est1D <- sdata1 %>% 
#  filter(seedmix != "RSM") %>%
#  count(name, presence) %>% 
#  mutate(presence = replace(presence, presence == 1, "pres")) %>%
#  mutate(presence = replace(presence, presence == 0, "abs")) %>%
#  tidyr::spread(presence, n) %>%
#  mutate(abs = replace(abs, is.na(abs), 0)) %>%
#  mutate(presRatio =  pres / (abs + pres)) %>%
#  mutate(rounded = round(presRatio,2)) %>%
#  arrange(presRatio))
#write.table(est1D,"just_for_copy_1D.txt",sep="\t",row.names=F)

#(est1S <- sdata1 %>% 
#    filter(seedmix == "RSM") %>%
#    count(name, presence) %>% 
#    mutate(presence = replace(presence, presence == 1, "pres")) %>%
#    mutate(presence = replace(presence, presence == 0, "abs")) %>%
#    tidyr::spread(presence, n) %>%
#    mutate(abs = replace(abs, is.na(abs), 0)) %>%
#    mutate(presRatio =  pres / (abs + pres)) %>%
#    mutate(rounded = round(presRatio,2)) %>%
#    arrange(presRatio))
#write.table(est1S,"just_for_copy_1S.txt",sep="\t",row.names=F)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

####a Data exploration####
#establishment per plot
edata %>% group_by(seedmix) %>% summarise(mean.estRate = mean(estRate), 
                                          sd.estRate = sd(estRate))
#establishment per species of designed seed mixtures
sdata %>% group_by(poolD) %>% summarise(mean.estRate = mean(estRate1D, na.rm = T), 
                                             sd.estRate = sd(estRate1D, na.rm = T))
table(sdata$poolD)
0.334 / sqrt(40)
#establishment per species of standarad seed mixture
sdata %>% group_by(poolS) %>% summarise(mean.estRate = mean(estRate1S, na.rm = T), 
                                          sd.estRate = sd(estRate1S, na.rm = T))
table(sdata$poolS)
0.334 / sqrt(16)

#1 way
par(mfrow = c(2,2));plot(estRate ~ seedmix, edata);boxplot(estRate ~ brickRatio, edata);plot(estRate ~ acid, edata);plot(estRate ~ watering, edata)
#2way
ggplot(edata, aes(seedmix, estRate, color = brickRatio)) + geom_boxplot() + geom_quasirandom(dodge.width=.7)
ggplot(edata, aes(seedmix, estRate, color = watering)) + geom_boxplot() + geom_quasirandom(dodge.width=.7)
ggplot(edata, aes(acid, estRate, color = brickRatio)) + geom_boxplot() + geom_quasirandom(dodge.width=.7)
ggplot(edata, aes(brickRatio, estRate, color = watering)) + geom_boxplot() + geom_quasirandom(dodge.width=.7)
