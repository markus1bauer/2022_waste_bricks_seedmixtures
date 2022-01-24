# Brick-based substrates and designed seedmixtures
# Make seed mixtures with Laughlin function for Experiment 1 and 2 ####
# Markus Bauer
# 2022-01-24
# Citation: 
## Bauer M, Krause M, Heizinger V, Kollmann J (submitted) 
## Using waste bricks for recultivation: no negative effects of brick-augmented substrates with varying acid pre-treatment, soil type and moisture on contrasting seed mixtures
## Unpublished data.



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Packages ###
library(here)
library(tidyverse)
library(Select)

### Start ###
rm(list = ls())
setwd(here("data/processed"))
#library(installr);updateR(browse_news=F, install_R=T, copy_packages = T,copy_Rprofile.site = T,keep_old_packages = T, update_packages = T)

### Load species list ###
traits <- read_table("data_processed_experiment_1_2_3_traits.txt", col_names = T, na = "na", col_types =
                       cols(
                         .default = col_double(),
                         name = col_factor(),
                         family = col_factor(),
                         poolD = col_factor(),
                         poolS = col_factor()
                         )        
)
traits <- traits %>%
  filter(poolD == 1) %>%
  filter(name != "Ranunculus_spec") %>%
  mutate(grass = if_else(family == "Poaceae", 1, 0),
         legume = if_else(family == "Fabaceae", 1, 0))
traits$name <- factor(traits$name)
###Logarithmize trait data
traits$sla <- log(traits$sla)
traits$height <- log(traits$height)
traits$seedmass <- log(traits$seedmass)
###Dummies
traits[c(which(is.na(traits$sla))), "sla"] <- 3.068053
traits[c(which(is.na(traits$seedmass))), "seedmass"] <- 0
traits[c(which(is.na(traits$r))), "r"] <- 6.5
###Create subsamples
herbs <- traits %>%
  filter(!(family == "Poaceae" | family == "Fabaceae")) %>%
  select(name, family)
grass <- traits %>%
  filter(family == "Poaceae") %>%
  select(name, family)
legumes <- traits %>%
  filter(family == "Fabaceae") %>%
  select(name, family)



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Make seed mixtures ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### Taxonomic selection #########################################################################################################

compositions <- as.data.frame(replicate(72, {comp <- c(as.character(sample(herbs$name, 12)),
                                                       as.character(sample(grass$name, 5)),
                                                       as.character(sample(legumes$name, 3))
                                                   )})) %>%
  gather("comp", "name", 1:72)

table(compositions$name)
length(table(compositions$name))

compositions <- traits %>%
  inner_join(compositions, by = "name") %>%
  select(name, comp, sla, seedmass, r, grass, legume) %>%
  mutate(name = as.factor(name),
         comp = as.numeric(gsub("V", "", comp))
         )


### Determine abundances with the Laughlin function #########################################################################################################

ratioResults <- c(0)
for (i in 1:72) { #intermediate
  plotcompositions <- compositions[which(compositions$comp == i),]
  plotcompositions <- column_to_rownames(plotcompositions, "name")
  plotcompositions <- plotcompositions[,-(1)]
  mix <- selectSpecies(as.matrix(plotcompositions),
                       constraints = c(sla = 3.068053,
                                       seedmass = 0,
                                       r = 7,
                                       grass = 0.45,
                                       legume = 0.1),
                       as.matrix(plotcompositions),
                       obj = "QH",
                       capd = T
                       )
  ratioResults <- append(ratioResults, mix$prob)
}
compositions <- arrange(compositions, comp)
compositions$ratio <- ratioResults[-1]


## 1. Control of seedmixtures #########################################################################################################

###Does a species dominate single plots?
compositions[which(compositions$ratio > 0.75),]
###Is the summed ratio of all species 1 per plot?
plotRatio <- compositions %>% 
  group_by(comp) %>%
  summarise(sum = sum(ratio))
plotRatio[which(plotRatio$sum < 0.99 | plotRatio$sum > 1.01),]
###What is the total seed weight per plot?
plotWeights <- compositions %>% 
  group_by(comp) %>% 
  summarise(sum = sum(weight))
table(plotWeights$sum)
###correct unsolvable taxonomic composition
compositions[compositions$comp == 4, "name"] <- as.character(comp <- c(as.character(sample(herbs$name, 12)),as.character(sample(grass$name, 5)),as.character(sample(legumes$name, 3))))
compositions[compositions$comp == 15, "name"] <- as.character(comp <- c(as.character(sample(herbs$name, 12)),as.character(sample(grass$name, 5)),as.character(sample(legumes$name, 3))))
compositions[compositions$comp == 16, "name"] <- as.character(comp <- c(as.character(sample(herbs$name, 12)),as.character(sample(grass$name, 5)),as.character(sample(legumes$name, 3))))
compositions[compositions$comp == 40, "name"] <- as.character(comp <- c(as.character(sample(herbs$name, 12)),as.character(sample(grass$name, 5)),as.character(sample(legumes$name, 3))))
compositions[compositions$comp == 56, "name"] <- as.character(comp <- c(as.character(sample(herbs$name, 12)),as.character(sample(grass$name, 5)),as.character(sample(legumes$name, 3))))
names <- select(compositions, name, comp)
compositions <- traits %>%
  inner_join(names, by = "name") %>%
  select(name, comp, sla, seedmass, r, grass, legumes)
####Correct wrong mixture ratios (always change comp number in first and last row of this section)
plotcompositions <- compositions[which(compositions$comp == 56),]
plotcompositions <- column_to_rownames(plotcompositions, "name")
plotcompositions <- plotcompositions[,-1]
mix <- selectSpecies(as.matrix(plotcompositions),
                     constraints = c(sla = 3.068053,
                                     seedmass = 0,
                                     r = 7,
                                     grass = 0.45,
                                     legumes = 0.1),
                     as.matrix(plotcompositions),
                     obj = "QH",
                     capd = T
                     )
X56 <- mix$prob
###implement new ratios to compositions
compositions <- arrange(compositions,comp)
compositions$ratio <- ratioResults
compositions[compositions$comp == 4,"ratio"] <- X4
compositions[compositions$comp == 15,"ratio"] <- X15
compositions[compositions$comp == 16,"ratio"] <- X16
compositions[compositions$comp == 40,"ratio"] <- X40
compositions[compositions$comp == 56,"ratio"] <- X56
ratioResults <- compositions$ratio
rm(X4,X15,X16,X40,X56)


## 2. Control of seedmixtures #################################################################################################

###Does a species dominate single plots?
compositions[which(compositions$ratio > 0.6),]
###Is the summed ratio of all species 1 per plot?
plotRatio <- compositions %>% 
  group_by(comp) %>% 
  summarise(sum = sum(ratio))
table(round(plotRatio$sum, 2))
plotRatio[which(plotRatio$sum < 0.995 | plotRatio$sum >1.005),]
###What is the total seed weight per plot?
plotWeights <- compositions %>% 
  group_by(comp) %>% 
  summarise(sum = sum(weight))
table(plotWeights$sum)


## How much seeds do we have to buy? ####################################################

compositions %>%
  group_by(name) %>%
  summarise(sum = sum(weight))



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# C Export ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

compositions$weight <- round(compositions$ratio * 0.48, 2)
#write.table(compositions, here("data/raw/data_raw_experiment_1_2_seed_mixtures.txt"), sep = "\t", row.names = F, quote = F)
