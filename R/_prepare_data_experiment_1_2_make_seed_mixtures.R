# Brick-based substrates and designed seedmixtures
# Seed mixtures with Laughlin function for Experiment 3 ####
# Markus Bauer
# 2022-01-24



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ##############################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### Packages ###
library(here)
library(tidyverse)
library(Select)

### Start ###
rm(list = ls())
setwd(here("data", "processed"))

### Load species list ###
traits <- read_csv("data_processed_experiment_1_2_3_traits.txt",
                   col_names = TRUE, na = "na", col_types =
                     cols(
                       .default = "d",
                       name = "f",
                       family = "f",
                       poolD = "f",
                       poolS = "f"
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



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Seed mixture creation #####################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## 1 Taxonomic composition ####################################################

compData <- as.data.frame(replicate(128, {comp <- c(sample(herbs$name, 12), 
                                               sample(grass$name, 5),
                                               sample(legume$name, 3)
                                               )})) %>%
  gather(compData, "comp", "name", 1:128)

table(compData$name)
length(table(compData$name))

compData <- traits %>%
  inner_join(compData, by = "name")


## 2 Calculate abundance values ##############################################

compData[c(which(is.na(compData$sla))), "sla"] <- 3.068053
compData[c(which(is.na(compData$seedmass))), "seedmass"] <- 0
compData[c(which(is.na(compData$r))), "r"] <- 6.5
compData <- compData %>%
  select(name, comp, sla, seedmass, r, grass, legume) %>%
  mutate(comp = as.numeric(gsub("V", "", comp)))

ratioResults <- c(0);
for (i in 1:48) { #robust
  plotcompData <- compData[which(compData$comp == i), ]
  row.names(plotcompData) <- plotcompData[,"name"]
  plotcompData <- plotcompData[, -(1:2)]
  mix <- selectSpecies(as.matrix(plotcompData),
                       constraints = c(sla = 2.995732,
                                       seedmass = 0.2231436,
                                       r = 7,
                                       grass = 0.3,
                                       legume = 0.15),
                       as.matrix(plotcompData),
                       obj = "QH",
                       capd = TRUE)
  ratioResults <- append(ratioResults, mix$prob)
}
ratioResults <- ratioResults[-1]
for (i in 49:80) { #intermediate
  plotcompData <- compData[which(compData$comp == i), ]
  row.names(plotcompData) <- plotcompData[, "name"]
  plotcompData <- plotcompData[, -(1:2)]
  mix <- selectSpecies(as.matrix(plotcompData),
                       constraints = c(sla = 3.068053,
                                       seedmass = 0,
                                       r = 7,
                                       grass = 0.45,
                                       legume = 0.1),
                       as.matrix(plotcompData),
                       obj = "QH",
                       capd = TRUE
                       )
  ratioResults <- append(ratioResults, mix$prob)
}
for (i in 81:128) { #vigorous
  plotcompData <- compData[which(compData$comp == i), ]
  row.names(plotcompData) <- plotcompData[, "name"]
  plotcompData <- plotcompData[, -(1:2)]
  mix <- selectSpecies(as.matrix(plotcompData),
                       constraints = c(sla = 3.135494,
                                       seedmass = -0.2876821,
                                       r = 7,
                                       grass = 0.6,
                                       legume = 0.05),
                       as.matrix(plotcompData),
                       obj = "QH",
                       capd = TRUE)
  ratioResults <- append(ratioResults, mix$prob)
}
rm(plotcompData,mix)
compData <- arrange(compData,comp)
compData$ratio <- ratioResults


## 3 Control of seedmixtures #################################################

### a Control of seedmixtures ------------------------------------------------
#### Control
compData[which(compData$ratio > 0.75), ]
plotRatio <- compData %>% 
  group_by(comp) %>% 
  summarise(sum = sum(ratio))
table(round(plotRatio$sum, 2))
plotRatio[which(plotRatio$sum < 0.99 | plotRatio$sum > 1.01), ]

#### Correct unsolvable taxonomic composition #####
compData[compData$comp == 7, "name"] <-
  as.character(comp <- c(sample(herbs$name, 12),
                         sample(grass$name, 5),
                         sample(legume$name, 3)))
compData[compData$comp == 25, "name"] <-
  as.character(comp <- c(sample(herbs$name, 12),
                         sample(grass$name, 5),
                         sample(legume$name, 3)))
compData[compData$comp == 71, "name"] <-
  as.character(comp <- c(sample(herbs$name, 12),
                         sample(grass$name, 5),
                         sample(legume$name, 3)))
compData[compData$comp == 82, "name"] <-
  as.character(comp <- c(sample(herbs$name, 12),
                         sample(grass$name, 5),
                         sample(legume$name, 3)))
compData[compData$comp == 94, "name"] <-
  as.character(comp <- c(sample(herbs$name, 12),
                         sample(grass$name, 5),
                         sample(legume$name, 3)))
compData[compData$comp == 103, "name"] <-
  as.character(comp <- c(sample(herbs$name, 12),
                         sample(grass$name, 5),
                         sample(legume$name, 3)))
compData[compData$comp == 104, "name"] <-
  as.character(comp <- c(sample(herbs$name, 12),
                         sample(grass$name, 5),
                         sample(legume$name, 3)))
compData[compData$comp == 106, "name"] <-
  as.character(comp <- c(sample(herbs$name, 12),
                         sample(grass$name, 5),
                         sample(legume$name, 3)))
compData[compData$comp == 112, "name"] <-
  as.character(comp <- c(sample(herbs$name, 12),
                         sample(grass$name, 5),
                         sample(legume$name, 3)))
compData[compData$comp == 113, "name"] <-
  as.character(comp <- c(sample(herbs$name, 12),
                         sample(grass$name, 5),
                         sample(legume$name, 3)))
compData[compData$comp == 117, "name"] <-
  as.character(comp <- c(sample(herbs$name, 12),
                         sample(grass$name, 5),
                         sample(legume$name, 3)))
compData[compData$comp == 126, "name"] <-
  as.character(comp <- c(sample(herbs$name, 12),
                         sample(grass$name, 5),
                         sample(legume$name, 3)))
compData[compData$comp == 127, "name"] <-
  as.character(comp <- c(sample(herbs$name, 12),
                         sample(grass$name, 5),
                         sample(legume$name, 3)))
compData[compData$comp == 128, "name"] <-
  as.character(comp <- c(sample(herbs$name, 12),
                         sample(grass$name, 5),
                         sample(legume$name, 3)))
names <- select(compData, name, comp)
compData <- inner_join(vdata, names, by = "name")
compData[c(which(is.na(compData$sla))), "sla"] <- 3.068053
compData[c(which(is.na(compData$seedmass))), "seedmass"] <- 0
compData[c(which(is.na(compData$r))), "r"] <- 6.5
compData <- select(compData, name, comp, sla, seedmass, r, grass, legume)

#### Correct wrong mixture ratios ####
plotcompData <- compData[which(compData$comp == 128), ]
row.names(plotcompData) <- plotcompData[, "name"]
plotcompData <- plotcompData[, -(1:2)]
mix <- selectSpecies(as.matrix(plotcompData),
                     constraints = c(sla = 3.135494,
                                     seedmass = -0.2876821,
                                     r = 7,
                                     grass = 0.6,
                                     legume = 0.05),
                     as.matrix(plotcompData),
                     obj = "QH",
                     capd = TRUE)
X128 <- mix$prob

#### Implement new ratios to compData ####
compData <- arrange(compData,comp)
compData$ratio <- ratioResults
compData[compData$comp == 7, "ratio"] <- X7
compData[compData$comp == 25, "ratio"] <- X25
compData[compData$comp == 71, "ratio"] <- X71
compData[compData$comp == 94, "ratio"] <- X94
compData[compData$comp == 103, "ratio"] <- X103
compData[compData$comp == 104, "ratio"] <- X104
compData[compData$comp == 106, "ratio"] <- X106
compData[compData$comp == 112, "ratio"] <- X112
compData[compData$comp == 117, "ratio"] <- X117
compData[compData$comp == 126, "ratio"] <- X126
compData[compData$comp == 127, "ratio"] <- X127
compData[compData$comp == 128, "ratio"] <- X128
ratioResults <- compData$ratio
rm(X7,X25,X71,X94,X103,X104,X106,X112,X117,X126,X127,X128)

### b Control of seedmixtures ------------------------------------------------
####Control
compData[which(compData$ratio > 0.6), ]
plotRatio <- compData %>% 
  group_by(comp) %>% 
  summarise(sum = sum(ratio))
table(round(plotRatio$sum, 2))
plotRatio[which(plotRatio$sum < 0.995 | plotRatio$sum > 1.005), ]

#### Correct unsolvable taxonomic composition ####
compData[compData$comp == 48, "name"] <-
  as.character(comp <- c(sample(herbs$name,12),
                         sample(grass$name,5),
                         sample(legume$name,3)))
compData[compData$comp == 82, "name"] <-
  as.character(comp <- c(sample(herbs$name,12),
                         sample(grass$name,5),
                         sample(legume$name,3)))
compData[compData$comp == 83, "name"] <-
  as.character(comp <- c(sample(herbs$name,12),
                         sample(grass$name,5),
                         sample(legume$name,3)))
compData[compData$comp == 113, "name"] <-
  as.character(comp <- c(sample(herbs$name,12),
                         sample(grass$name,5),
                         sample(legume$name,3)))
names <- select(compData, name, comp)
compData <- inner_join(vdata, names, by = "name")
compData[c(which(is.na(compData$sla))), "sla"] <- 3.068053
compData[c(which(is.na(compData$seedmass))), "seedmass"] <- 0
compData[c(which(is.na(compData$r))), "r"] <- 6.5
compData <- select(compData, name, comp, sla, seedmass, r, grass, legume)

#### Correct wrong mixture ratios ####
plotcompData <- compData[which(compData$comp == 48), ]
row.names(plotcompData) <- plotcompData[, "name"]
plotcompData <- plotcompData[, -(1:2)]
mix <- selectSpecies(as.matrix(plotcompData),
                     constraints = c(sla = 2.995732,
                                     seedmass = 0.2231436,
                                     r = 7,
                                     grass = 0.3,
                                     legume = 0.15),
                     as.matrix(plotcompData),
                     obj = "QH",
                     capd = TRUE)
X48 <- mix$prob

#### Implement new ratios to compData ####
compData <- arrange(compData,comp)
compData$ratio <- ratioResults
compData[compData$comp == 82, "ratio"] <- X82
compData[compData$comp == 83, "ratio"] <- X83
compData[compData$comp == 113, "ratio"] <- X113
ratioResults <- compData$ratio
rm(X82,X83,X113)

### c Control of seedmixtures -------------------------------------------------
#### Control
compData[which(compData$ratio > 0.4), ]
plotRatio <- compData %>% 
  group_by(comp) %>% 
  summarise(sum = sum(ratio))
table(round(plotRatio$sum, 2))
plotRatio[which(plotRatio$sum < 0.995 | plotRatio$sum > 1.005), ]

#### Correct unsolvable taxonomic composition ####
compData[compData$comp == 48, "name"] <-
  as.character(comp <- c(sample(herbs$name, 12),
                         sample(grass$name, 5),
                         sample(legume$name, 3)))
names <- select(compData, name, comp)
compData <- inner_join(vdata, names, by = "name")
compData[c(which(is.na(compData$sla))), "sla"] <- 3.068053
compData[c(which(is.na(compData$seedmass))), "seedmass"] <- 0
compData[c(which(is.na(compData$r))), "r"] <- 6.5
compData <- select(compData, name, comp, sla, seedmass, r, grass, legume)

#### Correct wrong mixture ratios ####
plotcompData <- compData[which(compData$comp == 48), ]
row.names(plotcompData) <- plotcompData[, "name"]
plotcompData <- plotcompData[, -(1:2)]
mix <- selectSpecies(as.matrix(plotcompData),
                     constraints = c(sla = 2.995732,
                                     seedmass = 0.2231436,
                                     r = 7,
                                     grass = 0.3,
                                     legume = 0.15),
                     as.matrix(plotcompData),
                     obj = "QH",
                     capd = TRUE)
X48 <- mix$prob

#### Implement new ratios to compData ####
compData <- arrange(compData,comp)
compData$ratio <- ratioResults
compData[compData$comp == 48, "ratio"] <- X48
ratioResults <- compData$ratio
rm(X48)

### d Control of seedmixtures -------------------------------------------------
#### Control
compData[which(compData$ratio > 0.5), ]
plotRatio <- compData %>% 
  group_by(comp) %>% 
  summarise(sum = sum(ratio))
table(round(plotRatio$sum, 2))
plotRatio[which(plotRatio$sum < 0.995 | plotRatio$sum > 1.005), ]



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## C Export ##################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

####Assign right composition numbers
compData <- read.table("composition.txt", header = TRUE, na.strings = "na",
                       dec = ".")
compData[compData$comp == 1, "comp"] <- "x5"
compData[compData$comp == 2, "comp"] <- "x6"
compData[compData$comp == 3, "comp"] <- "x7"
compData[compData$comp == 4, "comp"] <- "x8"
compData[compData$comp == 5, "comp"] <- "x17"
compData[compData$comp == 6, "comp"] <- "x18"
compData[compData$comp == 7, "comp"] <- "x25"
compData[compData$comp == 8, "comp"] <- "x26"
compData[compData$comp == 9, "comp"] <- "x27"
compData[compData$comp == 10, "comp"] <- "x28"
compData[compData$comp == 11, "comp"] <- "x37"
compData[compData$comp == 12, "comp"] <- "x38"
compData[compData$comp == 13, "comp"] <- "x45"
compData[compData$comp == 14, "comp"] <- "x46"
compData[compData$comp == 15, "comp"] <- "x47"
compData[compData$comp == 16, "comp"] <- "x48"
compData[compData$comp == 17, "comp"] <- "x57"
compData[compData$comp == 18, "comp"] <- "x58"
compData[compData$comp == 19, "comp"] <- "x65"
compData[compData$comp == 20, "comp"] <- "x66"
compData[compData$comp == 21, "comp"] <- "x67"
compData[compData$comp == 22, "comp"] <- "x68"
compData[compData$comp == 23, "comp"] <- "x77"
compData[compData$comp == 24, "comp"] <- "x78"
compData[compData$comp == 25, "comp"] <- "x85"
compData[compData$comp == 26, "comp"] <- "x86"
compData[compData$comp == 27, "comp"] <- "x87"
compData[compData$comp == 28, "comp"] <- "x88"
compData[compData$comp == 29, "comp"] <- "x97"
compData[compData$comp == 30, "comp"] <- "x98"
compData[compData$comp == 31, "comp"] <- "x105"
compData[compData$comp == 32, "comp"] <- "x106"
compData[compData$comp == 33, "comp"] <- "x107"
compData[compData$comp == 34, "comp"] <- "x108"
compData[compData$comp == 35, "comp"] <- "x117"
compData[compData$comp == 36, "comp"] <- "x118"
compData[compData$comp == 37, "comp"] <- "x125"
compData[compData$comp == 38, "comp"] <- "x126"
compData[compData$comp == 39, "comp"] <- "x127"
compData[compData$comp == 40, "comp"] <- "x128"
compData[compData$comp == 41, "comp"] <- "x137"
compData[compData$comp == 42, "comp"] <- "x138"
compData[compData$comp == 43, "comp"] <- "x145"
compData[compData$comp == 44, "comp"] <- "x146"
compData[compData$comp == 45, "comp"] <- "x147"
compData[compData$comp == 46, "comp"] <- "x148"
compData[compData$comp == 47, "comp"] <- "x157"
compData[compData$comp == 48, "comp"] <- "x158"
compData[compData$comp == 49, "comp"] <- "x9"
compData[compData$comp == 50, "comp"] <- "x10"
compData[compData$comp == 51, "comp"] <- "x11"
compData[compData$comp == 52, "comp"] <- "x12"
compData[compData$comp == 53, "comp"] <- "x29"
compData[compData$comp == 54, "comp"] <- "x30"
compData[compData$comp == 55, "comp"] <- "x31"
compData[compData$comp == 56, "comp"] <- "x32"
compData[compData$comp == 57, "comp"] <- "x49"
compData[compData$comp == 58, "comp"] <- "x50"
compData[compData$comp == 59, "comp"] <- "x51"
compData[compData$comp == 60, "comp"] <- "x52"
compData[compData$comp == 61, "comp"] <- "x69"
compData[compData$comp == 62, "comp"] <- "x70"
compData[compData$comp == 63, "comp"] <- "x71"
compData[compData$comp == 64, "comp"] <- "x72"
compData[compData$comp == 65, "comp"] <- "x89"
compData[compData$comp == 66, "comp"] <- "x90"
compData[compData$comp == 67, "comp"] <- "x91"
compData[compData$comp == 68, "comp"] <- "x92"
compData[compData$comp == 69, "comp"] <- "x109"
compData[compData$comp == 70, "comp"] <- "x110"
compData[compData$comp == 71, "comp"] <- "x111"
compData[compData$comp == 72, "comp"] <- "x112"
compData[compData$comp == 73, "comp"] <- "x129"
compData[compData$comp == 74, "comp"] <- "x130"
compData[compData$comp == 75, "comp"] <- "x131"
compData[compData$comp == 76, "comp"] <- "x132"
compData[compData$comp == 77, "comp"] <- "x149"
compData[compData$comp == 78, "comp"] <- "x150"
compData[compData$comp == 79, "comp"] <- "x151"
compData[compData$comp == 80, "comp"] <- "x152"
compData[compData$comp == 81, "comp"] <- "x13"
compData[compData$comp == 82, "comp"] <- "x14"
compData[compData$comp == 83, "comp"] <- "x15"
compData[compData$comp == 84, "comp"] <- "x16"
compData[compData$comp == 85, "comp"] <- "x19"
compData[compData$comp == 86, "comp"] <- "x20"
compData[compData$comp == 87, "comp"] <- "x33"
compData[compData$comp == 88, "comp"] <- "x34"
compData[compData$comp == 89, "comp"] <- "x35"
compData[compData$comp == 90, "comp"] <- "x36"
compData[compData$comp == 91, "comp"] <- "x39"
compData[compData$comp == 92, "comp"] <- "x40"
compData[compData$comp == 93, "comp"] <- "x53"
compData[compData$comp == 94, "comp"] <- "x54"
compData[compData$comp == 95, "comp"] <- "x55"
compData[compData$comp == 96, "comp"] <- "x56"
compData[compData$comp == 97, "comp"] <- "x59"
compData[compData$comp == 98, "comp"] <- "x60"
compData[compData$comp == 99, "comp"] <- "x73"
compData[compData$comp == 100, "comp"] <- "x74"
compData[compData$comp == 101, "comp"] <- "x75"
compData[compData$comp == 102, "comp"] <- "x76"
compData[compData$comp == 103, "comp"] <- "x79"
compData[compData$comp == 104, "comp"] <- "x80"
compData[compData$comp == 105, "comp"] <- "x93"
compData[compData$comp == 106, "comp"] <- "x94"
compData[compData$comp == 107, "comp"] <- "x95"
compData[compData$comp == 108, "comp"] <- "x96"
compData[compData$comp == 109, "comp"] <- "x99"
compData[compData$comp == 110, "comp"] <- "x100"
compData[compData$comp == 111, "comp"] <- "x113"
compData[compData$comp == 112, "comp"] <- "x114"
compData[compData$comp == 113, "comp"] <- "x115"
compData[compData$comp == 114, "comp"] <- "x116"
compData[compData$comp == 115, "comp"] <- "x119"
compData[compData$comp == 116, "comp"] <- "x120"
compData[compData$comp == 117, "comp"] <- "x133"
compData[compData$comp == 118, "comp"] <- "x134"
compData[compData$comp == 119, "comp"] <- "x135"
compData[compData$comp == 120, "comp"] <- "x136"
compData[compData$comp == 121, "comp"] <- "x139"
compData[compData$comp == 122, "comp"] <- "x140"
compData[compData$comp == 123, "comp"] <- "x153"
compData[compData$comp == 124, "comp"] <- "x154"
compData[compData$comp == 125, "comp"] <- "x155"
compData[compData$comp == 126, "comp"] <- "x156"
compData[compData$comp == 127, "comp"] <- "x159"
compData[compData$comp == 128, "comp"] <- "x160"
#### Export
compData$weight <- round(compData$ratio * 0.48, 2)
(weights <- compData %>%  
  group_by(name) %>%
  summarise(sum = sum(weight)))
plotWeights <- compData %>% 
  group_by(comp) %>% 
  summarise(sum = sum(weight))
table(plotWeights$sum)

write_csv(compData,
          here("data", "raw",
               "data_raw_experiment_3_compositions.csv"))
