### Seed mixtures with Laughlin function for Experiment 1 and 2 ###



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Packages ###
library(tidyverse)
library(data.table)
library(FD)
library(Select)

### Start ###
rm(list = ls())
setwd("Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_for_restoration/data/raw")

### Load data ###
vdata <- read_table2("data_raw_experiment_1_2_traits.txt", col_names = T, na = "na", col_col_tcol_types =
                       cols(
                         name = col_factor(),
                         family = col_factor(),
                         poolD = col_factor(),
                         abs1D = col_double(),
                         pres1D = col_double(),
                         estRate1D = col_double(),
                         estRate2D = col_double(),
                         poolS = col_factor(),
                         estRate1S = col_double()
                       )        
)
### Create grass and legume variable
vdata <- mutate(vdata, grass = if_else(family == "Poaceae", 1, 0))
vdata <- mutate(vdata, legume = if_else(family == "Fabaceae", 1, 0))


## TRY data ######################################################################################################################

####txt-Dateien einlesen
tryData1 <- fread("try_sla-height-seedmass.txt", header = T, sep = "\t", dec = ".", quote = "", data.table = T)
tryData1 <- tryData1 %>%
  filter(TraitID,c(26,125,3106)) %>%
  select(AccSpeciesName,ObservationID,TraitID,TraitName,StdValue,UnitName) %>%
  group_by(TraitName,AccSpeciesName) %>%
  summarise(value = median(StdValue)) %>%
  filter(!is.na(value)) %>%
  rename(name = AccSpeciesName) %>%
  ungroup();
tryData1$name <- gsub(" ","_",tryData1$name)
tryData1$name <- sub("Centaurea_scabiosa_subsp._scabiosa","Centaurea_scabiosa",tryData1$name)
tryData1$name <- sub("Cerastium_fontanum_subsp._vulgare","Cerastium_fontanum_ssp_vulgare",tryData1$name)
tryData1$name <- sub("Dianthus_carthusianorum_subsp._carthusianorum","Dianthus_carthusianorum",tryData1$name)
tryData1$name <- sub("Leucanthemum_ircutianum","Leucanthemum_vulgare",tryData1$name)
tryData1$name <- sub("Pastinaca_sativa_subsp._sativa","Pastinaca_sativa",tryData1$name)
tryData2 <- fread("try_rootingdepth.txt", header = T, sep = "\t", dec = ".", quote = "", data.table = T)
tryData2 <- tryData2 %>%
  filter(TraitID,6) %>%
  select(AccSpeciesName,ObservationID,TraitID,TraitName,StdValue,UnitName) %>%
  group_by(TraitName,AccSpeciesName) %>%
  summarise(value = median(StdValue)) %>%
  filter(!is.na(value)) %>%
  rename(name = AccSpeciesName) %>%
  ungroup();
tryData2$name <- gsub(" ","_",tryData2$name)
tryData3 <- fread("try_srl-sfrl-myc.txt", header = T, sep = "\t", dec = ".", quote = "", data.table = T)
tryData3 <- tryData3 %>%
  filter(TraitID,c(155,614,1080,1433)) %>%
  select(AccSpeciesName,ObservationID,TraitID,TraitName,StdValue,UnitName) %>%
  group_by(TraitName,AccSpeciesName) %>%
  summarise(value = median(StdValue)) %>%
  filter(!is.na(value)) %>%
  rename(name = AccSpeciesName) %>%
  ungroup();
tryData3$name <- gsub(" ","_",tryData3$name)
tryData3$name <- sub("Cerastium_fontanum_subsp._vulgare","Cerastium_fontanum_ssp_vulgare",tryData3$name)
tryData4 <- fread("try_sla.txt", header = T, sep = "\t", dec = ".", quote = "", data.table = T)
tryData4 <- tryData4 %>%
  filter(TraitID,3115) %>%
  select(AccSpeciesName,ObservationID,TraitID,TraitName,StdValue,UnitName) %>%
  group_by(TraitName,AccSpeciesName) %>%
  summarise(value = median(StdValue)) %>%
  filter(!is.na(value)) %>%
  rename(name = AccSpeciesName) %>%
  ungroup();
tryData4$name <- gsub(" ","_",tryData4$name)
tryData4$name <- sub("Centaurea_scabiosa_subsp._scabiosa","Centaurea_scabiosa",tryData4$name)
tryData4$name <- sub("Cerastium_fontanum_subsp._vulgare","Cerastium_fontanum_ssp_vulgare",tryData4$name)
tryData4$name <- sub("Dianthus_carthusianorum_subsp._carthusianorum","Dianthus_carthusianorum",tryData4$name)
tryData4$name <- sub("Leucanthemum_ircutianum","Leucanthemum_vulgare",tryData4$name)
tryData4$name <- sub("Pastinaca_sativa_subsp._sativa","Pastinaca_sativa",tryData4$name)
####SLA data
slaData <- tryData4 %>%
  filter(TraitName == "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole excluded")%>%
  select(name, value)
vdata <- left_join(vdata,slaData,by="name")
####Seed data
seedmassData <- tryData1 %>%
  filter(TraitName == "Seed dry mass")%>%
  select(name, value)
vdata <- left_join(vdata,seedmassData,by="name")
####Height data
heightData <- tryData1 %>%
  filter(TraitName == "Plant height vegetative")%>%
  select(name, value)
vdata <- left_join(vdata,heightData,by="name")
####Finalise
vdata <- rename(vdata,slaTry = value.x, seedmassTry = value.y, heightTry = value)
####How many species with trait values
9/90#SLA: vdata$name[which(!(vdata$name %in% slaData$name))];length(which(!(vdata$name %in% slaData$name)))
72/90#Seed mass: vdata$name[which(!(vdata$name %in% seedmassData$name))];length(which(!(vdata$name %in% seedmassData$name)))
86/90#Height: vdata$name[which(!(vdata$name %in% heightData$name))];length(which(!(vdata$name %in% heightData$name)))
rm(tryData1,tryData2,rootdepthData,tryData3,mycorrhizaData,srlData,sfrlData,tryData4)


## LEDA data ######################################################################################################################

####SLA data
slaData <- read.table("leda_sla.txt",header=T, na.strings="na", dec =".")
slaData <- slaData %>%
  group_by(name) %>%
  summarise(valueLEDA = median(value))
vdata <- left_join(vdata,slaData,by="name")
####Seed data
seedmassData <- read.table("leda_seedmass.txt",header=T, na.strings="na", dec =".")
seedmassData <- seedmassData %>%
  group_by(name) %>%
  summarise(valueLEDA = median(value))
vdata <- left_join(vdata,seedmassData,by="name")
####Height data
heightData <- read.table("leda_height.txt",header=T, na.strings="na", dec =".")
heightData <- heightData %>%
  group_by(name) %>%
  summarise(valueLEDA = median(value))
vdata <- left_join(vdata,heightData,by="name")
vdata <- rename(vdata,slaLeda = valueLEDA.x, seedmassLeda = valueLEDA.y, heightLeda = valueLEDA)
rm(slaData,seedmassData,heightData)


## Merge TRY and LEDA data ######################################################################################################################

vdata <- mutate(vdata, sla = coalesce(slaLeda, slaTry))
vdata <- mutate(vdata, seedmass = coalesce(seedmassLeda, seedmassTry))
vdata <- mutate(vdata, height = coalesce(heightLeda, heightTry))
#table(is.na(vdata$sla));88/90; table(is.na(vdata$seedmass));89/90; table(is.na(vdata$height));89/90; table(is.na(vdata$srl));67/90; table(is.na(vdata$sfrl));32/90

## Calculate CWM of seedmixes and Oberdorfer --> Results in Excel ######################################################################################################################

#vdata$sla <- log(vdata$sla);vdata$seedmass <- log(vdata$seedmass);vdata$height <- log(vdata$height);vdata$srl <- log(vdata$srl);vdata$sfrl <- log(vdata$sfrl);
#speclist <- select(vdata, od, kr5050,kr3070, kr6040, rsm712);zeros <- which(rowSums(speclist)==0);zeros;speclist <- speclist[-zeros,];
#slaS <- data.frame(t(select(speclist, od, kr5050, kr3070, kr6040, rsm712)));seedmassS <- data.frame(t(select(speclist, od, kr5050, kr3070, kr6040, rsm712)));nS <- data.frame(t(select(speclist, od, kr5050, kr3070, kr6040, rsm712)));fS <- data.frame(t(select(speclist, od, kr5050, kr3070, kr6040, rsm712)));rS <- data.frame(t(select(speclist, od, kr5050, kr3070, kr6040, rsm712)));rm(speclist)
#traits <- select(vdata,name,sla,seedmass,n,f,r,grass,legume);traits <- traits[-zeros,];slaT <- select(traits,name,sla);seedmassT <- select(traits,name,seedmass);nT <- select(traits,name,n);fT <- select(traits,name,f);rT <- select(traits,name,r);
#NAs <- as.numeric(which(is.na(slaT$sla)));NAs;slaT <- slaT[-c(NAs),];slaS <- slaS[,-c(NAs)];NAs <- as.numeric(which(is.na(seedmassT$seedmass)));NAs;seedmassT <- seedmassT[-c(NAs),];seedmassS <- seedmassS[,-c(NAs)];NAs <- as.numeric(which(is.na(nT$n)));NAs;nT <- nT[-c(NAs),];nS <- nS[,-c(NAs)];NAs <- as.numeric(which(is.na(fT$f)));NAs;fT <- fT[-c(NAs),];fS <- fS[,-c(NAs)];NAs <- as.numeric(which(is.na(rT$r)));NAs;rT <- rT[-c(NAs),];rS <- rS[,-c(NAs)];
#colnames(slaS) <- slaT$name;colnames(seedmassS) <- seedmassT$name;colnames(nS) <- nT$name;colnames(fS) <- fT$name;colnames(rS) <- rT$name;
#row.names(slaT) <- slaT$name;row.names(seedmassT) <- seedmassT$name;row.names(fT) <- fT$name;row.names(nT) <- nT$name;row.names(rT) <- rT$name;
#slaT$name <- NULL;seedmassT$name <- NULL;nT$name <- NULL;fT$name <- NULL;rT$name <- NULL;
#slaC <- dbFD(slaT,slaS,corr="sqrt");slaC <- slaC$CWM;seedmassC <- dbFD(seedmassT,seedmassS,corr="sqrt");seedmassC <- seedmassC$CWM;nC <- dbFD(nT,nS,corr="sqrt");nC <- nC$CWM;fC <- dbFD(fT,fS,corr="sqrt");fC <- fC$CWM;rC <- dbFD(rT,rS,corr="sqrt");rC <- rC$CWM;rm(slaS,slaT,seedmassS,seedmassT,fS,fT,nS,nT,rS,rT)
#print(data.frame(slaC,seedmassC,fC,nC,rC))
#herbs <- vdata %>%
  #filter(!(family=="Poaceae" | family =="Fabaceae")) %>%
  #summarise(sum = sum(kr6040))
#grass <- vdata %>%
  #filter(family=="Poaceae") %>%
  #summarise(sum = sum(kr6040))
#legume <- vdata %>%
  #filter(family=="Fabaceae") %>%
  #summarise(sum = sum(kr6040))
#grass / (herbs+grass+legume) 
#legume / (herbs+grass+legume)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Make seed mixtures ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

###Taxonomic selection-----------------------------------------------------------------------------------------------------------
vdata$sla <- log(vdata$sla)
vdata$seedmass <- log(vdata$seedmass)
herbs <- filter(vdata, pool == "1" & !(family == "Poaceae" | family == "Fabaceae"))
grass <- filter(vdata, pool == "1" & family == "Poaceae")
legume <- filter(vdata, pool == "1" & family == "Fabaceae")
compData <- as.data.frame(replicate(72, {comp <- c(sample(herbs$name,12),
                                                   sample(grass$name,5),
                                                   sample(legume$name,3)
                                                   )}
                                    )
                          )
compData <- gather(compData, "comp", "name", 1:72)
table(compData$name)
length(table(compData$name))
compData <- inner_join(vdata, compData, by = "name");

###Determine abundances with the Laughlin function-------------------------------------------------------------------------------------------------------------------------
compData[c(which(is.na(compData$sla))), "sla"] <- 3.068053
compData[c(which(is.na(compData$seedmass))), "seedmass"] <- 0
compData[c(which(is.na(compData$r))), "r"] <- 6.5
compData <- select(compData, name, comp, sla, seedmass, r, grass, legume)
compData$comp <- as.numeric(gsub("V", "", compData$comp))
ratioResults <- c(0)
for (i in 1:72) { #intermediate
  plotcompData <- compData[which(compData$comp == i),]
  row.names(plotcompData) <- plotcompData[,"name"]
  plotcompData <- plotcompData[,-(1:2)]
  mix <- selectSpecies(as.matrix(plotcompData),
                       constraints = c(sla = 3.068053,
                                       seedmass = 0,
                                       r = 7,
                                       grass = 0.45,
                                       legume = 0.1),
                       as.matrix(plotcompData),
                       obj = "QH",
                       capd = T
                       )
  ratioResults <- append(ratioResults, mix$prob)
}
rm(plotcompData,mix)
compData <- arrange(compData, comp)
compData$ratio <- ratioResults


## 1. Control of seedmixtures #########################################################################################################

####Control
compData[which(compData$ratio > 0.75),]
plotRatio <- compData %>% 
  group_by(comp) %>%
  summarise(sum = sum(ratio))
table(round(plotRatio$sum, 2))
plotRatio[which(plotRatio$sum < 0.99 | plotRatio$sum >1.01),]
####correct unsolvable taxonomic composition
compData[compData$comp == 4, "name"] <- as.character(comp <- c(sample(herbs$name, 12),sample(grass$name, 5),sample(legume$name, 3)))
compData[compData$comp == 15, "name"] <- as.character(comp <- c(sample(herbs$name, 12),sample(grass$name, 5),sample(legume$name, 3)))
compData[compData$comp == 16, "name"] <- as.character(comp <- c(sample(herbs$name, 12),sample(grass$name, 5),sample(legume$name, 3)))
compData[compData$comp == 40, "name"] <- as.character(comp <- c(sample(herbs$name, 12),sample(grass$name, 5),sample(legume$name, 3)))
compData[compData$comp == 56, "name"] <- as.character(comp <- c(sample(herbs$name, 12),sample(grass$name, 5),sample(legume$name, 3)))
names <- select(compData, name, comp)
compData <- inner_join(vdata,names, by = "name")
compData[c(which(is.na(compData$sla))),"sla"] <- 3.068053
compData[c(which(is.na(compData$seedmass))),"seedmass"] <- 0
compData[c(which(is.na(compData$r))), "r"] <- 6.5
compData <- select(compData, name, comp, sla, seedmass, r, grass, legume)
####Correct wrong mixture ratios (always change comp number in first and third row)
plotcompData <- compData[which(compData$comp == 56),]
row.names(plotcompData) <- plotcompData[,"name"]
plotcompData <- plotcompData[,-(1:2)]
mix <- selectSpecies(as.matrix(plotcompData),
                     constraints = c(sla = 3.068053,
                                     seedmass = 0,
                                     r = 7,
                                     grass = 0.45,
                                     legume = 0.1),
                     as.matrix(plotcompData),
                     obj = "QH",
                     capd = T
                     )
X56 <- mix$prob
###implement new ratios to compData
compData <- arrange(compData,comp)
compData$ratio <- ratioResults
compData[compData$comp==4,"ratio"] <- X4
compData[compData$comp==15,"ratio"] <- X15
compData[compData$comp==16,"ratio"] <- X16
compData[compData$comp==40,"ratio"] <- X40
compData[compData$comp==56,"ratio"] <- X56
ratioResults <- compData$ratio;rm(X4,X15,X16,X40,X56)


## 2. control of seedmixtures #################################################################################################

####Control
compData[which(compData$ratio > 0.6),]
plotRatio <- compData %>% 
  group_by(comp) %>% 
  summarise(sum = sum(ratio))
table(round(plotRatio$sum, 2))
plotRatio[which(plotRatio$sum < 0.995 | plotRatio$sum >1.005),]


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# C Export ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

compData$weight <- round(compData$ratio * 0.48, 2)
compData %>%
  group_by(name) %>%
  summarise(sum = sum(weight))
plotWeights <- compData %>% 
  group_by(comp) %>% 
  summarise(sum = sum(weight))
table(plotWeights$sum)
write.table(compData, "composition_soil.txt")
