# Brick-based substrates and designed seedmixtures
# Seed mixtures with Laughlin function for Experiment 3 ####
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
library(FD)
library(data.table)

### Start ###
rm(list = ls())
setwd(here("data/processed"))
#library(installr);updateR(browse_news=F, install_R=T, copy_packages = T,copy_Rprofile.site = T,keep_old_packages = T, update_packages = T)

### Load data ###
vdata <- read_table("speciespool.txt", col_names = T, na = "na")
vdata$name <- sub("Rhinanthus_glacialis/minor", "Rhinanthus_minor", vdata$name)
vdata <- vdata %>%
  mutate(grass = if_else(family == "Poaceae", 1, 0),
         legume = if_else(family == "Fabaceae", 1, 0)
         )

## 1. TRY data ######################################################################################################################

####txt-Dateien einlesen
tryData1 <- fread("try_sla-height-seedmass.txt", header = T, sep = "\t", dec = ".", quote = "", data.table = T)
tryData1 <- tryData1 %>%
  filter(TraitID,c(26,125,3106)) %>%
  select(AccSpeciesName, ObservationID,TraitID, TraitName, StdValue, UnitName) %>%
  group_by(TraitName, AccSpeciesName) %>%
  summarise(value = median(StdValue)) %>%
  filter(!is.na(value)) %>%
  rename(name = AccSpeciesName) %>%
  ungroup();
tryData1$name <- gsub(" ", "_", tryData1$name)
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
  ungroup()
tryData2$name <- gsub(" ", "_", tryData2$name)
tryData3 <- fread("try_srl-sfrl-myc.txt", header = T, sep = "\t", dec = ".", quote = "", data.table = T)
tryData3 <- tryData3 %>%
  filter(TraitID,c(155,614,1080,1433)) %>%
  select(AccSpeciesName,ObservationID,TraitID,TraitName,StdValue,UnitName) %>%
  group_by(TraitName,AccSpeciesName) %>%
  summarise(value = median(StdValue)) %>%
  filter(!is.na(value)) %>%
  rename(name = AccSpeciesName) %>%
  ungroup();
tryData3$name <- gsub(" ", "_", tryData3$name)
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
tryData4$name <- gsub(" ", "_", tryData4$name)
tryData4$name <- sub("Centaurea_scabiosa_subsp._scabiosa", "Centaurea_scabiosa", tryData4$name)
tryData4$name <- sub("Cerastium_fontanum_subsp._vulgare", "Cerastium_fontanum_ssp_vulgare", tryData4$name)
tryData4$name <- sub("Dianthus_carthusianorum_subsp._carthusianorum" ,"Dianthus_carthusianorum", tryData4$name)
tryData4$name <- sub("Leucanthemum_ircutianum", "Leucanthemum_vulgare", tryData4$name)
tryData4$name <- sub("Pastinaca_sativa_subsp._sativa", "Pastinaca_sativa", tryData4$name)
####SLA data
slaData <- tryData4 %>%
  filter(TraitName == "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole excluded")%>%
  select(name, value)
9/90#vdata$name[which(!(vdata$name %in% slaData$name))];length(which(!(vdata$name %in% slaData$name)))
vdata <- left_join(vdata,slaData,by="name")
####Seed data
seedmassData <- tryData1 %>%
  filter(TraitName == "Seed dry mass")%>%
  select(name, value)
72/90#vdata$name[which(!(vdata$name %in% seedmassData$name))];length(which(!(vdata$name %in% seedmassData$name)))
vdata <- left_join(vdata,seedmassData,by="name")
####Height data
heightData <- tryData1 %>%
  filter(TraitName == "Plant height vegetative")%>%
  select(name, value)
86/90#vdata$name[which(!(vdata$name %in% heightData$name))];length(which(!(vdata$name %in% heightData$name)))
####Finalise
vdata <- left_join(vdata,heightData,by="name") %>%
  rename(slaTry = value.x, seedmassTry = value.y, heightTry = value.x.x, rootdepthTry = value.y.y, srlTry = value.x.x.x, sfrlTry = value.y.y.y, mycTry = value)
rm(tryData1,tryData2,rootdepthData,tryData3,mycorrhizaData,srlData,sfrlData,tryData4)

## 2. LEDA data ######################################################################################################################

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
vdata <- left_join(vdata, seedmassData,by="name")
####Height data
heightData <- read.table("leda_height.txt",header=T, na.strings="na", dec =".")
heightData <- heightData %>%
  group_by(name) %>%
  summarise(valueLEDA = median(value))
vdata <- left_join(vdata, heightData,by="name") %>%
  rename(slaLeda = valueLEDA.x, seedmassLeda = valueLEDA.y, heightLeda = valueLEDA)
rm(slaData,seedmassData,heightData)

## 3. Merge data ###########################################################################################
vdata <- mutate(vdata, 
                sla = coalesce(slaLeda, slaTry),
                seedmass = coalesce(seedmassLeda, seedmassTry),
                height = coalesce(heightLeda, heightTry)
                )
#table(is.na(vdata$sla));88/90; table(is.na(vdata$seedmass));89/90; table(is.na(vdata$height));89/90; table(is.na(vdata$srl));67/90; table(is.na(vdata$sfrl));32/90

## 4. Calculate CWM of seedmixes and Oberdorfer --> Results in Excel ######################################################################################################################

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



# B Seed mixture creation ######################################################################################################################


## 1. Taxonomic composition #################################################################################

vdata$sla <- log(vdata$sla)
vdata$seedmass <- log(vdata$seedmass)
herbs <- filter(vdata, pool == "1" & !(family == "Poaceae" | family == "Fabaceae"))
grass <- filter(vdata, pool == "1" & family == "Poaceae")
legume <- filter(vdata, pool == "1" & family == "Fabaceae")
compData <- as.data.frame(replicate(128,{comp <- c(sample(herbs$name,12), 
                                               sample(grass$name,5),
                                               sample(legume$name,3)
                                               )}
                                    )
                          )
compData <- gather(compData, "comp", "name", 1:128);
table(compData$name)
length(table(compData$name))
compData <- inner_join(vdata, compData, by = "name")


## 2. Calculate abundance values ############################################################################################################

compData[c(which(is.na(compData$sla))), "sla"] <- 3.068053;
compData[c(which(is.na(compData$seedmass))), "seedmass"] <- 0
compData[c(which(is.na(compData$r))), "r"] <- 6.5
compData <- select(compData, name, comp, sla, seedmass, r, grass, legume)
compData$comp <- as.numeric(gsub("V", "", compData$comp))

ratioResults <- c(0);
for (i in 1:48) { #robust
  plotcompData <- compData[which(compData$comp==i),]
  row.names(plotcompData) <- plotcompData[,"name"]
  plotcompData <- plotcompData[,-(1:2)]
  mix <- selectSpecies(as.matrix(plotcompData),
                       constraints = c(sla = 2.995732,
                                       seedmass = 0.2231436,
                                       r = 7,
                                       grass = 0.3,
                                       legume = 0.15),
                       as.matrix(plotcompData),
                       obj = "QH",
                       capd = T
                       )
  ratioResults <- append(ratioResults, mix$prob)
};ratioResults <- ratioResults[-1];
for (i in 49:80) { #intermediate
  plotcompData <- compData[which(compData$comp==i),]
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
for (i in 81:128) { #vigorous
  plotcompData <- compData[which(compData$comp==i),]
  row.names(plotcompData) <- plotcompData[,"name"]
  plotcompData <- plotcompData[,-(1:2)]
  mix <- selectSpecies(as.matrix(plotcompData),
                       constraints = c(sla = 3.135494,
                                       seedmass = -0.2876821,
                                       r = 7,
                                       grass = 0.6,
                                       legume = 0.05),
                       as.matrix(plotcompData),
                       obj = "QH",
                       capd = T)
  ratioResults <- append(ratioResults, mix$prob)
};rm(plotcompData,mix)
compData <- arrange(compData,comp);compData$ratio <- ratioResults;


## 3. Control of seedmixtures #########################################################################################################

### 3.1 Control of seedmixtures --------------------------------------------------------------
####Control
compData[which(compData$ratio > 0.75),];
plotRatio <- compData %>% 
  group_by(comp) %>% 
  summarise(sum = sum(ratio))
table(round(plotRatio$sum,2))
plotRatio[which(plotRatio$sum < 0.99 | plotRatio$sum >1.01),]
#### a Correct unsolvable taxonomic composition #####
compData[compData$comp==7,"name"] <- as.character(comp <- c(sample(herbs$name,12),sample(grass$name,5),sample(legume$name,3)))
compData[compData$comp==25,"name"] <- as.character(comp <- c(sample(herbs$name,12),sample(grass$name,5),sample(legume$name,3)))
compData[compData$comp==71,"name"] <- as.character(comp <- c(sample(herbs$name,12),sample(grass$name,5),sample(legume$name,3)))
compData[compData$comp==82,"name"] <- as.character(comp <- c(sample(herbs$name,12),sample(grass$name,5),sample(legume$name,3)))
compData[compData$comp==94,"name"] <- as.character(comp <- c(sample(herbs$name,12),sample(grass$name,5),sample(legume$name,3)))
compData[compData$comp==103,"name"] <- as.character(comp <- c(sample(herbs$name,12),sample(grass$name,5),sample(legume$name,3)))
compData[compData$comp==104,"name"] <- as.character(comp <- c(sample(herbs$name,12),sample(grass$name,5),sample(legume$name,3)))
compData[compData$comp==106,"name"] <- as.character(comp <- c(sample(herbs$name,12),sample(grass$name,5),sample(legume$name,3)))
compData[compData$comp==112,"name"] <- as.character(comp <- c(sample(herbs$name,12),sample(grass$name,5),sample(legume$name,3)))
compData[compData$comp==113,"name"] <- as.character(comp <- c(sample(herbs$name,12),sample(grass$name,5),sample(legume$name,3)))
compData[compData$comp==117,"name"] <- as.character(comp <- c(sample(herbs$name,12),sample(grass$name,5),sample(legume$name,3)))
compData[compData$comp==126,"name"] <- as.character(comp <- c(sample(herbs$name,12),sample(grass$name,5),sample(legume$name,3)))
compData[compData$comp==127,"name"] <- as.character(comp <- c(sample(herbs$name,12),sample(grass$name,5),sample(legume$name,3)))
compData[compData$comp==128,"name"] <- as.character(comp <- c(sample(herbs$name,12),sample(grass$name,5),sample(legume$name,3)))
names <- select(compData, name,comp)
compData <- inner_join(vdata,names, by = "name")
compData[c(which(is.na(compData$sla))),"sla"] <- 3.068053
compData[c(which(is.na(compData$seedmass))),"seedmass"] <- 0
compData[c(which(is.na(compData$r))),"r"] <- 6.5
compData <- select(compData, name, comp, sla, seedmass, r, grass, legume)
####b Correct wrong mixture ratios ####
plotcompData <- compData[which(compData$comp==128),]
row.names(plotcompData) <- plotcompData[,"name"]
plotcompData <- plotcompData[,-(1:2)]
mix <- selectSpecies(as.matrix(plotcompData),
                     constraints = c(sla = 3.135494,
                                     seedmass = -0.2876821,
                                     r = 7,
                                     grass = 0.6,
                                     legume = 0.05),
                     as.matrix(plotcompData),
                     obj = "QH",
                     capd = T)
X128 <- mix$prob;
#### c Implement new ratios to compData ####
compData <- arrange(compData,comp);compData$ratio <- ratioResults
compData[compData$comp==7,"ratio"] <- X7
compData[compData$comp==25,"ratio"] <- X25
compData[compData$comp==71,"ratio"] <- X71
compData[compData$comp==94,"ratio"] <- X94
compData[compData$comp==103,"ratio"] <- X103
compData[compData$comp==104,"ratio"] <- X104
compData[compData$comp==106,"ratio"] <- X106
compData[compData$comp==112,"ratio"] <- X112
compData[compData$comp==117,"ratio"] <- X117
compData[compData$comp==126,"ratio"] <- X126
compData[compData$comp==127,"ratio"] <- X127
compData[compData$comp==128,"ratio"] <- X128
ratioResults <- compData$ratio;rm(X7,X25,X71,X94,X103,X104,X106,X112,X117,X126,X127,X128)

### 3.2 Control of seedmixtures --------------------------------------------------------------
####Control
compData[which(compData$ratio > 0.6),]
plotRatio <- compData %>% 
  group_by(comp) %>% 
  summarise(sum = sum(ratio))
table(round(plotRatio$sum,2))
plotRatio[which(plotRatio$sum < 0.995 | plotRatio$sum >1.005),]
#### a Correct unsolvable taxonomic composition ####
compData[compData$comp==48,"name"] <- as.character(comp <- c(sample(herbs$name,12),sample(grass$name,5),sample(legume$name,3)))
compData[compData$comp==82,"name"] <- as.character(comp <- c(sample(herbs$name,12),sample(grass$name,5),sample(legume$name,3)))
compData[compData$comp==83,"name"] <- as.character(comp <- c(sample(herbs$name,12),sample(grass$name,5),sample(legume$name,3)))
compData[compData$comp==113,"name"] <- as.character(comp <- c(sample(herbs$name,12),sample(grass$name,5),sample(legume$name,3)))
names <- select(compData, name,comp)
compData <- inner_join(vdata,names, by = "name")
compData[c(which(is.na(compData$sla))),"sla"] <- 3.068053
compData[c(which(is.na(compData$seedmass))),"seedmass"] <- 0
compData[c(which(is.na(compData$r))),"r"] <- 6.5
compData <- select(compData, name, comp, sla, seedmass, r, grass, legume)
#### b Correct wrong mixture ratios ####
plotcompData <- compData[which(compData$comp==48),]
row.names(plotcompData) <- plotcompData[,"name"]
plotcompData <- plotcompData[,-(1:2)]
mix <- selectSpecies(as.matrix(plotcompData),
                     constraints = c(sla=2.995732,
                                     seedmass=0.2231436,
                                     r=7,
                                     grass=0.3,
                                     legume=0.15),
                     as.matrix(plotcompData),
                     obj="QH",
                     capd=T)
X48 <- mix$prob;
#### Implement new ratios to compData ####
compData <- arrange(compData,comp)
compData$ratio <- ratioResults
compData[compData$comp==82,"ratio"] <- X82
compData[compData$comp==83,"ratio"] <- X83
compData[compData$comp==113,"ratio"] <- X113
ratioResults <- compData$ratio
rm(X82,X83,X113)

### 3.3 Control of seedmixtures --------------------------------------------------------------
####Control
compData[which(compData$ratio > 0.4),]
plotRatio <- compData %>% 
  group_by(comp) %>% 
  summarise(sum = sum(ratio))
table(round(plotRatio$sum,2))
plotRatio[which(plotRatio$sum < 0.995 | plotRatio$sum >1.005),]
#### a Correct unsolvable taxonomic composition ####
compData[compData$comp==48,"name"] <- as.character(comp <- c(sample(herbs$name,12),sample(grass$name,5),sample(legume$name,3)))
names <- select(compData, name,comp)
compData <- inner_join(vdata,names, by = "name")
compData[c(which(is.na(compData$sla))),"sla"] <- 3.068053
compData[c(which(is.na(compData$seedmass))),"seedmass"] <- 0
compData[c(which(is.na(compData$r))),"r"] <- 6.5
compData <- select(compData, name, comp, sla, seedmass, r, grass, legume)
#### b Correct wrong mixture ratios ####
plotcompData <- compData[which(compData$comp==48),]
row.names(plotcompData) <- plotcompData[,"name"]
plotcompData <- plotcompData[,-(1:2)]
mix <- selectSpecies(as.matrix(plotcompData),
                     constraints = c(sla=2.995732,
                                     seedmass=0.2231436,
                                     r=7,
                                     grass=0.3,
                                     legume=0.15),
                     as.matrix(plotcompData),
                     obj="QH",
                     capd=T)
X48 <- mix$prob;
#### c Implement new ratios to compData ####
compData <- arrange(compData,comp)
compData$ratio <- ratioResults
compData[compData$comp==48,"ratio"] <- X48
ratioResults <- compData$ratio;rm(X48)

### 3.4 Control of seedmixtures --------------------------------------------------------------
####Control
compData[which(compData$ratio > 0.5),]
plotRatio <- compData %>% 
  group_by(comp) %>% 
  summarise(sum = sum(ratio))
table(round(plotRatio$sum,2))
plotRatio[which(plotRatio$sum < 0.995 | plotRatio$sum >1.005),]



## C Export #####################################################################################################################


####Assign right composition numbers
compData <- read.table("composition.txt", header = T, na.strings = "na", dec = ".")
compData[compData$comp==1,"comp"] <- "x5"
compData[compData$comp==2,"comp"] <- "x6"
compData[compData$comp==3,"comp"] <- "x7"
compData[compData$comp==4,"comp"] <- "x8"
compData[compData$comp==5,"comp"] <- "x17"
compData[compData$comp==6,"comp"] <- "x18"
compData[compData$comp==7,"comp"] <- "x25"
compData[compData$comp==8,"comp"] <- "x26"
compData[compData$comp==9,"comp"] <- "x27"
compData[compData$comp==10,"comp"] <- "x28"
compData[compData$comp==11,"comp"] <- "x37"
compData[compData$comp==12,"comp"] <- "x38"
compData[compData$comp==13,"comp"] <- "x45"
compData[compData$comp==14,"comp"] <- "x46"
compData[compData$comp==15,"comp"] <- "x47"
compData[compData$comp==16,"comp"] <- "x48"
compData[compData$comp==17,"comp"] <- "x57"
compData[compData$comp==18,"comp"] <- "x58"
compData[compData$comp==19,"comp"] <- "x65"
compData[compData$comp==20,"comp"] <- "x66"
compData[compData$comp==21,"comp"] <- "x67"
compData[compData$comp==22,"comp"] <- "x68"
compData[compData$comp==23,"comp"] <- "x77"
compData[compData$comp==24,"comp"] <- "x78"
compData[compData$comp==25,"comp"] <- "x85"
compData[compData$comp==26,"comp"] <- "x86"
compData[compData$comp==27,"comp"] <- "x87"
compData[compData$comp==28,"comp"] <- "x88"
compData[compData$comp==29,"comp"] <- "x97"
compData[compData$comp==30,"comp"] <- "x98"
compData[compData$comp==31,"comp"] <- "x105"
compData[compData$comp==32,"comp"] <- "x106"
compData[compData$comp==33,"comp"] <- "x107"
compData[compData$comp==34,"comp"] <- "x108"
compData[compData$comp==35,"comp"] <- "x117"
compData[compData$comp==36,"comp"] <- "x118"
compData[compData$comp==37,"comp"] <- "x125"
compData[compData$comp==38,"comp"] <- "x126"
compData[compData$comp==39,"comp"] <- "x127"
compData[compData$comp==40,"comp"] <- "x128"
compData[compData$comp==41,"comp"] <- "x137"
compData[compData$comp==42,"comp"] <- "x138"
compData[compData$comp==43,"comp"] <- "x145"
compData[compData$comp==44,"comp"] <- "x146"
compData[compData$comp==45,"comp"] <- "x147"
compData[compData$comp==46,"comp"] <- "x148"
compData[compData$comp==47,"comp"] <- "x157"
compData[compData$comp==48,"comp"] <- "x158"
compData[compData$comp==49,"comp"] <- "x9"
compData[compData$comp==50,"comp"] <- "x10"
compData[compData$comp==51,"comp"] <- "x11"
compData[compData$comp==52,"comp"] <- "x12"
compData[compData$comp==53,"comp"] <- "x29"
compData[compData$comp==54,"comp"] <- "x30"
compData[compData$comp==55,"comp"] <- "x31"
compData[compData$comp==56,"comp"] <- "x32"
compData[compData$comp==57,"comp"] <- "x49"
compData[compData$comp==58,"comp"] <- "x50"
compData[compData$comp==59,"comp"] <- "x51"
compData[compData$comp==60,"comp"] <- "x52"
compData[compData$comp==61,"comp"] <- "x69"
compData[compData$comp==62,"comp"] <- "x70"
compData[compData$comp==63,"comp"] <- "x71"
compData[compData$comp==64,"comp"] <- "x72"
compData[compData$comp==65,"comp"] <- "x89"
compData[compData$comp==66,"comp"] <- "x90"
compData[compData$comp==67,"comp"] <- "x91"
compData[compData$comp==68,"comp"] <- "x92"
compData[compData$comp==69,"comp"] <- "x109"
compData[compData$comp==70,"comp"] <- "x110"
compData[compData$comp==71,"comp"] <- "x111"
compData[compData$comp==72,"comp"] <- "x112"
compData[compData$comp==73,"comp"] <- "x129"
compData[compData$comp==74,"comp"] <- "x130"
compData[compData$comp==75,"comp"] <- "x131"
compData[compData$comp==76,"comp"] <- "x132"
compData[compData$comp==77,"comp"] <- "x149"
compData[compData$comp==78,"comp"] <- "x150"
compData[compData$comp==79,"comp"] <- "x151"
compData[compData$comp==80,"comp"] <- "x152"
compData[compData$comp==81,"comp"] <- "x13"
compData[compData$comp==82,"comp"] <- "x14"
compData[compData$comp==83,"comp"] <- "x15"
compData[compData$comp==84,"comp"] <- "x16"
compData[compData$comp==85,"comp"] <- "x19"
compData[compData$comp==86,"comp"] <- "x20"
compData[compData$comp==87,"comp"] <- "x33"
compData[compData$comp==88,"comp"] <- "x34"
compData[compData$comp==89,"comp"] <- "x35"
compData[compData$comp==90,"comp"] <- "x36"
compData[compData$comp==91,"comp"] <- "x39"
compData[compData$comp==92,"comp"] <- "x40"
compData[compData$comp==93,"comp"] <- "x53"
compData[compData$comp==94,"comp"] <- "x54"
compData[compData$comp==95,"comp"] <- "x55"
compData[compData$comp==96,"comp"] <- "x56"
compData[compData$comp==97,"comp"] <- "x59"
compData[compData$comp==98,"comp"] <- "x60"
compData[compData$comp==99,"comp"] <- "x73"
compData[compData$comp==100,"comp"] <- "x74"
compData[compData$comp==101,"comp"] <- "x75"
compData[compData$comp==102,"comp"] <- "x76"
compData[compData$comp==103,"comp"] <- "x79"
compData[compData$comp==104,"comp"] <- "x80"
compData[compData$comp==105,"comp"] <- "x93"
compData[compData$comp==106,"comp"] <- "x94"
compData[compData$comp==107,"comp"] <- "x95"
compData[compData$comp==108,"comp"] <- "x96"
compData[compData$comp==109,"comp"] <- "x99"
compData[compData$comp==110,"comp"] <- "x100"
compData[compData$comp==111,"comp"] <- "x113"
compData[compData$comp==112,"comp"] <- "x114"
compData[compData$comp==113,"comp"] <- "x115"
compData[compData$comp==114,"comp"] <- "x116"
compData[compData$comp==115,"comp"] <- "x119"
compData[compData$comp==116,"comp"] <- "x120"
compData[compData$comp==117,"comp"] <- "x133"
compData[compData$comp==118,"comp"] <- "x134"
compData[compData$comp==119,"comp"] <- "x135"
compData[compData$comp==120,"comp"] <- "x136"
compData[compData$comp==121,"comp"] <- "x139"
compData[compData$comp==122,"comp"] <- "x140"
compData[compData$comp==123,"comp"] <- "x153"
compData[compData$comp==124,"comp"] <- "x154"
compData[compData$comp==125,"comp"] <- "x155"
compData[compData$comp==126,"comp"] <- "x156"
compData[compData$comp==127,"comp"] <- "x159"
compData[compData$comp==128,"comp"] <- "x160"
#### Export
compData$weight <- round(compData$ratio * 0.48,2)
(weights <- compData %>%  
  group_by(name) %>%
  summarise(sum = sum(weight)))
plotWeights <- compData %>% 
  group_by(comp) %>% 
  summarise(sum = sum(weight))
table(plotWeights$sum)
#write.table(compData, here("composition.txt"), sep="\t", row.names = F, quote = F)
