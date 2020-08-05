# Model for experiment 3 ####



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#library(installr);updateR(browse_news=F, install_R=T, copy_packages = T,copy_Rprofile.site = T,keep_old_packages = T, update_packages = T)

### Packages ###
library(tidyverse)
library(ggplot2)
library(ggbeeswarm)
library(car); #Anova(); vif(): variance inflation factors --> checking for dependence (Collinearity) (below 3 is ok)
library(nlme); #use for vif()
library(lme4)
library(lmerTest)
library(DHARMa)
#library(vcd)
library(sjPlot) #plot random effects
library(MuMIn)
library(emmeans)
library(ggeffects)

### Start ###
rm(list = ls())
setwd("Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_for_restoration/data/processed")

### Load data ###
edata <- read_table2("data_processed_experiment_3.txt", col_names = T, na="na", col_types =
                       cols(
                         plot = col_factor(),
                         brickRatio = col_factor(levels = c("5","30")),
                         texture = col_factor(levels=c("Loam","Medium","Sand")),
                         compaction = col_factor(levels=c("Control","Compaction")),
                         coal = col_factor(levels=c("Control","Coal")),
                         biomass = col_double(),
                         estRate = col_double()
                       )
)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Data exploration #####################################################################################

#### a Graphs ---------------------------------------------------------------------------------------------
#simple effects:
par(mfrow = c(2,2))
boxplot(biomass ~ brickRatio, edata)
plot(biomass ~ texture, edata)
plot(biomass ~ compaction, edata)
plot(biomass ~ coal, edata)
#2way: brickRatio:compaction, texture:compaction possible
ggplot(edata,aes(texture, biomass, color = brickRatio)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
ggplot(edata,aes(brickRatio, biomass, color = compaction)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
ggplot(edata,aes(texture, biomass, color = compaction)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
ggplot(edata,aes(brickRatio, biomass, color = coal)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
ggplot(edata,aes(texture, biomass, color = coal)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
pd <- position_dodge(1.5)
#3way (brickRatio:texture:compaction):
ggplot(edata,aes(brickRatio, biomass, color=compaction)) + geom_boxplot()+  geom_quasirandom(data=edata,aes(brickRatio, biomass, color = compaction),dodge.width = .7) +  facet_grid(.~texture)
#3way (brickRatio:texture:coal)
ggplot(edata,aes(brickRatio, biomass, color=coal)) + geom_boxplot() + geom_quasirandom(data=edata,aes(brickRatio, biomass, color = coal),dodge.width = .7) + facet_grid(.~texture)

##### b Outliers, zero-inflation, transformations? -----------------------------------------------------
par(mfrow=c(1,1))
boxplot(edata$biomass, ylim = c(0,20));#identify(rep(1,length(edata$bioMass)),edata$bioMass, labels = c(edata$no))
ggplot(edata, aes(biomass)) + geom_density()
ggplot(edata, aes(sqrt(biomass))) + geom_density()
ggplot(edata, aes(log(biomass))) + geom_density()
par(mfrow = c(2,2))
dotchart((edata$biomass), groups = factor(edata$brickRatio), main = "Cleveland dotplot")
dotchart((edata$biomass), groups = factor(edata$texture), main = "Cleveland dotplot")
dotchart((edata$biomass), groups = factor(edata$compaction), main = "Cleveland dotplot comp")
dotchart((edata$biomass), groups = factor(edata$coal), main = "Cleveland dotplot coal")


## 2 Model building ################################################################################

#### a models ----------------------------------------------------------------------------------------
m1 <- lm(log(biomass) ~ (brickRatio + texture + compaction + coal)^2 +
            brickRatio:texture:compaction + brickRatio:texture:coal, edata)
simulationOutput <- simulateResiduals(m1, plot = T)
m2 <- lm(log(biomass) ~ (brickRatio + texture + compaction)^2 + coal +
            brickRatio:texture:compaction, edata)
simulationOutput <- simulateResiduals(m2, plot = T)
m3 <- lm(log(biomass) ~ brickRatio + texture + compaction + coal +
            brickRatio:compaction + texture:compaction + texture:brickRatio, edata)
simulationOutput <- simulateResiduals(m3, plot = T)
m4 <- lm(log(biomass) ~ brickRatio + texture + compaction + coal +
            brickRatio:compaction + texture:compaction, edata)
simulationOutput <- simulateResiduals(m4, plot = T)
m5 <- lm(log(biomass) ~ brickRatio + texture + compaction + coal + 
            brickRatio:texture, edata)
simulationOutput <- simulateResiduals(m5, plot = T)

#### b comparison -----------------------------------------------------------------------------------------
anova(m1,m2,m3,m4,m5) #--> m2
rm(m1,m3,m4,m5)

#### c model check -----------------------------------------------------------------------------------------
par(mfrow=c(2,2));
plotResiduals(main = "brickRatio", simulationOutput, form = edata$brickRatio);
plotResiduals(main = "texture", simulationOutput, form = edata$texture);
plotResiduals(main = "compaction", simulationOutput, form = edata$compaction);
plotResiduals(main = "coal", simulationOutput, form = edata$coal);


## 3 Chosen model output ################################################################################

### Model output ---------------------------------------------------------------------------------------------
summary(m2)
Anova(m2, type = 3)

### Effect sizes -----------------------------------------------------------------------------------------
(emm <- emmeans(m2, revpairwise ~ brickRatio | texture, type = "response"))
plot(emm, comparisons=T)
eff_size(emmeans(m2, "brickRatio", "texture"), sigma = sigma(m2), edf = 59)
(emm <- emmeans(m2, revpairwise ~ brickRatio * compaction | texture, type = "response"))
plot(emm, comparisons=T)
emm3way <- emmeans(m2, ~ brickRatio * texture * compaction)
pwpp((emm3way), by = "texture", type = "response")