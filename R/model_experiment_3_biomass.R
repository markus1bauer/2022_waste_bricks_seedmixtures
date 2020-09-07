# Model for experiment 3 ###



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Packages ###
library(tidyverse)
library(ggbeeswarm)
library(lmerTest)
library(DHARMa)
library(emmeans)

### Start ###
rm(list = ls())
setwd("Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_for_restoration/data/processed")

### Load data ###
environment <- read_table2("data_processed_experiment_3_environment.txt", col_names = T, na="na", col_types =
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
boxplot(biomass ~ brickRatio, environment)
plot(biomass ~ texture, environment)
plot(biomass ~ compaction, environment)
plot(biomass ~ coal, environment)
#2way: brickRatio:compaction, texture:compaction possible
ggplot(environment,aes(texture, biomass, color = brickRatio)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
ggplot(environment,aes(brickRatio, biomass, color = compaction)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
ggplot(environment,aes(texture, biomass, color = compaction)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
ggplot(environment,aes(brickRatio, biomass, color = coal)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
ggplot(environment,aes(texture, biomass, color = coal)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
pd <- position_dodge(1.5)
#3way (brickRatio:texture:compaction):
ggplot(environment,aes(brickRatio, biomass, color=compaction)) + geom_boxplot()+  geom_quasirandom(data=environment,aes(brickRatio, biomass, color = compaction),dodge.width = .7) +  facet_grid(.~texture)
#3way (brickRatio:texture:coal)
ggplot(environment,aes(brickRatio, biomass, color=coal)) + geom_boxplot() + geom_quasirandom(data=environment,aes(brickRatio, biomass, color = coal),dodge.width = .7) + facet_grid(.~texture)

##### b Outliers, zero-inflation, transformations? -----------------------------------------------------
par(mfrow = c(2,2))
dotchart((environment$biomass), groups = factor(environment$brickRatio), main = "Cleveland dotplot")
dotchart((environment$biomass), groups = factor(environment$texture), main = "Cleveland dotplot")
dotchart((environment$biomass), groups = factor(environment$compaction), main = "Cleveland dotplot comp")
dotchart((environment$biomass), groups = factor(environment$coal), main = "Cleveland dotplot coal")
par(mfrow=c(1,1))
boxplot(environment$biomass);#identify(rep(1, length(environment$bioMass)),environment$bioMass, labels = c(environment$no))
plot(table((environment$biomass)), type = "h", xlab = "Observed values", ylab = "Frequency")
ggplot(environment, aes(biomass)) + geom_density()
ggplot(environment, aes(sqrt(biomass))) + geom_density()
ggplot(environment, aes(log(biomass))) + geom_density()


## 2 Model building ################################################################################

#### a models ----------------------------------------------------------------------------------------
m1 <- lm(log(biomass) ~ (brickRatio + texture + compaction + coal)^2 +
            brickRatio:texture:compaction + brickRatio:texture:coal, environment)
simulateResiduals(m1, plot = T)
m2 <- lm(log(biomass) ~ (brickRatio + texture + compaction)^2 + coal +
            brickRatio:texture:compaction, environment)
simulateResiduals(m2, plot = T)
m3 <- lm(log(biomass) ~ brickRatio + texture + compaction + coal +
            brickRatio:compaction + texture:compaction + texture:brickRatio, environment)
simulateResiduals(m3, plot = T)
m4 <- lm(log(biomass) ~ brickRatio + texture + compaction + coal +
            brickRatio:compaction + texture:compaction, environment)
simulateResiduals(m4, plot = T)
m5 <- lm(log(biomass) ~ brickRatio + texture + compaction + coal + 
            brickRatio:texture, environment)
simulateResiduals(m5, plot = T)

#### b comparison -----------------------------------------------------------------------------------------
anova(m1,m2,m3,m4,m5) #--> m2
rm(m1,m3,m4,m5)

#### c model check -----------------------------------------------------------------------------------------
simulationOutput <- simulateResiduals(m2, plot = T)
par(mfrow=c(2,2));
plotResiduals(main = "brickType", simulationOutput$scaledResiduals, environment$brickType)
plotResiduals(main = "f.watering", simulationOutput$scaledResiduals, environment$f.watering)
plotResiduals(main = "seedmix", simulationOutput$scaledResiduals, environment$seedmix)
plotResiduals(main = "brickRatio", simulationOutput$scaledResiduals, environment$brickRatio)
plotResiduals(main = "position", simulationOutput$scaledResiduals, environment$position)
plotResiduals(main = "block", simulationOutput$scaledResiduals, environment$block)


## 3 Chosen model output ################################################################################

### Model output ---------------------------------------------------------------------------------------------
summary(m2)
car::Anova(m2, type = 3)

### Effect sizes -----------------------------------------------------------------------------------------
(emm <- emmeans(m2, revpairwise ~ brickRatio | texture, type = "response"))
plot(emm, comparisons = T)
(emm <- emmeans(m2, revpairwise ~ brickRatio * compaction | texture, type = "response"))
plot(emm, comparisons = T)
emm3way <- emmeans(m2, ~ brickRatio * texture * compaction)
pwpp((emm3way), by = "texture", type = "response")