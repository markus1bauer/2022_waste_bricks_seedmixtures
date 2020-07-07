# Model for experiment 2 ####



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
edata <- read_table2("data_processed_experiment_2.txt", col_names = T, na = "na", col_types = 
                       cols(
                         .default = col_double(),
                         plot = col_factor(),
                         block = col_factor(),
                         position = col_factor(),
                         brickType = col_factor(levels = c("Clean","Demolition")),
                         seedmix = col_factor(levels = c("Standard","Robust","Intermediate","Vigorous")),
                         brickRatio = col_factor(levels = c("5","30")),
                         acid = col_factor(levels = c("Control","Acid")),
                         f.watering = col_factor(levels = c("Dry", "Medium_dry", "Medium_moist","Moist"))
                       )        
)

edata$f.watering <- dplyr::recode(edata$f.watering,
                                  "Medium_dry" = "Medium dry", "Medium_moist" = "Medium moist")
edata$seedmix <- factor(edata$seedmix)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Data exploration #####################################################################################

#### a Graphs ---------------------------------------------------------------------------------------------
#simple effects:
par(mfrow = c(2,2));
plot(biomass ~ brickType, edata)
plot(biomass ~ brickRatio, edata)
plot(biomass ~ seedmix, edata)
plot(biomass ~ block, edata)
#2way (brickType:brickRatio):
ggplot(edata, aes(brickType, biomass, color = brickRatio)) + geom_boxplot() + geom_quasirandom(dodge.width=.7)
#2way (brickType:f.watering):
ggplot(edata, aes(f.watering, biomass, color = brickType)) + geom_boxplot() + geom_quasirandom(dodge.width=.7)
#2way (brickType:seedmix):
ggplot(edata, aes(seedmix, biomass, color = brickType)) + geom_boxplot() + geom_quasirandom(dodge.width=.7)
#3way (brickType:brickRatio:watering):
ggplot(edata, aes(brickType, biomass, color = brickRatio)) + facet_grid(~f.watering) + geom_boxplot() + geom_quasirandom(dodge.width=.7)
#3way (brickRatio:brickType:seedmix):
ggplot(edata, aes(brickType, biomass, color = brickRatio)) + facet_grid(~seedmix) + geom_boxplot() + geom_quasirandom(dodge.width=.7)

##### b Outliers, zero-inflation, transformations? -----------------------------------------------------
par(mfrow = c(2,2))
dotchart((edata$biomass), groups = factor(edata$brickType), main = "Cleveland dotplot")
dotchart((edata$biomass), groups = factor(edata$watering), main = "Cleveland dotplot")
dotchart((edata$biomass), groups = factor(edata$brickRatio), main = "Cleveland dotplot")
dotchart((edata$biomass), groups = factor(edata$seedmix), main = "Cleveland dotplot")
dotchart((edata$biomass), groups = factor(edata$grassRatio), main = "Cleveland dotplot")
par(mfrow=c(1,1))
boxplot(edata$biomass);#identify(rep(1,length(edata$biomass)),edata$biomass, labels = c(edata$no))
plot(table((edata$biomass)), type = "h", xlab = "Observed values", ylab = "Frequency")
plot(table(log(edata$biomass)), type = "h", xlab = "Observed values", ylab = "Frequency")
ggplot(edata, aes(biomass)) + geom_density()


## 2 Model building ################################################################################

#### a models ----------------------------------------------------------------------------------------
#random structure:
m1 <- lmer((biomass) ~ f.watering * brickType + (1|block), edata, REML = F)
VarCorr(m1)
#3way (both):
m2 <- lmer(log(biomass) ~ (f.watering + brickRatio + brickType + seedmix)^2 +  
              seedmix:brickRatio:brickType + f.watering:brickRatio:brickType + 
              (1|block), edata, REML = F)
isSingular(m2)
simulationOutput <- simulateResiduals(m2, plot=T)
#3way (f.watering:brickRatio:brickType):
m3 <- lmer(log(biomass) ~ (f.watering + brickRatio + brickType)^2 + seedmix +  
              f.watering:brickRatio:brickType + 
              (1|block), edata, REML = F)
isSingular(m3)
simulationOutput <- simulateResiduals(m3, plot = T)
#2way (full):
m4 <- lmer(log(biomass) ~ (f.watering + brickRatio + brickType)^2 + seedmix + 
              (1|block), edata, REML = F)
isSingular(m4)
simulationOutput <- simulateResiduals(m4, plot = T)
#2way (brickType:brickRatio):
m5 <- lmer(log(biomass) ~ f.watering + brickRatio + brickType + seedmix + 
              brickType:brickRatio + 
              (1|block), edata, REML = F)
isSingular(m5)
simulationOutput <- simulateResiduals(m5, plot=T)

#### b comparison -----------------------------------------------------------------------------------------
AIC(m2,m3,m4,m5) #--> m5
(re.effects <- plot_model(m5, type = "re", show.values = TRUE))
rm(m1,m2,m3,m4)

#### c model check -----------------------------------------------------------------------------------------
simulationOutput <- simulateResiduals(m5, plot=F)
par(mfrow=c(2,2));
plotResiduals(main = "brickType", simulationOutput$scaledResiduals, edata$brickType)
plotResiduals(main = "f.watering", simulationOutput$scaledResiduals, edata$f.watering)
plotResiduals(main = "seedmix", simulationOutput$scaledResiduals, edata$seedmix)
plotResiduals(main = "brickRatio", simulationOutput$scaledResiduals, edata$brickRatio)
plotResiduals(main = "position", simulationOutput$scaledResiduals, edata$position)
plotResiduals(main = "block", simulationOutput$scaledResiduals, edata$block)


## 3 Chosen model output ################################################################################

### Model output ---------------------------------------------------------------------------------------------
m5 <- lmer(log(biomass) ~ f.watering + brickRatio + brickType + seedmix + 
             brickType:brickRatio + 
             (1|block), edata, REML = F)
VarCorr(m5)
r.squaredGLMM(m5)
Anova(m5, type = 3)

### Effect sizes -----------------------------------------------------------------------------------------
(emm <- emmeans(m5, pairwise ~ brickType|brickRatio, typ = "response"))
plot(emm, comparison = T)
