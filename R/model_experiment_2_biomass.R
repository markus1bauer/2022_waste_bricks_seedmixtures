# Brick-based substrates and designed seedmixtures
# Model for Experiment 2 (biomass) ####
# Markus Bauer
# 2022-01-24



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### Packages ###
library(here)
library(tidyverse)
library(ggbeeswarm)
library(lmerTest)
library(DHARMa)
library(emmeans)

### Start ###
rm(list = ls())
setwd(here("data/processed"))

### Load data ###
environment <- read_csv("data_processed_experiment_2_environment.csv", col_names = T, na = "na", locale = locale(decimal_mark = "."),
                          col_types = 
                       cols(
                         .default = "d",
                         plot = "f",
                         block = "f",
                         position = "f",
                         f.watering = col_factor(levels = c("Dry", "Medium_dry", "Medium_moist","Moist")),
                         seedmix = col_factor(levels = c("Standard","Robust","Intermediate","Vigorous")),
                         brickType = col_factor(levels = c("Demolition","Clean")),
                         brickRatio = col_factor(levels = c("30","5")),
                         acid = col_factor(levels = c("Acid","Control"))
                       )) %>%
  mutate(f.watering = dplyr::recode(f.watering, "Medium_dry" = "Medium dry", "Medium_moist" = "Medium moist"))



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ####################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Data exploration ############################################################################

#### a Graphs --------------------------------------------------------------------------------------
#simple effects:
par(mfrow = c(2,2));
plot(biomass ~ brickType, environment)
plot(biomass ~ brickRatio, environment)
plot(biomass ~ seedmix, environment)
plot(biomass ~ block, environment)
#2way (brickType:brickRatio):
ggplot(environment, aes(brickType, biomass, color = brickRatio)) + geom_boxplot() + geom_quasirandom(dodge.width=.7)
#2way (brickType:f.watering):
ggplot(environment, aes(f.watering, biomass, color = brickType)) + geom_boxplot() + geom_quasirandom(dodge.width=.7)
#2way (brickType:seedmix):
ggplot(environment, aes(seedmix, biomass, color = brickType)) + geom_boxplot() + geom_quasirandom(dodge.width=.7)
#3way (brickType:brickRatio:watering):
ggplot(environment, aes(brickType, biomass, color = brickRatio)) + facet_grid(~f.watering) + geom_boxplot() + geom_quasirandom(dodge.width=.7)
#3way (brickRatio:brickType:seedmix):
ggplot(environment, aes(brickType, biomass, color = brickRatio)) + facet_grid(~seedmix) + geom_boxplot() + geom_quasirandom(dodge.width=.7)

##### b Outliers, zero-inflation, transformations? -----------------------------------------------------
par(mfrow = c(2,2))
dotchart((environment$biomass), groups = factor(environment$brickType), main = "Cleveland dotplot")
dotchart((environment$biomass), groups = factor(environment$watering), main = "Cleveland dotplot")
dotchart((environment$biomass), groups = factor(environment$brickRatio), main = "Cleveland dotplot")
dotchart((environment$biomass), groups = factor(environment$seedmix), main = "Cleveland dotplot")
dotchart((environment$biomass), groups = factor(environment$grassRatio), main = "Cleveland dotplot")
par(mfrow = c(1,1))
boxplot(environment$biomass);#identify(rep(1, length(environment$biomass)),environment$biomass, labels = c(environment$no))
plot(table((environment$biomass)), type = "h", xlab = "Observed values", ylab = "Frequency")
ggplot(environment, aes(biomass)) + geom_density()
ggplot(environment, aes(log(biomass))) + geom_density()


## 2 Model building ################################################################################

#### a models ----------------------------------------------------------------------------------------
#random structure:
m1 <- lmer((biomass) ~ f.watering * brickType + (1|block), environment, REML = F)
VarCorr(m1)
#3way (both):
m2 <- lmer(log(biomass) ~ (f.watering + brickRatio + brickType + seedmix)^2 +  
              seedmix:brickRatio:brickType + f.watering:brickRatio:brickType + 
              (1|block), environment, REML = F)
simulateResiduals(m2, plot=T)
isSingular(m2)
#3way (f.watering:brickRatio:brickType):
m3 <- lmer(log(biomass) ~ (f.watering + brickRatio + brickType)^2 + seedmix +  
              f.watering:brickRatio:brickType + 
              (1|block), environment, REML = F)
simulateResiduals(m3, plot = T)
isSingular(m3)
#2way (full):
m4 <- lmer(log(biomass) ~ (f.watering + brickRatio + brickType)^2 + seedmix + 
              (1|block), environment, REML = F)
simulateResiduals(m4, plot = T)
isSingular(m4)
#2way (brickType:brickRatio):
m5 <- lmer(log(biomass) ~ f.watering + seedmix + brickType + brickRatio + 
              brickType:brickRatio + 
              (1|block), environment, REML = F)
simulateResiduals(m5, plot=T)
isSingular(m5)

#### b comparison -------------------------------------------------------------------------------------
anova(m2,m3,m4,m5) #--> m5
rm(m1,m2,m3,m4)

#### c model check -------------------------------------------------------------------------------------
simulationOutput <- simulateResiduals(m5, plot = T)
par(mfrow=c(2,2));
plotResiduals(main = "brickType", simulationOutput$scaledResiduals, environment$brickType)
plotResiduals(main = "f.watering", simulationOutput$scaledResiduals, environment$f.watering)
plotResiduals(main = "seedmix", simulationOutput$scaledResiduals, environment$seedmix)
plotResiduals(main = "brickRatio", simulationOutput$scaledResiduals, environment$brickRatio)
plotResiduals(main = "position", simulationOutput$scaledResiduals, environment$position)
plotResiduals(main = "block", simulationOutput$scaledResiduals, environment$block)


## 3 Chosen model output ################################################################################

### Model output ----------------------------------------------------------------------------------------
m5 <- lmer(log(biomass) ~ f.watering + seedmix + brickType + brickRatio +
             brickType:brickRatio +
             (1|block), environment, REML = F)
MuMIn::r.squaredGLMM(m5)
VarCorr(m5)
sjPlot::plot_model(m5, type = "re", show.values = TRUE)
(table <- car::Anova(m5, type = 3))
tidytable <- broom::tidy(table)

### Effect sizes -----------------------------------------------------------------------------------------
(emm <- emmeans(m5, pairwise ~ brickType|brickRatio, typ = "response"))
(emm <- emmeans(m5, pairwise ~ brickType*brickRatio, typ = "response"))
plot(emm, comparison = T)

### Save ###
write.csv(tidytable, here("outputs/statistics/table_anova_experiment_2_biomass.csv"))