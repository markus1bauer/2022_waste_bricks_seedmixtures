# Brick-based substrates and designed seedmixtures
# Model for Experiment 1 (biomass) ####
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
setwd(here("data", "processed"))

### Load data ###
environment <- read_csv("data_processed_experiment_1_environment.csv", col_names = TRUE,
                        na = "na", locale = locale(decimal_mark = "."),
                          col_types = 
                       cols(
                         .default = "d",
                         plot = "f",
                         block = "f",
                         position = "f",
                         brickType = col_factor(levels = c("Clean","Demolition")),
                         seedmix = col_factor(levels = c("Standard","Robust","Intermediate","Vigorous")),
                         brickRatio = col_factor(levels = c("5","30")),
                         acid = col_factor(levels = c("Control","Acid")),
                         f.watering = col_factor(levels = c("Dry", "Medium_dry", "Medium_moist","Moist"))
                       )) %>%
  mutate(f.watering = dplyr::recode(f.watering, "Medium_dry" = "Medium dry", "Medium_moist" = "Medium moist"))



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### 1 Data exploration #####################################################################################

#### a Graphs ---------------------------------------------------------------------------------------------
#simple effects:
par(mfrow = c(2,2))
plot(biomass ~ brickRatio, environment)
plot(biomass ~ acid, environment)
plot(biomass ~ f.watering, environment)
plot(biomass ~ seedmix, environment)
par(mfrow = c(2,2))
plot(biomass ~ vegCov13, environment)
plot(biomass ~ grassRatio, environment)
par(mfrow = c(2,2))
plot(biomass ~ position, environment)
plot(biomass ~ pump, environment)
plot(biomass ~ block, environment)
#2way (brickRatio:acid):
ggplot(environment,aes(brickRatio, biomass, color = acid)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (brickRatio:watering):
ggplot(environment,aes(f.watering, biomass,color = brickRatio)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (brickRatio:seedmix):
ggplot(environment,aes(seedmix, biomass, color = brickRatio)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (seedmix:watering):
ggplot(environment,aes(f.watering, biomass,color = seedmix)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#2way (acid:watering):
ggplot(environment,aes(f.watering, biomass, color = acid)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (brickRatio:acid:seedmix):
ggplot(environment,aes(brickRatio, biomass, color = acid)) + facet_grid(~seedmix) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (brickRatio:watering:seedmix):
ggplot(environment,aes(f.watering, biomass, color = brickRatio)) + facet_grid(~seedmix) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#3way (brickRatio:acid:watering):
ggplot(environment,aes(brickRatio, biomass, color = acid)) + facet_grid(~f.watering) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
#4way (focus only on standard and intermediate): --> no effect under dry conditions
ggplot(environment,aes(f.watering, biomass, color = brickRatio, shape = acid)) + facet_grid(~seedmix) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
# interactions with block:
ggplot(environment,aes(brickRatio, biomass, color = acid)) + geom_boxplot() + facet_wrap(~block) + geom_quasirandom(dodge.width = .7)
ggplot(environment,aes(block, biomass, color = f.watering)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)
ggplot(environment,aes(block, biomass, color = seedmix)) + geom_boxplot() + geom_quasirandom(dodge.width = .7)

##### b Outliers, zero-inflation, transformations? -----------------------------------------------------
par(mfrow = c(2,2))
dotchart((environment$biomass), groups = factor(environment$brickRatio), main = "Cleveland dotplot")
dotchart((environment$biomass), groups = factor(environment$acid), main = "Cleveland dotplot")
dotchart((environment$biomass), groups = factor(environment$watering), main = "Cleveland dotplot")
dotchart((environment$biomass), groups = factor(environment$seedmix), main = "Cleveland dotplot")
dotchart((environment$biomass), groups = factor(environment$grassRatio), main = "Cleveland dotplot")
par(mfrow=c(1,1));
boxplot(environment$biomass, ylim = c(0,45));#identify(rep(1, length(environment$biomass)),environment$biomass, labels = c(environment$no))
par(mfrow = c(2,2));
plot(table((environment$biomass)),type = "h", xlab = "Observed values", ylab = "Frequency")
ggplot(environment, aes(biomass)) + geom_density()
ggplot(environment, aes(log(biomass))) + geom_density()


## 2 Model building ################################################################################

#### a models --------------------------------------------------------------------------------------
#random structure
m1 <- lmer((biomass) ~ f.watering * seedmix + (1|block), environment, REML = F)
VarCorr(m1)
#4w-model
m2 <- lmer(log(biomass) ~ (brickRatio + acid + f.watering + seedmix)^2 +  
              brickRatio:f.watering:seedmix + brickRatio:acid:seedmix + 
              brickRatio:acid:f.watering:seedmix + 
              (1|block), environment, REML = F)
simulateResiduals(m2, plot = T)
isSingular(m2)
#full 3w-model
m3 <- lmer(log(biomass) ~ (brickRatio + acid + f.watering + seedmix) +
              brickRatio:acid + brickRatio:f.watering + brickRatio:seedmix + 
              f.watering:seedmix + acid:seedmix +
              brickRatio:f.watering:seedmix + brickRatio:acid:seedmix + 
              (1|block), environment, REML = F)
simulateResiduals(m3, plot = T)
isSingular(m3)
#3w-model brick:water:mix
m4 <- lmer(log(biomass) ~ (brickRatio + acid + f.watering + seedmix) +
              brickRatio:acid + brickRatio:f.watering + brickRatio:seedmix + 
              f.watering:seedmix +
              brickRatio:f.watering:seedmix + 
              (1|block), environment, REML = F)
simulateResiduals(m4, plot = T)
isSingular(m4)
#3w-model brick:acid:mix
m5 <- lmer(log(biomass) ~ (brickRatio + acid + f.watering + seedmix) +  
              brickRatio:acid + brickRatio:f.watering + brickRatio:seedmix + 
              f.watering:seedmix + acid:seedmix + 
              brickRatio:acid:seedmix + 
              (1|block), environment, REML = F)
simulateResiduals(m5, plot = T)
isSingular(m5)
#2w-model
m6 <- lmer(log(biomass) ~ (brickRatio + acid + f.watering + seedmix) + 
              brickRatio:acid + brickRatio:f.watering + brickRatio:seedmix + 
              f.watering:seedmix + 
              (1|block), environment, REML = F)
simulateResiduals(m6, plot = T)
isSingular(m6)

#### b comparison ------------------------------------------------------------------------------
anova(m2,m3,m4,m5,m6) # --> m5
rm(m1,m2,m3,m4,m6)

#### c model check ------------------------------------------------------------------------------
simulationOutput <- simulateResiduals(m5, plot = T)
par(mfrow=c(2,2))
plotResiduals(main = "brickRatio", simulationOutput$scaledResiduals, environment$brickRatio)
plotResiduals(main = "acid", simulationOutput$scaledResiduals, environment$acid)
plotResiduals(main = "f.watering", simulationOutput$scaledResiduals, environment$f.watering)
plotResiduals(main = "seedmix", simulationOutput$scaledResiduals,environment$seedmix)
plotResiduals(main = "position", simulationOutput$scaledResiduals, environment$position)
plotResiduals(main = "block", simulationOutput$scaledResiduals, environment$block)


## 3 Chosen model output ########################################################################

### Model output --------------------------------------------------------------------------------
MuMIn::r.squaredGLMM(m5)
VarCorr(m5)
sjPlot::plot_model(m5, type = "re", show.values = T)
summary(m5)
(table <- car::Anova(m5, type = 3))
tidytable <- broom::tidy(table)

### Effect sizes --------------------------------------------------------------------------------
(emm <- emmeans(m5, revpairwise ~ seedmix | f.watering, type = "response"))
plot(emm, comparison = T)
contrast(emmeans(m5, ~ seedmix * f.watering, type = "response"), "trt.vs.ctrl", ref = 1)
(emm <- emmeans(m5, revpairwise ~ brickRatio * acid | seedmix, type="response"))
plot(emm, comparison = T)
(emm <- emmeans(m5, revpairwise ~ brickRatio | f.watering, type = "response"))
plot(emm, comparison = T)

### Save ###
write.csv(tidytable, here("outputs", "statistics", "table_anova_experiment_1_biomass.csv"))
