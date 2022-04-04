# Brick-based substrates and designed seedmixtures
# Show table A4 ####
# Markus Bauer
# 2022-01-24



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ###############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### Packages ###
library(here)
library(tidyverse)
library(ggbeeswarm)
library(lme4)
library(car)
library(emmeans)
library(ggeffects)
library(broom)

### Start ###
rm(list = ls())
setwd(here("dat", "processed"))

### Load data ###
edata <- read_table2("data_processed_experiment_1.txt",
                     col_names = TRUE, na = "na", col_types =
                       cols(
                         .default = "d",
                         plot = "f",
                         block = "f",
                         position = "f",
                         brickType = col_factor(
                           levels = c("Clean", "Demolition")
                           ),
                         seedmix = col_factor(
                           levels = c("Standard", "Robust",
                                      "Intermediate", "Vigorous")
                           ),
                         brickRatio = col_factor(levels = c("5", "30")),
                         acid = col_factor(levels = c("Control", "Acid")),
                         f.watering = col_factor(
                           levels = c("Dry", "Medium_dry",
                                      "Medium_moist", "Moist")
                           )
                       )) %>%
  mutate(f.watering = dplyr::recode(f.watering,
                                    "Medium_dry" = "Medium dry",
                                    "Medium_moist" = "Medium moist"))

#### Chosen model ###
m5 <- lmer(log(biomass) ~ (brickRatio + acid + f.watering + seedmix) +
             brickRatio:acid + brickRatio:f.watering + brickRatio:seedmix +
             f.watering:seedmix + acid:seedmix +
             brickRatio:acid:seedmix +
             (1 | block), edata, REML = FALSE)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Printing ##################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

t1 <- tidy(Anova(m5, type = 3))
write.csv2(t1, here("outputs", "tables", "supp", "supp_table_a4.csv"))
