# Brick-based substrates and designed seedmixtures
# Show figure 3 ####
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
library(emmeans)
library(ggeffects)

### Start ###
rm(list = ls())
setwd(here("data", "processed"))

### Load data ###
environment <- read_table("data_processed_experiment_1_environment.txt",
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
                               acid = col_factor(
                                 levels = c("Control", "Acid")
                                 ),
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
             (1 | block), environment, REML = FALSE)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plotten ###################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


theme_mb <- function() {
  theme(
    panel.background = element_rect(fill = "white"),
    text  = element_text(size = 10, color = "black"),
    axis.line.y = element_line(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.key = element_rect(fill = "white"),
    legend.position = "right",
    legend.margin = margin(0, 0, 0, 0, "cm"),
    plot.margin = margin(0, 0, 0, 0, "cm")
  )
}

### brickRatio:watering ###
pdata <- ggemmeans(m5, terms = c("brickRatio", "f.watering"), type = "fe")
pdata <- rename(pdata, biomass = predicted, brickRatio = x, f.watering = group)
meandata <- filter(pdata, brickRatio == "5")
pd <- position_dodge(.6)
ggplot(pdata, aes(brickRatio, biomass, shape = brickRatio,
                  ymin = conf.low, ymax = conf.high)) +
  geom_quasirandom(data = environment, aes(brickRatio, biomass),
                   color = "grey70", dodge.width = .6, size = 0.7) +
  geom_hline(aes(yintercept = biomass), meandata,
             color = "grey70", size = .25) +
  geom_hline(aes(yintercept = conf.low), meandata,
             color = "grey70", linetype = "dashed", size = .25) +
  geom_hline(aes(yintercept = conf.high), meandata,
             color = "grey70", linetype = "dashed", size = .25) +
  geom_errorbar(position = pd, width = 0.0, size = 0.4) +
  geom_point(position = pd, size = 2.5) +
  facet_grid(~ f.watering) +
  scale_y_continuous(limits = c(0, 33), breaks = seq(-100, 100, 5)) +
  scale_shape_manual(values = c(1, 16, 16, 16)) +
  labs(x = "Brick ratio [vol%]", y = expression(paste("Biomass [g]")),
       shape = "", color = "") +
  guides(shape = FALSE) +
  theme_mb()

ggsave("figure_3_800dpi_10x5cm.tiff",
       dpi = 800, width = 10, height = 5, units = "cm",
       path = here("outputs", "figures"))
