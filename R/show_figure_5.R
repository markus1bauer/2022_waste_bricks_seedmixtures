# Show Figure 5 ###



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Packages ###
library(tidyverse)
library(ggbeeswarm)
library(lme4)
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

#### Chosen model ###
m2 <- lm(log(biomass) ~ (brickRatio + texture + compaction)^2 + coal +
           brickRatio:texture:compaction, edata)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plotten ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
themeMB <- function(){
  theme(
    panel.background = element_rect(fill = "white"),
    text  = element_text(size=10, color = "black"),
    axis.line.y = element_line(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.key = element_rect(fill = "white"),
    legend.position = "right",
    legend.margin = margin(0, 0, 0, 0, "cm"),
    plot.margin = margin(0, 0, 0, 0, "cm")
  )
}

### brickRatio:soil texture ###
pd <- position_dodge(.6)
pdata <- ggemmeans(m2, terms = c("brickRatio","texture"), type = "fe")
pdata <- rename(pdata, biomass = predicted, brickRatio = x, texture = group);
meandata <- filter(pdata, brickRatio == "5")
ggplot(pdata,aes(brickRatio, biomass, shape = brickRatio, ymin = conf.low, ymax = conf.high))+
  geom_quasirandom(data = edata, aes(brickRatio, biomass, shape = brickRatio),
                   color = "grey70", dodge.width = .6, size = 0.7)+
  geom_hline(aes(yintercept = biomass), meandata, color = "grey70")+
  geom_errorbar(position = pd, width = 0.0, size = 0.4)+
  geom_point(position = pd, size = 2.5)+
  facet_grid(.~ texture)+
  scale_y_continuous(limits = c(0,18), breaks = seq(-100,100,5)) +
  scale_colour_manual(values = c("grey40","black")) +
  scale_shape_manual(values = c(1,16)) +
  labs(x = "Brick ratio [vol%]",y = expression(paste("Biomass [g]")), shape = "",color = "") +
  guides(shape = F)+
  themeMB()
#ggsave("figure_5_(800dpi_8x5cm).tiff",
#       dpi = 800, width = 8, height = 5, units = "cm", path = "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_for_restoration/outputs/figures")
