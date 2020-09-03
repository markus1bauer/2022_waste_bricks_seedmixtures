# Show Figure 2 ###



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
edata <- read_table2("experiment_1_data_processed.txt", col_names = T, na = "na", col_types = 
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

### Chosen model ###
m5 <- lmer(log(biomass) ~ (brickRatio + acid + f.watering + seedmix) +  
             brickRatio:acid + brickRatio:f.watering + brickRatio:seedmix + 
             f.watering:seedmix + acid:seedmix + 
             brickRatio:acid:seedmix + 
             (1|block), edata, REML = F)



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

### seedmix:brickRatio:acid ###
pdata <- ggemmeans(m5, terms = c("brickRatio","acid","seedmix"), type = "fe")
pdata <- rename(pdata, biomass = predicted, brickRatio = x, acid = group, seedmix = facet);
meandata <- filter(pdata, acid=="Control" & brickRatio=="5")
pd <- position_dodge(.6)
ggplot(pdata, aes(brickRatio, biomass, shape = brickRatio, color = acid, ymin = conf.low, ymax = conf.high))+
  geom_quasirandom(data = edata, aes(brickRatio, biomass, shape = brickRatio, color = acid), 
                   color = "grey70", dodge.width = .6, size = 0.7)+
  geom_hline(aes(yintercept = biomass), meandata, color = "grey70")+
  geom_errorbar(position = pd, width = 0.0, size = 0.4)+
  geom_point(position = pd, size = 2.5)+
  facet_grid(.~ seedmix)+
  scale_y_continuous(limits = c(0,33), breaks = seq(-100,100,5)) +
  scale_colour_manual(values = c("grey50","black")) +
  scale_shape_manual(values = c(1,16)) +
  labs(x = "Brick ratio [vol%]", y = expression(paste("Biomass [g]")), shape = "", color = "") +
  guides(shape = F)+
  themeMB()
#ggsave("figure_2_(800dpi_16x5cm).tiff",
#       dpi = 800, width = 16, height = 5, units = "cm", path = "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_for_restoration/outputs/figures/raw")
