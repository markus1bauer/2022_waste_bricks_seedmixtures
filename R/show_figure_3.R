# Show Figure 3 ####



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Packages ###
library(tidyverse)
library(ggplot2)
library(ggbeeswarm)
library(lme4)
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

### Chosen model ###
m5 <- lmer(log(biomass) ~ f.watering + brickRatio + brickType + seedmix + 
             brickType:brickRatio + 
             (1|block), edata, REML = F)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plotten ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
themeMB <- function(){
  theme(
    panel.background = element_rect(fill = "white"),
    text  = element_text(size=9, color = "black"),
    axis.line.y = element_line(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.key = element_rect(fill = "white"),
    legend.position = "right",
    legend.margin = margin(0, 0, 0, 0, "cm"),
    plot.margin = margin(0, 0, 0, 0, "cm")
  )
}


### brickType:brickRatio ###
pdata <- ggemmeans(m5, terms = c("brickRatio","brickType"), type = "fe")
pdata <- rename(pdata, biomass = predicted, brickRatio = x, brickType = group);
meandata <- filter(pdata, brickRatio == "5")
pd <- position_dodge(.6)
ggplot(pdata, aes(brickRatio, biomass, shape = brickRatio, ymin = conf.low, ymax = conf.high))+
  geom_quasirandom(data = edata, aes(brickRatio, biomass), 
                   color = "grey70", dodge.width = .6, size = 0.7)+
  geom_hline(aes(yintercept = biomass), meandata, color = "grey70") +
  geom_errorbar(position = pd, width = 0.0, size = 0.4) +
  geom_point(position = pd, size = 2.5) +
  facet_grid(~ brickType) +
  scale_y_continuous(limits = c(0,33), breaks = seq(-100, 100, 5)) +
  scale_shape_manual(values = c(1,16,16,16)) +
  labs(x = "Brick ratio [vol%]", y = expression(paste("Biomass [g]")), shape = "", color = "") +
  guides(shape = F)+
  themeMB()
ggsave("figure_3_(800dpi_6.5x5cm).tiff",
       dpi = 800, width = 6.5, height = 5, units = "cm", path = "Z:/Documents/0_Ziegelprojekt/3_Aufnahmen_und_Ergebnisse/2020_waste_bricks_for_restoration/outputs/figures")
#visreg(m5, "treatment", ylab = expression(paste(Delta,"Biomass [g g"^"-1"*"]")), xlab = "",data = edataBricktype,
#       trans = exp, type = "contrast", partial = T, rug = F, gg = T, overlay = F, band = T,
#       points = list(cex = 0.5, pch = 16), line = list(col="black"), whitespace=.2) +
#  themeMB()
