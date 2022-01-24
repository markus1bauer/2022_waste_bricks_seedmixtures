# Brick-based substrates and designed seedmixtures
# Show figure A1 grain size distribution ####
# Markus Bauer
# 2022-01-24
# Citation: 
## Bauer M, Krause M, Heizinger V, Kollmann J (submitted) 
## Using waste bricks for recultivation: no negative effects of brick-augmented substrates with varying acid pre-treatment, soil type and moisture on contrasting seed mixtures
## Unpublished data.



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Packages ###
library(here)
library(tidyverse)

### Start ###
rm(list = ls())
setwd(here("data/processed"))

### Load data ###
edata <- read_table2("supp_data_processed_experiment_1.txt", col_names = T, na = "na", col_types = 
                       cols(
                         .default = "d",
                         substrate = "f",
                         substrateAbb = "f"
                       )        
)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plotten ################################################################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
themeMB <- function(){
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(color = "grey80"),
    text  = element_text(size=10, color = "black"),
    axis.line.y = element_line(),
    axis.line.x = element_line(),
    axis.text.x = element_text(angle = 270),
    axis.ticks.x = element_line(),
    legend.key = element_rect(fill = "white"),
    legend.position = "right",
    legend.margin = margin(0, 0, 0, 0, "cm"),
    plot.margin = margin(0, 0, 0, 0, "cm")
  )
}


ggplot(edata, aes(x = grainSize, y = grainSizeCum, color = substrateAbb)) +
  geom_line(size = 0.8) +
  scale_color_manual(values = c("red4","green4","red","green")) +
  scale_x_log10(breaks = c(0.002,0.063,0.2,0.63,2,4,8,16,25,31.5)) +
  labs(x = "Grain size [mm]", y = "Cumulative ratio [wt%]", color = "") +
  themeMB()
ggsave("figure_A1_(800dpi_16x10cm).tiff",
      dpi = 800, width = 16, height = 10, units = "cm", path = here("outputs/figures/supp"))
