# Brick-based substrates and designed seedmixtures
# Show figure A3 grain size distribution ####
# Markus Bauer
# 2022-01-24



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ###############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### Packages ###
library(here)
library(tidyverse)

### Start ###
rm(list = ls())
setwd(here("data", "processed"))

### Load data ###
data <- read_csv("supp_data_processed_experiment_3.csv",
                    col_names = TRUE, na = "na", col_types =
                       cols(
                         .default = "d",
                         substrate = "f",
                         substrateAbb = "f"
                       ))



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plotten ###################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


theme_mb <- function() {
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_line(color = "grey80"),
    text  = element_text(size = 10, color = "black"),
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

ggplot(data, aes(x = grainSize, y = grainSizeCum, color = substrateAbb,
                 linetype = substrateAbb)) +
  geom_line(size = 0.8) +
  scale_color_manual(values = c("coral", "cyan", "brown3", "blue",
                                "brown3", "blue")) +
  scale_linetype_manual(values = c("dotted", "dotted", "dashed", "dashed",
                                   "solid", "solid")) +
  scale_x_log10(breaks = c(0.002, 0.063, 0.2, 0.63, 2, 4, 8, 16, 25, 31.5)) +
  labs(x = "Grain size [mm]", y = "Cumulative ratio [wt%]", linetype = "",
       color = "") +
  theme_mb()

ggsave("figure_a8_800dpi_16x10cm.tiff",
      dpi = 800, width = 16, height = 10, units = "cm",
      path = here("outputs", "figures"))
