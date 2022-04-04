# Brick-based substrates and designed seedmixtures
# Script to prepare data of experiment 1 and 2 grain size distribution ####
# Markus Bauer
# 2022-01-24



### Packages ###
library(here)
library(tidyverse)

### Start ###
rm(list = ls())
setwd(here("data", "raw"))

### Load data ###
data <- read_csv("supp_data_raw_experiment_1_2_3.csv",
                 col_names = TRUE, na = "na", col_types =
                       cols(
                         .default = "d"
                       )) %>%
  gather("substrate", "ratio", 2:13) %>%
  group_by(substrate) %>%
  mutate(grainSizeCum = cumsum(ratio))

### Create data frame for experiment 1 ###
data1 <- data %>%
  filter(substrate == "substrate_7" |
           substrate == "substrate_8" |
           substrate == "substrate_9" |
           substrate == "substrate_10") %>%
  mutate(substrateAbb = dplyr::recode(substrate,
                                    "substrate_7" = "30%_bricks_Acid",
                                    "substrate_8" = "5%_bricks_Acid",
                                    "substrate_9" = "30%_bricks_Control",
                                    "substrate_10" = "5%_bricks_Control",
                                    ))

### Create data frame for experiment 2 ###
data2 <- data %>%
  filter(substrate == "substrate_7" |
           substrate == "substrate_8" |
           substrate == "substrate_11" |
           substrate == "substrate_12") %>%
  mutate(substrateAbb = dplyr::recode(substrate,
                                    "substrate_7" = "Clean_bricks_30%",
                                    "substrate_8" = "Clean_bricks_5%",
                                    "substrate_11" = "Demolition_bricks_30%",
                                    "substrate_12" = "Demolition_bricks_5%"
                                    ))

### Create data frame for experiment 3 ###
data3 <- data %>%
  filter(substrate == "substrate_13" |
           substrate == "substrate_14" |
           substrate == "substrate_15" |
           substrate == "substrate_16" |
           substrate == "substrate_17" |
           substrate == "substrate_18") %>%
  mutate(substrateAbb = dplyr::recode(substrate,
                                    "substrate_13" = "Sand_30%_bricks",
                                    "substrate_14" = "Sand_5%_bricks",
                                    "substrate_15" = "Medium_30%_bricks",
                                    "substrate_16" = "Medium_5%_bricks",
                                    "substrate_17" = "Loam_30%_bricks",
                                    "substrate_18" = "Loam_5%_bricks"
                                    ))

### Save processed data ###
write_csv(data1, here("data", "processed",
                      "supp_data_processed_experiment_1.csv"))
write_csv(data2, here("data", "processed",
                      "supp_data_processed_experiment_2.csv"))
write_csv(data3, here("data", "processed",
                      "supp_data_processed_experiment_3.csv"))
