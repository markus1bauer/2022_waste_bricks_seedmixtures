# Brick-based substrates and designed seedmixtures
### Script to prepare data of experiment 1 and 2 grain size distribution ####
# Markus Bauer
# 2022-01-24
# Citation: 
## Bauer M, Krause M, Heizinger V, Kollmann J (submitted) 
## Using waste bricks for recultivation: no negative effects of brick-augmented substrates with varying acid pre-treatment, soil type and moisture on contrasting seed mixtures
## Unpublished data.




### Packages ###
library(here)
library(tidyverse)

### Start ###
rm(list = ls())
setwd(here("data/processed"))

### Load data ###
edata <- read_table2("supp_data_raw_experiment_1_2_3.txt", col_names = T, na = "na", col_types = 
                       cols(
                         .default = "d"
                       )) %>%
  gather("substrate", "ratio", 2:13) %>%
  group_by(substrate) %>%
  mutate(grainSizeCum = cumsum(ratio))

### Create data frame for experiment 1 ###
edata1 <- filter(edata, substrate == "substrate_7" | substrate == "substrate_8" | substrate == "substrate_9" | substrate == "substrate_10")
edata1$substrateAbb <- dplyr::recode(edata1$substrate,
                                    "substrate_7" = "30%_bricks_Acid",
                                    "substrate_8" = "5%_bricks_Acid",
                                    "substrate_9" = "30%_bricks_Control",
                                    "substrate_10" = "5%_bricks_Control",
                                    )

### Create data frame for experiment 2 ###
edata2 <- filter(edata, substrate == "substrate_7" | substrate == "substrate_8" | substrate == "substrate_11" | substrate == "substrate_12")
edata2$substrateAbb <- dplyr::recode(edata2$substrate,
                                    "substrate_7" = "Clean_bricks_30%",
                                    "substrate_8" = "Clean_bricks_5%",
                                    "substrate_11" = "Demolition_bricks_30%",
                                    "substrate_12" = "Demolition_bricks_5%"
                                    )

### Create data frame for experiment 3 ###
edata3 <- filter(edata, substrate == "substrate_13" | substrate == "substrate_14" | substrate == "substrate_15" | substrate == "substrate_16" | substrate == "substrate_17" | substrate == "substrate_18")
edata3$substrateAbb <- dplyr::recode(edata3$substrate,
                                    "substrate_13" = "Sand_30%_bricks",
                                    "substrate_14" = "Sand_5%_bricks",
                                    "substrate_15" = "Medium_30%_bricks",
                                    "substrate_16" = "Medium_5%_bricks",
                                    "substrate_17" = "Loam_30%_bricks",
                                    "substrate_18" = "Loam_5%_bricks"
                                    )

### Save processed data ###
write.table(edata1, here("data/processed/supp_data_processed_experiment_1.txt"), sep = "\t", row.names = F)
write.table(edata2, here("data/processed/supp_data_processed_experiment_2.txt"), sep = "\t", row.names = F)
write.table(edata3, here("data/processed/supp_data_processed_experiment_3.txt"), sep = "\t", row.names = F)
