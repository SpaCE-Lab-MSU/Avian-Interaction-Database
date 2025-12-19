
library(dplyr)
library(tidyr)
library(diffdf)


source("./R/config.R")
file_paths <- get_file_paths()

s1 <- read.csv(paste0(file_paths$DATA_FOLDER, "/L0/ain_all_raw.csv"))
s2 <- read.csv(paste0(file_paths$DATA_FOLDER, "/L0/ain_all_raw_v0.1.csv"))

diffs <- diffdf(s1, s2)


d1 <- read.csv(paste0(file_paths$DATA_FOLDER, "/L1/ain_cac_breeding.csv"))
d2 <- read.csv(paste0(file_paths$DATA_FOLDER, "/L1/ain_cac_breeding_v0.1.csv"))

diffs <- diffdf(d1, d2)


# First difference of rows occurs with row 1124: Ross's goose -- df1 has it as
# Chen rossii, but the more updated name is Anser rossii which is in the
# new version of the data.


# d1 <- read.csv(paste0(file_paths$DATA_FOLDER, "/L0/AvianInteractionData_L0_stitch_20251211_kek.csv"))
d2 <- read.csv(paste0(file_paths$DATA_FOLDER, "/L0/AvianInteractionData_L0_stitch_20251212_jbb.csv"))
# d1 <- read.csv(paste0(file_paths$DATA_FOLDER, "/L0/AvianInteractionData_L0_stitch_LM.csv"))
d1 <- read.csv(paste0(file_paths$DATA_FOLDER, "/L0/AvianInteractionData_L0_stitch_11Dec2025_plz.csv"))
# d2 <- read.csv(paste0(file_paths$DATA_FOLDER, "/L0/AvianInteractionData_L0_stitch_06oct25.csv"))

start <- proc.time()
t <- waldo::compare(d1, d2)
proc.time() - start


diffs <- diffdf(d1, d2)
diffs


# d1 <- read.csv(paste0(file_paths$DATA_FOLDER, "/L1/AvianInteractionData_L1_20251212_LM.csv"))
# d2 <- read.csv(paste0(file_paths$DATA_FOLDER, "/L1/AvianInteractionData_L1_20251212_KEK.csv"))

diffs <- diffdf(d1, d2)
diffs

d1 %>% arrange(species1_common) -> o1
d2 %>% arrange(species1_common) -> o2

diffdf(o1, o2)



d1 <- read.csv(paste0(file_paths$DATA_FOLDER, "/L1/AvianInteractionData_L1_20251216_plz.csv"))
d2 <- read.csv(paste0(file_paths$DATA_FOLDER, "/L1/AvianInteractionData_L1_20251216_kek.csv"))

diffdf::diffdf(d1, d2)
dplyr::all_equal(d1, d2, ignore_row_order=TRUE)


d1 <- read.csv(paste0(file_paths$DATA_FOLDER, "/L1/ain_cac_breeding_v1.0.csv"))
d2 <- read.csv(paste0(file_paths$DATA_FOLDER, "/L1/ain_cac_breeding.csv"))
d3 <- read.csv(paste0(file_paths$DATA_FOLDER, "/L1/ain_cac_breeding_v0.1.csv"))


d1 <- read.csv(paste0(file_paths$DATA_FOLDER, "/L0/ain_all_raw.csv"))
d2 <- read.csv(paste0(file_paths$DATA_FOLDER, "/L0/AvianInteractionData_L0_stitch_20251216_plz.csv"))

diffdf::diffdf(d1, d2)
dplyr::all_equal(d1, d2, ignore_row_order=TRUE)



d1 <- read.csv(paste0(file_paths$DATA_FOLDER, "/L1/old_workflow_output/AvianInteractionData_CanadaAKCONUS_breeding_L1_20251216_plz.csv"))
d2 <- read.csv(paste0(file_paths$DATA_FOLDER, "/L1/ain_cac_breeding.csv"))

diffdf::diffdf(d1, d2)
dplyr::all_equal(d1, d2, ignore_row_order=TRUE)








t <- read.csv("./R/L1/manual_taxonomic_resolutions_L1_2025-10.csv")

t$scientific_name.raw %in% d2$species1_scientific
t$scientific_name.raw %in% d2$species2_scientific

temp <- t[which(t$scientific_name.raw %in% d2$species2_scientific),]



t$scientific_name.edit %in% d2$species1_scientific
t$scientific_name.edit %in% d2$species2_scientific


d1 <- read.csv(paste0(file_paths$DATA_FOLDER, "/L0/old_workflow_output/AvianInteractionData_L0_stitch_20251216_plz.csv"))
d2 <- read.csv(paste0(file_paths$DATA_FOLDER, "/L0/ain_all_raw.csv"))

diffdf::diffdf(d1, d2)
dplyr::all_equal(d1, d2, ignore_row_order=TRUE)


