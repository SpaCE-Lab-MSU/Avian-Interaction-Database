
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
