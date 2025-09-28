# filepaths.R
# this is an R script that all the other scripts use to set the locations of
# data files for must have the following variables set to match your
# computers file paths
# the script file filepaths.R should not be kept in git, only the example files.

# the process to create your own copy of this file is:
# 1. open filepaths_example.R (the file that has examplar variables)
# 2. save as filepaths.R
# 3. edit filespaths.R variables to match your computer
#    this is an R script and uses R syntax, not a config or environment file
# 4. in scripts use source(here::here('R/config.R')) and file_paths = get_file_config()

# see readme.md for details and R/config.R for script that reads these in
# by convention configuration variables (constants) are uppercase

SYNC_DATA_FOLDER <- "/path/to/google_drive" #or OneDrive folder with spreadsheets
# the L0 and L1 data folders are under this one:
DATA_FOLDER <-  "/path/to/data_respository/top_folder"
# for L0 checklist (Clements etc)
CHECKLIST_FOLDER = file.path(DATA_FOLDER, "L0", "species_checklists")
# currently using hard-coded folder path for curated L1 checklist

# add comments or save alternate paths using comments, for example
# old data folder
# DATA_FOLDER   "/Users/billspat/SpaCELab/atx/Avian-Interaction-Database-Working"
