# filepaths.R
# This is an R script that all the other scripts use to set the locations of
# data files. You must have the following variables set to match your
# computer's file paths.
# The script file 'filepaths.R' should not be kept in git, only the example files.
# This file is included in the gitignore file so that it will not be synced with
# this repository.

# The process to create your own copy of this file is:
# 1. open 'filepaths_example.R' (the file that has example variables).
# 2. save as 'filepaths.R'.
# 3. edit filespaths.R variables to match your computer;
#    this is an R script and uses R syntax, not a config or environment file.
# 4. in scripts use source(here::here('R/config.R')) and file_paths = get_file_config()

# See readme.md for details and R/config.R for script that reads these in.
# By convention configuration variables (constants) are uppercase.

# Paths to set and replace "filepath" with your computer's filepath:
# The L0 and L1 data folders are within the Avian-Interaction-Working repository's folder:
DATA_FOLDER <-  "/path/to/data_respository/top_folder"
# Path for for L0 checklists (Clements etc):
CHECKLIST_FOLDER = file.path(DATA_FOLDER, "L0", "species_checklists")
# Note: currently using hard-coded folder path for curated L1 checklist.

# Add comments or save alternate paths using comments, for example:
# Path for old data folder:
# DATA_FOLDER   "/Users/billspat/SpaCELab/atx/Avian-Interaction-Database-Working"
