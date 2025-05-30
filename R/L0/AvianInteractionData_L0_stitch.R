# TITLE:          Avian Interaction Pairs L0 Data Stitching together CSVs into 1
# AUTHORS:        Phoebe Zarnetske, Pat Bills, Emily Parker
# COLLABORATORS:  Vincent Miele, Stephane Dray
# DATA INPUT:     Data imported as csv https://github.com/SpaCE-Lab-MSU/Avian-Interaction-Database/blob/main/L0/species and ./species_in_review
# DATA OUTPUT:    L1 data: AvianInteractionData_L0.csv
# PROJECT:        Avian Interaction Database & Avian Meta-Network
# DATE:           20 Mar 2023 - 9 Dec 2024 - 29 May 2025 (last run on this date)
# NOTES:          Next script to run: /L1/AvianInteractionData_L1.R

# Clear all existing data
rm(list=ls())

#library(readr, dplyr, purrr)
library(dplyr)
library(tidyverse)

google_drive_folder <- "~/Google Drive/Shared drives/Avian_MetaNetwork/data/L0/avian_intxn_data/species_entry/EP"
L0_dir <- "/Users/plz/Documents/GitHub/Avian-Interaction-Database-Working/L0"
species_dir<- "/Users/plz/Documents/GitHub/Avian-Interaction-Database-Working/L0/species"
species_in_review_dir<- "/Users/plz/Documents/GitHub/Avian-Interaction-Database-Working/L0/species_in_review"

# 12/9/2024: some EP gsheets still need checking but contain useful data. We
# don't want to miss any in /species/ that are only in gsheet form.

# Set the directory paths for the Google Drive and Git repository folders
git_repo_folder <- species_dir

# List all filenames in both folders
#google_drive_files <- list.files(google_drive_folder, full.names = FALSE)
git_repo_files <- list.files(git_repo_folder, full.names = FALSE)

# Function to extract the main portion of filenames
# Extract up to the second underscore (if present) or use the entire name
get_main_name <- function(filename) {
  filename %>%
    tools::file_path_sans_ext() %>%  # Remove file extension
    tolower() %>%                    # Convert to lowercase
    str_extract("^[^_]+_[^_]+")      # Extract up to the second underscore
}

# Extract main names from filenames
google_drive_main_names <- sapply(google_drive_files, get_main_name)
git_repo_main_names <- sapply(git_repo_files, get_main_name)

# Find common names (intersection) and unique names (differences)
common_main_names <- intersect(google_drive_main_names, git_repo_main_names)
google_only_main_names <- setdiff(google_drive_main_names, git_repo_main_names)
git_only_main_names <- setdiff(git_repo_main_names, google_drive_main_names)

# Print results - not run May 29, 2025
cat("Files with matching main names in both Google Drive and Git repository:\n")
print(common_main_names)
# [1] "molothrus_aeneus"   "molothrus_ater"     "picoides_dorsalis"  "setophaga_coronata"

cat("\nFiles only in Google Drive:\n")
print(google_only_main_names)

cat("\nFiles only in Git repository:\n")
print(git_only_main_names)

# As of Dec 9, 2024: 27 species' csvs that still need the 2nd checking and
# placing into /species/. Of these, only 4 already exist in /species/. So use
# the temp species directory also (contains CSVs that were exported on Nov 27,
# 2024 from the Google Sheets)

####*****COMBINE INDIVIDUAL CSVs *****####
# Given a folder of Intxns CSV files, combine into one file
# This version can handle CSV files with some extra or different column names.

combine_by_species <- function(data_dir = "species_dir") {
  
  # Set the path based on the selected directory
  if (data_dir == "species_dir") {
    csv_dir <- file.path(species_dir)
    output_name <- "intxnsL0sp"  # output for species_dir
  } else if (data_dir == "species_temp_dir") {
    csv_dir <- file.path(species_temp_dir)
    output_name <- "intxnsL0sptemp"  # output for species_temp_dir
    } else if (data_dir == "species_in_review_dir") {
    csv_dir <- file.path(species_in_review_dir)
    output_name <- "intxnsL0spir"  # output for species_in_review_dir
  } else {
    stop("Invalid data_dir specified. Use either 'species_dir' or 'species_temp_dir' or 'species_in_review_dir'.")
  }
  
  # Validate the csv_dir path
  if (!file.exists(csv_dir)) {
    warning(paste("Directory unreachable:", csv_dir))
    return(NULL)
  }
  
  # Get list of CSV file paths in the directory, and define standard columns in them
  csv_files <- list.files(path = csv_dir, pattern = '.*\\.csv$', full.names = TRUE)
  standard_columns <- c("species1_common", "species2_common", "species1_scientific", "species2_scientific",
                        "effect_sp1_on_sp2", "effect_sp2_on_sp1", "interaction", "BOW_evidence", "n_studies",
                        "sourceA_URL", "sourceB_URL", "sourceC_URL", "sourceD_URL", "nonbreedingseason",
                        "notesA", "notesB", "notesC", "notesD", "recorder", "entry_date", "uncertain_interaction",
                        "entry_changes", "name_changes", "other_species1", "DatabaseSearchURL")
  
  
  # FUNCTION TO READ, RENAME COLUMNS, ADD MISSING COLUMNS, AND SELECT STANDARD COLUMNS
  read_and_process <- function(file) {
    # TRY TO READ THE CSV, CATCH ERRORS
    df <- tryCatch(read.csv(file, stringsAsFactors = FALSE, na.strings = ""),
                   error = function(e) {
                     warning(paste("Failed to read file:", file, "| Error:", e$message))
                     return(NULL)
                   })
    
    if (is.null(df) || nrow(df) == 0) return(NULL)  # Skip empty or unreadable files
    
    # Rename columns individually to avoid duplication issues
    if ("sourceAupdatedURL" %in% names(df)) names(df)[names(df) == "sourceAupdatedURL"] <- "sourceA_URL"
    if ("sourceBupdatedURL" %in% names(df)) names(df)[names(df) == "sourceBupdatedURL"] <- "sourceB_URL"
    if ("sourceCupdatedURL" %in% names(df)) names(df)[names(df) == "sourceCupdatedURL"] <- "sourceC_URL"
    if ("sourceDupdatedURL" %in% names(df)) names(df)[names(df) == "sourceDupdatedURL"] <- "sourceD_URL"
    
    # ADD ANY MISSING STANDARD COLUMNS WITH NA VALUES
    missing_cols <- setdiff(standard_columns, names(df))
    df[missing_cols] <- NA
    df <- df %>% select(all_of(standard_columns)) # SELECT STANDARD COLUMNS
    return(df)
  }
  
  # Read and process all CSV files; Filter out NULL elements (empty files) from intxns.list and csv_files
  intxns.list <- Filter(Negate(is.null), lapply(csv_files, read_and_process))
  # Now, `intxns.list` only contains non-empty files
  
  # Identify empty files (where NULL returned) and remove them
  empty_files <- csv_files[sapply(intxns.list, is.null)]
  if (length(empty_files) > 0) {
    cat("Empty files detected and omitted from processing:\n")
    print(empty_files)
  }
  
  # Convert columns to appropriate data types
  convert_columns <- function(df) {
    char_cols <- c("species1_common", "species2_common", "species1_scientific", "species2_scientific",
                   "interaction", "BOW_evidence", "sourceA_URL", "sourceB_URL", "sourceC_URL", "sourceD_URL",
                   "nonbreedingseason", "notesA", "notesB", "notesC", "notesD", "recorder", "entry_date",
                   "uncertain_interaction", "entry_changes", "name_changes", "other_species1", "DatabaseSearchURL")
    df[char_cols] <- lapply(df[char_cols], as.character)
    df$n_studies <- as.numeric(df$n_studies)
    df$effect_sp1_on_sp2 <- as.integer(df$effect_sp1_on_sp2)
    df$effect_sp2_on_sp1 <- as.integer(df$effect_sp2_on_sp1)
    return(df)
  }
  
  intxns.list.fix <- lapply(intxns.list, convert_columns)
  
  # Pre-binding unique value summaries
  pre_binding_summary <- lapply(c("n_studies", "effect_sp1_on_sp2", "effect_sp2_on_sp1"), function(col) {
    all_values <- unlist(lapply(intxns.list.fix, function(df) if (col %in% names(df)) df[[col]] else NULL))
    list(unique = unique(all_values), summary = summary(all_values), count = table(all_values, useNA = "ifany"))
  })
  names(pre_binding_summary) <- c("n_studies", "effect_sp1_on_sp2", "effect_sp2_on_sp1")
  
  # Combine all into a single data frame
  intxns <- dplyr::bind_rows(intxns.list.fix)  # **COMBINE DATA FRAMES**
  
  # Assign the output to the respective global variable based on input directory
  if (data_dir == "species_dir") {
    assign("intxnsL0sp", intxns, envir = .GlobalEnv)
  } else if (data_dir == "species_temp_dir") {
    assign("intxnsL0sptemp", intxns, envir = .GlobalEnv)
  } else if (data_dir == "species_in_review_dir") {
    assign("intxnsL0spir", intxns, envir = .GlobalEnv)
  }
  
  # Post-binding unique value summaries
  post_binding_summary <- lapply(c("n_studies", "effect_sp1_on_sp2", "effect_sp2_on_sp1"), function(col) {
    all_values <- intxns[[col]]
    list(unique = unique(all_values), summary = summary(all_values), count = table(all_values, useNA = "ifany"))
  })
  names(post_binding_summary) <- c("n_studies", "effect_sp1_on_sp2", "effect_sp2_on_sp1")
  
  # Display pre- and post-binding summaries
  cat("Comparison of Pre- and Post-Binding Values\n")
  for (col in names(pre_binding_summary)) {
    cat(paste("\nColumn:", col, "\n"))
    cat("Pre-Binding Summary:\n")
    print(pre_binding_summary[[col]]$summary)
    cat("Post-Binding Summary:\n")
    print(post_binding_summary[[col]]$summary)
    
    cat("Pre-Binding Unique Counts:\n")
    print(pre_binding_summary[[col]]$count)
    cat("Post-Binding Unique Counts:\n")
    print(post_binding_summary[[col]]$count)
  }
  
  return(intxns)
}

# Option 1: For `species_dir` - the Fully Checked Species
intxnsL0sp <- combine_by_species("species_dir")

# Option 2: For `species_in_review_dir`
intxnsL0spir <- combine_by_species("species_in_review_dir")

# Species Fully Checked
sp<-unique(intxnsL0sp$species1_common)
sp<-as.list(sp)
length(sp)
# 1266 species1 as of Dec 9, 2024 (all double checked)
# 1305 species1 as of May 29, 2025 (almost all double checked)

# Species Temp: Not Fully Checked BBS species
# sptemp<-unique(intxnsL0sptemp$species1_common)
# sptemp<-as.list(sptemp)
# length(sptemp)
## 170 species1 as of Dec 9, 2024 (not all double checked)

## Species In Review 
spir<-unique(intxnsL0spir$species1_common)
spir<-as.list(spir)
length(spir)
# 830 species1 as of Dec 9, 2024 (none double-checked)
# 873 species1 as of May 29, 2025 (none double-checked)

# Use only the Species Fully Checked:
intxnsL0<-intxnsL0sp

# Uncomment to merge the species and species temp interaction data into 1 
#intxnsL0<-rbind(intxnsL0sp, intxnsL0sptemp)

# Remove duplicate rows (these may be from the 4 species occurring in both the GSheets and /species/ folders)
intxnsL0 <- intxnsL0 %>% 
  distinct()

# Uncomment to merge the species and species_in_review interaction data into 1 
#intxnsL0<-rbind(intxnsL0sp, intxnsL0spir)

# Note that some species1 in a given species1 csv could also be other species
# because of entering many pair-wise interactions in, for example, mixed flock
# entry. Any duplicates will be omitted later.

# export the data to become the current L0 interaction data:
write.csv(intxnsL0, file.path(L0_dir, "AvianInteractionData_L0_29May2025.csv"), row.names=FALSE)

