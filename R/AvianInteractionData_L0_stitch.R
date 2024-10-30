# TITLE:          Avian Interaction Pairs L0 Data Stitching together CSVs into 1
# AUTHORS:        Phoebe Zarnetske, Pat Bills, Emily Parker
# COLLABORATORS:  Vincent Miele, Stephane Dray
# DATA INPUT:     Data imported as csv https://github.com/SpaCE-Lab-MSU/Avian-Interaction-Database/blob/main/L0/species and ./species_in_review
# DATA OUTPUT:    L1 data: AvianInteractionData_L0.csv
# PROJECT:        Avian Interaction Database & Avian Meta-Network
# DATE:           20 Mar 2023 -
# NOTES:          Next script to run: /L1/AvianInteractionData_L1.R

# Clear all existing data
rm(list=ls())

#library(readr, dplyr, purrr)
library(dplyr)

L0_dir <- "/Users/plz/Documents/GitHub/Avian-Interaction-Database/L0"
species_dir<- "/Users/plz/Documents/GitHub/Avian-Interaction-Database/L0/species"
species_in_review_dir<- "/Users/plz/Documents/GitHub/Avian-Interaction-Database/L0/species_in_review"
# Given a folder of Intxns CSV files, combine into one file
# This version can handle CSV files with some extra or different column names.

combine_by_species <- function(data_dir = "species_dir") {
  
  # Set the path based on the selected directory
  if (data_dir == "species_dir") {
    csv_dir <- file.path(species_dir)
    output_name <- "intxnsL0sp"  # output for species_dir
  } else if (data_dir == "species_in_review_dir") {
    csv_dir <- file.path(species_in_review_dir)
    output_name <- "intxnsL0spir"  # output for species_in_review_dir
  } else {
    stop("Invalid data_dir specified. Use either 'species_dir' or 'species_in_review_dir'.")
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
# 1171 species1 as of October 30, 2024 (all double checked)

## Species In Review 
spir<-unique(intxnsL0spir$species1_common)
spir<-as.list(spir)
length(spir)
# 659 species1 as of October 30, 2024

# Use only the Species Fully Checked:
intxnsL0<-intxnsL0sp

# Uncomment to merge the species and species_in_review interaction data into 1 
#intxnsL0<-rbind(intxnsL0sp, intxnsL0spir)

# Note that some species1 in a given species1 csv could also be other species
# because of entering many pair-wise interactions in, for example, mixed flock
# entry. Any duplicates will be omitted later.

# export the data to become the current L0 interaction data:
write.csv(intxnsL0, file.path(L0_dir, "AvianInteractionData_L0.csv"), row.names=FALSE)

