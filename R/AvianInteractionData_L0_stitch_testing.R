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

library(readr, dplyr)

L0_dir <- "/Users/plz/Documents/GitHub/Avian-Interaction-Database/L0"
species_dir<- "/Users/plz/Documents/GitHub/Avian-Interaction-Database/L0/species"
species_review_dir<- "/Users/plz/Documents/GitHub/Avian-Interaction-Database/L0/species_in_review"
# Given a folder of Intxns CSV files, combine into one file
# This version can handle CSV files with some extra or different column names.
combine_by_species <- function(data_dir = file.path(species_dir)) {
  # Validate data directory
  if (is.null(data_dir)) {
    data_dir <- tempdir()
    warning(paste('No data_dir param, using temp folder:', data_dir))
  } else {
    if (!file.exists(data_dir)) {
      warning(paste("Directory unreachable:", data_dir))
      return(NULL)
    }
  }
  
  # Get list of CSV file paths in the directory
  csv_files <- list.files(path = data_dir, pattern = '.*\\.csv$', full.names = TRUE)
  
  # Define the standard column names
  standard_columns <- c(
    "species1_common", "species2_common", "species1_scientific", "species2_scientific",
    "effect_sp1_on_sp2", "effect_sp2_on_sp1", "interaction", "BOW_evidence", "n_studies",
    "sourceA_URL", "sourceB_URL", "sourceC_URL", "sourceD_URL", "nonbreedingseason",
    "notesA", "notesB", "notesC", "notesD", "recorder", "entry_date", "uncertain_interaction",
    "entry_changes", "name_changes", "other_species1", "DatabaseSearchURL"
  )
  
  # FUNCTION TO READ, RENAME COLUMNS, ADD MISSING COLUMNS, AND SELECT STANDARD COLUMNS
  read_and_process <- function(file) {
    # TRY TO READ THE CSV, CATCH ERRORS
    df <- tryCatch(
      {
        read.csv(file, stringsAsFactors = FALSE, na.strings = "")  # **SET stringsAsFactors = FALSE TO PREVENT ISSUES WITH FACTORS**
      },
      error = function(e) {
        warning(paste("Failed to read file:", file, "| Error:", e$message))
        return(data.frame(matrix(ncol = length(standard_columns), nrow = 0)))  # **RETURN EMPTY DATA FRAME ON ERROR**
      }
    )
    
    # CHECK IF DATA FRAME IS EMPTY
    if (nrow(df) == 0) {
      warning(paste("File is empty or could not be read:", file))  # **WARN IF FILE IS EMPTY OR NOT READABLE**
      return(data.frame(matrix(ncol = length(standard_columns), nrow = 0)))  # **RETURN EMPTY DATA FRAME WITH STANDARD COLUMNS**
    }
    
    # CHECK IF THE COLUMNS THAT NEED RENAMING EXIST AND RENAME THEM
    if ("sourceAupdatedURL" %in% names(df)) {
      names(df)[names(df) == "sourceAupdatedURL"] <- "sourceA_URL"  # **RENAME COLUMN IF EXISTS**
    }
    if ("sourceBupdatedURL" %in% names(df)) {
      names(df)[names(df) == "sourceBupdatedURL"] <- "sourceB_URL"  # **RENAME COLUMN IF EXISTS**
    }
    if ("sourceCupdatedURL" %in% names(df)) {
      names(df)[names(df) == "sourceCupdatedURL"] <- "sourceC_URL"  # **RENAME COLUMN IF EXISTS**
    }
    if ("sourceDupdatedURL" %in% names(df)) {
      names(df)[names(df) == "sourceDupdatedURL"] <- "sourceD_URL"  # **RENAME COLUMN IF EXISTS**
    }
    
    # ADD ANY MISSING STANDARD COLUMNS WITH NA VALUES
    missing_cols <- setdiff(standard_columns, names(df))  # **IDENTIFY MISSING COLUMNS**
    if (length(missing_cols) > 0) {
      df[missing_cols] <- NA  # **ADD MISSING COLUMNS AS NA**
    }
    
    # SELECT AND ORDER ONLY THE STANDARD COLUMNS
    df <- df %>% select(all_of(standard_columns))  # **SELECT STANDARD COLUMNS**
    
    return(df)
  }
  
  # Read and process all CSV files
  intxns.list <- lapply(csv_files, read_and_process)
  
  # Combine all into a single data frame
  intxns <- dplyr::bind_rows(intxns.list)  # **COMBINE DATA FRAMES**
  
  # Check unique values of n_studies
  unique_n_studies <- unique(intxns$n_studies)  # **GET UNIQUE VALUES OF n_studies**
  cat("Unique values of n_studies across all files:\n")
  print(unique_n_studies)  # **PRINT UNIQUE VALUES**
  
  # Convert n_studies to numeric
  intxns$n_studies <- as.numeric(intxns$n_studies)  # **CONVERT TO NUMERIC TYPE**
  
  # Return as a data frame
  return(data.frame(intxns))  # **RETURN FINAL DATA FRAME**
}


## VERSION FROM CHATGPT TO CHECK THE merging across the files:

library(dplyr)

combine_by_species <- function(data_dir = file.path(species_dir)) {
  # Validate data directory
  if (is.null(data_dir)) {
    data_dir <- tempdir()
    warning(paste('No data_dir param, using temp folder:', data_dir))
  } else {
    if (!file.exists(data_dir)) {
      warning(paste("Directory unreachable:", data_dir))
      return(NULL)
    }
  }
  
  # Get list of CSV file paths in the directory
  csv_files <- list.files(path = data_dir, pattern = '.*\\.csv$', full.names = TRUE)
  
  # Define the standard column names
  standard_columns <- c(
    "species1_common", "species2_common", "species1_scientific", "species2_scientific",
    "effect_sp1_on_sp2", "effect_sp2_on_sp1", "interaction", "BOW_evidence", "n_studies",
    "sourceA_URL", "sourceB_URL", "sourceC_URL", "sourceD_URL", "nonbreedingseason",
    "notesA", "notesB", "notesC", "notesD", "recorder", "entry_date", "uncertain_interaction",
    "entry_changes", "name_changes", "other_species1", "DatabaseSearchURL"
  )
  
  # FUNCTION TO READ, RENAME COLUMNS, ADD MISSING COLUMNS, AND SELECT STANDARD COLUMNS
  read_and_process <- function(file) {
    # TRY TO READ THE CSV, CATCH ERRORS
    df <- tryCatch(
      {
        read.csv(file, stringsAsFactors = FALSE, na.strings = "")  # **SET stringsAsFactors = FALSE TO PREVENT ISSUES WITH FACTORS**
      },
      error = function(e) {
        warning(paste("Failed to read file:", file, "| Error:", e$message))
        return(data.frame(matrix(ncol = length(standard_columns), nrow = 0)))  # **RETURN EMPTY DATA FRAME ON ERROR**
      }
    )
    
    # CHECK IF DATA FRAME IS EMPTY
    if (nrow(df) == 0) {
      warning(paste("File is empty or could not be read:", file))  # **WARN IF FILE IS EMPTY OR NOT READABLE**
      return(data.frame(matrix(ncol = length(standard_columns), nrow = 0)))  # **RETURN EMPTY DATA FRAME WITH STANDARD COLUMNS**
    }
    
    # CHECK IF THE COLUMNS THAT NEED RENAMING EXIST AND RENAME THEM
    if ("sourceAupdatedURL" %in% names(df)) {
      names(df)[names(df) == "sourceAupdatedURL"] <- "sourceA_URL"  # **RENAME COLUMN IF EXISTS**
    }
    if ("sourceBupdatedURL" %in% names(df)) {
      names(df)[names(df) == "sourceBupdatedURL"] <- "sourceB_URL"  # **RENAME COLUMN IF EXISTS**
    }
    if ("sourceCupdatedURL" %in% names(df)) {
      names(df)[names(df) == "sourceCupdatedURL"] <- "sourceC_URL"  # **RENAME COLUMN IF EXISTS**
    }
    if ("sourceDupdatedURL" %in% names(df)) {
      names(df)[names(df) == "sourceDupdatedURL"] <- "sourceD_URL"  # **RENAME COLUMN IF EXISTS**
    }
    
    # ADD ANY MISSING STANDARD COLUMNS WITH NA VALUES
    missing_cols <- setdiff(standard_columns, names(df))  # **IDENTIFY MISSING COLUMNS**
    if (length(missing_cols) > 0) {
      df[missing_cols] <- NA  # **ADD MISSING COLUMNS AS NA**
    }
    
    # SELECT AND ORDER ONLY THE STANDARD COLUMNS
    df <- df %>% select(all_of(standard_columns))  # **SELECT STANDARD COLUMNS**
    
    return(df)
  }
  
  # Read and process all CSV files
  intxns.list <- lapply(csv_files, read_and_process)
  
  # Initialize a list to store data type checks
  column_types <- list()
  
  # Collect data types for each column across all data frames
  for (col in standard_columns) {
    column_types[[col]] <- unique(sapply(intxns.list, function(df) {
      if (col %in% names(df)) {
        return(class(df[[col]]))
      } else {
        return(NA)
      }
    }))
  }
  
  # Identify and print columns with mismatched types
  mismatched_columns <- sapply(column_types, function(types) {
    length(na.omit(types)) > 1
  })
  
  if (any(mismatched_columns)) {
    cat("Columns with mismatched data types:\n")
    print(names(mismatched_columns[mismatched_columns]))
    
    for (col in names(mismatched_columns[mismatched_columns])) {
      cat(paste(col, "has types:", paste(column_types[[col]], collapse = ", "), "\n"))
    }
  }
  
  # Convert specific columns based on type checks
  intxns.list <- lapply(intxns.list, function(df) {
    # Convert n_studies to numeric, allowing for NAs
    df$n_studies <- as.numeric(df$n_studies)
    
    # Convert effect_sp1_on_sp2 to character
    df$effect_sp1_on_sp2 <- as.character(df$effect_sp1_on_sp2)
    
    # Convert effect_sp2_on_sp1 to character
    df$effect_sp2_on_sp1 <- as.character(df$effect_sp2_on_sp1)
    
    # Assign other character types for mismatched columns
    for (col in names(mismatched_columns[mismatched_columns])) {
      # Check for character, NA, or logical types without integer
      if (any(c("character", "logical", NA) %in% column_types[[col]]) && !("integer" %in% column_types[[col]])) {
        df[[col]] <- as.character(df[[col]])
      }
    }
    
    return(df)
  })
  
  # Re-check for mismatched data types after reassignment
  column_types <- list()  # Reset column_types
  for (col in standard_columns) {
    column_types[[col]] <- unique(sapply(intxns.list, function(df) {
      if (col %in% names(df)) {
        return(class(df[[col]]))
      } else {
        return(NA)
      }
    }))
  }
  
  # Identify and print columns with mismatched types again
  mismatched_columns <- sapply(column_types, function(types) {
    length(na.omit(types)) > 1
  })
  
  if (any(mismatched_columns)) {
    cat("After type assignment, columns with mismatched data types:\n")
    print(names(mismatched_columns[mismatched_columns]))
    
    for (col in names(mismatched_columns[mismatched_columns])) {
      cat(paste(col, "has types:", paste(column_types[[col]], collapse = ", "), "\n"))
    }
  }
  
  # Print unique values only for specified columns
  for (col in c("n_studies", "effect_sp1_on_sp2", "effect_sp2_on_sp1")) {
    if (col %in% names(intxns.list[[1]])) {
      unique_values <- unique(unlist(lapply(intxns.list, function(df) {
        if (col %in% names(df)) {
          return(df[[col]])
        } else {
          return(NULL)
        }
      })))
      cat(paste("Unique values in", col, ":", paste(unique_values, collapse = ", "), "\n"))
    }
  }
  
  # Combine all into a single data frame
  intxns <- dplyr::bind_rows(intxns.list)  # **COMBINE DATA FRAMES**
  
  # Check unique values of n_studies
  unique_n_studies <- unique(intxns$n_studies)  # **GET UNIQUE VALUES OF n_studies**
  cat("Unique values of n_studies across all files:\n")
  print(unique_n_studies)  # **PRINT UNIQUE VALUES**
  
  # Return as a data frame
  return(data.frame(intxns))  # **RETURN FINAL DATA FRAME**
}

# Usage Example
intxnsL0sp <- combine_by_species()



combine_by_species_in_review <- function(data_dir = file.path(species_review_dir)) {
  
  #
  # for each csv file in data path
  # read and validate (check columns)
  # attempt to append to end
  if (is.null(data_dir)) {
    data_dir = tempdir()
    warning(paste(' no data_dir param, using temp folder ', data_dir))
  }
  else {
    if(!file.exists(data_dir)){
      warning(paste("directory unreachable", data_dir))
      return(NULL)
    }
  }
  
  
  # get list of file names
  csv_files <- list.files(path = data_dir, pattern = '.*\\.csv')
  
  # add relative paths to file names in list
  csv_files <- file.path(data_dir, csv_files)
  
  # use apply to read all files into a list
  intxns.list <- lapply(csv_files, read.csv)
  #intxns.list <- lapply(csv_files, read_csv, col_types = cols(.default = col_guess()))
  
  # combine all in to a single df
  intxns <- dplyr::bind_rows(intxns.list)
  
  # return as a data frame (the operation above is a list type)
  return(data.frame(intxns))
}

# Species Fully Checked
intxnsL0sp<-combine_by_species()
sp<-unique(intxnsL0sp$species1_common)
sp<-as.list(sp)
length(sp)
# 997 species1 as of Aug 8, 2024 (all double checked)

## Species In Review (all BBS species originally entered by Emily Parker)
intxnsL0spir<-combine_by_species_in_review()
spir<-unique(intxnsL0spir$species1_common)
spir<-as.list(spir)
length(spir)
# 389 species1 as of Aug 8, 2024

# Uncomment if you want to omit all species that haven't been checked by someone
# other than Emily, India; for Aug 2024 we are just proceeding with all 'species' and
# 'species_in_review' because Emily is experienced and entered the BBS birds in
# the review folder. So keep the below section commented out. Only keep unique
# set of species; remove the "in review" species that have already been checked
# in updated in "species" '%!in%'<- function(x,y)!('%in%'(x,y))
#
# intxnsL0spir_unchecked<-intxnsL0spir[intxnsL0spir[,3] %!in% sp,]
# length(unique(intxnsL0spir_unchecked$species1_scientific)) XXX unique species1
# that are truly unchecked

# Merge the species and species_in_review interaction data into 1 (as of Dec 21,
# 2023, EP created all the "in_review" species for BBS, so they are less likely
# to include errors):
intxnsL0<-rbind(intxnsL0sp, intxnsL0spir)

length(unique(intxnsL0$species1_scientific))
# 1198 unique species1 as of Aug 8, 2024 (these include some non-BBS species1).
# Note that some species1 in a given species1 csv could also be other species
# because of entering many pair-wise interactions in, for example, mixed flock
# entry. Any duplicates will be omitted later.

# export the data to become the current L0 interaction data:
write.csv(intxnsL0, file.path(L0_dir, "AvianInteractionData_L0.csv"), row.names=FALSE)

