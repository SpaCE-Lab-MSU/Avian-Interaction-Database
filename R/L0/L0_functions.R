# L0_functions.R
# TITLE:          Avian Interaction Pairs L0 Data Stitching together CSVs into 1
# AUTHORS:        Phoebe Zarnetske, Pat Bills, Emily Parker
# COLLABORATORS:  Vincent Miele, Stephane Dray
# DATA INPUT:     Data entry CSV files in L0 folder
# DATA OUTPUT:    L0 combined data: AvianInteractionData_L0.csv
# PROJECT:        Avian Interaction Database & Avian Meta-Network
# DATE:           20 Mar 2023 - August Dec 2025)
# NOTES:          Functions here are used in R/L0/L0_stitch.qmd notebook, see run date there

# this sets file paths using config file. See readme for details
suppressMessages(require(here))
source(here::here('R/config.R'))
library(dplyr)
library(readr)
library(magrittr)
library(stringr)

# set the global variable with list of file paths for use in all functions
# this is set here to ensure that it's set and check that a config file was
# created.

# to use a different set of files paths (e.g. for testing)
# put this statement in your script or notebook after sourcing this file
# and put the alternate config files,
# for example, at the top of your script...
# source(here::here("L0_functions.R")
# file_paths <- get_file_paths(here::here('testdata.R'))

file_paths <- get_file_paths() # get_file_config()

# configuration of the where to find this file
clements2024.url <- 'https://www.birds.cornell.edu/clementschecklist/wp-content/uploads/2024/10/Clements-v2024-October-2024-rev.csv'

clements_urls <- list(
  '2024' = 'https://www.birds.cornell.edu/clementschecklist/wp-content/uploads/2024/10/Clements-v2024-October-2024-rev.csv',
  '2023' = '',
  '2022' = 'https://www.birds.cornell.edu/clementschecklist/wp-content/uploads/2022/12/Clements-Checklist-v2022.csv'
)

#' read or download
#' read csv but don't re-download if we have it
#' @param csv.url string url to download
#' @param col_types Optional list of coltypes to use for readr::read_csv function
#'
#' @returns data frame of csv
#'
read_or_download_csv <- function(csv.url, col_types = NULL) {
    csv.file <- basename(csv.url)
    # this works on Mac, how about on Windows?
    home_folder <- Sys.getenv('HOME')
    csv.file <- file.path(home_folder, 'Downloads', csv.file)

    if(!exists(csv.file)) {
    curl::curl_download(csv.url, destfile = csv.file)
    }

    if(is.null(col_types)) {
    references.df <- readr::read_csv(csv.file)
    } else {
    references.df <- readr::read_csv(csv.file, col_types =  col_types)
    }
    return(references.df)
}



clements2024.cols <- readr::cols(
  `sort v2024` = col_character(),
  `species_code` = col_character(),
  `Clements v2024b change` = col_character(),
  `text for website v2024b` = col_character(),
  `category` = col_character(),
  `English name` = col_character(),
  `scientific name` = col_character(),
  `range` = col_character(),
  `order` = col_character(),
  `family` = col_character(),
  `extinct` = col_logical(),
  `extinct year` = col_character(),
  sort_v2023 = col_character()
)

#' documented function for downloading bird checklist from cornell
#'
#' the default is to read the 2024 version of this checklist, but
#' update these for future years.
read_latest_clements<-function(url = clements2024.url,col_types = clements2024.cols){
  clements.df <- read_or_download_csv(url,col_types)
  print(problems(clements.df))
  print(spec(clements.df))
  return(clements.df)
}

#' extract the species portion of CSV filename
#'
#' Files are names for the species the data is about,
#' Extract up to the second underscore (if present) or use the entire name
#' @param filename character file name (with our without a path)
#'
#' @return character first part of filename with underscore removed, lowercase
get_main_name <- function(filename) {
  filename %>%
    basename() %>%
    tools::file_path_sans_ext() %>%  # Remove file extension
    tolower() %>%                    # Convert to lowercase
    str_extract("^[^_]+_[^_]+") %>% # Extract up to the second underscore
    stringr::str_replace_all("_", " ") # make it a space
}

#' given a list of files, get just the names
#'
#' list of files by default is the default file list for the project
get_main_names_folder <- function(files = list_csvs()){
  # files <- list.files(folder_with_csvs, full.names = FALSE)
  main_names <- sapply(files, get_main_name)
  return(main_names )
}


##### UPDATE IF COLUMNS CHANGE

standard_columns<- c(
    "species1_common",
    "species2_common",
    "species1_scientific",
    "species2_scientific",
    "effect_sp1_on_sp2",
    "effect_sp2_on_sp1",
    "interaction",
    "BOW_evidence",
    "n_studies",
    "sourceA_URL",
    "sourceB_URL",
    "sourceC_URL",
    "sourceD_URL",
    "nonbreedingseason",
    "notesA",
    "notesB",
    "notesC",
    "notesD",
    "recorder",
    "entry_date",
    "uncertain_interaction",
    "entry_changes",
    "name_changes",
    "other_species1",
    "DatabaseSearchURL"
)

# standard columns are those used after stitching
# the following columns are in the data entry list but not in the std columns
# "OLDsourceA"
# "OLDsourceB"
# "name_changes",
# "other_species1",
# "DatabaseSearchURL"


##### UPDATE IF COLUMNS CHANGE

dataentry_columns <- c(
  "species1_common",
  "species2_common",
  "species1_scientific",
  "species2_scientific",
  "effect_sp1_on_sp2",
  "effect_sp2_on_sp1",
  "interaction",
  "BOW_evidence",
  "n_studies",
  "OLDsourceA",
  "OLDsourceB",
  "sourceAupdatedURL",
  "sourceBupdatedURL",
  "sourceCupdatedURL",
  "sourceDupdatedURL",
  "nonbreedingseason",
  "notesA",
  "notesB",
  "notesC",
  "notesD",
  "recorder",
  "entry_date",
  "uncertain_interaction",
  "entry_changes"
)

##### DATA ENTRY COLUMN SPECS FOR READING
##### UPDATE IF COLUMNS CHANGE

avian_intxn_column_spec <-
  readr::cols(
    species1_common = readr::col_character(),
    species2_common = readr::col_character(),
    species1_scientific = readr::col_character(),
    species2_scientific = readr::col_character(),
    effect_sp1_on_sp2 = readr::col_integer(),
    effect_sp2_on_sp1 = readr::col_integer(),
    interaction = readr::col_character(),
    BOW_evidence = readr::col_character(),
    n_studies = readr::col_integer(),
    OLDsourceA = readr::col_character(),
    OLDsourceB = readr::col_character(),
    sourceAupdatedURL = readr::col_character(),
    sourceBupdatedURL = readr::col_character(),
    sourceCupdatedURL = readr::col_character(),
    sourceDupdatedURL = readr::col_character(),
    nonbreedingseason = readr::col_character(),
    notesA = readr::col_character(),
    notesB = readr::col_character(),
    notesC = readr::col_character(),
    notesD = readr::col_character(),
    recorder = readr::col_character(),
    entry_date = readr::col_character(),
    uncertain_interaction = readr::col_character(),
    entry_changes = readr::col_character()
  )

#' check columns in data entry sheet
#'
#' currently this only prints warning if columns deviate from the list of
#' data entry columns above, that is missing or extra columns
#' this is useful for discovering problems only and doesn't fix

#' @param df data frame from 'raw' data entry csv
#' @returns TRUE if all columns are present even if there are extra,
#'          FALSE if any are missing
check_dataentry_columns <- function(intxns.df){
  # first add in the "optional" columns: B,C,D versions of url and notes

  if (! "sourceBupdatedURL" %in% names(intxns.df)) { intxns.df$sourceBupdatedURL <- NA }
  if (! "sourceCupdatedURL" %in% names(intxns.df)) { intxns.df$sourceCupdatedURL <- NA }
  if (! "sourceDupdatedURL" %in% names(intxns.df)) { intxns.df$sourceDupdatedURL <- NA }

  if (! "notesB" %in% names(intxns.df)) { intxns.df$notesB <- NA }
  if (! "notesC" %in% names(intxns.df)) { intxns.df$notesC <- NA }
  if (! "notesD" %in% names(intxns.df)) { intxns.df$notesD <- NA }

  no_missing_columns <- TRUE
  missing_dataentry_cols <- setdiff(dataentry_columns, names(intxns.df))
  if(length(missing_dataentry_cols)>0){
    msg <- paste("columns missing:", missing_dataentry_cols)
    warning(msg)
    no_missing_columns <- FALSE
  }

  # we don't really care about extra columns as those are ok to be missing
  extra_dataentry_cols <- setdiff(names(df), dataentry_columns)
  if(length(extra_dataentry_cols)>0){
    msg = paste("extra columns: ", extra_dataentry_cols)
    warning(msg)

  }

  return(no_missing_columns)
}


##### UPDATE IF COLUMNS CHANGE

#' Convert columns to appropriate data types
#'
#' given a list of columns in the data entry sheet that must be character,
#' converts those to char. to ensure if a number is read in it becomes char. and
#' ensures a handful of numeric columns are forced to be numeric
#'
#' This function has the names of the columns hard coded and so must be edited
#' if the columns change
#'
#' this is not needed if using the readr::read_csv with a spec
#' @param df interaction data frame, amended after reading in (eg. col names
#' updated)
#' @returns data frame with character columns as needed
amend_intxn_columns <- function(df) {
  # see standard_columns data above


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


# FUNCTION TO READ, RENAME COLUMNS, ADD MISSING COLUMNS, AND SELECT STANDARD COLUMNS

#' read avian interaction db data entry file and process
#'
#' amend the raw data files by altering the columns to accommodate
#' combining into single table
read_and_amend <- function(file) {
  # TRY TO READ THE CSV, CATCH ERRORS

  df <- tryCatch(
  # use readr and create a var with column names and types that readr can use
      readr::read_csv(file, col_types = avian_intxn_column_spec),
      error = function(e) {
        warning(paste("Failed to read file:", file, "| Error:", e$message))
        return(NA)
      }
    )
  problems.df <- problems(df)
  if(nrow(problems.df) > 0){
    # print(file)
    print(problems.df)
  }

  # this prints the file name if there are problems, but it will keep
  # going anyway.

  all_cols_present <- check_dataentry_columns(df)
  if(!all_cols_present){ print(file)}

  if (is.null(df) || nrow(df) == 0) return(NULL)  # Skip empty or unreadable files

  # Rename columns individually to avoid duplication issues
  if ("sourceAupdatedURL" %in% names(df)) names(df)[names(df) == "sourceAupdatedURL"] <- "sourceA_URL"
  if ("sourceBupdatedURL" %in% names(df)) names(df)[names(df) == "sourceBupdatedURL"] <- "sourceB_URL"
  if ("sourceCupdatedURL" %in% names(df)) names(df)[names(df) == "sourceCupdatedURL"] <- "sourceC_URL"
  if ("sourceDupdatedURL" %in% names(df)) names(df)[names(df) == "sourceDupdatedURL"] <- "sourceD_URL"


  # ADD ANY MISSING STANDARD COLUMNS WITH NA VALUES
  missing_cols <- setdiff(standard_columns, names(df))
  df[missing_cols] <- NA
  df <- df %>% dplyr::select(dplyr::all_of(standard_columns)) # SELECT STANDARD COLUMNS

  df<- amend_intxn_columns(df)
  return(df)
}


#' count unique species
#'
#' given a data frame and column to count, run n unique species
#' actually works on any data frame and character column but
#' defaults to intxn species col
#' @param intxns.df data frame containing the column specied
#' @param species_column name of the column to count, default first species
#' @returns n integer count of unique values
unique_speces_count <- function(intxns.df, species_column="species1_common" ){
  n <-intxns.df[[species_column]] |> unique() |> length()
  return(n)
}

#' get list of files that are 'checked'
#'
#' very simple function, just allows for using config for locating files
#'
#' @param file_paths list of paths to data with element L0 from config
#'  default is null, will use the configuration methods to read from config.yml
#' @param sub_folder the folder to look in, default species => check files
#'    override this to read some temporary or other folders
#'
#'@returns
get_data_file_list <- function(file_paths = NULL, sub_folder = "species"){
  # file_paths can be sent, but by default will look in config.yml
  if(is.null(file_paths)){
    file_paths = get_file_config()
  }

  # this is used in notebooks but here only for printing message
  csv_file_group_name='checked'
  # assemble the paths and get the list
  # sub_folder is pre-defined by protocol, "species" ==> checked files
  csv_file_path = file.path(file_paths$L0, sub_folder)
  # filter or add in google drive files here
  checked_file_list <- list.files(path = csv_file_path , pattern = ".*\\.csv",
    full.names = TRUE)

  print(paste(csv_file_path, ":", csv_file_group_name, "files to process", length(checked_file_list)))
  return(checked_file_list)
}

#' main stitching function.
#'
#' read all csvs from list, process and bind.  This creates a list structure that
#' stores not only the data but also pre-binding stats, the file list,
#' etc for printing in a report/notebook
#'
#' @param csv_file_list vector of character, full paths of files to include
#' @param csv_file_group_name character, name of this group to help track
#'
#' @returns list with the following elements
#'  file_list: list of files included, base names only (not full paths)
#'  $intxns:  interaction database
#'  $empty_files:
#'  $pre_binding_summary: counts/summaries of 3 columns in all files before binding
#'  $post_binding_summary: counts/summaries of 3 columns of intxns after binding
#'
L0_stitch<-function(csv_file_list, csv_file_group_name){
  # build a list object to hold data, stats, files, etc
  intxns <- list()
  # note this file list will have the FULL path including user name
  # but we want to keep that list of files, so just store the file name alone
  # this means if they came from different folders we may lose that info
  # this includes all the empty files as well, if any
  intxns$file_list <- unlist(lapply(csv_file_list,basename))

  # Read and process all CSV files; Filter out NULL elements (empty files) from intxns.list and csv_file_list
  intxns$list_of_df <- Filter(Negate(is.null), lapply(csv_file_list, read_and_amend))

  # Now list_of_df only contains non-empty files

  # Identify empty files (where NULL returned) and store for later diagnostics
  intxns$empty_files <- intxns$file_list[sapply(intxns$file_list, is.null)]

  # always issue a warning if any files are empty
  if (length(intxns$empty_files) > 0) {
    warning(paste("Empty files detected and omitted from processing:",intxns$empty_files))
  }

  # Pre-binding unique value summaries
  # what is this counting?
  cols_to_summarize = c("n_studies", "effect_sp1_on_sp2", "effect_sp2_on_sp1")
  # TODO CHECK THIS
  intxns$pre_binding_summary <- lapply(cols_to_summarize,
              function(col) {
               all_values <- lapply(intxns$list_of_df, function(df) { if (col %in% names(df)) df[[col]] else NULL} ) |> unlist()
               return(list(unique = unique(all_values), summary = summary(all_values), count = table(all_values, useNA = "ifany")))
              }
    )
  names(intxns$pre_binding_summary) <- cols_to_summarize

  # stitch
  # and remove duplicate rows.  Unlikely but possible (if including GSheets folders)
  intxns$intxns <- dplyr::bind_rows(intxns$list_of_df) |> dplyr::distinct()

  # post-binding stats.
  intxns$post_binding_summary <- lapply(cols_to_summarize,
    function(col) {
      all_values <- intxns[[col]]
      list(unique = unique(all_values), summary = summary(all_values), count = table(all_values, useNA = "ifany"))
      }
    )
  names(intxns$post_binding_summary) <- cols_to_summarize

  intxns$count <- unique_speces_count(intxns$intxns)

  # return the list object that includes data, stats, empty files, etc
  return(intxns)

} #END of stitching function





#' print summary of stitching process
#'
#' this printing is on it's own so that it can be called
#' or not when running in workflow or notebook, and can
#' be called on any collection
#' @param intxns a interaction list structure from L0_stitch function that
#'               has elements pre_binding_summary and post_binding_summary,
#'               assumes they both have the same names
print_binding_report <- function(intxns){

  cat("Comparison of Pre- and Post-Binding Values\n")
  for (col in names(intxns$pre_binding_summary)) {
    cat(paste("\nColumn:", col, "\n"))
    cat("Pre-Binding Summary:\n")
    print(intxns$pre_binding_summary[[col]]$summary)
    cat("Post-Binding Summary:\n")
    print(intxns$post_binding_summary[[col]]$summary)

    cat("Pre-Binding Unique Counts:\n")
    print(intxns$pre_binding_summary[[col]]$count)
    cat("Post-Binding Unique Counts:\n")
    print(intxns$post_binding_summary[[col]]$count)
  }

}

#' shortcut for UTF csv file writer
#'
#' ensures that data is written in UTF-8
#' @param df: data frame to write
#' @param filename: just the file name, not the path
#' @param data_dir: folder to write in
#' @returns file that was written
write_data_file<- function(df, filename, data_dir){
  data_dir = here::here(data_dir)
  csv_file_path = file.path(data_dir, filename)
  write.csv(df, csv_file_path,row.names = F, fileEncoding = "UTF-8")
  return(csv_file_path)
}


#' save interaction L0 file
#'
#' this simply provides the file name and saves the data frame as a
#' new csv, overwiting any existing file.  This is in it's own function
#' to allow inspection of the merged data frame before writing
save_L0_intxns<- function(intxnsL0, intxns_file_name = "AvianInteractionData_L0.csv", L0_dir = "L0") {
  data_file_path <- write_data_file(df = intxnsL0, filename = intxns_file_name, data_dir = L0_dir)
  return(data_file_path)
}




#' remove uncertain interactions
#'
## add after line 555 (?)
# set of standard keywords
uncertainty_keywords = c("alleged",
  "anecdotal",
  "artificial",
  "assumed",
  "captive",
  "captivity",
  "circumstantial",
  "conjectured",
  "disputed",
  "does not",
  "dubious",
  "erroneous",
  "human",
  "hypothesized",
  "little evidence",
  "may",
  "maybe",
  "might",
  "mounted",
  "none",
  "not",
  "no ",
  "perhaps",
  "playback",
  "possible",
  "possibly",
  "potential",
  "presumed",
  "purported",
  "name changes",
  "name_changes",
  "name",
  "speculative",
  "suggested",
  "suggests",
  "suspected",
  "unclear",
  "unknown",
  "questionable",
  "thought to be",
  "uncertain",
  "unconfirmed",
  "unfounded",
  "unlikely",
  "unspecified",
  "unsure",
  "different",
  "inferred",
  "referring",
  "experiment",
  "specified",
  "unidentified",
  "recently split",
  "previously",
  "?",
  "yes",
  "unsubstantiated"
)

#' L1:  keyword row filter
#'
#' remove rows that match keyword in field
#' @param df any data frame with column "uncertain_interaction"
#' keywords: data from of keywords such as read in by function above
#' uncertain_save_filepath: send full path of file to save the rows
#' that were removed
#' example: filtered.df <- remove_rows_by_keywords(atx.df, uncertainty_keywords)
#' @param keywords vector of character
#' @param uncertain_save_filepath if provided, will save the rows to a file,
#'    default NULL which means no saving
#' @returns data frame of remaining rows
remove_rows_by_keyword <- function(df, keywords, remove_blanks = FALSE, uncertain_save_filepath= NULL){

  # save the field name in a variable in the hope we can make this a generic
  field_name = "uncertain_interaction"


  # remove rows with blanks in the field name - in this case, don't we want to keep blanks?
  if(remove_blanks == TRUE){
    df <- df[! ( df[field_name]=="" | is.na(df[field_name]) ), ]
  }
  # df <- df %>% dplyr::filter(uncertain_interaction=="") %>% dplyr::filter(is.na(uncertain_interaction))

  # create the grep expression from all the keywords e.g may|maybe|might...
  grep_expression <- paste(keywords, collapse = "|")

  df <- df[!grepl(grep_expression, df[field_name],ignore.case = TRUE),]

  if(! is.null(uncertain_save_filename)) {
    uncertain_rows.df <-  df[grepl(grep_expression, df[field_name],ignore.case = TRUE),]
    write.csv(uncertain_rows.df, uncertain_save_filename,row.names = F, fileEncoding = "UTF-8")
    print(paste("wrote file listing rows that were filtered out to ", uncertain_save_filepath))
  }

  return(df)

}

# this is the function for a single value to be applied to a column
# corrections data frame must have two columns 'incorrect' and 'correct'
correct_text <- function(col_text, corrections.df) {
  if ( col_text %in% corrections.df$incorrect){
    corrections.df$correct[corrections.df$incorrect==col_text]}
  else {
    col_text
  }
}

# standardize text column case and value using a corrections file
standardize_text_column <- function(text_vector, corrections.df) {
  # Remove extra end spaces and make all lowercase
  text_vector <- text_vector |> trimws( "r") |> tolower()
  corrected_text_vector <- sapply(text_vector, correct_text, corrections.df = corrections.df, USE.NAMES=FALSE)
  return(corrected_text_vector)
}

#' convert multi source cols (wide) to arows (long)
#'
#' given a dataframe of interactions with sourceA, sourceB etc
#' assume columns sourceA_URL, sourceB_URL, sourceC_URL, sourceD_URL
sources_wide_to_long<- function(intxnsL0.wide){
  # check that all columns are there
  if(! ( all(c("notesA", "notesB", "notesC", "notesD") %in% names(intxnsL0.wide)) &&
         all(c("sourceA_URL","sourceB_URL","sourceC_URL","sourceD_URL") %in% names(intxnsL0.wide))
       )
    ) {
      warning("data frame does not have all the required rows notesA,... and/or sourceA_URL,... A through D.  Returning NA")
      return(NA)
   }


  intxnsL0 <- intxnsL0.wide |>
      dplyr::rename(source_URLXsource1 = sourceA_URL,
                    source_URLXsource2 = sourceB_URL,
                    source_URLXsource3 = sourceC_URL,
                    source_URLXsource4 = sourceD_URL) |>
      dplyr::rename(text_excerptXsource1 = notesA,
                    text_excerptXsource2 = notesB,
                    text_excerptXsource3 = notesC,
                    text_excerptXsource4 = notesD) |>
      tidyr::pivot_longer(
        cols = contains("source"),
        names_to = c(".value", "source"),
        names_sep = "X",
        values_drop_na = TRUE
      )

  return(intxnsL0)

}

################################
# end of functions
# saved code and programming notes

#### CURRENTLY UNUSED- simplified the notebook code intead
#' list CSV files in one or more foders
#'
#' uses the config system to get the base folder name
#' and looks into each folder sent.  allows for looking into folders outside
#' our normal L0 folder structure
#'
#' @param folders character vector of file paths, full paths needed.
#' defaults to 'L0/species' To add multiple folders, use c('L0/species', 'L0/species_in_reivew')
#' @param config_file
#' @param config_section
#'
#' @returns character vector of file names
# list_csvs <- function(folders=c('L0/species')){
#   file_paths <- get_file_config(config_file, config_section)
#   # start with an empty list and add files for each folder sent
#   file_list <- c()
#   for(f in folders){
#     sub_folder <- file.path(file_paths$DATA_FOLDER, f)

#     if(!dir.exists(sub_folder)){
#       warning(paste("directory doesn't exist", sub_folder))
#       stop()
#     }

#     files <- list.files(path = sub_folder, pattern = ".*\\.csv", full.names = full.names)
#     file_list <- c(file_list, files)
#   }

#   return(file_list)

# }
