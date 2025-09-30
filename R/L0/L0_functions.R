# L0_functions.R
# TITLE:          Avian Interaction Pairs L0 Data Stitching together CSVs into 1
# AUTHORS:        Phoebe Zarnetske, Pat Bills, Emily Parker
# COLLABORATORS:  Vincent Miele, Stephane Dray
# DATA INPUT:     Data entry CSV files in L0 folder
# DATA OUTPUT:    L0 combined data: AvianInteractionData_L0.csv
# PROJECT:        Avian Interaction Database & Avian Meta-Network
# DATE:           20 Mar 2023 - August Dec 2025)
# NOTES:          Functions here are used in R/L0/L0_stitch.qmd notebook, see run date there

# hide messages emitted when loading packages
suppressMessages({
  require(here)
  library(dplyr)
  library(readr)
  library(magrittr)
  library(stringr)
})

# this sets file paths using config file. See readme for details
source(here::here('R/config.R'))

# set the global variable with list of file paths for use in all functions
# this is set here to ensure that it's set and check that a config file was
# created.

# to use a different set of files paths (e.g. for testing)
# put this statement in your script or notebook after sourcing this file
# and put the alternate config files,
# for example, at the top of your script...
# source(here::here("L0_functions.R")
# file_paths <- get_file_paths(here::here('testdata.R'))

file_paths <- get_file_paths()

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


##### UPDATE WHEN COLUMNS CHANGE
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
  # first add in the "optional" columns if they are not present:
  #   B,C,D versions of url and notes

  if (! "sourceBupdatedURL" %in% names(intxns.df)) { intxns.df$sourceBupdatedURL <- NA }
  if (! "sourceCupdatedURL" %in% names(intxns.df)) { intxns.df$sourceCupdatedURL <- NA }
  if (! "sourceDupdatedURL" %in% names(intxns.df)) { intxns.df$sourceDupdatedURL <- NA }

  if (! "notesB" %in% names(intxns.df)) { intxns.df$notesB <- NA }
  if (! "notesC" %in% names(intxns.df)) { intxns.df$notesC <- NA }
  if (! "notesD" %in% names(intxns.df)) { intxns.df$notesD <- NA }

  # set up vars to report if columns are missing
  no_missing_columns <- TRUE
  missing_dataentry_cols <- setdiff(dataentry_columns, names(intxns.df))
  if(length(missing_dataentry_cols)>0){
    msg <- paste("columns missing:", missing_dataentry_cols)
    warning(msg)
    no_missing_columns <- FALSE
  }

  # we don't really care if there happen to be extra columns not in the spec,
  # but here is code if we want to enforce that
  # this is disabled while the data sheets undergo some changes
  #
  # extra_dataentry_cols <- setdiff(names(df), dataentry_columns)
  # if(length(extra_dataentry_cols)>0){
  #   msg = paste("data sheet has extra columns: ", extra_dataentry_cols, " ")
  #   warning(msg)
  #
  # }

  return(no_missing_columns)
}


##### UPDATE WHEN COLUMNS CHANGE

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

#' remove empty rows
#'
#' rows with no species names (sci or common) in them are essentially blank
#' this only works if the columns are in the data frame
#' @param df interactions database df
#' @returns data frame with empty rows removed
remove_rows_no_species<- function(df){
  # with(int.raw(!is.na(int.raw[["species1_scientific"]]) & !is.na(int.raw[["species2_scientific"]]) & !is.na(int.raw[["species2_common"]]))
  columns_to_check = c("species1_scientific",
                         "species2_scientific",
                         "species1_common",
                         "species2_common")

  if(!all(columns_to_check %in% names(df))){
    warning("couldn't check for blank rows, not all columns present")
    return(df)
  }

  df <- dplyr::filter(df,!(is.na(species1_scientific)&
                    is.na(species2_scientific) &
                    is.na(species1_common) &
                    is.na(species2_common)
  ))

  return(df)
}


############################################################
### TYPO FIXES

#' fix common typos in species.common
#'
#' @param intxnsL0 data frame of avian interactions database, must have fields
#'  species1_scientific, species2_scientific, species1_common, species2_common
#'  @returns data frame with typos corrected in place
fix_taxon_typos <- function(intxns.df){

  # trim whitespace
  intxns.df<- dplyr::mutate(intxns.df,
            species1_scientific = trimws(species1_scientific,which=c("right")),
            species2_scientific = trimws(species2_scientific,which=c("right")),
            species1_common = trimws(species1_common,which=c("right")),
            species2_common = trimws(species2_common,which=c("right"))
    )

  # sometimes there is no space before "unid" aka Unid.duck"
  intxns.df$species1_common<- str_replace(intxns.df$species1_common, "[Uu]nid\\.([A-Za-z])", "Unid. \1")
  intxns.df$species2_common<- str_replace(intxns.df$species2_common, "[Uu]nid\\.([A-Za-z])", "Unid. \1")

  # make unid/sp. text consistent in species1
  intxns.df<- dplyr::mutate(intxns.df,
               species1_scientific = ifelse(
                 str_starts(species1_scientific, "unid."),  # Exception case
                 str_replace(species1_scientific, "(unid.)s*(w+)", "1 U2"),
                 str_to_sentence(species1_scientific)       # Regular case
               ) |> str_replace("spp.", "sp.")

  )

  # make unid/sp. text consistent in species2
  intxns.df<- dplyr::mutate(intxns.df,
            species2_scientific = ifelse(
               str_starts(species2_scientific, "unid."),  # Exception case
               str_replace(species2_scientific, "(unid.)s*(w+)", "1 U2"),
               str_to_sentence(species2_scientific)       # Regular case
             ) |> str_replace("spp.", "sp.")
  )

  # add a period to the end of all Genus sp.
  intxns.df$species1_scientific<- str_replace(intxns.df$species1_scientific, " sp$", " sp.")
  intxns.df$species2_scientific<- str_replace(intxns.df$species2_scientific, " sp$", " sp.")



  # remove double spaces from binomials
  intxns.df<- intxns.df %>% dplyr::mutate(
    species1_scientific = stringr::str_replace_all(species1_scientific, "  ", " "),
    species2_scientific = stringr::str_replace_all(species2_scientific, "  ", " ")
  )


  # tidy common_names
  intxns.df<- intxns.df %>% dplyr::mutate(
    species1_common =
      stringr::str_trim(species1_common) %>%  # Start with the raw data
      stringr::str_replace_all("[Uu]nid ", "unid. ") %>%
      stringr::str_replace_all("Unid.", "unid.") %>%
      stringr::str_to_title(), # Capitalize each word
    species2_common =
      stringr::str_trim(species2_common) %>%  # Start with the raw data
      stringr::str_replace_all("[Uu]nid ", "unid. ") %>% # add period to unids
      stringr::str_replace_all("Unid.", "unid.") %>%
      stringr::str_to_title()  # Capitalize each word

  )

  return(intxns.df)
}

# FUNCTION TO READ, RENAME COLUMNS, ADD MISSING COLUMNS, AND SELECT STANDARD COLUMNS

#' read avian interaction db data entry file and process
#'
#' amend the raw data files by altering the columns to accommodate
#' combining into single table
#' @param entry_file full path to a csv file containing interaction data
#' @param add_enry_file boolean if true, add colummn with the entry file basename
#'    really useful for finding and correcting errors but
#'    this column can be problematic if removing redundant rows since
#'    identical data might come from two entry files
read_and_amend <- function(entry_file, add_entry_file_column= FALSE, fix_errors_on_read = TRUE) {
  # TRY TO READ THE CSV, CATCH ERRORS

  intxns.df <- tryCatch(
  # use readr and create a var with column names and types that readr can use
      readr::read_csv(entry_file, col_types = avian_intxn_column_spec),
      error = function(e) {
        warning(paste("Failed to read file:", entry_file, "| Error:", e$message))
        return(NA)
      }
    )
  problems.df <- problems(intxns.df)
  if(nrow(problems.df) > 0){
    # print(file)
    print(problems.df)
  }

  # this prints the file name if there are problems, but it will keep
  # going anyway.

  all_cols_present <- check_dataentry_columns(intxns.df)
  if(!all_cols_present){ warning(paste("missing columns in ", entry_file))}

  if (is.null(intxns.df) || nrow(intxns.df) == 0) return(NULL)  # Skip empty or unreadable files

  # Rename columns individually to avoid duplication issues
  if ("sourceAupdatedURL" %in% names(df)) names(df)[names(df) == "sourceAupdatedURL"] <- "sourceA_URL"
  if ("sourceBupdatedURL" %in% names(df)) names(df)[names(df) == "sourceBupdatedURL"] <- "sourceB_URL"
  if ("sourceCupdatedURL" %in% names(df)) names(df)[names(df) == "sourceCupdatedURL"] <- "sourceC_URL"
  if ("sourceDupdatedURL" %in% names(df)) names(df)[names(df) == "sourceDupdatedURL"] <- "sourceD_URL"


  # ADD ANY MISSING STANDARD COLUMNS WITH NA VALUES
  missing_cols <- setdiff(standard_columns, names(intxns.df))
  intxns.df[missing_cols] <- NA
  intxns.df <- intxns.df %>% dplyr::select(dplyr::all_of(standard_columns)) # SELECT STANDARD COLUMNS

  intxns.df<- amend_intxn_columns(intxns.df)

  # add column indicating which file it came from
  # useful for error discovery
  if(add_entry_file_column == TRUE){
    intxns.df$entry_file<- basename(entry_file)
  }

  #### fix issues with file when it's read in if that is request

  if(fix_errors_on_read == FALSE){
    # flag to fix errors set to false means don't fix problems yet
    return(intxns.df)
  }

  # flag to fix errors set to false

  # remove blank rows
  intxns.df<-remove_rows_no_species(intxns.df)

  # fix formatting/typos/inconsistencies in scientific and common name cols
  intxns.df <- fix_taxon_typos(intxns.df)

  # assign 0,0 to interactions that are supposed to be that
  intxns.df <- fix_interaction_errors(intxns.df)

  # fix typos in other columns here

  return(intxns.df)
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
L0_stitch<-function(csv_file_list, csv_file_group_name, add_entry_file_column = FALSE, fix_errors_on_read = TRUE ){
  # build a list object to hold data, stats, files, etc
  intxns <- list()
  # note this file list will have the FULL path including user name
  # but we want to keep that list of files, so just store the file name alone
  # this means if they came from different folders we may lose that info
  # this includes all the empty files as well, if any
  intxns$file_list <- unlist(lapply(csv_file_list,basename))

  # Read and process all CSV files; Filter out NULL elements (empty files) from intxns.list and csv_file_list
  intxns$list_of_df <- Filter(Negate(is.null), lapply(csv_file_list, read_and_amend,
                                                      add_entry_file_column = add_entry_file_column,
                                                      fix_errors_on_read = fix_errors_on_read))

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
#' (or not) when running in a workflow or in a notebook, and can
#' be called on any collection of data files
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


######## UNCERTAIN INTERACTIONS:
# this was used by Emily Parker to find rows to re-check and evaluate
# but is most likely no longer needed

#' remove uncertain interactions
#'
#' @returns set of standard keywords
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

#' get rows by keyword for checking them
#'
#' identify rows that match keyword in field.  originally created by Emily
#' Parker to get a list of rows in the db that needed to be double
#' checked with some keywords in the 'uncertain_interaction' column
#'
#' @param df any data frame with column "uncertain_interaction"
#' keywords: data from of keywords such as read in by function above
#' uncertain_save_filepath: send full path of file to save the rows
#' that were removed
#' This can be run after all files are stitched or on single 'file' data frame
#' example: filtered.df <- remove_rows_by_keywords(atx.df, uncertainty_keywords)
#' @param keywords vector of character
#' @param uncertain_save_filepath if provided, will save the rows to a file,
#'    default NULL which means no saving
#' @returns data frame of remaining rows
get_rows_by_keyword <- function(df, keywords, column_name = "uncertain_interaction", remove_blanks = FALSE, extracted_row_save_filepath= NULL){

  # we are working on a copy of the original data frame, since R passes by value
  # remove rows with blanks in the column - we just want
  if(remove_blanks == TRUE){
    df <- df[! ( df[column_name]=="" | is.na(df[column_name]) ), ]
  }

  # create the grep expression from all the keywords e.g may|maybe|might...
  grep_expression <- paste(keywords, collapse = "|")

  # df <- df[!grepl(grep_expression, df[field_name],ignore.case = TRUE),]
  extracted_rows.df <-  df[!grepl(grep_expression, df[column_name],ignore.case = TRUE),]
  # IF a file path is sent with the function, save these extracted rows
  if(! is.null(extracted_row_save_filepath)) {
    write.csv(extracted_rows.df, extracted_row_save_filepath,row.names = F, fileEncoding = "UTF-8")
  }
  return(extracted_rows.df)

}

#' make text corrections
#'
#' in a single column, given a data.frame with 'corrections',
#' change the values that are 'incorrect' to the 'correct' value
#' corrections data frame must have two columns 'incorrect' and 'correct'
#' this was originally for correcting only the 'interactions' column but can be
#' used for other categorical columns
correct_text <- function(col_text, corrections.df) {
  if ( col_text %in% corrections.df$incorrect){
    corrections.df$correct[corrections.df$incorrect==col_text]}
  else {
    col_text
  }
}

#' make text corrections, vectorized version
#'
#' standardize text column case and value using a corrections file
#' by calling `correct_text` function above for all values in a vector
#' keep a CSV file with a header and column names 'incorrect' and 'correct'
#' which can be read in and used to correct common typos for any one database
#' field/column
#' @param text_vector any character vector, such as a column from a data frame
#' @param corrections.df a data frame with columns 'incorrect' and 'correct'
#' @returns character vector with same length as input, values corrected
standardize_text_column <- function(text_vector, corrections.df) {
  # Remove extra end spaces and make all lowercase
  text_vector <- text_vector |> trimws( "r") |> tolower()
  corrected_text_vector <- sapply(text_vector, correct_text, corrections.df = corrections.df, USE.NAMES=FALSE)
  return(corrected_text_vector)
}



######## INTERACTIONS #########


# commensalism??
# list of all valid


read_valid_interaction_types <- function(file_paths, interactions_def_file_name = NULL) {
  # set the default metadata file, but use a parameter to allow for variations
  if(is.null(interactions_def_file_name)) {
    interactions_def_file <- file.path(file_paths$L0, 'AvianInteractionData_metadata_interactiondefinitions.csv')
  }

  interaction_types <- read.csv(interactions_def_file)
  # read a character vector
  # interaction_types <- readr::read_readlines(interaction_types_file)

  return(interaction_types)
}

# all interactionst that should have zero values

interactions.0 <- c(
  "hybridization",
  "co-occur",
  "play",
  "courtship",
  "copulation",
  "copulation?",
  "breeding",
  "combined species"
  )


#' find the problems and list them
#' @returns data frame of rows with problems
discover_interaction_errors <- function(intxns.df){

  # create empty data frame with same columns to hold the errors with an R trick
  intxn_errors.df <- intxns.df[FALSE,]

  # loop through all the 0,0 interactionss and collect errors binding into errors df
  for (intxn.keyword in interactions.0){
    intxn_errors.df <- dplyr::bind_rows(intxn_errors.df,
                       intxns.df[intxns.df$interaction == intxn.keyword & intxns.df$effect_sp1_on_sp2!= 0, ]
                       )
    intxn_errors.df <- dplyr::bind_rows(intxn_errors.df,
                       intxns.df[intxns.df$interaction == intxn.keyword & intxns.df$effect_sp2_on_sp1!= 0, ]
                       )
  }

  return(unique(intxn_errors.df))
}


#' fix errors
#'
#' this code doesn't report, just fixes isses with interaction fields
#' 1. fix known typos
#' 2. for those interactions that should be zeron, set all zero, overwrite everything
#'
fix_interaction_errors <- function(intxns.df){

  # fix typos in the interaction column using
  # text file with errors that have been discovered and cataloged
  corrections_file <- here::here("R/L0/interaction_text_corrections.csv")
  interaction_corrections.df <- read_csv(corrections_file,show_col_types = FALSE)
  intxns.df$interaction <- standardize_text_column(intxns.df$interaction, interaction_corrections.df )


  # loop through all the 0,0 interactions and set to zero

  for (intxn.keyword in interactions.0){
    if(intxn.keyword %in% intxns.df$interaction) {
      intxns.df[intxns.df$interaction == intxn.keyword,]$effect_sp1_on_sp2 <- 0
      intxns.df[intxns.df$interaction == intxn.keyword,]$effect_sp2_on_sp1 <- 0
    }
  }

  return(intxns.df)
}



# FOR NOTEBOOK - SHOW INTERACTION TYPES
# Unique interaction types:
# sort(unique(interactions))

# list number of rows with blank interactions
# list rows with blank interactions

# Ignore these interactions for now:
# "combined species"
# "copulation?" - for 2 swallows


# Which are NA?
# intxns.int.entries.NA<-intxns[which(is.na(intxns$effect_sp2_on_sp1)), ]
# dim(intxns.int.entries.NA)
# intxns.int.entries.NA
# One is a NA for brood parasitism but it's for NZ species so ignore for now.

#' discover interaction errors
#'




#' correct the interactions column
#'
#' issues warning messages if there are problems
#' uses a corrections file to standardize the interactions column
#'
#' @param intxns a dataframe with an interactions column to be corrected
#'
#' @return the dataframe with the interactions column corrected
#'



#'
#'
#'
#'
#' issues warning messages if there are problems
#' uses a corrections file to standardize the interactions column
#'
#' @param intxns a dataframe with an interactions column to be corrected
#' @return the dataframe with the interactions column corrected
correct_interactions <- function(intxns) {

  # read in file use to store corrections to interactions column, and use it to standardize
  interaction_corrections.df <- read_csv(corrections_file,"R/L0/text_corrections.csv")

  intxns$interaction <- standardize_text_column(intxns$interaction, interaction_corrections.df )

  # collect the warnings and problems when fixing interactions
  # but return a df with interactions corrected?

  # Remove the blank entries for interaction type if they exist

  intxns <- intxns %>% filter(!(interaction==""))
  # no blanks exist





  # At least one has competition as +1 - this is not in BBS so not worrying about it now
  # 21                    competition                -1                 1
  # 22                    competition                 1                -1
  # in BBS: commensalism as 0,0; ignore it for now
  # 6470	Altamira Oriole	Tyrannus melancholicus	Tropical Kingbird	Icterus gularis	0	0	commensalism

  # At least one has brood parasitism as 0,0 - this is not in BBS so not worrying about it now


  return(intxns)

}

#' convert multi source cols (wide) to rows (long)
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
