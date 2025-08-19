require(here)
library(dplyr)

# stitch workflow:
#   source this file
#   o
#   intxns <- stitch_L0(species_dir = "L0/species", species_review_dir = "L0/species_in_review" )
#   save_L0_intxns(intxns, "L0")

# this from other code and includes a column '"DatabaseSearchURL" that is not in
# all CSVs, used in
standard_columns <- c("species1_common", "species2_common", "species1_scientific", "species2_scientific",
                      "effect_sp1_on_sp2", "effect_sp2_on_sp1", "interaction", "BOW_evidence", "n_studies",
                      "sourceA_URL", "sourceB_URL", "sourceC_URL", "sourceD_URL", "nonbreedingseason",
                      "notesA", "notesB", "notesC", "notesD", "recorder", "entry_date", "uncertain_interaction",
                      "entry_changes", "name_changes", "other_species1", "DatabaseSearchURL")


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

#' merge a folder of csvs
#' read in all CSVs in a folder and merge into one dataframe
#' assumes all the CSV files are the same format with the same headers

merge_csvs <- function(data_dir = here::here("L0/species")) {
  if(!file.exists(data_dir)){
      warning(paste("directory unreachable", data_dir))
      return(NULL)
  }

  # get list of file names
  csv_files <- list.files(path = data_dir,
      pattern = '.*\\.csv',
      full.names = TRUE)


  # use apply to read all files into a list
  # use readr and create a var with column names and types that readr can use
  csv_data.list <- lapply(csv_files, readr::read_csv, col_types = avian_intxn_column_spec)
  #TODO validate each file in the list

  # if any are invalid, report and return NULL

  # combine all in to a single df
  merged_data.df <- data.frame(dplyr::bind_rows(csv_data.list, .id = "sheet_id"))

  # return as a data frame (the operation above is a list type)
  return(merged_data.df)
}


#' stitch L0 CSV
#'
#' combine all the CSVs into one file - no corrections made
#' adding columns indicating if it ws reviewed or not.
#' save the output using write.csv()
#' inputs
#'    species_dir default is L0/species, path from root of project OR full path
#'    species_review_dir will be used if sent, send full path!   default is  L0/species_in_review
#' output
#'     a data frame with of all CSVs merge, with column "reviewed" value True/False
#'
stitch_L0 <- function(
        species_dir = "L0/species",
        species_review_dir = NULL # "L0/species_in_review"
    ){

  # get full path
  species_dir = here::here(species_dir)
  # the following will have warnings from read.csv if the file has an issue
  intxnsL0sp<- merge_csvs(species_dir) %>% dplyr::mutate(reviewed = TRUE)

  # optionally include the species in review folder if one is sent
  if(!is.null(species_review_dir)) {
    species_review_dir =  here::here(species_review_dir)
    intxnsL0sp_in_review <- merge_csvs(species_review_dir)  %>% dplyr::mutate(reviewed = FALSE)
    intxnsL0sp <- rbind(intxnsL0sp, intxnsL0sp_in_review)
  }

  return(intxnsL0sp)

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

  #' keyword row filter
  #'
  #' remove rows that match keyword in field
  #' parameters
  #'  df:  any data frame
  #'  keywords: data from of keywords such as read in by function above
  #'  uncertain_save_filepath: send full path of file to save the rows that were removed
  #' example: filtered.df <- remove_rows_by_keywords(atx.df, uncertainty_keywords)
  #'
  remove_rows_by_keyword <- function(df, keywords, uncertain_save_filepath= NULL){

    # save the field name in a variable in the hope we can make this a generic filtering function
    field_name = "uncertain_interaction"

    # remove rows with blanks in the field name
    df <- df[! ( df[field_name]=="" | is.na(df[field_name]) ), ]

    # df <- df %>% dplyr::filter(uncertain_interaction=="") %>% dplyr::filter(is.na(uncertain_interaction))

    # create the grep expression from all the keywords e.g may|maybe|might...
    grep_expression <- paste(keywords, collapse = "|")

    df <- df[!grepl(grep_expression, df[field_name],ignore.case = TRUE),]
    if(! is.null(uncertain_save_filename)) {
      uncertain_rows.df <-  df[grepl(grep_expression, df[field_name],ignore.case = TRUE),]
      write_data_file(uncertain_rows.df, uncertain_save_filepath, "L0")
      print(paste("wrote file listing rows that were filtered out to ", uncertain_save_filepath))
    }

    return(df)

  }


  ## standard text column corrections, used on several columns
  #

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


# put this in the notebook
#
#
# uncertainty_keywords<- function(df){
#
#   print("remaining uncertain interactions =:")
#   return(sort(unique(df$uncertain_interaction)))
#
# }

##############################################################################
# COPIED OVER this was transferred to the new repository
##############################################################################

#' shortcut function csv file writer with UTF In L0 folder
#' that way paths can change to data/L0 etc
#' parameters
#' df: data frame to write
#' filename: just the file name, not the path
#' data_dir: default L0 but change to L1 or L2 as needed
write_data_file<- function(df, filename, data_dir = "L0"){
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
  atx_data_file_path <- write_data_file(intxnsL0, intxns_file_name = "AvianInteractionData_L0.csv", L0_dir = "L0")
  return(L0_data_file_path)
}

