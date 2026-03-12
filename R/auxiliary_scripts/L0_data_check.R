#!/usr/bin/env Rscript

# TITLE:          Avian MetaNetwork L0 data check
#
#                 This is a quick attempt to ID issues that may be present in the
#                 'stiched' or combine file from individual L0 CSVs for issues, mostly
#                 arising because of divergence of a CSV raw input file from standard
#                 The goal is for this to be added to shared functions and be a test
#                 QC check in the pipeline
#
# AUTHORS:        Pat Bills
# COLLABORATORS:  Phoebe Zarnetske, Kelly Kapsar, Lucas Mansfield
# REQUIRES        R/lib/shared_functions.R is sourced in, requiring file_paths.R
# DATA INPUT:     L0 data: output from R/L0/2_stitch_species.qmd
# DATA OUTPUT:    List of errors or cells missing data
# DATE:           9 March 2026
#
# USAGE           source this script and then put run_checks() in console to use
#                  default file name and path
#
# Next script to run: L1 scripts

# this has filepaths and also the 'no.data' function
source(here::here('R/lib/shared_functions.R'))

#### CONSTANTS - UPDATE THESE OR REPLACE WITH CONFIGURATION SOMEWHERE
amn_columns<- c("taxa1_common", "taxa2_common", "taxa1_scientific", "taxa2_scientific", "effect_tx1_on_tx2", "effect_tx2_on_tx1", "interaction", "BOW_evidence", "n_studies", "nonbreedingseason", "recorder", "entry_date", "uncertain_interaction", "entry_changes", "name_changes", "other_species1", "DatabaseSearchURL", "entry_file", "source", "source_URL", "text_excerpt")


#### FUNCTIONS

#' read in the L0 stictch file.
#' @param AvianMetaNetwork_L0_file filename (only) of the csv file
#' @returns data frame of file
read_stitched_file <- function(AvianMetaNetwork_L0_file="amn_all_raw.csv", path_to_file = NULL){

    if(is.null(path_to_file)){
    file_paths <- get_file_paths()
    path_to_file <- file_paths$L0
  }

  full_path_stitched_file <- file.path(path_to_file,AvianMetaNetwork_L0_file)
  amn.df<-read.csv(full_path_stitched_file)
  stopifnot(nrow(amn.df)>0)
  return(amn.df)

}

#' check column consistency in AMN db file
#'
#' this fn to be written
#' will check if columns match the standard
#' and
check_colum_match<-function(amn.df){
  return(TRUE)
}


#' check the contents of the source field
#' uses the no.data function in lib/shared_functions.R
#' @param amn.df data frame containing AMN database must have field source_URL
check_empty_source_url_field<- function(amn.df){
  # check field is there
  stopifnot("source_URL" %in% names(amn.df))
  empty <- length(which(no.datas(amn.df$source_URL)))
  return(empty)
}

check_empty_excerpt_field<- function(amn.df){
  # check field is there
  stopifnot("text_excerpt" %in% names(amn.df))
  empty <- length(which(no.datas(amn.df[,"text_excerpt"])))
  return(empty)
}

#' check the contents of the source field
#' uses the no.datas function in lib/shared_functions.R
#'
#'
#' @param amn.df data frame containing AMN database must have field source_URL
#' @param field_name string of the name of the field to check
check_empty_field<- function(amn.df, field_name){
  # note because we are using field_name string var and not the literal field name
  # can't use tidyverse stuff in here

  # check field is there
  stopifnot(field_name %in% names(amn.df))
  # FUTURE instead of counting here, just return the row numbers
  empty <- length(which(no.datas(amn.df[,field_name])))
  return(empty)

}

#' run various tests on full AMN db file
#'
#' @param AvianMetaNetwork_L0_file optional string of just the filename, no path, defaults to
#' @param path_to_file optional string of fullpath to the file, default NULL will use file_paths setup
#' @returns
run_checks <- function(AvianMetaNetwork_L0_file="amn_all_raw.csv", path_to_file = NULL){
  amn.df <- read_stitched_file(AvianMetaNetwork_L0_file,path_to_file)
  print(paste('rows with empty source_URL', check_empty_field(amn.df, "source_URL")))
  print(paste('rows with empty text_excerpt', check_empty_field(amn.df, "text_excerpt")))

  # future return T/F if all checks pass

}

