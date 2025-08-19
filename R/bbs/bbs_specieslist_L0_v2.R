# TITLE:          bbs_specieslist_L0.R  L0 BBS Species List Data: Pulls in raw BBS SpeciesList from 2024 release (up to 2023 BBS data),
#                 so it can be used in avian-meta-network (align with intxnpairs data before merging in L2)
# AUTHORS:        Phoebe Zarnetske, Patrick Bills
# COLLABORATORS:  Vincent Miele, Stephane Dray, Emily Parker
# DATA INPUT:     Imports L0 raw species list data from the USGS North American Breeding Bird Survey (SpeciesList.txt is updated every year).
# DATA OUTPUT:    L0 data: bbs_specieslist_2023_L0.csv - this is a copy of the raw data, just omitting the top lines without data
# PROJECT:        Avian Interaction Database & avian-meta-network
# DATE:           17 January 2022 - April 2025
# NOTES:          bbs_specieslist_2024_L1.csv is produced in bbs_specieslist_L1.R
#                 Next script to run: bbs_specieslist_L1.R


# USAGE:
# this assumes the L0 folder in the top of this project /L0
# edit this file and update the BBS Data file URL if necessary
#   you can find the URL by going to the USGS bbs page
#   https://www.sciencebase.gov/catalog/item/66d9ed16d34eef5af66d534b
# source this file from top of this project.
#   source(R/L0/bbs_species_L0_psb.R)
# run the function main() for workflow or just run
#  main(L0_dir)
#  this downloads the BBS 24 file if it doesn't exist and default bbs filenames

require(curl)
library(dplyr)

### CONSTANTS
# 2024 DATA FILE URL
CURRENT_BBS_CSV_URL <- "https://www.sciencebase.gov/catalog/file/get/66d9ed16d34eef5af66d534b?f=__disk__50%2F9f%2F80%2F509f8039f5d6f62305b9e9779c2b8b6f7295f6d1"
source(here::here("R/config.R"))
file_paths <- get_file_config()
L0_dir<- file.path(file_paths$DATA_FOLDER, "L0")
L1_dir <-file.path(file_paths$DATA_FOLDER, "L1")
R_L0 <- here::here("/R/bbs")
source(here::here("R/AvianInteractionData_functions.R"))

#
# CHECKLIST_FILE <- file.path(L0_dir,"eBird-Clements-v2024-integrated-checklist-October-2024-rev.csv")
# BBS_LIST_FILE <- file.path(L0_dir,"bbs_splist_2024_L0.csv")

# this is very specific to our copy of the 2023 and should be updated to work
# with the version from sciencebase.gov if available

read_bbs_2023<-function(bbs.splist.2023.file, fileEncoding="ISO-8859-1"){
  bbs.splist.2023<-read.csv(bbs.splist.2023.file ,fileEncoding=fileEncoding)
  # alter to max future files
  bbs.splist.2023<- bbs.splist.2023 %>%
      dplyr::rename(Order = ORDER) %>%
      dplyr::select(!Spanish_Common_Name) %>%
      dplyr::mutate(genus_species = paste(Genus, Species, sep = " "))

  return(bbs.splist.2023)

}

#' download and add column genus_species for use in later programs
download_current_bbs_csv <- function(bbs_csv_file_path, url = CURRENT_BBS_URL, overwrite = FALSE){
  # CSV file URL The original file SpeciesList.csv (from 2024 BBS release) is here: https://www.sciencebase.gov/catalog/item/66d9ed16d34eef5af66d534b
  # Reading in a copy placed in the L0 directory on October 22, 2024, and renamed BBS2024_SpeciesListL0.txt

  # don't re-download and overwrite except on purpose
  if(file.exists(bbs_csv_file_path) && ! overwrite){
    warning(paste("BBS file exists", bbs_csv_file_path, "not overwriting. set overwrite = TRUE to write new file"))
    bbs.splist<-read.csv(bbs_csv_file_path,  stringsAsFactors = FALSE, fileEncoding="UTF-8")
  }
  else {
  ## download
    curl::curl_download(CURRENT_BBS_CSV_URL, destfile = bbs_csv_file_path)

    # no file => no download.
    if(! file.exists(bbs_csv_file_path)){
      warning(paste("BBS download did not write file", bbs_csv_file_path))
      return(FALSE)
    }


    ## add column for genes_species for later work and save again
    # PSB note this doesn't seem like L0 data then - I would add this column as needed rather that to original file
    bbs.splist<-read.csv(bbs_csv_file_path,  stringsAsFactors = FALSE, fileEncoding="UTF-8")
    bbs.splist<- dplyr::mutate(bbs.splist, genus_species = paste(Genus, Species, sep = " "))
    write.csv(bbs.splist, bbs_csv_file_path, fileEncoding="UTF-8", row.names=F)
  }

  return(bbs.splist)
}

#' show BBS changes 2022 to 2024

difference_bbs_species_list_2023_24<- function(bbs.splist.2023 , bbs.splist.2024){
  library(dplyr)
  library(tidyr)


  # Compare the list from last year to this year to identify changes:
  # Full join on "AOU" only, ignoring extra rows in the 2024 data

  differences <- bbs.splist.2023 %>%
    dplyr::full_join(bbs.splist.2024, by = "AOU", suffix = c(".2023", ".2024")) %>%
    # Use rowwise to apply comparisons across columns
      dplyr::rowwise() %>%
    # Create a data frame of differing values
        dplyr::mutate(
        differing_values = list(
        tibble(
          # AOU = AOU,
          English_Common_Name = if_else(English_Common_Name.2023 != English_Common_Name.2024,
                                        paste(English_Common_Name.2023, "->", English_Common_Name.2024),
                                        ""),
          French_Common_Name = if_else(French_Common_Name.2023 != French_Common_Name.2024,
                                       paste(French_Common_Name.2023, "->", French_Common_Name.2024),
                                       ""),
          Order = if_else(Order.2023 != Order.2024,
                          paste(Order.2023, "->", Order.2024),
                          ""),
          Family = if_else(Family.2023 != Family.2024,
                           paste(Family.2023, "->", Family.2024),
                           ""),
          Genus = if_else(Genus.2023 != Genus.2024,
                          paste(Genus.2023, "->", Genus.2024),
                          ""),
          Species = if_else(Species.2023 != Species.2024,
                            paste(Species.2023, "->", Species.2024),
                            "")
        )
      )
    ) %>%
      # Filter for rows where any differences exist
    dplyr::filter(!all(differing_values[[1]] == "")) %>%
        # Select only the relevant columns
      dplyr::select(AOU, differing_values)

    # Unnest the differing values into a more readable format with unique names
    clean_differences <- differences %>% tidyr::unnest_wider(differing_values, names_sep = "_diff")

  # Print out the differences
  return(clean_differences)
}


#' main workflow to create differences file, can try with different files
main <- function(L0_dir=LO_dir,  bbs_23_file="bbs_splist_2022_L0.csv", bbs_24_file="BBS2024_SpeciesListL0.csv"){
  bbs_23_file_path <- file.path(L0_dir,bbs_23_file)
  bbs_24_file_path <- file.path(L0_dir, bbs_24_file)

  # read and harmonize columns for compare
  bbs.splist.2023 <- read_bbs_2023( bbs_23_file_path)
  bbs.splist.2024 <- download_current_bbs_csv(bbs_24_file_path)

  #
  clean_diffs <- difference_bbs_species_list_2023_24(bbs.splist.2023, bbs.splist.2024)
  print(clean_diffs)

  diffs_file <- file.path(L0_dir,"bbs23-24.differences.csv")
  write.csv(clean_diffs, diffs_file, row.names = FALSE, fileEncoding = "UTF-8")

}


# Next script to run: bbs_specieslist_L1.R
# Then, if combining with bbs_obs data: AvianInteractionData_L1.R


