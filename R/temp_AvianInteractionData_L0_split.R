# TITLE:          Avian Interaction Pairs L0 Data Split
# AUTHORS:        Phoebe Zarnetske, Pat Bills
# COLLABORATORS:  Vincent Miele, Stephane Dray, Sara Zonneveld, ...
# DATA INPUT:     Data imported as csv L0/AvianInteractionData_raw.csv
# DATA OUTPUT:    L0 data: many per-species files in L0
# PROJECT:        avian-meta-network
# DATE:           2022-12-05
# NOTES:          This is a temporary script run once to facilitate new data entry procedures.
#


library(readr, dplyr, magrittr)

#' convert a species binomial for use as a linux filename, no punction or spaces
#'

species2filename <- function(species_binomial, delimiter = "_"){
    tolower(species_binomial) %>% gsub(pattern = "[[:punct:]]", replacement = "", .) %>% gsub(" ", delimiter,.)
}


#' given an Intxns CSV with multiple species, split it up on species and save in many files.
#'
#'  Do not run this function, it's already been run but saved here for documentation.   Originally
#' the main data CSV was a single file which proved difficult to use git.  This function was written to split
#' that main file up along species 1 sci names.   This function is not smart, if there is a problem when
#' writing one of the files, it will simply crash, and leave the files it was able to write in the folder.
#'  However if a file with the same name exists in the target directory, it will not over write it.
#'
#' params
#'   intxns.file : relative path to the CSV file with interaction data, with minimally a column species1_scientific
#'   save_dir : the folder where the split CSVs are saved.  if there is data, it will automatically be overwriten

split_by_species <- function( intxns.file = file.path("L0","AvianInteractionData_raw.csv"),
                              save_dir = NULL) {


    if(! file.exists(intxns.file)) {
        warning(paste("intxns file", intxns.file, "not found, exiting"))
        return(FALSE)
    }


    if (is.null(save_dir)) {
        data_dir = tempdir()
        warning(paste(' no save_dir param, using temp folder ', save_dir))
    } else {
        dir.create(save_dir)
        warning(paste("writing files to ", save_dir, " any files present will be overwritten"))
    }

    intxns.all <- read.csv(intxns.file)
    # add column in order to ignore case and spacing when matching
    intxns.all$species_filename <- species2filename(intxns.all$species1_scientific) # tolower(intxns.all$species1_scientific) %>% gsub(pattern = "[[:punct:]]", replacement = "", .) %>% gsub(" ", delimiter,.)

    # loop through each item.
    for(s in unique(intxns.all$species_filename )) {
    # select rows for species, remove extra column we just added
        print(s)
        csv_file_path = file.path(save_dir, paste0(s, ".csv") )
        if (file.exists(csv_file_path)){
            warning(paste(csv_file_path, "file exists, not overwriting"))
        }

        intxns.one_species <- dplyr::filter(intxns.all, species_filename == s) %>% dplyr::select(-species_filename)
        # check we have data
        if(nrow(intxns.one_species) > 0) {
            readr::write_csv( intxns.one_species,  csv_file_path)
            if (! file.exists(csv_file_path)) {
                warning(paste("no species file written for", s))
            } else {
                print(paste(s, "complete"))
            }
        } else {
        warning(paste("no data returned when filtering on ", s, ": no file written"))
        }

    }
}

