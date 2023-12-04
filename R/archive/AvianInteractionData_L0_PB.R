# TITLE:          Avian Interaction Pairs L0 Data Checking
# AUTHORS:        Phoebe Zarnetske, Pat Bills
# COLLABORATORS:  Vincent Miele, Stephane Dray, Sara Zonneveld, ...
# DATA INPUT:     Data imported as csv https://github.com/SpaCE-Lab-MSU/Avian-Interaction-Database/blob/main/L0/AvianInteractionData_raw.csv
# DATA OUTPUT:    L1 data: AvianInteractionData_L0.csv
# PROJECT:        avian-meta-network
# DATE:           27 Oct 2022 -
# NOTES:          Next script to run: /L1/AvianInteractionData_L1.R
#
#


library(readr, dplyr, magrittr)

validate_intxns_data <- function(intxns.file) {


    if(file.exists(intxns.file)){
        # Read in csv with avian interactions from primary, secondary cavity nesting birds in North America.
        intxns.raw<-read.csv(intxns.file)
    } else {
        warning(paste(" could not find interaction data file", intxns.file ))
        return(NULL)
    }

    # ... checking code here create new intxns file
    intxns.cleaned <- intxns.raw

    return(intxns.cleaned)
}

write_cleaned_intxns_data <- function() {
    #Clear all existing data
    rm(list=ls())
    #
    # Set working directory
    L0_dir <- Sys.getenv("L0DIR")

    # # Above .Renviron not working for PLZ; hard-coding in here
    L0_dir <- "/Users/plz/DATA/git/Avian-Interaction-Database/L0"
    validate_intxns_data
    write.csv(intxns.raw, file.path(L0_dir, "AvianInteractionData_L0.csv"), row.names=F)

}

#' convert a species binomial for use as a linux filename, no punctuation or spaces
species2filename <- function(species_binomial){
    tolower(species_binomial) %>% gsub(pattern = "[[:punct:]]", replacement = "", .) %>% gsub(" ", delimiter,.)
}

read_species_file <- function(species, data_dir = file.path("L0", "species")) {
    filepath = file.path(data_dir,
                         paste0(species2filename(species),".csv")
    )
    if (file.exists(filepath)){
        intxns.species = read.csv(filepath)
        # if(! intxns_validated(intxns.species)) { return None }
        return(intxns.species)

    } else {
        warning(paste("species file not found", filepath))
        return(NULL)
    }

}

#' given a folder of Intxns CSV files, combine into one file
#' assumes all the CSV files are the same format with the same headers!
combine_by_species <- function(data_dir = file.path("L0","species")) {

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

    # combine all in to a single df
    intxns <- dplyr::bind_rows(intxns.list)

    # return as a data frame (the operation above is a list type)
    return(data.frame(intxns))
}
