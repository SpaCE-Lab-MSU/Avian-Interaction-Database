# TITLE:          Avian Interaction Pairs L0 Data Stitching together CSVs into 1
# AUTHORS:        Phoebe Zarnetske, Pat Bills, Emily Parker
# COLLABORATORS:  Vincent Miele, Stephane Dray, ...
# DATA INPUT:     Data imported as csv https://github.com/SpaCE-Lab-MSU/Avian-Interaction-Database/blob/main/L0/species and ./species_in_review
# DATA OUTPUT:    L1 data: AvianInteractionData_L0.csv
# PROJECT:        Avian Interaction Database & Avian Meta-Network
# DATE:           20 Mar 2023 -
# NOTES:          Next script to run: /L1/AvianInteractionData_L1.R


library(readr, dplyr, magrittr)

L0_dir <- "/Users/plz/Documents/GitHub/Avian-Interaction-Database/L0"
species_dir<- "/Users/plz/Documents/GitHub/Avian-Interaction-Database/L0/species"
species_review_dir<- "/Users/plz/Documents/GitHub/Avian-Interaction-Database/L0/species_in_review"
#' given a folder of Intxns CSV files, combine into one file
#' assumes all the CSV files are the same format with the same headers!
combine_by_species <- function(data_dir = file.path(species_dir)) {

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
  
  # combine all in to a single df
  intxns <- dplyr::bind_rows(intxns.list)
  
  # return as a data frame (the operation above is a list type)
  return(data.frame(intxns))
}

intxnsL0sp<-combine_by_species()
sp<-unique(intxnsL0sp$species1_scientific)
sp<-as.list(sp)
length(sp)
# 623 species1 as of Dec 5, 2023 (all double checked)
intxnsL0spir<-combine_by_species_in_review()
## ERROR here - need to fix this so that all files can be combined [species1_common is sometimes coded logical and other times character...
## Error in `dplyr::bind_rows()`:
#! Can't combine `..1$species1_common` <character> and `..144$species1_common` <logical>.
#Run `rlang::last_trace()` to see where the error occurred.
#Called from: signal_abort(cnd, .file)
#spir<-unique(intxnsL0spir$species1_scientific)
#length(spir)
#
# XXX species1 as of Nov 22, 2023

# Uncomment if you want to omit all species that haven't been checked by someone other than Emily;
# for Nov 2023 we are just proceeding with all 'species' and 'species_in_review' because Emily
# entered the BBS birds in the review folder. So keep the below section commented out
# Only keep unique set of species; remove the "in review" species that have already been checked in updated in "species"
# '%!in%'<- function(x,y)!('%in%'(x,y))
# 
# intxnsL0spir_unchecked<-intxnsL0spir[intxnsL0spir[,3] %!in% sp,]
# length(unique(intxnsL0spir_unchecked$species1_scientific))
# XXX unique species1 that are truly unchecked

# Merge the species and species_in_review interaction data into 1 
# (as of Nov 22, 2023, EP created all the "in_review" species, so they are less likely to include errors):
# first add "other_species1" to intxnsL0sp
intxnsL0sp$other_species1<-""
intxnsL0<-rbind(intxnsL0sp, intxnsL0spir)

length(unique(intxnsL0$species1_scientific))
# XXX unique species1 !

# export the data to become the current L0 interaction data:
write.csv(intxnsL0, file.path(L0_dir, "AvianInteractionData_L0.csv"), row.names=FALSE)



