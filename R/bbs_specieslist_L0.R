# TITLE:          L0 BBS Species List Data: Pulls in raw BBS SpeciesList from 2023 release (up to 2022 BBS data), 
#                 so it can be used in avian-meta-network (align with intxnpairs data before merging in L2) 
# AUTHORS:        Phoebe Zarnetske
# COLLABORATORS:  Vincent Miele, Stephane Dray, Emily Parker
# DATA INPUT:     Imports L0 raw species list data from the USGS North American Breeding Bird Survey (SpeciesList.txt is updated every year). 
# DATA OUTPUT:    L0 data: bbs_specieslist_L0.csv - this is a copy of the raw data, just omitting the top lines without data
# PROJECT:        Avian Interaction Database & avian-meta-network
# DATE:           17 January 2022 - 6 December 2023
# NOTES:          Adds a row for recent split of Sedge Wren to Sedge and Grass Wren. 
#
#               Next script to run: for avian-meta-network: 
#               NOTES: check out this site for code w BBS: https://rdrr.io/github/davharris/mistnet/src/extras/BBS-analysis/data_extraction/data-extraction.R               
    

# Clear all existing data
rm(list=ls())

#Load packages
library(dplyr)

# Set working directory
L0_dir <- Sys.getenv("L0DIR")

# Above .Renviron not working for PLZ; hard-coding in here
L0_dir <- "/Users/plz/Documents/GitHub/Avian-Interaction-Database/L0"

# Below section is modified from: https://rdrr.io/github/davharris/mistnet/src/extras/BBS-analysis/data_extraction/species-handling.R
# Read in the SpeciesList file: 

# The original file SpeciesList.txt (from 2023 BBS release) is here: https://www.sciencebase.gov/catalog/item/64ad9c3dd34e70357a292cee
# Reading in a copy placed in the L0 directory on Dec. 5, 2023, and renamed BBS_SpeciesListL0.txt

# This function determines if a species is "valid" by looking at its latin and
# common names.  It is called below to read in the data. 

validateSpecies = function(){
  
# The first 10 lines do not contain data; read in first 12 lines which contain column names in 12th line
skip = readLines(file.path(L0_dir,"BBS_SpeciesListL0.txt"), n = 12)

# The 11th line has column names separated by lots of spaces
column.names = grep(".$", strsplit(skip[11], " ")[[1]], value = TRUE)

# The 12th line has dashes that can be used for determining column widths
dashes = skip[12]

bbs.splist = na.omit(
  read.fwf(
    file.path(L0_dir,"BBS_SpeciesListL0.txt"),
    skip = 12, 
    widths = nchar(strsplit(dashes, " ")[[1]]) + 1,
    # below line ensures there are no errors with "invalid multibyte strings"
    fileEncoding="latin1", 
    as.is = TRUE,
    colClasses = "character",
    strip.white = TRUE
  )
)
colnames(bbs.splist) = column.names
rownames(bbs.splist) = bbs.splist$AOU

dim(bbs.splist)
# 760 birds observed in BBS (as of 2023 release)

# species with slashes, " or ", " x " or " X " or " sp." "unid", etc.
# are either unknown or hybrids.
pattern = "/|( or )|( X )|( x )|( sp\\.)|(unid)|hybrid|Admin Code"

bad.latin = grep(
  pattern,
  bbs.splist$Spanish_Common_Name
)

bad.english = grep(
  pattern,
  bbs.splist$English_Common_Name
)

# Something is a potential subspecies if it has three words separated by
# spaces.
possible.subspecies = grep("^.* .* .*$", bbs.splist$Spanish_Common_Name)
subspecies.ID = possible.subspecies[
  is.na(match(possible.subspecies, bad.latin))
]
subspecies = bbs.splist$Spanish_Common_Name[subspecies.ID]

# Assign the genus species to the subspecies: 
# binomial nomenclature version of the subspecies name (i.e., discard the third name)
subspecies.binomial = sapply(
  strsplit(subspecies, " "), 
  function(x){
    paste(x[1:2], collapse = " ")
  }
)

## PLZ suggests NOT INCLUDING THIS in resulting subsetting below bc it would remove 95 species##
# I've also decided to throw out species that have associated subspecies
# (e.g., with half a dozen junco races, the unknown-race-junco isn't really
# informative either)
#has.subspecies = which(bbs.splist$Spanish_Common_Name %in% subspecies.binomial)

#bad.species = sort(
#  unique(c(bad.latin, bad.english, subspecies.ID, has.subspecies))
#)

# get rid of leading 0's in AOU, since they're not used elsewhere
## THIS DOESN'T WORK - needs fixing
rownames(bbs.splist) = gsub("^0", "", rownames(bbs.splist))

## NOT INCLUDING THIS ##
#return(bbs.splist[-bad.species, ])

return(bbs.splist)

}

# Clean up the data with the validateSpecies modified function above.
bbs.splist.2022 = validateSpecies()
dim(bbs.splist.2022)

# A few species have been recently combined or split but there may be entries in the interaction database
# with the old name. Add them here:

# Sedge Wren (Cistothorus	stellaris) and Grass Wren (Cistothorus	platensis) are 2 different species,
# Previously Sedge Wren (Cistothorus	platensis).

bbs.splist.2022[nrow(bbs.splist.2022) + 1,] = list(Seq = "",
                                                    AOU = "07240", 
                                                    English_Common_Name = "Grass Wren",
                                                    French_Common_Name = "Troglodyte bec court",
                                                    Spanish_Common_Name = "Cistothorus platensis",
                                                    ORDER = "Passeriformes",
                                                    Family = "Troglodytidae",
                                                    Genus = "Cistothorus",
                                                    Species = "platensis")

dim(bbs.splist.2022)
# 761 species in BBS with the split to 2 species: Sedge and Grass Wren

# Export the cleaned data (note the encoding to maintain special characters)
write.csv(bbs.splist.2022, file.path(L1_dir,"bbs_splist_2022.csv"), fileEncoding="latin1", row.names=F) 

# Next script to run if combining with bbs_obs data: AvianInteractionData_L1.R

