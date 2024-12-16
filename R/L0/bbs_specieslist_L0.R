# TITLE:          L0 BBS Species List Data: Pulls in raw BBS SpeciesList from 2024 release (up to 2023 BBS data), 
#                 so it can be used in avian-meta-network (align with intxnpairs data before merging in L2) 
# AUTHORS:        Phoebe Zarnetske
# COLLABORATORS:  Vincent Miele, Stephane Dray, Emily Parker
# DATA INPUT:     Imports L0 raw species list data from the USGS North American Breeding Bird Survey (SpeciesList.txt is updated every year). 
# DATA OUTPUT:    L0 data: bbs_specieslist_2023_L0.csv - this is a copy of the raw data, just omitting the top lines without data
# PROJECT:        Avian Interaction Database & avian-meta-network
# DATE:           17 January 2022 - 30 October 2024
# NOTES:          bbs_specieslist_2024_L1.csv is produced in bbs_specieslist_L1.R 
#
#               Next script to run: bbs_specieslist_L1.R
#               NOTES: check out this site for code w BBS: https://rdrr.io/github/davharris/mistnet/src/extras/BBS-analysis/data_extraction/data-extraction.R               
    

# Clear all existing data
rm(list=ls())

# Load necessary libraries
library(dplyr)
library(tidyr)

# Above .Renviron not working for PLZ; hard-coding in here
L0_dir <- "/Users/plz/Documents/GitHub/Avian-Interaction-Database/L0"
L1_dir <- "/Users/plz/Documents/GitHub/Avian-Interaction-Database/L1"

# Below section is modified from: https://rdrr.io/github/davharris/mistnet/src/extras/BBS-analysis/data_extraction/species-handling.R
# Read in the SpeciesList file: 

# The original file SpeciesList.csv (from 2024 BBS release) is here: https://www.sciencebase.gov/catalog/item/66d9ed16d34eef5af66d534b
# Reading in a copy placed in the L0 directory on October 22, 2024, and renamed BBS2024_SpeciesListL0.txt
guess_encoding(file.path(L0_dir,"bbs_splist_2022_L0.csv"))
#ISO-8859-1
bbs.splist.2023<-read.csv(file.path(L0_dir,"bbs_splist_2022_L0.csv"),fileEncoding="ISO-8859-1")
guess_encoding(file.path(L0_dir,"BBS2024_SpeciesListL0.csv"))
#UTF-8
bbs.splist.2024<-read.csv(file.path(L0_dir,"BBS2024_SpeciesListL0.csv"),fileEncoding="UTF-8")

# Make a column for genus_species
bbs.splist.2024$genus_species<- do.call(paste, c(bbs.splist.2024[c("Genus", "Species")], sep = " "))
names(bbs.splist.2023)
names(bbs.splist.2024)
bbs.splist.2023$Spanish_Common_Name<-NULL
bbs.splist.2023$Order<-bbs.splist.2023$ORDER
bbs.splist.2023$ORDER<-NULL

# Make a column for genus_species
bbs.splist.2024$genus_species<- do.call(paste, c(bbs.splist.2024[c("Genus", "Species")], sep = " "))
dim(bbs.splist.2024)
# 763 species

# Compare the list from last year to this year to identify changes:

# Full join on "AOU" only, ignoring extra rows in the 2024 data
differences <- bbs.splist.2023 %>%
  full_join(bbs.splist.2024, by = "AOU", suffix = c(".2023", ".2024")) %>%
  # Use rowwise to apply comparisons across columns
  rowwise() %>%
  # Create a data frame of differing values
  mutate(
    differing_values = list(
      tibble(
        AOU = AOU,
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
  filter(!all(differing_values[[1]] == "")) %>%
  # Select only the relevant columns
  select(AOU, differing_values)

# Unnest the differing values into a more readable format with unique names
clean_differences <- differences %>%
  unnest_wider(differing_values, names_sep = "_diff")

# Print out the differences
print(clean_differences, n = Inf)

# Export to take a look (ignore the encodings which don't match)
write.csv(clean_differences,file.path(L0_dir,"BBS_specieslist_diffs2023-2024.csv"), fileEncoding="UTF-8", row.names=F)
# The current comparison lists these species as changing since last time:
# differing_values_diffEnglish_Common_Name
# Northern Goshawk -> American Goshawk
# (unid. Red/Yellow Shafted) Northern Flicker -> (unid. Red / Yellow Shafted) Northern Flicker
# Pacific-slope Flycatcher -> (Pacific-slope Flycatcher) Western Flycatcher
# Cordilleran Flycatcher -> (Cordilleran Flycatcher) Western Flycatcher
# unid. Cordilleran / Pacific-slope Flycatcher -> (unid. Cordilleran / Pac-slope) Western Flycatcher
# Swinhoe¬ís White-eye -> Swinhoe‚Äôs White-eye
# Unid. Cassia Crossbill / Red Crossbill -> unid. Cassia Crossbill / Red Crossbill
# (unid. Myrtle/Audubon's) Yellow-rumped Warbler -> (unid. Myrtle / Audubon's) Yellow-rumped Warbler

# The species re-assignment checking will occur in the next script. 
# Export the cleaned data (note the encoding to maintain special characters)
write.csv(bbs.splist.2024, file.path(L0_dir,"bbs_splist_2024_L0.csv"), fileEncoding="UTF-8", row.names=F) 

# Next script to run: bbs_specieslist_L1.R 
# Then, if combining with bbs_obs data: AvianInteractionData_L1.R

# ************************* #
# Older code when the file read in was in .txt. format and had many lines in the beginning to ignore:
# This function determines if a species is "valid" by looking at its latin and
# common names.  It is called below to read in the data. 

# validateSpecies = function(){
#   
# # On older versions the first 10 lines do not contain data; read in first 12 lines which contain column names in 12th line
# skip = readLines(file.path(L0_dir,"BBS_SpeciesListL0.txt"), n = 12)
# 
# # The 11th line has column names separated by lots of spaces
# column.names = grep(".$", strsplit(skip[11], " ")[[1]], value = TRUE)
# 
# # The 12th line has dashes that can be used for determining column widths
# dashes = skip[12]
# 
# bbs.splist = na.omit(
#   read.fwf(
#     file.path(L0_dir,"BBS2024_SpeciesListL0.csv"),
#     # below line ensures there are no errors with "invalid multibyte strings"
#     fileEncoding="latin1", 
#     as.is = TRUE,
#     colClasses = "character",
#     strip.white = TRUE
#   )
# )
# colnames(bbs.splist) = column.names
# rownames(bbs.splist) = bbs.splist$AOU
# 
# dim(bbs.splist)
# # 760 birds observed in BBS (as of 2023 release)
# 
# # species with slashes, " or ", " x " or " X " or " sp." "unid", etc.
# # are either unknown or hybrids.
# pattern = "/|( or )|( X )|( x )|( sp\\.)|(unid)|hybrid|Admin Code"
# 
# bad.latin = grep(
#   pattern,
#   bbs.splist$Spanish_Common_Name
# )
# 
# bad.english = grep(
#   pattern,
#   bbs.splist$English_Common_Name
# )
# 
# # Something is a potential subspecies if it has three words separated by
# # spaces.
# possible.subspecies = grep("^.* .* .*$", bbs.splist$Spanish_Common_Name)
# subspecies.ID = possible.subspecies[
#   is.na(match(possible.subspecies, bad.latin))
# ]
# subspecies = bbs.splist$Spanish_Common_Name[subspecies.ID]
# 
# # Assign the genus species to the subspecies: 
# # binomial nomenclature version of the subspecies name (i.e., discard the third name)
# subspecies.binomial = sapply(
#   strsplit(subspecies, " "), 
#   function(x){
#     paste(x[1:2], collapse = " ")
#   }
# )
# 
# ## PLZ suggests NOT INCLUDING THIS in resulting subsetting below bc it would remove 95 species##
# # I've also decided to throw out species that have associated subspecies
# # (e.g., with half a dozen junco races, the unknown-race-junco isn't really
# # informative either)
# #has.subspecies = which(bbs.splist$Spanish_Common_Name %in% subspecies.binomial)
# 
# #bad.species = sort(
# #  unique(c(bad.latin, bad.english, subspecies.ID, has.subspecies))
# #)
# 
# # get rid of leading 0's in AOU, since they're not used elsewhere
# ## THIS DOESN'T WORK - needs fixing
# rownames(bbs.splist) = gsub("^0", "", rownames(bbs.splist))
# 
# ## NOT INCLUDING THIS ##
# #return(bbs.splist[-bad.species, ])
# 
# return(bbs.splist)
# 
# }
# # Clean up the data with the validateSpecies modified function above.
# bbs.splist.2024 = validateSpecies()
# dim(bbs.splist.2024)
