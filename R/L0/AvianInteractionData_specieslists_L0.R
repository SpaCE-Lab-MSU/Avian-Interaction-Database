# TITLE:          L0 Species List Data: Pulls in raw lists from sources and 
#                   creates formatted versions of lists

# AUTHORS:        Phoebe Zarnetske
# COLLABORATORS:  Vincent Miele, Stephane Dray, Emily Parker
# DATA INPUT:     Imports L0 raw species list data from:
#                 (1) the USGS North American Breeding Bird Survey (BBS) SpeciesList 
#                 from 2024 release (up to 2023 BBS data; SpeciesList.txt is updated yearly). 
#                 (2) CA-CONUS List = AviBase Canada list + AviBase Lower 48 US + 
#                 AviBase Alaska list (= CONUS List(taxonomy from Clements 2024 list)
# DATA OUTPUT:    (1) BBS List L0 data: bbs_specieslist_2023_L0.csv - this is a 
#                     copy of the raw data, just omitting the top lines without data
#                 (2) CACONUS list L0 data: caconus_specieslist_2024_L0.csv - this is 
#                     data pulled from the AviBase species list website and formatted
# PROJECT:        Avian Interaction Database & avian-meta-network
# DATE:           17 January 2022 - 8 August 2025
# NOTES:          bbs_specieslist_2024_L1.csv is produced in bbs_specieslist_L1.R 
#
#               Next script to run: bbs_specieslist_L1.R
#               NOTES: check out this site for code w BBS: https://rdrr.io/github/davharris/mistnet/src/extras/BBS-analysis/data_extraction/data-extraction.R               
    

# Clear all existing data
rm(list=ls())

# Load necessary libraries
library(dplyr)
library(tidyr)
library(rvest) # for web scraping
library(stringr)

# Above .Renviron not working for PLZ; hard-coding in here
L0_dir <- "/Users/plz/Documents/GitHub/Avian-Interaction-Database-Working/L0"
L1_dir <- "/Users/plz/Documents/GitHub/Avian-Interaction-Database-Working/L1"

#### (1) BBS List ####
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
write.csv(bbs.splist.2024, file.path(L0_dir,"./species_checklists/bbs_splist_2024_L0.csv"), fileEncoding="UTF-8", row.names=F) 

# Next script to run: bbs_specieslist_L1.R 
# Then, if combining with bbs_obs data: AvianInteractionData_L1.R



#### (2) CA-CONUS List ####
# This list represents birds observed in Canada and the Continental United States
# from AviBase, using Clements 2024 taxonomy.

# CANADA species: https://avibase.bsc-eoc.org/checklist.jsp?lang=EN&p2=1&list=clements&synlang=&region=US48&version=text&lifelist=&highlight=0
 
ca<- "https://avibase.bsc-eoc.org/checklist.jsp?lang=EN&p2=1&list=clements&synlang=&region=US48&version=text&lifelist=&highlight=0" 
avi.ca.page <- read_html(ca)

# Extract all tables and select the desired table
ca.tables <- html_nodes(avi.ca.page, "table")
ca.table <- html_table(ca.tables[[1]], fill = TRUE) 
head(ca.table) # it's the correct table

# Rename columns
ca.table <- ca.table %>%
  rename(common_name = X1, scientific_name = X2, status=X3)

# Move the order and family info to new columns
ca.table <- ca.table %>%
  mutate(
    # Capture order (all caps) and family (first letter caps) separately
    order = str_extract(common_name, "\\b[A-Z]+(?: [A-Z]+)*(?=: )"),
    family = str_extract(common_name, "(?<=: )[A-Z][a-z]+")
  ) %>%
  fill(order, family) %>%
  filter(!(common_name == paste(order, family, sep = ": ")))
ca.table

# US Lower 48 species: https://avibase.bsc-eoc.org/checklist.jsp?lang=EN&p2=1&list=clements&synlang=&region=US48&version=text&lifelist=&highlight=0

us48<-"https://avibase.bsc-eoc.org/checklist.jsp?lang=EN&p2=1&list=clements&synlang=&region=US48&version=text&lifelist=&highlight=0" 
avi.us48.page <- read_html(us48)

# Extract all tables and select the desired table
us48.tables <- html_nodes(avi.us48.page, "table")
us48.table <- html_table(us48.tables[[1]], fill = TRUE) 
head(us48.table) # it's the correct table

# Rename columns
us48.table <- us48.table %>%
  rename(common_name = X1, scientific_name = X2, status=X3)

# Move the order and family info to new columns
us48.table <- us48.table %>%
  mutate(
    # Capture order (all caps) and family (first letter caps) separately
    order = str_extract(common_name, "\\b[A-Z]+(?: [A-Z]+)*(?=: )"),
    family = str_extract(common_name, "(?<=: )[A-Z][a-z]+")
  ) %>%
  fill(order, family) %>%
  filter(!(common_name == paste(order, family, sep = ": ")))
us48.table

# Alaska species: https://avibase.bsc-eoc.org/checklist.jsp?lang=EN&p2=1&list=clements&synlang=&region=USak&version=text&lifelist=&highlight=0

ak<-"https://avibase.bsc-eoc.org/checklist.jsp?lang=EN&p2=1&list=clements&synlang=&region=USak&version=text&lifelist=&highlight=0" 
avi.ak.page <- read_html(ak)

# Extract all tables and select the desired table
ak.tables <- html_nodes(avi.ak.page, "table")
ak.table <- html_table(ak.tables[[1]], fill = TRUE) 
head(ak.table) # it's the correct table

# Rename columns
ak.table <- ak.table %>%
  rename(common_name = X1, scientific_name = X2, status=X3)

# Move the order and family info to new columns
ak.table <- ak.table %>%
  mutate(
    # Capture order (all caps) and family (first letter caps) separately
    order = str_extract(common_name, "\\b[A-Z]+(?: [A-Z]+)*(?=: )"),
    family = str_extract(common_name, "(?<=: )[A-Z][a-z]+")
  ) %>%
  fill(order, family) %>%
  filter(!(common_name == paste(order, family, sep = ": ")))
ak.table

# Merge CA, Lower 48, Alaska and omit duplicates. 
# First check that there isn't an issue with merging and finding mismatches.
library(dplyr)
library(stringr)

# Add region tags
ak.table   <- ak.table   %>% mutate(region = "AK")
ca.table   <- ca.table   %>% mutate(region = "CA")
us48.table <- us48.table %>% mutate(region = "US48")

# Merge all three
merged.table <- bind_rows(ak.table, ca.table, us48.table)

# Normalize whitespace in key columns
merged.table <- merged.table %>%
  mutate(
    scientific_name = str_squish(scientific_name),
    common_name     = str_squish(common_name),
    order           = str_squish(order),
    family          = str_squish(family),
    status          = str_squish(status)
  )

# Set priority: US48 first
region_priority <- c("US48", "CA", "AK")

merged.table <- merged.table %>%
  mutate(region_factor = factor(region, levels = region_priority))

# Select one row per scientific_name by priority region
ca.conus <- merged.table %>%
  arrange(scientific_name, region_factor) %>%
  group_by(scientific_name) %>%
  slice(1) %>%
  ungroup() %>%
  select(-region_factor)

cat("Number of species in final cleaned table:", n_distinct(ca.conus$scientific_name), "\n")
cat("Number of rows in final cleaned table:", nrow(ca.conus), "\n")

ca.conus

# Export this for final cleaning in L1 script where species are furthe subset.
write.csv(ca.conus, file.path(L0_dir,"./species_checklists/avibase_ca.conus_splist_2024_L0.csv"), fileEncoding="UTF-8", row.names=F) 



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
# 
