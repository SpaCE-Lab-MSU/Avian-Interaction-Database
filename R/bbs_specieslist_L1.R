# TITLE:          L1 BBS Species List Data: Pulls in raw BBS SpeciesList from 2023 release (up to 2022 BBS data), 
#                 so it can be used in avian-meta-network (align with intxnpairs data before merging in L2) 
# AUTHORS:        Phoebe Zarnetske
# COLLABORATORS:  Vincent Miele, Stephane Dray, Emily Parker
# DATA INPUT:     Imports L0 raw species list data from the USGS North American Breeding Bird Survey. 
# DATA OUTPUT:    L1 data: bbs_specieslist.csv
# PROJECT:        Avian Interaction Database & avian-meta-network
# DATE:           17 January 2022 - 5 December 2023
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
L1_dir <- Sys.getenv("L1DIR")
list.files(L1_dir)

# Above .Renviron not working for PLZ; hard-coding in here
L0_dir <- "/Users/plz/Documents/GitHub/Avian-Interaction-Database/L0"
L1_dir <- "/Users/plz/Documents/GitHub/Avian-Interaction-Database/L1"

# Below section is modified from: https://rdrr.io/github/davharris/mistnet/src/extras/BBS-analysis/data_extraction/species-handling.R
# Read in the SpeciesList file: 

# The original file SpeciesList.txt (from 2023 BBS release) exists here: https://www.sciencebase.gov/catalog/item/64ad9c3dd34e70357a292cee
# Reading in a copy placed in the L0 directory on Dec. 5, 2023, and renamed SpeciesListL0.txt

# This function determines if a species is "valid" by looking at its latin and
# common names.  It is called below to read in the data. 

validateSpecies = function(){
  
# The first 10 lines do not contain data; read in first 12 lines which contain column names in 12th line
skip = readLines(file.path(L0_dir,"SpeciesListL0.txt"), n = 12)

# The 11th line has column names separated by lots of spaces
column.names = grep(".$", strsplit(skip[11], " ")[[1]], value = TRUE)

# The 12th line has dashes that can be used for determining column widths
dashes = skip[12]

bbs.splist = na.omit(
  read.fwf(
    file.path(L0_dir,"SpeciesListL0.txt"),
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

# Export the cleaned data (note the encoding to maintain special characters)
write.csv(bbs.splist.2022, file.path(L1_dir,"bbs_splist.csv"), fileEncoding="latin1", row.names=F) 

#### END OF SCRIPT TO EXPORT CLEANED Species List; BELOW = notes from before Dec 2023####
# Some of the species from interactions sourced from BOW are not in BBS data 
# Some of these birds could be in the Mexico data: https://www.sciencebase.gov/catalog/item/5f32af1082cee144fb313837

# Auklets:
#  Aethia cristatella
#  Aethia psittacula
#  Aethia pusilla
#  
#  Alle alle
#
# Hummingbirds:
#  Amazilia beryllinus
#  Amazilia rutila
#  Basilinna leucotis
#  Basilinna xantusii
#  Colibri thalassinus
#  Cynanthus canivetii
#  Lampornis amethystinus
#  Phaethornis superciliosus
#  
# Noddy: 
#  Anous stolidus
#
# Duck:
#  Nomonyx dominicus
#
# Puffin:
#  Fratercula arctica
#
# Magpies:
#  Calocitta formosa
#  Pica pica
#
# Scrub-Jays: 
#  Aphelocoma sp.
#  Aphelocoma ultramarina (Transvolcanic Jay: distribution in narrow band of Mexico)
#  
# Jay:
#  Cyanocorax morio
# 
# Phalarope:
#  Phalaropus fulicarius
#
# Crow:
#  Corvus imparatus
#
# Woodpecker:
#  Dryocopus martius
#
# Chaffinch:
#  Fringilla coelebs
#
# Snipe:
#  Gallinago gallinago
#
# Stercorarius spp.
# 
# Goose:
#  Branta sandvicensis
#
# Storm Petrel:
#  Hydrobates furcatus
#  Hydrobates melania
# 
# Gull:
#  Larus livens
# 
# Sheerwater:
#   Puffinus opisthomelas
#
# Rail:
#  Rallus longirostris
#
# Rhea sp.
# 
# Smew:
#  Mergellus albellus
#
# Wagtail:
#  Motacilla flava
#
# Chickadee:
#  Poecile cinctus
#
# Gnatcatcher:
#  Polioptila nigriceps
# 
# Warbler:
#  Leiothlypis crissalis
#  Setophaga adelaidae
# 
# Vireo:
#  Vireo modestus
#
# Bluethroat:
#  Luscinia svecica
#
# Parrot:
#  Rhynchopsitta pachyrhyncha
#
# Kittiwake:
#  Rissa brevirostris
#
# Murrelet:
#  Synthliboramphus craveri
#  Synthliboramphus scrippsi
# 
# Cormorant:
#  Urile urile
#
# Lapwing:
#  Vanellus sp.
#
# Owl:
#  Strix aluco
#  Strix uralensis

### BELOW NOT RUN ###
# Name changes to be consistent with Birds of the World
bow.splist <- bbs.splist.final
bow.splist[] <- bbsbow$genus_species[match(unlist(bbs.splist.final), bbsbow$bow)]

new <- df
new[] <- look$class[match(unlist(df), look$pet)]

# Export the cleaned data (note the encoding to maintain special characters)
write.csv(bbs.splist.final, file.path(L1_dir,"bow_splist.csv"), fileEncoding="latin1", row.names=F) 

# bbs.splist.final Otus flammeolus

# Subset out list of 27 primary cavity nesting birds found in the BBS by referencing their AOU
str(bbs.splist.final)

primarycavnest<-bbs.splist.final[grep("4080|4060|4070|4110|4100|4090|4040|4020|4021|4030|4022|4010|4000|3940|3970|3960|3950|3930|3990|3975|4123|4120|4130|4125|4140|4050|3920",bbs.splist.final$AOU),]

dim(primarycavnest)                                        
unique(primarycavnest$AOU)
# Checks out: 27 species

dim(secondcavnest)                                        
unique(secondcavnest$AOU)

# Assign a column "cavitynester"
primarycavnest$cavitynester<-"primary"

# rbind the 3 sets


# Export the primary cavity nester list (note the encoding to maintain special characters)
# write.csv(cavnest, file.path(L1_dir,"cavitynest_interactors.csv"), fileEncoding="latin1", row.names=F) 

