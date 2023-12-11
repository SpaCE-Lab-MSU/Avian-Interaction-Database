# TITLE:          Avian Interaction Pairs Data: L0 to L1, including an option to subset species1 for only BBS species
# AUTHORS:        Phoebe Zarnetske, Pat Bills
# COLLABORATORS:  Vincent Miele, Stephane Dray, Emily Parker
# DATA INPUT:     From AvianInteractionData_L0_stitch.R: Data imported as csv https://github.com/SpaCE-Lab-MSU/Avian-Interaction-Database/blob/main/L0/AvianInteractionData_L0.csv
#                 From bbs_specieslist_L1.R: Data imported as csv https://github.com/SpaCE-Lab-MSU/Avian-Interaction-Database/blob/main/L0/bbs_splist_L0.csv
# DATA OUTPUT:    L1 data: AvianInteractionData_L1.csv
# PROJECT:        Avian Interaction Database 
# DATE:           27 Oct 2022; updated 20 Mar 2023, Dec. 5, 2023  
# NOTES:          Next script to run: 
#                 This script is used to refine species name changes to align with BOW, and to create AvianInteractionData_L1.csv 
#                 L0 data are checked to assign BOW scientific and common names to the interaction pairs data (which were originally from BBS species list). 
#               
#               
#         ******June 8, 2022: updated to copy Colaptes auratus interactions to all Colaptes auratus subspecies
#               This assumes that they all have the same ranges (they don't), so a refined set of interactions should be edited to
#               match the actual overlapping areas with interactors
#
#               
# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)

# Set working directory
L0_dir <- Sys.getenv("L0DIR")
L1_dir <- Sys.getenv("L1DIR")
list.files(L1_dir)

# Above .Renviron not working for PLZ; hard-coding in here
L0_dir <- "/Users/plz/Documents/GitHub/Avian-Interaction-Database/L0"
L1_dir <- "/Users/plz/Documents/GitHub/Avian-Interaction-Database/L1"

# Read in csv with avian interactions from primary, secondary cavity nesting birds in North America.
int.raw<-read.csv(file.path(L0_dir,"AvianInteractionData_L0.csv"))

# Read in species list: all species in BBS (the 2023 release which includes all species as of 2022)
splist<-read.csv(file.path(L0_dir,"bbs_splist_L0.csv"))

# Read in the look-up table with the different bbs & bow & old names for species
namechg<-read.csv(file.path(L0_dir,"bbsbow_names.csv"))

# make "genus species" columns able to merge
splist$species1_scientific<- do.call(paste, c(splist[c("Genus", "Species")], sep = " "))

# Rename some columns and omit others; indicate that the common name is coming from BBS Species List
names(splist)[names(splist) == "English_Common_Name"] <-"bbs_sp1_common"
names(splist)[names(splist) == "Seq"] <-"sp1_Seq"
names(splist)[names(splist) == "AOU"] <-"sp1_AOU"

##### START OF NAME CHANGES #####
# We are using the Birds of The World naming conventions for species. Some BBS names differ. Some old names are included.
# Reference the bbsbow_names data to make initial changes to any "other_or_old_bow" names that might appear.
# Apply changes only to the species1 and species2 columns.
# First omit any rows with a blank in "other_or_old_bow"
dim(namechg)
namechg.orig <- namechg
namechg<-namechg[!(is.na(namechg$other_or_old_bow) | namechg$other_or_old_bow==""), ]
#namechg <- namechg[!is.na(namechg$other_or_old_bow), ]
dim(namechg)
# Save original copy of int.raw
int.raw.orig <- int.raw
dim(int.raw)

sort(unique(int.raw$species1_scientific))

# Remove extra end spaces:
int.raw$species1_scientific<-trimws(int.raw$species1_scientific, "r")
int.raw$species2_scientific<-trimws(int.raw$species2_scientific, "r")

sort(unique(int.raw$species1_scientific))

# Some misspellings/typos:
int.raw[int.raw=="Vireo olivaceous"] <- "Vireo olivaceus"
int.raw[int.raw=="Vermivora chyrsoptera"] <- "Vermivora chrysoptera"
int.raw[int.raw=="tyto alba"] <- "Tyto alba"
int.raw[int.raw=="Tyrannus couchi"] <- "Tyrannus couchii"
int.raw[int.raw=="Sternulla antillarum"] <- "Sternula antillarum"
int.raw[int.raw=="Quisculus major"] <- "Quiscalus major"
int.raw[int.raw=="Quisculus mexicanus"] <- "Quiscalus mexicanus"
int.raw[int.raw=="Psaltriparus minimum"] <- "Psaltriparus minimus"
int.raw[int.raw=="Poecila atricapillus"] <- "Poecile atricapillus"
int.raw[int.raw=="Pitandus sulphuratus"] <- "Pitangus sulphuratus"
int.raw[int.raw=="Passerina caerula"] <- "Passerina caerulea"
int.raw[int.raw=="Moticilla alba"] <- "Motacilla alba"
int.raw[int.raw=="Moticilla alba lugens"] <- "Motacilla alba lugens"
int.raw[int.raw=="Moticilla alba leucopsis"] <- "Motacilla alba leucopsis"
int.raw[int.raw=="Moticilla alba ocularis"] <- "Motacilla alba ocularis"
int.raw[int.raw=="mergus merganser"] <- "Mergus merganser"
int.raw[int.raw=="Icterus cuccullatus"] <- "Icterus cucullatus"
int.raw[int.raw=="Helmitheros vermivora"] <- "Helmitheros vermivorum"
int.raw[int.raw=="Cochlearius cohlearius"] <- "Cochlearius cochlearius"

# Change names in species1_scientific and species2_scientific according to the look-up table
# so that all species1 and species2 interactors have the up-to-date BOW name.
int.raw$species1_scientific <- stringr::str_replace_all(int.raw$species1_scientific, 
                                         setNames(namechg$bow, namechg$other_or_old_bow))
int.raw$species2_scientific <- stringr::str_replace_all(int.raw$species2_scientific, 
                                         setNames(namechg$bow, namechg$other_or_old_bow))


# Some interaction scientific names are still not matching Birds of the World 
# See: ./L0/bbsbow_names.csv
# Find them by starting with the original look-up table
#namechg.unmatch = na.omit(namechg.orig)
namechg.unmatch = subset(namechg.orig, namechg.orig$bbs2022 != namechg.orig$bow)
# remove the NAs in bbs2022 column
namechg.unmatch <- namechg.unmatch[-which(namechg.unmatch$bbs2022 == ""), ]
dim(namechg.unmatch) # 10 species on Dec 6, 2023
namechg.unmatch[,1:2]

# Standardize based on BOW 

# RENAME: BBS: Streptopelia chinensis = BOW: Spilopelia chinensis
dplyr::filter(splist, species1_scientific %in% c("Streptopelia chinensis")) # in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Streptopelia chinensis")) # in interactions
dplyr::filter(int.raw, species2_scientific %in% c("Streptopelia chinensis")) # in interactions
dplyr::filter(splist, species1_scientific %in% c("Spilopelia chinensis")) # not in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Spilopelia chinensis")) # in interactions
dplyr::filter(int.raw, species2_scientific %in% c("Spilopelia chinensis")) # in interactions
# Update all to new species name
splist$species1_scientific[splist$species1_scientific == "Streptopelia chinensis"] <- "Spilopelia chinensis"
int.raw$species1_scientific[int.raw$species1_scientific == "Streptopelia chinensis"] <- "Spilopelia chinensis"
int.raw$species2_scientific[int.raw$species2_scientific == "Streptopelia chinensis"] <- "Spilopelia chinensis"

##   RECENT SPLIT: Pica pica (Eurasia), P. hudsonia (North America), and P. nuttalli (North America); check common name for species info
dplyr::filter(splist, species1_scientific %in% c("Pica pica")) # not in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Pica pica")) # in interactions
dplyr::filter(int.raw, species2_scientific %in% c("Pica pica")) # in interactions
dplyr::filter(splist, species1_scientific %in% c("Pica hudsonia")) # in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Pica hudsonia")) # in interactions
dplyr::filter(int.raw, species2_scientific %in% c("Pica hudsonia")) # in interactions
dplyr::filter(splist, species1_scientific %in% c("Pica nuttalli")) # in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Pica nuttalli")) # in interactions
dplyr::filter(int.raw, species2_scientific %in% c("Pica nuttalli")) # in interactions
# For now, keep as is; the BBS observations include P. hudsonia and P. nuttalli

# Recent split: Hen Harrier (Circus cyaneus; Eurasia) & Northern Harrier (Circus hudsonius; North America)
dplyr::filter(splist, species1_scientific %in% c("Circus cyaneus")) # not in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Circus cyaneus")) # in interactions - as Hen and Northern
dplyr::filter(int.raw, species2_scientific %in% c("Circus cyaneus")) # in interactions - as Hen and Northern
# Update all N Harrier to new species name
int.raw$species1_scientific[int.raw$species1_scientific == "Circus cyaneus" & int.raw$species1_common == "Northern Harrier"] <- "Circus hudsonius"
int.raw$species1_scientific[int.raw$species1_scientific == "Circus cyaneus" & int.raw$species1_common == "northern harrier"] <- "Circus hudsonius"
int.raw$species2_scientific[int.raw$species2_scientific == "Circus cyaneus" & int.raw$species2_common == "Northern Harrier"] <- "Circus hudsonius"
int.raw$species2_scientific[int.raw$species2_scientific == "Circus cyaneus" & int.raw$species2_common == "northern harrier"] <- "Circus hudsonius"

## American Crow Subspecies = Northwestern Crow: Corvus brachyrhynchos caurinus (instead of Corvus caurinus "Northwestern Crow")
dplyr::filter(splist, species1_scientific %in% c("Corvus brachyrhynchos")) # in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Corvus brachyrhynchos")) # in interactions 
dplyr::filter(splist, species1_scientific %in% c("Corvus brachyrhynchos caurinus")) # not in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Corvus brachyrhynchos caurinus")) # in interactions 
dplyr::filter(splist, species1_scientific %in% c("Corvus caurinus")) # not in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Corvus caurinus")) # not in interactions 
dplyr::filter(int.raw, species2_scientific %in% c("Corvus caurinus")) # not in interactions 
# RENAME: BBS & BOW: change Corvus caurinus to Corvus brachyrhynchos caurinus 
splist$species1_scientific[splist$species1_scientific == "Corvus caurinus"] <- "Corvus brachyrhynchos caurinus"
int.raw$species1_scientific[int.raw$species1_scientific == "Corvus caurinus"] <- "Corvus brachyrhynchos caurinus"
int.raw$species1_scientific[int.raw$species2_scientific == "Corvus caurinus"] <- "Corvus brachyrhynchos caurinus"

##  RECENT SPLIT: Larus brachyrhynchus split into L. brachyrhynchus (North America; Short-Billed Gull) and L. canus (Eurasia)
##  https://birdsoftheworld.org/bow/species/mewgul/cur/introduction#sys
# BBS: Larus brachyrhynchus (endemic to North America) = BOW: Larus canus (endemic to Eurasia) & Larus brachyrhynchus (endemic to North America)
dplyr::filter(splist, species1_scientific %in% c("Larus canus")) # not in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Larus canus")) # in interactions as Common or Mew Gull
dplyr::filter(int.raw, species2_scientific %in% c("Larus canus")) # in interactions as Common or Mew Gull
dplyr::filter(splist, species1_scientific %in% c("Larus brachyrhynchus")) # in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Larus brachyrhynchus")) # in interactions as
dplyr::filter(int.raw, species2_scientific %in% c("Larus brachyrhynchus")) # in interactions as Short-Billed Gull
# L. brachyrhynchus interactions - these are entered as of Dec 6, 2023. 
# Confirmed that L. canus interactions are only Eurasian interactions.
# No change needed to these data.

## Spelling difference: BBS: Porphyrio martinicus = BOW: Porphyrio martinica
dplyr::filter(splist, species1_scientific %in% c("Porphyrio martinicus")) # in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Porphyrio martinicus")) # not in interactions 
dplyr::filter(int.raw, species2_scientific %in% c("Porphyrio martinicus")) # not in interactions 
dplyr::filter(splist, species1_scientific %in% c("Porphyrio martinica")) # not in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Porphyrio martinica")) # in interactions 
dplyr::filter(int.raw, species2_scientific %in% c("Porphyrio martinica")) # in interactions 
# RENAME: BBS & BOW: change Corvus caurinus to Corvus brachyrhynchos caurinus 
splist$species1_scientific[splist$species1_scientific == "Porphyrio martinicus"] <- "Porphyrio martinica"
int.raw$species1_scientific[int.raw$species1_scientific == "Porphyrio martinicus"] <- "Porphyrio martinica"
int.raw$species1_scientific[int.raw$species2_scientific == "Porphyrio martinicus"] <- "Porphyrio martinica"

## Update name: BBS: Cyanecula svecica = BOW: Luscinia svecica
dplyr::filter(splist, species1_scientific %in% c("Cyanecula svecica")) # in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Cyanecula svecica")) # not in interactions 
dplyr::filter(int.raw, species2_scientific %in% c("Cyanecula svecica")) # not in interactions 
dplyr::filter(splist, species1_scientific %in% c("Luscinia svecica")) # not in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Luscinia svecica")) # in interactions 
dplyr::filter(int.raw, species2_scientific %in% c("Luscinia svecica")) # in interactions 
# RENAME: BBS & BOW: change Cyanecula svecica to Luscinia svecica
splist$species1_scientific[splist$species1_scientific == "Cyanecula svecica"] <- "Luscinia svecica"
int.raw$species1_scientific[int.raw$species1_scientific == "Cyanecula svecica"] <- "Luscinia svecica"
int.raw$species1_scientific[int.raw$species2_scientific == "Cyanecula svecica"] <- "Luscinia svecica"

## Update name: BBS: Charadrius nivosus = BOW: Anarhynchus nivosus
dplyr::filter(splist, species1_scientific %in% c("Charadrius nivosus")) # in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Charadrius nivosus")) # in interactions 
dplyr::filter(int.raw, species2_scientific %in% c("Charadrius nivosus")) # in interactions 
dplyr::filter(splist, species1_scientific %in% c("Anarhynchus nivosus")) # not in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Anarhynchus nivosus")) # not in interactions 
dplyr::filter(int.raw, species2_scientific %in% c("Anarhynchus nivosus")) # in interactions 
# RENAME: BBS & BOW: change Charadrius nivosus to Anarhynchus nivosus
splist$species1_scientific[splist$species1_scientific == "Charadrius nivosus"] <- "Anarhynchus nivosus"
int.raw$species1_scientific[int.raw$species1_scientific == "Charadrius nivosus"] <- "Anarhynchus nivosus"
int.raw$species1_scientific[int.raw$species2_scientific == "Charadrius nivosus"] <- "Anarhynchus nivosus"

## Update name: BBS: Charadrius wilsonia = BOW: Anarhynchus wilsonia
dplyr::filter(splist, species1_scientific %in% c("Charadrius wilsonia")) # in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Charadrius wilsonia")) # in interactions 
dplyr::filter(int.raw, species2_scientific %in% c("Charadrius wilsonia")) # in interactions 
dplyr::filter(splist, species1_scientific %in% c("Anarhynchus wilsonia")) # not in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Anarhynchus wilsonia")) # not in interactions 
dplyr::filter(int.raw, species2_scientific %in% c("Anarhynchus wilsonia")) # in interactions 
# RENAME: BBS & BOW: change Charadrius wilsonia to Anarhynchus wilsonia
splist$species1_scientific[splist$species1_scientific == "Charadrius wilsonia"] <- "Anarhynchus wilsonia"
int.raw$species1_scientific[int.raw$species1_scientific == "Charadrius wilsonia"] <- "Anarhynchus wilsonia"
int.raw$species1_scientific[int.raw$species2_scientific == "Charadrius wilsonia"] <- "Anarhynchus wilsonia"

## Update name: BBS: Charadrius montanus = BOW: Anarhynchus montanus
dplyr::filter(splist, species1_scientific %in% c("Charadrius montanus")) # in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Charadrius montanus")) # in interactions 
dplyr::filter(int.raw, species2_scientific %in% c("Charadrius montanus")) # in interactions 
dplyr::filter(splist, species1_scientific %in% c("Anarhynchus montanus")) # not in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Anarhynchus montanus")) # not in interactions 
dplyr::filter(int.raw, species2_scientific %in% c("Anarhynchus montanus")) # not in interactions 
# RENAME: BBS & BOW: change Charadrius montanus to Anarhynchus montanus
splist$species1_scientific[splist$species1_scientific == "Charadrius montanus"] <- "Anarhynchus montanus"
int.raw$species1_scientific[int.raw$species1_scientific == "Charadrius montanus"] <- "Anarhynchus montanus"
int.raw$species1_scientific[int.raw$species2_scientific == "Charadrius montanus"] <- "Anarhynchus montanus"

## Cordilleran & Pacific Flycatcher are now Western Flycatcher as of 2023
## Update name: BBS: Empidonax occidentalis = BOW: Empidonax difficilis
dplyr::filter(splist, species1_scientific %in% c("Empidonax occidentalis")) # in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Empidonax occidentalis")) # in interactions 
dplyr::filter(int.raw, species2_scientific %in% c("Empidonax occidentalis")) # in interactions 
dplyr::filter(splist, species1_scientific %in% c("Empidonax difficilis")) # in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Empidonax difficilis")) # in interactions 
dplyr::filter(int.raw, species2_scientific %in% c("Empidonax difficilis")) # in interactions 
# RENAME: BBS & BOW: change Empidonax occidentalis to Empidonax difficilis
splist$species1_scientific[splist$species1_scientific == "Empidonax occidentalis"] <- "Empidonax difficilis"
int.raw$species1_scientific[int.raw$species1_scientific == "Empidonax occidentalis"] <- "Empidonax difficilis"
int.raw$species1_scientific[int.raw$species2_scientific == "Empidonax occidentalis"] <- "Empidonax difficilis"

##### END OF NAME CHANGES #####

# duplicate it for easier merging below
sp2list<-splist
# rename
names(sp2list)[names(sp2list) == "sp1_Seq"] <-"sp2_Seq"
names(sp2list)[names(sp2list) == "sp1_AOU"] <-"sp2_AOU"
names(sp2list)[names(sp2list) == "bbs_sp1_common"] <-"bbs_sp2_common"
names(sp2list)[names(sp2list) == "species1_scientific"] <-"species2_scientific"

# Merge into paired intxns by sp1 
intxns1<-merge(int.raw,splist,by=c("species1_scientific"),all.x=T, all.y=T)
dim(int.raw)
# 21387 rows
dim(intxns1)
# 21522 rows
length(unique(int.raw$species1_scientific))
# 919 species treated as species1 in original avian interaction data
length(unique(splist$species1_scientific))
# 760 species in entire BBS dataset (Grass and Sedge Wren are same spp?)
length(unique(intxns1$species1_scientific))
# 999 species in the merged data
length(unique(intxns1$species2_scientific))
# 2667 species as species2 but these *may* include the scientific names without a match in sp1
sum(is.na(intxns1$species2_scientific)) 
# 81 - species that exist in the BBS Species List but are not entered yet in original avian interaction data as species2
length(unique(int.raw$species2_scientific))
# 2667 species as species2 

# Repeat above but now for sp2 
# Merge into paired intxns by sp1
intxns2<-merge(int.raw,sp2list,by=c("species2_scientific"),all.x=T, all.y=T)
dim(int.raw)
# 21387 rows
dim(intxns2)
# 21480 rows
length(unique(int.raw$species2_scientific))
# 2667 species treated as species2 in original avian interaction data
length(unique(splist$species1_scientific))
# 760 species in entire BBS dataset
length(unique(intxns2$species2_scientific))
# 2746 species in the merged data 
sum(is.na(intxns2$species1_scientific)) 
# 79 NAs - species that exist in the BBS Species List but are not entered yet in original avian interaction data as species1
length(unique(intxns2$species1_scientific))
# 920 species as species1 but these *may* include the scientific names without a match in sp1
length(unique(int.raw$species1_scientific))
# 919 species as species1 but these *may* include the scientific names without a match in sp1

# Export to check species names: if the row has an AOU associated with species1, 
# it is in BBS; if those rows are without a complete entry, they are missing entries for those species
write.csv(intxns1, file.path(L1_dir,"intxns1_names.csv"), row.names=F) 
intxns1.11dec23<-merge(int.raw,splist,by=c("species1_scientific"),all.x=T, all.y=T)
# Subset out to just include the species1 in BBS without complete entries (i.e., missing species2)
intxns1.11dec23<-intxns1.11dec23[!is.na(intxns1.11dec23$sp1_AOU),] # only species with an AOU
intxns1.11dec23<-intxns1.11dec23[(is.na(intxns1.11dec23$species2_scientific) | intxns1.11dec23$species2_scientific==""),] 
write.csv(intxns1.11dec23, file.path(L1_dir,"BBS_species1_without_complete_entry11Dec2023.csv"), row.names=F) 

sort(intxns1.11dec23$species1_scientific)
# Some are "Xxx sp." and probably not useful / worth checking.

# Checking the missing BBS species entries in the original data:
dplyr::filter(int.raw, species1_scientific %in% c("Vidua macroura")) # in interactions
dplyr::filter(int.raw, species2_scientific %in% c("Vidua macroura")) # in interactions
dplyr::filter(int.raw, species2_scientific %in% c("Aechmophorus occidentalis / clarkii")) # in interactions
# Jaeger missing species2_scientific name- but exists in original data...
dplyr::filter(int.raw.orig, species1_scientific %in% c("Aechmophorus occidentalis / clarkii")) # in interactions
dplyr::filter(int.raw.orig, species2_scientific %in% c("Aechmophorus occidentalis / clarkii")) # in interactions

write.csv(intxns2, file.path(L1_dir,"intxns2_names.csv"), row.names=F) 

#intxns1 yields the following issues:
# Anas diazi
# Buteo jamaicensis harlani
# Calonectris diomedea
# Colaptes auratus cafer
# Corvus corax
# Dryocopus pileatus
# Ixoreus naevius
# Junco hyemalis aikeni
# Leptotila verreauxi
# Meleagris gallopavo
# Perisoreus canadensis
# Setophaga coronata audoboni

## Emily is checking these on Dec 12.


## END OF CHECKING ##

# Omit common names from int.raw ahead of merging with BBS Species List (we will replace them with the official BBS names)
# Note that this removes X rows where there is no species2_scientific in int.raw, but there are interactions recorded
# make a copy of int.raw
int.l1<-int.raw
int.l1$species1_common <-NULL
int.l1$species2_common <-NULL
dim(int.l1)

# Remove the NA rows - this is not updated for Nov 22, 2023
int.l1<-int.l1[!is.na(int.l1$effect_sp1_on_sp2), ]
dim(int.l1)
# 16807
# Assign common name columns with the scientific name
int.l1<-merge(int.l1,splist,by=c("species1_scientific"),all.x=T, all.y=T)
int.l1<-merge(int.l1,sp2list,by=c("species2_scientific"),all.x=T, all.y=T)
dim(int.l1)
# 17030
# Remove the NA rows (there shouldn't be any; they are all situations where common names occur but are not in the database)
dim(int.l1)
# 17030
int.l1<-int.l1[!is.na(int.l1$effect_sp1_on_sp2), ]
dim(int.l1)
#16807 - Seems like there are some afterall... *** need to check this EMILY
write.csv(int.l1,file.path(L1_dir,"int.l1.csv"), row.names=F)

# rename
#names(int.l1)[names(int.l1) == "bbs_sp2_common"] <-"sp2_common"
#names(int.l1)[names(int.l1) == "bbs_sp1_common"] <-"sp1_common"

## EXPORT the cleaned interaction pairs data:
## - this is not updated for Nov 22, 2023
write.csv(int.raw,file.path(L1_dir,"AvianInteractionData_L1.csv"), row.names=F)

#write.csv(int.l1,file.path(L1_dir,"AvianInteractionData_L1.csv"), row.names=F)

# keep just essential columns
intxns.raw<-subset(int.raw,select=c("species1_common",
                                       "species2_common",
                                       "species1_scientific",
                                       "species2_scientific",
                                       "effect_sp1_on_sp2",
                                       "effect_sp2_on_sp1",
                                       "interaction",
                                       "BOW_evidence",
                                       "n_studies",
                                       "recorder",
                                       "entry_date"))

# Save as intxns.raw; work on merging w species list in L1
write.csv(intxns.raw, file.path(L1_dir,"AvianInteractionData_L1.csv"), row.names=F) 

## Later work:
## EDIT Colaptes auratus subspecies
# Here we should assign the same interactions for the subspecies as occur for the main species
# Tricky- need to select all Colaptes auratus and replicate, but assigning the 3 subspecies.
# For now, drop the 3 subspecies (assign them Colaptes auratus in the bbs obs data: bbs_obs_L1.R) until we can refine this to represent their real potential interactions.



