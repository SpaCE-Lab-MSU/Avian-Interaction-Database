# TITLE:          Avian Interaction Pairs Data: L0 to L1, including an option to 
#                   subset species1 for only BBS species
# AUTHORS:        Phoebe Zarnetske, Pat Bills
# COLLABORATORS:  Vincent Miele, Stephane Dray, Emily Parker
# DATA INPUT:     From AvianInteractionData_L0_stitch.R: Data imported as csv 
#                   https://github.com/SpaCE-Lab-MSU/Avian-Interaction-Database/blob/main/L0/AvianInteractionData_L0.csv
#                 From bbs_specieslist_L1.R: Data imported as csv 
#                   https://github.com/SpaCE-Lab-MSU/Avian-Interaction-Database/blob/main/L0/bbs_splist_L0.csv
# DATA OUTPUT:    L1 data: AvianInteractionData_L1.csv
#                 L1 data: AvianInteractionData_L1_BBS.csv for BBS analysis
# PROJECT:        Avian Interaction Database 
# DATE:           27 Oct 2022; updated 20 Mar 2023, Dec. 21, 2023  
# NOTES:          Next script to run: 
#                 This script is used to refine species name changes to align with BOW, 
#                 and to create AvianInteractionData_L1.csv 
#                 L0 data are checked to assign BOW scientific and common names 
#                 to the interaction pairs data (which were originally from BBS species list). 
#               
# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)

# .Renviron not working for PLZ; hard-coding in here
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

#*******************************#
#### Scientific Name Changes ####
#*******************************#
# We are using the Birds of The World naming conventions for species. 
# Some BBS names differ. Some old names are included.
# Reference the bbsbow_names data to make initial changes to any 
# "other_or_old_bow" names that might appear.
# Apply changes only to the species1_scientific and species2_scientific columns.
# First omit any rows with a blank in "other_or_old_bow"
dim(namechg)
namechg.orig <- namechg
namechg<-namechg[!(is.na(namechg$other_or_old_bow) | namechg$other_or_old_bow==""), ]
dim(namechg)
# Save original copy of int.raw
int.raw.orig <- int.raw
dim(int.raw)

sort(unique(int.raw$species1_scientific))

# Remove extra beginning and end spaces:
int.raw$species1_scientific<-trimws(int.raw$species1_scientific, "r")
int.raw$species1_scientific<-trimws(int.raw$species1_scientific, "l")
int.raw$species2_scientific<-trimws(int.raw$species2_scientific, "r")
int.raw$species2_scientific<-trimws(int.raw$species2_scientific, "l")

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

sort(unique(int.raw$species1_scientific))

# Some interaction scientific names are still not matching Birds of the World 
# See: ./L0/bbsbow_names.csv
# Find them by starting with the original look-up table
#namechg.unmatch = na.omit(namechg.orig)
namechg.unmatch = subset(namechg.orig, namechg.orig$bbs2022 != namechg.orig$bow)
# remove the NAs in bbs2022 column
namechg.unmatch <- namechg.unmatch[-which(namechg.unmatch$bbs2022 == ""), ]
dim(namechg.unmatch) # 10 species on Dec 21, 2023
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

## SUBSPECIES Considerations ##
# Either lump them as species or keep as subspecies...

#For now we decide to keep as is; any subspecies will occur if they are
#explicitly called out as having an interaction. Otherwise, they don't occur in
#the interaction database. In the future we may decide to lump to species and
#remove all subspecies. Depends on how many are observed in BBS...

# Example: Colaptes auratus subspecies
# Colaptes auratus (AOU = 4123) 
# subspecies to the main species Colaptes auratus auratus (AOU = 4120) 
# Colaptes auratus auratus x auratus cafer (AOU = 4125) 
# Colaptes auratus cafer (AOU = 4130)

# int.raw$AOU[int.raw$AOU == 4120] <- 4123
# int.raw$AOU[int.raw$AOU == 4125] <- 4123
# int.raw$AOU[int.raw$AOU == 4130] <- 4123

#       Consider assigning Colaptes auratus interactions to all Colaptes auratus subspecies
#       This assumes that they all have the same ranges (they don't), so a refined set of interactions should be edited to
#       match the actual overlapping areas with interactors

## Remove blank species scientific names
# If there is no entry for species1_scientific or species2_scientific, omit row
dim(int.raw)
# 21837
int.raw<-int.raw %>% drop_na(species1_scientific)
int.raw <- int.raw %>% filter(!(species1_scientific==""))
dim(int.raw)
# 21820: 17 removed: Dec 21, 2023
int.raw <- int.raw %>% filter(!(species2_scientific==""))
dim(int.raw)
# 21820: 0 more removed: Dec 21, 2023

## End of Species' Scientific name changes ##

#*******************************#
#*#### Checking numbers of species & BBS List ####
#*******************************#
# duplicate it for species2 assessment
sp2list<-splist
# rename
names(sp2list)[names(sp2list) == "sp1_Seq"] <-"sp2_Seq"
names(sp2list)[names(sp2list) == "sp1_AOU"] <-"sp2_AOU"
names(sp2list)[names(sp2list) == "bbs_sp1_common"] <-"bbs_sp2_common"
names(sp2list)[names(sp2list) == "species1_scientific"] <-"species2_scientific"
names(sp2list)[names(sp2list) == "Genus"] <-"Genus2"
names(sp2list)[names(sp2list) == "Species"] <-"Species2"
sp2list$French_Common_Name<-NULL
sp2list$Spanish_Common_Name<-NULL
sp2list$ORDER<-NULL
sp2list$Family<-NULL

# Merge into paired intxns by sp1 (numbers below as of Dec 21, 2023)
intxns1<-merge(int.raw,splist,by=c("species1_scientific"),all.x=T, all.y=T)
dim(int.raw)
# 21820 rows
dim(intxns1)
# 21949 rows
length(unique(int.raw$species1_scientific))
# 928 species treated as species1 in original avian interaction data
length(unique(splist$species1_scientific))
# 760 species in entire BBS dataset (Grass and Sedge Wren are same spp?)
length(unique(intxns1$species1_scientific))
# 1002 species in the merged data
length(unique(intxns1$species2_scientific))
# 2884 species as species2 but these *may* include the scientific names without a match in sp1
sum(is.na(intxns1$species2_scientific)) 
# 74 - species that exist in the BBS Species List but are not entered yet in 
# original avian interaction data as species2 - these are subspecies and unidentified
length(unique(int.raw$species2_scientific))
# 2883 species as species2 

# Repeat above but now for sp2 
# Merge into paired intxns by sp1
intxns2<-merge(int.raw,sp2list,by=c("species2_scientific"),all.x=T, all.y=T)
dim(int.raw)
# 21820 rows
dim(intxns2)
# 21913 rows
length(unique(int.raw$species2_scientific))
# 2883 species treated as species2 in original avian interaction data
length(unique(splist$species1_scientific))
# 760 species in entire BBS dataset
length(unique(intxns2$species2_scientific))
# 2961 species in the merged data 
sum(is.na(intxns2$species1_scientific)) 
# 78 NAs - species that exist in the BBS Species List but are not entered yet in original avian interaction data as species1
length(unique(intxns2$species1_scientific))
# 929 species as species1 but these *may* include the scientific names without a match in sp1
length(unique(int.raw$species1_scientific))
# 928 species as species1 but these *may* include the scientific names without a match in sp1

# Export to check species names: if the row has an AOU associated with species1,
# it is in BBS; if those rows are without a complete entry, they are missing
# entries for those species There are 74 here as of Dec. 21, 2023. All are
# either rare subspecies (without a BOW acct), or they are species which the
# observer could not distinguish, or they are just the Genus level.
# Subset out to just include the species1 in BBS without complete entries (i.e., missing species2)
intxns1a<-intxns1[!is.na(intxns1$sp1_AOU),] # only species with an AOU
intxns1a<-intxns1a[(is.na(intxns1a$species2_scientific) | intxns1a$species2_scientific==""),] 
sort(intxns1a$species1_scientific)
length(intxns1a$species1_scientific)

# Subset out to just include the species2 in BBS without complete entries (i.e., missing species1)
intxns2a<-intxns2[!is.na(intxns2$sp2_AOU),] # only species with an AOU
intxns2a<-intxns2a[(is.na(intxns2a$species1_scientific) | intxns2a$species1_scientific==""),] 
sort(intxns2a$species2_scientific)
length(intxns2a$species2_scientific)
# The species2 above just have occurrence as species1. That's ok.

#*******************************#
#### Fixing Species' Common Names ####
#*******************************#
# This is done for BBS look-up; need to replace with global names later, e.g., BirdNet 
# Create intxns12 which merges splist and species1_scientific, then sp2list and 
# species2_scientific, by only keeps interaction data.
intxns12<-merge(int.raw,splist,by=c("species1_scientific"),all.x=T)
dim(int.raw)
# 21820
dim(intxns12)
# 21875
#write.csv(intxns12, file.path(L1_dir, "intxns12.csv"), row.names=F) 

intxns12<-merge(intxns12,sp2list,by=c("species2_scientific"),all.x=T)
dim(intxns12)
# 21892

# Create an extra species1 column to test mutate & re-assignment below
intxns12$species1_common_orig<-intxns12$species1_common

# If a species has a AOU, assign the common name based on the BBS splist
intxns12 <- intxns12 %>% 
  mutate(species1_common = ifelse(!is.na(sp1_AOU), bbs_sp1_common, species1_common))
dim(intxns12)
# 21892
#write.csv(intxns12, file.path(L1_dir,"intxns12.csv"), row.names=F) 

intxns12$species2_common_orig<-intxns12$species2_common
# If a species has a AOU, assign the common name based on the BBS splist
intxns12 <- intxns12 %>% 
  mutate(species2_common = ifelse(!is.na(sp2_AOU), bbs_sp2_common, species2_common))
#write.csv(intxns12, file.path(L1_dir,"intxns12.csv"), row.names=F) 
dim(intxns12)
# 21892

# These all worked for replacements. Now remove extra columns
intxns12$species1_common_orig<-NULL
intxns12$species2_common_orig<-NULL
intxns12$bbs_sp1_common<-NULL
intxns12$bbs_sp2_common<-NULL
intxns12$French_Common_Name<-NULL
intxns12$Spanish_Common_Name<-NULL
intxns12$Spanish_Common_Name<-NULL
intxns12$ORDER<-NULL
intxns12$Family<-NULL
intxns12$Genus<-NULL
intxns12$Genus2<-NULL
intxns12$Species<-NULL
intxns12$Species2<-NULL
intxns12$sp1_Seq<-NULL
intxns12$sp2_Seq<-NULL

#*******************************#
#### Editing the interaction columns to standardize names ####
#*******************************#
# Unique interaction types:
sort(unique(intxns12$interaction))
# Remove extra end spaces:
intxns12$interaction<-trimws(intxns12$interaction, "r")
# Make all lowercase
intxns12$interaction<-tolower(intxns12$interaction)
sort(unique(intxns12$interaction))

# Some misspellings/typos:
intxns12$interaction[intxns12$interaction=="amenslism"] <- "amensalism"
intxns12$interaction[intxns12$interaction=="brood"] <- "brood parasitism"
intxns12$interaction[intxns12$interaction=="brood-parasitism"] <- "brood parasitism"
intxns12$interaction[intxns12$interaction=="call mimicking"] <- "call mimicry"
intxns12$interaction[intxns12$interaction=="call mimickry"] <- "call mimicry"
intxns12$interaction[intxns12$interaction=="comensalism"] <- "commensalism"
intxns12$interaction[intxns12$interaction=="commenalism"] <- "commensalism"
intxns12$interaction[intxns12$interaction=="commesalism"] <- "commensalism"
intxns12$interaction[intxns12$interaction=="commensalism -call mimicry"] <- "commensalism-call mimicry"
intxns12$interaction[intxns12$interaction=="commenslism - call mimicry"] <- "commensalism-call mimicry"
intxns12$interaction[intxns12$interaction=="commensalism-chick adoptio"] <- "commensalism-chick adoption"
intxns12$interaction[intxns12$interaction=="comeptition"] <- "competition"
intxns12$interaction[intxns12$interaction=="competiton"] <- "competition"
intxns12$interaction[intxns12$interaction=="competition - nest site"] <- "competition-nest site"
intxns12$interaction[intxns12$interaction=="courting"] <- "courtship"
intxns12$interaction[intxns12$interaction=="faciliation - comigration"] <- "facilitation-comigration"
intxns12$interaction[intxns12$interaction=="faciliation - comigration"] <- "facilitation-comigration"
intxns12$interaction[intxns12$interaction=="facilitation-comigrate"] <- "facilitation-comigration"
intxns12$interaction[intxns12$interaction=="facilitation-comigratio"] <- "facilitation-comigration"
intxns12$interaction[intxns12$interaction=="facilitaion-comigration"] <- "facilitation-comigration"
intxns12$interaction[intxns12$interaction=="faciltiation-mixed flocking"] <- "facilitation-mixed flocking"
intxns12$interaction[intxns12$interaction=="facillitation"] <- "facilitation"
intxns12$interaction[intxns12$interaction=="faciliation - mixed flocking"] <- "facilitation-mixed flocking"
intxns12$interaction[intxns12$interaction=="faciltation-mixed flocking"] <- "facilitation-mixed flocking"
intxns12$interaction[intxns12$interaction=="faciliation - mixed flocking"] <- "facilitation-mixed flocking"
intxns12$interaction[intxns12$interaction=="facilitation-mixed flock"] <- "facilitation-mixed flocking"
intxns12$interaction[intxns12$interaction=="faciltation-feeding"] <- "facilitation-feeding"
intxns12$interaction[intxns12$interaction=="faciltiation-feeding"] <- "facilitation-feeding"
intxns12$interaction[intxns12$interaction=="faciltiation"] <- "facilitation"
intxns12$interaction[intxns12$interaction=="hybrization"] <- "hybridization"
intxns12$interaction[intxns12$interaction=="kleptoparasitsim"] <- "kleptoparasitism"
intxns12$interaction[intxns12$interaction=="kleptoparasitsm"] <- "kleptoparasitism"
intxns12$interaction[intxns12$interaction=="kleptoparisitism"] <- "kleptoparasitism"
intxns12$interaction[intxns12$interaction=="kleptoparasitism of nest material"] <- "kleptoparasitism-nest material"
# Checked and all of these are brood parasitism as of Dec 21, 2023
intxns12$interaction[intxns12$interaction=="parasitism"] <- "brood parasitism"
intxns12$interaction[intxns12$interaction=="predation-scavenger"] <- "predation-scavenging"

sort(unique(intxns12$interaction))

# Ignore these interactions for now: 
# "combined species"
# "copulation?" - for 2 swallows

# Check the codings for interaction types
# 0 if "hybridization"
int.entries<-intxns12 %>% distinct(interaction, effect_sp1_on_sp2, effect_sp2_on_sp1)
arrange(int.entries, by=interaction)

# If a row is "hybridization" make it 0,0
intxns12$effect_sp1_on_sp2[intxns12$interaction == "hybridization"] <- 0
intxns12$effect_sp2_on_sp1[intxns12$interaction == "hybridization"] <- 0

# If a row is "co-occur" make it 0,0
intxns12$effect_sp1_on_sp2[intxns12$interaction == "co-occur"] <- 0
intxns12$effect_sp2_on_sp1[intxns12$interaction == "co-occur"] <- 0

# If a row is "play" make it 0,0
intxns12$effect_sp1_on_sp2[intxns12$interaction == "play"] <- 0
intxns12$effect_sp2_on_sp1[intxns12$interaction == "play"] <- 0

# If a row is "courtship" make it 0,0
intxns12$effect_sp1_on_sp2[intxns12$interaction == "courtship"] <- 0
intxns12$effect_sp2_on_sp1[intxns12$interaction == "courtship"] <- 0

# If a row is "copulation" or "copulation?" or "breeding" make it 0,0
intxns12$effect_sp1_on_sp2[intxns12$interaction == "copulation"] <- 0
intxns12$effect_sp2_on_sp1[intxns12$interaction == "copulation"] <- 0
intxns12$effect_sp1_on_sp2[intxns12$interaction == "copulation?"] <- 0
intxns12$effect_sp2_on_sp1[intxns12$interaction == "copulation?"] <- 0
intxns12$effect_sp1_on_sp2[intxns12$interaction == "breeding"] <- 0
intxns12$effect_sp2_on_sp1[intxns12$interaction == "breeding"] <- 0

int.entries<-intxns12 %>% distinct(interaction, effect_sp1_on_sp2, effect_sp2_on_sp1)
arrange(int.entries, by=interaction)

#write.csv(intxns12, file.path(L1_dir,"intxns_types_check.csv"), row.names=F) 
# Check if brood parasitism is coded correctly for Brown Headed Cowbird. Did
# this by filtering out these species in exported csv... for next iteration, do
# this in code. As of Dec. 18, 2023, all are correct. There are a few funny ones
# but they are checked and ok:

#Brown thrasher	Brown-headed Cowbird	Toxostoma rufum	Molothrus ater	-1	1	nest takeover
#Verdin	Brown-headed Cowbird	Auriparus flaviceps	Molothrus ater	-1	1	brood parasitism
#Verdin	Bronzed Cowbird	Auriparus flaviceps	Molothrus aeneus	-1	1	brood parasitism

# Remove the blank entries for interaction type if they exist 
dim(intxns12)
intxns12 <- intxns12 %>% filter(!(interaction==""))
dim(intxns12)
# no blanks exist

#*******************************#
#### Clean up other columns ####
#*******************************#
## nonbreeding season
sort(unique(intxns12$nonbreedingseason))
intxns12$nonbreedingseason[intxns12$nonbreedingseason == "Yes"] <- "yes"
intxns12$nonbreedingseason[intxns12$nonbreedingseason == "YES"] <- "yes"
intxns12$nonbreedingseason[intxns12$nonbreedingseason == "YSE"] <- "yes"
intxns12$nonbreedingseason[intxns12$nonbreedingseason == "yes "] <- "yes"
intxns12$nonbreedingseason[intxns12$nonbreedingseason == "potential"] <- "possibly"
# nonbreeding season: if there is a blank or NA, make it "no"
intxns12$nonbreedingseason[intxns12$nonbreedingseason==""] <- "no"
intxns12$nonbreedingseason[is.na(intxns12$nonbreedingseason)] <- "no"
sort(unique(intxns12$nonbreedingseason))

## BOW_evidence
# Remove extra end spaces:
intxns12$BOW_evidence<-trimws(intxns12$BOW_evidence, "r")
# Make all lowercase
intxns12$BOW_evidence<-tolower(intxns12$BOW_evidence)

intxns12$BOW_evidence[intxns12$BOW_evidence == "1ref"] <- "1 ref"
intxns12$BOW_evidence[intxns12$BOW_evidence == "one ref"] <- "1 ref"
intxns12$BOW_evidence[intxns12$BOW_evidence == "possible"] <- "potential"
intxns12$BOW_evidence[intxns12$BOW_evidence == "stong"] <- "strong"
intxns12$BOW_evidence[intxns12$BOW_evidence == "strong very strong!"] <- "strong"
intxns12$BOW_evidence[intxns12$BOW_evidence == "weak (circumstantial evidence)"] <- "weak"
intxns12$BOW_evidence[intxns12$BOW_evidence == "circumstantial"] <- "weak"
intxns12$BOW_evidence[intxns12$BOW_evidence == "weal"] <- "weak"
sort(unique(intxns12$BOW_evidence))

## n_studies
sort(unique(intxns12$n_studies))

## uncertain_interaction - lots here; @Emily we need to work through these to check them.
sort(unique(intxns12$uncertain_interaction))
# 579 entries with some kind of note

## EXPORT the cleaned interaction pairs data:
# Order the data by species1_scientific
intxns12 <- intxns12 %>% relocate(species2_scientific, .after = species1_common)
## Ran for Dec 21, 2023 - but, need to update common names based on BirdNet in future
write.csv(intxns12,file.path(L1_dir,"AvianInteractionData_L1.csv"), row.names=F)

## prep for BBS work: 
## Remove non-breeding season interactions from the BBS subset of data because
# BBS observations are only during breeding season.
int.bbs<-intxns12
table(int.bbs$nonbreedingseason)

# Remove the "yes" for nonbreedingseason 
int.bbs <- int.bbs %>% filter(nonbreedingseason!="yes")
dim(intxns12)-dim(int.bbs)
# 2449 cases where interaction is "yes" for nonbreeding season

# Decide whether to drop the entries with uncertain interactions; these need to be updated & checked.

# keep just essential columns
int.bbs<-subset(int.bbs,select=c("species1_scientific",
                                       "species1_common",
                                       "species2_scientific",
                                       "species2_common",
                                       "effect_sp1_on_sp2",
                                       "effect_sp2_on_sp1",
                                       "interaction",
                                       "BOW_evidence",
                                       "n_studies",
                                       "nonbreedingseason",
                                       "recorder",
                                       "entry_date",
                                       "uncertain_interaction"))

write.csv(int.bbs,file.path(L1_dir,"AvianInteractionData_BBS_L1.csv"), row.names=F)


