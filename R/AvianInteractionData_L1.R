# TITLE:          Avian Interaction Pairs Data: L0 to L1, including an option to 
#                   subset species1 for only BBS species
# AUTHORS:        Phoebe Zarnetske, Pat Bills
# COLLABORATORS:  Vincent Miele, Stephane Dray, Emily Parker
# DATA INPUT:     From AvianInteractionData_L0_stitch.R: Data imported as csv 
#                   https://github.com/SpaCE-Lab-MSU/Avian-Interaction-Database/blob/main/L0/AvianInteractionData_L0.csv
#                 From bbs_specieslist_L1.R: Data imported as csv 
#                   https://github.com/SpaCE-Lab-MSU/Avian-Interaction-Database/blob/main/L1/bbs_splist_2022_L1.csv
#                 For BBS subset: bbs_allobs_runtype1 produced in 
#                   https://github.com/SpaCE-Lab-MSU/avian-meta-network/blob/main/R/L1/bbs_obs_L1.R: 
#                   Data imported as csv: /Google Drive/Shared drives/Avian_MetaNetwork/data/L1/bbs_obs/bbs_allobs_runtype1.csv
#
# DATA OUTPUT:    L1 data: AvianInteractionData_L1.csv
#                 L1 data: AvianInteractionData_L1_BBS.csv for BBS analysis
#                 L1 data: bbs_splist_2022_L1.csv for BBS analysis
# PROJECT:        Avian Interaction Database 
# DATE:           27 Oct 2022; updated 20 Mar 2023, Dec. 22, 2023  
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

# Read in species list: all species in BBS (the 2023 release which includes all species as of 2022, plus the additional AOUcombo.index column for use w BBS index)
splist<-read.csv(file.path(L0_dir,"bbs_splist_2022_L1.csv"))

# Read in our look-up table with the different bbs & bow & old names for species
namechg<-read.csv(file.path(L0_dir,"bbsbow_names.csv"))

# Rename some columns and omit others; indicate that the common name is coming from BBS Species List
names(splist)[names(splist) == "English_Common_Name"] <-"bbs_sp1_common"
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
# Standardize "sp."
int.raw$species1_scientific<-gsub(" spp."," sp.",int.raw$species1_scientific)
int.raw$species2_scientific<-gsub(" spp."," sp.",int.raw$species2_scientific)

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
int.raw[int.raw=="Accipiter cooperi"] <- "Accipiter cooperii"
sort(unique(int.raw$species1_scientific))

sort(unique(int.raw$species2_scientific))
int.raw[int.raw=="Accipiter getilis"] <- "Accipiter gentilis"
int.raw[int.raw=="Accipitiridae sp."] <- "Accipitridae sp."
int.raw[int.raw=="Accipitrid sp."] <- "Accipitridae sp."

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

## American Crow Subspecies = Northwestern Crow: Corvus brachyrhynchos caurinus (instead of Corvus caurinus "Northwestern Crow"); but Audubon absorbs it into American Crow. Rename to American Crow.
dplyr::filter(splist, species1_scientific %in% c("Corvus brachyrhynchos")) # in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Corvus brachyrhynchos")) # in interactions 
dplyr::filter(splist, species1_scientific %in% c("Corvus brachyrhynchos caurinus")) # not in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Corvus brachyrhynchos caurinus")) # in interactions 
dplyr::filter(splist, species1_scientific %in% c("Corvus caurinus")) # not in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Corvus caurinus")) # not in interactions 
dplyr::filter(int.raw, species2_scientific %in% c("Corvus caurinus")) # not in interactions 
# RENAME: BBS & BOW: change Corvus caurinus and Corvus brachyrhynchos caurinus to Corvus brachyrhynchos
splist$species1_scientific[splist$species1_scientific == "Corvus caurinus"] <- "Corvus brachyrhynchos"
int.raw$species1_scientific[int.raw$species1_scientific == "Corvus caurinus"] <- "Corvus brachyrhynchos"
int.raw$species2_scientific[int.raw$species2_scientific == "Corvus caurinus"] <- "Corvus brachyrhynchos"
splist$species1_scientific[splist$species1_scientific == "Corvus brachyrhynchos caurinus"] <- "Corvus brachyrhynchos"
int.raw$species1_scientific[int.raw$species1_scientific == "Corvus brachyrhynchos caurinus"] <- "Corvus brachyrhynchos"
int.raw$species2_scientific[int.raw$species2_scientific == "Corvus brachyrhynchos caurinus"] <- "Corvus brachyrhynchos"

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
int.raw$species2_scientific[int.raw$species2_scientific == "Porphyrio martinicus"] <- "Porphyrio martinica"

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
int.raw$species2_scientific[int.raw$species2_scientific == "Cyanecula svecica"] <- "Luscinia svecica"

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
int.raw$species2_scientific[int.raw$species2_scientific == "Charadrius nivosus"] <- "Anarhynchus nivosus"

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
int.raw$species2_scientific[int.raw$species2_scientific == "Charadrius wilsonia"] <- "Anarhynchus wilsonia"

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
int.raw$species2_scientific[int.raw$species2_scientific == "Charadrius montanus"] <- "Anarhynchus montanus"

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
int.raw$species2_scientific[int.raw$species2_scientific == "Empidonax occidentalis"] <- "Empidonax difficilis"

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

# Update genus_species column with name changes
splist$genus_species<-splist$species1_scientific

## End of Species' Scientific name changes ##

#*******************************#
#*#### Checking numbers of species & BBS List ####
#*******************************#
# duplicate BBS species list with the changes to names for species2 assessment
sp2list<-splist
# rename
names(sp2list)[names(sp2list) == "sp1_Seq"] <-"sp2_Seq"
names(sp2list)[names(sp2list) == "sp1_AOU"] <-"sp2_AOU"
names(sp2list)[names(sp2list) == "bbs_sp1_common"] <-"bbs_sp2_common"
names(sp2list)[names(sp2list) == "species1_scientific"] <-"species2_scientific"
names(sp2list)[names(sp2list) == "Genus"] <-"Genus2"
names(sp2list)[names(sp2list) == "Species"] <-"Species2"
names(sp2list)[names(sp2list) == "genus_species"] <-"genus_species2"
sp2list$French_Common_Name<-NULL
sp2list$Spanish_Common_Name<-NULL
sp2list$ORDER<-NULL
sp2list$Family<-NULL

# Merge into paired intxns by sp1 (numbers below as of Dec 21, 2023)
intxns1<-merge(int.raw,splist,by=c("species1_scientific"),all.x=T, all.y=T)
dim(int.raw)
# 21820 rows
dim(intxns1)
# 21939 rows
length(unique(int.raw$species1_scientific))
# 927 species treated as species1 in original avian interaction data
length(unique(splist$species1_scientific))
# 759 species in entire BBS dataset (Grass and Sedge Wren are same spp?)
length(unique(intxns1$species1_scientific))
# 1001 species in the merged data
length(unique(intxns1$species2_scientific))
# 2879 species as species2 but these *may* include the scientific names without a match in sp1
sum(is.na(intxns1$species2_scientific)) 
# 74 - species that exist in the BBS Species List but are not entered yet in 
# original avian interaction data as species2 - these are subspecies and unidentified
length(unique(int.raw$species2_scientific))
# 2878 species as species2 

# Repeat above but now for sp2 
# Merge into paired intxns by sp1
intxns2<-merge(int.raw,sp2list,by=c("species2_scientific"),all.x=T, all.y=T)
dim(int.raw)
# 21820 rows
dim(intxns2)
# 21924 rows
length(unique(int.raw$species2_scientific))
# 2878 species treated as species2 in original avian interaction data
length(unique(splist$species1_scientific))
# 759 species in entire BBS dataset
length(unique(intxns2$species2_scientific))
# 2955 species in the merged data 
sum(is.na(intxns2$species1_scientific)) 
# 77 NAs - species that exist in the BBS Species List but are not entered yet in original avian interaction data as species1
length(unique(intxns2$species1_scientific))
# 928 species as species1 but these *may* include the scientific names without a match in sp1
length(unique(int.raw$species1_scientific))
# 927 species as species1 but these *may* include the scientific names without a match in sp1

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
# 21865
#write.csv(intxns12, file.path(L1_dir, "intxns12.csv"), row.names=F) 
#write.csv(intxns12,file.path(L1_dir,"test_intxns12.csv"), row.names=F)

intxns12<-merge(intxns12,sp2list,by=c("species2_scientific"),all.x=T)
dim(intxns12)
# 21896

# Create an extra species1 column to test mutate & re-assignment below
intxns12$species1_common_orig<-intxns12$species1_common

# If a species has a AOU, assign the common name based on the BBS splist
intxns12 <- intxns12 %>% 
  mutate(species1_common = ifelse(!is.na(sp1_AOU), bbs_sp1_common, species1_common))
dim(intxns12)
# 21896
#write.csv(intxns12, file.path(L1_dir,"intxns12.csv"), row.names=F) 

intxns12$species2_common_orig<-intxns12$species2_common
# If a species has a AOU, assign the common name based on the BBS splist
intxns12 <- intxns12 %>% 
  mutate(species2_common = ifelse(!is.na(sp2_AOU), bbs_sp2_common, species2_common))
#write.csv(intxns12, file.path(L1_dir,"intxns12.csv"), row.names=F) 
dim(intxns12)
# 21896

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
intxns12$genus_species<-NULL
intxns12$genus_species2<-NULL

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
## Ran for Jan 7, 2024 - but, need to update common names based on BirdNet in future
write.csv(intxns12,file.path(L1_dir,"AvianInteractionData_L1.csv"), row.names=F)

#*******************************#
## BBS work: 
#*******************************#
int.bbs<-intxns12
bbs.splist<-splist
# Sedge Wren & Grass Wren - rename to match BBS 2022 list (not BOW) for easier merging later
# Sedge Wren (Cistothorus	stellaris) and Grass Wren (Cistothorus	platensis) are 2 different species,
# Previously Sedge Wren (Cistothorus	platensis).
dplyr::filter(int.bbs, species1_scientific %in% c("Cistothorus stellaris")) # in interactions 
dplyr::filter(int.bbs, species2_scientific %in% c("Cistothorus stellaris")) # in interactions 
dplyr::filter(bbs.splist, species1_scientific %in% c("Cistothorus stellaris")) # in BBS list 
dplyr::filter(int.bbs, species1_scientific %in% c("Cistothorus platensis")) # in interactions 
dplyr::filter(int.bbs, species2_scientific %in% c("Cistothorus platensis")) # in interactions 
dplyr::filter(bbs.splist, species1_scientific %in% c("Cistothorus	platensis")) # not in splist 
# RENAME: BBS & BOW: change Cistothorus platensis to Cistothorus stellaris
#splist$species1_scientific[splist$species1_scientific == "Cistothorus platensis"] <- "Cistothorus stellaris"
int.bbs$species1_scientific[int.bbs$species1_scientific == "Cistothorus platensis"] <- "Cistothorus stellaris"
int.bbs$species2_scientific[int.bbs$species2_scientific == "Cistothorus platensis"] <- "Cistothorus stellaris"
# Update the AOU for this species (assign 7240 which is the Sedge Wren)
# Make a selection for the species
cisste1<-int.bbs$species1_scientific == "Cistothorus stellaris"
cisste2<-int.bbs$species2_scientific == "Cistothorus stellaris"

# Assign AOU to species level:
int.bbs$sp1_AOU[cisste1] <- 7240
int.bbs$sp2_AOU[cisste2] <- 7240

## Remove non-breeding season interactions from the BBS subset of data because
# BBS observations are only during breeding season.
table(int.bbs$nonbreedingseason)

# Remove the "yes" for nonbreedingseason 
int.bbs <- int.bbs %>% filter(nonbreedingseason!="yes")
dim(intxns12)-dim(int.bbs)
# 2449 cases where interaction is "yes" for nonbreeding season

# Decide whether to drop the entries with uncertain interactions; these need to be updated & checked.

# Assign subspecies to main species (just in scientific name)
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
                                       "uncertain_interaction",
                                       "sp1_AOU",
                                       "sp2_AOU"))

#*******************************#
# Omit the non-BBS rows (rows that do not contain one of the BBS species)
#*******************************#

# Remove rows that do not have an AOU associated with them 
# # make a temporary version int.bbs1 to check
int.bbs1<-int.bbs %>% 
  filter(!is.na(sp1_AOU) | !is.na(sp2_AOU))
dim(int.bbs)-dim(int.bbs1)
# 1165 rows that do not contain a BBS species
int.bbs.noAOU<-int.bbs %>% 
  filter(is.na(sp1_AOU) & is.na(sp2_AOU))

sort(unique(int.bbs.noAOU$species1_scientific))
# 155 species without an AOU (not in BBS) as of Jan 3, 2024

#write.csv(int.bbs.noAOU,file.path(L1_dir,"int.bbs.noAOU.csv"), row.names=F)
# According to BBS species list 2022, there are a few subspecies. Keep these?
# Anser caerulescens (blue form)
# # not in interactions
# Ardea herodias occidentalis 
# # also in interactions as sp1 (3 times as communal nesting) & sp2 (4 times as communal nesting)
# Branta bernicla nigricans 
# # also in interactions as sp2 (7 times as various interactions)
# Buteo jamaicensis harlani 
# # also in interactions as sp1 (once as hybridization) & sp2 twice: as mobbing and predation
# Colaptes auratus auratus 
# # also in interactions with other Colaptes auratus as sp1; twice as hybridization
# Colaptes auratus cafer
# # also in interactions with other Colaptes auratus as sp1: twice as hybridization; as sp2 3x (competition, nest takeover, hybrid)
# Junco hyemalis aikeni 
# # also in interactions as with other Junco spp. sp2 - only once as hybridization
# Junco hyemalis caniceps 
# # also in interactions with other Junco spp. as sp1 - 7 times include 3x as hybridization; 5 times as sp2 including twice for hybridization
# Junco hyemalis hyemalis 
# # also in interactions with other Junco spp. as sp1 & sp2 - once each as hybridization
# Junco hyemalis mearnsi 
# # also in interactions with other Junco spp. as sp1 - 6 times as hybridizations; as sp2 once as hybridation
# Junco hyemalis oreganus 
# # also in interactions with other Junco spp. as sp1; as sp2 4x as competition w non-Junco
# Setophaga coronata audoboni 
# # not in interactions
# Setophaga coronata coronata 
# # also in interactions once as sp1 facilitation; 4 times as sp2 as hybrid (1x), facilitation (2x), competition (1x)

# For subspecies in BBS (=species with AOU), there are not many instances; deciding to assign them all to species level.

# # species with slashes, " or ", " x " or " X " or " sp." "unid", etc.
# # are either unknown or hybrids.
pattern = "/|( or )|( X )|( x )|( sp\\.)|(unid)|hybrid|Admin Code"

bad.latin = grep(
  pattern,
  bbs.splist$species1_scientific
)
# Something is a potential subspecies if it has three words separated by
# spaces.
possible.subspecies = grep("^.* .* .*$", bbs.splist$species1_scientific)
subspecies.ID = possible.subspecies[
  is.na(match(possible.subspecies, bad.latin))
]
subspecies = bbs.splist$species1_scientific[subspecies.ID]

# Assign the genus species to replace the subspecies; in bbs.splist just drop subspecies.
is.subspecies = which(bbs.splist$species1_scientific %in% subspecies)

bbs.splist1<-bbs.splist[-is.subspecies, ]
dim(bbs.splist1)-dim(bbs.splist)
# -13; We are going to keep some subspecies and edit interactions accordingly.

# Consider making the code below more elegant but for now we are just manually
# re-assigning scientific, common, AOU, based on what we find. 
# We are not re-assigning the AOUs in the observations.
#is.subspecies.intsp1 = which(int.bbs$species1_scientific %in% subspecies)
#is.subspecies.intsp2 = which(int.bbs$species2_scientific %in% subspecies)

# Print the 13 subspecies
subspecies

# Check how many times these species vs. subspecies are observed in BBS:
bbs_allobs_rpid101 <- read_csv("~/Google Drive/Shared drives/Avian_MetaNetwork/data/L1/bbs_obs/bbs_rpid101obs.csv")

# Make a copy of sp1 and sp2 columns for checking
int.bbs$sp1_orig<-int.bbs$species1_scientific
int.bbs$sp2_orig<-int.bbs$species2_scientific

# Make a column to track the change
int.bbs$sp1_subspecies_status<-""
int.bbs$sp2_subspecies_status<-""

#### SNOW GOOSE Anser caerulescens and its subspecies in interactions & BBS obs... ####
# View the AOU for the genus species
subset(bbs.splist, genus_species=="Anser caerulescens")
subset(bbs.splist, genus_species=="Anser caerulescens (blue form)")
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(1690)) # Anser caerulescens 35 times 
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(1691)) # Anser caerulescens (blue form) 0 times
dplyr::filter(int.bbs, species1_scientific %in% c("Anser caerulescens")) # Anser caerulescens 16 times 
dplyr::filter(int.bbs, species2_scientific %in% c("Anser caerulescens")) # Anser caerulescens 19 times 
dplyr::filter(int.bbs, species1_scientific %in% c("Anser caerulescens (blue form)")) # Anser caerulescens (blue form) 0 times
dplyr::filter(int.bbs, species2_scientific %in% c("Anser caerulescens (blue form)")) # Anser caerulescens (blue form) 0 times 

# No change needed bc the Snow Goose subspecies doesn't exist in BBS obs or in the breeding season interactions.

#### BRANT GOOSE Branta bernicla and its subspecies in interactions & BBS obs... ####
# View the AOU for the genus species
subset(bbs.splist, genus_species=="Branta bernicla")
subset(bbs.splist, genus_species=="Branta bernicla nigricans")
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(1730)) # Branta bernicla 0 times 
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(1740)) # Branta bernicla nigricans 16 times
dplyr::filter(int.bbs, species1_scientific %in% c("Branta bernicla")) # species 32 times 
dplyr::filter(int.bbs, species2_scientific %in% c("Branta bernicla")) # species 16 times 
dplyr::filter(int.bbs, species1_scientific %in% c("Branta bernicla nigricans")) # subspecies 0 times
dplyr::filter(int.bbs, species2_scientific %in% c("Branta bernicla nigricans")) # subspecies 7 times 

# There are no BBS obs of the species (1730), only subspecies; assign all the
# breeding season interactions to the subspecies (1740). Most interactions are at
# the species level. Make a selection for the species
braber1<-int.bbs$species1_scientific == "Branta bernicla"
braber2<-int.bbs$species2_scientific == "Branta bernicla"

# Assign AOU to species level:
int.bbs$sp1_AOU[braber1] <- 1740
int.bbs$sp1_subspecies_status[braber1]<-"sp1 and AOU originally species Branta bernicla; changed to subspecies bc only BBS obs of Branta bernicla nigricans; see sp1_orig"
int.bbs$sp2_AOU[braber2] <- 1740
int.bbs$sp2_subspecies_status[braber2]<-"sp2 and AOU originally species Branta bernicla; changed to subspecies bc only BBS obs of Branta bernicla nigricans; see sp2_orig"
# Assign species to subspecies
int.bbs$species1_scientific[braber1] <- "Branta bernicla nigricans"
int.bbs$species2_scientific[braber2] <- "Branta bernicla nigricans"
int.bbs$species1_common[braber1] <- "(Black Brant) Brant"
int.bbs$species2_common[braber2] <- "(Black Brant) Brant"

dplyr::filter(int.bbs, species1_scientific %in% c("Branta bernicla")) # species 0 times 
dplyr::filter(int.bbs, species2_scientific %in% c("Branta bernicla")) # species 0 times 
dplyr::filter(int.bbs, species1_scientific %in% c("Branta bernicla nigricans")) # subspecies 32 times
dplyr::filter(int.bbs, species2_scientific %in% c("Branta bernicla nigricans")) # subspecies 23 times 

#### GREAT BLUE HERON Ardea herodias and its subspecies in interactions & BBS obs... ####
# View the AOU for the genus species
subset(bbs.splist, genus_species=="Ardea herodias")
subset(bbs.splist, genus_species=="Ardea herodias occidentalis")
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(1940)) # Ardea herodias 45376 times 
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(1920)) # Ardea herodias occidentalis 61 times
dplyr::filter(int.bbs, species1_scientific %in% c("Ardea herodias")) # species 75 times 
dplyr::filter(int.bbs, species2_scientific %in% c("Ardea herodias")) # species 103 times 
dplyr::filter(int.bbs, species1_scientific %in% c("Ardea herodias occidentalis")) # subspecies 3 times - all communal nesting
dplyr::filter(int.bbs, species2_scientific %in% c("Ardea herodias occidentalis")) # subspecies 4 times - all communal nesting

# There are some observations in BBS of the subspecies and we have a few
# interactions for subspecies but most interactions are found at the
# species level. Since this subspecies is considered to fill similar niche, we
# will duplicate the species-level interactions to the subspecies.

# Make a selection for the species.
ardher1<-int.bbs[int.bbs$species1_scientific == "Ardea herodias",]
ardher2<-int.bbs[int.bbs$species2_scientific == "Ardea herodias",]

# Make copies of these interactions for each subspecies observed in BBS by
# over-writing species1 and sp1_AOU and species1_common 

### Ardea herodias occidentalis; AOU = 1920; (Great White Heron) Great Blue Heron
ardher.occ1<-ardher1
ardher.occ1$sp1_AOU<-1920
ardher.occ1$species1_scientific<-"Ardea herodias occidentalis"
ardher.occ1$species1_common<-"(Great White Heron) Great Blue Heron"
ardher.occ1$sp1_subspecies_status<-"sp1 and sp1_AOU originally Ardea herodias (see sp1_orig); also assigned species-level interactions to Ardea herodias occidentalis"
ardher.occ2<-ardher2
ardher.occ2$sp2_AOU<-1920
ardher.occ2$species2_scientific<-"Ardea herodias occidentalis"
ardher.occ2$species2_common<-"(Great White Heron) Great Blue Heron"
ardher.occ2$sp2_subspecies_status<-"sp2 and sp2_AOU originally Ardea herodias (see sp2_orig); also assigned species-level interactions to Ardea herodias occidentalis"

ardher.occ<-rbind(ardher.occ1,ardher.occ2)

# row bind these subspecies to the original interactions. Any duplicates will be
# removed later in network scripts.
dim(ardher.occ)
dim(int.bbs)
dim(int.bbs) + dim(ardher.occ)
int.bbs<-rbind(int.bbs, ardher.occ)
dim(int.bbs)
#19625

#### RED TAILED HAWK Buteo jamaicensis and its subspecies in interactions & BBS obs... ####
# View the AOU for the genus species
subset(bbs.splist, genus_species=="Buteo jamaicensis")
subset(bbs.splist, genus_species=="Buteo jamaicensis harlani")
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(3370)) # Buteo jamaicensis 60,777 times 
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(3380)) # Buteo jamaicensis harlani 78 times
dim(dplyr::filter(int.bbs, species1_scientific %in% c("Buteo jamaicensis"))) # species 73 times 
dim(dplyr::filter(int.bbs, species2_scientific %in% c("Buteo jamaicensis"))) # species 202 times 
dplyr::filter(int.bbs, species1_scientific %in% c("Buteo jamaicensis harlani")) # subspecies 1 time as hybridization
dplyr::filter(int.bbs, species2_scientific %in% c("Buteo jamaicensis harlani")) # subspecies 2 times as mobbing / predation

# Interactions are mostly at the species level and BBS observations mostly at
# species. Because this is a large top predator and the subspecies fills a
# similar niche, duplicate the species-level interactions to the subspecies.

# Make a selection for the species.
butjam1<-int.bbs[int.bbs$species1_scientific == "Buteo jamaicensis",]
butjam2<-int.bbs[int.bbs$species2_scientific == "Buteo jamaicensis",]

# Make copies of these interactions for each subspecies observed in BBS by
# over-writing species1 and sp1_AOU and species1_common 

### Buteo jamaicensis harlani; AOU = 3380; (Harlan's Hawk) Red-tailed Hawk
butjam.har1<-butjam1
butjam.har1$sp1_AOU<-3380
butjam.har1$species1_scientific<-"Buteo jamaicensis harlani"
butjam.har1$species1_common<-"(Harlan's Hawk) Red-tailed Hawk"
butjam.har1$sp1_subspecies_status<-"sp1 and sp1_AOU originally Buteo jamaicensis (see sp1_orig); also assigned species-level interactions to Buteo jamaicensis harlani"
butjam.har2<-butjam2
butjam.har2$sp2_AOU<-3380
butjam.har2$species2_scientific<-"Buteo jamaicensis harlani"
butjam.har2$species2_common<-"(Harlan's Hawk) Red-tailed Hawk"
butjam.har2$sp2_subspecies_status<-"sp2 and sp2_AOU originally Buteo jamaicensis (see sp2_orig); also assigned species-level interactions to Buteo jamaicensis harlani"

butjam.har<-rbind(butjam.har1,butjam.har2)

# row bind these subspecies to the original interactions. Any duplicates will be
# removed later in network scripts.
dim(butjam.har)
dim(int.bbs)
dim(int.bbs) + dim(butjam.har)
int.bbs<-rbind(int.bbs, butjam.har)
dim(int.bbs)
#19901

#### NORTHERN FLICKER Colaptes auratus and its subspecies in interactions & BBS obs... ####
# View the AOU for the genus species
subset(bbs.splist, genus_species=="Colaptes auratus")
subset(bbs.splist, genus_species=="Colaptes auratus auratus")
subset(bbs.splist, genus_species=="Colaptes auratus cafer")
subset(bbs.splist, genus_species=="Colaptes auratus auratus x auratus cafer")
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(4123)) # Colaptes auratus 1299 times 
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(4120)) # Colaptes auratus auratus 68195 times
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(4130)) # Colaptes auratus cafer 21912 times
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(4125)) # Colaptes auratus auratus x auratus cafer 34 times
dim(dplyr::filter(int.bbs, species1_scientific %in% c("Colaptes auratus"))) # species 51 times 
dim(dplyr::filter(int.bbs, species2_scientific %in% c("Colaptes auratus"))) # species 140 times 
dplyr::filter(int.bbs, species1_scientific %in% c("Colaptes auratus auratus")) # subspecies 2 times as hybridization
dplyr::filter(int.bbs, species2_scientific %in% c("Colaptes auratus auratus")) # subspecies 0 times 
dplyr::filter(int.bbs, species1_scientific %in% c("Colaptes auratus cafer")) # subspecies 2 times as hybridization
dplyr::filter(int.bbs, species2_scientific %in% c("Colaptes auratus cafer")) # subspecies 3 times; hybrid, nest takeover, competition 
dplyr::filter(int.bbs, species1_scientific %in% c("Colaptes auratus auratus x auratus cafer")) # subspecies 0 times 
dplyr::filter(int.bbs, species2_scientific %in% c("Colaptes auratus auratus x auratus cafer")) # subspecies 0 times  

# Interactions are mostly at the species level but BBS observations mostly at
# subspecies. Because the observations are unique to subspecies and it's
# unlikely they overlap as much geographically (though need to check this),
# duplicate the species-level interactions to each subspecies.

# Make a selection for the species.
colaur1<-int.bbs[int.bbs$species1_scientific == "Colaptes auratus",]
colaur2<-int.bbs[int.bbs$species2_scientific == "Colaptes auratus",]

# Make copies of these interactions for each subspecies observed in BBS by
# over-writing species1 and sp1_AOU and species1_common 

### Colaptes auratus auratus; AOU = 4120; (Yellow-shafted Flicker) Northern Flicker
colaur.aur1<-colaur1
colaur.aur1$sp1_AOU<-4120
colaur.aur1$species1_scientific<-"Colaptes auratus auratus"
colaur.aur1$species1_common<-"(Yellow-shafted Flicker) Northern Flicker"
colaur.aur1$sp1_subspecies_status<-"sp1 and sp1_AOU originally Colaptes auratus (see sp1_orig); also assigned species-level interactions to Colaptes auratus auratus"
colaur.aur2<-colaur2
colaur.aur2$sp2_AOU<-4120
colaur.aur2$species2_scientific<-"Colaptes auratus auratus"
colaur.aur2$species2_common<-"(Yellow-shafted Flicker) Northern Flicker"
colaur.aur2$sp2_subspecies_status<-"sp2 and sp2_AOU originally Colaptes auratus (see sp2_orig); also assigned species-level interactions to Colaptes auratus auratus"

colaur.aur<-rbind(colaur.aur1,colaur.aur2)

### Colaptes auratus cafer; AOU = 4130; (Red-shafted Flicker) Northern Flicker
colaur.caf1<-colaur1
colaur.caf1$sp1_AOU<-4130
colaur.caf1$species1_scientific<-"Colaptes auratus cafer"
colaur.caf1$species1_common<-"(Red-shafted Flicker) Northern Flicker"
colaur.caf1$sp1_subspecies_status<-"sp1 and sp1_AOU originally Colaptes auratus (see sp1_orig); also assigned species-level interactions to Colaptes auratus cafer"
colaur.caf2<-colaur2
colaur.caf2$sp2_AOU<-4130
colaur.caf2$species2_scientific<-"Colaptes auratus cafer"
colaur.caf2$species2_common<-"(Red-shafted Flicker) Northern Flicker"
colaur.caf2$sp2_subspecies_status<-"sp2 and sp2_AOU originally Colaptes auratus (see sp2_orig); also assigned species-level interactions to Colaptes auratus cafer"

colaur.caf<-rbind(colaur.caf1,colaur.caf2)

### Colaptes auratus auratus x auratus cafer; AOU = 4125; hybrid Northern Flicker (Red x Yellow-shafted)
colaur.axc1<-colaur1
colaur.axc1$sp1_AOU<-4125
colaur.axc1$species1_scientific<-"Colaptes auratus auratus x auratus cafer"
colaur.axc1$species1_common<-"hybrid Northern Flicker (Red x Yellow-shafted)"
colaur.axc1$sp1_subspecies_status<-"sp1 and sp1_AOU originally Colaptes auratus (see sp1_orig); also assigned species-level interactions to Colaptes auratus auratus x auratus cafer"
colaur.axc2<-colaur2
colaur.axc2$sp2_AOU<-4125
colaur.axc2$species2_scientific<-"Colaptes auratus auratus x auratus cafer"
colaur.axc2$species2_common<-"hybrid Northern Flicker (Red x Yellow-shafted)"
colaur.axc2$sp2_subspecies_status<-"sp2 and sp2_AOU originally Colaptes auratus (see sp2_orig); also assigned species-level interactions to Colaptes auratus auratus x auratus cafer"

colaur.axc<-rbind(colaur.axc1,colaur.axc2)

# row bind these subspecies to the original interactions. Any duplicates will be
# removed later in network scripts.
colaur <- do.call("rbind", list(colaur.aur, colaur.caf, colaur.axc))
dim(colaur)
dim(int.bbs)
dim(int.bbs) + dim(colaur)
int.bbs<-rbind(int.bbs, colaur)
dim(int.bbs)
#20474

#### JUNCOS Junco hyemalis and its subspecies in interactions & BBS obs... ####
# View the AOU for the genus species
subset(bbs.splist, genus_species=="Junco hyemalis")
subset(bbs.splist, genus_species=="Junco hyemalis hyemalis")
subset(bbs.splist, genus_species=="Junco hyemalis oreganus")
subset(bbs.splist, genus_species=="Junco hyemalis mearnsi")
subset(bbs.splist, genus_species=="Junco hyemalis aikeni")
subset(bbs.splist, genus_species=="Junco hyemalis caniceps")
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(5677)) # Junco hyemalis 755 times 
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(5670)) # Junco hyemalis hyemalis 15846 times
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(5671)) # Junco hyemalis oreganus 12297 times
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(5680)) # Junco hyemalis mearnsi 92 times
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(5660)) # Junco hyemalis aikeni 407 times
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(5690)) # Junco hyemalis caniceps 2729 times
dplyr::filter(int.bbs, species1_scientific %in% c("Junco hyemalis")) # species 33 times 
dplyr::filter(int.bbs, species2_scientific %in% c("Junco hyemalis")) # species 50 times 
dplyr::filter(int.bbs, species1_scientific %in% c("Junco hyemalis hyemalis")) # subspecies 1 time as hybridization
dplyr::filter(int.bbs, species2_scientific %in% c("Junco hyemalis hyemalis")) # subspecies 1 time as hybridization
dplyr::filter(int.bbs, species1_scientific %in% c("Junco hyemalis oreganus")) # subspecies 11 times (9 as hybridization)
dplyr::filter(int.bbs, species2_scientific %in% c("Junco hyemalis oreganus")) # subspecies 4 times as competition 
dplyr::filter(int.bbs, species1_scientific %in% c("Junco hyemalis mearnsi")) # subspecies 6 times (3 as hybridization)
dplyr::filter(int.bbs, species2_scientific %in% c("Junco hyemalis mearnsi")) # subspecies 1 time as hybridization
dplyr::filter(int.bbs, species1_scientific %in% c("Junco hyemalis aikeni")) # subspecies 0 times 
dplyr::filter(int.bbs, species2_scientific %in% c("Junco hyemalis aikeni")) # subspecies 1 time as hybridization
dplyr::filter(int.bbs, species1_scientific %in% c("Junco hyemalis caniceps")) # subspecies 7 times (3 as hybridization)
dplyr::filter(int.bbs, species2_scientific %in% c("Junco hyemalis caniceps")) # subspecies 5 times (2 as hybridization)

# Interactions are mostly at the species level but BBS observations mostly at
# subspecies. Because the observations are unique to subspecies and it's
# unlikely they overlap as much geographically (though need to check this),
# duplicate the species-level interactions to each subspecies.

# Make a selection for the species.
junhye1<-int.bbs[int.bbs$species1_scientific == "Junco hyemalis",]
junhye2<-int.bbs[int.bbs$species2_scientific == "Junco hyemalis",]

# Make copies of these interactions for each subspecies observed in BBS by
# over-writing species1 and sp1_AOU and species1_common 

### Junco hyemalis hyemalis; AOU = 5670; (Slate-colored Junco) Dark-eyed Junco
junhye.hye1<-junhye1
junhye.hye1$sp1_AOU<-5670
junhye.hye1$species1_scientific<-"Junco hyemalis hyemalis"
junhye.hye1$species1_common<-"(Slate-colored Junco) Dark-eyed Junco"
junhye.hye1$sp1_subspecies_status<-"sp1 and sp1_AOU originally Junco hyemalis (see sp1_orig); also assigned species-level interactions to Junco hyemalis hyemalis"
junhye.hye2<-junhye2
junhye.hye2$sp2_AOU<-5670
junhye.hye2$species2_scientific<-"Junco hyemalis hyemalis"
junhye.hye2$species2_common<-"(Slate-colored Junco) Dark-eyed Junco"
junhye.hye2$sp2_subspecies_status<-"sp2 and sp2_AOU originally Junco hyemalis (see sp2_orig); also assigned species-level interactions to Junco hyemalis hyemalis"

junhye.hye<-rbind(junhye.hye1,junhye.hye2)

### Junco hyemalis oreganus; AOU = 5671; (Oregon Junco) Dark-eyed Junco
junhye.ore1<-junhye1
junhye.ore1$sp1_AOU<-5671
junhye.ore1$species1_scientific<-"Junco hyemalis oreganus"
junhye.ore1$species1_common<-"(Oregon Junco) Dark-eyed Junco"
junhye.ore1$sp1_subspecies_status<-"sp1 and sp1_AOU originally Junco hyemalis (see sp1_orig); also assigned species-level interactions to Junco hyemalis oreganus"
junhye.ore2<-junhye2
junhye.ore2$sp2_AOU<-5671
junhye.ore2$species2_scientific<-"Junco hyemalis oreganus"
junhye.ore2$species2_common<-"(Oregon Junco) Dark-eyed Junco"
junhye.ore2$sp2_subspecies_status<-"sp2 and sp2_AOU originally Junco hyemalis (see sp2_orig); also assigned species-level interactions to Junco hyemalis oreganus"

junhye.ore<-rbind(junhye.ore1,junhye.ore2)

### Junco hyemalis mearnsi; AOU = 5680; (Pink-sided Junco) Dark-eyed Junco
junhye.mea1<-junhye1
junhye.mea1$sp1_AOU<-5680
junhye.mea1$species1_scientific<-"Junco hyemalis mearnsi"
junhye.mea1$species1_common<-"(Pink-sided Junco) Dark-eyed Junco"
junhye.mea1$sp1_subspecies_status<-"sp1 and sp1_AOU originally Junco hyemalis (see sp1_orig); also assigned species-level interactions to Junco hyemalis mearnsi"
junhye.mea2<-junhye2
junhye.mea2$sp2_AOU<-5680
junhye.mea2$species2_scientific<-"Junco hyemalis mearnsi"
junhye.mea2$species2_common<-"(Pink-sided Junco) Dark-eyed Junco"
junhye.mea2$sp2_subspecies_status<-"sp2 and sp2_AOU originally Junco hyemalis (see sp2_orig); also assigned species-level interactions to Junco hyemalis mearnsi"

junhye.mea<-rbind(junhye.mea1,junhye.mea2)

### Junco hyemalis aikeni; AOU = 5660; (White-winged Junco) Dark-eyed Junco
junhye.aik1<-junhye1
junhye.aik1$sp1_AOU<-5660
junhye.aik1$species1_scientific<-"Junco hyemalis aikeni"
junhye.aik1$species1_common<-"(White-winged Junco) Dark-eyed Junco"
junhye.aik1$sp1_subspecies_status<-"sp1 and sp1_AOU originally Junco hyemalis (see sp1_orig); also assigned species-level interactions to Junco hyemalis aikeni"
junhye.aik2<-junhye2
junhye.aik2$sp2_AOU<-5660
junhye.aik2$species2_scientific<-"Junco hyemalis aikeni"
junhye.aik2$species2_common<-"(White-winged Junco) Dark-eyed Junco"
junhye.aik2$sp2_subspecies_status<-"sp2 and sp2_AOU originally Junco hyemalis (see sp2_orig); also assigned species-level interactions to Junco hyemalis aikeni"

junhye.aik<-rbind(junhye.aik1,junhye.aik2)

### Junco hyemalis caniceps; AOU = 5690; (Gray-headed Junco) Dark-eyed Junco
junhye.can1<-junhye1
junhye.can1$sp1_AOU<-5690
junhye.can1$species1_scientific<-"Junco hyemalis caniceps"
junhye.can1$species1_common<-"(Gray-headed Junco) Dark-eyed Junco"
junhye.can1$sp1_subspecies_status<-"sp1 and sp1_AOU originally Junco hyemalis (see sp1_orig); also assigned species-level interactions to Junco hyemalis caniceps"
junhye.can2<-junhye2
junhye.can2$sp2_AOU<-5690
junhye.can2$species2_scientific<-"Junco hyemalis caniceps"
junhye.can2$species2_common<-"(Gray-headed Junco) Dark-eyed Junco"
junhye.can2$sp2_subspecies_status<-"sp2 and sp2_AOU originally Junco hyemalis (see sp2_orig); also assigned species-level interactions to Junco hyemalis caniceps"

junhye.can<-rbind(junhye.can2,junhye.can2)

# row bind these subspecies to the original interactions. Any duplicates will be
# removed later in network scripts.
junhye <- do.call("rbind", list(junhye.hye, junhye.ore, junhye.mea, junhye.aik, junhye.can))
dim(junhye)
dim(int.bbs)
dim(int.bbs) + dim(junhye)
int.bbs<-rbind(int.bbs, junhye)
dim(int.bbs)
#20906

#### YELLOW-RUMPED WARBLER Setophaga coronata and its subspecies in interactions & BBS obs... ####
# View the AOU for the genus species
subset(bbs.splist, genus_species=="Setophaga coronata")
subset(bbs.splist, genus_species=="Setophaga coronata coronata")
subset(bbs.splist, genus_species=="Setophaga coronata audoboni")
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(6556)) # Setophaga coronata 634 times 
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(6550)) # Setophaga coronata coronata 18871 times
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(6560)) # Setophaga coronata audoboni 12809 times
dim(dplyr::filter(int.bbs, species1_scientific %in% c("Setophaga coronata"))) # species 45 times 
dim(dplyr::filter(int.bbs, species2_scientific %in% c("Setophaga coronata"))) # species 48 times 
dplyr::filter(int.bbs, species1_scientific %in% c("Setophaga coronata coronata")) # subspecies 1 time as facilitation
dplyr::filter(int.bbs, species2_scientific %in% c("Setophaga coronata coronata")) # subspecies 4 times (once as hybridization)
dplyr::filter(int.bbs, species1_scientific %in% c("Setophaga coronata audoboni")) # subspecies 0 times 
dplyr::filter(int.bbs, species2_scientific %in% c("Setophaga coronata audoboni")) # subspecies 0 times 

# Interactions are mostly at the species level but BBS observations mostly at
# subspecies. Because the observations are unique to subspecies and it's
# unlikely they overlap as much geographically (though need to check this),
# duplicate the species-level interactions to each subspecies.

# Make a selection for the species.
setcor1<-int.bbs[int.bbs$species1_scientific == "Setophaga coronata",]
setcor2<-int.bbs[int.bbs$species2_scientific == "Setophaga coronata",]

# Make copies of these interactions for each subspecies observed in BBS by
# over-writing species1 and sp1_AOU and species1_common 

### Setophaga coronata coronata; AOU = 6550; (Myrtle Warbler) Yellow-rumped Warbler
setcor.cor1<-setcor1
setcor.cor1$sp1_AOU<-6550
setcor.cor1$species1_scientific<-"Setophaga coronata coronata"
setcor.cor1$species1_common<-"(Myrtle Warbler) Yellow-rumped Warbler"
setcor.cor1$sp1_subspecies_status<-"sp1 and sp1_AOU originally Setophaga coronata (see sp1_orig); also assigned species-level interactions to Setophaga coronata coronata"
setcor.cor2<-setcor2
setcor.cor2$sp2_AOU<-6550
setcor.cor2$species2_scientific<-"Setophaga coronata coronata"
setcor.cor2$species2_common<-"(Myrtle Warbler) Yellow-rumped Warbler"
setcor.cor2$sp2_subspecies_status<-"sp2 and sp2_AOU originally Setophaga coronata (see sp2_orig); also assigned species-level interactions to Setophaga coronata coronata"

setcor.cor<-rbind(setcor.cor1,setcor.cor2)

### Setophaga coronata audoboni; AOU = 6560; (Audubon's Warbler) Yellow-rumped Warbler
setcor.aud1<-setcor1
setcor.aud1$sp1_AOU<-6560
setcor.aud1$species1_scientific<-"Setophaga coronata audoboni"
setcor.aud1$species1_common<-"(Audubon's Warbler) Yellow-rumped Warbler"
setcor.aud1$sp1_subspecies_status<-"sp1 and sp1_AOU originally Setophaga coronata (see sp1_orig); also assigned species-level interactions to Setophaga coronata audoboni"
setcor.aud2<-setcor2
setcor.aud2$sp2_AOU<-6560
setcor.aud2$species2_scientific<-"Setophaga coronata audoboni"
setcor.aud2$species2_common<-"(Audubon's Warbler) Yellow-rumped Warbler"
setcor.aud2$sp2_subspecies_status<-"sp2 and sp2_AOU originally Setophaga coronata (see sp2_orig); also assigned species-level interactions to Setophaga coronata audoboni"

setcor.aud<-rbind(setcor.aud1,setcor.aud2)

# row bind these subspecies to the original interactions. Any duplicates will be
# removed later in network scripts.
setcor <- do.call("rbind", list(setcor.cor, setcor.aud))
dim(setcor)
dim(int.bbs)
dim(int.bbs) + dim(setcor)
int.bbs<-rbind(int.bbs, setcor)
dim(int.bbs)
# 21092

#### End of subspecies edits. Export L1 interaction data for BBS analysis

## Note to check this before using new version:
write.csv(int.bbs,file.path(L1_dir,"AvianInteractionData_BBS_L1_7Aug2024.csv"), row.names=F)

# Export updated bbs species names list
names(bbs.splist)[names(bbs.splist) == "bbs_sp1_common"] <-"English_Common_Name"
names(bbs.splist)[names(bbs.splist) == "sp1_AOU"] <-"AOU"

write.csv(bbs.splist,file.path(L1_dir,"bbs_splist_2022_L1.csv"), row.names=F)


