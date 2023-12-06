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

# We are using the Birds of The World naming conventions for species. Some BBS names differ. Some old names are included.
# Rename some columns and omit others; indicate that the common name is coming from BBS Species List
names(splist)[names(splist) == "English_Common_Name"] <-"bbs_sp1_common"
names(splist)[names(splist) == "Seq"] <-"sp1_Seq"
names(splist)[names(splist) == "AOU"] <-"sp1_AOU"
#splist$ORDER <-NULL
#splist$Family <-NULL
#splist$Genus <-NULL
#splist$Species <-NULL
#splist$Spanish_Common_Name <-NULL
#splist$French_Common_Name <-NULL

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

# Some misspellings:
int.raw[int.raw=="Dryobates nutallii"] <- "Dryobates nuttallii"

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
dim(namechg.unmatch) # 19 species on Dec 6, 2023
namechg.unmatch[,1:2]

# Standardize based on BOW 

# RENAME: BBS: Phalacrocorax penicillatus	= BOW: Urile penicillatus
dplyr::filter(splist, species1_scientific %in% c("Phalacrocorax penicillatus")) # not in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Phalacrocorax penicillatus")) # in interactions
dplyr::filter(int.raw, species2_scientific %in% c("Phalacrocorax penicillatus")) # in interactions
dplyr::filter(splist, species1_scientific %in% c("Urile penicillatus")) # in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Urile penicillatus")) # in interactions
dplyr::filter(int.raw, species2_scientific %in% c("Urile penicillatus")) # in interactions
# Update all to new species name
splist$species1_scientific[splist$species1_scientific == "Phalacrocorax penicillatus"] <- "Urile penicillatus"
int.raw$species1_scientific[int.raw$species1_scientific == "Phalacrocorax penicillatus"] <- "Urile penicillatus"
int.raw$species2_scientific[int.raw$species2_scientific == "Phalacrocorax penicillatus"] <- "Urile penicillatus"

# RENAME: BBS: Phalacrocorax pelagicus = BOW:	Urile pelagicus
dplyr::filter(splist, species1_scientific %in% c("Phalacrocorax pelagicus")) # not in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Phalacrocorax pelagicus")) # in interactions
dplyr::filter(int.raw, species2_scientific %in% c("Phalacrocorax pelagicus")) # in interactions
dplyr::filter(splist, species1_scientific %in% c("Urile pelagicus")) # in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Urile pelagicus")) # not in interactions
dplyr::filter(int.raw, species2_scientific %in% c("Urile pelagicus")) # in interactions
# Update all to new species name
splist$species1_scientific[splist$species1_scientific == "Phalacrocorax pelagicus"] <- "Urile pelagicus"
int.raw$species1_scientific[int.raw$species1_scientific == "Phalacrocorax pelagicus"] <- "Urile pelagicus"
int.raw$species2_scientific[int.raw$species2_scientific == "Phalacrocorax pelagicus"] <- "Urile pelagicus"

# RENAME: BBS: Falcipennis canadensis = BOW: Canachites canadensis
dplyr::filter(splist, species1_scientific %in% c("Falcipennis canadensis")) # not in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Falcipennis canadensis")) # in interactions
dplyr::filter(int.raw, species2_scientific %in% c("Falcipennis canadensis")) # in interactions
dplyr::filter(splist, species1_scientific %in% c("Canachites canadensis")) # in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Canachites canadensis")) # not in interactions
dplyr::filter(int.raw, species2_scientific %in% c("Canachites canadensis")) # in interactions
# Update all to new species name
splist$species1_scientific[splist$species1_scientific == "Falcipennis canadensis"] <- "Canachites canadensis"
int.raw$species1_scientific[int.raw$species1_scientific == "Falcipennis canadensis"] <- "Canachites canadensis"
int.raw$species2_scientific[int.raw$species2_scientific == "Falcipennis canadensis"] <- "Canachites canadensis"

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

# RENAME: BBS: Regulus calendula = BOW: Corthylio calendula
dplyr::filter(splist, species1_scientific %in% c("Regulus calendula")) # not in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Regulus calendula")) # in interactions
dplyr::filter(int.raw, species2_scientific %in% c("Regulus calendula")) # in interactions
dplyr::filter(splist, species1_scientific %in% c("Corthylio calendula")) # in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Corthylio calendula")) # in interactions
dplyr::filter(int.raw, species2_scientific %in% c("Corthylio calendula")) # in interactions
# Update all to new species name
splist$species1_scientific[splist$species1_scientific == "Regulus calendula"] <- "Corthylio calendula"
int.raw$species1_scientific[int.raw$species1_scientific == "Regulus calendula"] <- "Corthylio calendula"
int.raw$species2_scientific[int.raw$species2_scientific == "Regulus calendula"] <- "Corthylio calendula"

## Checked up to here 5pm Dec 6 2023

##   RECENT SPLIT: Pica pica (Eurasia), P. hudsonia (North America), and P. nuttalli (North America); check common name for species info
dplyr::filter(splist, species1_scientific %in% c("Pica pica")) # not in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Pica pica")) # not in interactions 
dplyr::filter(splist, species1_scientific %in% c("Pica hudsonia")) # in BBS list
dplyr::filter(int.raw.orig, species1_scientific %in% c("Pica hudsonia")) # in interactions (seem to be all N. America)
dplyr::filter(splist, species1_scientific %in% c("Pica nuttalli")) # in BBS list
dplyr::filter(int.raw.orig, species1_scientific %in% c("Pica nuttalli")) # in interactions (seem to be all N. America)
# For now, keep as is; the BBS observations will include P. hudsonia and P. nuttalli


## American Crow Subspecies = Northwestern Crow: Corvus brachyrhynchos caurinus (instead of Corvus caurinus "Northwestern Crow")
dplyr::filter(splist, species1_scientific %in% c("Corvus brachyrhynchos")) # in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Corvus brachyrhynchos")) # in interactions 
dplyr::filter(splist, species1_scientific %in% c("Corvus brachyrhynchos caurinus")) # not in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Corvus brachyrhynchos caurinus")) # in interactions 
dplyr::filter(splist, species1_scientific %in% c("Corvus caurinus")) # not in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Corvus caurinus")) # not in interactions 
dplyr::filter(int.raw, species2_scientific %in% c("Corvus caurinus")) # in interactions 
# RENAME: BBS & BOW: change Corvus caurinus to Corvus brachyrhynchos caurinus 
int.raw$species1_scientific[int.raw$species1_scientific == "Corvus caurinus"] <- "Corvus brachyrhynchos caurinus"
int.raw$species1_scientific[int.raw$species2_scientific == "Corvus caurinus"] <- "Corvus brachyrhynchos caurinus"

##  RECENT SPLIT: Larus brachyrhynchus split into L. brachyrhynchus (North America) and L. canus (Eurasia)
##  https://birdsoftheworld.org/bow/species/mewgul/cur/introduction#sys
# BBS: Larus brachyrhynchus (endemic to North America) = BOW: Larus canus (endemic to Eurasia) & Larus brachyrhynchus (endemic to North America)
dplyr::filter(splist, species1_scientific %in% c("Larus canus")) # not in BBS list
dplyr::filter(splist, species1_scientific %in% c("Larus brachyrhynchus")) # in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Larus canus")) # in interactions
dplyr::filter(int.raw, species1_scientific %in% c("Larus brachyrhynchus")) # not in interactions
# L. brachyrhynchus interactions - these are entered as of Dec 6, 2023. 
# Confirmed that L. canus interactions are only Eurasian interactions.
# No change needed to these data.

##   RECENT SPLIT: C. hudsonius (North America) and C. cyaneus (Eurasia); check common name for species info
##  "The only representative in North America of the cosmopolitan genus Circus, 
##  the Northern Harrier was until recently considered conspecific with the 
##  Hen Harrier (Circus cyaneus) of Eurasia, but differs from that species in genetics and plumage." 
##  https://birdsoftheworld.org/bow/species/norhar2/cur/introduction
dplyr::filter(splist, species1_scientific %in% c("Circus cyaneus")) # not in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Circus cyaneus")) # in interactions but N. America interaction
dplyr::filter(splist, species1_scientific %in% c("Circus hudsonius")) # in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Circus hudsonius")) # in interactions
# Only C. hudsonius occurs in BBS list - change all interactions to C. hudsonius (North America)
int.raw$species1_scientific[int.raw$species1_scientific == "Circus cyaneus"] <- "Circus hudsonius"
int.raw$species2_scientific[int.raw$species2_scientific == "Circus cyaneus"] <- "Circus hudsonius"


##  RENAME: Nannopterum auritum is new name for Phalacrocorax auritus
dplyr::filter(splist, species1_scientific %in% c("Phalacrocorax auritus")) # not in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Phalacrocorax auritus")) # in interactions
dplyr::filter(splist, species1_scientific %in% c("Nannopterum auritum")) # in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Nannopterum auritum")) # in interactions
# Assign BBS & BOW: Nannopterum auritum; some old names exist in interactions
int.raw$species1_scientific[int.raw$species1_scientific == "Phalacrocorax auritus"] <- "Nannopterum auritum"
int.raw$species2_scientific[int.raw$species2_scientific == "Phalacrocorax auritus"] <- "Nannopterum auritum"

##  RENAME: Nannopterum brasilianum is new name for Phalacrocorax brasilianus
dplyr::filter(splist, species1_scientific %in% c("Phalacrocorax brasilianus")) # not in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Phalacrocorax brasilianus")) # not in interactions
dplyr::filter(splist, species1_scientific %in% c("Nannopterum brasilianum")) # in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Nannopterum brasilianum")) # in interactions
# no action necessary; name updated for species (also checked species2)



# duplicate it for easier merging below
sp2list<-splist
# rename
names(sp2list)[names(sp2list) == "sp1_Seq"] <-"sp2_Seq"
names(sp2list)[names(sp2list) == "sp1_AOU"] <-"sp2_AOU"
names(sp2list)[names(sp2list) == "bbs_sp1_common"] <-"bbs_sp2_common"
names(sp2list)[names(sp2list) == "species1_scientific"] <-"species2_scientific"

# Merge into paired intxns by sp1 - this is not updated for Nov 22, 2023
intxns1<-merge(int.raw,splist,by=c("species1_scientific"),all.x=T, all.y=T)
dim(int.raw)
# 16878 rows
dim(intxns1)
# 17101 rows
length(unique(int.raw$species1_scientific))
# 697 species treated as species1 in original avian interaction data
length(unique(splist$species1_scientific))
# 756 species in entire BBS dataset
length(unique(intxns1$species1_scientific))
# 920 species in the merged data
length(unique(intxns1$species2_scientific))
# 2228 species as species2 but these *may* include the scientific names without a match in sp1
sum(is.na(intxns1$species2_scientific)) 
# 224 - species that exist in the BBS Species List but are not entered yet in original avian interaction data as species2
length(unique(int.raw$species2_scientific))
# 2228 species as species2 but 1 without a match in sp1?

# Repeat above but now for sp2 - this is not updated for Nov 22, 2023
# Merge into paired intxns by sp1
intxns2<-merge(int.raw,sp2list,by=c("species2_scientific"),all.x=T, all.y=T)
dim(int.raw)
# 16878 rows
dim(intxns2)
# 16970 rows
length(unique(int.raw$species2_scientific))
# 2228 species treated as species2 in original avian interaction data
length(unique(splist$species1_scientific))
# 756 species in entire BBS dataset
length(unique(intxns2$species2_scientific))
# 2320 species in the merged data 
sum(is.na(intxns2$species1_scientific)) 
# 92 NAs - species that exist in the BBS Species List but are not entered yet in original avian interaction data as species1
length(unique(intxns2$species1_scientific))
# 698 species as species1 but these *may* include the scientific names without a match in sp1
length(unique(int.raw$species1_scientific))
# 697 species as species1 but these *may* include the scientific names without a match in sp1

# Export to check species names: if there are rows without a complete entry, they are 
# species in BOW but not in BBS Species List.
write.csv(intxns1, file.path(L1_dir,"intxns1_names.csv"), row.names=F) 
write.csv(intxns2, file.path(L1_dir,"intxns2_names.csv"), row.names=F) 

#intxns1 yields the following issues:

## Missing effect_sp1_on_sp2 or effect_sp2_on_sp1 or both (issue # 108 on github)
# species1_scientific	species1_common	species2_common	species2_scientific
# Ardea alba	Great Egret	Brown Pelican	Pelecanus occidentalis
# Cepphus columba	Pigeon Guillemot	unid. gull	Laridae sp.
# Cepphus columba	Pigeon Guillemot	unid. gull	Laridae sp.
# Cepphus columba	Pigeon Guillemot	unid. puffin	Fratercula sp. 
# Cepphus columba	Pigeon Guillemot	Black Oystercatchers	Haematopus bachmani
# Cepphus columba	Pigeon Guillemot	unid. Comorant	Phalacrocorax sp. 
# Melanerpes erythrocephalus	Red-headed Woodpecker	Mountain Bluebird	Sialia currucoides
# Myiarchus crinitus	Great-crested flycatcher	Eastern Bluebird	Sialia sialis
# Turdus migratorius	American Robin	Common Grackles	Quiscalus quiscula
# Turdus migratorius	American Robin	European Starlings 	Sturnus vulgaris


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



## Later work:
## EDIT Colaptes auratus subspecies
# Here we should assign the same interactions for the subspecies as occur for the main species
# Tricky- need to select all Colaptes auratus and replicate, but assigning the 3 subspecies.
# For now, drop the 3 subspecies (assign them Colaptes auratus in the bbs obs data: bbs_obs_L1.R) until we can refine this to represent their real potential interactions.



