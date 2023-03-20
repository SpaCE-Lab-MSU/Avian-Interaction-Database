# TITLE:          Avian Interaction Pairs Data: L0 to L1
# AUTHORS:        Phoebe Zarnetske, Pat Bills
# COLLABORATORS:  Vincent Miele, Stephane Dray, Sara Zonneveld, ...
# DATA INPUT:     Data imported as csv https://github.com/SpaCE-Lab-MSU/Avian-Interaction-Database/blob/main/L0/AvianInteractionData_L0.csv
# DATA OUTPUT:    L1 data: AvianInteractionData_L1.csv
# PROJECT:        avian-meta-network
# DATE:           27 Oct 2022; updated 20 Mar 2023  
# NOTES:          Next script to run: 
#                 This script is used to refine species name changes to align with BOW, and to create intxns_L1.csv. 
#                 L0 data are checked to assign BOW scientific and common names to the interaction pairs data (which were originally from BBS species list). 
#               
#               
#         ******June 8, 2022: updated to copy Colaptes auratus interactions to all Colaptes auratus subspecies
#               This assumes that they all have the same ranges (they don't), so a refined set of interactions should be edited to
#               match the actual overlapping areas with interactors

#               Name changes in the 2019 Species List affected these species (already edited in original Google Sheet here:
#               "/Volumes/GoogleDrive/Shared drives/Avian_MetaNetwork/data/L0/avian_intxn_data"
#               Circus cyaneus -> Circus hudsonius
#               Oreothlypis celata -> Leiothlypis celata
#               Oreothlypis luciae -> Leiothlypis luciae
#               Oreothlypis ruficapilla -> Leiothlypis ruficapilla
#               Picoides albolarvatus -> Dryobates albolarvatus
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
L0_dir <- "/Users/phoebezarnetske/Documents/GitHub/Avian-Interaction-Database/L0"
L1_dir <- "/Users/phoebezarnetske/Documents/GitHub/Avian-Interaction-Database/L1"

# Read in csv with avian interactions from primary, secondary cavity nesting birds in North America.
int.raw<-read.csv(file.path(L0_dir,"AvianInteractionData_L0.csv"))

# Read in species list: all species in BBS
splist<-read.csv(file.path(L0_dir,"bbs_splist.csv"))

# make "genus species" columns able to merge
splist$species1_scientific<- do.call(paste, c(splist[c("Genus", "Species")], sep = " "))

# Consider using ITIS for species naming conventions
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

# Some interaction scientific names are still not matching Birds of the World 
# See: ./L0/bbs_intxn_data/bbsbow_names.xlsx
# Standardize based on BOW:
# BBS: Falcipennis canadensis = BOW: Canachites canadensis
# BBS: Phalacrocorax penicillatus	= BOW: Urile penicillatus
# BBS: Phalacrocorax pelagicus = BOW:	Urile pelagicus
# BBS: Poecile hudsonica = BOW: Poecile hudsonicus

splist$species1_scientific[splist$species1_scientific == "Falcipennis canadensis"] <- "Canachites canadensis"
splist$species1_scientific[splist$species1_scientific == "Phalacrocorax penicillatus"] <- "Urile penicillatus"
splist$species1_scientific[splist$species1_scientific == "Phalacrocorax pelagicus"] <- "Urile pelagicus"
splist$species1_scientific[splist$species1_scientific == "Poecile hudsonica"] <- "Poecile hudsonicus"

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
# 14700 rows
dim(intxns1)
# 14943 rows
length(unique(int.raw$species1_scientific))
# 642 species treated as species1 in original avian interaction data
length(unique(splist$species1_scientific))
# 756 species in entire BBS dataset
length(unique(intxns1$species1_scientific))
# 885 species in the merged data
length(unique(intxns1$species2_scientific))
# 2060 species as species2 but these *may* include the scientific names without a match in sp1
sum(is.na(intxns1$species2_scientific)) 
# 244 - species that exist in the BBS Species List but are not entered yet in original avian interaction data as species2
length(unique(int.raw$species2_scientific))
# 2060 species as species2 but 1 without a match in sp1?

# Repeat above but now for sp2
# Merge into paired intxns by sp1
intxns2<-merge(int.raw,sp2list,by=c("species2_scientific"),all.x=T, all.y=T)
dim(int.raw)
# 14700 rows
dim(intxns2)
# 14801 rows
length(unique(int.raw$species2_scientific))
# 2060 species treated as species2 in original avian interaction data
length(unique(splist$species1_scientific))
# 756 species in entire BBS dataset
length(unique(intxns2$species2_scientific))
# 2161 species in the merged data 
sum(is.na(intxns2$species1_scientific)) 
# 101 NAs - species that exist in the BBS Species List but are not entered yet in original avian interaction data as species1
length(unique(intxns2$species1_scientific))
# 643 species as species1 but these *may* include the scientific names without a match in sp1
length(unique(int.raw$species1_scientific))
# 642 species as species1 but these *may* include the scientific names without a match in sp1

# Export to check species names: if there are rows without a complete entry, they are 
# species in BOW but not in BBS Species List.
write.csv(intxns1, file.path(L1_dir,"intxns1_names.csv"), row.names=F) 
write.csv(intxns2, file.path(L1_dir,"intxns2_names.csv"), row.names=F) 

## END OF CHECKING ##

# Omit common names from int.raw ahead of merging with BBS Species List (we will replace them with the official BBS names)
# Note that this removes X rows where there is no species2_scientific in int.raw, but there are interactions recorded
# make a copy of int.raw
int.l1<-int.raw
int.l1$species1_common <-NULL
int.l1$species2_common <-NULL
dim(int.l1)

# Remove the NA rows
int.l1<-int.l1[!is.na(int.l1$effect_sp1_on_sp2), ]
dim(int.l1)
# 14629
# Assign common name columns with the scientific name
int.l1<-merge(int.l1,splist,by=c("species1_scientific"),all.x=T, all.y=T)
int.l1<-merge(int.l1,sp2list,by=c("species2_scientific"),all.x=T, all.y=T)
dim(int.l1)
# 14974
# Remove the NA rows (there shouldn't be any; they are all situations where common names occur but are not in the database)
dim(int.l1)
# 14974
int.l1<-int.l1[!is.na(int.l1$effect_sp1_on_sp2), ]
dim(int.l1)
#14629 - Seems like there are some afterall... *** need to check this EMILY

# rename
#names(int.l1)[names(int.l1) == "bbs_sp2_common"] <-"sp2_common"
#names(int.l1)[names(int.l1) == "bbs_sp1_common"] <-"sp1_common"

## EXPORT the cleaned interaction pairs data:
write.csv(int.l1,file.path(L1_dir,"AvianInteractionData_L1.csv"), row.names=F)

## EDIT Colaptes auratus subspecies
# Here we should assign the same interactions for the subspecies as occur for the main species
# Tricky- need to select all Colaptes auratus and replicate, but assigning the 3 subspecies.
# For now, drop the 3 subspecies (assign them Colaptes auratus in the bbs obs data: bbs_obs_L1.R) until we can refine this to represent their real potential interactions.



