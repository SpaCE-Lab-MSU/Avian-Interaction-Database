# TITLE:          Avian Interaction Pairs Data: L1 post-taxa cleaning from 
#                 AvianInteractionData_L1.R 
# AUTHORS:        Phoebe Zarnetske
# COLLABORATORS:  Vincent Miele, Stephane Dray, Emily Parker
# DATA INPUT:     int.namefix.bbs <- load(file.path(L1_dir,"int.namefix.bbs.RData"))
#
# DATA OUTPUT:    L1 data: AvianInteractionData_L1.csv
#                 L1 data: AvianInteractionData_L1_BBS.csv for BBS analysis
#                 L1 data: bbs_splist_2024_L1.csv for BBS analysis (species name changes)
# PROJECT:        Avian Interaction Database 
# DATE:           27 Oct 2022; updated through 12 Dec 2024  
# NOTES:          Next script to run: 
#                 This script is used to clean up columns in the interaction dataset.
#                 ** Currently, the script is working for BBS species only; needs 
#                           updating for the remainder.
#                                  
#                 Updates a new column that also includes scientific name 
#                 changes associated with the AOUcombo.index for merging with 
#                 those names in the AvianInteractionData_AOUindex_L1.csv
#               
# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)
library(dplyr)
library(stringr)

# .Renviron not working for PLZ; hard-coding in here
L0_dir <- "/Users/plz/Documents/GitHub/Avian-Interaction-Database/L0"
#L0_dir <- "/Users/phoebezarnetske/Documents/GitHub/Avian-Interaction-Database/L0"

L1_dir <- "/Users/plz/Documents/GitHub/Avian-Interaction-Database/L1"
#L1_dir <- "/Users/phoebezarnetske/Documents/GitHub/Avian-Interaction-Database/L1"

L1_RData_dir <- "~/Google Drive/Shared drives/Avian_MetaNetwork/data/L1"

# Read in species list: all species in BBS (the 2024 release which includes all
# species as of 2023, plus the additional AOUcombo.index column for use w BBS
# index)
splist2024<-read.csv(file.path(L1_dir,"bbs_splist_2024_L1.csv"))
# Rename some columns and omit others; indicate that the common name is coming
# from BBS Species List
names(splist2024)[names(splist2024) == "English_Common_Name"] <-"bbs_common_name"

# Read in the current version of the database
load(file.path(L1_RData_dir,"int.namefix.bbs.RData"))

#*******************************************#
#### Scientific Name Checking: Data Prep ####
#*******************************************#

intxns12<-int.namefix.bbs
intxns12$sp1sci.replaced<-NULL
intxns12$sp2sci.replaced<-NULL

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
intxns12$interaction[intxns12$interaction=="ammenalism"] <- "amensalism"
intxns12$interaction[intxns12$interaction=="amensalism"] <- "amensalism"
intxns12$interaction[intxns12$interaction=="amenslism"] <- "amensalism"
intxns12$interaction[intxns12$interaction=="amenalism"] <- "amensalism"
intxns12$interaction[intxns12$interaction=="brood"] <- "brood parasitism"
intxns12$interaction[intxns12$interaction=="brood-parasitism"] <- "brood parasitism"
intxns12$interaction[intxns12$interaction=="brood parasitsm"] <- "brood parasitism"
intxns12$interaction[intxns12$interaction=="call mimicking"] <- "commensalism-call mimicry"
intxns12$interaction[intxns12$interaction=="call mimickry"] <- "commensalism-call mimicry"
intxns12$interaction[intxns12$interaction=="comensalism"] <- "commensalism"
intxns12$interaction[intxns12$interaction=="commenalism"] <- "commensalism"
intxns12$interaction[intxns12$interaction=="commesalism"] <- "commensalism"
intxns12$interaction[intxns12$interaction=="call mimicry"] <- "commensalism-call mimicry"
intxns12$interaction[intxns12$interaction=="commensalism -call mimicry"] <- "commensalism-call mimicry"
intxns12$interaction[intxns12$interaction=="commenslism - call mimicry"] <- "commensalism-call mimicry"
intxns12$interaction[intxns12$interaction=="commensalism-chick adoptio"] <- "commensalism-chick adoption"
intxns12$interaction[intxns12$interaction=="commesalism-call mimicry"] <- "commensalism-call mimicry"
intxns12$interaction[intxns12$interaction=="comeptition"] <- "competition"
intxns12$interaction[intxns12$interaction=="competiton"] <- "competition"
intxns12$interaction[intxns12$interaction=="competion"] <- "competition"
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
intxns12$interaction[intxns12$interaction=="faciltation-mixed flock"] <- "facilitation-mixed flocking"
intxns12$interaction[intxns12$interaction=="faciltiation-mixed flock"] <- "facilitation-mixed flocking"
intxns12$interaction[intxns12$interaction=="mixed flock"] <- "facilitation-mixed flocking"
intxns12$interaction[intxns12$interaction=="faciltation-feeding"] <- "facilitation-feeding"
intxns12$interaction[intxns12$interaction=="facilitation - feeding"] <- "facilitation-feeding"
intxns12$interaction[intxns12$interaction=="faciltiation-feeding"] <- "facilitation-feeding"
intxns12$interaction[intxns12$interaction=="faciliation-feeding"] <- "facilitation-feeding"
intxns12$interaction[intxns12$interaction=="faciliation-feeding"] <- "facilitation-feeding"
intxns12$interaction[intxns12$interaction=="faciltiation"] <- "facilitation"
intxns12$interaction[intxns12$interaction=="hybrization"] <- "hybridization"
intxns12$interaction[intxns12$interaction=="kleptoparasitsim"] <- "kleptoparasitism"
intxns12$interaction[intxns12$interaction=="kleptoparasitsm"] <- "kleptoparasitism"
intxns12$interaction[intxns12$interaction=="kleptoparisitism"] <- "kleptoparasitism"
intxns12$interaction[intxns12$interaction=="kleptoparasitism of nest material"] <- "kleptoparasitism-nest material"

# Checked and all of these are brood parasitism as of Dec 21, 2023 - did not re-check in Dec 2024
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

# If a row is "combined species" make it 0,0
intxns12$effect_sp1_on_sp2[intxns12$interaction == "combined species"] <- 0
intxns12$effect_sp2_on_sp1[intxns12$interaction == "combined species"] <- 0

int.entries<-intxns12 %>% distinct(interaction, effect_sp1_on_sp2, effect_sp2_on_sp1)
arrange(int.entries, by=interaction)

# At least one has competition as +1 - this is not in BBS so not worrying about it now
# 21                    competition                -1                 1
# 22                    competition                 1                -1

# in BBS: commensalism as 0,0; ignore it for now
# 6470	Altamira Oriole	Tyrannus melancholicus	Tropical Kingbird	Icterus gularis	0	0	commensalism

# At least one has brood parasitism as 0,0 - this is not in BBS so not worrying about it now

# One is a NA for brood parasitism but it's for NZ species so ignore for now.
# Which are NA?
intxns12.int.entries.NA<-intxns12[which(is.na(intxns12$effect_sp2_on_sp1)), ]
dim(intxns12.int.entries.NA)
intxns12.int.entries.NA
# One is a NA for brood parasitism but it's for NZ species so ignore for now.

#write.csv(intxns12, file.path(L1_dir,"intxns_types_check.csv"), row.names=F) 
# Check if brood parasitism is coded correctly for Brown Headed Cowbird. Did
# this by filtering out these species in exported csv... for next iteration, do
# this in code. As of Dec. 18, 2023, all are correct. There are a few funny ones
# but they are checked and ok:
# Print out any results that are not "Not Found"
bhcb.sp1 <- intxns12 %>%
  filter(species1_common == "Brown-headed Cowbird")
bncb.sp1.int.entries<-bhcb.sp1 %>% distinct(interaction, effect_sp1_on_sp2, effect_sp2_on_sp1)
arrange(bncb.sp1.int.entries, by=interaction)
# These make sense
bhcb.sp2 <- intxns12 %>%
  filter(species2_common == "Brown-headed Cowbird")
bncb.sp2.int.entries<-bhcb.sp2 %>% distinct(interaction, effect_sp1_on_sp2, effect_sp2_on_sp1)
arrange(bncb.sp2.int.entries, by=interaction)
# These make sense except there are some instances of -1 on BHCB which checks out ok as a rare event.
dplyr::filter(bhcb.sp2, interaction %in% c("brood parasitism") & effect_sp2_on_sp1 == 1)

# Remove the blank entries for interaction type if they exist 
dim(intxns12)
intxns12 <- intxns12 %>% filter(!(interaction==""))
dim(intxns12)
# no blanks exist

save.image(file.path(L1_RData_dir,"AvianInteractionData_L1_post_taxa_cleaning.RData"))

#*******************************#
#### Clean up other columns ####
#*******************************#

##*** nonbreeding season ***##
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

# NEXT TIME: Write code to search and summarize the notes columns for keywords
# like "migrat" "winter" to make sure they occur outside breeding season
write.csv(intxns12,file.path(L1_RData_dir,"intxns12.inprogress.csv"))

##** BOW_evidence ***##
# Remove extra end spaces:
intxns12$BOW_evidence<-trimws(intxns12$BOW_evidence, "r")
# Make all lowercase
intxns12$BOW_evidence<-tolower(intxns12$BOW_evidence)
# Fix typos
intxns12$BOW_evidence[intxns12$BOW_evidence == "1ref"] <- "1 ref"
intxns12$BOW_evidence[intxns12$BOW_evidence == "one ref"] <- "1 ref"
intxns12$BOW_evidence[intxns12$BOW_evidence == "possible"] <- "potential"
intxns12$BOW_evidence[intxns12$BOW_evidence == "stong"] <- "strong"
intxns12$BOW_evidence[intxns12$BOW_evidence == "strong very strong!"] <- "strong"
intxns12$BOW_evidence[intxns12$BOW_evidence == "weak (circumstantial evidence)"] <- "weak"
intxns12$BOW_evidence[intxns12$BOW_evidence == "circumstantial"] <- "weak"
intxns12$BOW_evidence[intxns12$BOW_evidence == "weal"] <- "weak"
intxns12$BOW_evidence[intxns12$BOW_evidence == "weak 1 ref"] <- "weak"
intxns12$BOW_evidence[intxns12$BOW_evidence == "weak-probable"] <- "potential"
intxns12$BOW_evidence[intxns12$BOW_evidence == "indirect evidence"] <- "potential"
intxns12$BOW_evidence[intxns12$BOW_evidence == "suspected"] <- "potential"
intxns12$BOW_evidence[intxns12$BOW_evidence == "few refs"] <- "weak"

sort(unique(intxns12$BOW_evidence))

##*** n_studies ***##
sort(unique(intxns12$n_studies))
# OK. All numbers

##** uncertain_interaction **## - lots here
sort(unique(intxns12$uncertain_interaction))
# 630 entries with some kind of note

## EXPORT the cleaned interaction pairs data:
write.csv(intxns12,file.path(L1_RData_dir,"intxns12.inprogress.csv"))

# Order the data by species1_scientific
intxns12 <- intxns12 %>% relocate(species2_scientific, .after = species1_common)
## Ran for Dec 12, 2024

#*******************************#
## BBS work: 
#*******************************#
intxns12.bbs1<-intxns12
#bbs.splist<-splist
#*******************************#
# Omit the non-BBS rows (rows that do not contain one of the BBS species)
#*******************************#

# Remove rows that do not have an AOU associated with them 
intxns12.bbs<-intxns12.bbs1 %>% 
  filter(!is.na(AOU.sp1) | !is.na(AOU.sp2))
dim(intxns12.bbs)
dim(intxns12.bbs1)
# 22444 rows that have at least one BBS species (Dec 12, 2024)
dim(intxns12.bbs1)-dim(intxns12.bbs)
# 3855 rows that do not contain at least one BBS species (Dec 12, 2024)

# Remove rows with NA value in 'AOU.sp1'
intxns12.bbs.no.na<-intxns12.bbs1 %>%
  filter(!is.na(AOU.sp1))
dim(intxns12.bbs.no.na)
# 21556 remain
# Then, remove rows with NA value in 'AOU.sp2'
intxns12.bbs.no.na<-intxns12.bbs.no.na %>%
  filter(!is.na(AOU.sp2))
dim(intxns12.bbs.no.na)
# 17361 remain
intxns12.bbs<-intxns12.bbs.no.na

int.bbs.noAOU<-intxns12.bbs1 %>% 
  filter(is.na(AOU.sp1) & is.na(AOU.sp2))
sort(unique(int.bbs.noAOU$species1_scientific))
# 436 species not involved in BBS species interactions

## Remove non-breeding season interactions from the BBS subset of data because
# BBS observations are only during breeding season.
table(intxns12.bbs$nonbreedingseason)

# Remove the "yes" for nonbreedingseason (n=2654)
dim(intxns12.bbs)
intxns12.bbs <- intxns12.bbs %>% filter(nonbreedingseason!="yes")
dim(intxns12.bbs)
# 15327 remain

## EXPORT the cleaned interaction pairs data:
write.csv(intxns12.bbs,file.path(L1_RData_dir,"bbs.intxns12.inprogress.csv"))

#*******************************#
# SUBSPECIES: combine into species for our analysis; check AOU.combo
#*******************************#

# Previously, in AvianInteractionData_L0.R we assigned species to:
# AOU.combo.sp1, AOU.combo.sp2
# genus_species.combo.sp1, genus_species.combo.sp2
# Use these combos to analyze data in network analysis.

# Move the useful columns to the left
intxns12.bbs <- intxns12.bbs %>% relocate(AOU.sp1, .after = interaction)
intxns12.bbs <- intxns12.bbs %>% relocate(AOU.sp2, .after = AOU.sp1)
intxns12.bbs <- intxns12.bbs %>% relocate(genus_species.combo.sp1, .after = AOU.sp2)
intxns12.bbs <- intxns12.bbs %>% relocate(genus_species.combo.sp2, .after = genus_species.combo.sp1)
intxns12.bbs <- intxns12.bbs %>% relocate(AOU.combo.sp1, .after = genus_species.combo.sp2)
intxns12.bbs <- intxns12.bbs %>% relocate(AOU.combo.sp2, .after = AOU.combo.sp1)

# Export it!
write.csv(intxns12.bbs,file.path(L1_RData_dir,"AvianInteractionData_L1_BBS.csv"))
save.image(file.path(L1_RData_dir,"AvianInteractionData_L1_post_taxa_cleaning.RData"))
