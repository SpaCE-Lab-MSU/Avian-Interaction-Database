# TITLE:          Avian Interaction Pairs Data: L1 post-taxa cleaning 
# AUTHORS:        Phoebe Zarnetske
# COLLABORATORS:  Vincent Miele, Stephane Dray, Emily Parker
# DATA INPUT:     int.namefix.bbs <- load(file.path(L1_dir,"int.namefix.bbs.RData"))
#
# DATA OUTPUT:    L1 data: AvianInteractionData_L1.csv
#                 L1 data: AvianInteractionData_L1_BBS.csv for BBS analysis
#                 L1 data: bbs_splist_2024_L1.csv for BBS analysis (species name changes)
# PROJECT:        Avian Interaction Database 
# DATE:           27 Oct 2022; updated through 9 Dec 2024  
# NOTES:          Next script to run: 
#                 This script is used to refine species name changes to align 
#                 with BOW (Clements & eBird checklist), 
#                 and to create AvianInteractionData_L1.csv.
#                 ** Currently, the script is working for BBS species only; needs 
#                           updating for the remainder.
#                 L0 data are checked to assign updated scientific and common names 
#                 to the interaction pairs data.
#                 
#                 Makes a new column that also includes scientific name 
#                 changes associated with the AOUcombo.index for merging with 
#                 those names in the AvianInteractionData_AOUindex_L1.csv
# 
#               
# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)
#library(taxize)
#library(taxadb)
library(dplyr)
library(stringr)
library(stringdist)

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
load("~/Documents/GitHub/Avian-Interaction-Database/L1/int.namefix.bbs.RData")

#*******************************************#
#### Scientific Name Checking: Data Prep ####
#*******************************************#

intxns12<-int.namefix.bbs

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
intxns12$interaction[intxns12$interaction=="amensalism"] <- "amensalism"
intxns12$interaction[intxns12$interaction=="amenslism"] <- "amensalism"
intxns12$interaction[intxns12$interaction=="amenalism"] <- "amensalism"
intxns12$interaction[intxns12$interaction=="brood"] <- "brood parasitism"
intxns12$interaction[intxns12$interaction=="brood-parasitism"] <- "brood parasitism"
intxns12$interaction[intxns12$interaction=="brood parasitsm"] <- "brood parasitism"
intxns12$interaction[intxns12$interaction=="call mimicking"] <- "call mimicry"
intxns12$interaction[intxns12$interaction=="call mimickry"] <- "call mimicry"
intxns12$interaction[intxns12$interaction=="comensalism"] <- "commensalism"
intxns12$interaction[intxns12$interaction=="commenalism"] <- "commensalism"
intxns12$interaction[intxns12$interaction=="commesalism"] <- "commensalism"
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
intxns12$interaction[intxns12$interaction=="faciltiation"] <- "facilitation"
intxns12$interaction[intxns12$interaction=="hybrization"] <- "hybridization"
intxns12$interaction[intxns12$interaction=="kleptoparasitsim"] <- "kleptoparasitism"
intxns12$interaction[intxns12$interaction=="kleptoparasitsm"] <- "kleptoparasitism"
intxns12$interaction[intxns12$interaction=="kleptoparisitism"] <- "kleptoparasitism"
intxns12$interaction[intxns12$interaction=="kleptoparasitism of nest material"] <- "kleptoparasitism-nest material"

# Checked and all of these are brood parasitism as of Dec 21, 2023 - did not re-check in Aug 2024
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
## STOPPED HERE Dec 12 AM
# One is a NA for brood parasitism but it's for NZ species so ignore for now.

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

save.image(file.path(L1_dir,"AvianInteractionData_L1_post_taxa_cleaning.RData"))




