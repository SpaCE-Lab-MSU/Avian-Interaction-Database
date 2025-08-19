# TITLE:          L1/AvianInteractionData_L1_post_taxa_cleaning_psb.R Avian Interaction Pairs Data: L1 post-taxa cleaning from
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

##### RE-ORGANIZATION
# 1) testing
#    data checks; functions to make sure a column's values are the correct type.  report if not
# 2) reporting
#    state of the data, number of of rows, # of unique values in a column current list of unique values in a column
#    issues/mismatches not found by simple rules in #1
#   report of which data needs to be changed.   Then change the data, commit to gig, and re-run
# 3) transform
#    functions to make changes, corrections, adds etc
#    these are necessary changes that can't be made in the actual data files
#    assign categories, don't assign corrections - correct the data



#Load packages
library(tidyverse)
library(dplyr)
library(stringr)

# .Renviron not working for PLZ; hard-coding in here
L0_dir <- here::here("L0")
L1_dir <- here::here("L1")
R_dir <- here::here('R/v2')
L1_RData_dir <- "~/Google Drive/Shared drives/Avian_MetaNetwork/data/L1/bbs_intxns"

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

BOW_evidence_corrections.df <- data.frame(
  incorrect = c(
      "1ref"
    , "one ref"
    , "possible"
    , "stong"
    , "strong very strong!"
    , "weak (circumstantial evidence)"
    , "circumstantial"
    , "weal"
    , "weak 1 ref"
    , "weak-probable"
    , "indirect evidence"
    , "suspected"
    , "few refs"
),
  correct = c(
    "1 ref"
    , "1 ref"
    , "potential"
    , "strong"
    , "strong"
    , "weak"
    , "weak"
    , "weak"
    , "weak"
    , "potential"
    , "potential"
    , "potential"
    , "weak"
  )
)

intxns12$BOW_evidence <- standardize_text_column(intxns12$BOW_evidence, BOW_evidence_corrections.df )



# # Remove extra end spaces:
# intxns12$BOW_evidence<-trimws(intxns12$BOW_evidence, "r")
# # Make all lowercase
# intxns12$BOW_evidence<-tolower(intxns12$BOW_evidence)
# # Fix typos
# intxns12$BOW_evidence[intxns12$BOW_evidence == "1ref"] <- "1 ref"
# intxns12$BOW_evidence[intxns12$BOW_evidence == "one ref"] <- "1 ref"
# intxns12$BOW_evidence[intxns12$BOW_evidence == "possible"] <- "potential"
# intxns12$BOW_evidence[intxns12$BOW_evidence == "stong"] <- "strong"
# intxns12$BOW_evidence[intxns12$BOW_evidence == "strong very strong!"] <- "strong"
# intxns12$BOW_evidence[intxns12$BOW_evidence == "weak (circumstantial evidence)"] <- "weak"
# intxns12$BOW_evidence[intxns12$BOW_evidence == "circumstantial"] <- "weak"
# intxns12$BOW_evidence[intxns12$BOW_evidence == "weal"] <- "weak"
# intxns12$BOW_evidence[intxns12$BOW_evidence == "weak 1 ref"] <- "weak"
# intxns12$BOW_evidence[intxns12$BOW_evidence == "weak-probable"] <- "potential"
# intxns12$BOW_evidence[intxns12$BOW_evidence == "indirect evidence"] <- "potential"
# intxns12$BOW_evidence[intxns12$BOW_evidence == "suspected"] <- "potential"
# intxns12$BOW_evidence[intxns12$BOW_evidence == "few refs"] <- "weak"

## REPORT sort(unique(intxns12$BOW_evidence))

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
