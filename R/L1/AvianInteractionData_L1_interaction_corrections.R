# TITLE:          AvianInteractionData_L1_interaction_corrections :
#           corrections to interaction encoding
# AUTHORS:        Phoebe Zarnetske, Pat Bills
# COLLABORATORS:  Vincent Miele, Stephane Dray, Emily Parker
# DATA INPUT:     file with interaction data, with taxa corrected or not, that has interaction columns
#                 this currently is  int.namefix.bbs <- load(file.path(L1_dir,"int.namefix.bbs.RData"))
#
# DATA OUTPUT:    data frame with corrected interaction fields
#                 # previously, L1 data: AvianInteractionData_L1_BBS.csv for BBS analysis
#                 # previously, L1 data: bbs_splist_2024_L1.csv for BBS analysis (species name changes)
# PROJECT:        Avian Interaction Database
# DATE:           27 Oct 2022; updated through 12 Dec 2024
# NOTES:          Next script to run:
#                 code in this script is used to clean up interaction columns only as part
#                 of a workflow file (notebook or other script)
#                 ** Currently, the script is tailored to work for BBS species only;
#
#                 Updates a new column that also includes scientific name
#                 changes associated with the AOUcombo.index for merging with
#                 those names in the AvianInteractionData_AOUindex_L1.csv
#

#Load packages
require(tidyverse)
library(dplyr)
library(stringr)

# dir setup
L0_dir <- here::here("L0")
L1_dir <- here::here("L1")
R_dir <- here::here('R/v2')

# this file currently has a wide variety of functions
source(file.path(R_dir, 'AvianInteractionData_functions.R'))

#*******************************#
#### Editing the interaction columns to standardize names ####
#*******************************#
# FOR NOTEBOOK - SHOW INTERACTION TYPES
# Unique interaction types:
# sort(unique(interactions))


####### INTERACTIONS


correct_interactions <- function(intxns) {
  # read in file use to store corrections to interactions column, and use it to standardize
  interaction_corrections.df <- read_csv(corrections_file,"R/L0/text_corrections.csv")

  intxns$interaction <- standardize_text_column(intxns$interaction, interaction_corrections.df )


  ### NOTEBOOK
  # Remove the blank entries for interaction type if they exist
  #dim(intxns)
  intxns <- intxns %>% filter(!(interaction==""))
  # dim(intxns)
  # no blanks exist


  # FOR NOTEBOOK - SHOW INTERACTION TYPES
  # Unique interaction types:
  # sort(unique(interactions))


  # Ignore these interactions for now:
  # "combined species"
  # "copulation?" - for 2 swallows

  ############ CODINGS

  ##### THESE CODINGS SHOULD BE CORRECTED IN THE DATA FILES IF THEY ARE STANDARD
  ### CAN CREATE A CHECK PROGRAM THAT LOOKS FOR THESE

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
  # dim(intxns12.int.entries.NA)
  # intxns12.int.entries.NA
  # One is a NA for brood parasitism but it's for NZ species so ignore for now.

  return(intxns12)

}


# # main workflow
main_intxn_correct<- function(output_file_name ="intxns_types_check.csv" ){
  # # Read in the current version of the database
  # ultimately this would be flexible so we can use different files
  ## this loads a data frame intxns12
  load(file.path(L1_RData_dir,"int.namefix.bbs.RData"))
  intxns_corrected<- correct_interactions(intxns12)
  # from L0 functions
  output_file_path <- write_data_file(intxns_corrected, file.path(L1_dir,output_file_name), row.names=F)
  # ultimately remove this
  save.image(file.path(L1_RData_dir,"AvianInteractionData_L1_post_taxa_cleaning.RData"))

  return(output_file_path)
}



#### BROWN HEADED COWBIRD
## move to notebook to check this data and then add the fix in the function above
bhcb<-function(intxns){
  # Check if brood parasitism is coded correctly for Brown Headed Cowbird. Did
  # this by filtering out these species in exported csv... for next iteration, do
  # this in code. As of Dec. 18, 2023, all are correct. There are a few funny ones
  # but they are checked and ok:
  # Print out any results that are not "Not Found"
  bhcb.sp1 <- intxns %>%
    filter(species1_common == "Brown-headed Cowbird")
  bncb.sp1.int.entries<-bhcb.sp1 %>% distinct(interaction, effect_sp1_on_sp2, effect_sp2_on_sp1)
  arrange(bncb.sp1.int.entries, by=interaction)
  # These make sense
  bhcb.sp2 <- intxns %>%
    filter(species2_common == "Brown-headed Cowbird")
  bncb.sp2.int.entries<-bhcb.sp2 %>% distinct(interaction, effect_sp1_on_sp2, effect_sp2_on_sp1)
  arrange(bncb.sp2.int.entries, by=interaction)
  # These make sense except there are some instances of -1 on BHCB which checks out ok as a rare event.
  dplyr::filter(bhcb.sp2, interaction %in% c("brood parasitism") & effect_sp2_on_sp1 == 1)

}
