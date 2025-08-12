# TITLE:          L1 Canada & Conterminous US Species Checklist Data: 
#                 Reads in avibase_ca.conus_splist_2024_L0.csv (created in
#                 R/L0/AvianInteractionData_specieslists_L0.R) = CA-CONUS List,
#                 which is the AviBase Canada list + AviBase Lower 48 US + 
#                 AviBase Alaska list (taxonomy from Clements 2024), 
#                 so it can be used in the North American Avian Interaction data 
#                 paper and the avian-meta-network paper. 
# AUTHORS:        Phoebe Zarnetske
# COLLABORATORS:  Vincent Miele, Stephane Dray, Emily Parker, Pat Bills
# DATA INPUT:     L0 data: avibase_ca.conus_splist_2024_L0.csv 
#                       from AvianInteractionData_specieslists_L0.R
# DATA OUTPUT:    L1 data: avibase_ca.conus_splist_2024_L1.csv 
# PROJECT:        Avian Interaction Database & avian-meta-network
# DATE:           17 January 2022 - 8 Aug 2025
#                 
#                 Next script to run: AvianInteractionData_L1.R

# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)
library(readr)

# Local directories
L0_dir <- "/Users/plz/Documents/GitHub/Avian-Interaction-Database-Working/L0/species_checklists"
L1_dir <- "/Users/plz/Documents/GitHub/Avian-Interaction-Database-Working/L1"

# Read in species list: all species in Canada and CONUS (with Clements 2024 taxonomy)
ca.conus<-read.csv(file.path(L0_dir,"avibase_ca.conus_splist_2024_L0.csv"), fileEncoding="UTF-8")
#ca.conus <- read_csv("L0/species_checklists/avibase_ca.conus_splist_2024_L0.csv")

# Name columns for merging later
ca.conus$list<-"AviBase Canada & CONUS"

# Official 2024 eBird & Clements checklist
#for some reason this isn't working:
clements2024<-read.csv(file.path(L0_dir,"eBird-Clements-v2024-integrated-checklist-October-2024-rev.csv"))

clements2024<-read_csv("L0/species_checklists/eBird-Clements-v2024-integrated-checklist-October-2024-rev.csv")
# Clements List: Name the columns for merging later
clements2024$scientific_name<-clements2024$scientific.name
clements2024$common_name<-clements2024$English.name
clements2024<-subset(clements2024, select=c("scientific_name","common_name"))

clements2024$list<-"Clements-eBird2024"

## Read in the Google Sheet exported CSV to check how this relates to our list 
## from the data entry lookup GSheet. 
dataentry.list<-read.csv(file.path(L0_dir,"AvianInteractionData_SpeciesList_dataentry_11Aug2025.csv"))

# Name some columns for merging later
dataentry.list$list<-"Data Entry Aug 2025"

dataentry.list$scientific_name<-dataentry.list$Spanish_Common_Name
dataentry.list$common_name<-dataentry.list$English_Common_Name
# Omit rows that do not include CA or US:
unique(dataentry.list$Region)
# Keep only the rows with "CONUS & Canada", and "Europe & North America & Greenland"
dataentry.list1<-dataentry.list[dataentry.list$Region %in%c("CONUS & Canada","Europe & North America & Greenland"), ]
dataentry.list1<-subset(dataentry.list1,select=c("scientific_name","common_name","list",
                                                 "Region","InteractionPairs_entry",
                                                 "ORDER","Family","BBS_AOU","notes"))
# Merge the lists to see the mismatches 
merged_lists <- full_join(dataentry.list1,ca.conus, 
                          by = c("scientific_name","common_name","list")) %>%
  full_join(clements2024, by = c("scientific_name","common_name","list"))

unique(merged_lists$list) # All 3 lists

# Filter out rows where ALL specified columns are NA for Clements 
# Columns to check for all NAs
cols_to_check <- c("Region","InteractionPairs_entry","ORDER","Family",
                   "BBS_AOU","notes","status","order","family","region") 
merged_lists1 <- merged_lists %>%
  filter(!if_all(all_of(cols_to_check), is.na))

unique(merged_lists1$list)
# The list contains no Clements 2024 list; this means the AviBase is complete 
# and up to date with Clements

# Filter out rows that contain the same scientific AND common names
# Filter rows with duplicates in col1 and col2, and a mismatch in col3
matched_lists <- merged_lists1 %>%
  group_by(scientific_name, common_name) %>%
  filter(n() > 1 & length(unique(list)) > 1) %>%
  ungroup()

mismatch <- merged_lists1 %>%
  group_by(scientific_name, common_name) %>%
  filter(n_distinct(list) == 1 | n() == 1) %>% # Keep if list is unique within the group OR if there's only one row in the group (no duplicates)
  ungroup()

# Export to inspect the mismatches; most will be same common name or scientific name change
write.csv(mismatch,file.path(L0_dir,"ca.conus.mismatches.csv"), row.names=F)

# Subset the rows that have Rare species
rare <- merged_lists1 %>%
  filter(grepl("Rare", status))
# Merge that back with just the dataentry.list to make sure that if we drop 
# these, they won't omit some species we've included previously
dataentry.rare<-inner_join(dataentry.list1,rare, 
                            by = c("common_name"))
# That leaves 48 species that would be omitted if we were to omit Rare species.  

# Subset the rows that have duplicate scientific_name.
duplicate_sci <- merged_lists[duplicated(merged_lists$scientific_name) | 
                                      duplicated(merged_lists$scientific_name, fromLast = TRUE), ]
duplicate_sci <- merged_lists[!complete.cases(merged_lists), ]



# Move the useful columns to the left
splist <- splist %>% relocate(genus_species, .after = AOU)
splist <- splist %>% relocate(AOU.combo, .after = genus_species)
splist <- splist %>% relocate(genus_species.combo, .after = AOU.combo)

# Export the cleaned data. 
write.csv(splist,file.path(L1_dir,"bbs_splist_2024_L1.csv"), row.names=F)

# Next script to run if combining with bbs_obs data: AvianInteractionData_L1.R.
# In that script, the Genus species associated with the AOU.combo will be
# assigned in 2 new columns (one for species1 and another for species2).

