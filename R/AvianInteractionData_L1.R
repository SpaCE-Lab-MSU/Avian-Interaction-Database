# TITLE:          Avian Interaction Pairs Data: L0 to L1, including an option to 
#                   subset species1 for: (1) only BBS species, or 
#                                       (2) BBS & other North America species
# AUTHORS:        Phoebe Zarnetske, Pat Bills
# COLLABORATORS:  Vincent Miele, Stephane Dray, Emily Parker
# DATA INPUT:     From AvianInteractionData_L0_stitch.R: Data imported as csv 
#                   https://github.com/SpaCE-Lab-MSU/Avian-Interaction-Database
#                   /blob/main/L0/AvianInteractionData_L0.csv
#                 From bbs_specieslist_L1.R: Data imported as csv 
#                   https://github.com/SpaCE-Lab-MSU/Avian-Interaction-Database
#                   /blob/main/L1/bbs_splist_2024_L1.csv
#                 For BBS observation subset: bbs_allobs_runtype1 produced in 
#                   https://github.com/SpaCE-Lab-MSU/avian-meta-network
#                   /blob/main/R/L1/bbs_obs_L1.R: 
#                   Data imported as csv: /Google Drive/Shared drives/
#                   Avian_MetaNetwork/data/L1/bbs_obs/bbs_allobs_runtype1.csv
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
library(taxadb)
library(dplyr)
library(stringr)
library(stringdist)

# .Renviron not working for PLZ; hard-coding in here
L0_dir <- "/Users/plz/Documents/GitHub/Avian-Interaction-Database/L0"
#L0_dir <- "/Users/phoebezarnetske/Documents/GitHub/Avian-Interaction-Database/L0"

L1_dir <- "/Users/plz/Documents/GitHub/Avian-Interaction-Database/L1"
#L1_dir <- "/Users/phoebezarnetske/Documents/GitHub/Avian-Interaction-Database/L1"

L1_RData_dir <- "~/Google Drive/Shared drives/Avian_MetaNetwork/data/L1"

# Read in csv with avian interactions from primary, secondary cavity nesting
# birds in North America.
int.raw<-read.csv(file.path(L0_dir,"AvianInteractionData_L0.csv"))

# Read in species list: all species in BBS (the 2024 release which includes all
# species as of 2023, plus the additional AOUcombo.index column for use w BBS
# index)
splist2024<-read.csv(file.path(L1_dir,"bbs_splist_2024_L1.csv"))
# Rename some columns and omit others; indicate that the common name is coming
# from BBS Species List
names(splist2024)[names(splist2024) == "English_Common_Name"] <-"bbs_common_name"

# Read in our look-up table with the different bbs & bow & old names for species
#namechg<-read.csv(file.path(L0_dir,"bbsbow_names.csv"))

# Read in the official eBird/Clements checklist 2024 version
checklist <- read_csv(file.path(L0_dir,"eBird-Clements-v2024-integrated-checklist-October-2024-rev.csv"))

# We are using the GBIF list (via taxadb) to resolve scientific names and the
# eBird/Clements checklist via Birds of The World naming conventions for
# species. Some BBS names differ from these lists so they have to be resolved
# manually.

#*******************************************#
#### Scientific Name Checking: Data Prep ####
#*******************************************#
# Rename the columns for merging later
names(checklist)[names(checklist) == "scientific name"] <-"genus_species"
names(checklist)[names(checklist) == "English name"] <-"common_name"

# Reference the bbsbow_names data to make initial changes to any 
# "other_or_old_bow" names that might appear.
# Apply changes only to the species1_scientific and species2_scientific columns.
# First omit any rows with a blank in "other_or_old_bow"
#dim(namechg)
#namechg.orig <- namechg
#namechg<-namechg[!(is.na(namechg$other_or_old_bow) | namechg$other_or_old_bow==""), ]
#dim(namechg)

# Minor cleaning on int.raw: remove rows with NA in both columns of scientific names
int.raw<-int.raw[!with(int.raw,is.na(species1_scientific)& is.na(species2_scientific)),]

# Fix a few rows that have one or the other scientific name missing
int.raw[is.na(int.raw$species1_scientific),]
#       species1_common                       species2_common species1_scientific
# 10655 Rhinoceros Auklet hybrid Western x Glaucous-winged Gull                <NA>
#   species2_scientific effect_sp1_on_sp2 effect_sp2_on_sp1
# 10655 Larus occidentalis x glaucescens                 1                 1
# interaction BOW_evidence n_studies
# 10655 facilitation-mixed flocking       strong         2

# CHECKLIST Rhinoceros Auklet = Cerorhinca monocerata
subset(checklist, common_name == "Rhinoceros Auklet")
int.raw$species1_scientific[
  with(
    int.raw,
    (species1_common == "Rhinoceros Auklet") &
      (species2_common == "hybrid Western x Glaucous-winged Gull") &
      (species2_scientific == "Larus occidentalis x glaucescens")
  )
] <- "Cerorhinca monocerata"
int.raw[is.na(int.raw$species1_scientific),] # Fixed

int.raw[is.na(int.raw$species2_scientific),]
# 2 rows need editing for adding species names: 
# species1_common species2_common         species1_scientific
# 866         White-cheeked Pintail   Cinnamon Teal Anas bahamensis rubirostris
# 14223 Middle American Screech-Owl            <NA>        Megascops guatamalae
# species2_scientific effect_sp1_on_sp2 effect_sp2_on_sp1          interaction
# 866                  <NA>                 1                 1 facilitation-feeding
# 14223                <NA>                NA                NA                 <NA>
# 2nd one has no interactions entered; delete it

# CHECKLIST Cinnamon Teal=Spatula cyanoptera
subset(checklist, common_name == "Cinnamon Teal")

int.raw$species2_scientific[
  with(
    int.raw,
    (species1_common == "White-cheeked Pintail") &
      (species2_common == "Cinnamon Teal") &
      (species1_scientific == "Anas bahamensis rubirostris")
  )
] <- "Spatula cyanoptera"

int.raw[is.na(int.raw$species2_scientific),] # Fixed; now delete the last row without data
dim(int.raw) #26300
int.raw<-int.raw[!with(int.raw,is.na(species2_scientific)),]
dim(int.raw) #26299

#*** Create EDITED names and assign RAW names***#
# Determine how many unique entries are in int.raw$species1_scientific and
# int.raw$species2_scientific

unique.sp.raw <- data.frame(genus_species.raw = with(int.raw, union(
  species1_scientific, species2_scientific
)))

dim(unique.sp.raw) #4333
# 4333 unique genus_species.raw entries

# Create unique genus_species.raw and common_name.raw pairs with formatting
# adjustments. Also create genus_species.edit which is cleaned up
# genus_species.raw: removes blank spaces, capitalizes, and keeps unique rows.

int.raw.names <- int.raw %>%
  # Select and stack the relevant species columns
  select(species1_scientific, species1_common, species2_scientific, species2_common) %>%
  transmute(
    genus_species.raw = species1_scientific,  # Original genus_species
    common_name.raw = species1_common
  ) %>%
  bind_rows(
    int.raw %>%
      transmute(
        genus_species.raw = species2_scientific,  # Original genus_species
        common_name.raw = species2_common
      )
  ) %>%
  # Remove duplicates and clean up formatting
  distinct() %>%
  mutate(
    # Clean genus_species while keeping the raw version
    genus_species.edit = str_trim(genus_species.raw),  # Start with the raw data
    genus_species.edit = ifelse(
      str_starts(genus_species.edit, "unid\\."),  # Exception case
      str_replace(genus_species.edit, "(unid\\.)\\s*(\\w+)", "\\1 \\U\\2"),
      str_to_sentence(genus_species.edit)       # Regular case
    ),
    genus_species.edit = str_replace(genus_species.edit, "\\bspp\\.\\b", "sp."),  # Replace "spp." with "sp."
    
    # Clean common_name
    common_name.edit = str_trim(common_name.raw),  # Start with the raw data
    common_name.edit = str_to_title(common_name.edit),  # Capitalize each word
    common_name.edit = str_replace_all(common_name.edit, "\\bunid\\b", "unid.")  # Add period to "unid"
  ) %>%
  # Remove rows where genus_species.edit is <NA>
  filter(!is.na(genus_species.edit))

# Display the resulting cleaned dataframe of the species from interaction data
int.raw[1:30,1:4]
int.raw.names[1:10,]
length(unique(int.raw.names$genus_species.raw))
# 4333 - OK

# Test case to track: Accipiter cooperi is misspelled - exists in int.raw.names
dplyr::filter(int.raw.names, genus_species.raw %in% c("Accipiter cooperi"))
# genus_species.raw common_name.raw genus_species.edit common_name.edit
# 1 Accipiter cooperi   Cooper's Hawk  Accipiter cooperi    Cooper's Hawk

# Note: Accipiters are off (these are original raw data entries without taxonomic change)
# genus_species.raw      common_name       genus_species
# 1    Acanthis flammea   Common Redpoll    Acanthis flammea
# 2 Acanthis hornemanni    Hoary Redpoll Acanthis hornemanni
# 3  Accipiter cooperii    Cooper's Hawk  Accipiter cooperii
# 4  Accipiter gentilis Northern Goshawk  Accipiter gentilis
# 5  Accipiter gentilis Northern Goshawk  Accipiter gentilis
# 6  Accipiter gentilis          Goshawk  Accipiter gentilis

#**************************************************#
#### Scientific Name Checking: taxadb with GBIF ####
#**************************************************#

# taxadb package: Modified from this code: https://docs.ropensci.org/taxadb/articles/intro.html
# In previous code, tried ITIS and COL to see if they were better than GBIF.
# GBIF has the fewest NA values, so we are sticking with it (it is the most comprehensive).

# Create a local GBIF database 
td_create("gbif")

# Resolve scientific names for the scientific names, based on genus_species.edit
# using GBIF (last run Dec. 9, 2024)
int.gbif.names <- int.raw.names %>%
  mutate(
    scientific_id = get_ids(genus_species.edit, "gbif"),
    accepted_scientific_name = get_names(scientific_id, "gbif")
  ) 

# These 12 species have >1 identifier and need to be resolved w BOW info:
options(tibble.width = 200) # allow tibble columns to not be truncated

# KEEP ORIGINAL- Anser anser anser - European Graylag Goose; checklist common_name: Graylag Goose (European)
# Common name change will be fixed later, below, with merging. 
int.raw[which(int.raw$species1_scientific == "Anser anser anser"), ]
int.raw[which(int.raw$species2_scientific == "Anser anser anser"), ]
checklist[which(checklist$genus_species == "Anser anser anser"), ]

# KEEP ORIGINAL - Eudynamys orientalis - Pacific Koel; same as checklist common_name
head(int.raw[which(int.raw$species1_scientific == "Eudynamys orientalis"), ])
head(int.raw[which(int.raw$species2_scientific == "Eudynamys orientalis"), ])
checklist[which(checklist$genus_species == "Eudynamys orientalis"), ]

# KEEP ORIGINAL - Larus fuscus fuscus - Lesser Black-backed Gull 
# checklist common_name: Lesser Black-backed Gull (fuscus) 	
head(int.raw[which(int.raw$species1_scientific == "Larus fuscus fuscus"), ])
head(int.raw[which(int.raw$species2_scientific == "Larus fuscus fuscus"), ])
checklist[which(checklist$genus_species == "Larus fuscus fuscus"), ]

# KEEP ORIGINAL - Psittacula eques - Echo Parakeet
# checklist common_name: Echo Parakeet
head(int.raw[which(int.raw$species1_scientific == "Psittacula eques"), ])
head(int.raw[which(int.raw$species2_scientific == "Psittacula eques"), ])
checklist[which(checklist$genus_species == "Psittacula eques"), ]

# KEEP ORIGINAL - Anas poecilorhyncha - Indian Spot-billed Duck
# checklist common_name: Indian Spot-billed Duck
head(int.raw[which(int.raw$species1_scientific == "Anas poecilorhyncha"), ])
head(int.raw[which(int.raw$species2_scientific == "Anas poecilorhyncha"), ])
checklist[which(checklist$genus_species == "Anas poecilorhyncha"), ]

# KEEP ORIGINAL - Batis molitor - Chinspot Batis
# checklist common_name: Chinspot Batis
head(int.raw[which(int.raw$species1_scientific == "Batis molitor"), ])
head(int.raw[which(int.raw$species2_scientific == "Batis molitor"), ])
checklist[which(checklist$genus_species == "Batis molitor"), ]

# KEEP ORIGINAL - Heteromyias armiti - Black-capped Robin
# checklist common_name: Black-capped Robin
dplyr::filter(int.raw, species1_scientific %in% c("Heteromyias armiti"))
dplyr::filter(int.raw, species2_scientific %in% c("Heteromyias armiti"))
checklist[which(checklist$genus_species == "Heteromyias armiti"), ]

# KEEP ORIGINAL - Chloris sinica - Oriental Greenfinch
# checklist common_name: Oriental Greenfinch
dplyr::filter(int.raw, species1_scientific %in% c("Chloris sinica"))
dplyr::filter(int.raw, species2_scientific %in% c("Chloris sinica"))
checklist[which(checklist$genus_species == "Chloris sinica"), ]

# KEEP ORIGINAL - Passer cinnamomeus - Russet Sparrow
# checklist common_name: Russet Sparrow
dplyr::filter(int.raw, species1_scientific %in% c("Passer cinnamomeus"))
dplyr::filter(int.raw, species2_scientific %in% c("Passer cinnamomeus"))
checklist[which(checklist$genus_species == "Passer cinnamomeus"), ]

# CHANGE TO NEW NAME below: Dendroica pinus - Pine Warbler 
# checklist common_name: Pine Warbler; genus_species = Setophaga pinus
dplyr::filter(int.raw, species1_scientific %in% c("Dendroica pinus"))
dplyr::filter(int.raw, species2_scientific %in% c("Dendroica pinus"))
checklist[which(checklist$genus_species == "Dendroica pinus"), ]
checklist[which(checklist$genus_species == "Setophaga pinus"), ]

# KEEP ORIGINAL - Turdus nudigenis - Spectacled Thrush
# checklist common_name: Spectacled Thrush
head(int.raw[which(int.raw$species1_scientific == "Turdus nudigenis"), ])
head(int.raw[which(int.raw$species2_scientific == "Turdus nudigenis"), ])
checklist[which(checklist$genus_species == "Turdus nudigenis"), ]

# KEEP ORIGINAL - Anas flavirostris - Yellow-billed Teal
# checklist common_name: Yellow-billed Teal
head(int.raw[which(int.raw$species1_scientific == "Anas flavirostris"), ])
head(int.raw[which(int.raw$species2_scientific == "Anas flavirostris"), ])
checklist[which(checklist$genus_species == "Anas flavirostris"), ]

#************************************************************#
#### Scientific Name Changes: Resolved & Unresolved Names ####
#************************************************************#

dim(int.gbif.names)
# 6065
length(unique(int.gbif.names$genus_species.raw))
# 4333 OK       

# Separate resolved and unresolved names based on specified criteria
resolved.gbif <- int.gbif.names %>%
  filter(!is.na(scientific_id) | grepl(" sp\\.$", genus_species.edit))
length(unique(resolved.gbif$genus_species.raw))
# 4000

unresolved.gbif <- int.gbif.names %>%
  filter(is.na(scientific_id) & !grepl(" sp\\.$", genus_species.edit))
length(unique(unresolved.gbif$genus_species.raw))
# 333
length(unique(resolved.gbif$genus_species.raw))+length(unique(unresolved.gbif$genus_species.raw))
# 4333 - OK

# Display both results: GBIF resolved and GBIF unresolved 
head(resolved.gbif)
# Test: Accipiter cooperi is misspelled
dplyr::filter(resolved.gbif, genus_species.raw %in% c("Accipiter cooperi"))
# doesn't exist

head(unresolved.gbif)
# Test: Accipiter cooperi is misspelled - exists in unresolved.gbif
dplyr::filter(unresolved.gbif, genus_species.raw %in% c("Accipiter cooperi"))
# genus_species.raw common_name.raw genus_species.edit common_name.edit scientific_id
# 1 Accipiter cooperi   Cooper's Hawk  Accipiter cooperi    Cooper's Hawk          <NA>
#   accepted_scientific_name
# 1                     <NA>
# Omit scientific_id and accepted_scientific_name since they are blank
unresolved.gbif$scientific_id<-NULL
unresolved.gbif$accepted_scientific_name<-NULL

# Work with unresolved.gbif to try and determine what misspellings exist in the
# genus_species.raw, and what they should be, based on the reference list from
# eBird & Clements CHECKLIST 2024.

# Reference list of scientific names from eBird Clements CHECKLIST 2024
reference_names <- tibble(
  genus_species = checklist$genus_species,
  common_name = checklist$common_name
)

# Function to clean names for comparison (ignore sp., Unid., remove whitespace
# after name)
clean_name <- function(name) {
  name %>%
    gsub("\\b(unid\\.|sp\\.)\\b", "", .) %>%   # Remove "Unid." and "sp."
    trimws()                                   # Trim extra spaces
}

# Use fuzzy logic function to find closest match from the CHECKLIST reference list
# Function to find the closest match with a similarity score, and ignoring the
# name aspects above.
find_closest_match_with_score <- function(name, reference_list) {
  if (is.na(name) || name == "") {
    return(list(match = NA_character_, score = NA_real_))
  }
  name_cleaned <- clean_name(name)
  reference_cleaned <- clean_name(reference_list) # Cleaned for comparison only
  # Calculate string distances
  distances <- stringdist::stringdist(name_cleaned, reference_cleaned, method = "jw") # Jaro-Winkler distance
  if (length(distances) == 0 || all(is.na(distances)) || min(distances, na.rm = TRUE) > 0.5) {
    return(list(match = NA_character_, score = NA_real_))
  }
  closest_match_index <- which.min(distances)
  closest_match <- reference_list[closest_match_index]
  similarity_score <- 1 - distances[closest_match_index]
  return(list(match = closest_match, score = similarity_score)) # Convert distance to similarity (1 = exact match)
}

# Resolve matches based on genus_species.edit and CHECKLIST
genus_species_matches <- unresolved.gbif %>%
  rowwise() %>%
  mutate(
    closest_genus_species_match = find_closest_match_with_score(genus_species.edit, checklist$genus_species)$match,
    genus_species_match_score = find_closest_match_with_score(genus_species.edit, checklist$genus_species)$score
  ) %>%
  ungroup()

# Test: Accipiter cooperi is misspelled
dplyr::filter(genus_species_matches, genus_species.raw %in% c("Accipiter cooperi"))
# # A tibble: 1 × 6
# genus_species.raw common_name.raw genus_species.edit common_name.edit closest_genus_species_match
# <chr>             <chr>           <chr>              <chr>            <chr>                      
#   1 Accipiter cooperi Cooper's Hawk   Accipiter cooperi  Cooper's Hawk    Accipiter poliogaster      
# genus_species_match_score
# <dbl>
#   1                     0.849

# Adjust so that columns are grouped close by for easier reference
# final_matches <- final_matches %>%
#   select(
#     genus_species, closest_genus_species_match,
#     common_name, closest_common_name_match,
#     everything()  # Keep the remaining columns in their original order
#   )

#************************************************************#
#### Scientific Name Changes: High & Low Confidence Matches ####
#************************************************************#
length(unique(genus_species_matches$genus_species.raw))
# 333 OK because it matches the unresolved.gbif number

# Extract final_matches with high confidence (genus_species_match_score > 0.90)
high_confidence_matches <- genus_species_matches %>%
  filter(
    genus_species_match_score >= 0.9 
  )
length(unique(high_confidence_matches$genus_species.raw))
# 262 out of 333
# Define range for printing in increments of 0.005; use the genus_species match
score_start <- min(high_confidence_matches$genus_species_match_score)
score_end <- max(high_confidence_matches$genus_species_match_score)
increment <- 0.005

# Loop through each score range and print the matches within that range
for (i in seq(score_start, score_end, by = increment)) {
  current_range <- high_confidence_matches %>%
    filter(genus_species_match_score >= i & genus_species_match_score <= i + increment)
  
  # Print the current range if it has any entries
  if (nrow(current_range) > 0) {
    cat("\nMatch Score Range:", sprintf("%.2f", i), "to", sprintf("%.2f", i + increment), "\n")
    print(current_range, n=100) # print up to 100 rows in a section
  }
}

# For most of the names, the match is 1 or quite high, so assign them the
# closest match, then edit the few below noted "KEEP ORIGINAL".
fixed_names1<-high_confidence_matches
length(unique(high_confidence_matches$genus_species.raw))
# 262 out of 333
# Test: Accipiter cooperi is misspelled - it disappeared here... hopefully in
# low_confidence_matches and final_names2
dplyr::filter(fixed_names1, genus_species.raw %in% c("Accipiter cooperi"))

# Assign genus_species and common_name to the closest_matches based on fuzzy
# coding match. Dec. 9, 2024: only doing genus_species for now.
names(fixed_names1)[names(fixed_names1) == "closest_genus_species_match"] <-"genus_species"
fixed_names1$common_name.raw<-NULL
fixed_names1 <- fixed_names1 %>% 
  distinct()

save.image(file.path(L1_RData_dir,"AvianInteractionData_L1.RData"))

length(unique(fixed_names1$genus_species.raw))
# 262 out of 333
dim(fixed_names1)
# 274
# Scroll through these 274 High Confidence Genus Species Matches. Use
# genus_species.edit to make changes given that it doesn't have extra spaces or
# other issues. All look okay except:

# genus_species.raw    closest_genus_species_match    genus_species_match_score

# CHANGE TO CLOSEST MATCH: checklist says Corvus brachyrhynchos caurinus
# Larus brachyrhynchos caurinus	 Northwestern Crow Corvus brachyrhynchos caurinus	0.900468284

# CHANGE TO CLOSEST MATCH: checklist says Campylorhynchus brunneicapillus
# Campylorhynchos brunneicapillus		Cactus Wren	Campylorhynchus brunneicapillus	0.900716846

# CHANGE TO CLOSEST MATCH: checklist says Cathartes aura
# Cathartus aura	Turkey Vulture	Cathartes aura	0.901098901

# CHANGE TO CLOSEST MATCH: checklist says Motacilla alba lugens 
# Moticilla alba lugens	White Wagtail (Black-Backed)	Motacilla alba lugens	0.901587302

# CHANGE TO CLOSEST MATCH: checklist says Stercorarius sp. (drop extra p)
# Stercorarius spp.		Jaeger	Stercorarius sp.	0.901960784

# CHANGE TO CLOSEST MATCH: checklist says Stercorarius sp. (drop extra p)
# Stercorarius spp.	Stercorarius spp.	NA	Stercorarius sp.	0.901960784

# CHANGE TO CLOSEST MATCH: checklist says Stercorarius sp. (drop extra p)
# Stercorarius spp. 	Unid. Jaeger	Stercorarius sp.	0.901960784

# KEEP ORIGINAL and change to species later when lumping subspecies
# checklist: Parkesia noveboracensis; 
# int.raw and unresolved.gbif: common_name = hybrid Barnacle x Bar-headed Goose
# BOW says the hybrid exists but is rare
# 1 Branta leucopsis x anser indicus Branta leucopsis x canadensis       0.905
# This one has extra spaces after it in raw version
dplyr::filter(int.raw, species1_scientific %in% c("Branta leucopsis x Anser indicus       "))
dplyr::filter(int.raw, species2_scientific %in% c("Branta leucopsis x Anser indicus       "))
dplyr::filter(unresolved.gbif, genus_species.edit %in% c("Branta leucopsis x anser indicus"))
dplyr::filter(unresolved.gbif, genus_species.edit %in% c("Branta leucopsis x anser indicus"))
fixed_names1$genus_species[fixed_names1$genus_species.edit == "Branta leucopsis x anser indicus"] <- "Branta leucopsis x Anser indicus"

# KEEP ORIGINAL and change to species later when lumping subspecies
# checklist: Cuculus canorus; common_name = Common Cuckoo
# BOW says "sometimes separated subspecifically as telephonus on basis of
# size (smaller than subtelephonus) and pale plumage (like subtelephonus), but
# birds in this area are not constant in these characters and overlap with other
# races occurs"
# 2 Cuculus canorus telephonus       Cuculus canorus subtelephonus       0.908
int.raw[which(int.raw$species1_scientific == "Cuculus canorus telephonus"), ]
int.raw[which(int.raw$species2_scientific == "Cuculus canorus telephonus"), ]
fixed_names1[which(fixed_names1$genus_species.edit == "Cuculus canorus telephonus"), ]
fixed_names1$genus_species[fixed_names1$genus_species.edit == "Cuculus canorus telephonus"] <- "Cuculus canorus telephonus"

# CHANGE TO CLOSEST MATCH: BOW says No subspecies, following Eaton (1957a) and Molina et
# al. (2000). Hence, P. n. notabilis (Ridgway, 1880), P. n. limnaeus (McCabe and
# Miller, 1933), and P. n. uliginosus (Burleigh and Peters, 1948) are junior
# synonyms of P. noveboracensis (Gmelin, 1788).
# 3 Parkesia noveboracensis limnaeus Parkesia noveboracensis             0.906
int.raw[which(int.raw$species1_scientific == "Parkesia noveboracensis limnaeus"), ]
int.raw[which(int.raw$species2_scientific == "Parkesia noveboracensis limnaeus"), ]

# CHANGE TO CLOSEST MATCH bc unresolved: BOW says: In Australia, birds in W
# previously known as race gouldi, but chloronotus has priority and not
# preoccupied by “chloronothos”.
# 4 Zosterops lateralis gouldi          Zosterops lateralis              0.910
int.raw[which(int.raw$species1_scientific == "Zosterops lateralis gouldi"), ]
int.raw[which(int.raw$species2_scientific == "Zosterops lateralis gouldi"), ]

# CHANGE TO CLOSEST MATCH bc unclear: BOW: Mainland races tend to intergrade;
# intermediates between erythronotus and tricolor sometimes referred to as
# “nigriceps”, a name better applied to a “swarm of intergrades” in NC India
# (3).
# 5 Lanius schach erythronotus/tricolor Lanius schach erythronotus       0.914
int.raw[which(int.raw$species1_scientific == "Lanius schach erythronotus/tricolor"), ]
int.raw[which(int.raw$species2_scientific == "Lanius schach erythronotus/tricolor"), ]

# KEEP ORIGINAL for now because it is a subspecies: BOW: A. w. suttoni Phillips, 1965.
# Includes A. w. mesolega Oberholser, 1974 (see Browning 1990). Breeds in
# foothills of Rocky Mountains from northern and eastern Utah (east of the Great
# Salt Lake Basin; Behle 1985) and southern Wyoming south through
# northeasternmost Arizona, northern Arizona, Colorado, central New Mexico, and
# westernmost Oklahoma to northern Chihuahua and western Texas (Pitelka 1945a,
# Pitelka 1951d) [type locality = Pueblo, Colorado]; some individuals wander in
# winter to lowlands south of the breeding range, such as the Colorado Desert
# (Phillips et al. 1964a, Patten et al. 2003) and Texas Panhandle (Seyffert
# 1985).
# 1 Aphelocoma woodhouseii suttoni   Aphelocoma woodhouseii cyanotis       0.916
int.raw[which(int.raw$species1_scientific == "Aphelocoma woodhouseii suttoni"), ]
int.raw[which(int.raw$species2_scientific == "Aphelocoma woodhouseii suttoni"), ]
fixed_names1[which(fixed_names1$genus_species.edit == "Aphelocoma woodhouseii suttoni"), ]
fixed_names1$genus_species[fixed_names1$genus_species.edit == "Aphelocoma woodhouseii suttoni"] <- "Aphelocoma woodhouseii suttoni"

# KEEP ORIGINAL and add period after sp
# 4 Strigidae sp                     Strigidae                             0.917
int.raw[which(int.raw$species2_scientific == "Strigidae sp"), ]
fixed_names1[which(fixed_names1$genus_species.edit == "Strigidae sp"), ]
fixed_names1$genus_species[fixed_names1$genus_species.edit == "Strigidae sp"] <- "Strigidae sp."

#************************************************************#
#### Scientific Name Changes: Low Confidence Matches ####
#************************************************************#

# Extract final_matches with lower confidence (match_score <= 0.90) for further checking
low_confidence_matches <- genus_species_matches %>%
  filter(
    (genus_species_match_score < 0.9 & !is.na(genus_species_match_score)) 
  )

length(unique(low_confidence_matches$genus_species.raw))
# 71 out of 333 - checks out OK

# Then check the Low Confidence Matches, based on genus_species:
# Define range for printing in increments of 0.005
score_start <- min(low_confidence_matches$genus_species_match_score)
score_end <- max(low_confidence_matches$genus_species_match_score)
increment <- 0.005

# Loop through each score range and print the matches within that range
for (i in seq(score_start, score_end, by = increment)) {
  current_range <- low_confidence_matches %>%
    filter(genus_species_match_score >= i & genus_species_match_score <= i + increment)
  
  # Print the current range if it has any entries
  if (nrow(current_range) > 0) {
    cat("\nMatch Score Range:", sprintf("%.2f", i), "to", sprintf("%.2f", i + increment), "\n")
    print(current_range, n=60)
  }
}
dim(low_confidence_matches)
# 74

# For most of the names, assign them the closest match, then edit the few above
# noted "KEEP ORIGINAL".
fixed_names2<-low_confidence_matches
# Test: Accipiter cooperi is misspelled - it is present here
dplyr::filter(fixed_names2, genus_species.raw %in% c("Accipiter cooperi"))
# # A tibble: 1 × 6
# genus_species.raw common_name.raw genus_species.edit common_name.edit closest_genus_species_match
# <chr>             <chr>           <chr>              <chr>            <chr>                      
#   1 Accipiter cooperi Cooper's Hawk   Accipiter cooperi  Cooper's Hawk    Accipiter poliogaster      
# genus_species_match_score
# <dbl>
#   1                     0.849

# Assign genus_species and common_name to the closest_matches based on fuzzy
# coding match. Dec. 2, 2024: only doing genus_species for now.
names(fixed_names2)[names(fixed_names2) == "closest_genus_species_match"] <-"genus_species"
fixed_names2$common_name.raw<-NULL
fixed_names2 <- fixed_names2 %>% 
  distinct()

length(unique(fixed_names2$genus_species.raw))
# 71 out of 333 OK

# Scroll through these 71 Low Confidence Matches in order from highest to lowest
# match score. Many look okay except check these:

# genus_species.raw                     genus_species           genus_species_match_score

# ACCEPT CHANGE TO CLOSEST MATCH because BOW states "No subspecies, following Eaton
# (1957a) and Molina et al. (2000).". Common name in int.raw is Northern Waterthrush.
# 1 Parkesia noveboracensis notabilis Parkesia noveboracensis       0.899
int.raw[which(int.raw$species1_scientific == "Parkesia noveboracensis notabilis"), ]
int.raw[which(int.raw$species2_scientific == "Parkesia noveboracensis notabilis"), ]

# KEEP ORIGINAL but edit it. This is "Rock Dove" which is also known as Rock
# Pigeon. Should be Columba livia.
# 3 Columbina livia              Columbina inca                      0.886
int.raw[which(int.raw$species1_scientific == "Columbina livia"), ]
int.raw[which(int.raw$species2_scientific == "Columbina livia"), ]
fixed_names2[which(fixed_names2$genus_species.edit == "Columbina livia"), ]
fixed_names2$genus_species[fixed_names2$genus_species.edit == "Columbina livia"] <- "Columba livia"

# ACCEPT CHANGE TO CLOSEST MATCH: BOW states no subspecies: "No subspecies, following
# Parkes (1954), who could not diagnose a difference between northeastern
# breeders and those farther west, which were named S. s. lurida (Burleigh and
# Peters, 1948)."
# 1 Setophaga striata / tigrina Setophaga striata       0.877
int.raw[which(int.raw$species1_scientific == "Setophaga striata / tigrina"), ]
int.raw[which(int.raw$species2_scientific == "Setophaga striata / tigrina"), ]

# KEEP ORIGINAL but edit it. BOW and checklist: Polytypic American Pipit Anthus
# rubescens is split into monotypic Siberian Pipit Anthus japonicus and
# polytypic American Pipit Anthus rubescens (with subspecies pacificus,
# rubescens, and alticola). int.raw interaction appears to be for North american
# species and observation (Parasitic Jaeger). Interaction is with "American
# Pipit"; checklist states "Anthus rubescens". 
# Anthus americanus     Anthus cervinus  0.871
int.raw[which(int.raw$species1_scientific == "Anthus americanus"), ]
int.raw[which(int.raw$species2_scientific == "Anthus americanus"), ]
fixed_names2[which(fixed_names2$genus_species.edit == "Anthus americanus"), ]
checklist[which(checklist$common_name == "American Pipit"), ]
fixed_names2$genus_species[fixed_names2$genus_species.edit == "Anthus americanus"] <- "Anthus rubescens"

# KEEP ORIGINAL and edit: checklist: Lesser Scaup =	Aythya affinis
# 5 Anas affinis   Argya affinis       Lesser Scaup  Lesser Scaup   0.868
int.raw[which(int.raw$species1_scientific == "Anas affinis"), ]
int.raw[which(int.raw$species2_scientific == "Anas affinis"), ]
fixed_names2[which(fixed_names2$genus_species.edit == "Anas affinis"), ]
checklist[which(checklist$common_name == "Lesser Scaup"), ]
fixed_names2$genus_species[fixed_names2$genus_species.edit == "Anas affinis"] <- "Aythya affinis"

# KEEP ORIGINAL and edit: Common name in int.raw is Hairy Woodpecker, so
# genus is off. Dryobates villosus
# 3 Leucophaeus villosus                   Leucophaeus modestus          0.867
int.raw[which(int.raw$species1_scientific == "Leucophaeus villosus"), ]
int.raw[which(int.raw$species2_scientific == "Leucophaeus villosus"), ]
fixed_names2[which(fixed_names2$genus_species.edit == "Leucophaeus villosus"), ]
fixed_names2$genus_species[fixed_names2$genus_species.edit == "Leucophaeus villosus"] <- "Dryobates villosus"

# KEEP ORIGINAL and edit: checklist: White-Eared Bronze-Cuckoo = Chalcites meyerii
# 1 Chrysococcyx meyerii   Chrysococcyx sp.  White-Eared Bronze-Cuckoo  White-eared Bronze-Cuckoo     0.867
int.raw[which(int.raw$species1_scientific == "Chrysococcyx meyerii"), ]
int.raw[which(int.raw$species2_scientific == "Chrysococcyx meyerii"), ]
fixed_names2[which(fixed_names2$genus_species.edit == "Chrysococcyx meyerii"), ]
checklist[which(checklist$common_name == "White-Eared Bronze-Cuckoo"), ]
fixed_names2$genus_species[fixed_names2$genus_species.edit == "Chrysococcyx meyerii"] <- "Chalcites meyerii"

# KEEP ORIGINAL and edit: checklist: Double-Crested Cormorant = Nannopterum auritum 
# 4 Phalacrocorax saltatrix Phalacrocorax capillatus  Double-Crested Cormorant Double-crested Cormorant  0.859
int.raw[which(int.raw$species1_scientific == "Phalacrocorax saltatrix"), ]
int.raw[which(int.raw$species2_scientific == "Phalacrocorax saltatrix"), ]
fixed_names2[which(fixed_names2$genus_species.edit == "Phalacrocorax saltatrix"), ]
checklist[which(checklist$common_name == "Double-crested Cormorant"), ]
fixed_names2$genus_species[fixed_names2$genus_species.edit == "Phalacrocorax saltatrix"] <- "Nannopterum auritum"

# KEEP ORIGINAL and edit: Phalacrocorax sp. instead of spp. Also raw version has
# spaces after: "Phalacrocorax spp. "
#2 Phalacrocorax spp.  Phalacrocorax varius    0.861
int.raw[which(int.raw$species1_scientific == "Phalacrocorax spp."), ]
int.raw[which(int.raw$species2_scientific == "Phalacrocorax spp."), ]
fixed_names2[which(fixed_names2$genus_species.edit == "Phalacrocorax spp."), ]
fixed_names2$genus_species[fixed_names2$genus_species.edit == "Phalacrocorax spp."] <- "Phalacrocorax sp."

# ACCEPT CHANGE TO CLOSEST MATCH: checklist: 	Black-Necked Stilt = Himantopus mexicanus 
# 6 Himantopus alexandrus   Himantopus mexicanus   Black-Necked Stilt  Black-necked Stilt 0.857
int.raw[which(int.raw$species1_scientific == "Himantopus alexandrus"), ]
int.raw[which(int.raw$species2_scientific == "Himantopus alexandrus"), ]
checklist[which(checklist$common_name == "Black-necked Stilt"), ]

# KEEP ORIGINAL but edit: checklist: Pomarine Jaeger = Stercorarius pomarinus
# Also raw has space at end "Stercorarius stercorarius "
# 5 Stercorarius stercorarius Stercorarius parasiticus    Pomarine Jaeger     Pomarine Jaeger  0.857
int.raw[which(int.raw$species1_scientific == "Stercorarius stercorarius"), ]
int.raw[which(int.raw$species2_scientific == "Stercorarius stercorarius"), ]
checklist[which(checklist$common_name == "Pomarine Jaeger"), ]
fixed_names2[which(fixed_names2$genus_species.edit == "Stercorarius stercorarius"),]
fixed_names2$genus_species[fixed_names2$genus_species.edit == "Stercorarius stercorarius"] <- "Stercorarius pomarinus"

# KEEP ORIGINAL, Common name in int.raw is the White Wagtail. 
# 1 Moticilla alba            Motacilla citreola             0.858
int.raw[which(int.raw$species1_scientific == "Moticilla alba"), ]
int.raw[which(int.raw$species2_scientific == "Moticilla alba"), ]
fixed_names2[which(fixed_names2$genus_species.edit == "Moticilla alba"), ]
fixed_names2$genus_species[fixed_names2$genus_species.edit == "Moticilla alba"] <- "Moticilla alba"

# KEEP ORIGINAL and edit: According to BOW there is no Anas flavirostris / anas
# andium. Common name in int.raw is Speckled Teal but checklist is Andean/Yellow-billed Teal. 
# 3 Anas flavirostris / anas andium Anas flavirostris           0.849
int.raw[which(int.raw$species1_scientific == "Anas flavirostris / Anas andium"), ]
int.raw[which(int.raw$species2_scientific == "Anas flavirostris / Anas andium"), ]
fixed_names2[which(fixed_names2$genus_species.raw == "Anas flavirostris / Anas andium"), ]
fixed_names2$genus_species[fixed_names2$genus_species.edit == "Anas flavirostris / anas andium"] <- "Anas andium/flavirostris"
# Common name will be changed later
#fixed_names2$common_name[fixed_names2$common_name.orig == "Speckled Teal"] <- "Andean/Yellow-billed Teal"

# KEEP ORIGINAL and edit. Common name in int.raw is House Finch (Haemorhous mexicanus).
# 2 Hirundo mexicanus               Todus mexicanus             0.851
int.raw[which(int.raw$species1_scientific == "Hirundo mexicanus"), ]
int.raw[which(int.raw$species2_scientific == "Hirundo mexicanus"), ]
fixed_names2[which(fixed_names2$genus_species.edit == "Hirundo mexicanus"), ]
fixed_names2$genus_species[fixed_names2$genus_species.edit == "Hirundo mexicanus "] <- "Haemorhous mexicanus"

# KEEP ORIGINAL and edit. Checklist shows genus moved to Astur. Also species is cooperii.
# 1 Accipiter cooperi               Accipiter poliogaster       0.849
fixed_names2[which(fixed_names2$genus_species.edit == "Accipiter cooperi"), ]
fixed_names2$genus_species[fixed_names2$genus_species.edit == "Accipiter cooperi"] <- "Astur cooperii"
# 3 Accipiter spp.            Accipiter nisus                   0.840
fixed_names2[which(fixed_names2$genus_species.raw == "Accipiter spp."), ] # No; has a space after it "Accipiter spp. "
fixed_names2[which(fixed_names2$genus_species.edit == "Accipiter spp."), ]
fixed_names2$genus_species[fixed_names2$genus_species.edit == "Accipiter spp."] <- "Aerospiza/Tachyspiza/Accipiter/Astur sp."
# Listed as American Goshawk; KEEP ORIGINAL and edit to Astur
# 2 Accipiter atricapillus    Astur cooperii/atricapillus       0.839
fixed_names2[which(fixed_names2$genus_species.edit == "Accipiter atricapillus"), ] 
fixed_names2$genus_species[fixed_names2$genus_species.edit == "Accipiter atricapillus"] <- "Astur atricapillus"

# KEEP ORIGINAL and edit: BOW and checklist: "Black-winged Babbler" is most likely Jungle
# Babbler (Black-winged) Argya striata somervillei 
# 1 Argya affinis somervillei  Argya affinis                     0.84
int.raw[which(int.raw$species1_scientific == "Argya affinis somervillei"), ]
int.raw[which(int.raw$species2_scientific == "Argya affinis somervillei"), ]
fixed_names2[which(fixed_names2$genus_species.edit == "Argya affinis somervillei"), ]
fixed_names2$genus_species[fixed_names2$genus_species.edit == "Argya affinis somervillei"] <- "Argya striata somervillei"
# Common name will be changed later
#fixed_names2$common_name[fixed_names2$common_name.orig == "Black-winged Babbler"] <- "Jungle Babbler (Black-winged)"

# KEEP ORIGINAL and edit. Checklist shows genus moved to Astur atricapillus. 
# 1 Accipiter getilis Accipiter poliogaster       0.839
int.raw[which(int.raw$species1_scientific == "Accipiter getilis"), ]
int.raw[which(int.raw$species2_scientific == "Accipiter getilis"), ]
fixed_names2[which(fixed_names2$genus_species.edit == "Accipiter getilis"), ]
fixed_names2$genus_species[fixed_names2$genus_species.edit == "Accipiter getilis"] <- "Astur atricapillus"
# Check it: OK
fixed_names2[which(fixed_names2$genus_species.raw == "Accipiter getilis"),]

# CHANGE TO CLOSEST MATCH: original common name: Rufous-and-white Wren;
# checklist confirms the suggested genus_species change.
# 2 Thyrothorus rufalbus Thryophilus rufalbus       0.831 
int.raw[which(int.raw$species1_scientific == "Thyrothorus rufalbus"), ]
int.raw[which(int.raw$species2_scientific == "Thyrothorus rufalbus"), ]

# KEEP ORIGINAL and edit. Common name in int.raw = White-eared Bronze-Cuckoo.
# Checklist states: Move from Chrysococcyx into Chalcites to become Chalcites
# meyerii.
# 1 Chrysococcyx meyerii Chrysococcyx caprius       0.833
int.raw[which(int.raw$species1_scientific == "Chrysococcyx meyerii"), ]
int.raw[which(int.raw$species2_scientific == "Chrysococcyx meyerii"), ]
fixed_names2[which(fixed_names2$genus_species.edit == "Chrysococcyx meyerii"), ]
fixed_names2$genus_species[fixed_names2$genus_species.edit == "Chrysococcyx meyerii"] <- "Chalcites meyerii"

# KEEP ORIGINAL and edit to remove p in spp. Also has extra space in raw "Molothrus spp. "
# 2 Molothrus spp.   Myioborus sp.          0.828
fixed_names2[which(fixed_names2$genus_species.raw == "Molothrus spp."),]
fixed_names2[which(fixed_names2$genus_species.edit == "Molothrus spp."),]
fixed_names2$genus_species[fixed_names2$genus_species.edit == "Molothrus spp."] <- "Molothrus sp."

# KEEP ORIGINAL and edit. This is the Eurasian Goshawk based on the location of
# the interaction (Svalbard): was Accipter atricapillus and is now Astur gentilis
# 1 Accipter atricapillus Accipiter striatus       0.821
int.raw[which(int.raw$species2_scientific == "Accipter atricapillus"), ]
fixed_names2[which(fixed_names2$genus_species.edit == "Accipter atricapillus"), ]
fixed_names2$genus_species[fixed_names2$genus_species.edit == "Accipter atricapillus"] <- "Astur gentilis"

# KEEP ORIGINAL and edit: int.raw shows common name is White-throated Swifts.
# checklist and BOW: Aeronautes saxatalis and this species has interactions
# documented in int.raw with Violet-green swallows (competition).
# 3 Panyptila saxatilis  Pachyptila salvini       0.814
int.raw[which(int.raw$species2_scientific == "Panyptila saxatilis"), ]
fixed_names2[which(fixed_names2$genus_species.edit == "Panyptila saxatilis"), ]
fixed_names2$genus_species[fixed_names2$genus_species.edit == "Panyptila saxatilis"] <- "Aeronautes saxatalis"

# CHANGE TO CLOSEST MATCH: int.raw common name = Eurasian Magpie; 
# BOW states it is in revision so assign to main species for now.
# 1 Pica pica anderssoni Pica pica                0.817
int.raw[which(int.raw$species2_scientific == "Pica pica anderssoni"), ]
int.raw[which(int.raw$species1_scientific == "Pica pica anderssoni"), ]

# KEEP ORIGINGAL and edit: int.raw common name is Cliff Swallow, Checklist and
# BOW: changed to Petrochelidon pyrrhonota.
# 2 Hirundo pyrrhonta    Hirundo nigrorufa        0.817
int.raw[which(int.raw$species2_scientific == "Hirundo pyrrhonta"), ]
int.raw[which(int.raw$species1_scientific == "Hirundo pyrrhonta"), ]
fixed_names2[which(fixed_names2$genus_species.edit == "Hirundo pyrrhonta"), ]
fixed_names2$genus_species[fixed_names2$genus_species.edit == "Hirundo pyrrhonta"] <- "Petrochelidon pyrrhonota"

# KEEP ORIGINAL and edit: remove p from spp. Also extra space in raw "Aechmophorus spp. "
#2 Aechmophorus spp.  Aechmophorus clarkii        Unid. Grebe          Junin Grebe              
fixed_names2[which(fixed_names2$genus_species.raw == "Aechmophorus spp."),]
fixed_names2[which(fixed_names2$genus_species.edit == "Aechmophorus spp."),]
fixed_names2$genus_species[fixed_names2$genus_species.edit == "Aechmophorus spp."] <- "Aechmophorus sp."

# KEEP ORIGINAL and edit: Pyrrhuloxia
# BOW and checklist is Cardinalis sinuatus
# int.raw Common name = Pyrrhuloxia
#2 Pyrrhuloxia sinuata                 Pyrrhula owstoni                        0.799
int.raw[which(int.raw$species1_scientific == "Pyrrhuloxia sinuata"), ]
int.raw[which(int.raw$species2_scientific == "Pyrrhuloxia sinuata"), ]
fixed_names2[which(fixed_names2$genus_species.edit == "Pyrrhuloxia sinuata"), ]
fixed_names2$genus_species[fixed_names2$genus_species.edit == "Pyrrhuloxia sinuata"] <- "Cardinalis sinuatus"

# KEEP ORIGINAL and edit: Yellow Warbler (Mangrove) 
# BOW and checklist: Setophaga petechia [erithachorides Group]
#3 Dendroica petechia (erithachordies) Setophaga petechia erithachorides       0.801
int.raw[which(int.raw$species1_scientific == "Dendroica petechia (erithachordies)"), ]
int.raw[which(int.raw$species2_scientific == "Dendroica petechia (erithachordies)"), ]
fixed_names2[which(fixed_names2$genus_species.edit == "Dendroica petechia (erithachordies)"), ]
fixed_names2$genus_species[fixed_names2$genus_species.edit == "Dendroica petechia (erithachordies)"] <- "Setophaga petechia [erithachorides Group]"

# KEEP ORIGINAL and edit: Pine Warbler
# BOW and checklist: Setophaga pinus
# 4 Dendroica pinus                     Dendrocincla sp.                        0.803 
int.raw[which(int.raw$species1_scientific == "Dendroica pinus"), ]
int.raw[which(int.raw$species2_scientific == "Dendroica pinus"), ]
fixed_names2[which(fixed_names2$genus_species.edit == "Dendroica pinus"), ]
fixed_names2$genus_species[fixed_names2$genus_species.edit == "Dendroica pinus"] <- "Setophaga pinus"

# KEEP ORIGINAL and edit: Collared Sparrowhawk
# BOW and checklist: Genus changed to Tachyspiza cirrocephala
# 1 Accipiter cirrhocephalus Accipiter striatus       0.797
int.raw[which(int.raw$species1_scientific == "Accipiter cirrhocephalus"), ]
int.raw[which(int.raw$species2_scientific == "Accipiter cirrhocephalus"), ]
fixed_names2[which(fixed_names2$genus_species.edit == "Accipiter cirrhocephalus"), ]
checklist[which(checklist$common_name == "Collared Sparrowhawk"), ]
fixed_names2$genus_species[fixed_names2$genus_species.edit == "Accipiter cirrhocephalus"] <- "Tachyspiza cirrocephala"

# KEEP ORIGINAL and edit: California Quail
# BOW and checklist: California Quail; Genus changed to Callipepla californica
# 1 Lophortyx californicus Lophornis ornatus       0.788
int.raw[which(int.raw$species1_scientific == "Lophortyx californicus"), ]
int.raw[which(int.raw$species2_scientific == "Lophortyx californicus"), ]
fixed_names2[which(fixed_names2$genus_species.edit == "Lophortyx californicus"), ]
checklist[which(checklist$common_name == "California Quail"), ]
fixed_names2$genus_species[fixed_names2$genus_species.edit == "Lophortyx californicus"] <- "Callipepla californica"

# KEEP ORIGINAL and edit; checklist: Leach's Storm-Petrel = Hydrobates leucorhous
#3 Oceanodroma leucorrhoa Paraclaravis mondetoura ochoterena Leach's Storm-Petrel Leach's Storm-Petrel 
int.raw[which(int.raw$species1_scientific == "Oceanodroma leucorrhoa"), ]
int.raw[which(int.raw$species2_scientific == "Oceanodroma leucorrhoa"), ]
fixed_names2[which(fixed_names2$genus_species.edit == "Oceanodroma leucorrhoa"), ]
checklist[which(checklist$common_name == "Leach's Storm-Petrel"), ]
fixed_names2$genus_species[fixed_names2$genus_species.edit == "Oceanodroma leucorrhoa"] <- "Hydrobates leucorhous"

# # KEEP ORIGINAL and edit; Duck sp. = Anatidae sp. 
# 2 Duck sp 0.737 Anatidae sp. 
fixed_names2[which(fixed_names2$genus_species.raw == "Duck sp"),]
fixed_names2[which(fixed_names2$genus_species.edit == "Duck sp"),]
fixed_names2$genus_species[fixed_names2$genus_species.edit == "Duck sp"] <- "Anatidae sp."

# KEEP ORIGINAL and edit int.raw; this entry has multiple columns off for
# scientific and common BOW and checklist: genus_species of Roseate Spoonbill = Platalea ajaja 1
# Roseate spoonbill Cormobates placens meridionalis       0.736
int.raw[which(int.raw$species1_scientific == "Roseate spoonbill"), ]
int.raw[which(int.raw$species2_scientific == "Roseate spoonbill"), ]
checklist[which(checklist$common_name == "Roseate Spoonbill"), ]
fixed_names2[which(fixed_names2$genus_species.edit == "Roseate spoonbill"), ]
int.raw$species2_common[int.raw$species1_scientific == "Roseate Spoonbill"] <- "Roseate Spoonbill"
int.raw$species1_scientific[int.raw$species1_scientific == "Roseate Spoonbill"] <- "Ardea herodias"
fixed_names2$genus_species[fixed_names2$genus_species.edit == "Roseate spoonbill"] <- "Platalea ajaja"
# Common name will be changed later
#fixed_names2$common_name[fixed_names2$genus_species.raw == "Roseate spoonbill"] <- "Roseate Spoonbill"

# KEEP ORIGINAL and edit: Unid. Storm Petrel. Also raw has extra space "Oceanodroma spp. "
# BOW: now Genus is Hydrobates instead of Oceanodrama
#1 Oceanodroma spp.                    Oceanitidae sp.                          0.691
int.raw[which(int.raw$species1_scientific == "Oceanodroma spp."), ]
int.raw[which(int.raw$species2_scientific == "Oceanodroma spp."), ]
fixed_names2[which(fixed_names2$genus_species.raw == "Oceanodroma spp."), ]
fixed_names2[which(fixed_names2$genus_species.edit == "Oceanodroma spp."), ]
fixed_names2$genus_species[fixed_names2$genus_species.edit == "Oceanodroma spp."] <- "Hydrobates sp."

# Typos 
# 1 unid. 2 hawl  Turdus hauxwelli       0.674
# Also missing: unid. Accipiter hawl
int.raw[which(int.raw$species1_scientific == "unid. 2 hawl"), ]
int.raw[which(int.raw$species2_scientific == "unid. 2 hawl"), ]
fixed_names2[which(fixed_names2$genus_species.raw == "unid. 2 hawl"), ]
fixed_names2[which(fixed_names2$genus_species.edit == "unid. 2 hawl"), ]
int.raw[which(int.raw$species1_scientific == "unid. Accipiter hawl"), ]
int.raw[which(int.raw$species2_scientific == "unid. Accipiter hawl"), ]
fixed_names2$genus_species[fixed_names2$genus_species.edit == "unid. 2 hawl"] <- "Aerospiza/Tachyspiza/Accipiter/Astur sp."
fixed_names2$genus_species[fixed_names2$genus_species.edit == "unid. Accipiter hawl"] <- "Aerospiza/Tachyspiza/Accipiter/Astur sp."

save.image(file.path(L1_RData_dir,"AvianInteractionData_L1.RData"))

# Combine these together for the full set of fixed genus_species for the
# unresolved_gbif names which were fixed by referencing the CHECKLIST. Remove
# extra columns that are not needed for combining w genus_species.
fixed.unresolved.gs<-rbind(fixed_names1,fixed_names2)
fixed.unresolved.gs$genus_species_match_score<-NULL
length(unique(fixed.unresolved.gs$genus_species.raw))
# 333 OK
dim(fixed.unresolved.gs)
# 345 fixed unresolved species names

save.image(file.path(L1_RData_dir,"AvianInteractionData_L1.RData"))

# Assign genus_species.edit and common_name.edit to the closest_matches based on
# fuzzy coding match. Dec. 9, 2024: only doing genus_species for now.
names(resolved.gbif)[names(resolved.gbif) == "accepted_scientific_name"] <-"genus_species"
resolved.gbif$common_name.raw<-NULL
resolved.gbif$scientific_id<-NULL

# Check recent changes in resolved.gbif (where GBIF may be incorrect):
# See if the resolved.gbif contain Accipiter gentilis or Accipiter atricapillus
# as genus_species ... Yes, so need to update these
resolved.gbif[which(resolved.gbif$genus_species.edit == "Accipiter gentilis"), ]
resolved.gbif$genus_species[resolved.gbif$genus_species.edit == "Accipiter gentilis"] <- "Astur atricapillus"

save.image(file.path(L1_RData_dir,"AvianInteractionData_L1.RData"))

###********FIXING INCORRECT GBIF RESOLVED EARLIER IN WORKFLOW******###
# Some of the GBIF assignments or misspelling corrections did not work well.
# Refer to the bbs.splist.NA and int.raw.bbs_subset (both created way below)-
# they have missing species in .raw, that actually exist in int.raw. Need to
# edit these here by changing the before merging in. 

# Spruce Grouse: raw is Canachites
# canadensis, Dendragapus canadensis, Falcipennis canadensis - in fact it should
# be Canachites canadensis (GBIF is incorrect) Fix
resolved.gbif[which(resolved.gbif$common_name == "Spruce Grouse"), ]
splist2024[which(splist2024$bbs_common_name == "Spruce Grouse"), ]
resolved.gbif$genus_species[resolved.gbif$common_name == "Spruce Grouse"] <- "Canachites canadensis"

# American Three-toed Woodpecker Picoides tridactylus - raw/edit is Picoides dorsalis
# Confirmed it is North American species, not European for this interaction.
splist2024[which(splist2024$bbs_common_name == "American Three-toed Woodpecker"), ]
resolved.gbif[which(resolved.gbif$common_name == "American Three-Toed Woodpecker"), ]
resolved.gbif$genus_species[resolved.gbif$common_name == "American Three-Toed Woodpecker"] <- "Picoides dorsalis"

# # pileated Woodpecker Drycopus pileatus - typo: Dryocopus pileatus
# splist2024[which(splist2024$bbs_common_name == "Pileated Woodpecker"), ]
# resolved.gbif$genus_species[resolved.gbif$common_name == "American Three-Toed Woodpecker"] <- "Picoides dorsalis"


# colima warbler Leiothlypis crissalis
# Ruby-crowned Kinglet Regulus calendula
# Clark's Grebe Aechmophorus clarkii transitionalis 
# Western Grebe Aechmophorus occidentalis occidentalis
# Northern Saw-whet Owl Aegolius acadicus
# Flammulated Owl Otus flammeolus
# Boreal Owl Aegolius funereus (no NA intxns)
# Crested Auklet Aethia cristatella (not in bbs.splist)
# Least Auklet Aethia pusilla (not in bbs.splist)
# Whiskered Auklet Aethia pygmaea (not in bbs.splist)
# Horned Puffin Fratercula corniculata
# Glaucous-winged Gull Larus glaucescens
# Glaucous Gull Larus hyperboreus
# 	Peregrine Falcon 	Falco peregrinus 


# Take the genus_species in resolved.gbif and fixed.unresolved.gs and rbind them,
# remove duplicates, then merge with CHECKLIST to get common name assigned. Keep
# reference of the genus_species.raw bc need to merge back into the int.raw data.
int.final.names<-rbind(resolved.gbif,fixed.unresolved.gs)
dim(int.final.names)
# 6057 
length(unique(int.final.names$genus_species.raw))
# 4333 OK

# Remove duplicate rows, if any exist
int.final.names <- int.final.names %>% 
  distinct()
dim(int.final.names)
# 4989
length(unique(int.final.names$genus_species.raw))
# 4333 OK

# NOTE TO FIX THESE 4989 LATER; decide whether to assign Family level or Genus for
# this instead of the genus_species.raw. Some have some typos (only a few). Some
# of the int.final.names genus_species are blank. Most are because they are
# Genus sp. For now, leave these as is since we cannot get to species level on
# these interactions.
int.final.names.Gsp<-int.final.names[which(is.na(int.final.names$genus_species)), ]
dim(int.final.names.Gsp)
# 734 Genus-level entries 

# All the NA in genus_species are "Genus sp.". If the NA exists in
# genus_species, assign it the genus_species.edit which includes this
# designation.
int.final.names$genus_species1 <- ifelse(is.na(int.final.names$genus_species), 
                                        int.final.names$genus_species.edit, 
                                        int.final.names$genus_species)
names(int.final.names)[names(int.final.names) == "genus_species"] <-"genus_species.gbif.chklist"
names(int.final.names)[names(int.final.names) == "genus_species1"] <-"genus_species"

# Now merge the GBIF & checked CHECKLIST-derived int.final.names$genus_species
# with the CHECKLIST to get final checklist common_name. First drop some
# checklist columns.
checklist.narrow<-subset(checklist, select=c("genus_species",
                                             "common_name",
                                             "category",
                                             "order",
                                             "family"))
checklist.narrow<-data.frame(checklist.narrow)
int.checklist<-merge(int.final.names,checklist.narrow, by=c("genus_species"),all.x=T)
length(unique(int.checklist$genus_species.raw))
# 4333 OK
head(int.checklist)
dim(int.checklist)
# 4989

# 1005 missing common names - about 30% are "Genus sp." but others are just
# missing?? For now we are just going to focus on fixing the BBS species. Later
# this needs to be edited with a better workflow. 
int.checklist.NAcommon<-int.checklist[which(is.na(int.checklist$common_name)), ]
dim(int.checklist.NAcommon)
# 1002
dim(int.checklist.NAcommon)-dim(int.final.names.Gsp)
# 734 Genus-level entries from above but 268 missing common name! Some are
# duplicate rows from genus_species.raw. Also some of the GBIF species
# assignments do not work (GBIF taxonomic backbone is not quite up to date for
# these birds)

# Fix these by assigning the appropriate genus_species based on CHECKLIST, then
# merge with CHECKLIST to get common names; add common name from
# common_name.orig when it doesn't exist in CHECKLIST

int.checklist$genus_species[int.checklist$genus_species.edit == "Acanthis flammea"] <- "Acanthis flammea hornemanni"
int.checklist$common_name[int.checklist$genus_species.edit == "Acanthiza apicalis albiventris"] <- "Red-Tailed Thornbill"
int.checklist$genus_species[int.checklist$genus_species.edit == "Accipiter bicolor"] <- "Astur bicolor"
int.checklist$genus_species[int.checklist$genus_species.edit == "Accipiter cooperii"] <- "Astur cooperii"
int.checklist$genus_species[int.checklist$genus_species.edit == "Accipiter gentilis"] <- "Astur atricapillus"
int.checklist$genus_species[int.checklist$genus_species.edit == "Accipiter gentilis laingi"] <- "Astur atricapillus laingi"
int.checklist$genus_species[int.checklist$genus_species.edit == "Accipiter melanoleucus"] <- "Astur melanoleucus"
int.checklist$genus_species[int.checklist$genus_species.edit == "Accipiter sp."] <- "Aerospiza/Tachyspiza/Accipiter/Astur sp."
int.checklist$common_name[int.checklist$genus_species.edit == "Accipiter striatus venator"] <- "Sharp-Shinned Hawk (Puerto Rican)"
#? Acrocephalus scirpaceus baeticatus and Acrocephalus gracilirostris leptorhynchus without CHECKLIST common_name
int.checklist$common_name[int.checklist$genus_species.edit == "Aechmophorus clarkii clarkii"] <- "Clark's Grebe (clarkii)"
int.checklist$common_name[int.checklist$genus_species.edit == "Aechmophorus clarkii transitionalis"] <- "Clark's Grebe (transitionalis)"
# no subspecies in CHECKLIST
int.checklist$genus_species[int.checklist$genus_species.edit == "Aechmophorus occidentalis ephemeralis"] <- "Aechmophorus occidentalis"
int.checklist$genus_species[int.checklist$genus_species.edit == "Aechmophorus occidentalis occidentalis"] <- "Aechmophorus occidentalis"
# ? Aegolius funereus funereus:  Tengmalm's Owl - not in CHECKLIST
# ? Agelaioides badius badius and Agelaioides badius bolivianus: Grayish Baywing subspecies but no common_name in CHECKLIST
# ? Agelaioides badius fringillarius: Pale Baywing subspecies? only species in CHECKLIST 
int.checklist$genus_species[int.checklist$genus_species.edit == "Agelaioides badius fringillarius"] <- "Agelaioides fringillarius"
# Aimophila ruficeps eremoeca: Rufus-crowned sparrow; no common_name in CHECKLIST
int.checklist$common_name[int.checklist$genus_species.edit == "Aimophila ruficeps eremoeca"] <- "Rufous-Crowned Sparrow (Eremoeca)"
int.checklist$common_name[int.checklist$genus_species.edit == "Aimophila ruficeps scottii"] <- "Rufous-Crowned Sparrow (Scottii)"
int.checklist$common_name[int.checklist$genus_species.edit == "Schoeniparus castaneceps"] <- "Rufous-winged Fulvetta"
int.checklist$genus_species[int.checklist$genus_species.edit == "Schoeniparus castaneceps"] <- "Schoeniparus castaneceps"
int.checklist$common_name[int.checklist$genus_species.edit == "Schoeniparus cinereus"] <- "Yellow-Throated Fulvetta"
int.checklist$genus_species[int.checklist$genus_species.edit == "Schoeniparus cinereus"] <- "Schoeniparus cinereus"
int.checklist$common_name[int.checklist$genus_species.edit == "Schoeniparus dubius"] <- "Rusty-Capped Fulvetta"
int.checklist$genus_species[int.checklist$genus_species.edit == "Schoeniparus dubius"] <- "Schoeniparus dubius"
# Alcippe poioicephala phayrei is a subspecies but no common_name in CHECKLIST
# GBIF WAS INCORRECT:
int.checklist$common_name[int.checklist$genus_species.edit == "Ammospiza caudacuta caudacuta"] <- "Saltmarsh Sparrow (Caudacuta)"
int.checklist$genus_species[int.checklist$genus_species.edit == "Ammospiza caudacuta caudacuta"] <- "Ammospiza caudacuta caudacuta"
int.checklist$common_name[int.checklist$genus_species.edit == "Ammospiza caudacuta diversa"] <- "Saltmarsh Sparrow (Diversa)"
int.checklist$genus_species[int.checklist$genus_species.edit == "Ammospiza caudacuta diversa"] <- "Ammospiza caudacuta diversa"

# ... 

# For now, focus on BBS splist because need to get these data cleaned and
# aligned for merging with bbs.obs for network modeling. Return to the cleaning
# below after this is completed...

#******** BBS WORK **********# 
# Merge genus_species in splist with genus_species in int.checklist
# to check current common_name list and identify gaps and updates to
# genus_species. This will update most of the BBS species, but not necessarily the ones
# outside of BBS. "genus_species" = the names agreed upon by CHECKLIST and by
# the GBIF-screened species. common_name = the CHECKLIST common_name.
int.checklist <- subset(
  int.checklist,
  select = c("genus_species", "common_name", "genus_species.raw","genus_species.edit")
)
dim(int.checklist)
# 4989
int.checklist <- int.checklist %>% 
  distinct()
dim(int.checklist)
# 4333 OK
length(unique(int.checklist$genus_species.raw))
# 4333 OK

save.image(file.path(L1_RData_dir,"AvianInteractionData_L1.RData"))

##****BBS SPECIES LIST WORK****## 
## Make a new column in bbs.splist to maintain the BBS genus_species. First make
#a copy of splist to save.
splist<-splist2024
splist$genus_species.bbs2024<-splist$genus_species
# Work on splist genus_species to remove spaces between "/" names (to match
# names in CHECKLIST)
splist$genus_species <- gsub("\\s*/\\s*", "/", splist$genus_species)

# Some species need to be added to bbs.splist (Accipiter gentilis)
# Add common_name
int.checklist$common_name[int.checklist$genus_species.edit == "Accipiter gentilis"] <- "American Goshawk"
int.checklist$common_name[int.checklist$genus_species.edit == "Accipiter getilis"] <- "American Goshawk"
int.checklist$common_name[int.checklist$genus_species.edit == "Accipiter gentilis laingi"] <- "Queen Charlotte Goshawk"

accip.ge <- int.checklist %>%
  filter(str_starts(genus_species.raw, "Accipiter ge"))

# View the result
print(accip.ge)

# Merge splist and int.checklist to get the common_names and the
# genus_species.raw that we will use to assign the correct common_names in the
# int.raw data later. First rename the BBS to merge-by column to match
# int.checklist$genus_species.edit. 
splist$genus_species.edit<-splist$genus_species

# Merge in the Accipiter gentilis rows from above
splist<-merge(splist,accip.ge,by=c("genus_species.edit","genus_species"),all.x=T,all.y=T)
# Assign these missing data based on Astur atricapillus
# Identify the rows to fill and the reference row
rows_to_fill <- splist %>% filter(genus_species == "Astur atricapillus")
reference_row <- splist %>% filter(genus_species.edit == "Accipiter atricapillus")

# Fill missing values in the matching rows with values from the reference row
splist <- splist %>%
  mutate(across(
    everything(),
    ~ if_else(
      genus_species == "Astur atricapillus" & is.na(.),
      reference_row[[cur_column()]],
      .
    )
  ))

#splist$genus_species<-NULL
splist$common_name<-NULL
splist$genus_species.raw<-NULL

# Reduce columns
splist$French_Common_Name<-NULL
splist$Order<-NULL
splist$Family<-NULL
splist$Genus<-NULL
splist$Species<-NULL

# Merge these - need to merge by each: genus_species and separately, because both could match
# genus_species.edit to make sure we get them all (but we don't want all of int.checklist).
bbs.splist1<-merge(splist,int.checklist,by=c("genus_species.edit"),all.x=T)
# Here the bbs.splist1$genus_species.x belongs to "splist" and the
# genus_species.y belongs to int.checklist
# Rename - assign the int.checklist$genus_species as the master
names(bbs.splist1)[names(bbs.splist1) == "genus_species.x"] <-"genus_species.splist"
names(bbs.splist1)[names(bbs.splist1) == "genus_species.y"] <-"genus_species"
# Here the bbs.splist2$genus_species.edit.x belongs to "splist" and the
# genus_species.edit.y belongs to int.checklist.
# TEST CASE: Drycopus pileatus is in bbs.splist2
bbs.splist2<-merge(splist,int.checklist,by=c("genus_species"),all.x=T)
# Rename - assign the int.checklist$genus_species.edit as the master
names(bbs.splist2)[names(bbs.splist2) == "genus_species.edit.x"] <-"genus_species.edit.splist"
names(bbs.splist2)[names(bbs.splist2) == "genus_species.edit.y"] <-"genus_species.edit"

# Row bind them together and remove any duplicate rows. (Merging removes rows even with all.x=T and all.y=T; for example the bbs.splist2$genus_species.raw=="Drycopus pileatus" disappears).
bbs.splist2$genus_species.splist<-""
bbs.splist1$genus_species.edit.splist<-""

bbs.splist<-rbind(bbs.splist1,bbs.splist2)
dim(bbs.splist)
# 2159
# Remove the auxilliary columns just used in merging
bbs.splist$genus_species.edit.splist<-NULL
bbs.splist$genus_species.splist<-NULL

# Remove any duplicate rows
bbs.splist <- bbs.splist %>% 
  distinct()
dim(bbs.splist)
# 1289
# > 778 because because we are keeping track of the genus_species.raw entry. 
length(unique(bbs.splist$genus_species.raw))
# 1106 unique genus_species.raw

# ****** OMIT the spaces after the names in .raw and then get unique rows
# DO THIS EARLIER IN REVISION - remember to do it also for bbs.int.raw data
bbs.splist$genus_species.raw<-trimws(bbs.splist$genus_species.raw,which=c("right"))
bbs.splist <- bbs.splist %>% 
  distinct()
dim(bbs.splist)
# 1096 unique rows
length(unique(bbs.splist$genus_species.raw))
# 918 unique genus_species.raw (without whitespace issues)
## STOPPED HERE - now need to remove duplicate .raw rows by some other column (omit what column??)

# Fill in Astur atricapillus laingi info which didn't carry over
# Identify the rows to fill and the reference row. We're using Accipiter getilis
# to get just 1 row (because otherwise there are multiple matches)
# Fix common_name
bbs.splist$bbs_common_name[bbs.splist$genus_species == "Astur atricapillus laingi"] <- "Queen Charlotte Goshawk"

rows_to_fill <- bbs.splist %>% filter(genus_species == "Astur atricapillus laingi")
reference_row <- bbs.splist %>% filter(genus_species.raw == "Accipiter getilis")

# Fill missing values in the matching rows with values from the reference row
bbs.splist <- bbs.splist %>%
  mutate(across(
    everything(),
    ~ if_else(
      genus_species == "Astur atricapillus laingi" & is.na(.),
      reference_row[[cur_column()]],
      .
    )
  ))

# Which are NA?
bbs.splist.NA<-bbs.splist[which(is.na(bbs.splist$genus_species.raw)), ]
dim(bbs.splist.NA)
# 165 
# Search for matches and summarize the columns where each genus_species.edit
# is found
bbs.splist.NA <- bbs.splist.NA %>%
  rowwise() %>%
  mutate(
    found_in_columns = paste(
      c(
        if (genus_species.edit %in% int.checklist$genus_species) "genus_species",
        if (genus_species.edit %in% int.checklist$genus_species.edit) "genus_species.edit"
      ),
      collapse = ", "
    )
  ) %>%
  ungroup()

# Replace empty matches with "Not Found"
bbs.splist.NA <- bbs.splist.NA %>%
  mutate(found_in_columns = ifelse(found_in_columns == "", "Not Found", found_in_columns))

# Print out any results that are not "Not Found"
matches_found <- bbs.splist.NA %>%
  filter(found_in_columns != "Not Found")

print(matches_found)
# Only 1 species and its grouping should be updated here; the rest are BBS
# species that just don't occur in our interaction dataset; they are mostly
# unidentified species of 2 or more species, so that's okay. 
# genus_species.edit                  AOU 
# 1 Aphelocoma californica/woodhouseii 34810 
# 2 Aphelocoma californica/woodhouseii  4810
bbs.splist[which(bbs.splist$genus_species.edit == "Aphelocoma californica/woodhouseii"), ]
checklist[which(checklist$genus_species == "Aphelocoma californica/woodhouseii"), ]
int.raw[which(int.raw$species1_scientific == "Aphelocoma californica/woodhouseii"), ]
bbs.splist$genus_species[bbs.splist$genus_species.edit == "Aphelocoma californica/woodhouseii"] <- "Aphelocoma californica/woodhouseii"
bbs.splist$genus_species.raw[bbs.splist$genus_species.edit == "Aphelocoma californica/woodhouseii"] <- "Aphelocoma californica / woodhouseii"
bbs.splist$common_name[bbs.splist$genus_species.edit == "Aphelocoma californica/woodhouseii"] <- "California/Woodhouse's Scrub-Jay"

# 165 BBS species without genus_species.raw, meaning they have no lookup match in
# CHECKLIST. The reason is that these BBS names did not match the CHECKLIST
# edits in 2024, or the species is not a species that exists in the int.raw
# data (most of them). Make the changes that reflect updates to species names in the CHECKLIST.

# Edit the genus_species column in bbs.splist (in the future, edit this to
# create look-up table that is merged in instead):

print(bbs.splist.NA[,5])
# 67 - check these later to update all; only changed ones that appear in int.raw
# [1] "Aechmophorus occidentalis/clarkii"            
# [2] "Aechmophorus occidentalis/clarkii"            
# [3] "Anas platyrhynchos x rubripes/diazi/fulvigula"
# [4] "Anser caerulescens (blue form)"               
# [5] "Aphelocoma californica/woodhouseii"           
# [6] "Aphelocoma californica/woodhouseii"           
# [7] "Archilochus colubris/alexandri"               
# [8] "Ardeid sp."                                   
# [9] "Artemisiospiza nevadensis/belli"              
# [10] "Artemisiospiza nevadensis/belli"              
# [11] "Baeolophus bicolor/atricristatus"             
# [12] "Baeolophus inornatus/ridgwayi"                
# [13] "Bombycilla garrulus/cedrorum"                 
# [14] "Brotogeris versicolurus/chiriri"              
# [15] "Bucephala clangula/islandica"                 
# [16] "Cardinalis cardinalis/sinuatus"               
# [17] "Carduelis flammea/hornemanni"                 
# [18] "Carpodacus purpureus/cassinii/mexicanus"      
# [19] "Chordeiles acutipennis/minor"                 
# [20] "Chordeiles minor/gundlachii"                  
# [21] "Coccyzus americanus/erythropthalmus"          
# [22] "Colaptes auratus auratus x auratus cafer"     
# [23] "Coragyps/Cathartes atratus/aura"              
# [24] "Corvus brachyrhynchos/caurinus"               
# [25] "Corvus brachyrhynchos/ossifragus"             
# [26] "Corvus cryptoleucus/corax"                    
# [27] "Cyanecula svecica"                            
# [28] "Dendragapus obscurus/fuliginosus"             
# [29] "Empidonax alnorum/traillii"                   
# [30] "Empidonax alnorum/traillii"                   
# [31] "Empidonax difficilis/occidentalis"            
# [32] "Empidonax hammondii/oberholseri"              
# [33] "Icterus bullockii/galbula"                    
# [34] "Limnodromus griseus/scolopaceus"              
# [35] "Loxia curvirostra/leucoptera"                 
# [36] "Loxia sinesciuris/curvirostra"                
# [37] "Molothrus aeneus/ater"                        
# [38] "Nannopterum auritus/carbo"                    
# [39] "Nannopterum brasilianum/auritum"              
# [40] "Nycticorax/Nyctanassa nycticorax/violacea"    
# [41] "Passerina amoena x cyanea"                    
# [42] "Petrochelidon pyrrhonota/fulva"               
# [43] "Pipilo maculatus/erythrophthalmus"            
# [44] "Plegadis falcinellus/chihi"                   
# [45] "Poecile atricapillus/gambeli"                 
# [46] "Poecile atricapillus/hudsonicus"              
# [47] "Poecile carolinensis/atricapillus"            
# [48] "Polioptila caerulea/melanura"                 
# [49] "Porphyrio martinicus"                         
# [50] "Quiscalus major/mexicanus"                    
# [51] "Rallus crepitans/elegans"                     
# [52] "Selasphorus rufus/sasin"                      
# [53] "Setophaga coronata audoboni"                  
# [54] "Setophaga townsendi x occidentalis"           
# [55] "Setophaga townsendi/occidentalis"             
# [56] "Tringa melanoleuca/flavipes"                  
# [57] "Trochilid sp."                                
# [58] "Troglodytes pacificus/hiemalis"               
# [59] "Turdus eunomus"                               
# [60] "Tyrannus melancholicus/couchii"               
# [61] "Tyrannus vociferans/verticalis"               
# [62] "Uria aalge/lomvia"                            
# [63] "Urile sp."                                    
# [64] "Vermivora chrysoptera x cyanoptera"           
# [65] "Vermivora cyanoptera x chrysoptera"           
# [66] "Vireo cassinii/solitarius"                    
# [67] "Vireo plumbeus/cassinii" 

# Now look each of these up in the CHECKLIST object - if it doesn't exist it
# could need a different genus_species (to match CHECKLIST). If it exists and
# doesn't have a common_name, that's fine; we will use the BBS2024.common_name.
# Note that we are using genus_species.bbs2024 as the lookup - it has spaces
# between "/".

# [1] "Aechmophorus occidentalis/clarkii"            
# [2] "Aechmophorus occidentalis/clarkii"            
bbs.splist$genus_species[bbs.splist$genus_species.bbs2024 == "Aechmophorus occidentalis / clarkii"] <- "Aechmophorus occidentalis/clarkii"
bbs.splist$common_name[bbs.splist$genus_species.bbs2024 == "Aechmophorus occidentalis / clarkii"] <- "Western/Clark's Grebe"
# According to checklist: move to Acanthis genus
# [17] "Carduelis flammea/hornemanni"           
bbs.splist$genus_species[bbs.splist$genus_species.bbs2024 == "Carduelis flammea / hornemanni"] <- "Acanthis flammea [flammea Group/hornemanni/exilipes]"

# Checklist says: House and Cassin's and Purple finch moved to Haemorhous Genus
# [18] "Carpodacus purpureus/cassinii/mexicanus"  
bbs.splist$genus_species[bbs.splist$genus_species.bbs2024 == "Carpodacus purpureus / cassinii / mexicanus"] <- "Haemorhous purpureus / cassinii / mexicanus"

## EXTRA CHANGES: Checklist says: these plovers moved to Anarhynchus montanus
# # "Charadrius montanus"                              
# # "Charadrius nivosus"                               
# # "Charadrius wilsonia"                              
bbs.splist$genus_species[bbs.splist$genus_species.bbs2024 == "Charadrius montanus"] <- "Anarhynchus montanus"
bbs.splist$genus_species[bbs.splist$genus_species.bbs2024 == "Charadrius nivosus"] <- "Anarhynchus nivosus"
bbs.splist$genus_species[bbs.splist$genus_species.bbs2024 == "Charadrius wilsonia"] <- "Anarhynchus wilsonia"
 
# Checklist changes are made below:
# "Colaptes auratus auratus"                         
# "Colaptes auratus auratus" All Flickers or Yellow                        
bbs.splist$genus_species[bbs.splist$genus_species.bbs2024 == "Colaptes auratus auratus"] <- "Colaptes auratus"
# [22] "Colaptes auratus auratus x auratus cafer"    Yellow x Red     
bbs.splist$genus_species[bbs.splist$genus_species.bbs2024 == "Colaptes auratus auratus x auratus cafer"] <- "Colaptes auratus luteus x cafer"
# "Colaptes auratus cafer"       Red-shafted                    
bbs.splist$genus_species[bbs.splist$genus_species.bbs2024 == "Colaptes auratus cafer"] <- "Colaptes auratus [cafer Group]"

# Checklist says: Corvus brachyrhynchos caurinus
# "Corvus caurinus"                                  
bbs.splist$genus_species[bbs.splist$genus_species.bbs2024 == "Corvus caurinus"] <- "Corvus brachyrhynchos caurinus"

# Checklist says: Luscinia svecica cyanecula
# "Cyanecula svecica"                                
bbs.splist$genus_species[bbs.splist$genus_species.bbs2024 == "Cyanecula svecica"] <- "Luscinia svecica cyanecula"

# Checklist says moved Ixobrychus into Genus Botaurus
# "Ixobrychus exilis"                                
bbs.splist$genus_species[bbs.splist$genus_species.bbs2024 == "Ixobrychus exilis"] <- "Botaurus exilis"

# Checklist says Junco hyemalis hyemalis/carolinensis but also exists in
# Checklist as Junco hyemalis hyemalis without common_name so keep as is. Same
# with Junco hyemalis [oreganus Group].

# [70] "Junco hyemalis hyemalis"                          
# [71] "Junco hyemalis oreganus"         

# Checklist says Phalacrocorax carbo/Nannopterum auritum
# "Nannopterum auritus / carbo"                      
bbs.splist$genus_species[bbs.splist$genus_species.bbs2024 == "Nannopterum auritus / carbo"] <- "Phalacrocorax carbo/Nannopterum auritum"

# Checklist switched order: Nannopterum auritum/brasilianum
# "Nannopterum brasilianum / auritum"                

# Checklist says Nycticorax nycticorax/Nyctanassa violacea
# "Nycticorax / Nyctanassa nycticorax / violacea"    
bbs.splist$genus_species[bbs.splist$genus_species.bbs2024 == "Nycticorax / Nyctanassa nycticorax / violacea"] <- "Nycticorax nycticorax/Nyctanassa violacea"

# Checklist says: Porphyrio martinica
# "Porphyrio martinicus"                             
bbs.splist$genus_species[bbs.splist$genus_species.bbs2024 == "Porphyrio martinicus"] <- "Porphyrio martinica"

# Checklist says: Spilopelia chinensis  Spotted Dove
# "Streptopelia chinensis"                           
bbs.splist$genus_species[bbs.splist$genus_species.bbs2024 == "Streptopelia chinensis"] <- "Streptopelia chinensis"

# Checklist says: Vermivora chrysoptera x cyanoptera (F2 backcross) is Lawrence's Warbler
# But also checklist says Vermivora chrysoptera x cyanoptera is 	
# Golden-winged x Blue-winged Warbler (hybrid).            
# "Vermivora chrysoptera x cyanoptera"               
bbs.splist$genus_species[bbs.splist$genus_species.bbs2024 == "Vermivora chrysoptera x cyanoptera"] <- "Vermivora chrysoptera x cyanoptera (F2 backcross)"

# Checklist says: Vermivora chrysoptera x cyanoptera (F1 hybrid) is Brewster's Warbler
# "Vermivora cyanoptera x chrysoptera"               
bbs.splist$genus_species[bbs.splist$genus_species.bbs2024 == "Vermivora cyanoptera x chrysoptera"] <- "Vermivora chrysoptera x cyanoptera (F1 hybrid)"

# FOR BBS: Accept the bbs.splist$bbs_common_name as our common_name for now
# because there are blanks for the CHECKLIST-sourced common_name. 
# **** NOTE: for full North America list, should fix these common names to 
# ensure they exist in an updated CHECKLIST.
names(bbs.splist)[names(bbs.splist) == "common_name"] <-"gbif.chklist.common_name"
names(bbs.splist)[names(bbs.splist) == "bbs_common_name"] <-"common_name"

# Simplify the bbs.splist so that we remove duplicate rows
bbs.splist.final<-subset(bbs.splist,select=c("genus_species","AOU","AOU.combo",
                                             "genus_species.combo","common_name",
                                             "genus_species.raw"))

# Remove duplicate rows
dim(bbs.splist.final)
# 974 rows
bbs.splist.final <- bbs.splist.final %>% 
  distinct()
dim(bbs.splist.final)
# 972 rows - none lost; retains different genus_species.raw
length(unique(bbs.splist.final$genus_species.raw))
# 896 rows - some are NA bc they are BBS species that do not appear in int.raw
bbs.splist.final.NA<-bbs.splist.final[which(is.na(bbs.splist.final$genus_species.raw)), ]
dim(bbs.splist.final.NA)
# 65

#*** CLEAN UP genus_species.combo ***#
# For each row, if AOU < 5 digits, copy the genus_species and paste it into 
# genus_species.combo.
bbs.splist.final <- bbs.splist.final %>%
  mutate(
    genus_species.combo = ifelse(nchar(AOU.combo) < 5, genus_species, genus_species.combo)
  )

# BBS: Use bbs.splist as a lookup table to update int.raw with BBS/Clements/eBird
# names for all species1_scientific and species2_scientific. 
# Make a version just for this BBS work. 
int.raw.bbs<-int.raw

# int.checklist$genus_species[int.checklist$genus_species.edit == "Accipiter cooperii"] <- "Astur cooperii"
# int.checklist$genus_species[int.checklist$genus_species.edit == "Accipiter gentilis laingi"] <- "Astur atricapillus laingi"
# int.checklist$genus_species[int.checklist$genus_species.edit == "Accipiter melanoleucus"] <- "Astur melanoleucus"
# int.checklist$genus_species[int.checklist$genus_species.edit == "Accipiter sp."] <- "Aerospiza/Tachyspiza/Accipiter/Astur sp."
# int.checklist$common_name[int.checklist$genus_species.edit == "Accipiter striatus venator"] <- "Sharp-Shinned Hawk (Puerto Rican)"

# Make new copy columns to maintain raw entries: species1_scientific.raw,
# species2_scientific.raw, species1_common.raw, species2_common.raw
int.raw.bbs$species1_scientific.raw<-int.raw.bbs$species1_scientific
int.raw.bbs$species2_scientific.raw<-int.raw.bbs$species2_scientific
int.raw.bbs$species1_common.raw<-int.raw.bbs$species1_common
int.raw.bbs$species2_common.raw<-int.raw.bbs$species2_common
# Check a section that has species name changes. 
int.raw.bbs[c(53:60),c(1:4,26:29)]

# Identify duplicates in the join keys
duplicates <- int.raw.bbs %>%
  inner_join(
    bbs.splist.final %>% count(genus_species.raw) %>% filter(n > 1),
    by = c("species1_scientific.raw" = "genus_species.raw")
  ) %>%
  distinct(species1_scientific.raw)

# Display the duplicates
print("Duplicate matches detected in the join:")
print(duplicates)
# [1] "Duplicate matches detected in the join:"
# > print(duplicates)
# species1_scientific.raw
# 1          Anas platyrhynchos
# 2         Anas platyrhynchos 
# 3              Ardea herodias
# 4              Junco hyemalis
# 5           Buteo jamaicensis
# 6    Colaptes auratus auratus
# 7       Corvus brachyrhynchos
# 8 Setophaga coronata coronata

# Assign the correct genus_species and common_name for all species1_scientific.
# First resolve duplicate matches by selecting rows with AOU < 5 digits
bbs.splist.final_no_duplicates <- bbs.splist.final %>%
  group_by(genus_species.raw) %>%
  filter(n() == 1 | (nchar(AOU) < 5 & n() > 1)) %>%
  ungroup()

# Also pull in the AOU, AOU.combo, and genus_species.combo to ensure that these
# data are in the same dataset (renamed for sp1). Track which changes were made
# and which species1_scientific were left as is:

# Join with int.raw.bbs and create the requested columns
int.raw.bbs <- int.raw.bbs %>%
  left_join(
    bbs.splist.final_no_duplicates %>%
      select(genus_species.raw, genus_species, AOU, AOU.combo, genus_species.combo, common_name),
    by = c("species1_scientific.raw" = "genus_species.raw")
  ) %>%
  mutate(
    # Overwrite species1_scientific if a match is found
    species1_scientific = ifelse(!is.na(genus_species), genus_species, species1_scientific),
    # Overwrite species1_common if a match is found
    species1_common = ifelse(!is.na(common_name), common_name, species1_common),
    
    # Create new columns for associated data
    AOU.sp1 = ifelse(!is.na(genus_species), AOU, NA),
    AOU.combo.sp1 = ifelse(!is.na(genus_species), AOU.combo, NA),
    genus_species.combo.sp1 = ifelse(!is.na(genus_species), genus_species.combo, NA),
    
    # Create a column to indicate if a match was made
    sp1sci.replaced = ifelse(!is.na(genus_species), "yes", "no")
  ) %>%
  # Remove temporary join columns
  select(-genus_species, -AOU, -AOU.combo, -genus_species.combo, -common_name)

# Assign the correct genus_species and common_name for all species2_scientific.
# Also pull in the AOU, AOU.combo, and genus_species.combo to ensure that these
# data are in the same dataset (renamed for sp2). Track which changes were made
# and which species2_scientific were left as is:

# Join with int.raw.bbs and create the requested columns
int.raw.bbs <- int.raw.bbs %>%
  left_join(
    bbs.splist.final_no_duplicates %>%
      select(genus_species.raw, genus_species, AOU, AOU.combo, genus_species.combo, common_name),
    by = c("species2_scientific.raw" = "genus_species.raw")
  ) %>%
  mutate(
    # Overwrite species2_scientific if a match is found
    species2_scientific = ifelse(!is.na(genus_species), genus_species, species2_scientific),
    # Overwrite species2_common if a match is found
    species2_common = ifelse(!is.na(common_name), common_name, species2_common),
    
    # Create new columns for associated data
    AOU.sp2 = ifelse(!is.na(genus_species), AOU, NA),
    AOU.combo.sp2 = ifelse(!is.na(genus_species), AOU.combo, NA),
    genus_species.combo.sp2 = ifelse(!is.na(genus_species), genus_species.combo, NA),
    
    # Create a column to indicate if a match was made
    sp2sci.replaced = ifelse(!is.na(genus_species), "yes", "no")
  ) %>%
  # Remove temporary join columns
  select(-genus_species, -AOU, -AOU.combo, -genus_species.combo, -common_name)

# Optionally, view rows with multiple matches
# int.raw.bbs %>% filter(!is.na(replacement_report))

int.raw.bbs[c(53:60),c(1:4,26:29)]

# Save subset that has no AOU match and check for BBS species:
# Subset rows with missing AOU.sp1 or AOU.sp2
int.raw.bbs_subset <- int.raw.bbs %>%
  filter(is.na(AOU.sp1) | is.na(AOU.sp2))

write.csv(int.raw.bbs_subset,file.path(L1_dir,"int.raw.bbs_subset.csv"),row.names=F)

# Look at int.raw.bbs_subset - scroll through for any sp1 or sp2 that is
# obviously North American... look at its interactor
# Issues (need to edit these like we did above for Accipiter gentilis in the
# bbs.splist before merging)
# 	

# Simplify the data back to fewer columns
int.namefix.bbs<-int.raw.bbs
int.namefix.bbs$species1_scientific.raw<-NULL

  
save.image(file.path(L1_dir,"AvianInteractionData_L1.RData"))


# PARKING LOT FOR CONTINUED CLEANING...

# Perform a left join to match and update `genus_species`
bbs.splist <- bbs.splist %>%
  left_join(
    int.checklist.narrow %>%
      select(genus_species.edit, genus_species.checklist = genus_species),
    by = c("genus_species.2024bbs" = "genus_species.edit")
  ) %>%
  mutate(
    genus_species = ifelse(
      !is.na(genus_species.checklist),
      genus_species.checklist,
      genus_species
    )
  ) %>%
  select(-genus_species.checklist)

# Hoary Redpoll lumped into Redpoll, but not sure this is the case in the BBS...
# bbs.splist$genus_species[bbs.splist$genus_species.bbs2024 == "Acanthis
# flammea"] <- "Acanthis flammea hornemanni"


# # Find genus_species in bbs.splist and match it to genus_species.raw in int.checklist.narrow in order to  to ensure that CHECKLIST name changes are included in the BBS splist
# # Add a new column `genus_species.update` to bbs.splist
# # Update bbs.splist with genus_species.update
# bbs.splist <- bbs.splist %>%
#   left_join(lookup, by = c("bbs_genus_species" = "genus_species.raw")) %>%
#   mutate(
#     genus_species.update = ifelse(!is.na(genus_species), genus_species, bbs_genus_species)
#   ) %>%
#   select(-genus_species)



# First fix the Accipiters







# PARKING LOT IS BELOW FOR EXTRA CODE

#************************************************************#
#*#### Scientific Name Changes: GBIF Resolved Names work ####
#************************************************************#

# See whether the resolved.gbif have any further issues among the columns
# common_name has some NAs - check these; they seem to be duplicates but missing
# common_name
test<-resolved.gbif %>% 
  mutate(comparison = if_else(
    as.character(genus_species) == as.character(accepted_scientific_name), "equal", "different"))

head(test[which(test$comparison=="different"), ])

# Apply the function, find_closest_match_with_cleaning to the resolved.gbif data

# This takes many minutes to run; only do for genus_species
# Resolve genus_species matches (same code as above except for different dataframe)
genus_species_matches <- resolved.gbif %>%
  rowwise() %>%
  mutate(
    closest_genus_species_match = find_closest_match_with_cleaning(genus_species, checklist$genus_species)$match,
    genus_species_match_score = find_closest_match_with_cleaning(genus_species, checklist$genus_species)$score
  ) %>%
  ungroup()

# # Resolve common_name matches 
# common_name_matches <- resolved.gbif %>%
#   rowwise() %>%
#   mutate(
#     closest_common_name_match = find_closest_match_with_cleaning(common_name, checklist$common_name)$match,
#     common_name_match_score = find_closest_match_with_cleaning(common_name, checklist$common_name)$score
#   ) %>%
#   ungroup()

# Combine results
final_matches <- genus_species_matches #%>%
# left_join(
#   common_name_matches %>%
#     select(genus_species, common_name, closest_common_name_match, common_name_match_score),
#   by = c("genus_species", "common_name")
# )

# Adjust so that columns are grouped close by for easier reference
final_matches <- final_matches %>%
  select(
    genus_species, closest_genus_species_match,
    #    common_name, closest_common_name_match,
    everything()  # Keep the remaining columns in their original order
  )

#************************************************************#
#### Scientific Name Changes: High & Low Confidence Matches ####
#************************************************************#

# Extract final_matches with high confidence (genus_species_match_score > 0.90)
high_confidence_matches <- final_matches %>%
  filter(
    genus_species_match_score >= 0.9 
  )

# Define range for printing in increments of 0.005; use the genus_species match
score_start <- min(high_confidence_matches$genus_species_match_score)
score_end <- max(high_confidence_matches$genus_species_match_score)
increment <- 0.005

# Loop through each score range and print the matches within that range
for (i in seq(score_start, score_end, by = increment)) {
  current_range <- high_confidence_matches %>%
    filter(genus_species_match_score >= i & genus_species_match_score <= i + increment)
  
  # Print the current range if it has any entries
  if (nrow(current_range) > 0) {
    cat("\nMatch Score Range:", sprintf("%.2f", i), "to", sprintf("%.2f", i + increment), "\n")
    print(current_range, n=100) # print up to 100 rows in a section
  }
}

# For most of the names, the match is 1 or quite high, so assign them the
# closest match, then edit the few below noted "KEEP ORIGINAL".
fixed_names1<-merge(unresolved.gbif,high_confidence_matches, by=c("genus_species","common_name"))

# Rename the current genus_species and common_name to ".orig", and assign
# genus_species and common_name to the closest_matches
names(fixed_names1)[names(fixed_names1) == "genus_species"] <-"genus_species.raw"
names(fixed_names1)[names(fixed_names1) == "common_name"] <-"common_name.orig"
names(fixed_names1)[names(fixed_names1) == "closest_genus_species_match"] <-"genus_species"
names(fixed_names1)[names(fixed_names1) == "closest_common_name_match"] <-"common_name"

# # Resolve common_name matches - UNCOMMENT TO INCLUDE - but not necessary bc just
# working on fixing genus species, then assign common name from checklist.
# common_name_matches <- unresolved.gbif %>%
#   rowwise() %>%
#   mutate(
#     closest_common_name_match = find_closest_match_with_cleaning(common_name, checklist$common_name)$match,
#     common_name_match_score = find_closest_match_with_cleaning(common_name, checklist$common_name)$score
#   ) %>%
#   ungroup()

# # Combine results
# final_matches <- genus_species_matches %>%
#   left_join(
#     common_name_matches %>%
#       select(genus_species, common_name, closest_common_name_match, common_name_match_score),
#     by = c("genus_species", "common_name")
#   )
