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
# DATE:           27 Oct 2022; updated through 9 Aug 2024  
# NOTES:          Next script to run: 
#                 This script is used to refine species name changes to align with BOW, 
#                 and to create AvianInteractionData_L1.csv 
#                 L0 data are checked to assign BOW scientific and common names 
#                 to the interaction pairs data (which were originally from BBS species list).
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

# Read in csv with avian interactions from primary, secondary cavity nesting
# birds in North America.
int.raw<-read.csv(file.path(L0_dir,"AvianInteractionData_L0.csv"))

# Read in species list: all species in BBS (the 2024 release which includes all
# species as of 2023, plus the additional AOUcombo.index column for use w BBS
# index)
splist<-read.csv(file.path(L1_dir,"bbs_splist_2024_L1.csv"))

# Read in our look-up table with the different bbs & bow & old names for species
namechg<-read.csv(file.path(L0_dir,"bbsbow_names.csv"))

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

# Rename some columns and omit others; indicate that the common name is coming
# from BBS Species List
names(splist)[names(splist) == "genus_species"] <-"species1_scientific"
names(splist)[names(splist) == "English_Common_Name"] <-"bbs_sp1_common"
names(splist)[names(splist) == "AOU"] <-"sp1_AOU"
names(splist)[names(splist) == "AOU.combo"] <-"sp1AOU.combo"
names(splist)[names(splist) == "genus_species.combo"] <-"sp1sci.combo"

# Reference the bbsbow_names data to make initial changes to any 
# "other_or_old_bow" names that might appear.
# Apply changes only to the species1_scientific and species2_scientific columns.
# First omit any rows with a blank in "other_or_old_bow"
dim(namechg)
namechg.orig <- namechg
namechg<-namechg[!(is.na(namechg$other_or_old_bow) | namechg$other_or_old_bow==""), ]
dim(namechg)

# Create unique genus_species and common_name pairs with formatting adjustments.
# This code removes blank spaces, capitalizes, and keeps unique rows.
int.raw.names <- int.raw %>%
  # Select and stack the relevant species columns
  select(species1_scientific, species1_common, species2_scientific, species2_common) %>%
  transmute(
    genus_species = species1_scientific,
    common_name = species1_common
  ) %>%
  bind_rows(
    int.raw %>%
      transmute(
        genus_species = species2_scientific,
        common_name = species2_common
      )
  ) %>%
  # Remove duplicates and clean up formatting
  distinct() %>%
  mutate(
    # Remove extra beginning and end spaces from both columns
    genus_species = str_trim(genus_species),
    common_name = str_trim(common_name),
    
    # Format genus_species: capitalize first word, lowercase subsequent words
    genus_species = ifelse(
      str_starts(genus_species, "unid."),  # Exception case
      str_replace(genus_species, "(unid\\.)\\s*(\\w+)", "\\1 \\U\\2"),
      str_to_sentence(genus_species)       # Regular case
    ),
    
    # Replace "spp." with "sp."
    genus_species = str_replace(genus_species, "\\bspp\\.\\b", "sp."),
    
    # Capitalize each word in common_name
    common_name = str_to_title(common_name),
    
    # Replace "unid" with "unid." if found without a period
    common_name = str_replace_all(common_name, "\\bunid\\b", "unid.")
  ) %>%
# Remove rows where genus_species is <NA>
filter(!(is.na(genus_species)))

# Display the resulting cleaned dataframe of the species from interaction data
head(int.raw.names)
#         genus_species      common_name
# 1    Acanthis flammea   Common Redpoll
# 2 Acanthis hornemanni    Hoary Redpoll
# 3  Accipiter cooperii    Cooper's Hawk
# 4  Accipiter gentilis Northern Goshawk
# 5  Accipiter gentilis Northern Goshawk
# 6  Accipiter gentilis          Goshawk
#**************************************************#
#### Scientific Name Checking: taxadb with GBIF ####
#**************************************************#

# taxadb package: Modified from this code: https://docs.ropensci.org/taxadb/articles/intro.html
# In previous code, tried ITIS and COL to see if they were better than GBIF.
# GBIF has the fewest NA values, so we are sticking with it (it is the most comprehensive).

# Create a local GBIF database 
td_create("gbif")

# Resolve scientific names
intbird.names <- int.raw.names %>%
  mutate(
    scientific_id = get_ids(genus_species, "gbif"),
    accepted_scientific_name = get_names(scientific_id, "gbif")
  ) 

# These species have >1 identifier and need to be resolved w BOW info:
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
head(int.raw[which(int.raw$species1_scientific == "Heteromyias armiti"), ])
head(int.raw[which(int.raw$species2_scientific == "Heteromyias armiti"), ])
checklist[which(checklist$genus_species == "Heteromyias armiti"), ]

# KEEP ORIGINAL - Chloris sinica - Oriental Greenfinch
# checklist common_name: Oriental Greenfinch
head(int.raw[which(int.raw$species1_scientific == "Chloris sinica"), ])
head(int.raw[which(int.raw$species2_scientific == "Chloris sinica"), ])
checklist[which(checklist$genus_species == "Chloris sinica"), ]

# KEEP ORIGINAL - Passer cinnamomeus - Russet Sparrow
# checklist common_name: Russet Sparrow
head(int.raw[which(int.raw$species1_scientific == "Passer cinnamomeus"), ])
head(int.raw[which(int.raw$species2_scientific == "Passer cinnamomeus"), ])
checklist[which(checklist$genus_species == "Passer cinnamomeus"), ]

# CHANGE TO NEW NAME below: Dendroica pinus - Pine Warbler 
# checklist common_name: Pine Warbler; genus_species = Setophaga pinus
head(int.raw[which(int.raw$species1_scientific == "Dendroica pinus"), ])
head(int.raw[which(int.raw$species2_scientific == "Dendroica pinus"), ])
checklist[which(checklist$genus_species == "Dendroica pinus"), ]
checklist[which(checklist$genus_species == "Setophaga pinus"), ]

#************************************************************#
#### Scientific Name Changes: Resolved & Unresolved Names ####
#************************************************************#

# Separate resolved and unresolved names based on specified criteria
resolved_names <- intbird.names %>%
  filter(!is.na(scientific_id) | grepl(" sp\\.$", genus_species))

unresolved_names <- intbird.names %>%
  filter(is.na(scientific_id) & !grepl(" sp\\.$", genus_species))

# Display both results
head(resolved_names)
head(unresolved_names)

# Remove any duplicates
resolved_names <- resolved_names %>% 
  distinct()
dim(resolved_names)
# 3836 unique resolved as of Nov. 26, 2024

unresolved_names <- unresolved_names %>% 
  distinct()
dim(unresolved_names)
# 295 unique unresolved as of Nov. 26, 2024

# Work with unresolved_names to try and determine what misspellings exist, and what they should be.

# Reference list of scientific names eBird Clements checklist 2024
reference_names <- checklist$genus_species 

# Use fuzzy logic function to find closest match from the reference list
# Function to find closest match and return both the match and the similarity score
find_closest_match_with_score <- function(name, reference_list) {
  # Calculate string distances
  distances <- stringdist::stringdist(name, reference_list, method = "jw")  # Jaro-Winkler distance
  closest_match_index <- which.min(distances)
  closest_match <- reference_list[closest_match_index]
  similarity_score <- 1 - distances[closest_match_index]  # Convert distance to similarity (1 = exact match)
  
  return(list(match = closest_match, score = similarity_score))
}

# Check spelling in the genus_species column and suggest corrections with similarity score
misspelled_species <- unresolved_names %>%
  filter(!genus_species %in% reference_names) %>%
  distinct(genus_species) %>%
  rowwise() %>%
  mutate(
    closest_match = find_closest_match_with_score(genus_species, reference_names)$match,
    match_score = find_closest_match_with_score(genus_species, reference_names)$score
  ) %>%
  ungroup()

#************************************************************#
#### Scientific Name Changes: High Confidence Matches ####
#************************************************************#

# Extract misspelled_species with high confidence (match_score > 0.90)
high_confidence_matches <- misspelled_species %>%
  filter(match_score > 0.90)

# Extract misspelled_species with lower confidence (match_score <= 0.90) for further checking
low_confidence_matches <- misspelled_species %>%
  filter(match_score <= 0.90)

# Define range for printing in increments of 0.005
score_start <- min(high_confidence_matches$match_score)
score_end <- max(high_confidence_matches$match_score)
increment <- 0.005

# Loop through each score range and print the matches within that range
for (i in seq(score_start, score_end, by = increment)) {
  current_range <- high_confidence_matches %>%
    filter(match_score >= i & match_score <= i + increment)
  
  # Print the current range if it has any entries
  if (nrow(current_range) > 0) {
    cat("\nMatch Score Range:", sprintf("%.2f", i), "to", sprintf("%.2f", i + increment), "\n")
    print(current_range, n=50)
  }
}

# For most of the names, assign them the closest match, then edit the few below noted "KEEP ORIGINAL".
fixed_names1<-merge(unresolved_names,high_confidence_matches, by=c("genus_species"))

# Rename the current genus_species, and assign genus_species to the closest_match
names(fixed_names1)[names(fixed_names1) == "genus_species"] <-"genus_species.orig"
names(fixed_names1)[names(fixed_names1) == "closest_match"] <-"genus_species"
names(fixed_names1)[names(fixed_names1) == "common_name"] <-"common_name.orig"

save.image(file.path(L1_dir,"AvianInteractionData_L1.RData"))

# Scroll through these 180 High Confidence Matches. All look okay except:
# genus_species                    closest_match                 match_score

# KEEP ORIGINAL and change to species later when lumping subspecies
# checklist: Parkesia noveboracensis; 
# int.raw and unresolved_names: common_name = hybrid Barnacle x Bar-headed Goose
# BOW says the hybrid exists but is rare
# 1 Branta leucopsis x anser indicus Branta leucopsis x canadensis       0.905
int.raw[which(int.raw$species1_scientific == "Branta leucopsis x anser indicus"), ]
int.raw[which(int.raw$species2_scientific == "Branta leucopsis x anser indicus"), ]
unresolved_names[which(unresolved_names$genus_species == "Branta leucopsis x anser indicus"), ]
fixed_names1$genus_species[fixed_names1$genus_species.orig == "Branta leucopsis x anser indicus"] <- "Branta leucopsis x Anser indicus"

# KEEP ORIGINAL and change to species later when lumping subspecies
# checklist: Cuculus canorus; common_name = Common Cuckoo
# BOW says "sometimes separated subspecifically as telephonus on basis of
# size (smaller than subtelephonus) and pale plumage (like subtelephonus), but
# birds in this area are not constant in these characters and overlap with other
# races occurs"
# 2 Cuculus canorus telephonus       Cuculus canorus subtelephonus       0.908
int.raw[which(int.raw$species1_scientific == "Cuculus canorus telephonus"), ]
int.raw[which(int.raw$species2_scientific == "Cuculus canorus telephonus"), ]
fixed_names1$genus_species[fixed_names1$genus_species.orig == "Cuculus canorus telephonus"] <- "Cuculus canorus telephonus"

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
fixed_names1$genus_species[fixed_names1$genus_species.orig == "Aphelocoma woodhouseii suttoni"] <- "Aphelocoma woodhouseii suttoni"

# KEEP ORIGINAL and add period after sp
# 4 Strigidae sp                     Strigidae                             0.917
fixed_names1$genus_species[fixed_names1$genus_species.orig == "Strigidae sp"] <- "Strigidae sp."

#************************************************************#
#### Scientific Name Changes: Low Confidence Matches ####
#************************************************************#

# Then check the Low Confidence Matches:
# Define range for printing in increments of 0.005
score_start <- min(low_confidence_matches$match_score)
score_end <- max(low_confidence_matches$match_score)
increment <- 0.005

# Loop through each score range and print the matches within that range
for (i in seq(score_start, score_end, by = increment)) {
  current_range <- low_confidence_matches %>%
    filter(match_score >= i & match_score <= i + increment)
  
  # Print the current range if it has any entries
  if (nrow(current_range) > 0) {
    cat("\nMatch Score Range:", sprintf("%.2f", i), "to", sprintf("%.2f", i + increment), "\n")
    print(current_range, n=60)
  }
}
dim(low_confidence_matches)

# For most of the names, assign them the closest match, then edit the few above noted "KEEP ORIGINAL".
fixed_names2<-merge(unresolved_names,low_confidence_matches, by=c("genus_species"))

# Rename the current genus_species, and assign genus_species to the closest_match
names(fixed_names2)[names(fixed_names2) == "genus_species"] <-"genus_species.orig"
names(fixed_names2)[names(fixed_names2) == "closest_match"] <-"genus_species"
names(fixed_names2)[names(fixed_names2) == "common_name"] <-"common_name.orig"

# Scroll through these 55 Low Confidence Matches in order from highest to lowest
# match score. Many look okay except check these:

# genus_species                     closest_match           match_score

# CHANGE TO CLOSEST MATCH because BOW states "No subspecies, following Eaton
# (1957a) and Molina et al. (2000).". Common name in int.raw is Northern Waterthrush.
# 1 Parkesia noveboracensis notabilis Parkesia noveboracensis       0.899
int.raw[which(int.raw$species1_scientific == "Parkesia noveboracensis notabilis"), ]
int.raw[which(int.raw$species2_scientific == "Parkesia noveboracensis notabilis"), ]

# KEEP ORIGINAL but edit it. This is "Rock Dove" which is also known as Rock
# Pigeon. Should be Columba livia.
# 3 Columbina livia              Columbina inca                      0.886
int.raw[which(int.raw$species1_scientific == "Columbina livia"), ]
int.raw[which(int.raw$species2_scientific == "Columbina livia"), ]
fixed_names2$genus_species[fixed_names2$genus_species.orig == "Columbina livia"] <- "Columba livia"

# CHANGE TO CLOSEST MATCH: BOW states no subspecies: "No subspecies, following
# Parkes (1954), who could not diagnose a difference between northeastern
# breeders and those farther west, which were named S. s. lurida (Burleigh and
# Peters, 1948)."
# 1 Setophaga striata / tigrina Setophaga striata       0.877

# KEEP ORIGINAL and edit: Common name in int.raw is Hairy Woodpecker, so
# genus is off. Dryobates villosus
# 3 Leucophaeus villosus                   Leucophaeus modestus          0.867
int.raw[which(int.raw$species1_scientific == "Leucophaeus villosus"), ]
int.raw[which(int.raw$species2_scientific == "Leucophaeus villosus"), ]
fixed_names2$genus_species[fixed_names2$genus_species.orig == "Leucophaeus villosus"] <- "Dryobates villosus"

# KEEP ORIGINAL, Common name in int.raw is the White Wagtail. 
# 1 Moticilla alba            Motacilla citreola             0.858
fixed_names2$genus_species[fixed_names2$genus_species.orig == "Moticilla alba"] <- "Moticilla alba"
int.raw[which(int.raw$species1_scientific == "Moticilla alba"), ]
int.raw[which(int.raw$species2_scientific == "Moticilla alba"), ]

# KEEP ORIGINAL and edit: According to BOW there is no Anas flavirostris / anas
# andium. Common name in int.raw is Speckled Teal but checklist is Andean/Yellow-billed Teal. 
# 3 Anas flavirostris / anas andium Anas flavirostris           0.849
int.raw[which(int.raw$species1_scientific == "Anas flavirostris / Anas andium"), ]
int.raw[which(int.raw$species2_scientific == "Anas flavirostris / Anas andium"), ]
fixed_names2$genus_species[fixed_names2$genus_species.orig == "Anas flavirostris / Anas andium"] <- "Anas andium/flavirostris"
# Common name will be changed later
#fixed_names2$common_name[fixed_names2$common_name.orig == "Speckled Teal"] <- "Andean/Yellow-billed Teal"

# KEEP ORIGINAL and edit. Common name in int.raw is House Finch (Haemorhous mexicanus).
# 2 Hirundo mexicanus               Todus mexicanus             0.851
int.raw[which(int.raw$species1_scientific == "Hirundo mexicanus"), ]
int.raw[which(int.raw$species2_scientific == "Hirundo mexicanus"), ]
fixed_names2$genus_species[fixed_names2$genus_species.orig == "Hirundo mexicanus "] <- "Haemorhous mexicanus"

# KEEP ORIGINAL and edit. Checklist shows genus moved to Astur. Also species is cooperii.
# 1 Accipiter cooperi               Accipiter poliogaster       0.849
fixed_names2$genus_species[fixed_names2$genus_species.orig == "Accipiter cooperi"] <- "Astur cooperii"
# 3 Accipiter spp.            Accipiter nisus                   0.840
fixed_names2$genus_species[fixed_names2$genus_species.orig == "Accipiter spp."] <- "Aerospiza/Tachyspiza/Accipiter/Astur sp."
# Listed as American Goshawk; KEEP ORIGINAL and edit to Astur
# 2 Accipiter atricapillus    Astur cooperii/atricapillus       0.839
fixed_names2$genus_species[fixed_names2$genus_species.orig == "Accipiter atricapillus"] <- "Astur atricapillus"

# KEEP ORIGINAL and edit: BOW and checklist: "Black-winged Babbler" is most likely Jungle
# Babbler (Black-winged) Argya striata somervillei 
# 1 Argya affinis somervillei  Argya affinis                     0.84
int.raw[which(int.raw$species1_scientific == "Argya affinis somervillei"), ]
int.raw[which(int.raw$species2_scientific == "Argya affinis somervillei"), ]
fixed_names2$genus_species[fixed_names2$genus_species.orig == "Argya affinis somervillei"] <- "Argya striata somervillei"
# Common name will be changed later
#fixed_names2$common_name[fixed_names2$common_name.orig == "Black-winged Babbler"] <- "Jungle Babbler (Black-winged)"

# KEEP ORIGINAL and edit. Checklist shows genus moved to Astur atricapillus. 
# 1 Accipiter getilis Accipiter poliogaster       0.839
int.raw[which(int.raw$species1_scientific == "Accipiter getilis"), ]
int.raw[which(int.raw$species2_scientific == "Accipiter getilis"), ]
fixed_names2$genus_species[fixed_names2$genus_species.orig == "Accipiter getilis"] <- "Astur atricapillus"
# **** Do common name changes below for entire Astur group

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
fixed_names2$genus_species[fixed_names2$genus_species.orig == "Chrysococcyx meyerii"] <- "Chalcites meyerii"

# KEEP ORIGINAL and edit to remove p in spp.
# 2 Molothrus spp.   Myioborus sp.          0.828
fixed_names2$genus_species[fixed_names2$genus_species.orig == "Molothrus spp."] <- "Molothrus sp."

# KEEP ORIGINAL and edit. This is the Eurasian Goshawk: was Accipter atricapillus and is now Astur 
# 1 Accipter atricapillus Accipiter striatus       0.821
int.raw[which(int.raw$species2_scientific == "Accipter atricapillus"), ]
fixed_names2$genus_species[fixed_names2$genus_species.orig == "Accipter atricapillus"] <- "Astur gentilis"

# KEEP ORIGINAL and edit: int.raw shows common name is White-throated Swifts.
# checklist and BOW: Aeronautes saxatalis and this species has interactions
# documented in int.raw with Violet-green swallows (competition).
# 3 Panyptila saxatilis  Pachyptila salvini       0.814
int.raw[which(int.raw$species2_scientific == "Panyptila saxatilis"), ]
fixed_names2$genus_species[fixed_names2$genus_species.orig == "Panyptila saxatilis"] <- "Aeronautes saxatalis"

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
fixed_names2$genus_species[fixed_names2$genus_species.orig == "Hirundo pyrrhonta"] <- "Petrochelidon pyrrhonota"

# KEEP ORIGINAL and edit: Unid. Storm Petrel 
# BOW: now Genus is Hydrobates instead of Oceanodrama
#1 Oceanodroma spp.                    Cyanoderma sp.                          0.800
int.raw[which(int.raw$species1_scientific == "Oceanodroma spp."), ]
int.raw[which(int.raw$species2_scientific == "Oceanodroma spp."), ]
fixed_names2$genus_species[fixed_names2$genus_species.orig == "Oceanodroma spp."] <- "Hydrobates sp."

# KEEP ORIGINAL and edit: Pyrrhuloxia
# BOW and checklist is Cardinalis sinuatus
# int.raw Common name = Pyrrhuloxia
#2 Pyrrhuloxia sinuata                 Pyrrhula owstoni                        0.799
int.raw[which(int.raw$species1_scientific == "Pyrrhuloxia sinuata"), ]
int.raw[which(int.raw$species2_scientific == "Pyrrhuloxia sinuata"), ]
fixed_names2$genus_species[fixed_names2$genus_species.orig == "Pyrrhuloxia sinuata"] <- "Cardinalis sinuatus"

# KEEP ORIGINAL and edit: Yellow Warbler (Mangrove) 
# BOW and checklist: Setophaga petechia [erithachorides Group]
#3 Dendroica petechia (erithachordies) Setophaga petechia erithachorides       0.801
int.raw[which(int.raw$species1_scientific == "Dendroica petechia (erithachordies)"), ]
int.raw[which(int.raw$species2_scientific == "Dendroica petechia (erithachordies)"), ]
fixed_names2$genus_species[fixed_names2$genus_species.orig == "Dendroica petechia (erithachordies)"] <- "Setophaga petechia [erithachorides Group]"

# KEEP ORIGINAL and edit: Pine Warbler
# BOW and checklist: Setophaga pinus
# 4 Dendroica pinus                     Dendrocincla sp.                        0.803 
int.raw[which(int.raw$species1_scientific == "Dendroica pinus"), ]
int.raw[which(int.raw$species2_scientific == "Dendroica pinus"), ]
fixed_names2$genus_species[fixed_names2$genus_species.orig == "Dendroica pinus"] <- "Setophaga pinus"

# KEEP ORIGINAL and edit: Collared Sparrowhawk
# BOW and checklist: Genus changed to Tachyspiza cirrocephala
# 1 Accipiter cirrhocephalus Accipiter striatus       0.797
int.raw[which(int.raw$species1_scientific == "Accipiter cirrhocephalus"), ]
int.raw[which(int.raw$species2_scientific == "Accipiter cirrhocephalus"), ]
checklist[which(checklist$common_name == "Collared Sparrowhawk"), ]
fixed_names2$genus_species[fixed_names2$genus_species.orig == "Accipiter cirrhocephalus"] <- "Tachyspiza cirrocephala"

# KEEP ORIGINAL and edit: California Quail
# BOW and checklist: California Quail; Genus changed to Callipepla californica
# 1 Lophortyx californicus Lophornis ornatus       0.788
int.raw[which(int.raw$species1_scientific == "Lophortyx californicus"), ]
int.raw[which(int.raw$species2_scientific == "Lophortyx californicus"), ]
checklist[which(checklist$common_name == "California Quail"), ]
fixed_names2$genus_species[fixed_names2$genus_species.orig == "Lophortyx californicus"] <- "Callipepla californica"

# KEEP ORIGINAL and edit; this entry has multiple columns off for scientific and common
# BOW and checklist: genus_species = Platalea ajaja
# 1 Roseate spoonbill Cormobates placens meridionalis       0.736
int.raw[which(int.raw$species1_scientific == "Roseate Spoonbill"), ]
int.raw[which(int.raw$species2_scientific == "Roseate Spoonbill"), ]
checklist[which(checklist$common_name == "Roseate Spoonbill"), ]
fixed_names2$genus_species[fixed_names2$genus_species.orig == "Roseate spoonbill"] <- "Platalea ajaja"
# Common name will be changed later
#fixed_names2$common_name[fixed_names2$genus_species.orig == "Roseate spoonbill"] <- "Roseate Spoonbill"

# Typos 
# 1 unid. 2 hawl  Turdus hauxwelli       0.674
# Also missing: unid. Accipiter hawl
int.raw[which(int.raw$species1_scientific == "unid. 2 hawl"), ]
int.raw[which(int.raw$species2_scientific == "unid. 2 hawl"), ]
int.raw[which(int.raw$species2_scientific == "unid. Accipiter hawl"), ]
fixed_names2$genus_species[fixed_names2$genus_species.orig == "unid. Accipiter hawl"] <- "Aerospiza/Tachyspiza/Accipiter/Astur sp."

save.image(file.path(L1_dir,"AvianInteractionData_L1.RData"))

#************************************************************#
#*#### Scientific Name Changes: Merging Fixed Data into Interaction Data ####
#************************************************************#

# Remove duplicate rows
fixed_names1 <- fixed_names1 %>% 
                  distinct()
fixed_names2 <- fixed_names2 %>% 
                  distinct()

fixed_names3<-rbind(fixed_names1,fixed_names2)
dim(fixed_names3)
# 239 names
fixed_names3$scientific_id<-NULL
fixed_names3$accepted_scientific_name<-NULL

# The genus_species column in resolved_names and in fixed_names3 is the corrected genus_species.
# Row-bind these together as our 
# Merge the resolved_names and the fixed_names to get a complete list of names of species in the interaction data so far.
fixed_names<-merge(resolved_names,fixed_names3,by=c("genus_species"))

# resolved_names work
# See whether the resolved_names have any further issues among the columns
# common_name has some NAs - check these; they seem to be duplicates but missing common_name
test<-resolved_names %>% 
  mutate(comparison = if_else(
    as.character(genus_species) == as.character(accepted_scientific_name), "equal", "different"))

# I checked these, and all have another row with a complete genus_species and common_name of the same genus_species
resolved_names[which(is.na(resolved_names$common_name)), ]

# Remove any rows with NA in common_name
resolved_names1 <- resolved_names %>%
  filter(!is.na(common_name))

# Take the genus_species in resolved_names and in fixed_names and rbind them -
# then merge with checklist to get common name... figure out how to keep
# reference of the genus_species.orig bc need to merge back into the int.raw
# data. can I assign genus_species.orig to resolved_names$genus_species because
# the other column is "accepted_scientific_name" and should be the fixed name?
n_occur <- data.frame(table(resolved_names1$genus_species))







# The section below, using taxize, was last run on Aug 9, 2024.
tax <-gnr_datasources()
# GBIF taxonomy ID = 11
tax[tax$title=="GBIF Backbone Taxonomy","id"]
tax[tax$title=="BirdLife International","id"]

# Detecting name misspellings in our BBSvsBOW lookup:
gbif.bl.tax.bbsbow <- namechg$bow %>%
  gnr_resolve(data_source_ids = c(11,175), 
              with_canonical_ranks=T)

# Detecting name misspellings in our BBSvsBOW lookup:
gbif.bl.tax.bbsbow <- namechg$bow %>%
  gnr_resolve(data_source_ids = c(11,175), 
              with_canonical_ranks=T)
gbif.bl.tax.bbsbow<-subset(gbif.bl.tax.bbsbow, gbif.bl.tax.bbsbow$score<0.9,)
# Omit replacements without at least genus species (omit those that don't have a space)
gbif.bl.tax.bbsbow<-gbif.bl.tax.bbsbow[grepl(" ", gbif.bl.tax.bbsbow$matched_name2), ]
# They look ok. Assign the fixed spellings to 'bow'
matching <- gbif.bl.tax.bbsbow$matched_name2[match(namechg$bow, 
                                                   gbif.bl.tax.bbsbow$user_supplied_name)]
namechg$species1_scientific <- ifelse(is.na(matching),
                                      namechg$bow,
                                      matching)

gbif.bl.tax <- int.raw.sp %>%
  gnr_resolve(data_source_ids = c(11,175), 
              with_canonical_ranks=T)
dim(gbif.bl.tax)
# 6478

# GBIF and BirdLife identified misspellings:
tax.fix<-subset(gbif.bl.tax, gbif.bl.tax$score<0.9,)
dim(tax.fix)
# 903

#Not perfect; Remove non-avian species or abbreviated sp. name or incorrect assignments
tax.fix <-tax.fix %>% filter(str_detect(user_supplied_name,' sp.', negate = T))
dim(tax.fix)
# Omit replacements without at least genus species (omit those that don't have a space)
tax.fix<-tax.fix[grepl(" ", tax.fix$matched_name2), ]

# Remove records that match the currently entered name
tax.fix<-subset(tax.fix, (as.character(user_supplied_name) != as.character(matched_name2)))

# Trust all the BirdLife matches; if some only have GBIF, they need checking manually.
tax.fix.BL<-tax.fix[tax.fix$data_source_title %in% c("BirdLife International"), ]
tax.fix.BL$bird<-"yes"
tax.fix.BL$submitted_name<-NULL
tax.fix.BL$data_source_title<-NULL
tax.fix.BL$score<-NULL
tax.fix.BL$matched_name2<-NULL

tax.fix1<-merge(tax.fix,tax.fix.BL,by=c("user_supplied_name"),all.x=T,all.y=T)

# Look at these species to make sure they are birds
tax.fix1<-tax.fix1 %>% filter(if_any(everything(), is.na))
dim(tax.fix1)
tax.fix1
# 34 species to manually check

# Final changes (a few that were incorrect in BirdLife/GBIF)
tax.fix <- tax.fix[!tax.fix$matched_name2 %in% c("Anabarhynchus montanus", # not a bird
                                                 "Helmitheros vermivorus",
                                                 "Cochlearius cochlearia",
                                                 "Vermivora cyanoptera"), ]

# checking "Anarhynchus montanus"
int.raw[825:835,1:4]

matching <- tax.fix$matched_name2[match(int.raw$species1_scientific, tax.fix$user_supplied_name)]
int.raw$species1_scientific <- ifelse(is.na(matching),
                                       int.raw$species1_scientific,
                                matching)
# checking that it changed to "Aneurhynchus montanus"
int.raw[825:835,1:4]
# Yes it works.

# Apply for species2 also:
matching <- tax.fix$matched_name2[match(int.raw$species2_scientific, tax.fix$user_supplied_name)]
int.raw$species2_scientific <- ifelse(is.na(matching),
                                      int.raw$species2_scientific,
                                      matching)

# Save the relevant files here to avoid re-running:
save.image(file.path(L1_dir,"taxize.results.9Aug2024.RData"))

### *** END OF TAXIZE CHECKING *** ###

# This section below relies on an up to date list of current names. 
# Could just decide to use the code above and call it good.

# Change names in species1_scientific and species2_scientific according to the
# look-up table so that all species1 and species2 interactors have the
# up-to-date BOW name.

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
dim(namechg.unmatch) # 10 species on Aug 8, 2024
namechg.unmatch[,1:2]

# Standardize based on BOW 

# RENAME: BBS: Streptopelia chinensis to BOW: Spilopelia chinensis
dplyr::filter(splist, species1_scientific %in% c("Streptopelia chinensis")) # in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Streptopelia chinensis")) # not in interactions
dplyr::filter(int.raw, species2_scientific %in% c("Streptopelia chinensis")) # in interactions
dplyr::filter(splist, species1_scientific %in% c("Spilopelia chinensis")) # not in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Spilopelia chinensis")) # in interactions
dplyr::filter(int.raw, species2_scientific %in% c("Spilopelia chinensis")) # in interactions
# Update all to new species name
splist$species1_scientific[splist$species1_scientific == "Streptopelia chinensis"] <- "Spilopelia chinensis"
splist$sp1sci.combo[splist$sp1sci.combo == "Streptopelia chinensis"] <- "Spilopelia chinensis"
int.raw$species1_scientific[int.raw$species1_scientific == "Streptopelia chinensis"] <- "Spilopelia chinensis"
int.raw$species2_scientific[int.raw$species2_scientific == "Streptopelia chinensis"] <- "Spilopelia chinensis"

namechg.unmatch[,1:2]
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
namechg.unmatch[,1:2]
# Recent split: Hen Harrier (Circus cyaneus; Eurasia) & Northern Harrier (Circus hudsonius; North America)
dplyr::filter(splist, species1_scientific %in% c("Circus cyaneus")) # not in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Circus cyaneus")) # not in interactions 
dplyr::filter(int.raw, species2_scientific %in% c("Circus cyaneus")) # in interactions - as Hen and Northern
# Update all N Harrier to new species name
int.raw$species1_scientific[int.raw$species1_scientific == "Circus cyaneus" & int.raw$species1_common == "Northern Harrier"] <- "Circus hudsonius"
int.raw$species1_scientific[int.raw$species1_scientific == "Circus cyaneus" & int.raw$species1_common == "Northern Harrier"] <- "Circus hudsonius"
int.raw$species1_scientific[int.raw$species1_scientific == "Circus cyaneus" & int.raw$species1_common == "northern harrier"] <- "Circus hudsonius"
int.raw$species2_scientific[int.raw$species2_scientific == "Circus cyaneus" & int.raw$species2_common == "Northern Harrier"] <- "Circus hudsonius"
int.raw$species2_scientific[int.raw$species2_scientific == "Circus cyaneus" & int.raw$species2_common == "northern harrier"] <- "Circus hudsonius"

## This is an AOU combo species:
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
dplyr::filter(int.raw, species1_scientific %in% c("Larus brachyrhynchus")) # in interactions as Short-Billed Gull
dplyr::filter(int.raw, species2_scientific %in% c("Larus brachyrhynchus")) # in interactions as Short-Billed Gull
# L. brachyrhynchus interactions - these are entered as of Dec 6, 2023. 
# Confirmed that L. canus interactions are only Eurasian interactions.
# No change needed to these data.
namechg.unmatch[,1:2]
## Spelling difference: BBS: Porphyrio martinicus = BOW: Porphyrio martinica
dplyr::filter(splist, species1_scientific %in% c("Porphyrio martinicus")) # not in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Porphyrio martinicus")) # not in interactions 
dplyr::filter(int.raw, species2_scientific %in% c("Porphyrio martinicus")) # in interactions 
dplyr::filter(splist, species1_scientific %in% c("Porphyrio martinica")) # not in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Porphyrio martinica")) # in interactions 
dplyr::filter(int.raw, species2_scientific %in% c("Porphyrio martinica")) # in interactions 
# RENAME: BBS & BOW: change Porphyrio martinicus to Porphyrio martinica 
splist$species1_scientific[splist$species1_scientific == "Porphyrio martinicus"] <- "Porphyrio martinica"
splist$sp1sci.combo[splist$sp1sci.combo == "Porphyrio martinicus"] <- "Porphyrio martinica"
int.raw$species1_scientific[int.raw$species1_scientific == "Porphyrio martinicus"] <- "Porphyrio martinica"
int.raw$species2_scientific[int.raw$species2_scientific == "Porphyrio martinicus"] <- "Porphyrio martinica"

namechg.unmatch[,1:2]
## Update name: BBS: Cyanecula svecica = BOW: Luscinia svecica
dplyr::filter(splist, species1_scientific %in% c("Cyanecula svecica")) # in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Cyanecula svecica")) # not in interactions 
dplyr::filter(int.raw, species2_scientific %in% c("Cyanecula svecica")) # not in interactions 
dplyr::filter(splist, species1_scientific %in% c("Luscinia svecica")) # not in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Luscinia svecica")) # in interactions 
dplyr::filter(int.raw, species2_scientific %in% c("Luscinia svecica")) # in interactions 
# RENAME: BBS & BOW: change Cyanecula svecica to Luscinia svecica
splist$species1_scientific[splist$species1_scientific == "Cyanecula svecica"] <- "Luscinia svecica"
splist$sp1sci.combo[splist$sp1sci.combo == "Cyanecula svecica"] <- "Luscinia svecica"
int.raw$species1_scientific[int.raw$species1_scientific == "Cyanecula svecica"] <- "Luscinia svecica"
int.raw$species2_scientific[int.raw$species2_scientific == "Cyanecula svecica"] <- "Luscinia svecica"

namechg.unmatch[,1:2]
## Update name: BBS: Charadrius nivosus = BOW: Anarhynchus nivosus
dplyr::filter(splist, species1_scientific %in% c("Charadrius nivosus")) # in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Charadrius nivosus")) # in interactions 
dplyr::filter(int.raw, species2_scientific %in% c("Charadrius nivosus")) # in interactions 
dplyr::filter(splist, species1_scientific %in% c("Anarhynchus nivosus")) # not in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Anarhynchus nivosus")) # not in interactions 
dplyr::filter(int.raw, species2_scientific %in% c("Anarhynchus nivosus")) # in interactions 
# RENAME: BBS & BOW: change Charadrius nivosus to Anarhynchus nivosus
splist$species1_scientific[splist$species1_scientific == "Charadrius nivosus"] <- "Anarhynchus nivosus"
splist$sp1sci.combo[splist$sp1sci.combo == "Charadrius nivosus"] <- "Anarhynchus nivosus"
int.raw$species1_scientific[int.raw$species1_scientific == "Charadrius nivosus"] <- "Anarhynchus nivosus"
int.raw$species2_scientific[int.raw$species2_scientific == "Charadrius nivosus"] <- "Anarhynchus nivosus"

namechg.unmatch[,1:2]
## Update name: BBS: Charadrius wilsonia = BOW: Anarhynchus wilsonia
dplyr::filter(splist, species1_scientific %in% c("Charadrius wilsonia")) # in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Charadrius wilsonia")) # in interactions 
dplyr::filter(int.raw, species2_scientific %in% c("Charadrius wilsonia")) # in interactions 
dplyr::filter(splist, species1_scientific %in% c("Anarhynchus wilsonia")) # not in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Anarhynchus wilsonia")) # not in interactions 
dplyr::filter(int.raw, species2_scientific %in% c("Anarhynchus wilsonia")) # in interactions 
# RENAME: BBS & BOW: change Charadrius wilsonia to Anarhynchus wilsonia
splist$species1_scientific[splist$species1_scientific == "Charadrius wilsonia"] <- "Anarhynchus wilsonia"
splist$sp1sci.combo[splist$sp1sci.combo == "Charadrius wilsonia"] <- "Anarhynchus wilsonia"
int.raw$species1_scientific[int.raw$species1_scientific == "Charadrius wilsonia"] <- "Anarhynchus wilsonia"
int.raw$species2_scientific[int.raw$species2_scientific == "Charadrius wilsonia"] <- "Anarhynchus wilsonia"

namechg.unmatch[,1:2]
## Update name: BBS: Charadrius montanus = BOW: Anarhynchus montanus
dplyr::filter(splist, species1_scientific %in% c("Charadrius montanus")) # in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Charadrius montanus")) # in interactions 
dplyr::filter(int.raw, species2_scientific %in% c("Charadrius montanus")) # in interactions 
dplyr::filter(splist, species1_scientific %in% c("Anarhynchus montanus")) # not in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Anarhynchus montanus")) # not in interactions 
dplyr::filter(int.raw, species2_scientific %in% c("Anarhynchus montanus")) # not in interactions 
# RENAME: BBS & BOW: change Charadrius montanus to Anarhynchus montanus
splist$species1_scientific[splist$species1_scientific == "Charadrius montanus"] <- "Anarhynchus montanus"
splist$sp1sci.combo[splist$sp1sci.combo == "Charadrius montanus"] <- "Anarhynchus montanus"
int.raw$species1_scientific[int.raw$species1_scientific == "Charadrius montanus"] <- "Anarhynchus montanus"
int.raw$species2_scientific[int.raw$species2_scientific == "Charadrius montanus"] <- "Anarhynchus montanus"

namechg.unmatch[,1:2]
## Cordilleran & Pacific Flycatcher are now Western Flycatcher as of 2023
## Update name: BBS: Empidonax occidentalis = BOW: Empidonax difficilis
dplyr::filter(splist, species1_scientific %in% c("Empidonax occidentalis")) # in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Empidonax occidentalis")) # in interactions 
dplyr::filter(int.raw, species2_scientific %in% c("Empidonax occidentalis")) # in interactions 
dplyr::filter(splist, species1_scientific %in% c("Empidonax difficilis")) # in BBS list
dplyr::filter(int.raw, species1_scientific %in% c("Empidonax difficilis")) # in interactions 
dplyr::filter(int.raw, species2_scientific %in% c("Empidonax difficilis")) # in interactions 
# RENAME: BBS & BOW: change Empidonax occidentalis to Empidonax difficilis
# # ** DO NOT CHANGE THE sp1sci.combo bc this is an AOU combined species
splist$species1_scientific[splist$species1_scientific == "Empidonax occidentalis"] <- "Empidonax difficilis"
int.raw$species1_scientific[int.raw$species1_scientific == "Empidonax occidentalis"] <- "Empidonax difficilis"
int.raw$species2_scientific[int.raw$species2_scientific == "Empidonax occidentalis"] <- "Empidonax difficilis"

## Remove blank species scientific names
# If there is no entry for species1_scientific or species2_scientific, omit row
dim(int.raw)
# 24557
int.raw<-int.raw %>% drop_na(species1_scientific)
int.raw <- int.raw %>% filter(!(species1_scientific==""))
dim(int.raw)
# 24534: 23 removed: Aug 8, 2024
int.raw <- int.raw %>% filter(!(species2_scientific==""))
dim(int.raw)
# 24533: 1 more removed: Aug 8, 2024

## End of Species' Scientific name changes ##


#*******************************#
#*#### Checking numbers of species & BBS List ####
#*******************************#
# duplicate BBS species list with the changes to names for species2 assessment
sp2list<-splist
# rename
names(sp2list)[names(sp2list) == "sp1_AOU"] <-"sp2_AOU"
names(sp2list)[names(sp2list) == "species1_scientific"] <-"species2_scientific"
names(sp2list)[names(sp2list) == "sp1AOU.combo"] <-"sp2AOU.combo"
names(sp2list)[names(sp2list) == "sp1sci.combo"] <-"sp2sci.combo"
names(sp2list)[names(sp2list) == "bbs_sp1_common"] <-"bbs_sp2_common"
names(sp2list)[names(sp2list) == "Genus"] <-"Genus2"
names(sp2list)[names(sp2list) == "Species"] <-"Species2"

sp2list$French_Common_Name<-NULL
sp2list$Spanish_Common_Name<-NULL
sp2list$ORDER<-NULL
sp2list$Family<-NULL

# Merge into paired intxns by sp1 (numbers below as of Aug 8, 2024)
intxns1<-merge(int.raw,splist,by=c("species1_scientific"),all.x=T, all.y=T)
dim(int.raw)
# 24533 rows
dim(intxns1)
# 25060 rows
length(unique(int.raw$species1_scientific))
# 1130 species treated as species1 in original avian interaction data
length(unique(splist$species1_scientific))
# 759 species in entire BBS dataset (Grass and Sedge Wren are same spp?)
length(unique(intxns1$species1_scientific))
# 1204 species in the merged data
length(unique(intxns1$species2_scientific))
# 3389 species as species2 but these *may* include the scientific names without a match in sp1
sum(is.na(intxns1$species2_scientific)) 
# 79 - species that exist in the BBS Species List but are not entered yet in 
# original avian interaction data as species2 - these are subspecies and unidentified
length(unique(int.raw$species2_scientific))
# 3389 species as species2 

# Repeat above but now for sp2 
# Merge into paired intxns by sp1
intxns2<-merge(int.raw,sp2list,by=c("species2_scientific"),all.x=T, all.y=T)
dim(int.raw)
# 24533 rows
dim(intxns2)
# 25343 rows
length(unique(int.raw$species2_scientific))
# 3389 species treated as species2 in original avian interaction data
length(unique(splist$species1_scientific))
# 759 species in entire BBS dataset
length(unique(intxns2$species2_scientific))
# 3463 species in the merged data 
sum(is.na(intxns2$species1_scientific)) 
# 79 NAs - species that exist in the BBS Species List but are not entered yet in original avian interaction data as species1
length(unique(intxns2$species1_scientific))
# 1132 species as species1 but these *may* include the scientific names without a match in sp1
length(unique(int.raw$species1_scientific))
# 1131 species as species1 but these *may* include the scientific names without a match in sp1

# Export to check species names: if the row has an AOU associated with species1,
# it is in BBS; if those rows are without a complete entry, they are missing
# entries for those species There are 79 here as of Aug 8, 2024. All are
# either rare subspecies (without a BOW acct), or they are species which the
# observer could not distinguish, or they are just the Genus level.
# Subset out to just include the species1 in BBS without complete entries (i.e., missing species2)
intxns1a<-intxns1[!is.na(intxns1$sp1_AOU),] # only species with an AOU
intxns1a<-intxns1a[(is.na(intxns1a$species2_scientific) | intxns1a$species2_scientific==""),] 
sort(intxns1a$species1_scientific)
length(intxns1a$species1_scientific)
# 79

# Subset out to just include the species2 in BBS without complete entries (i.e., missing species1)
intxns2a<-intxns2[!is.na(intxns2$sp2_AOU),] # only species with an AOU
intxns2a<-intxns2a[(is.na(intxns2a$species1_scientific) | intxns2a$species1_scientific==""),] 
sort(intxns2a$species2_scientific)
length(intxns2a$species2_scientific)
# 79
# The species2 above just have occurrence as species1. That's ok.

#*******************************#
#### Fixing Species' Common Names ####
#*******************************#
# This is done for BBS look-up; need to replace with global names later, e.g., BirdNet 
# Create intxns12 which merges splist and species1_scientific, then sp2list and 
# species2_scientific, by only keeps interaction data.
intxns12<-merge(int.raw,splist,by=c("species1_scientific"),all.x=T)
dim(int.raw)
# 24533
dim(intxns12)
# 24981
#write.csv(intxns12, file.path(L1_dir, "intxns12.csv"), row.names=F) 
#write.csv(intxns12,file.path(L1_dir,"test_intxns12.csv"), row.names=F)

intxns12<-merge(intxns12,sp2list,by=c("species2_scientific"),all.x=T)
dim(intxns12)
# 25727

# Create an extra species1 column to test mutate & re-assignment below
intxns12$species1_common_orig<-intxns12$species1_common

# If a species has a AOU, assign the common name based on the BBS splist
intxns12 <- intxns12 %>% 
  mutate(species1_common = ifelse(!is.na(sp1_AOU), bbs_sp1_common, species1_common))
dim(intxns12)
# 25727
#write.csv(intxns12, file.path(L1_dir,"intxns12.csv"), row.names=F) 

intxns12$species2_common_orig<-intxns12$species2_common
# If a species has a AOU, assign the common name based on the BBS splist
intxns12 <- intxns12 %>% 
  mutate(species2_common = ifelse(!is.na(sp2_AOU), bbs_sp2_common, species2_common))
#write.csv(intxns12, file.path(L1_dir,"intxns12.csv"), row.names=F) 
dim(intxns12)
# 25727

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
# 610 entries with some kind of note

## EXPORT the cleaned interaction pairs data:
# Order the data by species1_scientific
intxns12 <- intxns12 %>% relocate(species2_scientific, .after = species1_common)
## Ran for Aug 8, 2024 - but, need to update common names based on BirdNet in future
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
int.bbs$sp1sci.combo[int.bbs$sp1sci.combo == "Cistothorus platensis"] <- "Cistothorus stellaris"
int.bbs$sp2sci.combo[int.bbs$sp2sci.combo == "Cistothorus platensis"] <- "Cistothorus stellaris"
# Update the AOU for this species (assign 7240 which is the Sedge Wren)
# Make a selection for the species
cisste1<-int.bbs$species1_scientific == "Cistothorus stellaris"
cisste2<-int.bbs$species2_scientific == "Cistothorus stellaris"

# Assign AOU to species level:
int.bbs$sp1_AOU[cisste1] <- 7240
int.bbs$sp2_AOU[cisste2] <- 7240
int.bbs$sp1AOU.combo[cisste1] <- 7240
int.bbs$sp2AOU.combo[cisste2] <- 7240

## Remove non-breeding season interactions from the BBS subset of data because
# BBS observations are only during breeding season.
table(int.bbs$nonbreedingseason)

# Remove the "yes" for nonbreedingseason 
int.bbs <- int.bbs %>% filter(nonbreedingseason!="yes")
dim(intxns12)-dim(int.bbs)
# 2612 cases where interaction is "yes" for nonbreeding season

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
                                       "sp1AOU.combo",
                                       "sp1sci.combo",
                                       "sp2_AOU",
                                       "sp2AOU.combo",
                                       "sp2sci.combo"))

#*******************************#
# Omit the non-BBS rows (rows that do not contain one of the BBS species)
#*******************************#

# Remove rows that do not have an AOU associated with them 
# # make a temporary version int.bbs1 to check
int.bbs1<-int.bbs %>% 
  filter(!is.na(sp1_AOU) | !is.na(sp2_AOU))
dim(int.bbs)-dim(int.bbs1)
# 3477 rows that do not contain a BBS species
int.bbs.noAOU<-int.bbs %>% 
  filter(is.na(sp1_AOU) & is.na(sp2_AOU))

sort(unique(int.bbs.noAOU$species1_scientific))
# 349 species without an AOU (not in BBS) as of Aug 8, 2024

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
# -15; We are going to keep some subspecies and edit interactions accordingly.

# Consider making the code below more elegant but for now we are just manually
# re-assigning scientific, common, AOU, based on what we find. 
# We are not re-assigning the AOUs in the observations.
#is.subspecies.intsp1 = which(int.bbs$species1_scientific %in% subspecies)
#is.subspecies.intsp2 = which(int.bbs$species2_scientific %in% subspecies)

# Print the 15 subspecies
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
subset(bbs.splist, species1_scientific=="Anser caerulescens")
subset(bbs.splist, species1_scientific=="Anser caerulescens (blue form)")
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(1690)) # Anser caerulescens 35 times 
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(1691)) # Anser caerulescens (blue form) 0 times
dplyr::filter(int.bbs, species1_scientific %in% c("Anser caerulescens")) # Anser caerulescens 16 times 
dplyr::filter(int.bbs, species2_scientific %in% c("Anser caerulescens")) # Anser caerulescens 19 times 
dplyr::filter(int.bbs, species1_scientific %in% c("Anser caerulescens (blue form)")) # Anser caerulescens (blue form) 0 times
dplyr::filter(int.bbs, species2_scientific %in% c("Anser caerulescens (blue form)")) # Anser caerulescens (blue form) 0 times 

# No change needed bc the Snow Goose subspecies doesn't exist in BBS obs or in the breeding season interactions.

#### BRANT GOOSE Branta bernicla and its subspecies in interactions & BBS obs... ####
# View the AOU for the genus species
subset(bbs.splist, species1_scientific=="Branta bernicla")
subset(bbs.splist, species1_scientific=="Branta bernicla nigricans")
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(1730)) # Branta bernicla 0 times 
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(1740)) # Branta bernicla nigricans 23 times
dplyr::filter(int.bbs, species1_scientific %in% c("Branta bernicla")) # species 32 times 
dplyr::filter(int.bbs, species2_scientific %in% c("Branta bernicla")) # species 16 times 
dplyr::filter(int.bbs, species1_scientific %in% c("Branta bernicla nigricans")) # subspecies 0 times
dplyr::filter(int.bbs, species2_scientific %in% c("Branta bernicla nigricans")) # subspecies 7 times 

# There are no BBS obs of the species (1730), only subspecies; assign all the
# breeding season interactions to the subspecies (1740). Most interactions are at
# the species level. Make a selection for the species
braber1<-int.bbs$species1_scientific == "Branta bernicla"
braber2<-int.bbs$species2_scientific == "Branta bernicla"

# Assign AOU to subspecies level:
int.bbs$sp1_AOU[braber1] <- 1740
int.bbs$sp1AOU.combo[braber1] <- 1740
int.bbs$sp1_subspecies_status[braber1]<-"sp1 and AOU originally species Branta bernicla; changed to subspecies bc only BBS obs of Branta bernicla nigricans; see sp1_orig"
int.bbs$sp2_AOU[braber2] <- 1740
int.bbs$sp2AOU.combo[braber2] <- 1740
int.bbs$sp2_subspecies_status[braber2]<-"sp2 and AOU originally species Branta bernicla; changed to subspecies bc only BBS obs of Branta bernicla nigricans; see sp2_orig"
# Assign species to subspecies
int.bbs$species1_scientific[braber1] <- "Branta bernicla nigricans"
int.bbs$sp1sci.combo[braber1] <- "Branta bernicla nigricans"
int.bbs$species2_scientific[braber2] <- "Branta bernicla nigricans"
int.bbs$sp2sci.combo[braber2] <- "Branta bernicla nigricans"
int.bbs$species1_common[braber1] <- "(Black Brant) Brant"
int.bbs$species2_common[braber2] <- "(Black Brant) Brant"

dplyr::filter(int.bbs, species1_scientific %in% c("Branta bernicla")) # species 0 times 
dplyr::filter(int.bbs, species2_scientific %in% c("Branta bernicla")) # species 0 times 
dplyr::filter(int.bbs, sp1sci.combo %in% c("Branta bernicla")) # species 0 times 
dplyr::filter(int.bbs, sp2sci.combo %in% c("Branta bernicla")) # species 0 times 
dplyr::filter(int.bbs, species1_scientific %in% c("Branta bernicla nigricans")) # subspecies 32 times
dplyr::filter(int.bbs, species2_scientific %in% c("Branta bernicla nigricans")) # subspecies 23 times 

#### GREAT BLUE HERON Ardea herodias and its subspecies in interactions & BBS obs... ####
# *** This one has a AOUcombo, so only make changes to the non-combo columns.
# View the AOU for the genus species
subset(bbs.splist, species1_scientific=="Ardea herodias")
subset(bbs.splist, species1_scientific=="Ardea herodias occidentalis")
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(1940)) # Ardea herodias 45376 times 
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(1920)) # Ardea herodias occidentalis 61 times
dplyr::filter(int.bbs, species1_scientific %in% c("Ardea herodias")) # species 162 times 
dplyr::filter(int.bbs, species2_scientific %in% c("Ardea herodias")) # species 218 times 
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
#23495

#### RED TAILED HAWK Buteo jamaicensis and its subspecies in interactions & BBS obs... ####
# *** This one has a AOUcombo, so only make changes to the non-combo columns.
# View the AOU for the genus species
subset(bbs.splist, species1_scientific=="Buteo jamaicensis")
subset(bbs.splist, species1_scientific=="Buteo jamaicensis harlani")
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(3370)) # Buteo jamaicensis 60,777 times 
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(3380)) # Buteo jamaicensis harlani 78 times
dim(dplyr::filter(int.bbs, species1_scientific %in% c("Buteo jamaicensis"))) # species 164 times 
dim(dplyr::filter(int.bbs, species2_scientific %in% c("Buteo jamaicensis"))) # species 430 times 
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
#24089

#### NORTHERN FLICKER Colaptes auratus and its subspecies in interactions & BBS obs... ####
# *** This one has a AOUcombo, so only make changes to the non-combo columns.
# View the AOU for the genus species
subset(bbs.splist, species1_scientific=="Colaptes auratus")
subset(bbs.splist, species1_scientific=="Colaptes auratus auratus")
subset(bbs.splist, species1_scientific=="Colaptes auratus cafer")
subset(bbs.splist, species1_scientific=="Colaptes auratus auratus x auratus cafer")
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(4123)) # Colaptes auratus 1299 times 
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(4120)) # Colaptes auratus auratus 68195 times
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(4130)) # Colaptes auratus cafer 21912 times
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(4125)) # Colaptes auratus auratus x auratus cafer 34 times
dim(dplyr::filter(int.bbs, species1_scientific %in% c("Colaptes auratus"))) # species 52 times 
dim(dplyr::filter(int.bbs, species2_scientific %in% c("Colaptes auratus"))) # species 144 times 
dplyr::filter(int.bbs, species1_scientific %in% c("Colaptes auratus auratus")) # subspecies 2 times as hybridization
dplyr::filter(int.bbs, species2_scientific %in% c("Colaptes auratus auratus")) # subspecies 0 times 
dplyr::filter(int.bbs, species1_scientific %in% c("Colaptes auratus cafer")) # subspecies 2 times as hybridization
dplyr::filter(int.bbs, species2_scientific %in% c("Colaptes auratus cafer")) # subspecies 4 times; hybrid, nest takeover, competition 
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
#24677

#### JUNCOS Junco hyemalis and its subspecies in interactions & BBS obs... ####
# *** This one has a AOUcombo, so only make changes to the non-combo columns.
# View the AOU for the genus species
subset(bbs.splist, species1_scientific=="Junco hyemalis")
subset(bbs.splist, species1_scientific=="Junco hyemalis hyemalis")
subset(bbs.splist, species1_scientific=="Junco hyemalis oreganus")
subset(bbs.splist, species1_scientific=="Junco hyemalis mearnsi")
subset(bbs.splist, species1_scientific=="Junco hyemalis aikeni")
subset(bbs.splist, species1_scientific=="Junco hyemalis caniceps")
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(5677)) # Junco hyemalis 755 times 
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(5670)) # Junco hyemalis hyemalis 15846 times
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(5671)) # Junco hyemalis oreganus 12297 times
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(5680)) # Junco hyemalis mearnsi 92 times
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(5660)) # Junco hyemalis aikeni 407 times
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(5690)) # Junco hyemalis caniceps 2729 times
dplyr::filter(int.bbs, species1_scientific %in% c("Junco hyemalis")) # species 33+ times 
dplyr::filter(int.bbs, species2_scientific %in% c("Junco hyemalis")) # species 102 times 
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
#25553

#### YELLOW-RUMPED WARBLER Setophaga coronata and its subspecies in interactions & BBS obs... ####
# *** This one has a AOUcombo, so only make changes to the non-combo columns.
# View the AOU for the genus species
subset(bbs.splist, species1_scientific=="Setophaga coronata")
subset(bbs.splist, species1_scientific=="Setophaga coronata coronata")
subset(bbs.splist, species1_scientific=="Setophaga coronata audoboni")
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(6556)) # Setophaga coronata 634 times 
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(6550)) # Setophaga coronata coronata 18871 times
dplyr::filter(bbs_allobs_rpid101, AOU %in% c(6560)) # Setophaga coronata audoboni 12809 times
dim(dplyr::filter(int.bbs, species1_scientific %in% c("Setophaga coronata"))) # species 53 times 
dim(dplyr::filter(int.bbs, species2_scientific %in% c("Setophaga coronata"))) # species 48 times 
dplyr::filter(int.bbs, species1_scientific %in% c("Setophaga coronata coronata")) # subspecies 2 time as facilitation
dplyr::filter(int.bbs, species2_scientific %in% c("Setophaga coronata coronata")) # subspecies 8 times (2x as hybridization)
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
# 25755

#### End of subspecies edits. Export L1 interaction data for BBS analysis# 
#### Check how above changes affected AOUs:
int.bbs$diff1<-int.bbs$sp1AOU.combo-int.bbs$sp1_AOU
int.bbs$diff2<-int.bbs$sp2AOU.combo-int.bbs$sp2_AOU
unique(int.bbs$diff1)
unique(int.bbs$diff2)
# Looks ok.
int.bbs$diff1<-NULL
int.bbs$diff2<-NULL

## Make sure the Genus species for to combined species via Jeff Hostetler @ BBS:
# Probably a more beautiful way to code this but this works:
int.bbs$sp1sci.combo[int.bbs$sp1AOU.combo == 31320] <- "Anas platyrhynchos"
int.bbs$sp2sci.combo[int.bbs$sp2AOU.combo == 31320] <- "Anas platyrhynchos"
int.bbs$sp1sci.combo[int.bbs$sp1AOU.combo == 31940] <- "Ardea herodias"
int.bbs$sp2sci.combo[int.bbs$sp2AOU.combo == 31940] <- "Ardea herodias"
int.bbs$sp1sci.combo[int.bbs$sp1AOU.combo == 33370] <- "Buteo jamaicensis"
int.bbs$sp2sci.combo[int.bbs$sp2AOU.combo == 33370] <- "Buteo jamaicensis"
int.bbs$sp1sci.combo[int.bbs$sp1AOU.combo == 34120] <- "Colaptes auratus auratus"
int.bbs$sp2sci.combo[int.bbs$sp2AOU.combo == 34120] <- "Colaptes auratus auratus"
int.bbs$sp1sci.combo[int.bbs$sp1AOU.combo == 35670] <- "Junco hyemalis"
int.bbs$sp2sci.combo[int.bbs$sp2AOU.combo == 35670] <- "Junco hyemalis"
int.bbs$sp1sci.combo[int.bbs$sp1AOU.combo == 36550] <- "Setophaga coronata coronata"
int.bbs$sp2sci.combo[int.bbs$sp2AOU.combo == 36550] <- "Setophaga coronata coronata"
int.bbs$sp1sci.combo[int.bbs$sp1AOU.combo == 30010] <- "Aechmophorus occidentalis / clarkii"
int.bbs$sp2sci.combo[int.bbs$sp2AOU.combo == 30010] <- "Aechmophorus occidentalis / clarkii"
int.bbs$sp1sci.combo[int.bbs$sp1AOU.combo == 34641] <- "Empidonax difficilis / occidentalis"
int.bbs$sp2sci.combo[int.bbs$sp2AOU.combo == 34641] <- "Empidonax difficilis / occidentalis"
int.bbs$sp1sci.combo[int.bbs$sp1AOU.combo == 34660] <- "Empidonax alnorum / traillii"
int.bbs$sp2sci.combo[int.bbs$sp2AOU.combo == 34660] <- "Empidonax alnorum / traillii"
int.bbs$sp1sci.combo[int.bbs$sp1AOU.combo == 34810] <- "Aphelocoma californica / woodhouseii"
int.bbs$sp2sci.combo[int.bbs$sp2AOU.combo == 34810] <- "Aphelocoma californica / woodhouseii"
int.bbs$sp1sci.combo[int.bbs$sp1AOU.combo == 35740] <- "Artemisiospiza nevadensis / belli"
int.bbs$sp2sci.combo[int.bbs$sp2AOU.combo == 35740] <- "Artemisiospiza nevadensis / belli"
int.bbs$sp1sci.combo[int.bbs$sp1AOU.combo == 34880] <- "Corvus brachyrhynchos"
int.bbs$sp2sci.combo[int.bbs$sp2AOU.combo == 34880] <- "Corvus brachyrhynchos"

# Move the useful columns to the left
int.bbs <- int.bbs %>% relocate(sp1_AOU, .after = interaction)
int.bbs <- int.bbs %>% relocate(sp2_AOU, .after = sp1_AOU)
int.bbs <- int.bbs %>% relocate(sp1sci.combo, .after = sp2_AOU)
int.bbs <- int.bbs %>% relocate(sp2sci.combo, .after = sp1sci.combo)
int.bbs <- int.bbs %>% relocate(sp1AOU.combo, .after = sp2sci.combo)
int.bbs <- int.bbs %>% relocate(sp2AOU.combo, .after = sp1AOU.combo)

## Note to check this before using new version:
write.csv(int.bbs,file.path(L1_dir,"AvianInteractionData_BBS_L1.csv"), row.names=F)

# Export updated bbs species names list (the only changes are the column headers 
# and a few species name updates)
names(bbs.splist)[names(bbs.splist) == "bbs_sp1_common"] <-"English_Common_Name"
names(bbs.splist)[names(bbs.splist) == "sp1_AOU"] <-"AOU"
names(bbs.splist)[names(bbs.splist) == "sp1AOU.combo"] <-"AOU.combo"
names(bbs.splist)[names(bbs.splist) == "sp1sci.combo"] <-"sp_scientific.combo"
names(bbs.splist)[names(bbs.splist) == "species1_scientific"] <-"species_scientific"

write.csv(bbs.splist,file.path(L1_dir,"bbs_splist_2022_L1.csv"), row.names=F)


