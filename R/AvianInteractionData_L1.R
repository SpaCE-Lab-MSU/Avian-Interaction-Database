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
# DATE:           27 Oct 2022; updated through 27 Nov 2024  
# NOTES:          Next script to run: 
#                 This script is used to refine species name changes to align 
#                 with BOW (Clements & eBird checklist), 
#                 and to create AvianInteractionData_L1.csv 
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

# Resolve scientific names using GBIF (last run Nov. 27, 2024)
int.gbif.names <- int.raw.names %>%
  mutate(
    scientific_id = get_ids(genus_species, "gbif"),
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

# Separate resolved and unresolved names based on specified criteria
resolved.gbif <- int.gbif.names %>%
  filter(!is.na(scientific_id) | grepl(" sp\\.$", genus_species))

unresolved.gbif <- int.gbif.names %>%
  filter(is.na(scientific_id) & !grepl(" sp\\.$", genus_species))

# Display both results
head(resolved.gbif)
head(unresolved.gbif)
# Omit scientific_id and accepted_scientific_name since they are blank
unresolved.gbif$scientific_id<-NULL
unresolved.gbif$accepted_scientific_name<-NULL

# Seem to be a lot of duplicates; remove any duplicates
dim(resolved.gbif)
# 5710
resolved.gbif <- resolved.gbif %>% 
  distinct()
dim(resolved.gbif)
# 4231 unique resolved as of Nov. 27, 2024
dim(unresolved.gbif)
# 353
unresolved.gbif <- unresolved.gbif %>% 
  distinct()
dim(unresolved.gbif)
# 338 unique unresolved as of Nov. 27, 2024

# Work with unresolved.gbif to try and determine what misspellings exist, and
# what they should be. Check spellings for both genus_species and
# common_name rows based on reference list from eBird & Clements checklist 2024.

# Reference list of scientific names eBird Clements checklist 2024
reference_names <- tibble(
  genus_species = checklist$genus_species,
  common_name = checklist$common_name
)

# Function to clean common names for comparison (ignore sp., Unid.)
clean_common_name <- function(name) {
  name %>%
    gsub("\\b(unid\\.|sp\\.)\\b", "", .) %>%   # Remove "Unid." and "sp."
    trimws()                                   # Trim extra spaces
}

# Use fuzzy logic function to find closest match from the reference list
# Function to find the closest match with a similarity score, and ignoring the
# common_name aspects above.
find_closest_match_with_cleaning <- function(name, reference_list) {
  if (is.na(name) || name == "") {
    return(list(match = NA_character_, score = NA_real_))
  }
  name_cleaned <- clean_common_name(name)
  reference_cleaned <- clean_common_name(reference_list) # Cleaned for comparison only
  distances <- stringdist::stringdist(name_cleaned, reference_cleaned, method = "jw")
  if (length(distances) == 0 || all(is.na(distances)) || min(distances, na.rm = TRUE) > 0.5) {
    return(list(match = NA_character_, score = NA_real_))
  }
  closest_match_index <- which.min(distances)
  closest_match <- reference_list[closest_match_index]
  similarity_score <- 1 - distances[closest_match_index]
  return(list(match = closest_match, score = similarity_score))
}

# Resolve genus_species matches
genus_species_matches <- unresolved.gbif %>%
  rowwise() %>%
  mutate(
    closest_genus_species_match = find_closest_match_with_cleaning(genus_species, checklist$genus_species)$match,
    genus_species_match_score = find_closest_match_with_cleaning(genus_species, checklist$genus_species)$score
  ) %>%
  ungroup()

# Resolve common_name matches 
common_name_matches <- unresolved.gbif %>%
  rowwise() %>%
  mutate(
    closest_common_name_match = find_closest_match_with_cleaning(common_name, checklist$common_name)$match,
    common_name_match_score = find_closest_match_with_cleaning(common_name, checklist$common_name)$score
  ) %>%
  ungroup()

# Combine results
final_matches <- genus_species_matches %>%
  left_join(
    common_name_matches %>%
      select(genus_species, common_name, closest_common_name_match, common_name_match_score),
    by = c("genus_species", "common_name")
  )

# Adjust so that columns are grouped close by for easier reference
final_matches <- final_matches %>%
  select(
    genus_species, closest_genus_species_match,
    common_name, closest_common_name_match,
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
names(fixed_names1)[names(fixed_names1) == "genus_species"] <-"genus_species.orig"
names(fixed_names1)[names(fixed_names1) == "common_name"] <-"common_name.orig"
names(fixed_names1)[names(fixed_names1) == "closest_genus_species_match"] <-"genus_species"
names(fixed_names1)[names(fixed_names1) == "closest_common_name_match"] <-"common_name"

save.image(file.path(L1_dir,"AvianInteractionData_L1.RData"))

dim(fixed_names1)

# Scroll through these 269 High Confidence Genus Species Matches. All look okay except:
# genus_species                    closest_match                 match_score

# KEEP ORIGINAL and change to species later when lumping subspecies
# checklist: Parkesia noveboracensis; 
# int.raw and unresolved.gbif: common_name = hybrid Barnacle x Bar-headed Goose
# BOW says the hybrid exists but is rare
# 1 Branta leucopsis x anser indicus Branta leucopsis x canadensis       0.905
int.raw[which(int.raw$species1_scientific == "Branta leucopsis x anser indicus"), ]
int.raw[which(int.raw$species2_scientific == "Branta leucopsis x anser indicus"), ]
unresolved.gbif[which(unresolved.gbif$genus_species == "Branta leucopsis x anser indicus"), ]
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

# Extract final_matches with lower confidence (match_score <= 0.90) for further checking
low_confidence_matches <- final_matches %>%
  filter(
    (genus_species_match_score < 0.9 & !is.na(genus_species_match_score)) 
  )

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
# 69

# For most of the names, assign them the closest match, then edit the few above noted "KEEP ORIGINAL".
fixed_names2<-merge(unresolved.gbif,low_confidence_matches, by=c("genus_species", "common_name"))

# Rename the current genus_species and common_name to ".orig", and assign
# genus_species and common_name to the CHECKLIST's closest_matches
names(fixed_names2)[names(fixed_names2) == "genus_species"] <-"genus_species.orig"
names(fixed_names2)[names(fixed_names2) == "common_name"] <-"common_name.orig"
names(fixed_names2)[names(fixed_names2) == "closest_genus_species_match"] <-"genus_species"
names(fixed_names2)[names(fixed_names2) == "closest_common_name_match"] <-"common_name"

# Scroll through these 69 Low Confidence Matches in order from highest to lowest
# match score. Many look okay except check these:

# genus_species                     closest_match           match_score

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
fixed_names2$genus_species[fixed_names2$genus_species.orig == "Columbina livia"] <- "Columba livia"

# ACCEPT CHANGE TO CLOSEST MATCH: BOW states no subspecies: "No subspecies, following
# Parkes (1954), who could not diagnose a difference between northeastern
# breeders and those farther west, which were named S. s. lurida (Burleigh and
# Peters, 1948)."
# 1 Setophaga striata / tigrina Setophaga striata       0.877
int.raw[which(int.raw$species1_scientific == "Setophaga striata / tigrina"), ]
int.raw[which(int.raw$species2_scientific == "Setophaga striata / tigrina"), ]

# KEEP ORIGINAL but edit it. BOW and checklist: Polytypic American Pipit Anthus rubescens is split into monotypic Siberian Pipit Anthus japonicus and polytypic American Pipit Anthus rubescens (with subspecies pacificus, rubescens, and alticola). int.raw interaction appears to by for North american species and observation (Parasitic Jaeger). Interaction is with "American Pipit"; checklist states "Anthus rubescens".
# Anthus americanus     Anthus cervinus  0.871
int.raw[which(int.raw$species1_scientific == "Anthus americanus"), ]
int.raw[which(int.raw$species2_scientific == "Anthus americanus"), ]
checklist[which(checklist$common_name == "American Pipit"), ]
fixed_names2$genus_species[fixed_names2$genus_species.orig == "Anthus americanus"] <- "Anthus rubescens"

# KEEP ORIGINAL and edit: checklist: Lesser Scaup =	Aythya affinis
# 5 Anas affinis   Argya affinis       Lesser Scaup  Lesser Scaup   0.868
int.raw[which(int.raw$species1_scientific == "Anas affinis"), ]
int.raw[which(int.raw$species2_scientific == "Anas affinis"), ]
checklist[which(checklist$common_name == "Lesser Scaup"), ]
fixed_names2$genus_species[fixed_names2$genus_species.orig == "Anas affinis"] <- "Aythya affinis"

# KEEP ORIGINAL and edit: Common name in int.raw is Hairy Woodpecker, so
# genus is off. Dryobates villosus
# 3 Leucophaeus villosus                   Leucophaeus modestus          0.867
int.raw[which(int.raw$species1_scientific == "Leucophaeus villosus"), ]
int.raw[which(int.raw$species2_scientific == "Leucophaeus villosus"), ]
fixed_names2$genus_species[fixed_names2$genus_species.orig == "Leucophaeus villosus"] <- "Dryobates villosus"

# KEEP ORIGINAL and edit: checklist: White-Eared Bronze-Cuckoo = Chalcites meyerii
# 1 Chrysococcyx meyerii   Chrysococcyx sp.  White-Eared Bronze-Cuckoo  White-eared Bronze-Cuckoo     0.867
int.raw[which(int.raw$species1_scientific == "Chrysococcyx meyerii"), ]
int.raw[which(int.raw$species2_scientific == "Chrysococcyx meyerii"), ]
checklist[which(checklist$common_name == "White-Eared Bronze-Cuckoo"), ]
fixed_names2$genus_species[fixed_names2$genus_species.orig == "Chrysococcyx meyerii"] <- "Chalcites meyerii"

# KEEP ORIGINAL and edit: checklist: Double-Crested Cormorant = Nannopterum auritum 
# 4 Phalacrocorax saltatrix Phalacrocorax capillatus  Double-Crested Cormorant Double-crested Cormorant  0.859
int.raw[which(int.raw$species1_scientific == "Phalacrocorax saltatrix"), ]
int.raw[which(int.raw$species2_scientific == "Phalacrocorax saltatrix"), ]
checklist[which(checklist$common_name == "Double-crested Cormorant"), ]
fixed_names2$genus_species[fixed_names2$genus_species.orig == "Phalacrocorax saltatrix"] <- "Nannopterum auritum"

# KEEP ORIGINAL and edit: Phalacrocorax sp. instead of spp. 
#2 Phalacrocorax spp.  Phalacrocorax varius    0.861
int.raw[which(int.raw$species1_scientific == "Phalacrocorax spp."), ]
int.raw[which(int.raw$species2_scientific == "Phalacrocorax spp."), ]
fixed_names2$genus_species[fixed_names2$genus_species.orig == "Phalacrocorax spp."] <- "Phalacrocorax sp."

# ACCEPT CHANGE TO CLOSEST MATCH: checklist: 	Black-Necked Stilt = Himantopus mexicanus 
# 6 Himantopus alexandrus   Himantopus mexicanus   Black-Necked Stilt  Black-necked Stilt 0.857
int.raw[which(int.raw$species1_scientific == "Himantopus alexandrus"), ]
int.raw[which(int.raw$species2_scientific == "Himantopus alexandrus"), ]
checklist[which(checklist$common_name == "Black-necked Stilt"), ]

# KEEP ORIGINAL but edit: checklist: Pomarine Jaeger = Stercorarius pomarinus
# 5 Stercorarius stercorarius Stercorarius parasiticus    Pomarine Jaeger     Pomarine Jaeger  0.857
int.raw[which(int.raw$species1_scientific == "Stercorarius stercorarius"), ]
int.raw[which(int.raw$species2_scientific == "Stercorarius stercorarius"), ]
checklist[which(checklist$common_name == "Pomarine Jaeger"), ]
fixed_names2$genus_species[fixed_names2$genus_species.orig == "Stercorarius stercorarius"] <- "Stercorarius pomarinus"

# KEEP ORIGINAL, Common name in int.raw is the White Wagtail. 
# 1 Moticilla alba            Motacilla citreola             0.858
int.raw[which(int.raw$species1_scientific == "Moticilla alba"), ]
int.raw[which(int.raw$species2_scientific == "Moticilla alba"), ]
fixed_names2$genus_species[fixed_names2$genus_species.orig == "Moticilla alba"] <- "Moticilla alba"

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

# KEEP ORIGINAL and edit. This is the Eurasian Goshawk based on the location of
# the interaction (Svalbard): was Accipter atricapillus and is now Astur gentilis
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

# KEEP ORIGINAL and edit: remove p from spp.
#2 Aechmophorus spp.  Aechmophorus clarkii        Unid. Grebe          Junin Grebe              
fixed_names2$genus_species[fixed_names2$genus_species.orig == "Aechmophorus spp."] <- "Aechmophorus sp."

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

# KEEP ORIGINAL and edit; checklist: Leach's Storm-Petrel = Hydrobates leucorhous
#3 Oceanodroma leucorrhoa Paraclaravis mondetoura ochoterena Leach's Storm-Petrel Leach's Storm-Petrel 
int.raw[which(int.raw$species1_scientific == "Oceanodroma leucorrhoa"), ]
int.raw[which(int.raw$species2_scientific == "Oceanodroma leucorrhoa"), ]
checklist[which(checklist$common_name == "Leach's Storm-Petrel"), ]
fixed_names2$genus_species[fixed_names2$genus_species.orig == "Oceanodroma leucorrhoa"] <- "Hydrobates leucorhous"

# # KEEP ORIGINAL and edit; Duck sp. = Anatidae sp. 
# 2 Duck sp 0.737 Anatidae sp. 
fixed_names2$genus_species[fixed_names2$genus_species.orig == "Duck sp"] <- "Anatidae sp."

# KEEP ORIGINAL and edit int.raw; this entry has multiple columns off for
# scientific and common BOW and checklist: genus_species of Roseate Spoonbill = Platalea ajaja 1
# Roseate spoonbill Cormobates placens meridionalis       0.736
int.raw[which(int.raw$species1_scientific == "Roseate Spoonbill"), ]
int.raw[which(int.raw$species2_scientific == "Roseate Spoonbill"), ]
checklist[which(checklist$common_name == "Roseate Spoonbill"), ]
int.raw$species2_common[int.raw$species1_scientific == "Roseate Spoonbill"] <- "Roseate Spoonbill"
int.raw$species1_scientific[int.raw$species1_scientific == "Roseate Spoonbill"] <- "Ardea herodias"
fixed_names2$genus_species[fixed_names2$genus_species.orig == "Roseate spoonbill"] <- "Platalea ajaja"
# Common name will be changed later
#fixed_names2$common_name[fixed_names2$genus_species.orig == "Roseate spoonbill"] <- "Roseate Spoonbill"

# KEEP ORIGINAL and edit: Unid. Storm Petrel 
# BOW: now Genus is Hydrobates instead of Oceanodrama
#1 Oceanodroma spp.                    Oceanitidae sp.                          0.691
int.raw[which(int.raw$species1_scientific == "Oceanodroma spp."), ]
int.raw[which(int.raw$species2_scientific == "Oceanodroma spp."), ]
fixed_names2$genus_species[fixed_names2$genus_species.orig == "Oceanodroma spp."] <- "Hydrobates sp."

# Typos 
# 1 unid. 2 hawl  Turdus hauxwelli       0.674
# Also missing: unid. Accipiter hawl
int.raw[which(int.raw$species1_scientific == "unid. 2 hawl"), ]
int.raw[which(int.raw$species2_scientific == "unid. 2 hawl"), ]
int.raw[which(int.raw$species2_scientific == "unid. Accipiter hawl"), ]
fixed_names2$genus_species[fixed_names2$genus_species.orig == "unid. Accipiter hawl"] <- "Aerospiza/Tachyspiza/Accipiter/Astur sp."

save.image(file.path(L1_dir,"AvianInteractionData_L1.RData"))

# **** Do common name changes below for entire Astur group

# Remove duplicate rows
fixed_names1 <- fixed_names1 %>% 
                  distinct()
fixed_names2 <- fixed_names2 %>% 
                  distinct()

# Combine these together for the full set of fixed genus_species for the
# unresolved_gbif names which were fixed by referencing the CHECKLIST. Remove
# extra columns that are not needed for combining w genus_species.
fixed.unresolved.gs<-rbind(fixed_names1,fixed_names2)
fixed.unresolved.gs$genus_species_match_score<-NULL
fixed.unresolved.gs$common_name_match_score<-NULL
fixed.unresolved.gs$common_name<-NULL
dim(fixed.unresolved.gs)
# 338 species names

save.image(file.path(L1_dir,"AvianInteractionData_L1.RData"))

# Rename genus_species to genus_species.orig. Rename accepted_scientific_name to genus_species
names(resolved.gbif)[names(resolved.gbif) == "genus_species"] <-"genus_species.orig"
names(resolved.gbif)[names(resolved.gbif) == "accepted_scientific_name"] <-"genus_species"
names(resolved.gbif)[names(resolved.gbif) == "common_name"] <-"common_name.orig"
resolved.gbif$scientific_id<-NULL

# Take the genus_species in resolved.gbif and fixed.unresolved.gs and rbind them,
# remove duplicates, then merge with checklist to get common name assigned. Keep
# reference of the genus_species.orig bc need to merge back into the int.raw data.
int.final.names<-rbind(resolved.gbif,fixed.unresolved.gs)
dim(int.final.names)
# 4569 

# Remove duplicate rows, if any exist
int.final.names <- int.final.names %>% 
  distinct()

# FIX THESE LATER; decide whether to assign Family level or Genus for this
# instead of the genus_species.orig. Some have some typos (only a few). Some of
# the int.final.names genus_species are blank. Most are because they are Genus
# sp. For now, leave these as is since we cannot get to species level on these
# interactions.
int.final.names.Gsp<-int.final.names[which(is.na(int.final.names$genus_species)), ]
dim(int.final.names.Gsp)
# 620 Genus-level entries

# All the NA in genus_species are "Genus sp.". If the NA exists in
# genus_species, assign it the genus_species.orig which includes this
# designation.
int.final.names$genus_species1 <- ifelse(is.na(int.final.names$genus_species), 
                                        int.final.names$genus_species.orig, 
                                        int.final.names$genus_species)
names(int.final.names)[names(int.final.names) == "genus_species"] <-"genus_species.gbif.chklist"
names(int.final.names)[names(int.final.names) == "genus_species1"] <-"genus_species"

# Now merge the GBIF & checked CHECKLIST-derived int.final.names$genus_species
# with the CHECKLIST to get final checklist common_name.
checklist.narrow<-subset(checklist, select=c("genus_species",
                                             "common_name",
                                             "category",
                                             "order",
                                             "family"))
checklist.narrow<-data.frame(checklist.narrow)
int.final.names.checklist<-merge(int.final.names,checklist.narrow, by=c("genus_species"),all.x=T)

head(int.final.names.checklist)

# 937 missing common names - about 30% are "Genus sp." but others are just
# missing?? For now we are just going to focus on fixing the BBS species. Later
# this needs to be edited with a better workflow. Check above bc the checklist
# should include all these species w genus_species and common_name... something is off
int.final.names.checklist.NAcommon<-int.final.names.checklist[which(is.na(int.final.names.checklist$common_name)), ]
dim(int.final.names.checklist.NAcommon)
# 937
# 620 Genus-level entries from above but ~300 missing common name.

# First fix the Accipiters

# Fix these by assigning the appropriate genus_species based on CHECKLIST, then
# merge with CHECKLIST to get common names; add common name from
# common_name.orig when it doesn't exist in CHECKLIST
int.final.names.checklist
int.final.names.checklist$genus_species[int.final.names.checklist$genus_species.orig == "Acanthis flammea"] <- "Acanthis flammea hornemanni"
int.final.names.checklist$common_name[int.final.names.checklist$genus_species.orig == "Acanthiza apicalis albiventris"] <- "Red-Tailed Thornbill"
int.final.names.checklist$genus_species[int.final.names.checklist$genus_species.orig == "Accipiter bicolor"] <- "Astur bicolor"
int.final.names.checklist$genus_species[int.final.names.checklist$genus_species.orig == "Accipiter cooperii"] <- "Astur cooperii"
int.final.names.checklist$genus_species[int.final.names.checklist$genus_species.orig == "Accipiter gentilis"] <- "Astur atricapillus"
int.final.names.checklist$genus_species[int.final.names.checklist$genus_species.orig == "Accipiter gentilis laingi"] <- "Astur atricapillus laingi"
int.final.names.checklist$genus_species[int.final.names.checklist$genus_species.orig == "Accipiter melanoleucus"] <- "Astur melanoleucus"
int.final.names.checklist$genus_species[int.final.names.checklist$genus_species.orig == "Accipiter sp."] <- "Aerospiza/Tachyspiza/Accipiter/Astur sp."
int.final.names.checklist$common_name[int.final.names.checklist$genus_species.orig == "Accipiter striatus venator"] <- "Sharp-Shinned Hawk (Puerto Rican)"
#? Acrocephalus scirpaceus baeticatus and Acrocephalus gracilirostris leptorhynchus without CHECKLIST common_name
int.final.names.checklist$common_name[int.final.names.checklist$genus_species.orig == "Aechmophorus clarkii clarkii"] <- "Clark's Grebe (clarkii)"
int.final.names.checklist$common_name[int.final.names.checklist$genus_species.orig == "Aechmophorus clarkii transitionalis"] <- "Clark's Grebe (transitionalis)"
# no subspecies in CHECKLIST
int.final.names.checklist$genus_species[int.final.names.checklist$genus_species.orig == "Aechmophorus occidentalis ephemeralis"] <- "Aechmophorus occidentalis"
int.final.names.checklist$genus_species[int.final.names.checklist$genus_species.orig == "Aechmophorus occidentalis occidentalis"] <- "Aechmophorus occidentalis"
# ? Aegolius funereus funereus:  Tengmalm's Owl - not in CHECKLIST
# ? Agelaioides badius badius and Agelaioides badius bolivianus: Grayish Baywing subspecies but no common_name in CHECKLIST
# ? Agelaioides badius fringillarius: Pale Baywing subspecies? only species in CHECKLIST 
int.final.names.checklist$genus_species[int.final.names.checklist$genus_species.orig == "Agelaioides badius fringillarius"] <- "Agelaioides fringillarius"
# Aimophila ruficeps eremoeca: Rufus-crowned sparrow; no common_name in CHECKLIST
int.final.names.checklist$common_name[int.final.names.checklist$genus_species.orig == "Aimophila ruficeps eremoeca"] <- "Rufous-Crowned Sparrow (Eremoeca)"
int.final.names.checklist$common_name[int.final.names.checklist$genus_species.orig == "Aimophila ruficeps scottii"] <- "Rufous-Crowned Sparrow (Scottii)"
int.final.names.checklist$common_name[int.final.names.checklist$genus_species.orig == "Schoeniparus castaneceps"] <- "Rufous-winged Fulvetta"
int.final.names.checklist$genus_species[int.final.names.checklist$genus_species.orig == "Schoeniparus castaneceps"] <- "Schoeniparus castaneceps"
int.final.names.checklist$common_name[int.final.names.checklist$genus_species.orig == "Schoeniparus cinereus"] <- "Yellow-Throated Fulvetta"
int.final.names.checklist$genus_species[int.final.names.checklist$genus_species.orig == "Schoeniparus cinereus"] <- "Schoeniparus cinereus"
int.final.names.checklist$common_name[int.final.names.checklist$genus_species.orig == "Schoeniparus dubius"] <- "Rusty-Capped Fulvetta"
int.final.names.checklist$genus_species[int.final.names.checklist$genus_species.orig == "Schoeniparus dubius"] <- "Schoeniparus dubius"
# Alcippe poioicephala phayrei is a subspecies but no common_name in CHECKLIST
# GBIF WAS INCORRECT:
int.final.names.checklist$common_name[int.final.names.checklist$genus_species.orig == "Ammospiza caudacuta caudacuta"] <- "Saltmarsh Sparrow (Caudacuta)"
int.final.names.checklist$genus_species[int.final.names.checklist$genus_species.orig == "Ammospiza caudacuta caudacuta"] <- "Ammospiza caudacuta caudacuta"
int.final.names.checklist$common_name[int.final.names.checklist$genus_species.orig == "Ammospiza caudacuta diversa"] <- "Saltmarsh Sparrow (Diversa)"
int.final.names.checklist$genus_species[int.final.names.checklist$genus_species.orig == "Ammospiza caudacuta diversa"] <- "Ammospiza caudacuta diversa"














  








### *** BBS WORK *** ###
splist$genus_species<-splist$species1_scientific
splist$common_name<-splist$bbs_sp1_common
checklist.narrow$genus_species.checklist<-checklist.narrow$genus_species
checklist.narrow$common_name.checklist<-checklist.narrow$common_name

# Merge with Clements Checklist, and fixed names from int.raw
bbs.checklist<-merge(splist, checklist.narrow, by=c("genus_species","common_name"),all.x=T)
bbs.checklist<-merge(bbs.checklist,int.final.names, by=c("genus_species"),all.x=T)
                     
# Merge with int.raw to update int.raw with Clements & eBird checklist names
int.raw.bbs<-int.raw
names(int.final.names.checklist)[names(int.final.names.checklist) == "genus_species"] <-"genus_species.gbif"

int.raw.edit$species1_scientific <- 
  stringr::str_replace_all(int.raw.update$species1_scientific, 
  setNames(int.final.names$genus_species, 
  int.final.names$genus_species.orig))

int.raw.edit$species2_scientific <- 
  stringr::str_replace_all(int.raw.update$species2_scientific, 
  setNames(int.final.names$genus_species, 
  int.final.names$genus_species.orig))

save.image(file.path(L1_dir,"AvianInteractionData_L1.RData"))



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
names(fixed_names1)[names(fixed_names1) == "genus_species"] <-"genus_species.orig"
names(fixed_names1)[names(fixed_names1) == "common_name"] <-"common_name.orig"
names(fixed_names1)[names(fixed_names1) == "closest_genus_species_match"] <-"genus_species"
names(fixed_names1)[names(fixed_names1) == "closest_common_name_match"] <-"common_name"

