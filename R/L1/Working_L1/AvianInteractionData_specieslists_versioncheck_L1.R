# TITLE:          L1 Species Lists Version Check, with a focus on determining whether original
#                   species list for Canada & Conterminous US aligns with AviBase 
#                   / BBS / Clements
#                 ** This code will not be included in final repo **
#                 Species Checklist Data: 
#                 Reads in L0 lists and keeps track of which species are in which list.
#                 Also keeps track of name changes in diff columns.
#                 Current goal is to create a list that can be used in the North 
#                 American Avian Interaction data paper and avian-meta-network paper.
#                 Ultimate goal is a master lookup list to subset lists of species.
#                 
#                 Reads in: (1) avibase_ca.conus_splist_2024_L0.csv (created in
#                 R/L0/AvianInteractionData_specieslists_L0.R) = CA-CONUS List,
#                 which is the AviBase Canada list + AviBase Lower 48 US + 
#                 AviBase Alaska list (taxonomy from Clements 2024) 
#                 (2) Clements-eBird 2024 list
#                 (3) BBS 2024 release
# AUTHORS:        Phoebe Zarnetske
# COLLABORATORS:  Vincent Miele, Stephane Dray, Emily Parker, Pat Bills
# DATA INPUT:     L0 data: avibase_ca.conus_splist_2024_L0.csv, 
#                         eBird-Clements-v2024-integrated-checklist-October-2024-rev.csv,
#                         AvianInteractionData_SpeciesList_dataentry_11Aug2025.csv
#                     all from AvianInteractionData_specieslists_L0.R
# DATA OUTPUT:    L1 data: CSVs tracking the different data subsets for checking 
# PROJECT:        Avian Interaction Database & avian-meta-network
# DATE:           17 January 2022 - 1 Sep 2025
#                 
#                 Next script to run: AvianInteractionData_L1.R

# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)
library(readr)
library(stringr)
#library(purrr)

# Local directories
L0_dir <- "/Users/plz/Documents/GitHub/Avian-Interaction-Database-Working/L0/species_checklists"
L1_dir <- "/Users/plz/Documents/GitHub/Avian-Interaction-Database-Working/L1/species_checklists"

#### Read in AviBase species list: all species in Canada and CONUS (with Clements 2024 taxonomy)
avibase<-read.csv(file.path(L0_dir,"avibase_ca.conus_splist_2024_L0.csv"), fileEncoding="UTF-8")
#ca.conus <- read_csv("L0/species_checklists/avibase_ca.conus_splist_2024_L0.csv")

# Name columns for merging later
names(avibase)
avibase$list<-"AviBase Canada & CONUS"
#avibase$AviBase.Clements2024<-"yes"
# Sort by scientific name
avibase <- avibase %>% arrange(scientific_name)
head(avibase)

#### Official 2024 eBird & Clements checklist
#for some reason this isn't working:
clements2024<-read.csv(file.path(L0_dir,"eBird-Clements-v2024-integrated-checklist-October-2024-rev.csv"))

#clements2024<-read_csv("eBird-Clements-v2024-integrated-checklist-October-2024-rev.csv")
# Clements List: Name the columns for merging later
clements2024<-subset(clements2024, select=c("English.name","scientific.name",
                                            "range","family","order"))

names(clements2024)
clements2024$list<-"Clements-eBird2024"
#clements2024$Clements.eBird2024<-"yes"
clements2024 <- clements2024 %>%
  rename(common_name = English.name,
         scientific_name = scientific.name,
         region = range,
         order = order, 
         family = family)
names(clements2024)
# Sort by scientific name
clements2024 <- clements2024 %>% arrange(scientific_name)
head(clements2024)

#### BBS List 2024: all species in BBS (the 2024 release which includes all
# BBS observed species as of 2023)
bbs2024<-read.csv(file.path(L0_dir,"bbs_splist_2024_L0.csv"), fileEncoding="UTF-8")

bbs2024$list<-"BBS 2024"
bbs2024 <- bbs2024 %>%
  rename(scientific_name = genus_species,
         common_name = English_Common_Name,
         order = Order, 
         family = Family,
         AOU = AOU)
names(bbs2024)
bbs2024$Genus<-NULL
bbs2024$Species<-NULL
bbs2024$Seq<-NULL
bbs2024$French_Common_Name<-NULL

# Sort by scientific name
bbs2024 <- bbs2024 %>% arrange(scientific_name)
head(bbs2024)

#### Read in the Google Sheet exported CSV to check how this relates to our Original list 
## from the data entry lookup GSheet. 
origlist<-read.csv(file.path(L0_dir,"AvianInteractionData_SpeciesList_dataentry_11Aug2025.csv"))

# Name some columns for merging later
names(origlist)
origlist$list<-"Original List Aug 2025"
#origlist$Original.Aug2025<-"yes"
# combine genus and species into 1 column
origlist$scientific_name <- paste(origlist$Genus, origlist$Species, sep = " ")
origlist <- origlist %>%
  rename(common_name = English_Common_Name,
         region = Region,
         order = ORDER,
         family = Family,
         origlist_dataentry = InteractionPairs_entry)
names(origlist)

origlist<-subset(origlist,select=c("scientific_name","common_name","region",
                                   "order","family","list","origlist_dataentry"))
# Sort by scientific name
origlist <- origlist %>% arrange(scientific_name)
head(origlist)

# Merge the datasets so we create a master lookup table of different names.
# Function to normalize names for joining
normalize_name <- function(x) {
  x %>%
    #str_to_lower() %>%                 # lowercase
    str_replace_all("\\s+", " ") %>%    # collapse multiple spaces
    str_replace_all("\\s*/\\s*", "/") %>%  # remove spaces around slashes
    str_replace_all("\\(\\s*", "(") %>%    # remove space after (
    str_replace_all("\\s*\\)", ")") %>%    # remove space before )
    str_trim()                          # trim leading/trailing spaces
}

# --- Function to suffix metadata columns, leaving key name columns untouched ---
suffix_metadata <- function(df, suffix, keep = c(
  "common_name_avibase8.17","scientific_name_avibase8.17",
  "common_name_bbs2024","scientific_name_bbs2024",
  "common_name_clements2024","scientific_name_clements2024",
  "common_name_orig","scientific_name_orig",
  "common_name_clean","scientific_name_clean",
  "in_avibase8.17","in_bbs2024","in_clements2024","in_origlist"
)) {
  rename_with(df, ~ paste0(., "_", suffix), .cols = setdiff(names(df), keep))
}

# --- Standardize each source table ---
avibase_std <- avibase %>%
  select(-list) %>%
  rename(common_name_avibase8.17 = common_name,
         scientific_name_avibase8.17 = scientific_name) %>%
  mutate(common_name_clean = normalize_name(common_name_avibase8.17),
         scientific_name_clean = normalize_name(scientific_name_avibase8.17),
         in_avibase8.17 = "yes") %>%
  suffix_metadata("avibase8.17")

bbs2024_std <- bbs2024 %>%
  select(-list) %>%
  rename(common_name_bbs2024 = common_name,
         scientific_name_bbs2024 = scientific_name) %>%
  mutate(common_name_clean = normalize_name(common_name_bbs2024),
         scientific_name_clean = normalize_name(scientific_name_bbs2024),
         in_bbs2024 = "yes") %>%
  suffix_metadata("bbs2024")

clements2024_std <- clements2024 %>%
  select(-list) %>%
  rename(common_name_clements2024 = common_name,
         scientific_name_clements2024 = scientific_name) %>%
  mutate(common_name_clean = normalize_name(common_name_clements2024),
         scientific_name_clean = normalize_name(scientific_name_clements2024),
         in_clements2024 = "yes") %>%
  suffix_metadata("clements2024")

origlist_std <- origlist %>%
  select(-list) %>%
  rename(common_name_orig = common_name,
         scientific_name_orig = scientific_name) %>%
  mutate(common_name_clean = normalize_name(common_name_orig),
         scientific_name_clean = normalize_name(scientific_name_orig),
         in_origlist = "yes") %>%
  suffix_metadata("origlist")

# --- Merge all sources on normalized keys ---
master <- avibase_std %>%
  full_join(bbs2024_std, by = c("common_name_clean", "scientific_name_clean")) %>%
  full_join(clements2024_std, by = c("common_name_clean", "scientific_name_clean")) %>%
  full_join(origlist_std, by = c("common_name_clean", "scientific_name_clean"))

# --- Unified names (priority: Clements → Avibase → BBS → Origlist) ---
master1 <- master %>%
  mutate(
    common_name = coalesce(common_name_clements2024, common_name_avibase8.17, common_name_bbs2024, common_name_orig),
    scientific_name = coalesce(scientific_name_clements2024, scientific_name_avibase8.17, scientific_name_bbs2024, scientific_name_orig)
  )

# --- Pre-clean scientific_name and common_name ---
master1 <- master1 %>%
  mutate(
    scientific_name = scientific_name %>%
      str_replace_all("\\s+", " ") %>%
      str_replace_all("\\s*/\\s*", "/") %>%
      str_replace_all("\\(\\s*", "(") %>%
      str_replace_all("\\s*\\)", ")") %>%
      str_trim(),
    common_name = common_name %>%
      str_replace_all("\\s+", " ") %>%
      str_replace_all("\\s*/\\s*", "/") %>%
      str_replace_all("\\(\\s*", "(") %>%
      str_replace_all("\\s*\\)", ")") %>%
      str_trim()
  )
# --- Fill in_* flags ---
master2 <- master1 %>%
  mutate(across(starts_with("in_"), ~ ifelse(is.na(.), "no", .))) %>%
  select(common_name, scientific_name, everything(), -common_name_clean, -scientific_name_clean)

# --- Collapse duplicate rows if some old names now map to the same Clements name ---
cols_to_collapse <- setdiff(names(master2), c("common_name", "scientific_name"))
master3 <- master2 %>%
  group_by(common_name, scientific_name) %>%
  summarise(across(all_of(cols_to_collapse), ~ paste(unique(na.omit(.)), collapse = "; "), .names = "{.col}"),
            .groups = "drop")

# --- Check output ---
head(master3) 
dim(master2) #35781
dim(master3) #35769
dim(master3)-dim(clements2024) #173 that differ

# Likely the remaining mismatches are recent name changes or spelling errors
# Small sample to test code
YRWA <- master3 %>%
  filter(if_any(everything(), ~ grepl("Yellow-rumped Warbler", ., ignore.case = TRUE)))

YRWA_combined <- YRWA %>%
  group_by(scientific_name) %>%
  summarise(across(everything(), ~ paste(unique(na.omit(.)), collapse = "; "), .names = "{.col}"),
            .groups = "drop")
# Misspelling in Setophaga coronata audoboni; should be Setophaga coronata auduboni
# Using recode from dplyr
YRWA <- YRWA %>%
  mutate(scientific_name = recode(scientific_name, 
                           "Setophaga coronata audoboni" = "Setophaga coronata auduboni"))
YRWA_combined <- YRWA %>%
  group_by(scientific_name) %>%
  summarise(across(everything(), ~ paste(unique(na.omit(.)), collapse = "; "), .names = "{.col}"),
            .groups = "drop")

# Remove the ; if it doesn't have any info after it or before it.
# Helper: remove semicolons only at the edges of a cell
clean_edge_semicolons <- function(x) {
  if (!is.character(x)) return(x)
  x <- gsub("^\\s*(;\\s*)+", "", x, perl = TRUE)  # remove any leading ; with optional spaces
  x <- gsub("(\\s*;\\s*)+$", "", x, perl = TRUE)  # remove any trailing ; with optional spaces
  # Optional: normalize internal spacing around semicolons (keep this if you like tidy spacing)
  x <- gsub("\\s*;\\s*", "; ", x, perl = TRUE)
  x <- trimws(x)
  # If a cell becomes empty after cleaning, make it NA (optional)
  x[x == ""] <- NA_character_
  x
}

# Apply to all character columns
YRWA_clean <- YRWA_combined %>%
  mutate(across(where(is.character), clean_edge_semicolons))
YRWA_clean # This worked.

# Run this YRWA code above across the full dataset. 

# Misspellings are likely happening for the remaining issues. 
# Run this in Setophaga coronata audoboni; should be Setophaga coronata auduboni
# Using recode from dplyr
master4 <- master3 %>%
  mutate(scientific_name = recode(scientific_name, 
                                  "Setophaga coronata audoboni" = "Setophaga coronata auduboni"))
# Apply the correction script Now do it on the entire list.
master5 <- master4 %>%
  group_by(scientific_name) %>%
  summarise(across(everything(), ~ paste(unique(na.omit(.)), collapse = "; "), .names = "{.col}"),
            .groups = "drop")
dim(master5)-dim(clements2024) #61 that still differ
# View the values of a column that should have semicolons added
unique(master4$in_bbs2024)
unique(master5$in_bbs2024) # worked.

# Remove the ";" if it doesn't have any info after it or before it, using the code from above.
# Apply to all character columns
master6 <- master5 %>%
  mutate(across(where(is.character), clean_edge_semicolons))
master6 

# Next step in 2 Parts: (1) check the duplicate common names that do not have a 
# Clements match; likely misspellings to fix or species changes. (2) Code these 
# manually in a list.
df <- master6

# ------------------------------------------------------------------
# Safety: ensure Clements columns exist (avoid missing-col errors)
# ------------------------------------------------------------------
if (!"scientific_name_clements2024" %in% names(df)) df$scientific_name_clements2024 <- NA_character_
if (!"common_name_clements2024"     %in% names(df)) df$common_name_clements2024     <- NA_character_

# ------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------
clean_name <- function(x) {
  ifelse(is.na(x), NA_character_,
         x %>%
           str_replace_all("\\s+", " ") %>%      # collapse multiple spaces
           str_replace_all("\\s*/\\s*", "/") %>% # remove spaces around slashes
           str_replace_all("\\(\\s*", "(") %>%   # remove space after "("
           str_replace_all("\\s*\\)", ")") %>%   # remove space before ")"
           str_trim())
}

combine_vals <- function(v) {
  v <- as.character(v)
  v <- v[!is.na(v) & v != ""]
  if (!length(v)) return(NA_character_)
  paste(unique(v), collapse = "; ")
}

reorder_clements_first <- function(all_names, clements_name) {
  if (is.na(all_names) || all_names == "") return(all_names)
  parts <- str_split(all_names, ";\\s*")[[1]] %>% unique()
  if (!is.na(clements_name) && clements_name != "" && clements_name %in% parts) {
    parts <- c(clements_name, setdiff(parts, clements_name))
  }
  paste(parts, collapse = "; ")
}

append_if_new <- function(target, add) {
  if (is.na(add) || add == "") return(target)
  if (is.na(target) || target == "") return(add)
  parts <- str_split(target, ";\\s*")[[1]]
  if (any(parts == add)) return(target)
  paste0(target, "; ", add)
}

# ------------------------------------------------------------------
# Light cleaning on the key name columns (ONLY these)
# ------------------------------------------------------------------
df <- df %>%
  mutate(
    scientific_name             = clean_name(scientific_name),
    common_name                 = clean_name(common_name),
    scientific_name_clements2024= clean_name(scientific_name_clements2024),
    common_name_clements2024    = clean_name(common_name_clements2024)
  )

# =====================================================
# PART 1: Merge duplicates ONLY where scientific_name_clements2024 is NA
# =====================================================
na_df <- df %>% filter(is.na(scientific_name_clements2024))
ok_df <- df %>% filter(!is.na(scientific_name_clements2024))

# report duplicate (scientific_name, common_name) among NA-clements rows
dups_na <- na_df %>%
  count(scientific_name, common_name, name = "n") %>%
  filter(n > 1) %>%
  arrange(scientific_name)

if (nrow(dups_na) > 0) {
  message("\n📋 Duplicate (scientific_name, common_name) with NA scientific_name_clements2024:")
  print(dups_na)
}

# merge NA-clements groups by (scientific_name, common_name)
na_merged <- if (nrow(na_df) > 0) {
  na_df %>%
    group_by(scientific_name, common_name) %>%
    summarise(across(everything(), combine_vals), .groups = "drop")
} else {
  na_df
}

# Rebuild df (keep original column order)
col_order <- names(df)
df <- bind_rows(ok_df, na_merged) %>% select(all_of(col_order))

# Ensure Clements-first ordering (where Clements name exists)
df <- df %>%
  mutate(
    scientific_name = mapply(reorder_clements_first,
                             scientific_name, scientific_name_clements2024,
                             USE.NAMES = FALSE),
    common_name     = mapply(reorder_clements_first,
                             common_name, common_name_clements2024,
                             USE.NAMES = FALSE)
  )

# =====================================================
# PART 2: Manual mapping merges (4 columns)
#  - Moves non-NA cells from incorrect row -> correct row (without touching *clements* cols)
#  - Appends with "; " (no overwriting), then deletes the incorrect row
#  - Works even when *clements* columns are not NA
# =====================================================
name_map <- tribble(
  ~incorrect_scientific,     ~incorrect_common,      ~correct_scientific,   ~correct_common,
  "Acanthis hornemanni",     "Hoary Redpoll",      "Acanthis flammea hornemanni/exilipes",  "Redpoll (Hoary)",
  "Accipiter atricapillus",      "American Goshawk",     "Astur atricapillus",  "American Goshawk",   
  "Accipiter bicolor",      "Bicolored Hawk",     "Astur bicolor",  "Bicolored Hawk",
  "Accipiter cooperii",      "Cooper's Hawk",     "Astur cooperii",  "Cooper's Hawk",
  "Accipiter gentilis",      "Northern Goshawk",     "Astur atricapillus",  "American Goshawk",   
  "Accipiter sp.",      "unid. Accipiter hawk",     "Aerospiza/Tachyspiza/Accipiter/Astur sp.", "Accipitrine hawk sp. (former Accipiter sp.)",
  "Anas platyrhynchos x rubripes/diazi/fulvigula",      "hybrid Mallard x Black/Mexican/Mottled Duck",     "Anas platyrhynchos x rubripes", "Mallard x American Black Duck (hybrid)",
  "Anser caerulescens (blue form)",      "(Blue Goose) Snow Goose",     "Anser caerulescens",  "Snow Goose",
  "Ardeid sp.",      "unid. heron/egret",     "Ardea sp.",  "Ardea sp.",
  "Bubulcus ibis",      "Cattle Egret",     "Ardea ibis",  "Western Cattle-Egret",
  "Cacomantis leucolophus",      "White-crowned Koel",     "Caliechthrus leucolophus",  "White-crowned Cuckoo",
  "Cacomantis pallidus",      "Pallid Cuckoo",     "Heteroscenes pallidus",  "Pallid Cuckoo",
  "Calocitta colliei",      "Black-throated Magpie-Jay",     "Cyanocorax colliei",  "Black-throated Magpie-Jay",
  "Calocitta formosa",      "White-throated Magpie-Jay",     "Cyanocorax formosus",  "White-throated Magpie-Jay",
  "Carduelis flammea/hornemanni",      "unid. Common Redpoll/Hoary Redpoll",     "Acanthis flammea [flammea Group/hornemanni/exilipes]",  "Redpoll (Common/Hoary)",  
  "Carpodacus purpureus/cassinii/mexicanus",      "unid. Purple Finch/Cassin's Finch/House Finch",     "Haemorhous sp.",  "Haemorhous sp.",
  "Charadrius montanus",      "Mountain Plover",     "Anarhynchus montanus",  "Mountain Plover", 
  "Charadrius nivosus",      "Snowy Plover",     "Anarhynchus nivosus",  "Snowy Plover", 
  "Charadrius wilsonia",      "Wilson's Plover",     "Anarhynchus wilsonia",  "Wilson's Plover", 
  "Chordeiles acutipennis/minor",      "unid. Lesser Nighthawk/Common Nighthawk",     "Chordeiles sp.",  "nighthawk sp.", 
  "Chrysococcyx basalis",      "Horsfield's Bronze-Cuckoo",     "Chalcites basalis",  "Horsfield's Bronze-Cuckoo", 
  "Chrysococcyx lucidus",      "Shining Bronze-Cuckoo",     "Chalcites lucidus",  "Shining Bronze-Cuckoo", 
  "Chrysococcyx megarhynchus",      "Long-billed Cuckoo",     "Chalcites megarhynchus",  "Long-billed Cuckoo", 
  "Chrysococcyx meyerii",      "White-eared Bronze Cuckoo",     "Chalcites meyerii",  "White-eared Bronze-Cuckoo", 
  "Chrysococcyx minutillus",      "Little Bronze Cuckoo",     "Chalcites minutillus",  "Little Bronze-Cuckoo", 
  "Chrysococcyx osculans",      "Black-eared Cuckoo",     "Chalcites osculans",  "Black-eared Cuckoo", 
  "Chrysococcyx ruficollis",      "Rufous-throated Bronze Cuckoo",     "Chalcites ruficollis",  "Rufous-throated Bronze-Cuckoo", 
  "Colaptes auratus auratus x auratus cafer",      "hybrid Northern Flicker (Red x Yellow-shafted)",    "Colaptes auratus luteus x cafer",  "Northern Flicker (Yellow-shafted x Red-shafted)",
  "Coragyps/Cathartes atratus/aura",      "unid. Black Vulture/Turkey Vulture",     "Cathartes sp.",  "Cathartes sp.",
  #  https://birdsoftheworld.org/bow/species/amecro/cur/introduction: Published August 8, 2025. Until recently, the American Crow in the northwest was classified as a separate species ("Northwestern Crow," Corvus caurinus), but because of extensive interbreeding with the Western American Crow (C. brachyrhynchos hesperis) in areas of co-occurrence, those in the northwest are now considered a subspecies of the American Crow (C. b. caurinus). 
  #  No common name in Clements for this species.
  "Corvus brachyrhynchos/caurinus",      "unid. American Crow/Northwestern Crow",     "Corvus brachyrhynchos caurinus",  NA,  
  "Corvus caurinus",      "Northwestern Crow",     "Corvus brachyrhynchos caurinus",  NA,  
  "Corvus cryptoleucus/corax",      "unid. Chihuahuan Raven/Common Raven",     "Corvus cryptoleucus/corax",  "Chihuahuan Raven/Common Raven",  
  "Cyanecula svecica",      "Bluethroat",     "Luscinia svecica",  "Bluethroat",
  "Empidonax difficilis/occidentalis",      "unid. Cordilleran/Pacific-slope Flycatcher",     "Empidonax difficilis occidentalis/hellmayri",  "Western Flycatcher (Cordilleran)",
  # Clements doesn't list name for the Cordilleran Flycatcher but it is referred toin BOW
  "Empidonax occidentalis",      "Cordilleran Flycatcher",     "Empidonax difficilis hellmayri",  NA,
  "Gull sp.",      "unid. gull",     "Larinae sp.",  "gull sp.",
  "Habia fuscicauda",      "Red-throated Ant-Tanager",     "Driophlox fuscicauda",  "Red-throated Ant-Tanager",
  "Hapalocrex flaviventer",      "Yellow-breasted Crake",     "Laterallus flaviventer",  "Yellow-breasted Crake",
  "Hydropsalis maculicaudus",      "Spot-tailed Nightjar",     "Antiurus maculicaudus",  "Spot-tailed Nightjar",
  "Ixobrychus exilis",      "Least Bittern",     "Botaurus exilis",  "Least Bittern",
  "Loxia sinesciuris/curvirostra",      "Unid. Cassia Crossbill/Red Crossbill; unid. Cassia Crossbill/Red Crossbill",     "Loxia curvirostra/sinesciuris",  "Red/Cassia Crossbill",
  "Nannopterum auritum/carbo",      "unid. Double-crested Cormorant/Great Cormorant",     "Phalacrocorax carbo/Nannopterum auritum",  "Great/Double-crested Cormorant",
  "Nannopterum brasilianum/auritum",      "unid. Neotropic/Double-crested Cormorant",     "Nannopterum auritum/brasilianum",  "Double-crested/Neotropic Cormorant",
  "Nycticorax/Nyctanassa nycticorax/violacea",      "unid Black-crowned/Yellow-crowned Night-Heron",     "Nycticorax nycticorax/Nyctanassa violacea",  "Yellow-crowned/Black-crowned Night Heron",
  "Poecile atricapillus/hudsonicus",      "unid. Black-capped Chickadee/Boreal Chickadee",     "Poecile atricapillus/hudsonicus",  "Black-capped/Boreal Chickadee",  
  "Polioptila caerulea/melanura",      "unid. Blue-gray/Black-tailed Gnatcatcher",     "Polioptila caerulea/melanura",  "Blue-gray/Black-tailed Gnatcatcher",  
  "Porphyrio martinicus",      "Purple Gallinule",     "Porphyrio martinica",  "Purple Gallinule",  
  "Psilorhinus morio",      "Brown Jay",     "Cyanocorax morio",  "Brown Jay",   
  "Rallus crepitans/elegans",      "unid. Clapper/King Rail",     "Rallus elegans/crepitans",  "King/Clapper Rail",   
  "Streptopelia chinensis",      "Spotted Dove",     "Spilopelia chinensis",  "Spotted Dove",  
  "Sturnella magna/neglecta",      "unid. Eastern Meadowlark/Western Meadowlark",     "Sturnella neglecta/magna",  "Western/Eastern Meadowlark",  
  "Tern sp.",      "unid. tern",     "Sterninae sp.",  "tern sp.",  
  "Tringa melanoleuca/flavipes",      "unid. Greater Yellowlegs/Lesser Yellowlegs",     "Tringa flavipes/melanoleuca",  "Lesser/Greater Yellowlegs",  
  "Trochilid sp.",      "unid. hummingbird",     "Trochilidae sp.",  "hummingbird sp.",  
  "Uria aalge/lomvia",      "unid. Common Murre/Thick-billed Murre",     "Uria lomvia/aalge",  "Thick-billed/Common Murre",
  "Vermivora cyanoptera x chrysoptera",      "Brewster's Warbler (Golden-winged x Blue-winged)",     "Vermivora chrysoptera x cyanoptera (F1 hybrid)",  "Brewster's Warbler (hybrid)",
  "Vireo plumbeus/cassinii",      "unid. Plumbeous Vireo/Cassin's Vireo",     "Vireo cassinii/plumbeus",  "Cassin's/Plumbeous Vireo",
  "Woodpecker sp.",      "unid. woodpecker",     "Picidae sp.",  "woodpecker sp."
)

for (i in seq_len(nrow(name_map))) {
  inc_sci <- name_map$incorrect_scientific[i]
  inc_com <- name_map$incorrect_common[i]
  cor_sci <- name_map$correct_scientific[i]
  cor_com <- name_map$correct_common[i]
  
  incorrect_idx <- which(df$scientific_name == inc_sci & df$common_name == inc_com)
  correct_idx   <- which(df$scientific_name == cor_sci & df$common_name == cor_com)
  
  if (length(incorrect_idx) == 1 && length(correct_idx) == 1) {
    # transfer every non-clements col
    for (col in names(df)) {
      if (grepl("clements", col, ignore.case = TRUE)) next
      wrong_val <- df[[col]][incorrect_idx]
      right_val <- df[[col]][correct_idx]
      if (!is.na(wrong_val) && wrong_val != "") {
        df[[col]][correct_idx] <- append_if_new(right_val, wrong_val)
      }
    }
    # reorder canonical names on the corrected row (Clements first)
    df$scientific_name[correct_idx] <-
      reorder_clements_first(df$scientific_name[correct_idx], df$scientific_name_clements2024[correct_idx])
    df$common_name[correct_idx] <-
      reorder_clements_first(df$common_name[correct_idx], df$common_name_clements2024[correct_idx])
    
    # drop the incorrect row
    df <- df[-incorrect_idx, ]
  }
}

# =====================================================
# FINAL: Checking & Reports
# =====================================================
# 1) Remaining NA in scientific_name_clements2024 (sorted)
remaining_na <- df %>%
  filter(is.na(scientific_name_clements2024)) %>%
  distinct(scientific_name, common_name) %>%
  arrange(scientific_name)

cat("\n--- Species with NA in scientific_name_clements2024 ---\n")
print(remaining_na) #11 rows; these include 'unid' and some rows that need manual edits

# 2) Rows with semicolons in either name (for inspection); these are the species
#  with 2 or more names in the dataset
semicolon_rows <- df %>%
  filter(str_detect(scientific_name, ";") | str_detect(common_name, ";")) %>%
  select(scientific_name, common_name, scientific_name_clements2024, common_name_clements2024) %>%
  distinct() %>%
  arrange(scientific_name)

cat("\n--- Species with ';' in scientific_name or common_name ---\n")
print(semicolon_rows) #153 rows

# 3) Mismatches (combined names vs Clements) — expected when aliases exist
name_mismatches <- df %>%
  filter(scientific_name != scientific_name_clements2024 |
           common_name     != common_name_clements2024) %>%
  distinct(scientific_name, scientific_name_clements2024,
           common_name,     common_name_clements2024) %>%
  arrange(scientific_name)

cat("\n--- Rows where names do not match *_clements2024 ---\n")
print(name_mismatches) # these are the same 153; makes sense.

# Check that the row that differ among initial data and modified make sense.
## Subset to rows with non-NA region_avibase8.17 (which are Canada & CONUS)
ca.conus.sp <- subset(df, !is.na(df[["region_avibase8.17"]]))
dim(avibase)-dim(ca.conus.sp) # no lost species
bbs.sp <- subset(df, !is.na(df[["scientific_name_bbs2024"]]))
dim(bbs2024)-dim(bbs.sp) # lost 1 species (Snow Goose, Blue Form merged. See ";" in AOU column) 
orig.sp <- subset(df, !is.na(df[["origlist_dataentry_origlist"]]))
origlist1 <- subset(origlist[origlist$origlist_dataentry != "",])
dim(origlist1)-dim(orig.sp) # 16 lost to the combination above (they have ";" in the columns)

# **** FOR LATER/when editing for Western Hemisphere: re-write above section to automatically check that the rows are the ones with the semicolons, by initial dataset.

# Change the yes/no columns to updated values reflecting current df rows 
# --- Ensure the expected lookup columns exist so we don't get "column not found" errors ---
lookup_cols <- c(
  "scientific_name_avibase8.17",
  "scientific_name_bbs2024",
  "scientific_name_clements2024",
  "scientific_name_orig"
)
for (col in lookup_cols) {
  if (!col %in% names(df)) df[[col]] <- NA_character_
}

# --- Map lookup columns to the in_* flag names you want updated ---
map_lookup_to_in <- list(
  scientific_name_avibase8.17 = "in_avibase8.17",
  scientific_name_bbs2024     = "in_bbs2024",
  scientific_name_clements2024= "in_clements2024",
  scientific_name_orig        = "in_origlist"
)

# --- Overwrite the in_* flags based on presence (non-NA & non-empty) of the lookup fields ---
for (lookup_col in names(map_lookup_to_in)) {
  in_col <- map_lookup_to_in[[lookup_col]]
  # create the in_* column if missing
  if (!in_col %in% names(df)) df[[in_col]] <- NA_character_
  df[[in_col]] <- ifelse(!is.na(df[[lookup_col]]) & df[[lookup_col]] != "", "yes", "no")
}

# --- Subset: rows that have a non-NA, non-empty region_avibase8.17 ---
if (!"region_avibase8.17" %in% names(df)) {
  df$region_avibase8.17 <- NA_character_
}
df_avibase_region <- df %>%
  filter(!is.na(region_avibase8.17) & region_avibase8.17 != "")

# --- Quick confirmations ---
cat("Number of rows in df:", nrow(df), "\n") # 35606
cat("Number of rows with region_avibase8.17:", nrow(df_avibase_region), "\n\n") #1091 #1104 with updated correct Canada list

cat("Counts for in_* flags:\n")
for (in_col in unique(unname(map_lookup_to_in))) {
  if (in_col %in% names(df)) {
    cat(in_col, ":\n")
    print(table(df[[in_col]], useNA = "ifany"))
    cat("\n")
  }
}
# in_avibase8.17 :
#   no   yes 
# 34502  1104 
#
# in_bbs2024 :
#   no   yes 
# 34844   762 
# 
# in_clements2024 :
#   no   yes 
# 11 35595 
# 
# in_origlist :
#   
#   no   yes 
# 34175  1431 

# Keep all the BBS species, and also the AviBase species that are in Canada and CONUS
# ---- Final subsetting ----

# 1. Keep rows with non-NA scientific_name_bbs2024 OR non-NA status_avibase8.17
df_bbs_or_avibase <- df %>%
  filter(!is.na(scientific_name_bbs2024) | !is.na(status_avibase8.17))

# 2. Subset further: drop "Rare" rows, but always keep rows with non-NA scientific_name_bbs2024
df_bbs_or_avibase_no_rare <- df_bbs_or_avibase %>%
  filter(
    !str_detect(status_avibase8.17, "\\bRare\\b") | !is.na(scientific_name_bbs2024)
  )
# indicate these should be kept fo ca.conus subset
df_bbs_or_avibase_no_rare$L1_ca.conus.data<-"yes"

# 3. Start from df_bbs_or_avibase_no_rare
# Omit rows where origlist_dataentry_origlist has NA or "NA" or "N/A"
# Examples: NA, "N/A; unidentified species", "N/A; hybrid species"
ca.conus.dataentered <- df_bbs_or_avibase_no_rare %>%
  filter(
    !is.na(origlist_dataentry_origlist) | # Exclude actual NA values
      origlist_dataentry_origlist != "N/A; hybrid species" | # Exclude string 
      origlist_dataentry_origlist != "N/A; unidentified species" |
      origlist_dataentry_origlist != "NA; no interactions from BOW bc incorporated into species above"
  )
dim(ca.conus.dataentered) #774

# 4. Keep the rows that DID have NA,N/A in origlist_dataentry_origlist
# Rows that DO contain NA,N/A in origlist_dataentry_origlist ----
missing_dataentry15Aug2025 <- df_bbs_or_avibase_no_rare %>%
  filter(
      is.na(origlist_dataentry_origlist) | # actual NA values
      origlist_dataentry_origlist == "N/A; hybrid species" | # string 
      origlist_dataentry_origlist == "N/A; unidentified species" |
      origlist_dataentry_origlist == "NA; no interactions from BOW bc incorporated into species above"
  )
dim(missing_dataentry15Aug2025) #117; #126 with new correct Canada list
# Upon inspection, the missing_dataentry15Aug2025 species are either hybrids, 
# species at the Genus level, or species that are not found in BOW. 
# So the original list is close enough.
# Here is the final subset for the full L1 list: df_bbs_or_avibase_no_rare (832 species)
# The *original* list L1 data that omits species that remain and are hybrid/Genus, etc: 
# bbs2024_or_avibase8.17_not_rare_entered.csv (707 species).

# Species we could enter out of the 125 non-entries in missing_dataentry15Aug2025: 
# Subset where NA is in AOU_bbs2024... these are the full species (not hybrids) 
# that we might enter but may not actually be North American birds. Check them on BOW.
df_bbs_or_avibase_no_rare1 <- df_bbs_or_avibase_no_rare %>%
  filter(is.na(AOU_bbs2024))
dim(df_bbs_or_avibase_no_rare1) #62; #71 rows with correct Canada list
print(df_bbs_or_avibase_no_rare1[,1:2],n=100)

# Create a column indicating the reason for rejection
df_bbs_or_avibase_no_rare$ca.conus.rejection<-NA
# Fill a new column 'description' based on the 'category' column
# List of species that are not found in North America (likely pets/accidental)
pets.accidental <- c("Acridotheres cristatellus",
                     "Agapornis roseicollis",
                     "Aix galericulata",
                     "Amazona autumnalis",
                     "Amazona finschi",
                     "Amazona oratrix",
                     "Anhinga rufa",
                     "Anser indicus",
                     "Anthropoides virgo",
                     "Apus nipalensis",
                     "Brotogeris versicolurus",
                     "Calidris tenuirostris",
                     "Callipepla douglasii",
                     "Cardellina rubra",
                     "Chloris chloris",
                     "Chloris sinica",
                     "Corvus cornix",
                     "Corvus splendens",
                     "Crithagra mozambica",
                     "Cyanocorax colliei",
                     "Cyanocorax colliei; Calocitta colliei",
                     "Daptrius chimachima",
                     "Egretta eulophotes",
                     "Fringilla coelebs",
                     "Geranoaetus polyosoma",
                     "Gracula religiosa",
                     "Lonchura atricapilla",
                     "Lophura nycthemera",
                     "Machetornis rixosa",
                     "Melopyrrha violacea",
                     "Milvus migrans",
                     "Paroaria capitata",
                     "Parus major",
                     "Pelecanus rufescens",
                     "Phasianus versicolor",
                     "Porphyrio poliocephalus",
                     "Psittacara erythrogenys",
                     "Psittacara mitratus",
                     "Pycnonotus cafer",
                     "Pycnonotus jocosus",
                     "Spinus spinus",
                     "Spodiopsar cineraceus",
                     "Sporophila bouvronides",
                     "Sporophila torqueola",
                     "Tadorna ferruginea",
                     "Tetraogallus himalayensis",
                     "Threskiornis aethiopicus",
                     "Toxostoma cinereum",
                     "Zonotrichia capensis",
                     "Zosterops japonicus")
# Find the rows where scientific_name contains any of the pets.accidental species
rows.pets.accidental <- df_bbs_or_avibase_no_rare$scientific_name %in% pets.accidental
# Replace the character values in ca.conus.rejection for those identified rows
df_bbs_or_avibase_no_rare$ca.conus.rejection[rows.pets.accidental] <- "no BBS, pets or accidental"

# List of species that are not found in North America (pelagic or marine focused)
pelagic.marine <- c("Ardenna bulleri",
                    "Ardenna carneipes",
                    "Ardenna creatopus",
                    "Ardenna grisea",
                    "Hydrobates cheimomnestes", # but close to CA/Channel Islands
                    "Hydrobates homochroa", # but close to CA/Channel Islands
                    "Hydrobates socorroensis", # but close to CA/Channel Islands
                    "Phoebastria immutabilis",
                    "Phoebastria nigripes",
                    "Phoebetria palpebrata",
                    "Pterodroma hasitata")

# Find the rows where species_name contains any of the pelagic.marine species
rows.pelagic.marine <- df_bbs_or_avibase_no_rare$scientific_name %in% pelagic.marine
# Replace the character values in ca.conus.rejection for those identified rows
df_bbs_or_avibase_no_rare$ca.conus.rejection[rows.pelagic.marine] <- "no BBS, pelagic or marine"

# List of species that are not found in North America (Hawaii)
hawaii <- c("Branta sandvicensis")
# Find the rows where species_name contains any of the hawaii species
rows.hawaii <- df_bbs_or_avibase_no_rare$scientific_name %in% hawaii
# Replace the character values in ca.conus.rejection for those identified rows
df_bbs_or_avibase_no_rare$ca.conus.rejection[rows.hawaii] <- "no BBS, Hawaii"

# List of species that are extinct, extirpated or critically endangered/super rare
extinct.or.extirpated.ce <- c("Campephilus principalis",
            "Ectopistes migratorius",
            "Numenius borealis")
# Find the rows where species_name contains any of the extinct or extirpated species
rows.gone <- df_bbs_or_avibase_no_rare$scientific_name %in% extinct.or.extirpated.ce
# Replace the character values in ca.conus.rejection for those identified rows
df_bbs_or_avibase_no_rare$ca.conus.rejection[rows.gone] <- "no BBS, extinct/extirpated/critically endangered/super rare"

# Species in CANADA/CONUS that we have already entered in 'species' folder:
# Fratercula arctica                    Atlantic Puffin
# Loxia sinesciuris                     Cassia Crossbill
# Synthliboramphus craveri              Craveri's Murrelet
# Vermivora bachmanii                   Bachman's Warbler

# Species in CANADA/CONUS that we have already entered in 'species_in_review' folder:
# Psittacara holochlorus                Green Parakeet
# Puffinus opisthomelas                 Black-vented Shearwater
# Do the final check on these interactions.

# CANADA/CONUS Species that we don't have entered and should be entered now:
# Synthliboramphus scrippsi Scripps's Murrelet
 
# Make sure these are the ones left:
df_bbs_or_avibase_no_rare2 <- df_bbs_or_avibase_no_rare %>%
  filter(is.na(AOU_bbs2024))
dim(df_bbs_or_avibase_no_rare2) #71 rows
print(df_bbs_or_avibase_no_rare2[,c(1:2,30)],n=100)
# Yes these match. Now update the 3 species interaction CSVs above to ensure 
# they are included when merged in.

# Omit the species that have a ca.conus.rejection value
# Replace empty strings with NA 
dim(df_bbs_or_avibase_no_rare)
ca.conus.splist <- df_bbs_or_avibase_no_rare %>%
  filter(is.na(ca.conus.rejection))
dim(ca.conus.splist)
# --- Export as CSVs
write_csv(ca.conus.splist, file.path(L1_dir,"canada.conus.splist.allnames_L1.csv"))
write_csv(df_bbs_or_avibase, file.path(L1_dir,"bbs2024_or_avibase8.17.csv"))
write_csv(df_bbs_or_avibase_no_rare,file.path(L1_dir, "bbs2024_or_avibase8.17_not_rare.csv"))
write_csv(ca.conus.dataentered, file.path(L1_dir,"bbs2024_or_avibase8.17_not_rare_ca.conus_entered15Aug2025.csv"))
write_csv(missing_dataentry15Aug2025, file.path(L1_dir,"bbs2024_or_avibase8.17_not_rare_not_entered15Aug2025.csv"))



