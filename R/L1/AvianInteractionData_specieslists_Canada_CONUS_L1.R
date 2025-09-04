# TITLE:          L1 Species Lists: North America (CONUS and Canada) 
#                 Reads in lists and keeps track of which species are in which list.
#                 Also keeps track of name changes in diff columns.
#                 Creates a final list of species in Canada and CONUS for use in the North 
#                 American Avian Interaction data paper and avian-meta-network paper.
#                 
#                 ****See AvianInteractionData_specieslists_L1.R for the 
#                 comprehensive global list.
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
#                         bbs_splist_2024_L0.csv
#                     all from AvianInteractionData_specieslists_L0.R
# DATA OUTPUT:    L1 data: canada.conus.splist_L1.CSV 
# PROJECT:        Avian Interaction Database & avian-meta-network
# DATE:           17 January 2022 - 1 Sep 2025
#                 
#                 Next script to run: AvianInteractionData_L1.R

# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)
library(stringr)
#library(purrr)

# Local directories
L0_dir <- "/Users/plz/Documents/GitHub/Avian-Interaction-Database-Working/L0/species_checklists"
L1_dir <- "/Users/plz/Documents/GitHub/Avian-Interaction-Database-Working/L1/species_checklists"

#### Read in AviBase species list: all species in Canada and CONUS (with Clements 2024 taxonomy)
avibase<-read.csv(file.path(L0_dir,"avibase_ca.conus_splist_2024_L0.csv"), fileEncoding="UTF-8")

# Name columns for merging later
names(avibase)
avibase$list<-"AviBase Canada & CONUS"

# Sort by scientific name
avibase <- avibase %>% arrange(scientific_name)
head(avibase)

#### Official 2024 eBird & Clements checklist
clements2024<-read.csv(file.path(L0_dir,"eBird-Clements-v2024-integrated-checklist-October-2024-rev.csv"))

# Clements List: Name the columns for merging later
clements2024<-subset(clements2024, select=c("English.name","scientific.name",
                                            "range","family","order"))

names(clements2024)
clements2024$list<-"Clements-eBird2024"
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
  "common_name_clean","scientific_name_clean",
  "in_avibase8.17","in_bbs2024","in_clements2024"
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

# --- Merge all sources on normalized keys ---
master <- avibase_std %>%
  full_join(bbs2024_std, by = c("common_name_clean", "scientific_name_clean")) %>%
  full_join(clements2024_std, by = c("common_name_clean", "scientific_name_clean")) 

# --- Unified names (priority: Clements → Avibase → BBS) ---
master1 <- master %>%
  mutate(
    common_name = coalesce(common_name_clements2024, common_name_avibase8.17, common_name_bbs2024),
    scientific_name = coalesce(scientific_name_clements2024, scientific_name_avibase8.17, scientific_name_bbs2024)
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
dim(master2) #35708
dim(master3) #35708
dim(master3)-dim(clements2024) #112 that differ

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
dim(master5)-dim(clements2024) #38 that still differ
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
  # Clements doesn't list name for the Cordilleran Flycatcher but it is referred to in BOW
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
print(remaining_na) #8 rows; these include 'unid' and some rows that need manual edits

# 2) Rows with semicolons in either name (for inspection); these are the species
#  with 2 or more names in the dataset
semicolon_rows <- df %>%
  filter(str_detect(scientific_name, ";") | str_detect(common_name, ";")) %>%
  select(scientific_name, common_name, scientific_name_clements2024, common_name_clements2024) %>%
  distinct() %>%
  arrange(scientific_name)

cat("\n--- Species with ';' in scientific_name or common_name ---\n")
print(semicolon_rows) #99 rows

# 3) Mismatches (combined names vs Clements) — expected when aliases exist
name_mismatches <- df %>%
  filter(scientific_name != scientific_name_clements2024 |
           common_name     != common_name_clements2024) %>%
  distinct(scientific_name, scientific_name_clements2024,
           common_name,     common_name_clements2024) %>%
  arrange(scientific_name)

cat("\n--- Rows where names do not match *_clements2024 ---\n")
print(name_mismatches) # these are the same 99; makes sense.

# Check that the row that differ among initial data and modified make sense.
## Subset to rows with non-NA regions_avibase8.17 (which are Canada & CONUS)
ca.conus.sp <- subset(df, !is.na(df[["regions_avibase8.17"]]))
dim(avibase)-dim(ca.conus.sp) # no lost species
bbs.sp <- subset(df, !is.na(df[["scientific_name_bbs2024"]]))
dim(bbs2024)-dim(bbs.sp) # lost 1 species (Snow Goose, Blue Form merged. See ";" in AOU column) 

# **** FOR LATER/when editing for Western Hemisphere: re-write above section to automatically check that the rows are the ones with the semicolons, by initial dataset.

# Change the yes/no columns to updated values reflecting current df rows 
# --- Ensure the expected lookup columns exist so we don't get "column not found" errors ---
lookup_cols <- c(
  "scientific_name_avibase8.17",
  "scientific_name_bbs2024",
  "scientific_name_clements2024"
)
for (col in lookup_cols) {
  if (!col %in% names(df)) df[[col]] <- NA_character_
}

# --- Map lookup columns to the in_* flag names you want updated ---
map_lookup_to_in <- list(
  scientific_name_avibase8.17 = "in_avibase8.17",
  scientific_name_bbs2024     = "in_bbs2024",
  scientific_name_clements2024= "in_clements2024"
)

# --- Overwrite the in_* flags based on presence (non-NA & non-empty) of the lookup fields ---
for (lookup_col in names(map_lookup_to_in)) {
  in_col <- map_lookup_to_in[[lookup_col]]
  # create the in_* column if missing
  if (!in_col %in% names(df)) df[[in_col]] <- NA_character_
  df[[in_col]] <- ifelse(!is.na(df[[lookup_col]]) & df[[lookup_col]] != "", "yes", "no")
}

# --- Subset: rows that have a non-NA, non-empty regions_avibase8.17 ---
if (!"regions_avibase8.17" %in% names(df)) {
  df$regions_avibase8.17 <- NA_character_
}
df_avibase_region <- df %>%
  filter(!is.na(regions_avibase8.17) & regions_avibase8.17 != "")

# --- Quick confirmations ---
cat("Number of rows in df:", nrow(df), "\n") # 35603
cat("Number of rows with region_avibase8.17:", nrow(df_avibase_region), "\n\n") #1104 with correct Canada list: #1104

cat("Counts for in_* flags:\n")
for (in_col in unique(unname(map_lookup_to_in))) {
  if (in_col %in% names(df)) {
    cat(in_col, ":\n")
    print(table(df[[in_col]], useNA = "ifany"))
    cat("\n")
  }
}
#in_avibase8.17 :
#   no   yes 
# 34499  1104 
# 
# in_bbs2024 :
#   no   yes 
# 34841   762 
# 
# in_clements2024 :
#   no   yes 
# 8 35595 

# Keep all the BBS species, and also the AviBase species that are in Canada and CONUS
# ---- Final subsetting ----

# 1. Keep rows with non-NA scientific_name_bbs2024 OR non-NA status_avibase8.17
# Create summary status column for CA, US48, USak
df.ca.conus <- df %>%
  mutate(
    status_CA_US48_USak = pmap_chr(
      list(status_CA_avibase8.17, status_US48_avibase8.17, status_USak_avibase8.17),
      ~ paste(na.omit(c(...)), collapse = "; ")
    )
  )

# 2. Keep only relevant status columns and all other metadata
# 2. Drop unwanted status_* columns, keep all others
df.ca.conus <- df.ca.conus %>%
  select(
    -any_of(c(
      "status_CAM_avibase8.17", "status_SAM_avibase8.17",
      "status_EUR_avibase8.17", "status_AFR_avibase8.17",
      "status_ASI_avibase8.17", "status_MID_avibase8.17",
      "status_OCE_avibase8.17", "status_AUS_avibase8.17",
      "status_PAC_avibase8.17", "status_hol_avibase8.17",
      "status_nea_avibase8.17", "status_pal_avibase8.17",
      "status_oaq_avibase8.17", "status_oat_avibase8.17",
      "status_oin_avibase8.17", "status_opa_avibase8.17",
      "status_XX_avibase8.17",  "status_UShi_avibase8.17",
      "status_CAR_avibase8.17", "status_nan_avibase8.17",
      "status_TT_avibase8.17"
    ))
  )
# Fill NAs where there are blanks now in the status column
df.ca.conus$status_CA_US48_USak[df.ca.conus$status_CA_US48_USak == "" | df.ca.conus$status_CA_US48_USak == " "] <- NA

# Omit NA rows
df_bbs_or_avibase <- df.ca.conus %>%
  filter(!is.na(scientific_name_bbs2024) | !is.na(status_CA_US48_USak)) 

dim(df_bbs_or_avibase) #1161 rows

# 3. Subset further: drop "Rare" rows, but always keep rows with non-NA scientific_name_bbs2024
# Where at least one of the 3 areas has the species as Rare (this is too strict):
#df_bbs_or_avibase_no_rare <- df_bbs_or_avibase %>%
#  filter(
#    !str_detect(status_CA_US48_USak, "\\bRare\\b") | !is.na(scientific_name_bbs2024)
#  )

# Drop where all 3 areas are Rare/Accidental:
df_bbs_or_avibase_no_rare <- df_bbs_or_avibase %>%
  filter((status_CA_US48_USak != "Rare/Accidental; Rare/Accidental; Rare/Accidental") & 
           (status_CA_US48_USak != "Rare/Accidental Near-threatened; Rare/Accidental Near-threatened; Rare/Accidental Near-threatened") & 
           (status_CA_US48_USak != "Rare/Accidental Vulnerable; Rare/Accidental Vulnerable; Rare/Accidental Vulnerable") & 
           (status_CA_US48_USak != "Rare/Accidental Vulnerable; Rare/Accidental Vulnerable; Rare/Accidental Vulnerable") | 
           !is.na(scientific_name_bbs2024))

# indicate these remaining should be kept for ca.conus subset
df_bbs_or_avibase_no_rare$L1_ca.conus.data<-"yes"

dim(df_bbs_or_avibase_no_rare) #1118

# Now work on omitting some of these species because they do not occur in Canada/CONUS continent.
# To do this, create a subset for the omissions.
# Subset where NA is in AOU_bbs2024... these are the full species (not hybrids) 
# but may not actually be North American birds. Check them on BOW.
df_bbs_or_avibase_no_rare1 <- df_bbs_or_avibase_no_rare %>%
  filter(is.na(AOU_bbs2024))
dim(df_bbs_or_avibase_no_rare1) #62 rows; 356 rows with correct Canada list.
write_csv(df_bbs_or_avibase_no_rare1, file.path(L0_dir,"df_bbs_or_avibase_no_rare1.csv"))

# Some of these are found in AFR, AUS, EUR, MID, XX, and are very likely accidentals in CONUS/Canada. Omit them.
df_bbs_or_avibase_no_rare2 <- df_bbs_or_avibase_no_rare1 %>%
    filter(
      !str_detect(regions_avibase8.17, "\\bAFR\\b|\\bAUS\\b|\\bEUR\\b|\\bMID\\b|\\bXX\\b")
    )
dim(df_bbs_or_avibase_no_rare2) #135 species

# Another dataset for the other portion:
df_bbs_or_avibase_no_rare3 <- df_bbs_or_avibase_no_rare1 %>%
  filter(
    str_detect(regions_avibase8.17, "\\bAFR\\b|\\bAUS\\b|\\bEUR\\b|\\bMID\\b|\\bXX\\b")
  )
dim(df_bbs_or_avibase_no_rare3) #221 species

# Work on each dataset by checking the species' range on BOW. 
# If it is outside Canada / CONUS, drop it.
# Create a column indicating the reason for rejection
df_bbs_or_avibase_no_rare$ca.conus.rejection<-NA
# Fill a new column 'description' based on the 'category' column
print(df_bbs_or_avibase_no_rare2[,1:2],n=135)
print(df_bbs_or_avibase_no_rare3[,1:2],n=221)

# List of species to consider keeping (pelagic):
# Aethia pygmaea Whiskered Auklet - Caroline is entering
# Synthliboramphus hypoleucus - Caroline is entering
# Rissa brevirostris	Red-legged Kittiwake - Caroline is entering

# Species to keep in ca.conus (and they are entered & checked):
# Aethia pusilla Least Auklet - all set
# Aethia cristatella Crested Auklet - all set
# Aethia psittacula Parakeet Auklet - all set
# Alle alle	Dovekie - all set
# Corvus imparatus - all set
# Fratercula arctica - all set
# Loxia sinesciuris Cassia Crossbill - all set
# Onychoprion fuscatus Sooty Tern - all set
# Plectrophenax hyperboreus McKay's Bunting - all set
# Psittacara holochlorus Green Parakeet - all set
# Puffinus opisthomelas - all set
# Synthliboramphus craveri - all set
# Synthliboramphus scrippsi - all set
# Urile urile - entered, needs checking
# Vermivora bachmanii - all set
# Vireo flavoviridis - entered by CR, needs checking

# List of species that are not found in North America (likely pets/accidental)
pets.accidental <- c("Accipiter nisus",
                     "Acridotheres cristatellus",
                     "Acrocephalus dumetorum",
                     "Acrocephalus schoenobaenus",
                     "Agapornis roseicollis",
                     "Agelaius humeralis",
                     "Aix galericulata",
                     "Amazona autumnalis",
                     "Amazona finschi",
                     "Amazona oratrix",
                     "Anarhynchus alexandrinus",
                     "Anarhynchus leschenaultii",
                     "Anarhynchus mongolus",
                     "Anas bahamensis",
                     "Anas zonorhyncha",
                     "Anser anser",
                     "Anhinga rufa",
                     "Anser brachyrhynchus",
                     "Anser erythropus",
                     "Anser indicus",
                     "Anthracothorax prevostii",
                     "Anthropoides virgo",
                     "Anthus gustavi",
                     "Anthus hodgsoni",
                     "Anthus trivialis",
                     "Apus nipalensis",
                     "Apus pacificus",
                     "Aramides axillaris",
                     "Ardea brachyrhyncha",
                     "Ardeola bacchus",
                     "Arundinax aedon",
                     "Astur gentilis",
                     "Basileuterus culicivorus",
                     "Basileuterus lachrymosus",
                     "Basileuterus rufifrons",
                     "Basilinna xantusii",
                     "Botaurus sinensis",
                     "Brotogeris versicolurus",
                     "Branta leucopsis",
                     "Buteo rufinus",
                     "Buteogallus urubitinga",
                     "Calidris falcinellus",
                     "Calidris pygmaea",
                     "Calidris subminuta",
                     "Calidris tenuirostris",
                     "Callipepla douglasii",
                     "Calonectris edwardsii",
                     "Calonectris leucomelas",
                     "Campephilus principalis",
                     "Camptorhynchus labradorius",
                     "Caprimulgus jotaka",
                     "Cardellina rubra",
                     "Carpodacus erythrinus",
                     "Carpodacus roseus",
                     "Catharus mexicanus",
                     "Chloris chloris",
                     "Chloris sinica",
                     "Chloroceryle amazona",
                     "Chroicocephalus cirrocephalus",
                     "Circus aeruginosus",
                     "Circus cyaneus",
                     "Coccothraustes coccothraustes",
                     "Coccyzus melacoryphus",
                     "Coereba flaveola",
                     "Colibri thalassinus",
                     "Coloeus monedula",
                     "Columba palumbus",
                     "Contopus caribaeus",
                     "Conuropsis carolinensis",
                     "Corvus cornix",
                     "Corvus splendens",
                     "Cyanerpes cyaneus",
                     "Crithagra mozambica",
                     "Cyanocompsa parellina",
                     "Cyanocorax colliei",
                     "Cyanocorax colliei; Calocitta colliei",
                     "Daptrius chimachima",
                     "Dendrocygna arborea",
                     "Elaenia parvirostris",
                     "Emberiza elegans",
                     "Emberiza variabilis",
                     "Empidonomus aurantioatrocristatus",
                     "Empidonomus varius",
                     "Euptilotis neoxenus",
                     "Falco rufigularis",
                     "Geothlypis poliocephala",
                     "Geotrygon chrysia",
                     "Geotrygon montana",
                     "Geranoaetus polyosoma",
                     "Geranospiza caerulescens",
                     "Harpagus bidentatus",
                     "Heliomaster constantii",
                     "Heliornis fulica",
                     "Hesperoburhinus bistriatus",
                     "Icterus abeillei",
                     "Icterus pustulatus",
                     "Icterus wagleri",
                     "Jabiru mycteria",
                     "Lampornis amethystinus",
                     "Legatus leucophaius",
                     "Leucophaeus modestus",
                     "Leucosticte arctoa",
                     "Lophura nycthemera",
                     "Machetornis rixosa",
                     "Margarops fuscatus",
                     "Melanoptila glabrirostris",
                     "Melanospiza bicolor",
                     "Melanotis caerulescens",
                     "Melopyrrha violacea",
                     "Mitrephanes phaeocercus",
                     "Mustelirallus erythrops",
                     "Myadestes occidentalis",
                     "Myiarchus nuttingi",
                     "Myiarchus sagrae",
                     "Myioborus miniatus",
                     "Myiopagis viridicata",
                     "Myiozetetes similis",
                     "Nesophlox evelynae",
                     "Oreothlypis superciliosa",
                     "Oriturus superciliosus",
                     "Pachyramphus major",
                     "Pardirallus maculatus",
                     "Paroaria capitata",
                     "Patagioenas squamosa",
                     "Periporphyrus celaeno",
                     "Phaetusa simplex",
                     "Phasianus versicolor",
                     "Pheucticus chrysopeplus",
                     "Piranga bidentata",
                     "Porphyrio flavirostris",
                     "Progne chalybea",
                     "Progne cryptoleuca",
                     "Progne elegans",
                     "Progne tapera",
                     "Psittacara erythrogenys",
                     "Psittacara mitratus",
                     "Ptiliogonys cinereus",
                     "Pygochelidon cyanoleuca",
                     "Rhynchopsitta pachyrhyncha",
                     "Ridgwayia pinicola",
                     "Rupornis magnirostris",
                     "Selasphorus heloisa",
                     "Spinus notatus",
                     "Spizella wortheni",
                     "Spodiopsar cineraceus",
                     "Sporophila bouvronides",
                     "Sporophila torqueola",
                     "Streptoprocne zonaris",
                     "Strix virgata",
                     "Tachornis phoenicobia",
                     "Tachycineta albilinea",
                     "Tachycineta cyaneoviridis",
                     "Thamnophilus doliatus",
                     "Tiaris olivaceus",
                     "Tigrisoma mexicanum",
                     "Tityra semifasciata",
                     "Toxostoma cinereum",
                     "Turdus assimilis",
                     "Turdus plumbeus",
                     "Turdus rufopalliatus",
                     "Tyrannus caudifasciatus",
                     "Vireo crassirostris",
                     "Vireo gundlachii",
                     "Vireo magister",
                     "Volatinia jacarina",
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
                    "Bulweria bulwerii",
                    "Calonectris edwardsii",
                    "Calonectris leucomelas",
                    "Creagrus furcatus",
                    "Hydrobates cheimomnestes", # but close to CA/Channel Islands
                    "Hydrobates homochroa", # but close to CA/Channel Islands
                    "Hydrobates hornbyi")
# Ardenna bulleri Buller's Shearwater - pelagic; needs entering
# Ardenna carneipes Flesh-footed Shearwater - pelagic; needs entering

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
                              "Camptorhynchus labradorius",
            "Conuropsis carolinensis",
            "Ectopistes migratorius",
            "Numenius borealis")
# Find the rows where species_name contains any of the extinct or extirpated species
rows.gone <- df_bbs_or_avibase_no_rare$scientific_name %in% extinct.or.extirpated.ce
# Replace the character values in ca.conus.rejection for those identified rows
df_bbs_or_avibase_no_rare$ca.conus.rejection[rows.gone] <- "no BBS, extinct/extirpated/critically endangered/super rare"

# Omit the species that have a ca.conus.rejection value
# Replace empty strings with NA 
dim(df_bbs_or_avibase_no_rare) #1118
ca.conus.splist <- df_bbs_or_avibase_no_rare %>%
  filter(is.na(ca.conus.rejection))
dim(ca.conus.splist) #769 species; #992 species

# --- Export the final 769 species CANADA & CONUS subset as a CSV
write_csv(ca.conus.splist, file.path(L1_dir,"canada.conus.splist_L1.csv"))

