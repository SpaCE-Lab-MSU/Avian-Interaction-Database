# TITLE:          L1 Species Lists: North America (CONUS and Alaska and Canada) 
#                 Reads in lists and keeps track of which species are in which list.
#                 Also keeps track of name changes in diff columns.
#                 Creates a final list of species in Canada / Alaska / CONUS for use in the North 
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
# DATA OUTPUT:    L1 data: canada.conus.splist_L1.CSV; splist_CanadaAKCONUS_L1.csv (column subset) 
# PROJECT:        Avian Interaction Database & avian-meta-network
# DATE:           17 January 2022 - 5 Nov 2025
#                 
#                 Next script to run: AvianInteractionData_L1.R

# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)
library(stringr)

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

# From a subsequent analysis, we learned that these species exist in the 
# 1966-2023 BBS observational data but are not included on the BBS list because they have
# been combined into American Crow. Here we add those 2 species to ensure the splist 
# creation goes smoothly; later we assign all species to American Crow and its AOU.combo.

# 4890	Corvus caurinus	34880	Corvus brachyrhynchos	(Northwestern Crow) American Crow
# 4882	Corvus brachyrhynchos / caurinus	34880	Corvus brachyrhynchos	unid. American Crow / Northwestern Crow
# 4882: Since either species is now Corvus brachyrhynchos we will make them a combo species with American Crow.
# 4890: Assign the Corvus caurinus to American crow (Corvus brachyrhynchos); this should be fixed in the creation of the list but for now we do it here.
# Add 2 new rows to the end
bbs2024 <- bbs2024 %>%
  add_row(AOU = 4890, 
          common_name = "Northwestern Crow",
          order = "Passeriformes",
          family = "Corvidae",
          scientific_name = "Corvus caurinus",
          list = "BBS 2024")

bbs2024 <- bbs2024 %>%
  add_row(AOU = 4882, 
          common_name = "unid. American Crow / Northwestern Crow",
          order = "Passeriformes",
          family = "Corvidae",
          scientific_name = "Corvus brachyrhynchos / caurinus",
          list = "BBS 2024")

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
dim(master2) #35710
dim(master3) #35710
dim(master3)-dim(clements2024) #114 rows that are not matching correctly with Clements 2024 names

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
dim(master5)-dim(clements2024) #40 that still differ
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
#  "Anser caerulescens (blue form)",      "(Blue Goose) Snow Goose",     "Anser caerulescens",  "Snow Goose",
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
  #  https://birdsoftheworld.org/bow/species/amecro/cur/introduction: Published August 8, 2025. Until recently, the American Crow in the northwest was classified as a separate species ("Northwestern Crow," Corvus caurinus), but because of extensive interbreeding with the Western American Crow (C. brachyrhynchos hesperis) in areas of co-occurrence, those in the northwest are now considered a subspecies of the American Crow (C. b. caurinus). We will assign all to American Crow.
  #  No common name in Clements for this species; assign American Crow (but this is done farther below, not here, as this step would omit a row)
  #"Corvus brachyrhynchos/caurinus",      "unid. American Crow/Northwestern Crow",     "Corvus brachyrhynchos",  "American Crow",  
  #"Corvus caurinus",      "Northwestern Crow",     "Corvus brachyrhynchos",  "American Crow",  
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
print(remaining_na) #11 rows
# these include 'unid' and some rows that need manual edits like Northwestern Crow

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
dim(bbs2024)-dim(bbs.sp) # 0 lost species 

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
cat("Number of rows in df:", nrow(df), "\n") # 35606
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
# 34502  1104 
# 
# in_bbs2024 :
#   no   yes 
# 34841   765 
# 
# in_clements2024 :
#   no   yes 
# 11 35595 

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
# Drop unwanted status_* columns, keep all others
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

dim(df_bbs_or_avibase) #1164 rows

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

dim(df_bbs_or_avibase_no_rare) #1121

# Now work on omitting some of these species because they do not occur in Canada/CONUS continent.
# To do this, create a subset for the omissions.
# Subset where NA is in AOU_bbs2024... these are the full species (not hybrids) 
# but may not actually be North American birds. 
df_bbs_or_avibase_no_rare1 <- df_bbs_or_avibase_no_rare %>%
  filter(is.na(AOU_bbs2024))
dim(df_bbs_or_avibase_no_rare1) #356 rows with correct Canada list.
# Export the list. Check them on BOW for their ranges.
# If it is outside Canada / CONUS, drop it.
write_csv(df_bbs_or_avibase_no_rare1, file.path(L0_dir,"df_bbs_or_avibase_no_rare_ca.conus_keep_or_not.csv"))

# Some of these are found in AFR, AUS, EUR, MID, XX, and are very likely accidentals in CONUS/Canada.
# After inspection, this list is too small to use as omission, and it includes some species
# that shouldn't be omitted.
df_bbs_or_avibase_no_rare2 <- df_bbs_or_avibase_no_rare1 %>%
    filter(
      !str_detect(regions_avibase8.17, "\\bAFR\\b|\\bAUS\\b|\\bEUR\\b|\\bMID\\b|\\bXX\\b")
    )
dim(df_bbs_or_avibase_no_rare2) #135 species

# List of species to consider keeping (pelagic):
# Aethia pygmaea Whiskered Auklet - Caroline is entering - plz checked. DONE
# Synthliboramphus hypoleucus - Caroline is entering - plz checked. DONE
# Rissa brevirostris	Red-legged Kittiwake - Caroline is entering- plz checked. DONE

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
# Vireo flavoviridis - entered by CR, plz checked. DONE

df_bbs_or_avibase_no_rare1a<-read.csv(file.path(L0_dir,"df_bbs_or_avibase_no_rare_ca.conus_keep_or_not_checked.csv"))
head(df_bbs_or_avibase_no_rare1a)
dim(df_bbs_or_avibase_no_rare1a) #356
dim(df_bbs_or_avibase_no_rare1) #356

# Merge the keep or omit list with the full list of species considered for North America
ca.conus.splist <- merge(df_bbs_or_avibase_no_rare,df_bbs_or_avibase_no_rare1a,
                         by=c("scientific_name","common_name","scientific_name_avibase8.17",
                              "common_name_avibase8.17","order_avibase8.17","family_avibase8.17"),
                         all.x=T, all.Y=T)
dim(ca.conus.splist) # 1121
# Fill in blank "keep" with "y"
ca.conus.splist <- ca.conus.splist %>%
  mutate(keep = replace_na(keep, "y"))
unique(ca.conus.splist$keep)
# Omit the species where keep="n"
ca.conus.splist <- ca.conus.splist %>%
  filter(keep !="n")
dim(ca.conus.splist) #785 species
# drop the 'keep' column
ca.conus.splist$keep<-NULL

# Create a combined AOU column (AOU_bbs2024.combo) to provide an option for 
# assigning subspecies to species when merging this list with interaction data.
ca.conus.splist$AOU_bbs2024.combo<-ca.conus.splist$AOU_bbs2024

# Grouping Rules provided by Jeff Hostetler @ BBS. 
# Update the splist AOU.combo for each subspecies species (assign it to the
# combo AOU). Make a selection for each subspecies group:

# 35280                     Redpoll (all groups)
# 5280 5275 5270
ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 5280] <- 35280
ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 5275] <- 35280
ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 5270] <- 35280

# 31320                     Mallard (including hybrid)
# 01320 01326
#ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 1320] <- 31320
#ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 1326] <- 31320


# 31940            Great Blue Heron (all forms)
# 01920 01940
ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 1920] <- 31940
ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 1940] <- 31940

# 33370             Red-tailed Hawk (all forms)
# 03370 03380
ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 3370] <- 33370
ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 3380] <- 33370

# 34120            Northern Flicker (all forms)
# 04120 04130 04123 04125
ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 4120] <- 34120
ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 4130] <- 34120
ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 4123] <- 34120
ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 4125] <- 34120

# 8  35670             Dark-eyed Junco (all forms) 
# 05660 05670 05671 05680 05690 05677
ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 5660] <- 35670
ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 5670] <- 35670
ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 5671] <- 35670
ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 5680] <- 35670
ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 5690] <- 35670
ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 5677] <- 35670

# 36550       Yellow-rumped Warbler (all forms)
# 06550 06560 06556
ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 6550] <- 36550
ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 6560] <- 36550
ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 6556] <- 36550

# 30010                 Western & Clark's Grebe; separate species in Clements so keep as separate
# 00010 00011 00012
# ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 10] <- 30010
# ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 11] <- 30010
# ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 12] <- 30010

# 34641  Cordilleran & Pacific-slope Flycatcher
# 04640 04641 04642
ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 4640] <- 34641
ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 4641] <- 34641
ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 4642] <- 34641

# 34660               Alder & Willow Flycatcher; separate species in Clements so keep as separate
# 04660 04661 04665
# ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 4660] <- 34660
# ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 4661] <- 34660
# ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 4665] <- 34660

# 34810      California & Woodhouse's Scrub-Jay; separate species in Clements so keep as separate
# 04812 04813 04810
# ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 4812] <- 34810
# ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 4813] <- 34810
# ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 4810] <- 34810

# 35740              Sagebrush & Bell's Sparrow; separate species in Clements so keep as separate
# 05738 05739 05740
# ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 5738] <- 35740
# ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 5739] <- 35740
# ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 5740] <- 35740

# 34880                           American / Northwestern Crow; same species in Clements 
# 04880 04882 04890
ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 4880] <- 34880
ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 4882] <- 34880
ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 4890] <- 34880

# Snow Goose (Blue and regular form)
ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 1690] <- 31690
ca.conus.splist$AOU_bbs2024.combo[ca.conus.splist$AOU_bbs2024 == 1691] <- 31690

# Add Clements name for Northwestern Crow:
#  https://birdsoftheworld.org/bow/species/amecro/cur/introduction: Published August 8, 2025. 
# Until recently, the American Crow in the northwest was classified as a separate species ("Northwestern Crow," Corvus caurinus), but because of extensive interbreeding with the Western American Crow (C. brachyrhynchos hesperis) in areas of co-occurrence, those in the northwest are now considered a subspecies of the American Crow (C. b. caurinus). We will assign all to American Crow.
#  No common name in Clements for this species; assign American Crow 
#"Corvus brachyrhynchos/caurinus",      "unid. American Crow/Northwestern Crow",     "Corvus brachyrhynchos",  "American Crow",  
#"Corvus caurinus",      "Northwestern Crow",     "Corvus brachyrhynchos",  "American Crow",  
ca.conus.splist$scientific_name_clements2024[ca.conus.splist$scientific_name == "Corvus brachyrhynchos/caurinus"] <- "Corvus brachyrhynchos"
ca.conus.splist$common_name_clements2024[ca.conus.splist$common_name == "unid. American Crow/Northwestern Crow"] <- "American Crow"
ca.conus.splist$scientific_name_clements2024[ca.conus.splist$scientific_name == "Corvus caurinus"] <- "Corvus brachyrhynchos caurinus"
# There is no common name for the subspecies in Clements, but it exists in BBS obs data
# so we will assign it American Crow here. 
ca.conus.splist$common_name_clements2024[ca.conus.splist$scientific_name_clements2024 == "Corvus brachyrhynchos caurinus"] <- "American Crow"

## Create a new column which contains Genus species for the combined species above, 
## based on the Clements name. Here we assign the Clements name.
ca.conus.splist$scientific_name_clements2024.combo<-ca.conus.splist$scientific_name_clements2024
# Same for common name combo
ca.conus.splist$common_name_clements2024.combo<-ca.conus.splist$common_name_clements2024

# Probably a more beautiful way to code this but this works:
# Assign all to new species combo name based on the Clements species-level name
#ca.conus.splist$scientific_name_clements2024.combo[ca.conus.splist$AOU_bbs2024.combo == 31320] <- "Anas platyrhynchos"
ca.conus.splist$scientific_name_clements2024.combo[ca.conus.splist$AOU_bbs2024.combo == 35280] <- "Acanthis flammea"
ca.conus.splist$common_name_clements2024.combo[ca.conus.splist$AOU_bbs2024.combo == 35280] <- "Redpoll"
ca.conus.splist$scientific_name_clements2024.combo[ca.conus.splist$AOU_bbs2024.combo == 31940] <- "Ardea herodias"
ca.conus.splist$common_name_clements2024.combo[ca.conus.splist$AOU_bbs2024.combo == 31940] <- "Great Blue Heron"
ca.conus.splist$scientific_name_clements2024.combo[ca.conus.splist$AOU_bbs2024.combo == 33370] <- "Buteo jamaicensis"
ca.conus.splist$common_name_clements2024.combo[ca.conus.splist$AOU_bbs2024.combo == 33370] <- "Red-tailed Hawk"
ca.conus.splist$scientific_name_clements2024.combo[ca.conus.splist$AOU_bbs2024.combo == 34120] <- "Colaptes auratus"
ca.conus.splist$common_name_clements2024.combo[ca.conus.splist$AOU_bbs2024.combo == 34120] <- "Northern Flicker"
ca.conus.splist$scientific_name_clements2024.combo[ca.conus.splist$AOU_bbs2024.combo == 35670] <- "Junco hyemalis"
ca.conus.splist$common_name_clements2024.combo[ca.conus.splist$AOU_bbs2024.combo == 35670] <- "Dark-eyed Junco"
ca.conus.splist$scientific_name_clements2024.combo[ca.conus.splist$AOU_bbs2024.combo == 36550] <- "Setophaga coronata"
ca.conus.splist$common_name_clements2024.combo[ca.conus.splist$AOU_bbs2024.combo == 36550] <- "Yellow-rumped Warbler"
#ca.conus.splist$scientific_name_clements2024.combo[ca.conus.splist$AOU.combo == 30010] <- "Aechmophorus occidentalis / clarkii"
ca.conus.splist$scientific_name_clements2024.combo[ca.conus.splist$AOU_bbs2024.combo == 34641] <- "Empidonax difficilis"
ca.conus.splist$common_name_clements2024.combo[ca.conus.splist$AOU_bbs2024.combo == 34641] <- "Western Flycatcher"
#ca.conus.splist$scientific_name_clements2024.combo[ca.conus.splist$AOU.combo == 34660] <- "Empidonax alnorum / traillii"
#ca.conus.splist$scientific_name_clements2024.combo[ca.conus.splist$AOU.combo == 34810] <- "Aphelocoma californica / woodhouseii"
#ca.conus.splist$scientific_name_clements2024.combo[ca.conus.splist$AOU.combo == 35740] <- "Artemisiospiza nevadensis / belli"
ca.conus.splist$common_name_clements2024.combo[ca.conus.splist$AOU_bbs2024.combo == 34880] <- "American Crow"
ca.conus.splist$scientific_name_clements2024.combo[ca.conus.splist$AOU_bbs2024.combo == 34880] <- "Corvus brachyrhynchos"
ca.conus.splist$scientific_name_clements2024.combo[ca.conus.splist$AOU_bbs2024.combo == 31690] <- "Anser caerulescens"
ca.conus.splist$common_name_clements2024.combo[ca.conus.splist$AOU_bbs2024.combo == 31690] <- "Snow Goose"

# Summary info on number of full species, subspecies, unid.
# Strings to exclude
unid_exclude <- "unid."
sp.exclude <- "sp."

# Filter out rows containing the string and count the remaining rows
ca.conus.splist.knownsp <- ca.conus.splist %>%
  filter(!str_detect(common_name_bbs2024, unid_exclude)) 
sum(grepl(unid_exclude, ca.conus.splist$common_name_bbs2024)) # 68 unique "unid."
dim(ca.conus.splist.knownsp) # 697 without "unid."
dim(ca.conus.splist)-dim(ca.conus.splist.knownsp) # 88 rows of "unid."

ca.conus.splist.knownsp1 <- ca.conus.splist.knownsp %>%
  filter(!str_detect(common_name_clements2024, sp.exclude)) 
sum(grepl(sp.exclude, ca.conus.splist$common_name_clements2024)) # 24 unique "unid."
dim(ca.conus.splist.knownsp1) # 683 without "unid." and "sp."

# Any remaining species are either full species or hybrid.

# The number of unique species is: 
length(unique(ca.conus.splist$scientific_name_clements2024)) #777
length(unique(ca.conus.splist$scientific_name_clements2024.combo)) #760
length(unique(ca.conus.splist$common_name_clements2024)) # 770
length(unique(ca.conus.splist$common_name_clements2024.combo)) # 760
# Some species lack a common name and American Crow is twice because of the 
# merging of Northwestern and American Crows:
ca.conus.splist$common_name_clements2024[duplicated(ca.conus.splist$common_name_clements2024)]
ca.conus.splist$scientific_name_clements2024[duplicated(ca.conus.splist$scientific_name_clements2024)]
# These are the species combos assigned above:
ca.conus.splist$common_name_clements2024.combo[duplicated(ca.conus.splist$common_name_clements2024.combo)]
ca.conus.splist$scientific_name_clements2024.combo[duplicated(ca.conus.splist$scientific_name_clements2024.combo)]

# --- Export the final 785 rows (777 species including subspecies) for the CANADA & CONUS subset as a CSV (this includes unid. and sp.)
write_csv(ca.conus.splist, file.path(L1_dir,"canada.conus.splist_L1.csv"))

#--- Export the final 785 rows for the CANADA & CONUS subset as a CSV without extra columns
splist_CanadaAKCONUS_L1<-subset(ca.conus.splist, select=c("scientific_name_clements2024.combo",
                                                          "common_name_clements2024.combo",
                                                          "AOU_bbs2024.combo",
                                                          "scientific_name_clements2024",
                                                          "common_name_clements2024",
                                                          "scientific_name_bbs2024",
                                                          "AOU_bbs2024",
                                                          "in_bbs2024"))
write_csv(splist_CanadaAKCONUS_L1, file.path(L1_dir,"splist_CanadaAKCONUS_L1.csv"))
