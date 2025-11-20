## Alternate code for AvianInteractionData_specieslists_L1.R that works.

#### Utility function: Merge with Clements first ####
merge_with_clements_first <- function(values, clements_value) {
  vals <- unique(na.omit(values))
  if (length(vals) == 0) return(NA_character_)
  if (!is.na(clements_value) && clements_value %in% vals) {
    vals <- c(clements_value, setdiff(vals, clements_value))
  }
  paste(vals, collapse = "; ")
}

#### Utility function: Clean name strings ####
clean_names <- function(x) {
  str_replace_all(x, "\\s+", " ") %>%
    str_replace_all("\\s*/\\s*", "/") %>%
    str_replace_all("\\(\\s*", "(") %>%
    str_replace_all("\\s*\\)", ")") %>%
    str_trim()
}

df<-master6

# =====================================================
# PART 1: Merge duplicates where scientific_name_clements2024 = NA
# =====================================================

# Identify duplicates by scientific_name + common_name where clements2024 is NA
dup_groups <- df %>%
  filter(is.na(scientific_name_clements2024)) %>%
  group_by(scientific_name, common_name) %>%
  filter(n() > 1) %>%
  group_split()

for (group in dup_groups) {
  keep_idx <- which(!is.na(group$scientific_name_clements2024))[1]
  if (is.na(keep_idx)) keep_idx <- 1
  
  drop_idx <- setdiff(seq_len(nrow(group)), keep_idx)
  
  for (col in names(df)) {
    if (grepl("clements", col, ignore.case = TRUE)) next
    for (idx in drop_idx) {
      val <- group[[col]][idx]
      if (!is.na(val) && val != "") {
        if (is.na(group[[col]][keep_idx]) || group[[col]][keep_idx] == "") {
          group[[col]][keep_idx] <- val
        } else if (!grepl(val, group[[col]][keep_idx], fixed = TRUE)) {
          group[[col]][keep_idx] <- paste0(group[[col]][keep_idx], "; ", val)
        }
      }
    }
  }
  
  df <- df %>%
    filter(!(scientific_name %in% group$scientific_name &
               common_name %in% group$common_name &
               row_number() %in% drop_idx))
}
# --- Report remaining NA scientific_name_clements2024 rows ---
remaining_na <- df %>%
  filter(is.na(scientific_name_clements2024)) %>%
  select(scientific_name, common_name) %>%
  distinct() %>%
  arrange(scientific_name)  # sorted by scientific_name

if (nrow(remaining_na) > 0) {
  message("\n📋 Remaining species with NA in scientific_name_clements2024:")
  print(remaining_na)
} else {
  message("\n🎉 No remaining species with NA in scientific_name_clements2024.")
}

# =====================================================
# PART 2: Manual mapping merges (now can merge regardless of NA in clements col)
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
  incorrect_idx <- which(
    df$scientific_name == name_map$incorrect_scientific[i] &
      df$common_name     == name_map$incorrect_common[i]
  )
  
  correct_idx <- which(
    df$scientific_name == name_map$correct_scientific[i] &
      df$common_name     == name_map$correct_common[i]
  )
  
  if (length(incorrect_idx) == 1 && length(correct_idx) == 1) {
    for (col in names(df)) {
      if (grepl("clements", col, ignore.case = TRUE)) next
      wrong_val <- df[[col]][incorrect_idx]
      right_val <- df[[col]][correct_idx]
      
      if (!is.na(wrong_val) && wrong_val != "") {
        if (is.na(right_val) || right_val == "") {
          df[[col]][correct_idx] <- wrong_val
        } else if (!grepl(wrong_val, right_val, fixed = TRUE)) {
          df[[col]][correct_idx] <- paste0(right_val, "; ", wrong_val)
        }
      }
    }
    df <- df[-incorrect_idx, ]
  }
}

# =====================================================
# FINAL: Report species that still have some edits to do 
# =====================================================

# === Final Report & Corrections ===
data<-df
# 1. Species with NA in scientific_name_clements2024
na_clements <- data %>%
  filter(is.na(scientific_name_clements2024)) %>%
  select(scientific_name, common_name) %>%
  distinct() %>%
  arrange(scientific_name)

cat("\n--- Species with NA in scientific_name_clements2024 ---\n")
print(na_clements)

# --- Manual fixes ---
# Remove row with no scientific name
df <- df[!is.na(df$scientific_name), ]
# 2. Detect and print species with semicolons in scientific_name or common_name
semicolon_rows <- data %>%
  filter(str_detect(scientific_name, ";") | str_detect(common_name, ";")) %>%
  select(scientific_name, common_name, scientific_name_clements2024, common_name_clements2024) %>%
  distinct() %>%
  arrange(scientific_name)

cat("\n--- Species with ';' in scientific_name or common_name ---\n")
print(semicolon_rows) # 153 rows

# 3. Detect and print mismatches between *_name and *_name_clements2024
name_mismatches <- data %>%
  filter(scientific_name != scientific_name_clements2024 |
           common_name != common_name_clements2024) %>%
  select(scientific_name, scientific_name_clements2024,
         common_name, common_name_clements2024) %>%
  distinct() %>%
  arrange(scientific_name)

cat("\n--- Rows where names do not match *_clements2024 ---\n")
print(name_mismatches) # 153 rows

