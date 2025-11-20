# TITLE:          L1 BBS Species List Data: Reads in bbs_specieslist_2024_L0.csv 
#                 (L0 BBS SpeciesList from 2024 release (up to 2023 BBS data), 
#                 so it can be used in the North American Avian Interaction data 
#                 paper and the avian-meta-network (align with combination 
#                 AOUs for subspecies before using in avian-meta-network) 
# AUTHORS:        Phoebe Zarnetske
# COLLABORATORS:  Vincent Miele, Stephane Dray, Emily Parker
# DATA INPUT:     L0 data: bbs_specieslist_2024_L0.csv from bbs_specieslist_L0.R
#                 SpeciesList2.csv from Jeff Hostetler @ BBS contains combo AOUs 
#                 for subspecies.
# DATA OUTPUT:    L1 data: bbs_specieslist_2024_L1.csv 
# PROJECT:        Avian Interaction Database & avian-meta-network
# DATE:           17 January 2022 - 30 October 2024
# NOTES:          Adds new column AOU.combo for use with BBS abundance index. 
#                 
#                 Next script to run: AvianInteractionData_L1.R

# Clear all existing data
rm(list=ls())

#Load packages
library(tidyverse)

# Above .Renviron not working for PLZ; hard-coding in here
L0_dir <- "/Users/plz/Documents/GitHub/Avian-Interaction-Database/L0"
L1_dir <- "/Users/plz/Documents/GitHub/Avian-Interaction-Database/L1"

# Read in species list: all species in BBS (the 2024 release which includes all
# BBS observed species as of 2023)
bbs.splist24<-read.csv(file.path(L0_dir,"bbs_splist_2024_L0.csv"), fileEncoding="UTF-8")
# Uncomment if want to work with our list from the data entry lookup GSheet. 
# More code is needed to develop this:
# all.splist<-read.csv(file.path(L0_dir,"AvianInteractionData_SpeciesList_1Nov2024.csv"))

# Official 2024 eBird & Clements checklist
checklist<-read.csv(file.path(L0_dir,"eBird-Clements-v2024-integrated-checklist-October-2024-rev.csv"))

# Rename the columns for merging later
checklist$genus_species<-checklist$scientific.name
checklist$English_Common_Name<-checklist$English.name
checklist<-subset(checklist, select=c("genus_species","English_Common_Name"))

#### Make some updates to the Species List. Read in modified Species List
#version with 12 extra entries for combined species (from Jeff Hostetler @ BBS:
#August 7, 2024; this version contains some different genus species as well,
#making 12 additions for combined species and 16 that are actually old outdated
#names as far as I can tell. We will first add the 12 combined species to our
#2024 BBS Species List (splist)).
bbs.splist23<-read.csv(file.path(L0_dir,"SpeciesList2.csv"), fileEncoding="ISO-8859-1")
# remove Seq before merging
bbs.splist23$Seq<-NULL
bbs.splist24$Seq<-NULL
bbs.splist23$Order<-bbs.splist23$ORDER
bbs.splist23$ORDER<-NULL
bbs.splist23$Spanish_Common_Name<-NULL
bbs.splist23$genus_species <- do.call(paste, c(bbs.splist23[c("Genus", "Species")], sep = " "))

# Look at the changes since the last iteration of the analysis.
# Compare the list from Jeff Hostetler (which we're calling 2023 because it was 
# provided before the official 2024 release), to official 2024 release species list:

# Full join on "AOU" to keep all rows and identify differences
# Full join on "AOU" to ensure all rows are kept and find differing columns
differences <- bbs.splist23 %>%
  full_join(bbs.splist24, by = "AOU", suffix = c(".2023", ".2024")) %>%
  rowwise() %>%
  mutate(
    differing_values = list(
      tibble(
        English_Common_Name = if_else(
          English_Common_Name.2023 != English_Common_Name.2024,
          paste(English_Common_Name.2023, "->", English_Common_Name.2024),
          NA_character_
        ),
        Family = if_else(
          Family.2023 != Family.2024,
          paste(Family.2023, "->", Family.2024),
          NA_character_
        ),
        Genus = if_else(
          Genus.2023 != Genus.2024,
          paste(Genus.2023, "->", Genus.2024),
          NA_character_
        ),
        Species = if_else(
          Species.2023 != Species.2024,
          paste(Species.2023, "->", Species.2024),
          NA_character_
        )
      )
    )
  ) %>%
  filter(!all(is.na(unlist(differing_values)))) %>%
  select(AOU, differing_values)

# Unnest the differing values for readability
non_blank_differences <- differences %>%
  unnest_wider(differing_values, names_sep = "_diff")

# Identify unique rows in 2023 data that do not exist in 2024
unique_2023_only <- bbs.splist23 %>%
  anti_join(bbs.splist24, by = "AOU")

# Identify unique rows in 2024 data that do not exist in 2023
unique_2024_only <- bbs.splist24 %>%
  anti_join(bbs.splist23, by = "AOU")

# Combine the unique rows from both years with non-blank differences
combined_differences <- bind_rows(
  non_blank_differences,
  unique_2023_only %>% mutate(note = "Only in 2023"),
  unique_2024_only %>% mutate(note = "Only in 2024")
)

# Print only the first 6 columns of the combined differences
print(combined_differences %>% select(1:6), n = Inf)

# Unique rows in 2024 data that do not exist in 2023 data: new species in BBS. 
unique_2024_only
# These are 4 new species observed in the BBS in 2024 release
### ***** RESULT: OK TO ACCEPT AND KEEP THE 2024 VERSION

# AOU   English_Common_Name    French_Common_Name           Order       Family     Genus  Species      genus_species
# 1  390            Ivory Gull       Mouette blanche Charadriiformes      Laridae Pagophila  eburnea  Pagophila eburnea
# 2 7421   Swinhoe’s White-eye  Zostérops de Swinhoe   Passeriformes Zosteropidae Zosterops  simplex  Zosterops simplex
# 3 7614          Dusky Thrush Grive à ailes rousses   Passeriformes     Turdidae    Turdus  eunomus     Turdus eunomus
# 4 5009 Chihuahuan Meadowlark   Sturnelle de Lilian   Passeriformes    Icteridae Sturnella lilianae Sturnella lilianae

# Unique rows in 2023 data that do not exist in 2024 data
unique_2023_only
### ***** RESULT: KEEP AND ADD TO THE 2024 VERSION

# The 3 species at the top may have had name changes? Check. Notes added below.
# The other 12 species are species combined from subspecies.
# AOU                     English_Common_Name          Genus                   Species
# 1   1240                     Red-faced Cormorant          Urile                     urile # Keep; newly observed in BBS
# 2   4890       (Northwestern Crow) American Crow         Corvus                  caurinus # Should be lumped w 34880 Corvus brachyrhynchos
# 3   4882 unid. American Crow / Northwestern Crow         Corvus brachyrhynchos / caurinus # Should be lumped w 34880 Corvus brachyrhynchos
# 4  31320                     Mallard (all forms)           Anas             platyrhynchos
# 5  31940            Great Blue Heron (all forms)          Ardea                  herodias
# 6  33370             Red-tailed Hawk (all forms)          Buteo               jamaicensis
# 7  34120            Northern Flicker (all forms)       Colaptes           auratus auratus
# 8  35670             Dark-eyed Junco (all forms)          Junco                  hyemalis
# 9  36550       Yellow-rumped Warbler (all forms)      Setophaga         coronata coronata
# 10 30010                 Western & Clark's Grebe   Aechmophorus    occidentalis / clarkii
# 11 34641  Cordilleran & Pacific-slope Flycatcher      Empidonax difficilis / occidentalis
# 12 34660               Alder & Willow Flycatcher      Empidonax        alnorum / traillii
# 13 34810      California & Woodhouse's Scrub-Jay     Aphelocoma californica / woodhouseii
# 14 35740              Sagebrush & Bell's Sparrow Artemisiospiza        nevadensis / belli
# 15 34880                           American Crow         Corvus            brachyrhynchos

# Differences: 
# Convert all character columns to UTF-8 - this doesn't work
combined_differences[] <- lapply(combined_differences, function(x) {
  if (is.character(x)) {
    iconv(x, from = "UTF-8", to = "UTF-8")
  } else {
    x
  }
})

print(combined_differences %>% select(1:3), n = Inf)
# Name Changes (diffs between 2023 and 2024 release)
### ******* RESULT: OK TO ACCEPT AND KEEP THE 2024 VERSION

# AOU23 AOU24 differing_values_diffEnglish_Common_Name
# 1326	1326	hybrid Mallard x Mexican, Black, or Mottled Duck -> hybrid Mallard x Black / Mexican / Mottled Duck
# 550	  550	  Mew Gull -> Short-billed Gull
# 3340	3340	Northern Goshawk -> American Goshawk
# 4123	4123	(unid. Red/Yellow Shafted) Northern Flicker -> (unid. Red / Yellow Shafted) Northern Flicker
# 4641	4641	Pacific-slope Flycatcher -> (Pacific-slope Flycatcher) Western Flycatcher
# 4640	4640	Cordilleran Flycatcher -> (Cordilleran Flycatcher) Western Flycatcher
# 4642	4642	unid. Cordilleran / Pacific-slope Flycatcher -> (unid. Cordilleran / Pac-slope) Western Flycatcher
# 5212	5212	Unid. Cassia Crossbill / Red Crossbill -> unid. Cassia Crossbill / Red Crossbill
# 5012	5012	unid. Eastern Meadowlark / Western Meadowlark -> unid. Meadowlark
# 6556	6556	(unid. Myrtle/Audubon's) Yellow-rumped Warbler -> (unid. Myrtle / Audubon's) Yellow-rumped Warbler

print(combined_differences %>% select(1,4,5), n = Inf)
# Name Changes (diffs between 2023 and 2024 release)
### ******* RESULT: OK TO ACCEPT AND KEEP THE 2024 VERSION

# AOU differing_values_diffGenus   differing_values_diffSpecies                                                                 
# <int> <chr>                        <chr>                                                                                        
# 1  1326 NA                           platyrhynchos x diazi x rubripes or fulvigula -> platyrhynchos x rubripes / diazi / fulvigula
# 2  2980 Falcipennis -> Canachites    NA        
# 3  4391 Leucolia -> Ramosomyia       NA
# 4   550 NA                           canus -> brachyrhynchus                                                                     
# 5  1220 Phalacrocorax -> Urile       NA                                                                                          
# 6  1230 Phalacrocorax -> Urile       NA                                                                                          
# 7  1207 Phalacrocorax -> Urile       NA                                                                                          
# 8  1206 Phalacrocorax -> Nannopterum NA                                                                                          
# 9  3340 NA                           gentilis -> atricapillus                                                                    
# 10  4123 NA                           NA                                                                                         
# 11  3620 NA                           cheriway -> plancus                                                                        
# 12  4641 NA                           difficilis -> difficilis difficilis                                                        
# 13  4640 NA                           occidentalis -> difficilis occidentalis                                                    
# 14  4642 NA                           difficilis / occidentalis -> difficilis                                                    
# 15  7240 NA                           platensis -> stellaris                                                                     
# 16  7490 Regulus -> Corthylio         NA                                                                                         
# 17  5212 NA                           NA                                                                                         
# 18  5012 NA                           magna / neglecta -> sp.                                                                    

# Excel does not render the encodings correctly so don't preview it in Excel.
write.csv(combined_differences,file.path(L1_dir,"BBS_specieslist_diffs2023-2024.csv"), fileEncoding="UTF-8", row.names=F)

# row-bind the 2024 list with the unique values in 2023 to ensure we have a comprehensive list
splist<-rbind(bbs.splist24,unique_2023_only)

# Now create a new column for AOUs to be used in the BBS index analysis. This
# will contain re-assignments to the 12 combined AOUs, for their subspecies:
splist$AOU.combo<-splist$AOU

# Grouping Rules provided by Jeff Hostetler @ BBS. 
# Update the splist AOU.combo for each subspecies species (assign it to the
# combo AOU). Make a selection for each subspecies group:

# 4  31320                     Mallard (all forms)
# 01320 01331
splist$AOU.combo[splist$AOU == 1320] <- 31320
splist$AOU.combo[splist$AOU == 1331] <- 31320

# 5  31940            Great Blue Heron (all forms)
# 2 31940    
# 01920 01940
splist$AOU.combo[splist$AOU == 1920] <- 31940
splist$AOU.combo[splist$AOU == 1940] <- 31940

# 6  33370             Red-tailed Hawk (all forms)
# 03370 03380
splist$AOU.combo[splist$AOU == 3370] <- 33370
splist$AOU.combo[splist$AOU == 3380] <- 33370

# 7  34120            Northern Flicker (all forms)
# 04120 04130 04123 04125
splist$AOU.combo[splist$AOU == 4120] <- 34120
splist$AOU.combo[splist$AOU == 4130] <- 34120
splist$AOU.combo[splist$AOU == 4123] <- 34120
splist$AOU.combo[splist$AOU == 4125] <- 34120

# 8  35670             Dark-eyed Junco (all forms) 
# 05660 05670 05671 05680 05690 05677
splist$AOU.combo[splist$AOU == 5660] <- 35670
splist$AOU.combo[splist$AOU == 5670] <- 35670
splist$AOU.combo[splist$AOU == 5671] <- 35670
splist$AOU.combo[splist$AOU == 5680] <- 35670
splist$AOU.combo[splist$AOU == 5690] <- 35670
splist$AOU.combo[splist$AOU == 5677] <- 35670

# 9  36550       Yellow-rumped Warbler (all forms)
# 06550 06560 06556
splist$AOU.combo[splist$AOU == 6550] <- 36550
splist$AOU.combo[splist$AOU == 6560] <- 36550
splist$AOU.combo[splist$AOU == 6556] <- 36550

# 10 30010                 Western & Clark's Grebe
# 00010 00011 00012
splist$AOU.combo[splist$AOU == 10] <- 30010
splist$AOU.combo[splist$AOU == 11] <- 30010
splist$AOU.combo[splist$AOU == 12] <- 30010

# 11 34641  Cordilleran & Pacific-slope Flycatcher
# 04640 04641 04642
splist$AOU.combo[splist$AOU == 4640] <- 34641
splist$AOU.combo[splist$AOU == 4641] <- 34641
splist$AOU.combo[splist$AOU == 4642] <- 34641

# 12 34660               Alder & Willow Flycatcher
# 04660 04661 04665
splist$AOU.combo[splist$AOU == 4660] <- 34660
splist$AOU.combo[splist$AOU == 4661] <- 34660
splist$AOU.combo[splist$AOU == 4665] <- 34660

# 13 34810      California & Woodhouse's Scrub-Jay
# 04812 04813 04810
splist$AOU.combo[splist$AOU == 4812] <- 34810
splist$AOU.combo[splist$AOU == 4813] <- 34810
splist$AOU.combo[splist$AOU == 4810] <- 34810

# 14 35740              Sagebrush & Bell's Sparrow
# 05738 05739 05740
splist$AOU.combo[splist$AOU == 5738] <- 35740
splist$AOU.combo[splist$AOU == 5739] <- 35740
splist$AOU.combo[splist$AOU == 5740] <- 35740

# 15 34880                           American Crow
# 04880 04880 04882 04890
splist$AOU.combo[splist$AOU == 4880] <- 34880
splist$AOU.combo[splist$AOU == 4880] <- 34880
splist$AOU.combo[splist$AOU == 4882] <- 34880
splist$AOU.combo[splist$AOU == 4890] <- 34880

# Snow Goose (Blue and regular form)
splist$AOU.combo[splist$AOU == 1690] <- 31690
splist$AOU.combo[splist$AOU == 1691] <- 31690

## Create a new column which contains Genus species for the combined species above.
splist$genus_species.combo<-splist$genus_species

# Probably a more beautiful way to code this but this works:
# Assign all to new species combo name
splist$genus_species.combo[splist$AOU.combo == 31320] <- "Anas platyrhynchos"
splist$genus_species.combo[splist$AOU.combo == 31940] <- "Ardea herodias"
splist$genus_species.combo[splist$AOU.combo == 33370] <- "Buteo jamaicensis"
splist$genus_species.combo[splist$AOU.combo == 34120] <- "Colaptes auratus auratus"
splist$genus_species.combo[splist$AOU.combo == 35670] <- "Junco hyemalis"
splist$genus_species.combo[splist$AOU.combo == 36550] <- "Setophaga coronata coronata"
splist$genus_species.combo[splist$AOU.combo == 30010] <- "Aechmophorus occidentalis / clarkii"
splist$genus_species.combo[splist$AOU.combo == 34641] <- "Empidonax difficilis / occidentalis"
splist$genus_species.combo[splist$AOU.combo == 34660] <- "Empidonax alnorum / traillii"
splist$genus_species.combo[splist$AOU.combo == 34810] <- "Aphelocoma californica / woodhouseii"
splist$genus_species.combo[splist$AOU.combo == 35740] <- "Artemisiospiza nevadensis / belli"
splist$genus_species.combo[splist$AOU.combo == 34880] <- "Corvus brachyrhynchos"
splist$genus_species.combo[splist$AOU.combo == 31690] <- "Anser caerulescens"

# # Try taxize to catch any other name changes:
# # The section below, using taxize, was last run on Aug 9, 2024.
# tax <-gnr_datasources()
# # GBIF taxonomy ID = 11
# #tax[tax$title=="GBIF Backbone Taxonomy","id"]
# tax[tax$title=="BirdLife International","id"]
# 
# # Detecting name misspellings in our BBS list:
# gbif.bl.tax.splist <- splist$genus_species %>%
#   gnr_resolve(data_source_ids = c(175), 
#               with_canonical_ranks=T,http = "get")
# gbif.bl.tax.splist<-subset(gbif.bl.tax.splist, gbif.bl.tax.splist$score<0.9,)

### Nov 7, 2024: Not working bc taxize is having issues connecting to https://resolver.globalnames.org??

# Note that after exporting this, there are additional changes to
# species with name changes in AvianInteractionData_L1.R: starting at Line 183.
# This then updates (and over-writes) bbs_splist_2022_L1.csv. They are summarized below:

# RENAME: BBS: Streptopelia chinensis to Spilopelia chinensis
# RENAME: BBS: change Corvus caurinus and Corvus brachyrhynchos caurinus to Corvus brachyrhynchos
# RENAME: BBS: change Porphyrio martinicus to Porphyrio martinica 
# RENAME: BBS: change Cyanecula svecica to Luscinia svecica
# RENAME: BBS: change Charadrius nivosus to Anarhynchus nivosus
# RENAME: BBS: change Charadrius wilsonia to Anarhynchus wilsonia
# RENAME: BBS: change Charadrius montanus to Anarhynchus montanus
# RENAME: BBS: change Empidonax occidentalis to Empidonax difficilis

# Move the useful columns to the left
splist <- splist %>% relocate(genus_species, .after = AOU)
splist <- splist %>% relocate(AOU.combo, .after = genus_species)
splist <- splist %>% relocate(genus_species.combo, .after = AOU.combo)

# Export the cleaned data. 
write.csv(splist,file.path(L1_dir,"bbs_splist_2024_L1.csv"), row.names=F)

# Next script to run if combining with bbs_obs data: AvianInteractionData_L1.R.
# In that script, the Genus species associated with the AOU.combo will be
# assigned in 2 new columns (one for species1 and another for species2).

