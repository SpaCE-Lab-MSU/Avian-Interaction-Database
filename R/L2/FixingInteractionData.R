# Title:   Cleaning Interaction DF
#          to align with Cornell tree
# Authors: Lucas Mansfield
# Date:    11 December 2025 -

# -----------------------------------------------
# Loading packages and data
# -----------------------------------------------
rm(list=ls())
library(tidyverse)
library(ape)
library(stringr)
library(clootl)

inter <- read.csv("R/L1/AvianInteractionData_CanadaAKCONUS_L1.csv") #Original data

###Testing for misaligned taxa
d <- extractTree(label_type = "scientific", taxonomy_year = 2024) #loading tree

#formatting species names to match
intertest <- inter %>%
  group_by(species1_scientific) %>%
  mutate(species1_scientific = str_replace_all(species1_scientific, " ", "_")) %>%
  mutate(species2_scientific = str_replace_all(species2_scientific, " ", "_"))

vals <- setdiff(union(intertest$species1_scientific, intertest$species2_scientific), d$tip.label)
vals_clean <- gsub("_", " ", vals)

cat(vals_clean, sep = "\n")

#There are 519 mismatches, most of which are "sp." or subspecies (more than 2 words)

intertest_nosp <- intertest %>%  #Remove rows with "sp." or "unid." in either column
  filter(
    !str_detect(species1_scientific, regex("sp\\.|unid\\.", ignore_case = TRUE)),
    !str_detect(species2_scientific, regex("sp\\.|unid\\.", ignore_case = TRUE))
  )

vals2 <- setdiff(union(intertest_nosp$species1_scientific, intertest_nosp$species2_scientific), d$tip.label)
vals2_clean <- gsub("_", " ", vals2)

cat(vals2_clean, sep = "\n")

#By removing unidentified species, we get down to only 175 mismatches, next let's
#remove hybrids which have "/" and "x" in their names

intertest_nohyb <- intertest %>% #Remove: sp., unid., slash hybrids, x hybrids
  filter(
    !str_detect(species1_scientific, regex("sp\\.|unid\\.|/|_x_", ignore_case = TRUE)),
    !str_detect(species2_scientific, regex("sp\\.|unid\\.|/|_x_", ignore_case = TRUE))
  )

vals3 <- setdiff(union(intertest_nohyb$species1_scientific, intertest_nohyb$species2_scientific), d$tip.label)
vals3_clean <- gsub("_", " ", vals3)

cat(vals3_clean, sep = "\n")

#Without hybirds, we removed another 10 of the mismatches, leaving us with 165
#Let's get rid of the subspecies by making each only the first two words of their
#species names

intertest_nosub <- intertest %>%
  filter(
    !str_detect(species1_scientific, regex("sp\\.|unid\\.|/|_x_", ignore_case = TRUE)),
    !str_detect(species2_scientific, regex("sp\\.|unid\\.|/|_x_", ignore_case = TRUE))
  ) %>%
  mutate(
    species1_scientific = str_replace(species1_scientific,
                                      "^([A-Za-z]+_+[A-Za-z]+).*$", "\\1"),
    species2_scientific = str_replace(species2_scientific,
                                      "^([A-Za-z]+_+[A-Za-z]+).*$", "\\1")
  )

vals4 <- setdiff(union(intertest_nosub$species1_scientific, intertest_nosub$species2_scientific), d$tip.label)
vals4_clean <- gsub("_", " ", vals4)

cat(vals4_clean, sep = "\n")

#After cleaning the subspecies, we are left with our only true errors, of which there are only 21!

#Manually replacing the remaining errors:

list <- read.csv("R/L2/splist_CanadaAKCONUS_L1.csv")
list <- gsub(" ", "_", list$scientific_name_clements2024.combo)
setdiff(list, d$tip.label)

corrections <- c(
  "Acanthis_hornemanni" = "Acanthis_flammea",
  "Accipiter_gentilis" = "Astur_atricapillus",
  "Bubulcus_ibis" = "Ardea_ibis",
  "Chrysococcyx_lucidus" = "Chalcites_lucidus",
  "Roseate_spoonbill" = "Platalea_ajaja",
  "Empidonax_occidentalis" = "Empidonax_difficilis",
  "Covus caurinus" = "Corvus brachyrhynchos",
  "Accipiter atricapillus" = "Astur atricapillus",
  "Accipiter melanoleucus" = "Astur melanoleucus",
  "Chen rossii" = "Anser rossii",
  "Colaptes cafer" = "Colaptes auratus",
  "Dendroica nigrescens" = "Setophaga nigrescens",
  "Psilorhinus morio" = "Cyanocorax morio",
  "Buteo polyosoma" = "Geranoaetus polyosoma",
  "Pterocnemia pennata" = "Rhea pennata",
  "Gallus domesticus" = "Gallus gallus",
  "Accipiter getilis" = "Astur atricapillus",
  "Bulbucus ibis" = "Ardea ibis",
  "Spizella arborea" = "Spizelloides arborea",
  "Dendroica petechia" = "Setophaga petechia",
  "Columbina livia" = "Columba livia"
)


