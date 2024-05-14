#this took ~1 hour :)


# Clear all existing data
rm(list=ls())

# Load packages
library(tidyverse, dplyr)

#set working directory - emily 
dir <- setwd("/Users/emilyparker/Documents/R/AvianMetanetwork")
Gdir <- setwd("/Users/emilyparker/Documents/GitHub/Avian-Interaction-Database/L1")


#read in data - emily
BBS <- read_csv(file.path(Gdir,"bbs_splist_2022_L1.csv"))
ABA <- read.csv(file.path(dir, "ABA_Checklist-Dec2023.csv"))
AOU <- read.csv(file.path(dir,"NACC_AOU_list_species.csv"))
Clements <- read.csv(file.path(dir, "Clements-v2023-October-2023.csv"))


#Clements - merge species codes 
## this is really messy - way to simplify?
Clements_sp <- Clements

#filters characters only (removes digits)
Clements_sp$species_code <- str_extract(Clements_sp$species_code,("[aA-zZ]+"))

Clements_sp  <- Clements_sp %>%
  mutate(range = paste(range, collapse = "; "), .by = species_code)

Clements <- left_join(Clements,Clements_sp, by = "scientific.name")


#remove unneeded columns
ABA[ ,c(1,3,5)] <- list(NULL)

AOU[,c('id',
       'rank',
       'subfamily',
       'annotation',
       'genus',
       'french_name',
       'order',
       'family')] <- list(NULL)

BBS[,c('Seq',
       'AOU',
       'French_Common_Name',
       'Spanish_Common_Name',
       'ORDER',
       'Family',
       'Genus',
       'Species',
       'species1_scientific')] <- list(NULL)

Clements[,c(1,2,3,4,5,6,7,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,28,29,30,32,33)] <- list(NULL)

#rename ABA columns
names(ABA)[1] <- "common_name"
names(ABA)[2] <- "species"
names(ABA)[3] <- "rarity"

#rename BBS column
names(BBS)[1] <- 'common_name'
names(BBS)[2] <- 'species'

#rename Clements column
names(Clements)[3] <- 'extinct'
names(Clements)[2] <- 'range'
names(Clements)[1] <- "species"

#remove blank rows
ABA <- ABA %>%
  filter(!(species =="" ))

AOU <- AOU %>%
  filter(!(species =="" ))

BBS <- BBS %>%
  filter(!(species == ""))

Clements <- Clements %>%
  filter(!(species == ""))

##ABA

#merge AOU info
ABA <- merge(ABA,AOU, all.x = TRUE, all.y = FALSE)


#merge clements
ABA <- merge(ABA,Clements, all.x = TRUE, all.y = FALSE)

#filter ABA by BBS
ABA_filtered <- ABA[!(ABA$species %in% BBS$species),]

#sort ABA_filtered by rarity
ABA_filtered <- ABA_filtered[order(ABA_filtered$rarity),]

#removing name changes 
ABA_filtered <- ABA_filtered %>%
  filter(!(species == 'Porphyrio martinica (martinicus)' | species == 'Anarhynchus (Charadrius) wilsonia' | species == 'Anarhynchus (Charadrius) nivosus' | species == 'Anarhynchus (Charadrius) montanus'))

ABA_filtered$status[ABA_filtered$status_accidental=="A"] <- "accidental"
ABA_filtered$status[ABA_filtered$status_hawaiian=="H"] <- "Hawaii"
ABA_filtered$status[ABA_filtered$status_introduced=="I"] <- "introduced"
ABA_filtered$status[ABA_filtered$status_nonbreeding=="N"] <- "non-breeding"
ABA_filtered$status[ABA_filtered$status_misplaced=="*"] <- "misplaced"
ABA_filtered$status[!(ABA_filtered$status_extinct=="")] <- "extinct"

ABA_filtered$status[(ABA_filtered$status_hawaiian=="H" & ABA_filtered$status_introduced == "I")] <- "Hawaii, introduced"
ABA_filtered$status[(ABA_filtered$status_hawaiian=="H" & ABA_filtered$status_accidental == "A")] <- "Hawaii, accidental"


ABA_filtered[,c(4,5,6,7,8,9)] <- list(NULL)

ABA_filtered$list <- "ABA"

## AOU

#merge Clements
AOU <- merge(AOU,Clements, all.x = TRUE, all.y = FALSE)

#remove AOU species that occur in ABA & BBS
AOU_filtered <- AOU[!((AOU$species %in% ABA$species) | (AOU$species %in% BBS$species)),]

#change occurrence column to more workable
AOU_filtered$status[AOU_filtered$status_accidental=="A"] <- "accidental"
AOU_filtered$status[AOU_filtered$status_hawaiian=="H"] <- "Hawaii"
AOU_filtered$status[AOU_filtered$status_introduced=="I"] <- "introduced"
AOU_filtered$status[AOU_filtered$status_nonbreeding=="N"] <- "non-breeding"
AOU_filtered$status[AOU_filtered$status_misplaced=="*"] <- "misplaced"
AOU_filtered$status[!(AOU_filtered$status_extinct=="")] <- "extinct"

AOU_filtered$status[(AOU_filtered$status_hawaiian=="H" & AOU_filtered$status_introduced == "I")] <- "Hawaii, introduced"
AOU_filtered$status[(AOU_filtered$status_hawaiian=="H" & AOU_filtered$status_accidental == "A")] <- "Hawaii, accidental"


#delete old columns
AOU_filtered[,c(3,4,5,6,7,8)] <- list(NULL)


AOU_filtered$list <- "AOU"
AOU_filtered$rarity <- NA

#merge
ABA_AOU_filtered <- rbind(AOU_filtered,ABA_filtered)

#write out
write.csv(ABA_AOU_filtered, file.path(dir,"ABA_AOU_filtered_lists.csv"), row.names=F)

