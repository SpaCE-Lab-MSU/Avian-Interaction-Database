library(tabulapdf)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)

## This script contains three examples of extracting tables from PDFs and converting
# them into lists of pairwise interactions. Eventually this will be functionalized
# for ease of use, but will still take a degree of human checking and input.


# Extraction from: Gouveia, Jessica Ann, "Are native plant gardens better for conserving bird populations than gardens with exotic plants?, August 2010" (2010). Monteverde Institute: Tropical Ecology and Conservation. 66.
# https://digitalcommons.usf.edu/tropical_ecology/66

plants_tables <- extract_tables("PlantsPDF.pdf")
plants_df <- plants_tables[[1]]
#Table consits of location as a factor in the "Garden" column, and species in the "Plant" column

#OPTION 1: Location contained in single factor column
plants_pairs <- plants_df %>%
  group_by(Garden) %>%
  summarise(
    pairs = list(as.data.frame(t(combn(unique(Plant), 2)), stringsAsFactors = FALSE)),
    .groups = "drop"
  ) %>%
  unnest(pairs) %>%
  rename(Species1 = V1, Species2 = V2) %>%
  select(Species1, Species2, Garden)

#Produces dataframe of pairwise interactions between all unique plant species occurring in the same garden. Adds garden column for reference.

# ------------------------------------------------------------------------------

#Extraction from: Brumfield, Robb T., and Oswaldo Maillard. "Birds of the central Rio Paracti Valley, a humid montane forest in Departamento Cochabamba, Bolivia." Ornitología Neotropical 18.3 (2007): 1.


birds_tables <- extract_tables("Brumfield.pdf")
birds_df <- bind_rows(birds_tables[[7]], birds_tables[[8]], birds_tables[[9]], birds_tables[[10]], birds_tables[[11]]) #Table is spread across multiple PDF pages, and needs to be merged togethe
birds_df<- birds_df[-which(is.na(birds_df$`English names`)), ] #removing rows with no associated bird
birds_df$Relative <- NULL #Removing relative occurrence column 
head(birds_df)
#Table consists of rows each corresponding to a unique bird species, and columns indicating unique "locations." 
#Each bird was observed in the same geographical location, but across different months and years, so 
#pairwise interactions are generated for those occurring at the same time.

#OPTION 2: Locations as separate columns with count data, species matched by columns with data

obs_cols <- birds_df %>% 
  select(-`English names`, -`Scientific names`) %>% #Defining the columns that contain unique locations, so need to remove the name columns
  names()

birds_pairs_seperate <- map(obs_cols, function(col) { #Runs across each column of locations
  
  present_species <- birds_df %>% #Pulls the english and scientific names of species present in location
    filter(!is.na(.data[[col]]) & .data[[col]] != "NA") %>% 
    select(`English names`, `Scientific names`)
  
  if (nrow(present_species) < 2) return(NULL) #If 0 or 1 species are present, no combinations are generated for that location
  
  combos <- combn(nrow(present_species), 2) #If 2 or more species are present, all combos are generated
  
  tibble(  #Creates tibble for location containing pairwise combo of common and scientific names
    species1_common = present_species$`English names`[combos[1,]],
    species2_common = present_species$`English names`[combos[2,]],
    species1_scientific = present_species$`Scientific names`[combos[1,]],
    species2_scientific = present_species$`Scientific names`[combos[2,]]
  )
})

birds_pairs_all <- bind_rows(birds_pairs_seperate) #Merges tibbles for the locations into a single dataframe

birds_pairs <- birds_pairs_all %>% #Removes duplicate rows
  mutate(

        pair_id = pmap_chr(
      list(species1_common, species2_common), #Creates an ID for each row which consists of it's species pair ordered alphabetically, to ensure B/A is the same as A/B
      ~ paste(sort(c(..1, ..2)), collapse = "||")
    )
  ) %>%
  distinct(pair_id, .keep_all = TRUE) %>% #Removes duplicate pair_id rows
  select(-pair_id)

#Resulting Dataframe contains all pairwise species interactions, with both common and scientific names.

#--------------------------------------------------------------------------


#Option 2 Example

#Extraction from: Gómez, Elizabeth Martin, et al. "Expedition Field Report." (2019).
EC_tables <- extract_tables("Ecuador.pdf")
#We will combine pairwise interactions for two separate tables, one from mist net data and one from CBC surveys
#In both tables, species are each on a row and columns are separate locations
#Start with mist net table

#Extraction pulls first data row as column names, so some manual cleaning needs to be done to rename the columns properly
EC_tables[[2]] <- rename(EC_tables[[2]], Species = `Acadian Flycatcher`)
EC_tables[[2]] <- rename(EC_tables[[2]], Ridge = ...2)
EC_tables[[2]] <- rename(EC_tables[[2]], ForLow = ...3)
EC_tables[[2]] <- rename(EC_tables[[2]], For2 = ...4)
EC_tables[[2]] <- rename(EC_tables[[2]], Rip2 = ...5)
EC_tables[[2]] <- rename(EC_tables[[2]], Yanez = `1...6`)
EC_tables[[2]] <- rename(EC_tables[[2]], Total = `1...7`)

EC_tables[[3]] <- rename(EC_tables[[3]], Species = `Ornate Flycatcher`)
EC_tables[[3]] <- rename(EC_tables[[3]], Ridge = `1...2`)
EC_tables[[3]] <- rename(EC_tables[[3]], ForLow = `1...3`)
EC_tables[[3]] <- rename(EC_tables[[3]], For2 = `3`)
EC_tables[[3]] <- rename(EC_tables[[3]], Rip2 = ...5)
EC_tables[[3]] <- rename(EC_tables[[3]], Yanez = ...6)
EC_tables[[3]] <- rename(EC_tables[[3]], Total = `5`)

#The mist net table is across two pages, so these are bound together
EC_nets_df <- bind_rows(EC_tables[[2]], EC_tables[[3]])
EC_nets_df$Total <- NULL #Total count is unnecessary and removed

#The rows that became column names are manually re-added, as well as a third page species which was not detected.
#Some long-named species (Scaly-throated Foliage-gleaner) are read as two separate rows, so this is corrected as well.
EC_nets_df <- EC_nets_df %>%
  add_row(Species = "Acadian Flycatcher", Ridge = NA, ForLow = NA, For2 = NA, Rip2 = NA, Yanez = 1) %>%
  add_row(Species = "Ornate Flycatcher", Ridge = 1, ForLow = 1, For2 = 3, Rip2 =NA, Yanez = NA) %>%
  add_row(Species = "Yellow-throated Bush-Tanager", Ridge = NA, ForLow = NA, For2 = 3, Rip2 = NA, Yanez = 3) %>%
  filter(Species!= "gleaner") %>%
  mutate(Yanez = ifelse(Species == "Scaly-throated Foliage-", 1, Yanez)) %>%
  mutate(Species = ifelse(Species == "Scaly-throated Foliage-", "Scaly-throated Foliage-gleaner", Species))


#Running the pairwise generation as in the prior example.
obs_cols <- EC_nets_df %>% #location columns are defined
  select(-Species) %>% 
  names()

EC_nets_pairs_seperate <- map(obs_cols, function(col) {
  
  present_species <- EC_nets_df %>% 
    filter(!is.na(.data[[col]]) & .data[[col]] != "NA") %>% #Run through species pairs for each column
    select(Species)
  
  if (nrow(present_species) < 2) return(NULL)
  
  combos <- combn(nrow(present_species), 2)
  
  tibble( #No scientific names are present on this table, so only common names are added
    species1_common = present_species$Species[combos[1,]],
    species2_common = present_species$Species[combos[2,]]
  )
})

EC_nets_pairs_all <- bind_rows(EC_nets_pairs_seperate) #Bind all locations together

EC_nets_pairs <- EC_nets_pairs_all %>% #Remove duplicate pairs
  mutate(
    
    pair_id = pmap_chr(
      list(species1_common, species2_common),
      ~ paste(sort(c(..1, ..2)), collapse = "||")
    )
  ) %>%
  distinct(pair_id, .keep_all = TRUE) %>%
  select(-pair_id)

#CBC table
#Again, first row is pulled as a column, so that needs to be corrected.
EC_tables[[7]] <- rename(EC_tables[[7]], Species = `Andean cock-of-the-rock`)
EC_tables[[7]] <- rename(EC_tables[[7]], T1 = ...2)
EC_tables[[7]] <- rename(EC_tables[[7]], T2 = `4`)
EC_tables[[7]] <- rename(EC_tables[[7]], HF = ...4)
EC_tables[[7]] <- rename(EC_tables[[7]], R = ...5)
EC_tables[[7]] <- rename(EC_tables[[7]], T3 = `2`)
EC_tables[[7]] <- rename(EC_tables[[7]], T4 = `1`)
EC_tables[[7]] <- rename(EC_tables[[7]], Total = `7`)
EC_tables[[7]]$N <- NA

EC_tables[[8]] <- rename(EC_tables[[8]], Species = `Brown Violetear`)
EC_tables[[8]] <- rename(EC_tables[[8]], T1 = `1...2`)
EC_tables[[8]] <- rename(EC_tables[[8]], T2 = `3`)
EC_tables[[8]] <- rename(EC_tables[[8]], HF = ...4)
EC_tables[[8]] <- rename(EC_tables[[8]], N = ...5)
EC_tables[[8]] <- rename(EC_tables[[8]], R = ...6)
EC_tables[[8]] <- rename(EC_tables[[8]], T3 = `1...7`)
EC_tables[[8]] <- rename(EC_tables[[8]], T4 = ...8)
EC_tables[[8]] <- rename(EC_tables[[8]], Total = `5`)

EC_tables[[9]] <- rename(EC_tables[[9]], Species = `Lineated Foliage-gleaner`)
EC_tables[[9]] <- rename(EC_tables[[9]], T1 = `4`)
EC_tables[[9]] <- rename(EC_tables[[9]], T2 = `1...3`)
EC_tables[[9]] <- rename(EC_tables[[9]], HF = ...4)
EC_tables[[9]] <- rename(EC_tables[[9]], N = ...5)
EC_tables[[9]] <- rename(EC_tables[[9]], R = ...6)
EC_tables[[9]] <- rename(EC_tables[[9]], T3 = `2`)
EC_tables[[9]] <- rename(EC_tables[[9]], T4 = `1...8`)
EC_tables[[9]] <- rename(EC_tables[[9]], Total = `8`)

EC_tables[[10]] <- rename(EC_tables[[10]], Species = `Smoke-colored Pewee`)
EC_tables[[10]] <- rename(EC_tables[[10]], T1 = `1`)
EC_tables[[10]] <- rename(EC_tables[[10]], T2 = ...3)
EC_tables[[10]] <- rename(EC_tables[[10]], HF = ...4)
EC_tables[[10]] <- rename(EC_tables[[10]], R = ...5)
EC_tables[[10]] <- rename(EC_tables[[10]], T3 = `3`)
EC_tables[[10]] <- rename(EC_tables[[10]], T4 = `2`)
EC_tables[[10]] <- rename(EC_tables[[10]], Total = `6`)
EC_tables[[10]]$N <- NA

#All 4 pages of the table are combined, and the Total count column is removed.
EC_CBC_df <- bind_rows(EC_tables[[7]], EC_tables[[8]], EC_tables[[9]], EC_tables[[10]])
EC_CBC_df$Total <- NULL

EC_CBC_df <- EC_CBC_df %>% #Misaligned rows are re-added, and long species names are corrected.
  add_row(Species = "Andean cock-of-the-rock", T1 = NA, T2 = 4, HF = NA, R = NA, N = NA, T3 = 2, T4 = 1) %>%
  add_row(Species = "Brown Violetear", T1 = 1, T2 = 3, HF = NA, R = NA, N = NA, T3 = 1, T4 = NA) %>%
  add_row(Species = "Lineated Foliage-gleaner", T1 = 4, T2 = 1, HF = NA, R = NA, N = NA, T3 = 2, T4 = 1) %>%
  add_row(Species = "Smoke-colored Pewee", T1 = 1, T2 = NA, HF = NA, R = NA, N = NA, T3 = 3, T4 = 2) %>%
  filter(Species!= "Tanager") %>%
  mutate(T2 = ifelse(Species == "Blue-winged Mountain-", 2, T2)) %>%
  mutate(T3 = ifelse(Species == "Blue-winged Mountain-", 2, T3)) %>%
  mutate(Species = ifelse(Species == "Blue-winged Mountain-", "Blue-winged Mountain-Tanager", Species)) %>%
  filter(Species!= "gleaner") %>%
  mutate(T1 = ifelse(Species == "scaly-throated Foliage-", 1, T1)) %>%
  mutate(T2 = ifelse(Species == "scaly-throated Foliage-", 1, T2)) %>%
  mutate(T4 = ifelse(Species == "scaly-throated Foliage-", 2, T4)) %>%
  mutate(Species = ifelse(Species == "scaly-throated Foliage-", "Scaly-throated Foliage-gleaner", Species)) %>%
  filter(Species!= "Swallow") %>%
  mutate(T1 = ifelse(Species == "Southern Rough-winged", 3, T1)) %>%
  mutate(T3 = ifelse(Species == "Southern Rough-winged", 24, T3)) %>%
  mutate(Species = ifelse(Species == "Southern Rough-winged", "Southern Rough-winged Swallow", Species)) %>%
  mutate(T1 = ifelse(Species == "Yellow-throated Bush-", 1, T1)) %>%
  mutate(T2 = ifelse(Species == "Yellow-throated Bush-", 9, T2)) %>%
  mutate(T2 = ifelse(Species == "Yellow-throated Bush-", 4, T2)) %>%
  mutate(T4 = ifelse(Species == "Yellow-throated Bush-", 5, T4)) %>%
  mutate(Species = ifelse(Species == "Yellow-throated Bush-", "Yellow-throated Bush-Tanager", Species))

#Running the pairwise creation as above
obs_cols <- EC_CBC_df %>% #Locations are defined
  select(-Species) %>% 
  names()

EC_CBC_pairs_seperate <- map(obs_cols, function(col) {
  
  present_species <- EC_CBC_df %>% #Species pairs are generated
    filter(!is.na(.data[[col]]) & .data[[col]] != "NA") %>% 
    select(Species)
  
  if (nrow(present_species) < 2) return(NULL)
  
  combos <- combn(nrow(present_species), 2)
  
  tibble(
    species1_common = present_species$Species[combos[1,]],
    species2_common = present_species$Species[combos[2,]]
  )
})

EC_CBC_pairs_all <- bind_rows(EC_CBC_pairs_seperate) #All locations are bound together

EC_CBC_pairs <- EC_CBC_pairs_all %>% #Duplicates are removed
  mutate(
    
    pair_id = pmap_chr(
      list(species1_common, species2_common),
      ~ paste(sort(c(..1, ..2)), collapse = "||") 
    )
  ) %>%
  distinct(pair_id, .keep_all = TRUE) %>%
  select(-pair_id)


#Both tables (mist net and CBC surveys) are combined, and duplicate rows between the two tables are removed
EC_pairs <- bind_rows(EC_nets_pairs, EC_CBC_pairs)
EC_pairs <- EC_pairs %>%
  mutate(
    
    pair_id = pmap_chr(
      list(species1_common, species2_common),
      ~ paste(sort(c(..1, ..2)), collapse = "||")
    )
  ) %>%
  distinct(pair_id, .keep_all = TRUE) %>%
  select(-pair_id)

#Resulting Dataframe contains all pairwise species interactions for the two tables, with common names only.
