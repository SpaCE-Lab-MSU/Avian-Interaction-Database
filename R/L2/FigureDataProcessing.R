# Title:   Shared Data Processing for Avian Interaction Analyses
# Authors: Lucas Mansfield
# Date:    18 December 2025 -
# Purpose: Centralized data loading, cleaning, and utility functions
#          for phylogeny, network, and distribution figure scripts


# -----------------------------------------------
# Loading packages
# -----------------------------------------------
library(tidyverse)
library(stringr)
library(clootl)

splist <- read.csv("C:/R MSU/Avian-Interaction-Database/data/L1/species_checklists/spp_joint_cac_colsubset.csv") #NA species list
inter <- read.csv("C:/R MSU/Avian-Interaction-Database-Working/L1/ain_all.csv") #Full data
inter_NA <- read.csv("C:/R MSU/Avian-Interaction-Database-Working/L1/ain_cac.csv") #data with one or either species in NA
inter_NA_only <- inter_NA %>% #data with BOTH species in NA -- interactions that occur in NA
  filter(species1_scientific %in% splist$scientific_name_clements2024,
         species2_scientific %in% splist$scientific_name_clements2024)

inter <- inter %>% #Remove one extinct species still in the data
  filter( species1_scientific != "Circus dossenus",
          species2_scientific != "Circus dossenus")

#Initial clean to remove hybrid taxa, unidentified taxa, and subspecies
initial_clean <- function(data,
                          species1_col = "species1_scientific",
                          species2_col = "species2_scientific",
                          remove_self = TRUE,
                          exclude_patterns = c("sp\\.", "unid\\.", "_x_") #"sp." "unid." or " x "
) {

  #Build regex pattern from exclude_patterns
  pattern <- paste(exclude_patterns, collapse = "|")

  #Filter out unidentified and hybrid species
  cleaned <- data %>%
    filter(
      !str_detect(.data[[species1_col]], regex(pattern, ignore_case = TRUE)),
      !str_detect(.data[[species2_col]], regex(pattern, ignore_case = TRUE))
    ) %>%
    #Collapse subspecies to their larger taxa
    mutate(
      "{species1_col}" := str_replace(.data[[species1_col]],
                                      "^([A-Za-z]+\\s+[A-Za-z]+).*$", "\\1"),
      "{species2_col}" := str_replace(.data[[species2_col]],
                                      "^([A-Za-z]+\\s+[A-Za-z]+).*$", "\\1")
    )

  #Optionally remove self interactions
  if (remove_self) {
    cleaned <- cleaned %>%
      filter(.data[[species1_col]] != .data[[species2_col]])
  }

  return(cleaned)
}

inter_clean <- initial_clean(inter)
inter_NA_clean <- initial_clean(inter_NA)
inter_NA_only_clean <- initial_clean(inter_NA_only)

# -----------------------------------------------
# Filtering interaction data
# -----------------------------------------------

#Currently, our data is formatted like this:
#sp1   int1    sp2
#sp1   int2    sp3

#We want to combine it into two columns, for easier analysis of total interactions
#per species like so
#sp1  int1
#sp2  int1
#sp1  int2
#sp3  int2

#This duplicates our dataframe, so that every interaction: sp1  int1  sp2
#is expressed in both sp1/sp2 permutations

bidirectional_dedup <- function(data, #biderectionalization and deduplication function!
                                species1_sci = "species1_scientific",
                                species2_sci = "species2_scientific",
                                species1_com = "species1_common",
                                species2_com = "species2_common",
                                interaction_col = "interaction",
                                return_duplicates = FALSE,
                                verbose = TRUE) {

  n_start <- nrow(data)
  if (verbose) message("Starting with ", n_start, " rows")

  #Flip the species columns to create bidirectional representation
  data_flip <- data %>%
    rename(
      temp_sp1_com = .data[[species2_com]],
      temp_sp2_com = .data[[species1_com]],
      temp_sp1_sci = .data[[species2_sci]],
      temp_sp2_sci = .data[[species1_sci]]
    ) %>%
    rename(
      "{species1_com}" := temp_sp1_com,
      "{species2_com}" := temp_sp2_com,
      "{species1_sci}" := temp_sp1_sci,
      "{species2_sci}" := temp_sp2_sci
    )

  #Union removes fully duplicated rows automatically
  data_full <- dplyr::union(data, data_flip)
  n_full <- nrow(data_full)
  n_removed_union <- n_start + nrow(data_flip) - n_full

  if (verbose) {
    message("After union: ", n_full, " rows (",
            n_removed_union, " full duplicates removed)")
  }
  #However, we might still have some rows where the same interaction is being represented but the
  #rows aren't identical (different sources, etc). We will double-check this by comparing any rows that
  #are equivalent for species1, interaction AND species2.

  #Check for remaining duplicates (same species pair + interaction, different metadata)
  data_dup <- data_full %>%
    group_by(.data[[species1_sci]], .data[[species2_sci]], .data[[interaction_col]]) %>%
    filter(n() > 1) %>%
    arrange(.data[[species1_sci]], .data[[species2_sci]], .data[[interaction_col]])

  n_dup <- nrow(data_dup)
  if (verbose && n_dup > 0) {
    message("Found ", n_dup, " rows representing identical interactions ",
            "(same species pair + interaction, different metadata)")
  }

  #Option to return duplicates
  if (return_duplicates) {
    return(data_dup)
  }

  #Remove duplicates, keeping one row per unique species1-species2-interaction combo
  data_dedup <- data_full %>%
    distinct(.data[[species1_sci]], .data[[species2_sci]],
             .data[[interaction_col]], .keep_all = TRUE)

  n_final <- nrow(data_dedup)
  n_removed_dedup <- n_full - n_final

  if (verbose) {
    message("After deduplication: ", n_final, " rows (",
            n_removed_dedup, " partial duplicates removed)")
    message("Total reduction: ", n_start, " → ", n_final)
  }

  return(data_dedup)
}

inter_dedup <- bidirectional_dedup(inter_clean)
inter_NA_dedup <- bidirectional_dedup(inter_NA_clean)
inter_NA_only_dedup <- bidirectional_dedup(inter_NA_only_clean)


#Next, we simplify the dataframe for the visualization. We create a new data frame with one row per species,
#column "n_int" for the total number of unique interactions that that species participates in, and "n_type"
#for the total number of distinct types of interactions per species.
summarize_species_interactions <- function(data,
                                           species_col = "species1_scientific",
                                           interaction_col = "interaction",
                                           replace_spaces = TRUE,
                                           output_col = "species") {

  result <- data %>%
    group_by(.data[[species_col]])

  #Replace spaces with underscores to match tree tips
  if (replace_spaces) {
    result <- result %>%
      mutate("{species_col}" := str_replace_all(.data[[species_col]], " ", "_"))
  }

  #Rename and summarize
  result <- result %>%
    rename("{output_col}" := .data[[species_col]]) %>%
    summarize(
      n_int = n(),
      n_type = n_distinct(.data[[interaction_col]]),
      .groups = "drop"
    )

  return(result)
}

inter_working <- summarize_species_interactions(inter_dedup)
inter_NA_working <- summarize_species_interactions(inter_NA_dedup)
inter_NA_only_working <- summarize_species_interactions(inter_NA_only_dedup)

#For visualization, we will add the family to each species using Clements from clootl


#Function to get family from Clements taxonomy via clootl
get_clements_family <- function(species_vector,
                                taxonomy_year = 2024,
                                return_full_taxonomy = FALSE,
                                verbose = TRUE) {

  #Access the taxonomy data from clootl_data
  taxonomy_name <- paste0("Year", taxonomy_year)

  if (!taxonomy_name %in% names(clootl_data$taxonomy.files)) {
    stop("Taxonomy year ", taxonomy_year, " not available. Use 2021-2024.")
  }

  clements <- clootl_data$taxonomy.files[[taxonomy_name]]

  if (verbose) {
    message("Using Clements taxonomy year: ", taxonomy_year)
    message("Clements columns: ", paste(names(clements), collapse = ", "))
    message("Looking up ", length(species_vector), " species...")
  }

  #Clean species names (remove underscores if present)
  species_clean <- str_replace_all(species_vector, "_", " ")

  #Create a lookup dataframe
  results <- data.frame(
    species = species_vector,
    species_clean = species_clean,
    family = NA_character_,
    order = NA_character_,
    stringsAsFactors = FALSE
  )

  #Match each species to Clements data
  for (i in seq_along(species_clean)) {
    sp <- species_clean[i]

    #Match on SCI_NAME column (confirmed from your screenshot)
    match <- clements %>%
      filter(SCI_NAME == sp)

    #If no exact match, try matching genus + species only
    if (nrow(match) == 0) {
      genus_species <- str_extract(sp, "^[A-Za-z]+\\s+[A-Za-z]+")
      if (!is.na(genus_species)) {
        #Escape special characters for regex
        pattern <- paste0("^", gsub(" ", "\\\\s+", genus_species))
        match <- clements %>%
          filter(str_detect(SCI_NAME, pattern))
      }
    }

    if (nrow(match) > 0) {
      #Extract FAMILY and ORDER from matched row
      if ("FAMILY" %in% names(match)) {
        results$family[i] <- match$FAMILY[1]
      }
      if ("ORDER" %in% names(match)) {
        results$order[i] <- match$ORDER[1]
      }
    }
  }

  if (verbose) {
    n_found <- sum(!is.na(results$family))
    n_missing <- sum(is.na(results$family))
    message("Found families for ", n_found, "/", length(species_vector), " species")
    if (n_missing > 0) {
      message("Missing families for ", n_missing, " species")
      message("First few missing: ", paste(head(species_clean[is.na(results$family)], 3), collapse = ", "))
    }
  }

  #Return appropriate format
  if (return_full_taxonomy) {
    return(results)
  } else {
    return(results$family)
  }
}

#Function to add Clements family to a dataframe
add_clements_family <- function(data,
                                species_col = "species",
                                taxonomy_year = 2024,
                                verbose = TRUE) {

  if (verbose) message("Adding Clements family information...")

  #Get families
  families <- get_clements_family(
    data[[species_col]],
    taxonomy_year = taxonomy_year,
    verbose = verbose
  )

  #Add to dataframe
  data$family <- families

  return(data)
}

inter_working <- add_clements_family(inter_working, species_col = "species")
inter_NA_working <- add_clements_family(inter_NA_working, species_col = "species")
inter_NA_only_working <- add_clements_family(inter_NA_only_working, species_col = "species")



# -----------------------------------------------
# Define categories and color scheme
# -----------------------------------------------
interaction_categories <- data.frame(
  interaction = c(
    # Trophic (Reds/Oranges)
    "predation", "nest predation",
    # Competition (Dark Blues)
    "competition", "competition-foraging", "competition-nest site", "competition-territory",
    # Facilitation (Greens)
    "facilitation", "facilitation-mixed flocking", "facilitation-comigration",
    "facilitation-feeding", "facilitation-foraging", "facilitation-creching",
    "facilitation-distress calls", "communal nesting", "communal roosting",
    # Commensalism (Purples)
    "commensalism", "commensalism-scavenge", "commesalism-scavenge",
    "commensalism-chick adoption", "commensalism-call mimicry",
    # Parasitism (Oranges/Browns)
    "kleptoparasitism", "kleptoparasitism-nest material", "kleptoparasitism-food",
    "brood parasitism", "nest parasitism", "nest takeover",
    # Amenalism (Grays)
    "amensalism",
    # Mobbing (Teals/Cyans)
    "mobbing",
    # N/A (Light Blues/Neutrals)
    "co-occur", "hybridization", "copulation", "courtship", "play", "shared scolding", "accidental killing"
  ),
  category = c(
    # Trophic
    rep("Trophic", 2),
    # Competition
    rep("Competition", 4),
    # Facilitation
    rep("Facilitation", 9),
    # Commensalism
    rep("Commensalism", 5),
    # Parasitism
    rep("Parasitism", 6),
    # Amenalism
    "Amenalism",
    # Mobbing
    "Mobbing",
    # N/A
    rep("N/A", 7)
  ),
  color = c(
    # Trophic (Reds/Oranges)
    "#8B0000", "#CD5C5C",
    # Competition (Dark Blues)
    "#00008B", "#1E3A8A", "#2563EB", "#3B82F6",
    # Facilitation (Greens)
    "#006400", "#228B22", "#32CD32", "#90EE90", "#98FB98",
    "#ADFF2F", "#7CFC00", "#66CDAA", "#3CB371",
    # Commensalism (Purples)
    "#4B0082", "#6A5ACD", "#6A5ACD", "#9370DB", "#BA55D3",
    # Parasitism (Oranges/Browns)
    "#D2691E", "#CD853F", "#DAA520", "#B8860B", "#DEB887", "#F4A460",
    # Amenalism (Grays)
    "#696969",
    # Mobbing (Teal)
    "#008B8B",
    # N/A (Light Blues/Neutrals)
    "#B0C4DE", "#87CEEB", "#ADD8E6", "#87CEFA", "#AFEEEE", "#B0E0E6", "#E0F2FE"
  ),
  stringsAsFactors = FALSE
)

# Define category order for plotting
category_order <- c("Trophic", "Competition", "Facilitation",
                    "Commensalism", "Parasitism", "Amenalism",
                    "Mobbing", "N/A")

interaction_categories$category <- factor(interaction_categories$category,
                                          levels = category_order)
