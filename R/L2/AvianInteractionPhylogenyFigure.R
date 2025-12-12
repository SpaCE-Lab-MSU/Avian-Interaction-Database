# Title:   Phylogeny figures for North
#          American avian interactions
# Authors: Lucas Mansfield
# Date:    9 December 2025 -
#
# WORKFLOW:
#
# Goal: Phylogeny for full dataset and for only NA interactions
# Interactions for each species should incorporate unid.interactions
# but these taxa need to be removed before fitting the tree.
#
#
# 1. Load full dataset: AvianInteractionData_L1.csv
# 2. Load AKCONUS dataset: AvianInteractionData_CanadaAKCONUS_L1.csv
# 3. Filter AKCONUS to only include data with both species in NA
# 4. Filter to remove "sp." and " x "
# 5. Collapse subspecies into larger species in each dataframe
# 6. Count interactions and types per species
# 7. Pull and prune tree, plot results!



# -----------------------------------------------
# Loading packages and data
# -----------------------------------------------
rm(list=ls())
library(tidyverse)
library(ape)
library(stringr)
library(ggtree)
library(ggplot2)
library(ggnewscale)
library(taxize)
library(clootl)

splist <- read.csv("C:/R MSU/Avian-Interaction-Database-Working/L1/species_checklists/splist_CanadaAKCONUS_L1.csv") #NA species list
inter <- read.csv("C:/R MSU/Avian-Interaction-Database-Working/L1/AvianInteractionData_L1.csv") #Full data
inter_NA <- read.csv("C:/R MSU/Avian-Interaction-Database-Working/L1/AvianInteractionData_CanadaAKCONUS_L1.csv") #data with one or either species in NA
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
# Phylogenetic Tree Loading
# -----------------------------------------------

#first, we need to remove the underscores to search with clootl
# (but we keep these in the working files to match with the trees later)
inter_list <- str_replace_all(inter_working$species, "_", " ")
inter_NA_list <- str_replace_all(inter_NA_working$species, "_", " ")
inter_NA_only_list <- str_replace_all(inter_NA_only_working$species, "_", " ")

full_tree <- extractTree(species = inter_list, label_type = "scientific", taxonomy_year = 2024)
NA_tree <- extractTree(species = inter_NA_list, label_type = "scientific", taxonomy_year = 2024)
NA_only_tree <- extractTree(species = inter_NA_only_list, label_type = "scientific", taxonomy_year = 2024)

# -----------------------------------------------
# Phylogenetic Visualization - NA
# -----------------------------------------------

#This section visualizes the data on the phylogenetic tree. There are two ways
#of coloring the species: tips and branches. This makes 4 plots: 2 each of tips
#and branches, for n_int and n_type

#TIPS COLORED
#color by n interactions
ggtree(NA_tree, layout = "circular") %<+% inter_NA_working +
  geom_tippoint(mapping = aes(color = n_int), size = 1) +
  ggtitle("Phylogenetic tree of # of interactions") +
  scale_color_viridis_c(trans = "log10")
#color by n unique types
ggtree(NA_tree, layout = "circular") %<+% inter_NA_working +
  geom_tippoint(mapping = aes(color = n_type), size = 1) +
  scale_color_viridis_c(trans = "log10")



#Branches COLORED
#color by n interactions
ggtree(NA_tree, layout = "circular") %<+% inter_NA_working +
  geom_tree(mapping = aes(color = n_int)) +
  ggtitle("Phylogenetic tree of # of interactions - North America") +
  scale_color_gradientn(
    trans = "log10",
    colors = c("gold", "orange", "red", "darkred"))
#color by n unique types
ggtree(NA_tree, layout = "circular") %<+% inter_NA_working +
  geom_tree(mapping = aes(color = n_type)) +
  ggtitle("Phylogenetic tree of # of types of interactions  - North America") +
  scale_color_gradientn(
    trans = "log10",
    colors = c("gold", "orange", "red", "darkred"))

#I will import the output of one of these plots into Inkscape, and add some
#aesthetics by labeling families of note and adding
#images/illustrations/silhouttes of some species

#The last step is to label one individual per family to be able to identify
#the family's location on the output tree, for aid in creating the final figure.

# Function to create a phylogenetic tree with family-colored tips and labels
plot_phylo_by_family <- function(tree,
                                 data,
                                 species_col = "species",
                                 family_col = "family",
                                 value_col = "n_int",
                                 layout = "circular",
                                 tip_size = 2,
                                 label_size = 2.3,
                                 label_offset = 0.6,
                                 color_palette = NULL,
                                 title = "Phylogenetic Tree — Families Labeled and Colored") {

  # Prepare the data
  plot_data <- data %>%
    rename(label = .data[[species_col]])

  # Get one representative species per family for labeling
  rep_df <- plot_data %>%
    filter(!is.na(.data[[family_col]])) %>%
    group_by(.data[[family_col]]) %>%
    slice(1) %>%
    ungroup()

  # Create a color palette for families if not provided
  families <- unique(plot_data[[family_col]][!is.na(plot_data[[family_col]])])
  n_families <- length(families)

  if (is.null(color_palette)) {
    # Use a colorful palette with enough distinct colors
    if (n_families <= 12) {
      color_palette <- RColorBrewer::brewer.pal(min(n_families, 12), "Set3")
    } else if (n_families <= 24) {
      color_palette <- c(
        RColorBrewer::brewer.pal(12, "Set3"),
        RColorBrewer::brewer.pal(min(n_families - 12, 12), "Paired")
      )
    } else {
      # For many families, use a continuous palette
      color_palette <- rainbow(n_families, s = 0.7, v = 0.9)
    }
  }

  # Create named color vector for families
  family_colors <- setNames(color_palette[1:n_families], families)

  # Build the plot
  p <- ggtree(tree, layout = layout) %<+% plot_data +
    # Add colored tip points
    geom_tippoint(aes(color = .data[[family_col]]), size = tip_size) +
    # Add family labels at representative tips (colored to match)
    geom_tiplab(
      aes(label = ifelse(label %in% rep_df$label, .data[[family_col]], ""),
          color = .data[[family_col]]),
      size = label_size,
      offset = label_offset,
      fontface = "bold"
    ) +
    # Apply family colors
    scale_color_manual(values = family_colors, na.value = "grey50") +
    ggtitle(title) +
    theme(legend.position = "none")

  return(p)
}


plot_phylo_by_family(
  tree = NA_tree,
  data = inter_NA_working,  # uses original column names
  species_col = "species",
  family_col = "family",
  title = ""
)


# -----------------------------------------------
# Phylogenetic Visualization -- Full dataset
# -----------------------------------------------
#Now we repeat the same plotting process, but without filtering for NA first

#TIPS COLORED
#color by n interactions
ggtree(full_tree, layout = "circular") %<+% inter_working +
  geom_tippoint(mapping = aes(color = n_int), size = 1) +
  ggtitle("Phylogenetic tree of # of interactions - Full dataset") +
  scale_color_viridis_c(trans = "log10")
#color by n unique types
ggtree(full_tree, layout = "circular") %<+% inter_working +
  geom_tippoint(mapping = aes(color = n_type), size = 1) +
  scale_color_viridis_c(trans = "log10")



#Branches COLORED
#color by n interactions
ggtree(full_tree, layout = "circular") %<+% inter_working +
  geom_tree(mapping = aes(color = n_int)) +
  ggtitle("Phylogenetic tree of # of interactions - Full dataset") +
  scale_color_gradientn(
    trans = "log10",
    colors = c("gold", "orange", "red", "darkred"))
#color by n unique types
ggtree(full_tree, layout = "circular") %<+% inter_working +
  geom_tree(mapping = aes(color = n_type)) +
  ggtitle("Phylogenetic tree of # of types of interactions - Full dataset") +
  scale_color_gradientn(
    trans = "log10",
    colors = c("gold", "orange", "red", "darkred"))

#Labels

plot_phylo_by_family(
  tree = full_tree,
  data = inter_working,  # uses original column names
  species_col = "species",
  family_col = "family",
  title=""
)


# -----------------------------------------------
# Phylogenetic Visualization -- NA onlyaset
# -----------------------------------------------
#Now we repeat the same plotting process, but without filtering for NA first

#TIPS COLORED
#color by n interactions
ggtree(NA_only_tree, layout = "circular") %<+% inter_NA_only_working +
  geom_tippoint(mapping = aes(color = n_int), size = 1) +
  ggtitle("Phylogenetic tree of # of interactions - Full dataset") +
  scale_color_viridis_c(trans = "log10")
#color by n unique types
ggtree(NA_only_tree, layout = "circular") %<+% inter_NA_only_working +
  geom_tippoint(mapping = aes(color = n_type), size = 1) +
  scale_color_viridis_c(trans = "log10")



#Branches COLORED
#color by n interactions
ggtree(NA_only_tree, layout = "circular") %<+% inter_NA_only_working +
  geom_tree(mapping = aes(color = n_int)) +
  ggtitle("Phylogenetic tree of # of interactions - Full dataset") +
  scale_color_gradientn(
    trans = "log10",
    colors = c("gold", "orange", "red", "darkred"))
#color by n unique types
ggtree(NA_only_tree, layout = "circular") %<+% inter_NA_only_working +
  geom_tree(mapping = aes(color = n_type)) +
  ggtitle("Phylogenetic tree of # of types of interactions - Full dataset") +
  scale_color_gradientn(
    trans = "log10",
    colors = c("gold", "orange", "red", "darkred"))

#Labels

plot_phylo_by_family(
  tree = NA_only_tree,
  data = inter_NA_only_working,  # uses original column names
  species_col = "species",
  family_col = "family",
  title = ""
)

