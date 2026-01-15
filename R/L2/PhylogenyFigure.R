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
library(ggrepel)

source("C:/R MSU/Avian-Interaction-Database/R/L2/FigureDataProcessing.R")


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


#Branches COLORED
#color by n interactions
ggtree(NA_tree, layout = "circular") %<+% inter_NA_working +
  geom_tree(mapping = aes(color = n_int)) +
  ggtitle("Phylogenetic tree of # of interactions - North America") +
  scale_color_gradientn(
    trans = "log10",
    colors = c("gold", "orange", "red", "darkred"),
    breaks = c(min(inter_NA_working$n_int, na.rm = TRUE),
               5, 25, 100,
               max(inter_NA_working$n_int, na.rm = TRUE)),
    labels = c(min(inter_NA_working$n_int, na.rm = TRUE),
               5, 25, 100,
               max(inter_NA_working$n_int, na.rm = TRUE)))

#color by n unique types
ggtree(NA_tree, layout = "circular") %<+% inter_NA_working +
  geom_tree(mapping = aes(color = n_type)) +
  ggtitle("Phylogenetic tree of # of types of interactions  - North America") +
  scale_color_gradientn(
    trans = "log10",
    colors = c("gold", "orange", "red", "darkred"),
    breaks = c(min(inter_NA_working$n_type, na.rm = TRUE),
               2, 5, 10,
               max(inter_NA_working$n_type, na.rm = TRUE)),
    labels = c(min(inter_NA_working$n_type, na.rm = TRUE),
               2, 5, 10,
               max(inter_NA_working$n_type, na.rm = TRUE)))

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
      color_palette <- RColorBrewer::brewer.pal(min(n_families, 12), "Greys")
    } else if (n_families <= 24) {
      color_palette <- c(
        RColorBrewer::brewer.pal(12, "Greys"),
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

#Branches COLORED
#color by n interactions
ggtree(full_tree, layout = "circular") %<+% inter_working +
  geom_tree(mapping = aes(color = n_int)) +
  ggtitle("Phylogenetic tree of # of interactions - Full dataset") +
  scale_color_gradientn(
    trans = "log10",
    colors = c("gold", "orange", "red", "darkred"),
    breaks = c(min(inter_working$n_int, na.rm = TRUE),
               5, 25, 100,
               max(inter_working$n_int, na.rm = TRUE)),
    labels = c(min(inter_working$n_int, na.rm = TRUE),
               5, 25, 100,
               max(inter_working$n_int, na.rm = TRUE)))

#color by n unique types
ggtree(full_tree, layout = "circular") %<+% inter_working +
  geom_tree(mapping = aes(color = n_type)) +
  ggtitle("Phylogenetic tree of # of types of interactions - Full dataset") +
  scale_color_gradientn(
    trans = "log10",
    colors = c("gold", "orange", "red", "darkred"),
    breaks = c(min(inter_working$n_type, na.rm = TRUE),
               2, 5, 10,
               max(inter_working$n_type, na.rm = TRUE)),
    labels = c(min(inter_working$n_type, na.rm = TRUE),
               2, 5, 10,
               max(inter_working$n_type, na.rm = TRUE)))

#Labels
plot_phylo_by_family(
  tree = full_tree,
  data = inter_working,  # uses original column names
  species_col = "species",
  family_col = "family",
  title=""
)



# -----------------------------------------------
# Phylogenetic Visualization -- NA only dataset
# -----------------------------------------------
#Now we repeat the same plotting process, but for NA only dataset

#Branches COLORED
#color by n interactions
ggtree(NA_only_tree, layout = "circular") %<+% inter_NA_only_working +
  geom_tree(mapping = aes(color = n_int)) +
  ggtitle("Phylogenetic tree of # of interactions - NA Only dataset") +
  scale_color_gradientn(
    trans = "log10",
    colors = c("gold", "orange", "red", "darkred"),
    breaks = c(min(inter_NA_only_working$n_int, na.rm = TRUE),
               5, 25, 100,
               max(inter_NA_only_working$n_int, na.rm = TRUE)),
    labels = c(min(inter_NA_only_working$n_int, na.rm = TRUE),
               5, 25, 100,
               max(inter_NA_only_working$n_int, na.rm = TRUE)))

#color by n unique types
ggtree(NA_only_tree, layout = "circular") %<+% inter_NA_only_working +
  geom_tree(mapping = aes(color = n_type)) +
  ggtitle("Phylogenetic tree of # of types of interactions - NA only dataset") +
  scale_color_gradientn(
    trans = "log10",
    colors = c("gold", "orange", "red", "darkred"),
    breaks = c(min(inter_NA_only_working$n_type, na.rm = TRUE),
               2, 5, 10,
               max(inter_NA_only_working$n_type, na.rm = TRUE)),
    labels = c(min(inter_NA_only_working$n_type, na.rm = TRUE),
               2, 5, 10,
               max(inter_NA_only_working$n_type, na.rm = TRUE)))

#Labels
plot_phylo_by_family(
  tree = NA_only_tree,
  data = inter_NA_only_working,  # uses original column names
  species_col = "species",
  family_col = "family",
  title = ""
)


#Attempting to plot families and branch colors at the same time


plot_phylo_combined <- function(tree,
                                data,
                                species_col = "species",
                                family_col = "family",
                                value_col = "n_int",
                                layout = "circular",
                                branch_colors = c("gold", "orange", "red", "darkred"),
                                breaks = NULL,
                                label_offset = 8,
                                label_size = 3.5,
                                tip_size = 2,
                                family_palette = NULL,
                                title = "",
                                min_species_for_label = 5,
                                legend_title = "") {

  #name species column as "label" to match tips
  plot_data <- data %>%
    rename(label = .data[[species_col]])

  #Count species per family and filter for families with > min_species_for_label
  family_counts <- plot_data %>%
    filter(!is.na(.data[[family_col]])) %>%
    count(.data[[family_col]], name = "n_species")

  families_to_label <- family_counts %>%
    filter(n_species > min_species_for_label) %>%
    pull(.data[[family_col]])

  #Get one representative species per family for labeling (only for families with enough species)
  rep_df <- plot_data %>%
    filter(!is.na(.data[[family_col]]),
           .data[[family_col]] %in% families_to_label) %>%
    group_by(.data[[family_col]]) %>%
    slice(1) %>%
    ungroup() %>%
    select(label, family = .data[[family_col]])

  #Set breaks if not provided
  if (is.null(breaks)) {
    val_range <- range(plot_data[[value_col]], na.rm = TRUE)
    breaks <- c(val_range[1], 5, 25, 100, val_range[2])
  }

  #Create color palette for families
  families <- unique(plot_data[[family_col]][!is.na(plot_data[[family_col]])])
  n_families <- length(families)

  if (is.null(family_palette)) {
    if (n_families <= 8) {
      family_palette <- brewer.pal(max(n_families, 3), "Dark2")
    } else if (n_families <= 12) {
      family_palette <- brewer.pal(n_families, "Set3")
    } else if (n_families <= 24) {
      #Mix multiple palettes for better color diversity
      family_palette <- c(
        brewer.pal(8, "Dark2"),
        brewer.pal(9, "Set1"),
        brewer.pal(min(n_families - 17, 8), "Set2")
      )
    } else {
      #For many families, use a diverse palette

      family_palette <- colorRampPalette(c("grey20", "grey95"))(n_families)

      #gray(seq(0, 1, length.out = n_families))
      #rainbow(n_families, s = 0.8, v = 0.85)

    }
  }

  family_colors <- setNames(family_palette[1:n_families], families)

  #Create base tree with colored branches by interaction data
  p <- ggtree(tree, layout = layout) %<+% plot_data +
    geom_tree(aes(color = .data[[value_col]])) +
    scale_color_gradientn(
      trans = "log10",
      colors = branch_colors,
      breaks = breaks,
      labels = breaks,
      name = legend_title
    ) +
    #This allows us to add a new color scale for the tips
    ggnewscale::new_scale_color() +
    #Add colored tip points by family
    geom_tippoint(aes(color = .data[[family_col]]), size = tip_size) +
    scale_color_manual(values = family_colors, na.value = "grey50", name = "Family", guide = "none") +
    ggtitle(title) +
    theme(legend.position = c(0.705, 0.24),
          legend.justification = c(1, 0),
          legend.title = element_text(size = 16),      #LEGEND TITLE SIZE
          legend.text = element_text(size = 15),       #LEGEND TEXT SIZE
          plot.title = element_text(size = 18))        #PLOT TITLE SIZE

  #Add horizontal family labels with connecting lines
  if (layout == "circular") {
    #For circular layout, use geom_cladelab for horizontal labels
    #First get the node numbers for each family representative
    tree_data <- p$data %>%
      filter(isTip) %>%
      inner_join(rep_df, by = "label")

    #Need to reset color scale for labels
    p <- p +
      ggnewscale::new_scale_color() +
      geom_segment(
        data = tree_data,
        aes(x = x, y = y, xend = x, yend = y),
        size = 0.3,   #LABEL TEXT SIZE
        color = "gray40",
        lineend = "round"
      ) +
      geom_tiplab2(
        data = . %>% filter(label %in% rep_df$label),
        aes(label = .data[[family_col]], color = .data[[family_col]]),
        size = label_size,
        offset = label_offset,
        hjust = 0,
        angle = 0,  # Force horizontal
        fontface = "bold"
      ) +
      #Add the scale back for the labels to use family colors
      scale_color_manual(values = family_colors, na.value = "grey50", guide = "none")
  } else {
    #For rectangular layout
    p <- p +
      ggnewscale::new_scale_color() +
      geom_tiplab2(
        data = . %>% filter(label %in% rep_df$label),
        aes(label = .data[[family_col]], color = .data[[family_col]]),
        size = label_size,
        offset = label_offset,
        hjust = 0,
        fontface = "bold"
      ) +
      scale_color_manual(values = family_colors, na.value = "grey50", guide = "none")
  }

  return(p)
}

# Example usage for NA dataset with n_int coloring
phyloplot <- plot_phylo_combined(
  tree = NA_tree,
  data = inter_NA_working,
  species_col = "species",
  family_col = "family",
  value_col = "n_int",
  layout = "circular",
  branch_colors = c("#440154FF", "#414487FF", "#2A788EFF", "#22A884FF", "#7AD151FF", "#FDE725FF"),
  breaks = c(min(inter_NA_working$n_int, na.rm = TRUE),
             5, 25, 100,
             max(inter_NA_working$n_int, na.rm = TRUE)),
  label_offset = 8,
  label_size = 4.5,
  tip_size = 1.5,
  min_species_for_label = 25,
  title = "",
  legend_title = "Number of interactions"
)
ggsave("phylo_plot.png", phyloplot, width = 16, height = 10, dpi = 600)

# Example usage for NA dataset with n_type coloring
plot_phylo_combined(
  tree = NA_tree,
  data = inter_NA_working,
  species_col = "species",
  family_col = "family",
  value_col = "n_type",
  layout = "circular",
  branch_colors = c("gold", "orange", "red", "darkred"),
  breaks = c(min(inter_NA_working$n_type, na.rm = TRUE),
             2, 5, 10,
             max(inter_NA_working$n_type, na.rm = TRUE)),
  label_offset = 8,
  label_size = 3.5,
  tip_size = 2,
  min_species_for_label = 25,
  title = "Phylogenetic tree of # of types of interactions - North America"
)

# For full dataset
plot_phylo_combined(
  tree = full_tree,
  data = inter_working,
  species_col = "species",
  family_col = "family",
  value_col = "n_int",
  layout = "circular",
  label_offset = 8,
  min_species_for_label = 25,
  title = "Phylogenetic tree of # of interactions - Full dataset"
)

# For NA only dataset
plot_phylo_combined(
  tree = NA_only_tree,
  data = inter_NA_only_working,
  species_col = "species",
  family_col = "family",
  value_col = "n_int",
  layout = "circular",
  label_offset = 8,
  min_species_for_label = 13,
  title = "Phylogenetic tree of # of interactions in North American Birds"
)
