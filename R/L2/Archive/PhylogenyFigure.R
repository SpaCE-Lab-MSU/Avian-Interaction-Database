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
library(RColorBrewer)

source("C:/R MSU/Avian-Interaction-Database/R/L2/Archive/FigureDataProcessing.R")


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
#ggtree(NA_tree, layout = "circular") %<+% inter_NA_working +
#  geom_tree(mapping = aes(color = n_int)) +
#  ggtitle("Phylogenetic tree of # of interactions - North America") +
#  scale_color_gradientn(
#    trans = "log10",
#    colors = c("gold", "orange", "red", "darkred"),
#    breaks = c(min(inter_NA_working$n_int, na.rm = TRUE),
#               5, 25, 100,
#               max(inter_NA_working$n_int, na.rm = TRUE)),
#    labels = c(min(inter_NA_working$n_int, na.rm = TRUE),
#               5, 25, 100,
#               max(inter_NA_working$n_int, na.rm = TRUE)))

#color by n unique types
#ggtree(NA_tree, layout = "circular") %<+% inter_NA_working +
#  geom_tree(mapping = aes(color = n_type)) +
#  ggtitle("Phylogenetic tree of # of types of interactions  - North America") +
#  scale_color_gradientn(
#    trans = "log10",
#    colors = c("gold", "orange", "red", "darkred"),
#    breaks = c(min(inter_NA_working$n_type, na.rm = TRUE),
#               2, 5, 10,
#               max(inter_NA_working$n_type, na.rm = TRUE)),
#    labels = c(min(inter_NA_working$n_type, na.rm = TRUE),
#               2, 5, 10,
#               max(inter_NA_working$n_type, na.rm = TRUE)))

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


#plot_phylo_by_family(
#  tree = NA_tree,
#  data = inter_NA_working,  # uses original column names
#  species_col = "species",
#  family_col = "family",
#  title = ""
#)


# -----------------------------------------------
# Phylogenetic Visualization -- Full dataset
# -----------------------------------------------
#Now we repeat the same plotting process, but without filtering for NA first

#Branches COLORED
#color by n interactions
#ggtree(full_tree, layout = "circular") %<+% inter_working +
#  geom_tree(mapping = aes(color = n_int)) +
#  ggtitle("Phylogenetic tree of # of interactions - Full dataset") +
#  scale_color_gradientn(
#    trans = "log10",
#    colors = c("gold", "orange", "red", "darkred"),
#    breaks = c(min(inter_working$n_int, na.rm = TRUE),
#               5, 25, 100,
#               max(inter_working$n_int, na.rm = TRUE)),
#    labels = c(min(inter_working$n_int, na.rm = TRUE),
#               5, 25, 100,
#               max(inter_working$n_int, na.rm = TRUE)))

#color by n unique types
#ggtree(full_tree, layout = "circular") %<+% inter_working +
#  geom_tree(mapping = aes(color = n_type)) +
#  ggtitle("Phylogenetic tree of # of types of interactions - Full dataset") +
#  scale_color_gradientn(
#    trans = "log10",
#    colors = c("gold", "orange", "red", "darkred"),
#    breaks = c(min(inter_working$n_type, na.rm = TRUE),
#               2, 5, 10,
#               max(inter_working$n_type, na.rm = TRUE)),
#    labels = c(min(inter_working$n_type, na.rm = TRUE),
#               2, 5, 10,
#               max(inter_working$n_type, na.rm = TRUE)))



# -----------------------------------------------
# Phylogenetic Visualization -- NA only dataset
# -----------------------------------------------
#Now we repeat the same plotting process, but for NA only dataset

#Branches COLORED
#color by n interactions
#ggtree(NA_only_tree, layout = "circular") %<+% inter_NA_only_working +
#  geom_tree(mapping = aes(color = n_int)) +
#  ggtitle("Phylogenetic tree of # of interactions - NA Only dataset") +
#  scale_color_gradientn(
#    trans = "log10",
#    colors = c("gold", "orange", "red", "darkred"),
#    breaks = c(min(inter_NA_only_working$n_int, na.rm = TRUE),
#               5, 25, 100,
#               max(inter_NA_only_working$n_int, na.rm = TRUE)),
#    labels = c(min(inter_NA_only_working$n_int, na.rm = TRUE),
#               5, 25, 100,
#               max(inter_NA_only_working$n_int, na.rm = TRUE)))

#color by n unique types
#ggtree(NA_only_tree, layout = "circular") %<+% inter_NA_only_working +
#  geom_tree(mapping = aes(color = n_type)) +
#  ggtitle("Phylogenetic tree of # of types of interactions - NA only dataset") +
#  scale_color_gradientn(
#    trans = "log10",
#    colors = c("gold", "orange", "red", "darkred"),
#    breaks = c(min(inter_NA_only_working$n_type, na.rm = TRUE),
#               2, 5, 10,
#               max(inter_NA_only_working$n_type, na.rm = TRUE)),
#    labels = c(min(inter_NA_only_working$n_type, na.rm = TRUE),
#               2, 5, 10,
#               max(inter_NA_only_working$n_type, na.rm = TRUE)))


#Attempting to plot families and branch colors at the same time

plot_phylo_combined <- function(tree,
                                data,
                                species_col = "species",
                                family_col = "family",
                                value_col = "n_int",
                                layout = "circular",
                                branch_colors = c("#440154FF", "#414487FF", "#2A788EFF", "#22A884FF", "#7AD151FF", "#FDE725FF"),
                                breaks = NULL,
                                label_offset = 8,
                                label_size = 3.5,
                                tip_size = 2,
                                family_palette = NULL,
                                title = "",
                                min_species = NULL,           # Minimum number of species
                                min_interactions = NULL,      # Minimum total interactions
                                min_interactions_per_species = NULL,  # Minimum interactions per species
                                legend_title = "") {

  # Rename species column to match tree tips
  plot_data <- data %>%
    rename(label = .data[[species_col]])

  # Calculate family metrics for filtering
  family_metrics <- plot_data %>%
    filter(!is.na(.data[[family_col]])) %>%
    group_by(.data[[family_col]]) %>%
    summarise(
      n_species = n(),
      total_interactions = sum(.data[[value_col]], na.rm = TRUE),
      interactions_per_species = total_interactions / n_species,
      .groups = "drop"
    )

  # Determine which families to label based on all provided thresholds (AND logic)
  families_to_label <- family_metrics

  if (!is.null(min_species)) {
    families_to_label <- families_to_label %>%
      filter(n_species >= min_species)
  }

  if (!is.null(min_interactions)) {
    families_to_label <- families_to_label %>%
      filter(total_interactions >= min_interactions)
  }

  if (!is.null(min_interactions_per_species)) {
    families_to_label <- families_to_label %>%
      filter(interactions_per_species >= min_interactions_per_species)
  }

  families_to_label <- families_to_label %>%
    pull(.data[[family_col]])

  # Get the central (median position) species per labeled family for labeling
  # Use tree tip order to find true phylogenetic center
  tree_tips <- tree$tip.label

  rep_df <- plot_data %>%
    filter(!is.na(.data[[family_col]]),
           .data[[family_col]] %in% families_to_label) %>%
    # Add tree order
    mutate(tree_order = match(label, tree_tips)) %>%
    group_by(.data[[family_col]]) %>%
    # Use median tree position to find center
    mutate(median_pos = median(tree_order, na.rm = TRUE)) %>%
    arrange(abs(tree_order - median_pos)) %>%
    slice(1) %>%  # Select species closest to median position
    ungroup() %>%
    select(label, family = .data[[family_col]])

  # Set breaks if not provided
  if (is.null(breaks)) {
    val_range <- range(plot_data[[value_col]], na.rm = TRUE)
    breaks <- c(val_range[1], 5, 25, 100, val_range[2])
  }

  # Create color palette only for labeled families (excluding grey tones)
  n_labeled <- length(families_to_label)

  if (is.null(family_palette)) {
    family_palette <- if (n_labeled <= 8) {
      brewer.pal(max(n_labeled, 3), "Dark2")
    } else if (n_labeled <= 12) {
      brewer.pal(n_labeled, "Set3")
    } else if (n_labeled <= 24) {
      c(brewer.pal(8, "Dark2"),
        brewer.pal(9, "Set1"),
        brewer.pal(min(n_labeled - 17, 8), "Set2"))
    } else {
      # Use rainbow instead of grey scale for many families
      rainbow(n_labeled, s = 0.7, v = 0.85)
    }
  }

  # Create named vector with colors only for labeled families
  family_colors <- setNames(family_palette[1:n_labeled], families_to_label)

  # Filter plot_data to only include labeled families for tip coloring
  plot_data_tips <- plot_data %>%
    mutate(family_filtered = ifelse(.data[[family_col]] %in% families_to_label,
                                    .data[[family_col]],
                                    NA_character_))

  # Create base tree plot
  p <- ggtree(tree, layout = layout) %<+% plot_data +
    geom_tree(aes(color = .data[[value_col]])) +
    scale_color_gradientn(
      trans = "log10",
      colors = branch_colors,
      breaks = breaks,
      labels = breaks,
      name = legend_title
    ) +
    ggnewscale::new_scale_color() +
    # Only plot tips for families that meet threshold
    geom_tippoint(data = . %>% filter(.data[[family_col]] %in% families_to_label),
                  aes(color = .data[[family_col]]),
                  size = tip_size) +
    scale_color_manual(values = family_colors, na.value = "grey50", name = "Family", guide = "none") +
    ggtitle(title) +
    theme(
      legend.position = c(0.705, 0.24),
      legend.justification = c(1, 0),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 15),
      plot.title = element_text(size = 18)
    )

  # Add family labels at center positions
  if (layout == "circular") {
    p <- p +
      ggnewscale::new_scale_color() +
      geom_tiplab2(
        data = . %>% filter(label %in% rep_df$label),
        aes(label = .data[[family_col]], color = .data[[family_col]]),
        size = label_size,
        offset = label_offset,
        hjust = 0,
        angle = 0,
        fontface = "bold"
      ) +
      scale_color_manual(values = family_colors, na.value = "grey50", guide = "none")
  } else {
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

# Example usage with species threshold (original behavior)
phyloplot_species <- plot_phylo_combined(
  tree = NA_tree,
  data = inter_NA_working,
  species_col = "species",
  family_col = "family",
  value_col = "n_int",
  layout = "circular",
  branch_colors = c("#440154FF", "#414487FF", "#2A788EFF", "#22A884FF", "#7AD151FF", "#FDE725FF"),
  breaks = c(min(inter_NA_working$n_int, na.rm = TRUE), 5, 25, 100,
             max(inter_NA_working$n_int, na.rm = TRUE)),
  label_offset = 8,
  label_size = 4.5,
  tip_size = 2,
  min_species = 25,
  title = "",
  legend_title = "Number of interactions"
)
phyloplot_species
ggsave("phylo_plot.png", phyloplot_species, width = 16, height = 10, dpi = 600)

# Example usage with total interactions threshold
phyloplot_total <- plot_phylo_combined(
  tree = NA_tree,
  data = inter_NA_working,
  species_col = "species",
  family_col = "family",
  value_col = "n_int",
  layout = "circular",
  branch_colors = c("#440154FF", "#414487FF", "#2A788EFF", "#22A884FF", "#7AD151FF", "#FDE725FF"),
  breaks = c(min(inter_NA_working$n_int, na.rm = TRUE), 5, 25, 100,
             max(inter_NA_working$n_int, na.rm = TRUE)),
  label_offset = 8,
  label_size = 4.5,
  tip_size = 2,
  min_interactions = 500,
  title = "",
  legend_title = "Number of interactions"
)
phyloplot_total
ggsave("phylo_plot2.png", phyloplot_total, width = 16, height = 10, dpi = 600)

# Example usage with interactions per species threshold
phyloplot_scaled <- plot_phylo_combined(
  tree = NA_tree,
  data = inter_NA_working,
  species_col = "species",
  family_col = "family",
  value_col = "n_int",
  layout = "circular",
  branch_colors = c("#440154FF", "#414487FF", "#2A788EFF", "#22A884FF", "#7AD151FF", "#FDE725FF"),
  breaks = c(min(inter_NA_working$n_int, na.rm = TRUE), 5, 25, 100,
             max(inter_NA_working$n_int, na.rm = TRUE)),
  label_offset = 8,
  label_size = 4.5,
  tip_size = 2,
  min_interactions_per_species = 30,
  min_species = 10,
  title = "",
  legend_title = "Number of interactions"
)
phyloplot_scaled
ggsave("phylo_plot3.png", phyloplot_scaled, width = 16, height = 10, dpi = 600)



##### TESTING SAME FAMILY COLORS

plot_phylo_combined <- function(tree,
                                data,
                                species_col = "species",
                                family_col = "family",
                                value_col = "n_int",
                                layout = "circular",
                                branch_colors = c("#440154FF", "#414487FF", "#2A788EFF", "#22A884FF", "#7AD151FF", "#FDE725FF"),
                                breaks = NULL,
                                label_offset = 8,
                                label_size = 3.5,
                                tip_size = 2,
                                family_palette = NULL,
                                title = "",
                                min_species = NULL,
                                min_interactions = NULL,
                                min_interactions_per_species = NULL,
                                legend_title = "") {

  # Rename species column to match tree tips
  plot_data <- data %>%
    rename(label = .data[[species_col]])

  # Calculate family metrics for filtering
  family_metrics <- plot_data %>%
    filter(!is.na(.data[[family_col]])) %>%
    group_by(.data[[family_col]]) %>%
    summarise(
      n_species = n(),
      total_interactions = sum(.data[[value_col]], na.rm = TRUE),
      interactions_per_species = total_interactions / n_species,
      .groups = "drop"
    )

  # Determine which families to label based on all provided thresholds (AND logic)
  families_to_label <- family_metrics

  if (!is.null(min_species)) {
    families_to_label <- families_to_label %>%
      filter(n_species >= min_species)
  }

  if (!is.null(min_interactions)) {
    families_to_label <- families_to_label %>%
      filter(total_interactions >= min_interactions)
  }

  if (!is.null(min_interactions_per_species)) {
    families_to_label <- families_to_label %>%
      filter(interactions_per_species >= min_interactions_per_species)
  }

  families_to_label <- families_to_label %>%
    pull(.data[[family_col]])

  # Get the central (median position) species per labeled family for labeling
  # Use tree tip order to find true phylogenetic center
  tree_tips <- tree$tip.label

  rep_df <- plot_data %>%
    filter(!is.na(.data[[family_col]]),
           .data[[family_col]] %in% families_to_label) %>%
    # Add tree order
    mutate(tree_order = match(label, tree_tips)) %>%
    group_by(.data[[family_col]]) %>%
    # Use median tree position to find center
    mutate(median_pos = median(tree_order, na.rm = TRUE)) %>%
    arrange(abs(tree_order - median_pos)) %>%
    slice(1) %>%
    ungroup() %>%
    select(label, family = .data[[family_col]])

  # Set breaks if not provided
  if (is.null(breaks)) {
    val_range <- range(plot_data[[value_col]], na.rm = TRUE)
    breaks <- c(val_range[1], 5, 25, 100, val_range[2])
  }

  # Handle family colors
  if (!is.null(family_palette)) {
    # Use only the families that are being labeled from the provided palette
    family_colors <- family_palette[families_to_label]
    # Remove any NAs (families not in the master palette)
    family_colors <- family_colors[!is.na(family_colors)]
  } else {
    # Create color palette only for labeled families (excluding grey tones)
    n_labeled <- length(families_to_label)

    if (n_labeled <= 8) {
      palette_colors <- brewer.pal(max(n_labeled, 3), "Dark2")
    } else if (n_labeled <= 12) {
      palette_colors <- brewer.pal(n_labeled, "Set3")
    } else if (n_labeled <= 24) {
      palette_colors <- c(brewer.pal(8, "Dark2"),
                          brewer.pal(9, "Set1"),
                          brewer.pal(min(n_labeled - 17, 8), "Set2"))
    } else {
      # Use rainbow instead of grey scale for many families
      palette_colors <- rainbow(n_labeled, s = 0.7, v = 0.85)
    }

    # Create named vector with colors only for labeled families
    family_colors <- setNames(palette_colors[1:n_labeled], families_to_label)
  }

  # Filter plot_data to only include labeled families for tip coloring
  plot_data_tips <- plot_data %>%
    mutate(family_filtered = ifelse(.data[[family_col]] %in% families_to_label,
                                    .data[[family_col]],
                                    NA_character_))

  # Create base tree plot
  p <- ggtree(tree, layout = layout) %<+% plot_data +
    geom_tree(aes(color = .data[[value_col]])) +
    scale_color_gradientn(
      trans = "log10",
      colors = branch_colors,
      breaks = breaks,
      labels = breaks,
      name = legend_title
    ) +
    ggnewscale::new_scale_color() +
    # Only plot tips for families that meet threshold
    geom_tippoint(data = . %>% filter(.data[[family_col]] %in% families_to_label),
                  aes(color = .data[[family_col]]),
                  size = tip_size) +
    scale_color_manual(values = family_colors, na.value = "grey50", name = "Family", guide = "none") +
    ggtitle(title) +
    theme(
      legend.position = c(0.705, 0.24),
      legend.justification = c(1, 0),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 15),
      plot.title = element_text(size = 18)
    )

  # Add family labels at center positions
  if (layout == "circular") {
    p <- p +
      ggnewscale::new_scale_color() +
      geom_tiplab2(
        data = . %>% filter(label %in% rep_df$label),
        aes(label = .data[[family_col]], color = .data[[family_col]]),
        size = label_size,
        offset = label_offset,
        hjust = 0,
        angle = 0,
        fontface = "bold"
      ) +
      scale_color_manual(values = family_colors, na.value = "grey50", guide = "none")
  } else {
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

# ============================================================================
# CREATE MASTER COLOR PALETTE FOR CONSISTENT COLORS ACROSS ALL PLOTS
# ============================================================================

# Get all unique families from the data
all_families <- inter_NA_working %>%
  filter(!is.na(family)) %>%
  pull(family) %>%
  unique() %>%
  sort()

# Generate diverse colors for all families
n_families <- length(all_families)

if (n_families <= 8) {
  master_colors <- brewer.pal(max(n_families, 3), "Dark2")
} else if (n_families <= 12) {
  master_colors <- brewer.pal(n_families, "Set3")
} else if (n_families <= 24) {
  master_colors <- c(brewer.pal(8, "Dark2"),
                     brewer.pal(9, "Set1"),
                     brewer.pal(min(n_families - 17, 8), "Set2"))
} else {
  # For many families, use multiple palettes interlaced for maximum diversity
  # This ensures adjacent families in alphabetical order get very different colors
  n_sets <- ceiling(n_families / 8)
  color_list <- list()

  # Generate multiple color sets
  color_list[[1]] <- brewer.pal(8, "Dark2")
  color_list[[2]] <- brewer.pal(9, "Set1")
  color_list[[3]] <- brewer.pal(8, "Set2")
  color_list[[4]] <- brewer.pal(8, "Accent")

  # If we need even more colors, use rainbow with different starting points
  if (n_sets > 4) {
    for (i in 5:n_sets) {
      offset <- ((i - 5) / max(1, (n_sets - 4))) * 0.9  # Scale to [0, 0.9]
      end_point <- min(1, offset + 0.8)  # Ensure end doesn't exceed 1
      if (end_point > 1) {
        # Wrap around if needed
        color_list[[i]] <- rainbow(8, s = 0.7, v = 0.85, start = offset)
      } else {
        color_list[[i]] <- rainbow(8, s = 0.7, v = 0.85, start = offset, end = end_point)
      }
    }
  }

  # Interlace colors from different sets to maximize diversity
  master_colors <- character(n_families)
  for (i in 1:n_families) {
    set_idx <- ((i - 1) %% length(color_list)) + 1
    color_idx <- ((i - 1) %/% length(color_list)) + 1
    if (color_idx <= length(color_list[[set_idx]])) {
      master_colors[i] <- color_list[[set_idx]][color_idx]
    } else {
      # Fallback to rainbow if we run out
      master_colors[i] <- rainbow(n_families, s = 0.7, v = 0.85)[i]
    }
  }
}

# Create named vector mapping each family to a color
family_palette_master <- setNames(master_colors, all_families)

# ============================================================================
# EXAMPLE USAGE - ALL PLOTS USE THE SAME MASTER PALETTE
# ============================================================================

# Example 1: Species threshold (original behavior)
phyloplot_species <- plot_phylo_combined(
  tree = NA_tree,
  data = inter_NA_working,
  species_col = "species",
  family_col = "family",
  value_col = "n_int",
  layout = "circular",
  branch_colors = c("#440154FF", "#414487FF", "#2A788EFF", "#22A884FF", "#7AD151FF", "#FDE725FF"),
  breaks = c(min(inter_NA_working$n_int, na.rm = TRUE), 5, 25, 100,
             max(inter_NA_working$n_int, na.rm = TRUE)),
  label_offset = 8,
  label_size = 4.5,
  tip_size = 2,
  min_species = 25,
  family_palette = family_palette_master,  # Use master palette
  title = "",
  legend_title = "Number of interactions"
)
phyloplot_species
ggsave("phylo_plotsame.png", phyloplot_species, width = 16, height = 10, dpi = 600)

# Example 2: Total interactions threshold
phyloplot_total <- plot_phylo_combined(
  tree = NA_tree,
  data = inter_NA_working,
  species_col = "species",
  family_col = "family",
  value_col = "n_int",
  layout = "circular",
  branch_colors = c("#440154FF", "#414487FF", "#2A788EFF", "#22A884FF", "#7AD151FF", "#FDE725FF"),
  breaks = c(min(inter_NA_working$n_int, na.rm = TRUE), 5, 25, 100,
             max(inter_NA_working$n_int, na.rm = TRUE)),
  label_offset = 8,
  label_size = 4.5,
  tip_size = 2,
  min_interactions = 500,
  family_palette = family_palette_master,  # Use master palette
  title = "",
  legend_title = "Number of interactions"
)
phyloplot_total
ggsave("phylo_plotsame2.png", phyloplot_total, width = 16, height = 10, dpi = 600)

# Example 3: Interactions per species threshold
phyloplot_scaled <- plot_phylo_combined(
  tree = NA_tree,
  data = inter_NA_working,
  species_col = "species",
  family_col = "family",
  value_col = "n_int",
  layout = "circular",
  branch_colors = c("#440154FF", "#414487FF", "#2A788EFF", "#22A884FF", "#7AD151FF", "#FDE725FF"),
  breaks = c(min(inter_NA_working$n_int, na.rm = TRUE), 5, 25, 100,
             max(inter_NA_working$n_int, na.rm = TRUE)),
  label_offset = 8,
  label_size = 4.5,
  tip_size = 2,
  min_interactions_per_species = 30,
  min_species = 10,
  family_palette = family_palette_master,  # Use master palette
  title = "",
  legend_title = "Number of interactions"
)
phyloplot_scaled
ggsave("phylo_plotsame3.png", phyloplot_scaled, width = 16, height = 10, dpi = 600)
