#This script provides the data formatting and cleaning needed to produce the figures that appear in the paper from the North American Avian Interaction Databse
#This script has little annotating information, as the same code is present in 5.5_figure_processing_vignette.qmd
#Vignette 5.5 contains a walkthrough of the process, with explanations of the logic and explanations of intermediate outputs.
#This script exists so it can be sourced by the final vignette, 6_summary_vignette.qmd

# --------------------------------------------------------------------
# Libraries and data
# --------------------------------------------------------------------

library(tidyverse)
library(stringr)
library(clootl)
library(ape)
library(ggtree)
library(ggplot2)
library(ggnewscale)
library(RColorBrewer)
library(igraph)
library(ggraph)
library(cowplot)

inter_NA <- read.csv(here::here("../Avian-Interaction-Database-Working/L1/ain_cac.csv")) #NA avian interaction data

# --------------------------------------------------------------------
# Interaction color scheme and ordering
# --------------------------------------------------------------------

interaction_categories <- data.frame(
  interaction = c(
    # Trophic (Dark Red)
    "predation", "nest predation",
    # Mobbing (Pink)
    "mobbing",
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
    # Amenalism (Black)
    "amensalism",
    # N/A (Grays)
    "co-occur", "hybridization", "copulation", "courtship", "play", "shared scolding", "accidental killing"
  ),
  category = c(
    # Trophic
    rep("Trophic", 2),
    # Mobbing
    "Mobbing",
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
    # N/A
    rep("N/A", 7)
  ),
  color = c(
    # Trophic (Reds/Oranges)
    "#800000", "#CD5C5C",
    # Mobbing (Teal)
    "#E0115F",
    # Competition (Dark Blues)
    "#00008B", "#1E3A8A", "#2563EB", "#3B82F6",
    # Facilitation (Greens)
    "#006400", "#228B22", "#32CD32", "#00A86B", "#50C878",
    "#ADFF2F", "#7CFC00", "#66CDAA", "#3CB371",
    # Commensalism (Purples)
    "#4B0082", "#6A5ACD", "#6A5ACD", "#9370DB", "#BA55D3",
    # Parasitism (Oranges/Browns)
    "#D2691E", "#CD853F", "#DAA520", "#B8860B", "#DEB887", "#F4A460",
    # Amenalism (Black)
    "#000000",
    # N/A (Grays)
    "#808080", "#696880", "#373737", "#7C6E7F", "#333333", "#9897A9", "#787276"
  ),
  stringsAsFactors = FALSE
)

# Define category order for plotting
category_order <- c("Trophic", "Mobbing", "Competition", "Facilitation",
                    "Commensalism", "Parasitism", "Amenalism",
                    "N/A")

interaction_categories$category <- factor(interaction_categories$category,
                                          levels = category_order)

# --------------------------------------------------------------------
# Slimming dataset
# --------------------------------------------------------------------

inter_NA_slim <- inter_NA %>%
  rowwise() %>% #This removes duplicate interactions (same sp pair and int type)
  mutate(       #by creating columns of the species pairs in alphabetical order
    sp_min = min(species1_scientific, species2_scientific),
    sp_max = max(species1_scientific, species2_scientific)
  ) %>%
  ungroup() %>%
  distinct(sp_min, sp_max, interaction, .keep_all = TRUE) %>% #removing duplicates
  select(-sp_min, -sp_max) #and deleting the helper columns


# --------------------------------------------------------------------
# Distribution figure data processing (no plot function for this fig)
# --------------------------------------------------------------------

type_summ <- inter_NA %>%
  group_by(interaction) %>%
  summarize(n = n()) %>%
  left_join(interaction_categories, by = "interaction") %>%
  arrange(category, desc(n))

#Create factor for ordered plotting
type_summ$interaction <- factor(type_summ$interaction,
                                levels = rev(type_summ$interaction))

#Repeat for slim dataset
type_summ_slim <- inter_NA_slim %>%
  group_by(interaction) %>%
  summarize(n = n()) %>%
  left_join(interaction_categories, by = "interaction") %>%
  arrange(category, desc(n))
type_summ_slim$interaction <- factor(type_summ_slim$interaction,
                                     levels = rev(type_summ_slim$interaction))

# --------------------------------------------------------------------
# Phylogeny figure data processing, tree generation, and plot function
# --------------------------------------------------------------------

inter_NA_trim <- inter_NA_slim %>%
  filter(
    !str_detect(.data[["species1_scientific"]], regex("sp\\.|unid\\.|_x_", ignore_case = TRUE)),
    !str_detect(.data[["species2_scientific"]], regex("sp\\.|unid\\.|_x_", ignore_case = TRUE))
  ) %>%
  #Collapse subspecies to species
  mutate(
    "species1_scientific" = str_replace(.data[["species1_scientific"]],
                                        "^([A-Za-z]+\\s+[A-Za-z]+).*$", "\\1"),
    "species2_scientific" = str_replace(.data[["species2_scientific"]],
                                        "^([A-Za-z]+\\s+[A-Za-z]+).*$", "\\1")
  ) %>%
  #Remove self interactions
  filter(.data[["species1_scientific"]] != .data[["species2_scientific"]])

inter_NA_clean <- inter_NA_trim %>%
  rowwise() %>% #This removes duplicate interactions (same sp pair and int type)
  mutate(       #by creating columns of the species pairs in alphabetical order
    sp_min = min(species1_scientific, species2_scientific),
    sp_max = max(species1_scientific, species2_scientific)
  ) %>%
  ungroup() %>%
  distinct(sp_min, sp_max, interaction, .keep_all = TRUE) %>% #removing duplicates
  select(-sp_min, -sp_max) #and deleting the helper columns

inter_NA_flip <- inter_NA_clean %>%
  rename(
    temp_sp1_com = .data[["species2_common"]],
    temp_sp2_com = .data[["species1_common"]],
    temp_sp1_sci = .data[["species2_scientific"]],
    temp_sp2_sci = .data[["species1_scientific"]]
  ) %>%
  rename(
    species1_common = temp_sp1_com,
    species2_common = temp_sp2_com,
    species1_scientific = temp_sp1_sci,
    species2_scientific = temp_sp2_sci
  )

inter_NA_full <- dplyr::union(inter_NA_clean, inter_NA_flip)

inter_NA_working <- inter_NA_full %>%
  group_by(.data[["species1_scientific"]]) %>%
  #Replace spaces with underscores to match tree tips
  mutate(species1_scientific = str_replace_all(.data[["species1_scientific"]], " ", "_")) %>%
  rename(species = species1_scientific) %>%
  summarize(
    n_int = n(),
    n_type = n_distinct(.data[["interaction"]]),
    .groups = "drop"
  )

get_clements_family <- function(species_vector,
                                taxonomy_year = 2024,
                                return_full_taxonomy = FALSE) {

  #Access the taxonomy data from clootl_data
  taxonomy_name <- paste0("Year", taxonomy_year)

  if (!taxonomy_name %in% names(clootl_data$taxonomy.files)) {
    stop("Taxonomy year ", taxonomy_year, " not available. Use 2021-2024.")
  }

  clements <- clootl_data$taxonomy.files[[taxonomy_name]]

  message("Using Clements taxonomy year: ", taxonomy_year)
  message("Looking up ", length(species_vector), " species...")


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

  n_found <- sum(!is.na(results$family))
  n_missing <- sum(is.na(results$family))
  message("Found families for ", n_found, "/", length(species_vector), " species")
  if (n_missing > 0) {
    message("Missing families for ", n_missing, " species")
    message("First few missing: ", paste(head(species_clean[is.na(results$family)], 3), collapse = ", "))
  }


  #Return appropriate format
  if (return_full_taxonomy) {
    return(results)
  } else {
    return(results$family)
  }
}

inter_NA_working$family <- get_clements_family(
  inter_NA_working[["species"]],
  taxonomy_year = 2024
)

inter_NA_list <- str_replace_all(inter_NA_working$species, "_", " ")
NA_tree <- extractTree(species = inter_NA_list, label_type = "scientific", taxonomy_year = 2024)

plot_phylo_combined <- function(tree, #tree produced earlier, should be NA_tree
                                data, #working dataset
                                value_col = "n_int", #value column, can replace with "n_type"
                                label_offset = 8, #offset of family labels
                                label_size = 4.5, #size of family labels
                                tip_size = 2, #size of colored circles representing families
                                min_species = NULL, #optional species size threshold for labeling
                                legend_title = "") { #legend title

  #Rename species column to match requirements in ggtree
  plot_data <- data %>%
    rename(label = .data[["species"]])

  #Calculate family size for filtering
  family_metrics <- plot_data %>%
    filter(!is.na(.data[["family"]])) %>%
    group_by(.data[["family"]]) %>%
    summarise(
      n_species = n(),
      .groups = "drop"
    )

  #Determine which families to label based on species threshold
  families_to_label <- family_metrics

  if (!is.null(min_species)) {
    families_to_label <- families_to_label %>%
      filter(n_species >= min_species)
  }

  families_to_label <- families_to_label %>%
    pull(.data[["family"]])

  #Get the central (median position) species per labeled family for labeling
  #Use tree tip order to find true phylogenetic center
  tree_tips <- tree$tip.label

  rep_df <- plot_data %>%
    filter(!is.na(.data[["family"]]),
           .data[["family"]] %in% families_to_label) %>%
    #Add tree order
    mutate(tree_order = match(label, tree_tips)) %>%
    group_by(.data[["family"]]) %>%
    #Use median tree position to find center
    mutate(median_pos = median(tree_order, na.rm = TRUE)) %>%
    arrange(abs(tree_order - median_pos)) %>%
    slice(1) %>%  #Select species closest to median position
    ungroup() %>%
    select(label, family = .data[["family"]])

  #Set breaks
  val_range <- range(plot_data[[value_col]], na.rm = TRUE)
  breaks <- c(val_range[1], 5, 25, 100, val_range[2])

  #Create color palette only for labeled families (excluding grey tones)
  n_labeled <- length(families_to_label)
  family_palette <- c(brewer.pal(8, "Dark2"),
                      brewer.pal(9, "Set1"),
                      brewer.pal(min(n_labeled - 17, 8), "Set2"))

  #Create named vector with colors only for labeled families
  family_colors <- setNames(family_palette[1:n_labeled], families_to_label)

  #Filter plot_data to only include labeled families for tip coloring
  plot_data_tips <- plot_data %>%
    mutate(family_filtered = ifelse(.data[["family"]] %in% families_to_label,
                                    .data[["family"]],
                                    NA_character_))

  #Create base tree plot
  p <- ggtree(tree, layout = "circular") %<+% plot_data +
    geom_tree(aes(color = .data[[value_col]])) +
    scale_color_gradientn(
      trans = "log10",
      colors = c("#440154FF", "#414487FF", "#2A788EFF", "#22A884FF", "#7AD151FF", "#FDE725FF"),
      breaks = breaks,
      labels = breaks,
      name = legend_title
    ) +
    ggnewscale::new_scale_color() +
    #Only plot tips for families that meet threshold
    geom_tippoint(data = . %>% filter(.data[["family"]] %in% families_to_label),
                  aes(color = .data[["family"]]),
                  size = tip_size) +
    scale_color_manual(values = family_colors, na.value = "grey50", name = "Family", guide = "none") +
    theme(
      legend.position = c(0.705, 0.24),
      legend.justification = c(1, 0),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 15),
      plot.title = element_text(size = 18)
    )

  # Add family labels at center positions
  p <- p +
    ggnewscale::new_scale_color() +
    geom_tiplab2(
      data = . %>% filter(label %in% rep_df$label),
      aes(label = .data[["family"]], color = .data[["family"]]),
      size = label_size,
      offset = label_offset,
      hjust = 0,
      angle = 0,
      fontface = "bold"
    ) +
    scale_color_manual(values = family_colors, na.value = "grey50", guide = "none")

  return(p)
}



# --------------------------------------------------------------------
# Network figure data processing and plot function
# --------------------------------------------------------------------

source(here::here("../Avian-Interaction-Database/website/code/avicommons_clem.r"))
avi <- jsonlite::fromJSON(here::here("../Avian-Interaction-Database/website/code/avicommons_full.json"))
clem <- readr::read_csv(here::here("../Avian-Interaction-Database/website/code/clemtax_2025.csv"))

inter_NA_int <- inter_NA_clean %>% #removing "non-interactions"
  filter(interaction != "co-occur" & interaction != "hybridization")

create_network <- function(data, #input dataset, should be inter_NA_int
                           focal_species, #scientific name of a species of interest
                           layout = "fr", #geometric layout algorithm for plot
                           show_labels = TRUE, #option to label species names
                           image_size = 480, #size of bird image, options are 240, 320, 480, 900
                           bird_width = 0.3, #relative size of bird image
                           network_width = 0.7, #relative size of network
                           edge_alpha = 0.5, #alpha value of interactions (transparency)
                           edge_width = 1.25, #linewidth of interactions
                           focal_node_size = 10, #size of species of interest
                           node_size = 8, #size of interacting species
                           node_stroke = 1.2, #node outline width
                           focal_node_stroke = 1.5, #focal species outline width
                           focal_label_size = 5, #focal species text size (if show_labels = T)
                           label_size = 4, #interacting species text size (if show_labels = T)
                           legend_text_size = 23, #interaction type text size
                           legend_title_size = 25, #legend title text size
                           curve_edges = FALSE) { #set true for visualizing multiple int. between two species

  #Filter interactions involving focal species
  focal_interactions <- data %>%
    filter(species1_scientific == focal_species |
             species2_scientific == focal_species)

  #Get network species and their interactions
  network_species <- unique(c(focal_interactions$species1_scientific,
                              focal_interactions$species2_scientific))

  network_data <- data %>%
    filter(species1_scientific %in% network_species,
           species2_scientific %in% network_species) %>%
    left_join(interaction_categories, by = "interaction")

  #Create graph with vertex and edge attributes
  network_graph <- graph_from_data_frame(
    d = network_data[, c("species1_scientific", "species2_scientific")],
    vertices = NULL)

  V(network_graph)$is_focal <- V(network_graph)$name == focal_species
  E(network_graph)$interaction <- network_data$interaction

  #Get bird information
  bird_info <- get_avicommons_image(focal_species, size = image_size)

  #Build network plot
  network_plot <- ggraph(network_graph, layout = layout)

  #Add edges
  if (curve_edges) {
    network_plot <- network_plot +
      geom_edge_fan(aes(color = interaction),
                    strength = 0.75,
                    width = edge_width,
                    alpha = edge_alpha)
  } else {
    network_plot <- network_plot +
      geom_edge_link(aes(color = interaction),
                     width = edge_width,
                     alpha = edge_alpha)
  }

  network_plot <- network_plot +
    scale_edge_color_manual(values = setNames(interaction_categories$color,
                                              interaction_categories$interaction))

  #Add nodes
  network_plot <- network_plot +
    geom_node_point(data = function(x) x[x$is_focal, ],
                    shape = 23,
                    size = focal_node_size,
                    stroke = focal_node_stroke,
                    color = "black",
                    fill = "red") +
    geom_node_point(data = function(x) x[!x$is_focal, ],
                    shape = 21,
                    color = "black",
                    fill = "white",
                    size = node_size,
                    stroke = node_stroke)

  #Add labels if requested
  if (show_labels) {
    network_plot <- network_plot +
      geom_node_text(data = function(x) x[x$is_focal, ],
                     aes(label = name),
                     repel = TRUE,
                     size = focal_label_size,
                     fontface = "bold",
                     color = "black") +
      geom_node_text(data = function(x) x[!x$is_focal, ],
                     aes(label = name),
                     repel = TRUE,
                     size = label_size)
  }

  #Apply theme and styling
  network_plot <- network_plot +
    theme_void() +
    labs(edge_color = "Interaction Type") +
    theme(legend.position = "right",
          legend.text = element_text(size = legend_text_size),
          legend.title = element_text(size = legend_title_size),
          plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
          plot.margin = margin(15, 15, 15, 15)) +
    guides(edge_color = guide_legend(ncol = 1))

  #Create bird image panel
  bird_plot <- ggdraw() +
    draw_image(bird_info$url, x = 0, y = 0.2, width = 1, height = 0.8) +
    draw_label(bird_info$common_name,
               x = 0.5, y = 0.26, size = 22, fontface = "bold", hjust = 0.5) +
    draw_label(bird_info$scientific_name,
               x = 0.5, y = 0.22, size = 20, fontface = "italic", hjust = 0.5) +
    draw_label(paste0("Photo: ", bird_info$photographer, " | ",
                      toupper(bird_info$license)),
               x = 0.5, y = 0.18, size = 18, hjust = 0.5)

  #Combine and return
  combined_plot <- plot_grid(
    bird_plot,
    ggdraw() + draw_plot(network_plot, x = 0.02, y = 0.02,
                         width = 0.96, height = 0.96),
    ncol = 2,
    rel_widths = c(bird_width, network_width)
  )

  final_plot <- ggdraw() +
    draw_plot(combined_plot, x = 0.02, y = 0, width = 0.96, height = 1)

  return(final_plot)
}

create_combined_network <- function(data, #dataset (inter_NA_int)
                                    focal_species_vector, #vector of three species scientific names
                                    layout = "fr", #geometric layout algorithm for plot
                                    image_size = 480, #size of bird image, options are 240, 320, 480, 900
                                    edge_alpha = 0.5, #alpha value of interactions (transparency)
                                    edge_width = 1.25, #linewidth of interactions
                                    focal_node_size = 10, #size of species of interest
                                    node_size = 8,#size of interacting species
                                    node_stroke = 1.2, #node outline width
                                    focal_node_stroke = 1.5, #focal species outline width
                                    legend_text_size = 23, #interaction type text size
                                    legend_title_size = 25, #legend title text size
                                    curve_edges = FALSE) { #set true for visualizing multiple int. between two species

  #Assign styles to each focal species
  species_styles <- data.frame(
    species = focal_species_vector,
    shape = c(23, 24, 22),
    color = c("red", "blue", "gold"),
    stringsAsFactors = FALSE
  )

  #Create lists to store components
  bird_plots <- list()
  network_plots <- list()
  all_interactions <- c()  #Track all interaction types, used later in mutual legend

  #Loop through each focal species
  for (i in seq_along(focal_species_vector)) {
    focal_species <- focal_species_vector[i]

    #Filter interactions involving focal species
    focal_interactions <- data %>%
      filter(species1_scientific == focal_species |
               species2_scientific == focal_species)

    #Get network species and their interactions
    network_species <- unique(c(focal_interactions$species1_scientific,
                                focal_interactions$species2_scientific))

    network_data <- data %>%
      filter(species1_scientific %in% network_species,
             species2_scientific %in% network_species) %>%
      left_join(interaction_categories, by = "interaction")

    #Track unique interactions across all networks
    all_interactions <- c(all_interactions, network_data$interaction)

    #Create graph with vertex and edge attributes
    network_graph <- graph_from_data_frame(
      d = network_data[, c("species1_scientific", "species2_scientific")],
      vertices = NULL
    )

    #Identify which nodes are focal species
    V(network_graph)$is_current_focal <- V(network_graph)$name == focal_species
    V(network_graph)$is_other_focal <- V(network_graph)$name %in% focal_species_vector &
      V(network_graph)$name != focal_species
    V(network_graph)$is_regular <- !V(network_graph)$is_current_focal &
      !V(network_graph)$is_other_focal

    #Assign shapes and colors
    V(network_graph)$node_shape <- sapply(V(network_graph)$name, function(sp) {
      if (sp %in% species_styles$species) {
        species_styles$shape[species_styles$species == sp]
      } else {
        21 #circle for non-focal species
      }
    })

    V(network_graph)$node_color <- sapply(V(network_graph)$name, function(sp) {
      if (sp %in% species_styles$species) {
        species_styles$color[species_styles$species == sp]
      } else {
        "white" #white color for non-focal species
      }
    })

    E(network_graph)$interaction <- network_data$interaction

    #Get bird information
    bird_info <- get_avicommons_image(focal_species, size = image_size)

    #Build network plot
    network_plot <- ggraph(network_graph, layout = layout)

    #Add edges
    if (curve_edges) {
      network_plot <- network_plot +
        geom_edge_fan(aes(color = interaction),
                      strength = 0.75,
                      width = edge_width,
                      alpha = edge_alpha)
    } else {
      network_plot <- network_plot +
        geom_edge_link(aes(color = interaction),
                       width = edge_width,
                       alpha = edge_alpha)
    }

    network_plot <- network_plot +
      scale_edge_color_manual(values = setNames(interaction_categories$color,
                                                interaction_categories$interaction))

    #Add nodes - current focal species
    network_plot <- network_plot +
      geom_node_point(data = function(x) x[x$is_current_focal, ],
                      aes(shape = node_shape, fill = node_color),
                      size = focal_node_size,
                      stroke = focal_node_stroke,
                      color = "black") +
      scale_shape_identity() +
      scale_fill_identity()

    #Add nodes - other focal species
    if (any(V(network_graph)$is_other_focal)) {
      network_plot <- network_plot +
        geom_node_point(data = function(x) x[x$is_other_focal, ],
                        aes(shape = node_shape, fill = node_color),
                        size = focal_node_size * 0.8,
                        stroke = node_stroke,
                        color = "black")
    }

    #Add nodes - regular species
    network_plot <- network_plot +
      geom_node_point(data = function(x) x[x$is_regular, ],
                      shape = 21,
                      color = "black",
                      fill = "white",
                      size = node_size,
                      stroke = node_stroke)

    #Apply theme - no legend
    network_plot <- network_plot +
      theme_void() +
      theme(legend.position = "none", #this is done because a unified legend is used instead
            plot.margin = margin(10, 10, 10, 10))

    #Get the shape and color for this focal species
    focal_shape <- species_styles$shape[species_styles$species == focal_species]
    focal_color <- species_styles$color[species_styles$species == focal_species]

    #Create a small ggplot with just the symbol
    symbol_plot <- ggplot() +
      geom_point(aes(x = 0, y = 0),
                 shape = focal_shape,
                 fill = focal_color,
                 color = "black",
                 size = 8,
                 stroke = 1.5) +
      theme_void() +
      coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1))

    #Create bird image panel with symbol
    bird_plot <- ggdraw() +
      draw_image(bird_info$url, x = 0, y = 0.15, width = 1, height = 0.85) +
      draw_plot(symbol_plot, x = 0.68, y = 0.14, width = 0.08, height = 0.16) +
      draw_label(bird_info$common_name,
                 x = 0.5, y = 0.08, size = 26, fontface = "bold", hjust = 0.5) +
      draw_label(bird_info$scientific_name,
                 x = 0.5, y = -0.02, size = 24, fontface = "italic", hjust = 0.5)

    #Store components
    bird_plots[[i]] <- bird_plot
    network_plots[[i]] <- network_plot
  }

  #Get unique interactions present across all three networks
  unique_interactions <- unique(all_interactions)

  #Filter interaction_categories to only include present interactions
  legend_data <- interaction_categories %>%
    filter(interaction %in% unique_interactions)

  legend_data$interaction <- factor(legend_data$interaction,
                                    levels = legend_data$interaction)

  #Create a standalone legend plot with LINES instead of points
  legend_plot <- ggplot(legend_data, aes(x = interaction, y = 1, color = interaction)) +
    geom_line(size = 2) +
    scale_color_manual(values = setNames(legend_data$color, legend_data$interaction),
                       name = "Interaction Type") +
    theme_void() +
    theme(legend.position = "bottom",
          legend.text = element_text(size = legend_text_size),
          legend.box.margin = margin(t = 10, b = 5),
          legend.title = element_blank()) +
    guides(color = guide_legend(nrow = 4, byrow = TRUE, override.aes = list(size = 2)))

  #Extract just the legend
  legend_grob <- cowplot::get_legend(legend_plot)

  #Arrange bird images in a row
  bird_row <- plot_grid(plotlist = bird_plots, ncol = 3, rel_widths = c(1, 1, 1))

  #Arrange network plots in a row
  network_row <- plot_grid(plotlist = network_plots, ncol = 3, rel_widths = c(1, 1, 1))

  #Add legend below networks
  network_with_legend <- plot_grid(network_row, legend_grob,
                                   ncol = 1,
                                   rel_heights = c(1, 0.15))

  #Combine birds and networks vertically
  combined_plot <- plot_grid(bird_row, network_with_legend,
                             ncol = 1,
                             rel_heights = c(0.2, 0.8))

  #Add dashed vertical lines between each bird/network column
  final_plot <- ggdraw(combined_plot) +
    geom_segment(aes(x = 1/3, xend = 1/3, y = 0.15, yend = 1),
                 linetype = "dashed", color = "black", linewidth = 0.8) +
    geom_segment(aes(x = 2/3, xend = 2/3, y = 0.15, yend = 1),
                 linetype = "dashed", color = "black", linewidth = 0.8)

  return(final_plot)
}
