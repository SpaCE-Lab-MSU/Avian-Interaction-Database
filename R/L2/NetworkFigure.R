# Title:   Example Network figures for North
#          American avian interactions
# Authors: Lucas Mansfield
# Date:    11 December 2025 -

# -----------------------------------------------
# Loading packages and data
# -----------------------------------------------



rm(list=ls())
library(tidyverse)
library(igraph)
library(ggraph)
library(cowplot)
source("C:/R MSU/Avian-Interaction-Database/R/L2/FigureDataProcessing.R")
source("C:/R MSU/Avian-Interaction-Database/website/code/avicommons_clem.r")
avi <- jsonlite::fromJSON("C:/R MSU/Avian-Interaction-Database/website/code/avicommons_full.json")
clem <- readr::read_csv("C:/R MSU/Avian-Interaction-Database/website/code/clemtax_2025.csv")

inter_NA_only_int <- inter_NA_only_clean %>% #removing "non-interactions" and fixing "copulation?"
  filter(interaction != "co-occur" & interaction != "hybridization") %>%
  rowwise() %>% #This removes duplicate interactions (same sp pair and int type)
  mutate(       #by creating columns of the species pairs in alphabetical order
    sp_min = min(species1_scientific, species2_scientific),
    sp_max = max(species1_scientific, species2_scientific)
  ) %>%
  ungroup() %>%
  distinct(sp_min, sp_max, interaction, .keep_all = TRUE) %>% #removing duplicates
  select(-sp_min, -sp_max) #and deleting the helper columns

# -----------------------------------------------
# Workflow Function
# -----------------------------------------------

#Function for generating network and bird image combined
create_network <- function(data,
                           focal_species,
                           layout = "fr",
                           show_labels = TRUE,
                           show_title = FALSE,
                           image_size = 480,
                           bird_width = 0.3,
                           network_width = 0.7,
                           edge_alpha = 0.5,
                           edge_width = 1.25,
                           focal_node_size = 10,
                           node_size = 8,
                           node_stroke = 1.2,
                           focal_node_stroke = 1.5,
                           focal_label_size = 5,
                           label_size = 4,
                           legend_text_size = 23,
                           legend_title_size = 25,
                           curve_edges = FALSE) {

  # Filter interactions involving focal species
  focal_interactions <- data %>%
    filter(species1_scientific == focal_species |
             species2_scientific == focal_species)

  # Get network species and their interactions
  network_species <- unique(c(focal_interactions$species1_scientific,
                              focal_interactions$species2_scientific))

  network_data <- data %>%
    filter(species1_scientific %in% network_species,
           species2_scientific %in% network_species) %>%
    left_join(interaction_categories, by = "interaction")

  # Create graph with vertex and edge attributes
  network_graph <- graph_from_data_frame(
    d = network_data[, c("species1_scientific", "species2_scientific")],
    directed = !curve_edges,
    vertices = NULL
  )

  V(network_graph)$is_focal <- V(network_graph)$name == focal_species
  E(network_graph)$interaction <- network_data$interaction
  E(network_graph)$color <- network_data$color

  # Get bird information
  bird_info <- get_avicommons_image(focal_species, size = image_size)

  plot_title <- if (show_title) {
    paste0("Interaction network for ", bird_info$common_name,
           " (", focal_species, ")")
  } else {
    ""
  }

  # Build network plot
  network_plot <- ggraph(network_graph, layout = layout)

  # Add edges
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

  # Add nodes
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

  # Add labels if requested
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

  # Apply theme and styling
  network_plot <- network_plot +
    theme_void() +
    labs(edge_color = "Interaction Type",
         title = plot_title) +
    theme(legend.position = "right",
          legend.text = element_text(size = legend_text_size),
          legend.title = element_text(size = legend_title_size),
          plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
          plot.margin = margin(15, 15, 15, 15)) +
    guides(edge_color = guide_legend(ncol = 1))

  # Create bird image panel
  bird_plot <- ggdraw() +
    draw_image(bird_info$url, x = 0, y = 0.2, width = 1, height = 0.8) +
    draw_label(bird_info$common_name,
               x = 0.5, y = 0.26, size = 22, fontface = "bold", hjust = 0.5) +
    draw_label(bird_info$scientific_name,
               x = 0.5, y = 0.22, size = 20, fontface = "italic", hjust = 0.5) +
    draw_label(paste0("Photo: ", bird_info$photographer, " | ",
                      toupper(bird_info$license)),
               x = 0.5, y = 0.18, size = 18, hjust = 0.5)

  # Combine and return
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

# -----------------------------------------------
# Test plotting
# -----------------------------------------------

oaktit <- create_network(inter_NA_only_int, "Baeolophus inornatus", curve_edges = T, show_labels = F)
oaktit
ggsave("network_plot.png", oaktit, width = 16, height = 10, dpi = 600)


juntit <- create_network(inter_NA_only_int, "Baeolophus ridgwayi", curve_edges = T)
juntit

bridtit <- create_network(inter_NA_only_int, "Baeolophus wollweberi", curve_edges = T)
bridtit

tuftit <- create_network(inter_NA_only_int, "Baeolophus bicolor", curve_edges = T)
tuftit

bctit <- create_network(inter_NA_only_int, "Baeolophus atricristatus", curve_edges = T)
bctit

# -----------------------------------------------
# Three species function!
# -----------------------------------------------

create_multi_network <- function(data,
                                 focal_species_vector,
                                 layout = "fr",
                                 show_labels = TRUE,
                                 show_title = FALSE,
                                 image_size = 480,
                                 bird_width = 0.3,
                                 network_width = 0.7,
                                 edge_alpha = 0.5,
                                 edge_width = 1.25,
                                 focal_node_size = 10,
                                 node_size = 8,
                                 node_stroke = 1.2,
                                 focal_node_stroke = 1.5,
                                 focal_label_size = 5,
                                 label_size = 4,
                                 legend_text_size = 23,
                                 legend_title_size = 25,
                                 curve_edges = FALSE) {

  # Define unique shapes and colors for focal species
  focal_styles <- data.frame(
    shape = c(23, 24, 22, 25, 21),  # diamond, triangle up, square, triangle down, circle
    color = c("red", "blue", "gold", "purple", "green"),
    stringsAsFactors = FALSE
  )

  # Assign styles to each focal species
  species_styles <- data.frame(
    species = focal_species_vector,
    shape = focal_styles$shape[1:length(focal_species_vector)],
    color = focal_styles$color[1:length(focal_species_vector)],
    stringsAsFactors = FALSE
  )

  # Create list to store all plots
  plot_list <- list()

  # Loop through each focal species
  for (i in seq_along(focal_species_vector)) {
    focal_species <- focal_species_vector[i]

    # Filter interactions involving focal species
    focal_interactions <- data %>%
      filter(species1_scientific == focal_species |
               species2_scientific == focal_species)

    # Get network species and their interactions
    network_species <- unique(c(focal_interactions$species1_scientific,
                                focal_interactions$species2_scientific))

    network_data <- data %>%
      filter(species1_scientific %in% network_species,
             species2_scientific %in% network_species) %>%
      left_join(interaction_categories, by = "interaction")

    # Create graph with vertex and edge attributes
    network_graph <- graph_from_data_frame(
      d = network_data[, c("species1_scientific", "species2_scientific")],
      directed = !curve_edges,
      vertices = NULL
    )

    # Identify which nodes are focal species (current or other focals)
    V(network_graph)$is_current_focal <- V(network_graph)$name == focal_species
    V(network_graph)$is_other_focal <- V(network_graph)$name %in% focal_species_vector &
      V(network_graph)$name != focal_species
    V(network_graph)$is_regular <- !V(network_graph)$is_current_focal &
      !V(network_graph)$is_other_focal

    # Assign shapes and colors
    V(network_graph)$node_shape <- sapply(V(network_graph)$name, function(sp) {
      if (sp %in% species_styles$species) {
        species_styles$shape[species_styles$species == sp]
      } else {
        21  # default circle
      }
    })

    V(network_graph)$node_color <- sapply(V(network_graph)$name, function(sp) {
      if (sp %in% species_styles$species) {
        species_styles$color[species_styles$species == sp]
      } else {
        "white"  # default
      }
    })

    E(network_graph)$interaction <- network_data$interaction
    E(network_graph)$color <- network_data$color

    # Get bird information
    bird_info <- get_avicommons_image(focal_species, size = image_size)

    plot_title <- if (show_title) {
      paste0("Interaction network for ", bird_info$common_name,
             " (", focal_species, ")")
    } else {
      ""
    }

    # Build network plot
    network_plot <- ggraph(network_graph, layout = layout)

    # Add edges
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

    # Add nodes - current focal species
    network_plot <- network_plot +
      geom_node_point(data = function(x) x[x$is_current_focal, ],
                      aes(shape = node_shape, fill = node_color),
                      size = focal_node_size,
                      stroke = focal_node_stroke,
                      color = "black") +
      scale_shape_identity() +
      scale_fill_identity()

    # Add nodes - other focal species appearing in network
    if (any(V(network_graph)$is_other_focal)) {
      network_plot <- network_plot +
        geom_node_point(data = function(x) x[x$is_other_focal, ],
                        aes(shape = node_shape, fill = node_color),
                        size = focal_node_size * 0.8,
                        stroke = node_stroke,
                        color = "black")
    }

    # Add nodes - regular species
    network_plot <- network_plot +
      geom_node_point(data = function(x) x[x$is_regular, ],
                      shape = 21,
                      color = "black",
                      fill = "white",
                      size = node_size,
                      stroke = node_stroke)

    # Add labels if requested
    if (show_labels) {
      network_plot <- network_plot +
        geom_node_text(data = function(x) x[x$is_current_focal, ],
                       aes(label = name),
                       repel = TRUE,
                       size = focal_label_size,
                       fontface = "bold",
                       color = "black") +
        geom_node_text(data = function(x) x[x$is_other_focal, ],
                       aes(label = name),
                       repel = TRUE,
                       size = focal_label_size * 0.9,
                       fontface = "bold",
                       color = "black") +
        geom_node_text(data = function(x) x[x$is_regular, ],
                       aes(label = name),
                       repel = TRUE,
                       size = label_size)
    }

    # Apply theme and styling
    network_plot <- network_plot +
      theme_void() +
      labs(edge_color = "Interaction Type",
           title = plot_title) +
      theme(legend.position = "right",
            legend.text = element_text(size = legend_text_size),
            legend.title = element_text(size = legend_title_size),
            plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
            plot.margin = margin(15, 15, 15, 15)) +
      guides(edge_color = guide_legend(ncol = 1))

    # Create bird image panel
    bird_plot <- ggdraw() +
      draw_image(bird_info$url, x = 0, y = 0.2, width = 1, height = 0.8) +
      draw_label(bird_info$common_name,
                 x = 0.5, y = 0.26, size = 22, fontface = "bold", hjust = 0.5) +
      draw_label(bird_info$scientific_name,
                 x = 0.5, y = 0.22, size = 20, fontface = "italic", hjust = 0.5) +
      draw_label(paste0("Photo: ", bird_info$photographer, " | ",
                        toupper(bird_info$license)),
                 x = 0.5, y = 0.18, size = 18, hjust = 0.5)

    # Combine plots
    combined_plot <- plot_grid(
      bird_plot,
      ggdraw() + draw_plot(network_plot, x = 0.02, y = 0.02,
                           width = 0.96, height = 0.96),
      ncol = 2,
      rel_widths = c(bird_width, network_width)
    )

    final_plot <- ggdraw() +
      draw_plot(combined_plot, x = 0.02, y = 0, width = 0.96, height = 1)

    # Store in list
    plot_list[[i]] <- final_plot
  }

  return(plot_list)
}


# -----------------------------------------------
# Test plotting
# -----------------------------------------------

plots <- create_multi_network(inter_NA_only_int, c("Baeolophus inornatus", "Dryobates nuttallii", "Megascops Kennicottii"), show_labels = F, curve_edges = T)

ggsave("netplot1.png", plots[[1]], width = 16, height = 10, dpi = 600)
ggsave("netplot2.png", plots[[2]], width = 16, height = 10, dpi = 600)
ggsave("netplot3.png", plots[[3]], width = 16, height = 10, dpi = 600)

# -----------------------------------------------
# Same panel function
# -----------------------------------------------


create_combined_network <- function(data,
                                    focal_species_vector,
                                    layout = "fr",
                                    show_labels = TRUE,
                                    show_title = FALSE,
                                    image_size = 480,
                                    edge_alpha = 0.5,
                                    edge_width = 1.25,
                                    focal_node_size = 10,
                                    node_size = 8,
                                    node_stroke = 1.2,
                                    focal_node_stroke = 1.5,
                                    focal_label_size = 5,
                                    label_size = 4,
                                    legend_text_size = 23,
                                    legend_title_size = 25,
                                    curve_edges = FALSE) {

  # Define unique shapes and colors for focal species
  focal_styles <- data.frame(
    shape = c(23, 24, 22, 25, 21),
    color = c("red", "blue", "gold", "purple", "green"),
    stringsAsFactors = FALSE
  )

  # Assign styles to each focal species
  species_styles <- data.frame(
    species = focal_species_vector,
    shape = focal_styles$shape[1:length(focal_species_vector)],
    color = focal_styles$color[1:length(focal_species_vector)],
    stringsAsFactors = FALSE
  )

  # Create lists to store components
  bird_plots <- list()
  network_plots <- list()
  all_interactions <- c()  # Track all interaction types

  # Loop through each focal species
  for (i in seq_along(focal_species_vector)) {
    focal_species <- focal_species_vector[i]

    # Filter interactions involving focal species
    focal_interactions <- data %>%
      filter(species1_scientific == focal_species |
               species2_scientific == focal_species)

    # Get network species and their interactions
    network_species <- unique(c(focal_interactions$species1_scientific,
                                focal_interactions$species2_scientific))

    network_data <- data %>%
      filter(species1_scientific %in% network_species,
             species2_scientific %in% network_species) %>%
      left_join(interaction_categories, by = "interaction")

    # Track unique interactions across all networks
    all_interactions <- c(all_interactions, network_data$interaction)

    # Create graph with vertex and edge attributes
    network_graph <- graph_from_data_frame(
      d = network_data[, c("species1_scientific", "species2_scientific")],
      directed = !curve_edges,
      vertices = NULL
    )

    # Identify which nodes are focal species
    V(network_graph)$is_current_focal <- V(network_graph)$name == focal_species
    V(network_graph)$is_other_focal <- V(network_graph)$name %in% focal_species_vector &
      V(network_graph)$name != focal_species
    V(network_graph)$is_regular <- !V(network_graph)$is_current_focal &
      !V(network_graph)$is_other_focal

    # Assign shapes and colors
    V(network_graph)$node_shape <- sapply(V(network_graph)$name, function(sp) {
      if (sp %in% species_styles$species) {
        species_styles$shape[species_styles$species == sp]
      } else {
        21
      }
    })

    V(network_graph)$node_color <- sapply(V(network_graph)$name, function(sp) {
      if (sp %in% species_styles$species) {
        species_styles$color[species_styles$species == sp]
      } else {
        "white"
      }
    })

    E(network_graph)$interaction <- network_data$interaction
    E(network_graph)$color <- network_data$color

    # Get bird information
    bird_info <- get_avicommons_image(focal_species, size = image_size)

    # Build network plot
    network_plot <- ggraph(network_graph, layout = layout)

    # Add edges
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

    # Add nodes - current focal species
    network_plot <- network_plot +
      geom_node_point(data = function(x) x[x$is_current_focal, ],
                      aes(shape = node_shape, fill = node_color),
                      size = focal_node_size,
                      stroke = focal_node_stroke,
                      color = "black") +
      scale_shape_identity() +
      scale_fill_identity()

    # Add nodes - other focal species
    if (any(V(network_graph)$is_other_focal)) {
      network_plot <- network_plot +
        geom_node_point(data = function(x) x[x$is_other_focal, ],
                        aes(shape = node_shape, fill = node_color),
                        size = focal_node_size * 0.8,
                        stroke = node_stroke,
                        color = "black")
    }

    # Add nodes - regular species
    network_plot <- network_plot +
      geom_node_point(data = function(x) x[x$is_regular, ],
                      shape = 21,
                      color = "black",
                      fill = "white",
                      size = node_size,
                      stroke = node_stroke)

    # Add labels if requested
    if (show_labels) {
      network_plot <- network_plot +
        geom_node_text(data = function(x) x[x$is_current_focal, ],
                       aes(label = name),
                       repel = TRUE,
                       size = focal_label_size,
                       fontface = "bold",
                       color = "black") +
        geom_node_text(data = function(x) x[x$is_other_focal, ],
                       aes(label = name),
                       repel = TRUE,
                       size = focal_label_size * 0.9,
                       fontface = "bold",
                       color = "black") +
        geom_node_text(data = function(x) x[x$is_regular, ],
                       aes(label = name),
                       repel = TRUE,
                       size = label_size)
    }

    # Apply theme - no legend
    network_plot <- network_plot +
      theme_void() +
      theme(legend.position = "none",
            plot.margin = margin(10, 10, 10, 10))

    # Get the shape and color for this focal species
    focal_shape <- species_styles$shape[species_styles$species == focal_species]
    focal_color <- species_styles$color[species_styles$species == focal_species]

    # Create a small ggplot with just the symbol
    symbol_plot <- ggplot() +
      geom_point(aes(x = 0, y = 0),
                 shape = focal_shape,
                 fill = focal_color,
                 color = "black",
                 size = 8,
                 stroke = 1.5) +
      theme_void() +
      coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1))

    # Create bird image panel with symbol
    bird_plot <- ggdraw() +
      draw_image(bird_info$url, x = 0, y = 0.15, width = 1, height = 0.85) +
      draw_plot(symbol_plot, x = 0.68, y = 0.14, width = 0.08, height = 0.16) +
      draw_label(bird_info$common_name,
                 x = 0.5, y = 0.08, size = 26, fontface = "bold", hjust = 0.5) +
      draw_label(bird_info$scientific_name,
                 x = 0.5, y = -0.02, size = 24, fontface = "italic", hjust = 0.5)

    # Store components
    bird_plots[[i]] <- bird_plot
    network_plots[[i]] <- network_plot
  }

  # Get unique interactions present across all three networks
  unique_interactions <- unique(all_interactions)

  # Filter interaction_categories to only include present interactions
  legend_data <- interaction_categories %>%
    filter(interaction %in% unique_interactions)

  # Create a standalone legend plot with LINES instead of points
  legend_plot <- ggplot(legend_data, aes(x = interaction, y = 1, color = interaction)) +
    geom_line(size = 2) +  # Changed from geom_point to geom_line
    scale_color_manual(values = setNames(legend_data$color, legend_data$interaction),
                       name = "Interaction Type") +
    theme_void() +
    theme(legend.position = "bottom",
          legend.text = element_text(size = legend_text_size),
          legend.box.margin = margin(t = 10, b = 5),
          legend.title = element_blank()) +
    guides(color = guide_legend(nrow = 4, byrow = TRUE, override.aes = list(size = 2)))

  # Extract just the legend
  legend_grob <- cowplot::get_legend(legend_plot)

  # Arrange bird images in a row
  bird_row <- plot_grid(plotlist = bird_plots, ncol = 3, rel_widths = c(1, 1, 1))

  # Arrange network plots in a row
  network_row <- plot_grid(plotlist = network_plots, ncol = 3, rel_widths = c(1, 1, 1))

  # Add legend below networks
  network_with_legend <- plot_grid(network_row, legend_grob,
                                   ncol = 1,
                                   rel_heights = c(1, 0.15))

  # Combine birds and networks vertically
  combined_plot <- plot_grid(bird_row, network_with_legend,
                             ncol = 1,
                             rel_heights = c(0.2, 0.8))

  # Add dashed vertical lines between each bird/network column
  final_plot <- ggdraw(combined_plot) +
    geom_segment(aes(x = 1/3, xend = 1/3, y = 0.15, yend = 1),
                 linetype = "dashed", color = "black", linewidth = 0.8) +
    geom_segment(aes(x = 2/3, xend = 2/3, y = 0.15, yend = 1),
                 linetype = "dashed", color = "black", linewidth = 0.8)

  return(final_plot)
}

plot <- create_combined_network(inter_NA_only_int, c("Baeolophus inornatus", "Dryobates nuttallii", "Glaucidium gnoma"), show_labels = F, curve_edges = T)
ggsave("netplot4.png", plot, width = 20, height = 15, dpi = 600)

# -----------------------------------------------
# Filter interactions by species
# -----------------------------------------------

get_interactions <- function(data, species_list, inc = T) {
  ifelse(inc,
        data <- data[data$species1_scientific %in% species_list & data$species2_scientific %in% species_list,],
        data <- data[data$species1_scientific %in% species_list | data$species2_scientific %in% species_list,])
return(data)
  }

ints <- get_interactions(inter_NA_only_int, c("Baeolophus inornatus", "Dryobates nuttallii", "Glaucidium gnoma"), inc=F)
intsinc <- get_interactions(inter_NA_only_int, c("Baeolophus inornatus", "Dryobates nuttallii", "Glaucidium gnoma"))
View(intsinc)

write.csv(intsinc, "intsinc.csv")
# -----------------------------------------------
# Generate networks for all species and save to PDF
# Organized by phylogenetic order from clem
# -----------------------------------------------

#All unique species from the dataset
#all_species <- unique(c(inter_NA_only_int$species1_scientific,
#                        inter_NA_only_int$species2_scientific))

#Sort species by the order they appear in clem (phylogenetic order, for visualization)
#species_sorted <- clem$`scientific name`[clem$`scientific name` %in% all_species]

#Add any species not in clem at the end
#species_not_in_clem <- setdiff(all_species, species_sorted)
#species_sorted <- c(species_sorted, species_not_in_clem)

#output_dir <- "C:/R MSU/Avian-Interaction-Database/R/L2"

#PDF output
#pdf_filename <- file.path(output_dir,
#                          paste0("species_networks_phylo_",
#                                 format(Sys.Date(), "%Y%m%d"),
#                                ".pdf"))

#cairo_pdf(pdf_filename, width = 16, height = 10, onefile = TRUE)

#Create a list to store plots temporarily
#plot_list <- list()
#lot_count <- 0


#Loop through all species in phylogenetic order
#for (i in seq_along(species_sorted)) {
#  species <- species_sorted[i]


  #Create network plot
#    plot <- create_network(inter_NA_only_int,
#                           species,
#                           curve_edges = TRUE,
#                           show_labels = FALSE)

#    plot_count <- plot_count + 1
#    plot_list[[length(plot_list) + 1]] <- plot

    #When we have 4 plots, print them as a grid and reset
 #   if (length(plot_list) == 4) {
 #     combined <- plot_grid(plotlist = plot_list, ncol = 2, nrow = 2)
 #     print(combined)
#      plot_list <- list()
#    }


#}

#Print any remaining plots (if not divisible by 4)
#if (length(plot_list) > 0) {
#  combined <- plot_grid(plotlist = plot_list, ncol = 2, nrow = 2)
#  print(combined)
#}

#Close the PDF device
#dev.off()


