# Title:   Example Network figures for North
#          American avian interactions
# Authors: Lucas Mansfield
# Date:    11 December 2025 -

# -----------------------------------------------
# Loading packages and data
# -----------------------------------------------

#TO DO
# - Add phylo functions to separate file for sourcing
# - check overlapping interactions


rm(list=ls())

library(rphylopic)
library(tidyverse)
library(igraph)
library(ggraph)
library(cowplot)
source("C:/R MSU/Avian-Interaction-Database/website/code/avicommons_clem.r")
avi <- jsonlite::fromJSON("C:/R MSU/Avian-Interaction-Database/website/code/avicommons_full.json")
clem <- readr::read_csv("C:/R MSU/Avian-Interaction-Database/website/code/clemtax_2025.csv")
source("C:/R MSU/Avian-Interaction-Database/R/L2/AvianInteractionPhylogenyFigure.R")

inter_NA_only_int <- inter_NA_only_dedup %>% #removing "non-interactions"
  filter(interaction != "co-occur" & interaction != "hybridization") %>%
  mutate(interaction = str_replace(interaction, "copulation\\?", "copulation"))

# -----------------------------------------------
# Workflow Function
# -----------------------------------------------

#Function for generating network and bird image combined
create_network <- function(data,
                           focal_species,
                           layout = "fr",
                           node_size = 3,
                           focal_node_size = 5,
                           show_labels = TRUE,
                           image_size = 480,
                           bird_width = 0.3,
                           network_width = 0.7) {

  #Filter for rows containing the focal species
  focal_interactions <- data %>%
    filter(species1_scientific == focal_species |
             species2_scientific == focal_species)

  #Get all species that interact with the focal species
  network_species <- unique(c(focal_interactions$species1_scientific,
                              focal_interactions$species2_scientific))

  #Get all interactions among these species
  network_data <- data %>%
    filter(species1_scientific %in% network_species,
           species2_scientific %in% network_species)

  #Create graph object
  network_graph <- graph_from_data_frame(
    d = network_data[, c("species1_scientific", "species2_scientific")],
    directed = TRUE,
    vertices = NULL
  )

  #Add vertex attributes to identify focal species
  V(network_graph)$is_focal <- V(network_graph)$name == focal_species

  #Add edge attributes
  E(network_graph)$interaction <- network_data$interaction
  E(network_graph)$effect_sp1_on_sp2 <- network_data$effect_sp1_on_sp2
  E(network_graph)$effect_sp2_on_sp1 <- network_data$effect_sp2_on_sp1

  #Get bird info
  bird_info <- get_avicommons_image(focal_species, size = image_size)

  #Create title with common name
  plot_title <- paste0("Interaction network for ",
                       bird_info$common_name, " (",
                       focal_species, ")")

  #Create the network plot with conditional node styling
  network_plot <- ggraph(network_graph, layout = layout) +
    geom_edge_link(aes(color = interaction),
                   end_cap = circle(3, 'mm')) +
    #Focal species as star
    geom_node_point(data = function(x) x[x$is_focal, ],
                    shape = 8,  # star shape
                    size = focal_node_size,
                    stroke = 1.5) +
    #Other species as circles
    geom_node_point(data = function(x) x[!x$is_focal, ],
                    size = node_size) +
    theme_void() +
    labs(edge_color = "Interaction Type",
         title = plot_title) +
    theme(legend.position = "right",
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

  #Add labels if requested
  if (show_labels) {
    network_plot <- network_plot +
      geom_node_text(aes(label = name), repel = TRUE, size = 3)
  }

  #Create bird image plot with same text styling as original
  bird_plot <- ggdraw() +
    draw_image(bird_info$url, x = 0, y = 0.2, width = 1, height = 0.8) +
    #Common name (bold)
    draw_label(
      bird_info$common_name,
      x = 0.5, y = 0.25,
      size = 16,
      fontface = "bold",
      hjust = 0.5
    ) +
    #Scientific name (italic)
    draw_label(
      bird_info$scientific_name,
      x = 0.5, y = 0.22,
      size = 14,
      fontface = "italic",
      hjust = 0.5
    ) +
    #Attribution
    draw_label(
      paste0("Photo: ", bird_info$photographer, " | ", toupper(bird_info$license)),
      x = 0.5, y = 0.18,
      size = 10,
      hjust = 0.5
    )

  #Combine them side by side
  combined_plot <- plot_grid(
    bird_plot,
    network_plot,
    ncol = 2,
    rel_widths = c(bird_width, network_width)
  )

  return(combined_plot)
}


# -----------------------------------------------
# Test plotting
# -----------------------------------------------

oaktit <- create_network(inter_NA_only_int, "Baeolophus inornatus")
oaktit

juntit <- create_network(inter_NA_only_int, "Baeolophus ridgwayi")
juntit

create_network(inter_NA_only_int, "")
