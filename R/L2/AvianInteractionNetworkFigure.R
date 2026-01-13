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
  mutate(interaction = str_replace(interaction, "copulation\\?", "copulation")) %>%
  mutate(interaction = str_replace(interaction, "commensalism-scavange", "commensalism-scavenge")) %>%
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
                           image_size = 480,
                           bird_width = 0.3,
                           network_width = 0.7,
                           curve_edges = FALSE) {

  # Filter for rows containing the focal species
  focal_interactions <- data %>%
    filter(species1_scientific == focal_species |
             species2_scientific == focal_species)

  # Get all species that interact with the focal species
  network_species <- unique(c(focal_interactions$species1_scientific,
                              focal_interactions$species2_scientific))

  # Get all interactions between these species
  network_data <- data %>%
    filter(species1_scientific %in% network_species,
           species2_scientific %in% network_species)

  # Add color column to network_data based on interaction type
  network_data <- network_data %>%
    left_join(interaction_categories, by = "interaction")

  # Create graph object
  network_graph <- graph_from_data_frame(
    d = network_data[, c("species1_scientific", "species2_scientific")],
    directed = !curve_edges,
    vertices = NULL
  )

  # Add vertex attributes
  V(network_graph)$is_focal <- V(network_graph)$name == focal_species

  # Add edge attributes
  E(network_graph)$interaction <- network_data$interaction
  E(network_graph)$color <- network_data$color

  # Get bird info
  bird_info <- get_avicommons_image(focal_species, size = image_size)

  # Create title
  plot_title <- paste0("Interaction network for ",
                       bird_info$common_name, " (",
                       focal_species, ")")

  # Create the network plot base
  network_plot <- ggraph(network_graph, layout = layout)

  # Add edges with colors
  if (curve_edges) {
    network_plot <- network_plot +
      geom_edge_fan(aes(color = interaction),
                    strength = 0.75,
                    width = 0.75) +
      scale_edge_color_manual(values = setNames(interaction_categories$color,
                                                interaction_categories$interaction))
  } else {
    network_plot <- network_plot +
      geom_edge_link(aes(color = interaction),
                     width = 0.75) +
      scale_edge_color_manual(values = setNames(interaction_categories$color,
                                                interaction_categories$interaction))
  }

  # Add nodes and styling
  network_plot <- network_plot +
    # Focal species as star
    geom_node_point(data = function(x) x[x$is_focal, ],
                    shape = 18,
                    size = 6,  #STAR SIZE
                    stroke = 1.5,
                    color = "red") +
    # Other species as circles
    geom_node_point(data = function(x) x[!x$is_focal, ],
                    size = 3) +  #NODE SIZE
    theme_void() +
    labs(edge_color = "Interaction Type",
         title = plot_title) +
    theme(legend.position = "right",
          legend.text = element_text(size = 15),#LEGEND TEXT SIZE
          legend.title = element_text(size = 16), #LEGEND TITLE SIZE
          plot.title = element_text(size = 18,  #TITLE FONT SIZE
                                    face = "bold",
                                    hjust = 0.5)) +
    guides(edge_color = guide_legend(ncol = 1))

  # Add labels with enhanced focal species styling
  if (show_labels) {
    network_plot <- network_plot +
      # Focal species label - BOLD and LARGER
      geom_node_text(data = function(x) x[x$is_focal, ],
                     aes(label = name),
                     repel = TRUE,
                     size = 5,  #FOCAL SPECIES LABEL SIZE
                     fontface = "bold",  # Bold text
                     color = "black") +
      # Other species labels - normal
      geom_node_text(data = function(x) x[!x$is_focal, ],
                     aes(label = name),
                     repel = TRUE,
                     size = 4) #OTHER SPECIES LABEL SIZE
  }

  # Create bird image
  bird_plot <- ggdraw() +
    draw_image(bird_info$url, x = 0, y = 0.2, width = 1, height = 0.8) +
    draw_label(
      bird_info$common_name,
      x = 0.5, y = 0.26,
      size = 22, #COMMON NAME SIZE
      fontface = "bold",
      hjust = 0.5
    ) +
    draw_label(
      bird_info$scientific_name,
      x = 0.5, y = 0.22,
      size = 20, #SCI NAME SIZE
      fontface = "italic",
      hjust = 0.5
    ) +
    draw_label(
      paste0("Photo: ", bird_info$photographer, " | ", toupper(bird_info$license)),
      x = 0.5, y = 0.18,
      size = 18, #CITATION SIZE
      hjust = 0.5
    )

  # Combine plots
  combined_plot <- plot_grid(
    bird_plot,
    network_plot,
    ncol = 2,
    rel_widths = c(bird_width, network_width)
  )

  # Add white space around the figure
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
# Generate networks for all species and save to PDF
# Organized by phylogenetic order from clem
# -----------------------------------------------

#All unique species from the dataset
all_species <- unique(c(inter_NA_only_int$species1_scientific,
                        inter_NA_only_int$species2_scientific))

#Sort species by the order they appear in clem (phylogenetic order, for visualization)
species_sorted <- clem$`scientific name`[clem$`scientific name` %in% all_species]

#Add any species not in clem at the end
species_not_in_clem <- setdiff(all_species, species_sorted)
species_sorted <- c(species_sorted, species_not_in_clem)

output_dir <- "C:/R MSU/Avian-Interaction-Database/R/L2"

#PDF output
pdf_filename <- file.path(output_dir,
                          paste0("species_networks_phylo_",
                                 format(Sys.Date(), "%Y%m%d"),
                                 ".pdf"))

cairo_pdf(pdf_filename, width = 16, height = 10, onefile = TRUE)

#Create a list to store plots temporarily
plot_list <- list()
plot_count <- 0


#Loop through all species in phylogenetic order
for (i in seq_along(species_sorted)) {
  species <- species_sorted[i]


  #Create network plot
    plot <- create_network(inter_NA_only_int,
                           species,
                           curve_edges = TRUE,
                           show_labels = FALSE)

    plot_count <- plot_count + 1
    plot_list[[length(plot_list) + 1]] <- plot

    #When we have 4 plots, print them as a grid and reset
    if (length(plot_list) == 4) {
      combined <- plot_grid(plotlist = plot_list, ncol = 2, nrow = 2)
      print(combined)
      plot_list <- list()
    }


}

#Print any remaining plots (if not divisible by 4)
if (length(plot_list) > 0) {
  combined <- plot_grid(plotlist = plot_list, ncol = 2, nrow = 2)
  print(combined)
}

#Close the PDF device
dev.off()
