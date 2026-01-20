# Title:   Distribution figures for North
#          American avian interactions
# Authors: Lucas Mansfield
# Date:    10 December 2025 -

# -----------------------------------------------
# Loading packages and data
# -----------------------------------------------
rm(list=ls())
library(tidyverse)
library(stringr)
source("C:/R MSU/Avian-Interaction-Database/R/L2/FigureDataProcessing.R")

# -----------------------------------------------
# Functionalization of plot
# -----------------------------------------------

int_dist <- function(dataset, colors = interaction_categories) {
  type_summ <- dataset %>%
    group_by(interaction) %>%
    summarize(n = n()) %>%
    left_join(colors, by = "interaction") %>%
    arrange(category, desc(n))

  # Create factor for ordered plotting
  type_summ$interaction <- factor(type_summ$interaction,
                                  levels = rev(type_summ$interaction))

  # Generating title
  title <- ifelse(identical(dataset, inter), "Total Interactions in dataset",
    "Total Interactions in North America")


  # Plotting
  ggplot(type_summ, aes(x = interaction, y = n, fill = interaction)) +
    theme_light() +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = setNames(interaction_categories$color,
                                        interaction_categories$interaction)) +
    geom_text(aes(label = n), hjust = -0.2, size = 4.25) +
    theme(axis.text.x = element_text(angle = 0, size = 14),  #Axis text
          axis.text.y = element_text(size = 14),             #Axis text
          axis.title = element_text(size = 16),              #Axis titles
          plot.title = element_text(size = 18),              #Plot title
          legend.position = "none") +
    xlab("") +
    ylab("") +
    labs(title = title) +
    coord_flip()
}





# -----------------------------------------------
# Visualizing distribution of interactions - full dataset
# -----------------------------------------------

#int_dist(inter)

# -----------------------------------------------
# Visualizing distribution of interactions - NA
# -----------------------------------------------

int_NA_plot <- int_dist(inter_NA)
int_NA_plot
ggsave("dist_plot.png", int_NA_plot, width = 16, height = 10, dpi = 600)

# -----------------------------------------------
# Visualizing distribution of interactions - NA ONLY
# -----------------------------------------------

#int_dist(inter_NA_only)
