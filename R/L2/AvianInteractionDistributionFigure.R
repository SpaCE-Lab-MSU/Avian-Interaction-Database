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

inter$interaction <- inter$interaction %>% #Removing alignment issue in uncertain interaction
  str_replace("copulation\\?", "copulation") %>%
  str_replace("commensalism-scavange", "commensalism-scavenge")

inter_NA$interaction <- inter_NA$interaction %>%
  str_replace("copulation\\?", "copulation") %>%
  str_replace("commensalism-scavange", "commensalism-scavenge")

inter_NA_only$interaction <- inter_NA_only$interaction %>%
  str_replace("copulation\\?", "copulation") %>%
  str_replace("commensalism-scavange", "commensalism-scavenge")


# -----------------------------------------------
# Visualizing distribution of interactions - full dataset
# -----------------------------------------------
type_summ <- inter %>%
  group_by(interaction) %>%
  summarize(n = n()) %>%
  left_join(interaction_categories, by = "interaction") %>%
  arrange(category, desc(n))

# Create factor for ordered plotting
type_summ$interaction <- factor(type_summ$interaction,
                                levels = rev(type_summ$interaction))

ggplot(type_summ, aes(x = interaction, y = n, fill = interaction)) +
  theme_light() +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = setNames(interaction_categories$color,
                                      interaction_categories$interaction)) +
  geom_text(aes(label = n), hjust = -0.2, size = 3.5) +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none") +
  xlab("Interaction Type") +
  labs(title = "Total Interactions in dataset") +
  coord_flip()

# -----------------------------------------------
# Visualizing distribution of interactions - NA
# -----------------------------------------------
type_summ_NA <- inter_NA %>%
  group_by(interaction) %>%
  summarize(n = n()) %>%
  left_join(interaction_categories, by = "interaction") %>%
  arrange(category, desc(n))

type_summ_NA$interaction <- factor(type_summ_NA$interaction,
                                   levels = rev(type_summ_NA$interaction))

ggplot(type_summ_NA, aes(x = interaction, y = n, fill = interaction)) +
  theme_light() +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = setNames(interaction_categories$color,
                                      interaction_categories$interaction)) +
  geom_text(aes(label = n), hjust = -0.2, size = 3.5) +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none") +
  xlab("Interaction Type") +
  ylab("") +
  labs(title = "Total Interactions in North America") +
  coord_flip()

# -----------------------------------------------
# Visualizing distribution of interactions - NA ONLY
# -----------------------------------------------
type_summ_NA_only <- inter_NA_only %>%
  group_by(interaction) %>%
  summarize(n = n()) %>%
  left_join(interaction_categories, by = "interaction") %>%
  arrange(category, desc(n))

type_summ_NA_only$interaction <- factor(type_summ_NA_only$interaction,
                                        levels = rev(type_summ_NA_only$interaction))

ggplot(type_summ_NA_only, aes(x = interaction, y = n, fill = interaction)) +
  theme_light() +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = setNames(interaction_categories$color,
                                      interaction_categories$interaction)) +
  geom_text(aes(label = n), hjust = -0.2, size = 3.5) +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "none") +
  xlab("Interaction Type") +
  ylab("") +
  labs(title = "Total Interactions in North America") +
  coord_flip()
