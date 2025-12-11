# Title:   Distribution figures for North
#          American avian interactions
# Authors: Lucas Mansfield
# Date:    10 December 2025 -

# -----------------------------------------------
# Loading packages and data
# -----------------------------------------------

rm(list=ls())
library(tidyverse)
library(ape)
library(stringr)

inter <- read.csv("R/L1/AvianInteractionData_CanadaAKCONUS_L1.csv") #Original data


# -----------------------------------------------
# Visualizing distribution of interactions - full dataset
# -----------------------------------------------

type_summ <- inter %>% group_by(interaction) %>% summarize(n=n())

ggplot(type_summ,
       aes(x=reorder(interaction,-n), y=n)) +
  theme_light() +
  geom_bar(stat = "identity", fill="#619CFF") +
  geom_text(aes(label=n), hjust=-0.2, size=3.5) +
  theme(axis.text.x = element_text(angle=0)) +
  xlab("Interaction Type") +
  labs(title = "Total Interactions in dataset") +
  coord_flip()


# -----------------------------------------------
# Visualizing distribution of interactions - NA only
# -----------------------------------------------

north_american_species <- unique(inter$species1_scientific) #get list of NA species
inter_NA <- inter %>%
  filter(species2_scientific %in% north_american_species)

type_summ_NA <- inter_NA %>% group_by(interaction) %>% summarize(n=n())

ggplot(type_summ_NA,
       aes(x=reorder(interaction,-n), y=n)) +
  theme_light() +
  geom_bar(stat = "identity", fill="#619CFF") +
  geom_text(aes(label=n), hjust=-0.2, size=3.5) +
  theme(axis.text.x = element_text(angle=0)) +
  xlab("Interaction Type") +
  labs(title = "Total Interactions in North America") +
  coord_flip()
