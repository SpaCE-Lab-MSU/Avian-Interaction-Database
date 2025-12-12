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

splist <- read.csv("C:/R MSU/Avian-Interaction-Database-Working/L1/species_checklists/splist_CanadaAKCONUS_L1.csv") #NA species list
inter <- read.csv("C:/R MSU/Avian-Interaction-Database-Working/L1/AvianInteractionData_L1.csv") #Full data
inter_NA <- read.csv("C:/R MSU/Avian-Interaction-Database-Working/L1/AvianInteractionData_CanadaAKCONUS_L1.csv") #data with one or either species in NA
inter_NA_only <- inter_NA %>% #data with BOTH species in NA -- interactions that occur in NA
  filter(species1_scientific %in% splist$scientific_name_clements2024,
         species2_scientific %in% splist$scientific_name_clements2024)

inter$interaction <- inter$interaction %>% #Removing alignment issue in uncertain interaction
  str_replace("copulation\\?", "copulation")

inter_NA$interaction <- inter_NA$interaction %>%
  str_replace("copulation\\?", "copulation")

inter_NA_only$interaction <- inter_NA_only$interaction %>%
  str_replace("copulation\\?", "copulation")

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
# Visualizing distribution of interactions - NA
# -----------------------------------------------

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

# -----------------------------------------------
# Visualizing distribution of interactions - NA ONLY
# -----------------------------------------------

type_summ_NA_only <- inter_NA_only %>% group_by(interaction) %>% summarize(n=n())

ggplot(type_summ_NA_only,
       aes(x=reorder(interaction,-n), y=n)) +
  theme_light() +
  geom_bar(stat = "identity", fill="#619CFF") +
  geom_text(aes(label=n), hjust=-0.2, size=3.5) +
  theme(axis.text.x = element_text(angle=0)) +
  xlab("Interaction Type") +
  labs(title = "Total Interactions in North America") +
  coord_flip()
