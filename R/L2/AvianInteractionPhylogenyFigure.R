# Title:   Phylogeny figures for North
#          American avian interactions
# Authors: Lucas Mansfield
# Date:    9 December 2025 -

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
library(taxize)

inter <- read.csv("R/L1/AvianInteractionData_CanadaAKCONUS_L1.csv") #Original data

#Loading phylotree from VertLife.org
trees <- read.tree(file = "R/L2/AllBirdsEricson1.tre") #This step takes a minute, there are 1000 bootstrapped trees
tree <- trees[[1]] #but we only need one

# -----------------------------------------------
# Filtering interaction data - NA only
# -----------------------------------------------

#First, our data features all interactions for North American species,
#including those with species that are not found on the Continent (that they
#interact with in South America, Europe, etc.). For the purposes of this
#visualization, we will filter out the interactions that do not take place
#between two, North American birds. Since all of the North American species
#are represented in the species1 column, we will filter the dataset to remove
#any rows in which the value for species 2 is not present in the species 1 column

north_american_species <- unique(inter$species1_scientific) #get list of NA species
inter_NA <- inter %>%
  filter(species2_scientific %in% north_american_species)

#Currently, our data is formatted like this:
#sp1   int1    sp2
#sp1   int2    sp3

#We want to combine it into two columns, for easier analysis of total interactions
#per species like so
#sp1  int1
#sp2  int1
#sp1  int2
#sp2  int2

#This duplicates our dataframe, so that every interaction: sp1  int1  sp2
#is expressed in both sp1/sp2 permutations
nrow(inter_NA) #start with 22120 rows

inter_NA_flip <- inter_NA %>%
  rename(species1_common1 = species2_common) %>%
  rename(species2_common = species1_common) %>%
  rename(species1_common = species1_common1) %>%
  rename(species1_scientific1 = species2_scientific) %>%
  rename(species2_scientific = species1_scientific) %>%
  rename(species1_scientific = species1_scientific1)
inter_NA_full <- union(inter_NA, inter_NA_flip) #union() automatically deletes rows that are fully duplicated

nrow(inter_NA_full) #now we have 43532 rows, so 708 duplicated rows were removed during this process

#However, we might still have some rows where the same interaction is being represented but the
#rows aren't identical (different sources, etc). We will double-check this by comparing any rows that
#are equivalent for species1, interaction AND species2.
inter_NA_dup <- inter_NA_full %>%
  group_by(species1_scientific, species2_scientific, interaction) %>%
  filter(n() > 1) %>%
  arrange(species1_scientific, species2_scientific, interaction)

nrow(inter_NA_dup) #There are 22364 rows that are representing an identical iteraction to another row
#For example:
inter_NA_dup[1:2,]

#Both rows are representing co-occurrence between A. flammea and A. hornemanni, but from different sources.
#We need to remove duplicated interactions with the same species, keeping only one per unique species1
#species2 interaction combination
inter_NA_dedup <- inter_NA_full %>%
  distinct(species1_scientific, species2_scientific, interaction, .keep_all = TRUE)
nrow(inter_NA_dedup)



#Next, we simplify the dataframe for the visualization. We create a new data frame with one row per species,
#column "n_int" for the total number of unique interactions that that species participates in, and "n_type"
#for the total number of distinct types of interactions per species.
inter_NA_working <- inter_NA_dedup %>%
  group_by(species1_scientific) %>%
  mutate(species1_scientific = str_replace_all(species1_scientific, " ", "_")) %>%
  rename(species = species1_scientific) %>%
  summarize(n_int = n(), n_type = n_distinct(interaction), .groups = "drop")
head(inter_NA_working) #here is an example of what the data looks like now



#Next we create a list of species that are both in the tree and in our list
#this will remove global birds from the tree that we don't have data for
#and also remove non-specific taxa from our list (i.e. Corvid sp.). NOTE:
#This will also remove subspecies (Genus species subspecies). If we want
#to generalize these up to the species for representation in the tree, we
#will have to delete all subspecies earlier in the workflow.
species_tree_NA <- intersect(inter_NA_working$species, tree$tip.label)
inter_NA_working_prune <- inter_NA_working %>%
  filter(species %in% species_tree_NA)
tree_prune <- keep.tip(tree, species_tree_NA)

#This is using taxize to assign each species a family. It is a few years outdated
#so I will change this to reference the most recent clements soon. This won't cause
#many problems at the moment, since we are only using families to locate them on the
#circular phylogeny we produce.
classifications <- classification(inter_NA_working_prune$species, db = "ncbi") # takes a few minutes

#The following function will extract the family for each species
species_family_NA <- sapply(classifications, function(x) {
  if (is.data.frame(x)) {
    fam <- x[x$rank == "family", "name"]
    if (length(fam) > 0) fam else NA
  } else {
    NA
  }
})

#And we add it to the dataframe
inter_NA_working_prune$family <- species_family_NA

# -----------------------------------------------
# Filtering interaction data -- Full dataset
# -----------------------------------------------
#Now we repeat the same process, but without filtering for NA first

inter_flip <- inter %>%
  rename(species1_common1 = species2_common) %>%
  rename(species2_common = species1_common) %>%
  rename(species1_common = species1_common1) %>%
  rename(species1_scientific1 = species2_scientific) %>%
  rename(species2_scientific = species1_scientific) %>%
  rename(species1_scientific = species1_scientific1)
inter_full <- union(inter, inter_flip)

nrow(inter_full)


inter_dup <- inter_full %>%
  group_by(species1_scientific, species2_scientific, interaction) %>%
  filter(n() > 1) %>%
  arrange(species1_scientific, species2_scientific, interaction)



inter_dedup <- inter_full %>%
  distinct(species1_scientific, species2_scientific, interaction, .keep_all = TRUE)
nrow(inter_dedup)



inter_working <- inter_dedup %>%
  group_by(species1_scientific) %>%
  mutate(species1_scientific = str_replace_all(species1_scientific, " ", "_")) %>%
  rename(species = species1_scientific) %>%
  summarize(n_int = n(), n_type = n_distinct(interaction), .groups = "drop")
head(inter_working)


species_tree <- intersect(inter_working$species, tree$tip.label)
inter_working_prune <- inter_working %>%
  filter(species %in% species_tree)
tree_prune <- keep.tip(tree, species_tree)


classifications <- classification(inter_working_prune$species, db = "ncbi")

species_family <- sapply(classifications, function(x) {
  if (is.data.frame(x)) {
    fam <- x[x$rank == "family", "name"]
    if (length(fam) > 0) fam else NA
  } else {
    NA
  }
})

inter_working_prune$family <- species_family

# -----------------------------------------------
# Phylogenetic Visualization - NA
# -----------------------------------------------

#This section visualizes the data on the phylogenetic tree. There are two ways
#of coloring the species: tips and branches. This makes 4 plots: 2 each of tips
#and branches, for n_int and n_type

#TIPS COLORED
#color by n interactions
ggtree(tree_prune, layout = "circular") %<+% inter_NA_working_prune +
  geom_tippoint(mapping = aes(color = n_int), size = 1) +
  ggtitle("Phylogenetic tree of # of interactions") +
  scale_color_viridis_c(trans = "log10")
#color by n unique types
ggtree(tree_prune, layout = "circular") %<+% inter_NA_working_prune +
  geom_tippoint(mapping = aes(color = n_type), size = 1) +
  scale_color_viridis_c(trans = "log10")



#Branches COLORED
#color by n interactions
ggtree(tree_prune, layout = "circular") %<+% inter_NA_working_prune +
  geom_tree(mapping = aes(color = n_int)) +
  ggtitle("Phylogenetic tree of # of interactions - North America") +
  scale_color_gradientn(
    trans = "log10",
    colors = c("gold", "orange", "red", "darkred"))
#color by n unique types
ggtree(tree_prune, layout = "circular") %<+% inter_NA_working_prune +
  geom_tree(mapping = aes(color = n_type)) +
  ggtitle("Phylogenetic tree of # of types of interactions  - North America") +
  scale_color_gradientn(
    trans = "log10",
    colors = c("gold", "orange", "red", "darkred"))

#I will import the output of one of these plots into Inkscape, and add some
#aesthetics by labeling families of note and adding
#images/illustrations/silhouttes of some species

#The last step is to label one individual per family to be able to identify
#the family's location on the output tree, for aid in creating the final figure.

#first we need to rename "species" to "label" to get the tip labels to work
inter_NA_plot <- inter_NA_working_prune %>%
  rename(label = species)   #

#Randomly select one species to label the family
rep_df <- inter_NA_plot %>%
  filter(!is.na(family)) %>%
  group_by(family) %>%
  slice(1)

#Plot family labels on the tree!
ggtree(tree_prune, layout="circular") %<+% inter_NA_plot +
  geom_tiplab(aes(label = ifelse(label %in% rep_df$label, family, "")),
              size = 2.3, offset = 0.6, fontface = "bold") +
  ggtitle("North American Birds — Families Labelled at Representative Tips")


# -----------------------------------------------
# Phylogenetic Visualization -- Full dataset
# -----------------------------------------------
#Now we repeat the same plotting process, but without filtering for NA first

#TIPS COLORED
#color by n interactions
ggtree(tree_prune, layout = "circular") %<+% inter_working_prune +
  geom_tippoint(mapping = aes(color = n_int), size = 1) +
  ggtitle("Phylogenetic tree of # of interactions - Full dataset") +
  scale_color_viridis_c(trans = "log10")
#color by n unique types
ggtree(tree_prune, layout = "circular") %<+% inter_working_prune +
  geom_tippoint(mapping = aes(color = n_type), size = 1) +
  scale_color_viridis_c(trans = "log10")



#Branches COLORED
#color by n interactions
ggtree(tree_prune, layout = "circular") %<+% inter_working_prune +
  geom_tree(mapping = aes(color = n_int)) +
  ggtitle("Phylogenetic tree of # of interactions - Full dataset") +
  scale_color_gradientn(
    trans = "log10",
    colors = c("gold", "orange", "red", "darkred"))
#color by n unique types
ggtree(tree_prune, layout = "circular") %<+% inter_working_prune +
  geom_tree(mapping = aes(color = n_type)) +
  ggtitle("Phylogenetic tree of # of types of interactions - Full dataset") +
  scale_color_gradientn(
    trans = "log10",
    colors = c("gold", "orange", "red", "darkred"))


#Species label
inter_plot <- inter_working_prune %>%
  rename(label = species)   #

rep_df <- inter_plot %>%
  filter(!is.na(family)) %>%
  group_by(family) %>%
  slice(1)

ggtree(tree_prune, layout="circular") %<+% inter_plot +
  geom_tiplab(aes(label = ifelse(label %in% rep_df$label, family, "")),
              size = 2.3, offset = 0.6, fontface = "bold") +
  ggtitle("Full dataset — Families Labelled at Representative Tips")
