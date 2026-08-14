# This script takes a taxonomically ordered species list
# and reorders the species to maximize diversity.

rm(list=ls())
library(tidyverse)
list <- read.csv("data/birdlist.csv")


# TRIMMING: may take some manual tweaking based on column names differing between lists
# basically we only need the taxonomic columns.
trimmedlist <- list %>% select(common_name, scientific_name, order, family)

# SCI NAME EXPANSION: splitting genus from species, maybe we separate these in future lists

trimmedlist$genus <- word(trimmedlist$scientific_name, 1)
head(trimmedlist$genus)


# SAMPLING TO REORDER
# ORDER: One random species per order, unless order is bigger than 10sp in list,
# in which case 2 are sampled. We can tweak these numbers a bit!
orders <- trimmedlist %>%
  add_count(order, name = "n_in_order")

big_orders <- orders %>%
  filter(n_in_order > 10) %>%
  group_by(order) %>%
  slice_sample(n = 2) %>%
  ungroup()

small_orders <- orders %>%
  filter(n_in_order <= 10) %>%
  group_by(order) %>%
  slice_sample(n = 1) %>%
  ungroup()

order_sample <- bind_rows(big_orders, small_orders) %>%
  select(-n_in_order)

# Randomize order of selected rows (otherwise alphabetical)
order_sample <- order_sample[sample(1:nrow(order_sample)), ]

# Remove those rows from the pool
remaining1 <- trimmedlist %>%
  anti_join(order_sample, by = "scientific_name")

# FAMILY: One random species per family, unless family is bigger than 10sp in list,
# in which case 2 are sampled. We can tweak these numbers a bit!
families <- remaining1 %>%
  add_count(family, name = "n_in_family")

big_families <- families %>%
  filter(n_in_family > 10) %>%
  group_by(family) %>%
  slice_sample(n = 2) %>%
  ungroup()

small_families <- families %>%
  filter(n_in_family <= 10) %>%
  group_by(family) %>%
  slice_sample(n = 1) %>%
  ungroup()

family_sample <- bind_rows(big_families, small_families) %>%
  select(-n_in_family)

# Randomize order of selected rows (otherwise alphabetical)
family_sample <- family_sample[sample(1:nrow(family_sample)), ]

# Remove those rows from the pool
remaining2 <- remaining1 %>%
  anti_join(family_sample, by = "scientific_name")

# One random species per GENUS
genera <- remaining2 %>%
  add_count(genus, name = "n_in_genus")

big_genera <- genera %>%
  filter(n_in_genus > 10) %>%
  group_by(genus) %>%
  slice_sample(n = 2) %>%
  ungroup()

small_genera <- genera %>%
  filter(n_in_genus <= 10) %>%
  group_by(genus) %>%
  slice_sample(n = 1) %>%
  ungroup()

genus_sample <- bind_rows(big_genera, small_genera) %>%
  select(-n_in_genus)

# Randomize order of selected rows (otherwise alphabetical)
genus_sample <- genus_sample[sample(1:nrow(genus_sample)), ]

# Remove those rows from the pool
remaining3 <- remaining2 %>%
  anti_join(genus_sample, by = "scientific_name")

# Randomize order of selected rows (otherwise alphabetical)
remaining3 <- remaining3[sample(1:nrow(remaining3)), ]

# Combine in hierarchical order
final_ordered <- bind_rows(
  order_sample,
  family_sample,
  genus_sample,
  remaining3
)

