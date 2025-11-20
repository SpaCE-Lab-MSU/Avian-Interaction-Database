# Now join the updated BBS list with the official eBird/Clements 2024 checklist 
# to check for name changes that haven't been updated in the BBS list:

splist.gs<-subset(splist,select=c("genus_species","English_Common_Name"))
splist.gs$genus_species1<-splist.gs$genus_species

# Full join on "genus_species" to ensure all rows are kept and find differing columns
# Step 1: Full join on genus_species and keep English_Common_Name with suffixes
combined_data <- splist.gs %>%
  full_join(checklist, by = "genus_species", suffix = c(".BBS", ".checklist"))

# Step 2: Identify rows where English_Common_Name does not match or is missing
combined_differences <- combined_data %>%
  mutate(
    English_Common_Name_diff = if_else(
      is.na(English_Common_Name.BBS) | is.na(English_Common_Name.checklist) | 
        English_Common_Name.BBS != English_Common_Name.checklist,
      TRUE,
      FALSE
    )
  ) %>%
  # Step 3: Filter rows where there are differences in English_Common_Name or unique genus_species
  filter(English_Common_Name_diff | is.na(English_Common_Name.BBS) | is.na(English_Common_Name.checklist)) %>%
  # Select relevant columns for output
  select(genus_species, English_Common_Name.BBS, English_Common_Name.checklist)

# Optional: Export to CSV if needed
#write.csv(combined_differences, "genus_species_name_differences.csv", row.names = FALSE, fileEncoding = "UTF-8")

# Unnest the differing values for readability
non_blank_differences <- differences %>%
  unnest_wider(differing_values, names_sep = "_diff")

# Identify unique rows in 2023 data that do not exist in 2024
unique_2023_only <- bbs.splist23 %>%
  anti_join(bbs.splist24, by = "AOU")

# Identify unique rows in 2024 data that do not exist in 2023
unique_2024_only <- bbs.splist24 %>%
  anti_join(bbs.splist23, by = "AOU")

# Combine the unique rows from both years with non-blank differences
combined_differences <- bind_rows(
  non_blank_differences,
  unique_2023_only %>% mutate(note = "Only in 2023"),
  unique_2024_only %>% mutate(note = "Only in 2024")
)

# Print only the first 6 columns of the combined differences
print(combined_differences %>% select(1:3), n = Inf)


