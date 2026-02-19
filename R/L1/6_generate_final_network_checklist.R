
# TITLE:          AvianMetaNetwork: North American Taxa List
# AUTHORS:        Phoebe Zarnetske, Kelly Kapsar
# COLLABORATORS:  Vincent Miele, Stephane Dray, Emily Parker, Pat Bills
# DESCRIPTION:    Generates a complete list of all of the taxa in the North American
#                 AvianMetaNetwork, including complete Clements name information,
#                 and an indicator column for whether the taxa is a N. American taxa
#                 (i.e., Canada, AK, CONUS).
# DATA INPUT:     eBird-Clements-v2024-integrated-checklist-October-2024-rev.csv
#                 spp_joint_cac.csv
#                 amn_cac.csv
# DATA OUTPUT:    spp_clem_in_amn_cac.csv
# PROJECT:        AvianMetaNet & avian-meta-network
# DATE:           17 January 2022 - 19 February 2026


# Load shared helper functions used across the project
source(here::here('R/lib/shared_functions.R'))

# Get standardized file paths for the project data directories
file_paths <- get_file_paths()

# Read the official eBird/Clements taxonomy checklist (L0 reference)
clem <- readr::read_csv(
  file.path(
    file_paths$CHECKLIST_L0,
    "eBird-Clements-v2024-integrated-checklist-October-2024-rev.csv"
  )
)

# Read the CAC species checklist subset (L1)
spp_cac <- readr::read_csv(
  file.path(
    file_paths$CHECKLIST_L1,
    "spp_joint_cac.csv"
  )
)

# Read the avian interaction network (amn) dataset
amn <- readr::read_csv(
  file.path(
    file_paths$L1,
    "amn_cac.csv"
  )
)

# Extract all unique Clements names used in the interaction data
# (from both taxa1 and taxa2 columns)
amn_clem_names <- unique(c(amn$taxa1_clements, amn$taxa2_clements))

# Display the actual names that are missing from the Clements checklist
amn_clem_names[which(!(amn_clem_names %in% clem$scientific.name))]

# Subset the Clements checklist to only taxa that appear in the amn dataset
clem_for_amn_spp <- clem[clem$scientific.name %in% amn_clem_names, ]

# Add a logical flag indicating whether each taxon occurs in
# the CAC species checklist (Canada / Alaska / CONUS)
clem_for_amn_spp$canada_ak_conus <- ifelse(
  clem_for_amn_spp$scientific.name %in% spp_cac$scientific_name_clements2024,
  TRUE,
  FALSE
)

# Ensure all names in amn are also in network and vice versa
which(!(amn_clem_names %in% clem$scientific.name))
which(!(clem_for_amn_spp$scientific.name %in% amn_clem_names))

sum(amn_clem_names %in% clem$scientific.name) == length(amn_clem_names)
sum(clem_for_amn_spp$scientific.name %in% amn_clem_names) == length(clem_for_amn_spp$scientific.name)

# Write the filtered and annotated Clements checklist to disk
write.csv(
  clem_for_amn_spp,
  here::here("./data/L1/species_checklists/spp_clem_in_amn_cac.csv")
)
