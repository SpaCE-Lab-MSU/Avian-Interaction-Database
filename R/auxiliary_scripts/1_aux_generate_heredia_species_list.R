source("./R/auxiliary_scripts/aux_scrape_avibase.R")

# Download Heredia species checklist
her = "https://avibase.bsc-eoc.org/checklist.jsp?lang=EN&p2=1&list=clements&region=crhe&version=text"
t <- read_avibase(region_code = "CRhe", url = her, L0_dir = "./data/L0")

# Save raw checklist
write.csv(t, "./data/L0/species_checklists/spp_avibase_heredia_2025.csv")

# Load in Canada/AK/CONUS Avibase species list and AvianMetaNetwork NA species list
spp <- read.csv("./data/L0/species_checklists/spp_avibase_cac_2024.csv")
# amn_spp <- read.csv("./data/L1/species_checklists/spp_clem_in_amn_cac.csv") %>% filter(canada_ak_conus == TRUE)
cac_spp <- read.csv("./data/L1/species_checklists/spp_joint_cac.csv")

# Compare overlap between the three lists
t_done <- t[(t$scientific_name %in% spp$scientific_name), ]
t_done_amn <- t[(t$scientific_name %in% amn_spp$scientific.name), ]
t_done_cac <- t[(t$scientific_name %in% cac_spp$scientific_name_clements2024), ]

t_new <- t[!(t$scientific_name %in% spp$scientific_name), ]
t_new_amn <- t[!(t$scientific_name %in% amn_spp$scientific.name), ]
t_new_cac <- t[!(t$scientific_name %in% cac_spp$scientific_name_clements2024), ]

# 155 species in the Heredia Avibase list that are also
# in the Canada-AK-CONUS Avibase list are not in the final
# AvianMetaNetwork
in_ls_not_amn <- setdiff(t_new_amn, t_new)

write.csv(
  in_ls_not_amn,
  "C:/Users/Kelly Kapsar/Downloads/temp_spp_in_avibase_cac_but_not_amn.csv"
)
