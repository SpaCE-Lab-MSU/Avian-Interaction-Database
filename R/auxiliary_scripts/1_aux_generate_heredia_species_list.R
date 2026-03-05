
# Specify functions
read_avibase <- function(region_code, url, L0_dir) {
  message("Processing region: ", region_code)

  # Read HTML and extract first table
  tab <- url %>%
    read_html() %>%
    html_table(fill = TRUE) %>%
    .[[1]]

  # Rename expected columns
  tab <- tab %>%
    rename(
      common_name     = X1,
      scientific_name = X2,
      status          = X3
    ) %>%
    # Extract order/family headings
    mutate(
      order  = str_extract(common_name, "\\b[A-Z]+(?: [A-Z]+)*(?=: )"),
      family = str_extract(common_name, "(?<=: )[A-Z][a-z]+")
    ) %>%
    # Carry order/family down through species rows
    fill(order, family) %>%
    filter(!(common_name == paste(order, family, sep = ": "))) %>%
    mutate(region = region_code) %>%
    # Trim whitespace just in case
    mutate(across(c(common_name, scientific_name, status), ~ str_squish(.)))

  # Save raw cleaned table for that region
  out_file <- file.path(L0_dir, paste0("avibase8.17_", region_code, ".csv"))
  write_csv(tab, out_file)

  return(tab)
}

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
t_done <- t[(t$scientific_name %in% spp$scientific_name),]
t_done_amn <- t[(t$scientific_name %in% amn_spp$scientific.name),]
t_done_cac <- t[(t$scientific_name %in% cac_spp$scientific_name_clements2024),]

t_new <- t[!(t$scientific_name %in% spp$scientific_name),]
t_new_amn <- t[!(t$scientific_name %in% amn_spp$scientific.name),]
t_new_cac <- t[!(t$scientific_name %in% cac_spp$scientific_name_clements2024),]

# 155 species in the Heredia Avibase list that are also
# in the Canada-AK-CONUS Avibase list are not in the final
# AvianMetaNetwork
in_ls_not_amn <- setdiff(t_new_amn, t_new)

write.csv(in_ls_not_amn, "C:/Users/Kelly Kapsar/Downloads/temp_spp_in_avibase_cac_but_not_amn.csv")



