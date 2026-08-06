# Harmonize data sheet versions

library(tidyverse)

file_info_path <- "./R/auxiliary_scripts/aux_files_with_schema.csv"
file_info <- read.csv(file_info_path)


load_schema <- function(df, schema_num) {
  temp <- df |> dplyr::filter(schema_name == schema_num)
  return(do.call(rbind, lapply(temp$file, read.csv)))
}

concatenate_cols <- function(df, old_cols, new_col) {
  df |>
    rowwise() |>
    mutate(
      !!new_col := c_across(all_of(old_cols)) |>
        keep(~ !is.na(.x) & str_trim(.x) != "") |>
        unique() |>
        str_c(collapse = "; ")
    ) |>
    ungroup() |>
    select(-all_of(old_cols))
}

s_2 <- load_schema(file_info, "schema_2") |> mutate(name_changes = NA)
s_5_2 <- load_schema(file_info, "schema_5") |>
  rbind(s_2) |>
  mutate(other_species1 = NA)
s_1_5_2 <- load_schema(file_info, "schema_1") |>
  rbind(s_5_2) |>
  mutate(version = "v1", DatabaseSearchURL = "not_evaluated")
s_4_1_5_2 <- load_schema(file_info, "schema_4") |>
  mutate(version = "v2") |>
  rbind(s_1_5_2)

OLDsource_cols <- c("OLDsourceA", "OLDsourceB")

s_6_4_1_5_2 <- load_schema(file_info, "schema_6") |>
  mutate(version = "v2") |>
  rename(DatabaseSearchURL = GoogleScholarURL) |>
  rbind(s_4_1_5_2) |>
  rename(
    sourceA_URL = sourceAupdatedURL,
    sourceB_URL = sourceBupdatedURL,
    sourceC_URL = sourceCupdatedURL,
    sourceD_URL = sourceDupdatedURL
  ) |>
  concatenate_cols(OLDsource_cols, "sourceA_URL")

url_cols <- c("sourceA_URL", "sourceB_URL", "sourceC_URL", "sourceD_URL")
notes_cols <- c("notesA", "notesB", "notesC", "notesD")

s_8_6_4_1_5_2 <- load_schema(file_info, "schema_8") |>
  mutate(version = "v2") |>
  rbind(s_6_4_1_5_2) |>
  concatenate_cols(url_cols, "source_URL") |>
  concatenate_cols(notes_cols, "text_excerpt") |>
  rename(breeding_migration = nonbreedingseason) |>
  mutate(
    interaction_strength = "not_evaluated",
    time_of_year = "not_evaluated",
    source_citation = "not_evaluated",
    species1_lifestage = "not_evaluated",
    species2_lifestage = "not_evaluated"
  )

s_9_8_6_4_1_5_2 <- load_schema(file_info, "schema_9") |>
  mutate(
    version = "v3",
    n_studies = "not_evaluated",
    BOW_evidence = "not_evaluated"
  ) |>
  rename(source_citation = Citation) |>
  rbind(s_8_6_4_1_5_2) |>
  rename(
    effect_tx1_on_tx2 = effect_sp1_on_sp2,
    effect_tx2_on_tx1 = effect_sp2_on_sp1,
    taxa1_common = species1_common,
    taxa2_common = species2_common,
    taxa1_scientific = species1_scientific,
    taxa2_scientific = species2_scientific,
    taxa1_lifestage = species1_lifestage,
    taxa2_lifestage = species2_lifestage,
    interaction_excerpt = text_excerpt
  ) |>
  mutate(
    tx1_life_history_season = "not_evaluated",
    tx2_life_history_season = "not_evaluated",
    country = "not_evaluated",
    location = "not_evaluated",
    timing_location_excerpt = "not_evaluated",
    year = "not_evaluated",
  ) |>
  dplyr::select(-other_species1)

s_7_9_8_6_4_1_5_2 <- load_schema(file_info, "schema_7") |>
  mutate(
    version = "v4",
    n_studies = "not_evaluated",
    BOW_evidence = "not_evaluated",
    breeding_migration = "not_evaluated"
  ) |>
  rbind(s_9_8_6_4_1_5_2) |>
  dplyr::select(-BOW_evidence, -n_studies)

# all(colnames(s_9_8_6_4_1_5_2) %in% colnames(s_7_9_8_6_4_1_5_2))
# colnames(s_9_8_6_4_1_5_2)[which(
#   !(colnames(s_9_8_6_4_1_5_2) %in% colnames(s_7_9_8_6_4_1_5_2))
# )]
# colnames(s_7_9_8_6_4_1_5_2)[which(
#   !(colnames(s_7_9_8_6_4_1_5_2) %in% colnames(s_9_8_6_4_1_5_2))
# )]
