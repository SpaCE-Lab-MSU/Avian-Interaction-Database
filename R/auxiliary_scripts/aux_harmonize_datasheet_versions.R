# Harmonize data sheet versions

library(tidyverse)

file_info_path <- "./R/auxiliary_scripts/aux_files_with_schema.csv"
file_info <- read.csv(file_info_path)


load_schema <- function(df, schema_num) {
  temp <- df |> dplyr::filter(schema_name == schema_num)
  do.call(
    rbind,
    lapply(temp$file, function(f) {
      read.csv(f) |> dplyr::mutate(source_file = basename(f))
    })
  )
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
    select(-all_of(setdiff(old_cols, new_col)))
}

clean_na <- function(df) {
  df[] <- lapply(df, function(x) {
    if (is.character(x)) {
      x <- trimws(x)
      x[x %in% c("", "NA")] <- NA
    }
    x
  })
  df
}

has_content <- function(x) {
  x <- trimws(as.character(x))
  !is.na(x) & x != "" & x != "NA"
}

reshape_sources <- function(df, url_cols, notes_cols, id_cols = NULL) {
  stopifnot(length(url_cols) == length(notes_cols))

  if (is.null(id_cols)) {
    id_cols <- setdiff(names(df), c(url_cols, notes_cols))
  }

  df$.orig_row <- seq_len(nrow(df))

  pieces <- lapply(seq_along(url_cols), function(i) {
    u <- df[[url_cols[i]]]
    n <- df[[notes_cols[i]]]
    keep <- has_content(u) | has_content(n)

    out <- df[keep, id_cols, drop = FALSE]
    out$source_URL <- ifelse(has_content(u[keep]), u[keep], NA)
    out$text_excerpt <- ifelse(has_content(n[keep]), n[keep], NA)
    out$.orig_row <- df$.orig_row[keep]
    out
  })

  result <- do.call(rbind, pieces)
  result <- result[order(result$.orig_row), ]
  result$.orig_row <- NULL
  rownames(result) <- NULL
  result
}
############################################################################################

s_2 <- load_schema(file_info, "schema_2") |>
  mutate(name_changes = NA) |>
  clean_na()
s_5_2 <- load_schema(file_info, "schema_5") |>
  rbind(s_2) |>
  mutate(other_species1 = NA) |>
  clean_na()
s_1_5_2 <- load_schema(file_info, "schema_1") |>
  rbind(s_5_2) |>
  mutate(version = "v1", DatabaseSearchURL = "not_evaluated") |>
  clean_na()
s_4_1_5_2 <- load_schema(file_info, "schema_4") |>
  mutate(version = "v2") |>
  rbind(s_1_5_2) |>
  clean_na()

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
  concatenate_cols(OLDsource_cols, "Oldsource") |>
  concatenate_cols(c("Oldsource", "sourceA_URL"), "sourceA_URL") |>
  clean_na()

url_cols <- c("sourceA_URL", "sourceB_URL", "sourceC_URL", "sourceD_URL")
notes_cols <- c("notesA", "notesB", "notesC", "notesD")

s_8_6_4_1_5_2 <- load_schema(file_info, "schema_8") |>
  mutate(version = "v2") |>
  rbind(s_6_4_1_5_2) |>
  reshape_sources(url_cols, notes_cols) |>
  rename(breeding_migration = nonbreedingseason) |>
  mutate(
    interaction_strength = "not_evaluated",
    time_of_year = "not_evaluated",
    source_citation = "not_evaluated",
    species1_lifestage = "not_evaluated",
    species2_lifestage = "not_evaluated"
  ) |>
  clean_na()

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
  dplyr::select(-other_species1) |>
  clean_na()

s_7_9_8_6_4_1_5_2 <- load_schema(file_info, "schema_7") |>
  mutate(
    version = "v4",
    n_studies = "not_evaluated",
    BOW_evidence = "not_evaluated",
    breeding_migration = "not_evaluated"
  ) |>
  rbind(s_9_8_6_4_1_5_2) |>
  dplyr::select(
    -BOW_evidence,
    -n_studies,
    -name_changes,
    -DatabaseSearchURL,
    -breeding_migration
  ) |>
  clean_na()

# all(colnames(s_9_8_6_4_1_5_2) %in% colnames(s_7_9_8_6_4_1_5_2))
# colnames(s_9_8_6_4_1_5_2)[which(
#   !(colnames(s_9_8_6_4_1_5_2) %in% colnames(s_7_9_8_6_4_1_5_2))
# )]
# colnames(s_7_9_8_6_4_1_5_2)[which(
#   !(colnames(s_7_9_8_6_4_1_5_2) %in% colnames(s_9_8_6_4_1_5_2))
# )]
