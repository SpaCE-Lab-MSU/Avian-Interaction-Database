suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(purrr)
})

# ---- helper predicates ----
is_initial_chunk <- function(x) {
  # initials are usually 2–3 letters, sometimes with hyphen; keep it strict-ish
  str_detect(x, "^[A-Za-z]{2,3}$")
}

# ---- main parser ----
parse_species_filenames <- function(files) {
  tibble(file = files) %>%
    mutate(
      # keep only basename, drop extension
      base = basename(file),
      stem = str_remove(base, "\\.csv$"),
      parts = str_split(stem, "_", simplify = FALSE)
    ) %>%
    mutate(
      n_parts = map_int(parts, length),
      genus  = map_chr(parts, ~ .x[1] %||% NA_character_),
      species = map_chr(parts, ~ .x[2] %||% NA_character_),
      tail = map(parts, ~ if (length(.x) >= 3) .x[3:length(.x)] else character(0))
    ) %>%
    mutate(
      # split tail into initials vs "name-ish" chunks
      tail_is_init = map(tail, ~ is_initial_chunk(.x)),
      initials_chunks = map2(tail, tail_is_init, ~ .x[.y]),
      noninit_chunks  = map2(tail, tail_is_init, ~ .x[!.y]),

      # subspecies is whatever non-initial chunk(s) exist (often length 0 or 1)
      subspecies = map_chr(noninit_chunks, ~ if (length(.x) == 0) NA_character_ else paste(.x, collapse = "_")),

      # initials may be 1–2 chunks; store both raw and collapsed
      initials = map_chr(initials_chunks, ~ if (length(.x) == 0) NA_character_ else paste(tolower(.x), collapse = "_"))
    ) %>%
    mutate(
      # basic sanity flags
      has_genus_species = !is.na(genus) & !is.na(species),

      pattern = case_when(
        !has_genus_species ~ "unparseable_missing_genus_or_species",
        is.na(subspecies) & is.na(initials) ~ "genus_species",
        !is.na(subspecies) & is.na(initials) ~ "genus_species_subspecies",
        is.na(subspecies) & !is.na(initials) ~ "genus_species_initials",
        !is.na(subspecies) & !is.na(initials) ~ "genus_species_subspecies_initials",
        TRUE ~ "other"
      ),

      genus = str_to_sentence(genus),   # optional normalization
      species = str_to_lower(species),
      subspecies = if_else(!is.na(subspecies), str_to_lower(subspecies), subspecies)
    ) %>%
    mutate(
      genus_species = if_else(has_genus_species, paste(genus, species, sep = "_"), NA_character_),
      genus_species_subspecies = if_else(has_genus_species,
                                         paste(genus, species, subspecies, sep = "_"),
                                         NA_character_)
    ) %>%
    select(file, base, stem, parts, genus, species, subspecies, initials, pattern, genus_species, genus_species_subspecies)
}

# ---- analysis helpers ----

summarize_duplicates <- function(parsed_df) {

  # A) genus+species that appear as BOTH species-only and subspecies variants
  species_vs_subspecies <- parsed_df %>%
    filter(!is.na(genus_species)) %>%
    group_by(genus_species) %>%
    summarise(
      n_files = n(),
      n_species_only = sum(pattern %in% c("genus_species", "genus_species_initials")),
      n_with_subspecies = sum(pattern %in% c("genus_species_subspecies", "genus_species_subspecies_initials")),
      example_files = paste(head(base, 5), collapse = "; "),
      .groups = "drop"
    ) %>%
    filter(n_species_only > 0, n_with_subspecies > 0) %>%
    arrange(desc(n_files), genus_species)

  # B) same base taxon (genus+species+subspecies OR genus+species if no subspecies)
  #    that has duplicates due to initials differences
  initials_duplicates <- parsed_df %>%
    filter(!is.na(genus_species)) %>%
    mutate(
      base_taxon = if_else(!is.na(subspecies),
                           paste(genus, species, subspecies, sep = "_"),
                           paste(genus, species, sep = "_"))
    ) %>%
    group_by(base_taxon) %>%
    summarise(
      n_files = n(),
      n_distinct_initials = n_distinct(initials, na.rm = TRUE),
      has_any_initials = any(!is.na(initials)),
      n_no_initials = sum(is.na(initials)),
      initials_seen = paste(sort(unique(na.omit(initials))), collapse = ", "),
      example_files = paste(head(base, 5), collapse = "; "),
      .groups = "drop"
    ) %>%
    filter(n_files > 1) %>%
    # keep ones where duplication plausibly comes from initials:
    filter(has_any_initials, n_distinct_initials >= 2 | n_no_initials >= 1) %>%
    arrange(desc(n_files), desc(n_distinct_initials), base_taxon)

  # C) overall counts by pattern
  pattern_counts <- parsed_df %>%
    count(pattern, sort = TRUE)

  list(
    pattern_counts = pattern_counts,
    species_vs_subspecies = species_vs_subspecies,
    initials_duplicates = initials_duplicates
  )
}

# ---- Example usage ----
# files <- c("Panthera_leo.csv", "Panthera_leo_krugeri.csv", "Panthera_leo_ih_ep.csv", "Panthera_leo_krugeri_ih.csv")
parsed <- parse_species_filenames(checked_file_list)
out <- summarize_duplicates(parsed)
out$pattern_counts
print(out$species_vs_subspecies, n= Inf)
print(out$initials_duplicates, n= Inf)





###############################################################################
get_flagged_file_paths <- function(parsed_df, out) {

  # rebuild base_taxon consistently
  df <- parsed_df %>%
    mutate(
      base_taxon = if_else(
        !is.na(subspecies),
        paste(genus, species, subspecies, sep = "_"),
        paste(genus, species, sep = "_")
      )
    )

  # ---- species vs subspecies ----
  species_vs_subspecies_paths <- df %>%
    filter(genus_species %in% out$species_vs_subspecies$genus_species) %>%
    group_by(genus_species) %>%
    summarise(files = list(file), .groups = "drop")

  # ---- initials duplicates ----
  initials_duplicate_paths <- df %>%
    filter(base_taxon %in% out$initials_duplicates$base_taxon) %>%
    group_by(base_taxon) %>%
    summarise(files = list(file), .groups = "drop")

  list(
    species_vs_subspecies = setNames(
      species_vs_subspecies_paths$files,
      species_vs_subspecies_paths$genus_species
    ),
    initials_duplicates = setNames(
      initials_duplicate_paths$files,
      initials_duplicate_paths$base_taxon
    )
  )
}
flagged_files <- get_flagged_file_paths(parsed, out)

test_files <- flagged_files$species_vs_subspecies$Cardellina_pusilla
