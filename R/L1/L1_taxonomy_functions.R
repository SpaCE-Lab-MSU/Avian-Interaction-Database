require(stringdist)

read_checklist <- function(){


}

#' read in the interactions file
read_interactions <- function(filename = "AvianInteractionData_L0.csv"){
  int.raw<-read.csv(file.path(L0_dir,filename))
  return(int.raw)
}



# Reference list of scientific names from eBird Clements CHECKLIST 2024
# reference_names <- tibble(
#   genus_species = checklist$genus_species,
#   common_name = checklist$common_name
# )

# Function to clean names for comparison (ignore sp., Unid., remove whitespace
# after name)
clean_name <- function(name) {
  name %>%
    gsub("\\b(unid\\.|sp\\.)\\b", "", .) %>%   # Remove "Unid." and "sp."
    trimws()                                   # Trim extra spaces
}

# Use fuzzy logic function to find closest match from the CHECKLIST reference list
# Function to find the closest match with a similarity score, and ignoring the
# name aspects above.
# reference list = data frame with colums "genus_species", "common_name"

find_closest_match_with_score <- function(name, reference_list) {
  if (is.na(name) || name == "") {
    return(list(match = NA_character_, score = NA_real_))
  }
  name_cleaned <- clean_name(name)
  reference_cleaned <- clean_name(reference_list) # Cleaned for comparison only
  # Calculate string distances
  distances <- stringdist::stringdist(name_cleaned, reference_cleaned, method = "jw") # Jaro-Winkler distance
  if (length(distances) == 0 || all(is.na(distances)) || min(distances, na.rm = TRUE) > 0.5) {
    return(list(match = NA_character_, score = NA_real_))
  }
  closest_match_index <- which.min(distances)
  closest_match <- reference_list[closest_match_index]
  similarity_score <- 1 - distances[closest_match_index]
  return(list(match = closest_match, score = similarity_score)) # Convert distance to similarity (1 = exact match)
}
