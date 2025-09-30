# L1_taxonomy_functions.R
# TITLE:          Functions to handle taxonomic processing for L0 to L1 cleaning
# AUTHORS:        Phoebe Zarnetske, Patrick Bills
# COLLABORATORS:  Vincent Miele, Stephane Dray
# PROJECT:        Avian Interaction Database & Avian Meta-Network
# DATE:           20 Mar 2023 - August 2025
# NOTES:
#   Functions here are used in R/L0/L0_stitch.qmd notebook
#



# load libraries use here but don't show loading messages so they
# don't pollute the Quarto and Rmarkdown notebooks that source this file
suppressMessages({
  library(taxadb)
  library(stringdist)
})


#' read in the interactions file
read_interactions <- function(filename = "AvianInteractionData_L0.csv"){
  int.raw<-read.csv(file.path(L0_dir,filename))
  return(int.raw)
}


# Function to clean names for comparison (ignore sp., Unid., remove whitespace
# after name)
clean_name <- function(name) {
  name %>%
    gsub("\\b(unid\\.|sp\\.)\\b", "", .) %>%   # Remove "Unid." and "sp."
    trimws()                                   # Trim extra spaces
}

#' fuzzy taxonomy match
#' Use fuzzy logic function to find closest match from the CHECKLIST reference list
#' Function to find the closest match with a similarity score, and ignoring the
#' name aspects above.
#' @param name a species binomial aka scientific name
#' @param reference list = data frame with columns "genus_species", "common_name"
#' @returns two item list,
find_closest_match_with_score <- function(name, reference_list) {
  if (is.na(name) || name == "") {
    return(list(match = NA_character_, score = NA_real_))
  }

  # use cleaned, un-cased names to improve matching
  name_cleaned <- tolower(clean_name(name))
  reference_cleaned <- tolower(clean_name(reference_list)) # Cleaned for comparison only

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




#### functions to manage taxonomy adjustments and comments

# the L0 to L1 process creates a table of taxonomic names that appear in the
# data as entered and require fix for typo or update to match
#

#' Create list of unique scientific_name.raw and common_name.raw pairs with
#' formatting adjustments, stacking and collecting anything in sp1 and sp2.
#'
#' columns with ".raw" are original from data-entry to L0 process
#' columns with ".edit" are changed versions,
#' and .edit names have some cleaning applied
#'    removes blank spaces, capitalizes, and keeps unique rows.
#'
create_names_edit_table <-function(intxns.df) {
  int.raw.names <- intxns.df %>%
    # Select and stack the relevant species columns
    select(species1_scientific, species1_common, species2_scientific, species2_common) %>%
    transmute(
      scientific_name.raw = species1_scientific,  # Original scientific_name
      common_name.raw = species1_common
    ) %>%
    bind_rows(
      int.raw %>%
        transmute(
          scientific_name.raw = species2_scientific,  # Original scientific_name
          common_name.raw = species2_common
        )
    ) %>%
    # Remove duplicates and clean up formatting
    distinct() %>%
    mutate(
      # Clean scientific_name while keeping the raw version
      scientific_name.edit = str_trim(scientific_name.raw),  # Start with the raw data
      scientific_name.edit = ifelse(
        str_starts(scientific_name.edit, "unid."),  # Exception case
        str_replace(scientific_name.edit, "(unid.)s*(w+)", "1 U2"),
        str_to_sentence(scientific_name.edit)       # Regular case
      ),
      scientific_name.edit = str_replace(scientific_name.edit, "spp.", "sp."),  # Replace "spp." with "sp."

      # Clean common_name
      common_name.edit = str_trim(common_name.raw),  # Start with the raw data
      common_name.edit = str_to_title(common_name.edit),  # Capitalize each word
      common_name.edit = str_replace_all(common_name.edit, "unid", "unid.")  # Add period to "unid"
    ) %>%
    filter(!is.na(scientific_name.edit)) # Remove rows where scientific_name.edit is <NA>


    # Add blank column to add notes to track dispensation

    int.raw.names$edit_notes <- NA
    return(int.raw.names)
}




#' update name edits table
#'
#' consistent function for add notes and keepting track of how and why
#' a scientific name was updated or fixed
#'
#' @param edits.df the table of edits, aka int.raw.names
#' @param scientific_name.raw required name to lookup in th in the raw table
add_name_edits<-function(edits.df, scientific_name.raw,
                         edit_notes,
                         scientific_name.edit=NULL,
                         common_name.edit=NULL) {

  if(nrow(edits.df[edits.df$scientific_name.raw == scientific_name.raw, ]) == 0){
    warning(paste(scientific_name.raw,  "not found in names list, no edits"))
    return(edits.df)
  }

  edits.df[edits.df$scientific_name.raw == scientific_name.raw,]$edit_notes <- edit_notes
  if(! is.null(scientific_name.raw)) {
    edits.df[edits.df$scientific_name.raw == scientific_name.raw,]$scientific_name.edit <- scientific_name.edit
  }
  if(! is.null(common_name.edit)){
    edits.df[edits.df$scientific_name.raw == scientific_name.raw,]$common_name.edit <- common_name.edit
  }
  return(edits.df)
}

#' add taxonomy edit matching common name
#'
#' this finds rows in the names table by matching common name,
#' looks up the scientific name and then calls the add_name_edits function
#' above for consistency.  IF the suggestion scientific name edit
#'
add_name_edits_by_common_name<- function(edits.df, common_name.raw,
                              edit_notes,
                              scientific_name.edit,
                              quiet=FALSE) {
  matching_rows <- edits.df[edits.df$common_name.raw == common_name.raw, ]

  if(nrow(matching_rows) == 0){
    warning(paste(common_name.raw,  "not found in names list, no edits"))
    return(edits.df)
  }
  # there could be more than on matching common name, but edit all occuring
  # sci names for those IF the edited name is different
  # if the table already matches the suggested edit, don't overwrite

  for(r in 1:nrow(matching_rows)){
    matching_scientific_name.raw <- matching_rows[r,]$scientific_name.raw
    matching_scientific_name.edit <- matching_rows[r,]$scientific_name.edit
    if (matching_scientific_name.edit != scientific_name.edit){
      edits.df <- add_name_edits(edits.df,
                  matching_scientific_name.raw, edit_notes, scientific_name.edit
                  )
    } else { #do nothing but warn
      if(!quiet){
        warning(paste(common_name.raw, " entry already has matching edit for ,",
                      scientific_name.edit, "...leaving as-as")
                )
      }

    }
  } # end loop


  return(edits.df)
}


#' add taxonomy edit matching common name

#' given a list of edits in a data frame with minimally the
#' scientific_name.raw to match and an edit text, apply the function
#' add_name_edits above.  This is in a loop to
add_edits_to_list <- function(edits.df, edit_list){
  # maybe should use apply but this also works
  for(e in edit_list ){
    # ensure all args have something
    # this is not the most R way to do this
    if(length(e)<3) {append(e,"NULL")}
    if(length(e)<4) {append(e,"NULL")}

    edits.df <- add_name_edits(edits.df,
                               scientific_name.raw = e[1],
                               edit_notes = e[2],
                               scientific_name.edit = e[3],
                               common_name.edit = e[4] )
  }
  return(edits.df)
}





#######################################################
#### short-cut/convenience functions for listing or counting records by species

# list those without a note AND are not UNIDs Genus sp.
unresolved_species<- function(edit.df){
  filter(edit.df,
         is.na(edit_notes) & !grepl(" sp\\.$", scientific_name.edit)
         )
}


#' interaction records where sp in sp1 OR sp2
#'
#' convenience function to help remember this syntax,
#' return only records where scientific name matches exactly
#' @param intxns.df data frame in same format as Avian Interaction  database
#' @param scientific_name name to find
#' @returns data frame of matching records
intxns_by_species<- function(intxns.df, scientific_name){
  return(filter(intxns.df, species1_scientific == scientific_name | species2_scientific == scientific_name))
}


#' count of interactions by scientific name
#'
#' convenience function, call the above lookup function and returns count of records
interaction_count_by_species<- function(intxns.df, scientific_name){
  rex = filter(intxns.df, species1_scientific == scientific_name | species2_scientific == scientific_name)
  return(nrow(rex))

}


#' count interactions by partial match (grep)
interaction_records_by_match<- function(intxns.df, scientific_name_fragment){
  rex = filter(intxns.df, grepl(scientific_name_fragment, species1_scientific) | grepl(scientific_name_fragment,species2_scientific))
  return(rex)
}

#' count interactions by partial match (grep)
interaction_count_by_match<- function(intxns.df, scientific_name_fragment){
  return(nrow(interaction_records_by_match(intxns.df, scientific_name_fragment)))
}

#' subset of interactions by partial match common name sp1 or sp2
interaction_records_by_match_common<- function(intxns.df, common_name_fragment){
  rex = filter(intxns.df,
               grepl(common_name_fragment, species1_common, ignore.case = TRUE) |
                 grepl(common_name_fragment,species2_common,ignore.case = TRUE))

  return(rex)
}

interaction_count_by_match_common<- function(intxns.df, common_name_fragment){
  rex = interaction_records_by_match_common(intxns.df, common_name_fragment)
  return(nrow(rex))
}

