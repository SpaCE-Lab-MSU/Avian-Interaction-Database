library(dplyr)
library(rvest)
library(purrr)
library(readr)
library(stringr)
library(readxl)

#Convert any .xlsx files to .csv function
convert_xlsx_to_csv <- function(input_dir, output_dir) {
  #Locate and load files of the .xlsx file type
  xlsx_files <- list.files(path = input_dir, pattern = "\\.xlsx$", full.names = TRUE)
  
  for (file in xlsx_files) {
    message("Converting: ", file)
    sheets <- excel_sheets(file)
    data <- read_excel(file, sheet = sheets[1])
    output_file <- file.path(output_dir, paste0(tools::file_path_sans_ext(basename(file)), ".csv"))
    write_csv(data, output_file)
    message("Saved CSV: ", output_file)
  }
}

#BOW Citation scraping function
get_bow_citation <- function(url) {
  if (is.na(url)) return(NA_character_)
  tryCatch({
    page <- read_html(url)
    citation <- page %>%
      html_nodes("cite.u-text-2-loose") %>%  #Citation key in HTML on all BOW pages
      html_text(trim = TRUE)
    if (length(citation) == 0) return(NA_character_)
    return(citation)
  }, error = function(e) NA_character_)
}

#CSV Processing Function: Generates citation column
process_and_resave <- function(file_path, output_dir = NULL) {
  message("Processing: ", file_path)
  data <- read_csv(file_path, show_col_types = FALSE)
  
  if (!"sourceAupdatedURL" %in% names(data)) { #Most of the processed files have sources in the SourceAupdatedURL column or source_URL
    warning("Skipping file: ", file_path, " — 'sourceAupdatedURL' column not found.")
    return(NULL)
  }
  
  #Detect the columns with BOW links
  bow_links <- data$sourceAupdatedURL[!is.na(data$sourceAupdatedURL) & str_detect(data$sourceAupdatedURL, "birdsoftheworld.org")]
  first_bow_link <- if (length(bow_links) > 0) bow_links[1] else NA_character_
  #Only copies the first BOW link (since the citation is the same across a species account) to save computation power and time
  bow_citation <- get_bow_citation(first_bow_link)
  
  #Applies the BOW citation to all columns with a BOW link
  data_with_citations <- data %>%
    mutate(citation = if_else(
      !is.na(sourceAupdatedURL) & str_detect(sourceAupdatedURL, "birdsoftheworld.org"),
      bow_citation,
      NA_character_
    ))
  
  output_file <- if (!is.null(output_dir)) {
    file.path(output_dir, basename(file_path))
  } else {
    file_path
  }
  
  write_csv(data_with_citations, output_file)
  message("Saved to: ", output_file)
}

#Directories
input_folder <- "inputcsvs"
output_folder <- "outputcsvs"

#Run through the files!
convert_xlsx_to_csv(input_folder, input_folder)
csv_files <- list.files(path = input_folder, pattern = "\\.csv$", full.names = TRUE)
walk(csv_files, ~process_and_resave(.x, output_dir = output_folder))