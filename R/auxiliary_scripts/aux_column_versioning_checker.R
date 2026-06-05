`%||%` <- function(x, y) if (is.null(x)) y else x

root_dir <- "C:/Users/kelly/OneDrive - Michigan State University/Research/AvianMetaNetwork/AvianMetaNetwork-Working/L0/species/"
in_review_dir <- "C:/Users/kelly/OneDrive - Michigan State University/Research/AvianMetaNetwork/AvianMetaNetwork-Working/L0/species_in_review/"

ignore_column_order <- TRUE

spp_files <- list.files(
  root_dir,
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)
rev_files <- list.files(
  in_review_dir,
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)
csv_files <- c(spp_files, rev_files)

get_cols <- function(f) {
  x <- tryCatch(
    names(read.csv(f, nrows = 0, check.names = FALSE)),
    error = function(e) NA
  )
  if (length(x) == 1 && is.na(x)) {
    return(NA)
  }
  if (ignore_column_order) {
    x <- sort(x)
  }
  x
}

## ---------------------------------------------------------------------------
## git: latest commit date per file, covering BOTH species + species_in_review
## ---------------------------------------------------------------------------
repo_root <- system2(
  "git",
  c("-C", shQuote(root_dir), "rev-parse", "--show-toplevel"),
  stdout = TRUE
)
repo_root <- normalizePath(repo_root[1], winslash = "/", mustWork = TRUE)

# repo-relative path for a directory (forward slashes, matching git's output)
to_rel <- function(d) {
  sub(
    paste0("^", repo_root, "/?"),
    "",
    normalizePath(d, winslash = "/", mustWork = FALSE)
  )
}

root_rel <- to_rel(root_dir)
review_rel <- to_rel(in_review_dir)

# one git log over both pathspecs; @@@ lines carry the commit date, the lines
# in between are the files changed in that commit (restricted to our pathspecs)
git_out <- system2(
  "git",
  c(
    "-C",
    shQuote(repo_root),
    "log",
    "--name-only",
    "--format=@@@%cI",
    "--",
    root_rel,
    review_rel
  ),
  stdout = TRUE
)

latest_commit_map <- list()
current_date <- NA_character_

for (line in git_out) {
  print(line)
  if (startsWith(line, "@@@")) {
    current_date <- sub("^@@@", "", line)
  } else if (nzchar(line) && !line %in% names(latest_commit_map)) {
    # first time we see a file = its most recent commit (git log is newest-first)
    latest_commit_map[[line]] <- current_date
  }
}

# repo-relative path for each CSV so we can look it up in the map
rel_paths <- sub(
  paste0("^", repo_root, "/?"),
  "",
  normalizePath(csv_files, winslash = "/", mustWork = FALSE)
)

## ---------------------------------------------------------------------------
## per-file schema + commit info
## ---------------------------------------------------------------------------
file_info <- do.call(
  rbind,
  lapply(seq_along(csv_files), function(i) {
    cols <- get_cols(csv_files[i])
    data.frame(
      file = csv_files[i],
      schema = if (all(is.na(cols))) NA else paste(cols, collapse = " | "),
      ncols = if (all(is.na(cols))) NA else length(cols),
      latest_commit = latest_commit_map[[rel_paths[i]]] %||% NA_character_,
      stringsAsFactors = FALSE
    )
  })
)

file_info$latest_commit <- lubridate::ymd_hms(
  file_info$latest_commit,
  tz = "UTC"
)

valid <- subset(file_info, !is.na(schema))

## ---------------------------------------------------------------------------
## give each unique schema a short name (ordered by how many files use it)
## ---------------------------------------------------------------------------
schema_counts <- sort(table(valid$schema), decreasing = TRUE)
schema_levels <- names(schema_counts)
schema_names <- paste0("schema_", seq_along(schema_levels))
names(schema_names) <- schema_levels # full schema string -> short name

valid$schema_name <- schema_names[valid$schema]
file_info$schema_name <- schema_names[file_info$schema] # NA schema stays NA

## ---------------------------------------------------------------------------
## presence/absence matrix: one row per unique column, one column per schema
## ---------------------------------------------------------------------------
schema_cols <- lapply(schema_levels, function(s) {
  strsplit(s, " | ", fixed = TRUE)[[1]]
})
names(schema_cols) <- schema_names

all_cols <- sort(unique(unlist(schema_cols)))

presence <- vapply(
  schema_cols,
  function(cols) all_cols %in% cols,
  logical(length(all_cols))
)
if (is.null(dim(presence))) {
  # guard: single column or single schema
  presence <- matrix(
    presence,
    nrow = length(all_cols),
    dimnames = list(NULL, schema_names)
  )
}

presence_df <- data.frame(
  column = all_cols,
  presence,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

## ---------------------------------------------------------------------------
## key: what each schema_N actually is
## ---------------------------------------------------------------------------
schema_key <- do.call(
  rbind,
  lapply(schema_names, function(nm) {
    d <- valid[valid$schema_name == nm, ]
    dates <- d$latest_commit[!is.na(d$latest_commit)]
    data.frame(
      schema_name = nm,
      file_count = nrow(d),
      ncols = d$ncols[1],
      earliest_latest_commit = if (length(dates)) {
        min(dates)
      } else {
        as.POSIXct(NA)
      },
      latest_latest_commit = if (length(dates)) max(dates) else as.POSIXct(NA),
      example_file = d$file[1],
      schema = d$schema[1],
      stringsAsFactors = FALSE
    )
  })
)
row.names(schema_key) <- NULL

## ---------------------------------------------------------------------------
## write outputs
## ---------------------------------------------------------------------------
write.csv(
  presence_df,
  "./R/auxiliary_scripts/aux_schema_cols.csv",
  row.names = FALSE
)
write.csv(
  schema_key,
  "./R/auxiliary_scripts/aux_schema_metadata.csv",
  row.names = FALSE
)
write.csv(
  file_info,
  "./R/auxiliary_scripts/aux_files_with_schema.csv",
  row.names = FALSE
)

presence_df

# Move obviously invalid species to
