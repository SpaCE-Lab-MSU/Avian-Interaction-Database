`%||%` <- function(x, y) if (is.null(x)) y else x

root_dir <- "C:/Users/Kelly Kapsar/OneDrive - Michigan State University/Research/AvianMetaNetwork/AvianMetaNetwork-Working/L0/species/"
ignore_column_order <- TRUE

csv_files <- list.files(root_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
csv_files <- normalizePath(csv_files, winslash = "/", mustWork = TRUE)

get_cols <- function(f) {
  x <- tryCatch(names(read.csv(f, nrows = 0, check.names = FALSE)), error = function(e) NA)
  if (length(x) == 1 && is.na(x)) return(NA)
  if (ignore_column_order) x <- sort(x)
  x
}

repo_root <- system2("git", c("-C", shQuote(root_dir), "rev-parse", "--show-toplevel"), stdout = TRUE)
repo_root <- normalizePath(repo_root[1], winslash = "/", mustWork = TRUE)

root_rel <- sub(paste0("^", repo_root, "/?"), "", normalizePath(root_dir, winslash = "/"))

git_out <- system2(
  "git",
  c("-C", shQuote(repo_root), "log", "--name-only", "--format=@@@%cI", "--", root_rel),
  stdout = TRUE
)

latest_commit_map <- list()
current_date <- NA_character_

for (line in git_out) {
  if (startsWith(line, "@@@")) {
    current_date <- sub("^@@@", "", line)
  } else if (nzchar(line) && !line %in% names(latest_commit_map)) {
    latest_commit_map[[line]] <- current_date
  }
}

rel_paths <- sub(paste0("^", repo_root, "/?"), "", csv_files)

file_info <- do.call(rbind, lapply(seq_along(csv_files), function(i) {
  cols <- get_cols(csv_files[i])
  data.frame(
    file = csv_files[i],
    schema = if (all(is.na(cols))) NA else paste(cols, collapse = " | "),
    ncols = if (all(is.na(cols))) NA else length(cols),
    latest_commit = latest_commit_map[[rel_paths[i]]] %||% NA_character_,
    stringsAsFactors = FALSE
  )
}))


file_info$latest_commit <- lubridate::ymd_hms(file_info$latest_commit, tz = "UTC")

valid <- subset(file_info, !is.na(schema))

summary_df <- do.call(rbind, lapply(split(valid, valid$schema), function(d) {
  dates <- d$latest_commit[!is.na(d$latest_commit)]
  data.frame(
    file_count = nrow(d),
    ncols = d$ncols[1],
    earliest_latest_commit = if (length(dates)) min(dates) else as.POSIXct(NA),
    latest_latest_commit = if (length(dates)) max(dates) else as.POSIXct(NA),
    schema = d$schema[1],
    stringsAsFactors = FALSE
  )
}))

summary_df <- summary_df[order(-summary_df$file_count), ]
row.names(summary_df) <- NULL

write.csv(summary_df, "schema_summary.csv", row.names = FALSE)
write.csv(file_info, "file_details.csv", row.names = FALSE)

summary_df
