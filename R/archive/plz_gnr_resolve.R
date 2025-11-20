function (sci, data_source_ids = NULL, resolve_once = FALSE, 
          with_context = FALSE, canonical = FALSE, highestscore = TRUE, 
          best_match_only = FALSE, preferred_data_sources = NULL, with_canonical_ranks = FALSE, 
          http = "get", cap_first = TRUE, fields = "minimal", names = NULL, 
          ...) 
{
  if (!is.null(names)) {
    lifecycle::deprecate_warn(when = "v0.9.97", what = "gnr_resolve(names)", 
                              with = "gnr_resolve(sci)")
    sci <- names
  }
  if (!is.null(names)) 
    sci <- names
  fields <- match.arg(fields, c("minimal", "all"))
  http <- match.arg(http, c("get", "post"))
  num <- NULL
  url <- "https://resolver.globalnames.org/name_resolvers.json"
  sci <- Filter(function(x) nzchar(x) && !is.na(x) && is.character(x), 
                sci)
  orig_names <- sci
  if (cap_first) 
    sci <- taxize_capwords(sci, onlyfirst = TRUE)
  names2 <- paste0(sci, collapse = "|")
  if (length(sci) > 300 && http == "get") 
    http <- "post"
  data_source_ids <- paste0(data_source_ids, collapse = "|")
  preferred_data_sources <- paste0(preferred_data_sources, 
                                   collapse = "|")
  if (nchar(preferred_data_sources, keepNA = FALSE) == 0) 
    preferred_data_sources <- NULL
  if (with_canonical_ranks) 
    canonical <- TRUE
  args <- tc(list(names = names2, data_source_ids = data_source_ids, 
                  resolve_once = cv(resolve_once), with_context = cv(with_context), 
                  best_match_only = cv(best_match_only), preferred_data_sources = preferred_data_sources, 
                  with_canonical_ranks = cv(with_canonical_ranks)))
  args <- argsnull(args)
  cli <- crul::HttpClient$new(url = url, headers = tx_ual, 
                              opts = list(...))
  if (http == "get") {
    tmp <- cli$get(query = args)
    tmp$raise_for_status()
    tmp2 <- tmp$parse("UTF-8")
    dat <- jsonlite::fromJSON(tmp2, FALSE)$data
  }
  else {
    args <- args[!names(args) %in% "names"]
    nms <- split(sci, ceiling(seq_along(sci)/500))
    datbits <- list()
    for (i in seq_along(nms)) {
      tt <- data.frame(paste0(seq_along(nms[[i]]), "|", 
                              sci))
      file <- tempfile(fileext = ".txt")
      write.table(tt, file = file, row.names = FALSE, col.names = FALSE, 
                  quote = FALSE)
      tmp <- cli$post(query = args, body = list(file = crul::upload(file)))
      tmp$raise_for_status()
      ss <- tmp$parse("UTF-8")
      datbits[[i]] <- jsonlite::fromJSON(ss, FALSE)$data
    }
    dat <- do.call("c", datbits)
  }
  dat <- Map(function(x, y) c(original_name = y, x), dat, orig_names)
  to_get <- if (is.null(preferred_data_sources)) 
    "results"
  else "preferred_results"
  data_ <- lapply(dat, function(y) {
    if (!is.null(unlist(y[[to_get]]))) {
      res <- lapply(y[[to_get]], function(x) {
        take_fields <- switch(fields, minimal = c("name_string", 
                                                  "data_source_title", "score", "canonical_form"), 
                              all = names(x))
        take <- x[take_fields]
        take[sapply(take, is.null)] <- NA
        return(data.frame(take, stringsAsFactors = FALSE))
      })
    }
    else {
      res <- NULL
    }
    list(y[c("original_name", "supplied_name_string")], res)
  })
  not_known <- Filter(function(x) is.null(x[[2]]), data_)
  not_known <- sapply(not_known, function(x) x[[1]]$original_name)
  data_ <- Filter(function(x) !is.null(x[[2]]), data_)
  drill <- tryCatch(data_[[1]], error = function(e) e)
  to_rename <- c("original_name", "supplied_name_string", "name_string", 
                 "canonical_form")
  if (inherits(drill, "simpleError")) {
    out <- data.frame(NULL)
  }
  else {
    data_2 <- dt2df(lapply(data_, function(x) data.frame(x[[1]], 
                                                         dt2df(if (length(x[[2]]) == 0) {
                                                           list(data.frame(name_string = "", data_source_title = "", 
                                                                           score = NaN, canonical_form = ""))
                                                         }
                                                         else {
                                                           x[[2]]
                                                         }, idcol = FALSE), stringsAsFactors = FALSE)), idcol = FALSE)
    names(data_2)[names(data_2) %in% to_rename] <- c("user_supplied_name", 
                                                     "submitted_name", "matched_name", "matched_name2")
    data_2$matched_name <- as.character(data_2$matched_name)
    data_2$data_source_title <- as.character(data_2$data_source_title)
    data_2$matched_name2 <- as.character(data_2$matched_name2)
    if (canonical) {
      data_2 <- data_2[, !names(data_2) %in% "matched_name"]
    }
    else {
      data_2 <- data_2[, !names(data_2) %in% "matched_name2"]
    }
    out <- unique(data_2)
  }
  row.names(out) <- NULL
  structure(tibble::as_tibble(out), not_known = not_known)
}
