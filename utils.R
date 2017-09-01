library(stringi)

#' Read text lines from a file and auto-detect encoding.
read_lines <- function(file) {
  if (!is.character(file) || length(file) != 1) {
    stop("invalid 'file' argument", call.=FALSE)
  }
  
  if (!file.exists(file)) {
    stop(sprintf("file '%s' does not exist", file), call.=FALSE)
  }

  # DCUO logs will likely be in Latin-1
  stri_read_lines(file, fallback_encoding="latin1")
}

#' Extract named capture groups from a string.
str_match_named <- function(string, pattern) {
  match_info <- regexpr(pattern, string, perl=TRUE)
  capture_names <- attr(match_info, "capture.names")
  
  if (is.null(capture_names) || any(capture_names == "")) {
    stop("pattern must have named capture groups")
  }
  
  matched <- which(match_info > 0)
  start <- attr(match_info, "capture.start")
  end <- start + attr(match_info, "capture.length") - 1
  
  str_matches <- substring(string[matched], start[matched, ], end[matched, ])
  str_matches <- matrix(str_matches, length(matched), length(capture_names))
  colnames(str_matches) <- capture_names

  structure(str_matches, matched=matched)
}

#' Get names of numeric capture groups from a pattern.
get_numeric_groups <- function(pattern) {
  matches <- stri_match_all_regex(pattern, "\\(?<(?<num>[a-zA-Z]+)>\\[0-9\\]", omit_no_match=TRUE)
  matches[[1]][, 2]
}

#' Sort a data table in place.
#'
#' @param dt A data table.
#' @param sort_by Column to sort by.
#' @param sort_order Sort in "desc" or "asc" order (desc by default).
sort_dt <- function(dt, by, order=c("desc", "asc")) {
  
  stopifnot(inherits(dt, "data.table"))
  
  by <- match.arg(by, choices=names(dt))
  order <- match.arg(order)
  setorderv(dt, by, order=ifelse(order == "asc", 1L, -1L))
  
  dt
}
