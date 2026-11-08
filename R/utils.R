#' Make a dataframe with taxonomic names
#'
#' @param taxa Character vector; taxon names to be parsed by
#' \code{\link{ts_parse_names}()} (via the internal pure-R parsing engine,
#' `tt_parsenames()`). Missing values not allowed. Must all be unique.
#'
#' @return Dataframe with two columns: `id` and `name`
#' @keywords internal
#' @examples
#' taxastand:::ts_make_name_df("Foogenus x barspecies var. foosubsp (L.) F. Bar")
ts_make_name_df <- function(taxa) {
  assertthat::assert_that(is.character(taxa))
  assertthat::assert_that(
    assertthat::noNA(taxa),
    msg = "Input taxa may not contain NAs"
  )
  assertthat::assert_that(
    !anyDuplicated(taxa),
    msg = "Input taxa must be unique"
  )

  # Format input names as data frame with unique ID
  # ID is combination of first 8 chars of hash of the
  # input (taxa), followed by "-" and integer
  taxa_df <- data.frame(name = taxa)
  taxa_df$id <- seq_len(nrow(taxa_df))
  taxa_df$id <- paste(substr(digest::digest(taxa), 1, 8), taxa_df$id, sep = "-")

  taxa_df[, c("id", "name")]
}

#' Classify results of name matching
#'
#' @param match_results Dataframe; output of \code{\link{ts_match_names}()}
#'
#' @return Dataframe with column `result_type` added
#' @keywords internal
#' @importFrom stats ave
ts_classify_result <- function(match_results) {
  assertthat::assert_that(
    inherits(match_results, "data.frame"),
    msg = "match_results must be of class 'data.frame'"
  )
  result <- as.data.frame(match_results)
  n <- ave(seq_len(nrow(result)), result$query, FUN = length)
  matched <- result$match_type != "no_match"
  result$result_type <- c("no_match", "single_match", "mult_match")[
    1L + matched + (matched & n > 1)
  ]
  assertthat::assert_that(
    !anyNA(result$result_type),
    msg = "result_type must not be NA"
  )
  result
}

# Left join of data frames `x` and `y` in base R (replaces dplyr joins).
# `y` rows matching a key in `x` are expanded (many-to-many as in dplyr); the
# key column of `y` is dropped. If `inner`, unmatched `x` rows are dropped.
.ts_join <- function(x, y, by_x, by_y, inner = FALSE) {
  x <- as.data.frame(x)
  y <- as.data.frame(y)
  rows_y <- split(seq_len(nrow(y)), y[[by_y]])
  hits <- rows_y[match(x[[by_x]], names(rows_y))]
  n_hit <- lengths(hits)
  xi <- rep(seq_len(nrow(x)), pmax(n_hit, 1L))
  yi <- unlist(lapply(hits, function(h) if (length(h)) h else NA_integer_),
    use.names = FALSE
  )
  out <- x[xi, , drop = FALSE]
  y <- y[, setdiff(colnames(y), by_y), drop = FALSE]
  for (col in colnames(y)) out[[col]] <- y[[col]][yi]
  if (inner) out <- out[n_hit[xi] > 0L, , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Convert to a tibble (tibble is only a suggested package)
.ts_as_tibble <- function(x) {
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop(
      "Package 'tibble' is required for `tbl_out = TRUE`; ",
      "please install it.",
      call. = FALSE
    )
  }
  tibble::as_tibble(x)
}
