#' Write out parsed names to a text file
#'
#' @param df Dataframe with parsed names
#' @param path Path to write dataframe. Callers use `tempfile()`; this
#' function does not write anywhere outside of what `path` specifies.
#'
#' Writes out parsed names in pipe-delimited format (one name per line),
#' as required by the internal matching engine.
#'
#' @return Path to parsed names
#' @keywords internal
#' @noRd
ts_write_names <- function(df, path) {
  # Make vector of standard taxon-tools columns
  tt_col_names <- c(
    "id",
    "genus_hybrid_sign",
    "genus_name",
    "species_hybrid_sign",
    "specific_epithet",
    "infraspecific_rank",
    "infraspecific_epithet",
    "author"
  )

  assertthat::assert_that(
    inherits(df, "data.frame"),
    msg = "df must be of class 'data.frame'"
  )
  assertthat::assert_that(
    isTRUE(all(tt_col_names %in% colnames(df))),
    msg = paste0(
      "df must include the following columns: ",
      toString(tt_col_names)
    )
  )

  # Subset to only taxon-tools columns, in order; replace NA values with ""
  df <- as.data.frame(df)[, tt_col_names]
  df[] <- lapply(df, function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x
  })

  # taxon-tools uses pipe as separator
  df$text <- do.call(paste, c(unname(as.list(df[, tt_col_names])), sep = "|"))

  # write out text
  writeLines(df$text, path)

  path
}
