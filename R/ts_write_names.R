#' Write out parsed names to a text file
#'
#' @param df Dataframe with parsed names
#' @param path Path to write dataframe
#'
#' Writes out parsed names in a format that can be used by [taxon-tools](https://github.com/camwebb/taxon-tools)
#' (each part of the scientific name is separated by the pipe symbol (|), with one name per line).
#'
#' @autoglobal
#' @return Path to parsed names
#' @export
#' @examples
#' parsed_names <- ts_parse_names("Foogenus x barspecies var. foosubsp (L.) F. Bar")
#' temp_file <- tempfile()
#' ts_write_names(parsed_names, temp_file)
#' readLines(temp_file)
#' file.remove(temp_file)
ts_write_names <- function(df, path) {

  # Make vector of standard taxon-tools columns
  tt_col_names = c(
    "id",
    "genus_hybrid_sign",
    "genus_name",
    "species_hybrid_sign",
    "specific_epithet",
    "infraspecific_rank",
    "infraspecific_epithet",
    "author"
  )

  assertthat::assert_that(inherits(df, "data.frame"), msg = "df must be of class 'data.frame'")
  assertthat::assert_that(
    isTRUE(all(tt_col_names %in% colnames(df))),
    msg = glue::glue("df must include the following columns: {paste(tt_col_names, collapse = ', ')}"))

  # Replace NA values with ""
  df <- dplyr::mutate(df, dplyr::across(dplyr::everything(), ~tidyr::replace_na(., "")))

  # Subset to only taxon-tools columns, in order
  df <- df[, tt_col_names]

  # taxon-tools uses pipe as separator
  df <- tidyr::unite(df, col = "text", dplyr::all_of(tt_col_names), sep = "|")

  # write out text
  writeLines(df$text, path)

  path

}
