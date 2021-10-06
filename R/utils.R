#' Make a dataframe with taxonomic names
#'
#' @param taxa Character vector; taxon names to be parsed by taxon-tools `parsenames`.
#' Missing values not allowed. Must all be unique.
#'
#' @return Dataframe with two columns: `id` and `name`
#' @keywords internal
#' @examples
#' \dontrun{
#' ts_make_name_df("Foogenus x barspecies var. foosubsp (L.) F. Bar")
#' }
ts_make_name_df <- function(taxa) {

  assertthat::assert_that(is.character(taxa))
  assertthat::assert_that(assertthat::noNA(taxa), msg = "Input taxa may not contain NAs")
  assertthat::assert_that(all(assertr::is_uniq(taxa)), msg = "Input taxa must be unique")

  # Format input names as data frame with unique ID
  # ID is combination of first 8 chars of hash of the
  # input (taxa), followed by "-" and integer
  taxa_df <- data.frame(name = taxa)
  taxa_df$id <- 1:nrow(taxa_df)
  taxa_df$id <- paste(substr(digest::digest(taxa), 1, 8), taxa_df$id, sep = "-")

  taxa_df[, c("id", "name")]
}

#' Classify results of taxon-tools matching
#'
#' @param match_results Dataframe; output of tt_match_names()
#'
#' @return Dataframe with column `result_type` added
#' @keywords internal
#' @autoglobal
ts_classify_result <- function(match_results) {
  assertthat::assert_that(inherits(match_results, "data.frame"), msg = "match_results must be of class 'data.frame'")
  match_results %>%
    dplyr::add_count(query) %>%
    dplyr::mutate(
      result_type = dplyr::case_when(
        match_type != "no_match" & n == 1 ~ "single_match",
        match_type != "no_match" & n > 1 ~ "mult_match",
        match_type == "no_match" ~ "no_match",
        TRUE ~ NA_character_
      )
    ) %>%
    assertr::assert(assertr::not_na, result_type) %>%
    dplyr::select(-n)
}
