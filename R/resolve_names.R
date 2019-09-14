#' Resolve a vector of taxonomic names
#'
#' Single hits, the best-matching taxon for multiple hits, and single synonyms
#' matched to their accepted names are retained. These can be excluded using the
#' exclude list to prevent false positive matches.
#'
#' @param names_to_resolve Vector of taxonomic names to resolve.
#' @param taxonomic_standard Dataframe of standard names to match to.
#' @param match_by One of "species", "taxon", or "scientific_name";
#' the type of standard name to use for matching.
#' Should match the type of names in `query`.
#' @param max_dist Integer; maximum distance to use during fuzzy matching.
#' @param exclude Optional vector of taxa to exclude from matches. Names must match
#' names in `names_to_resolve` exactly.
#' @param mult_syn_selection Character vector; in the case that multiple synonyms
#' were detected during matching, use the names specified. Names must match names in
#' `scientificName` column of `taxonomic_standard` exactly.
#'
#' @return Tibble. `taxonomicStatus` refers to the status of the queried name. If
#' the query was a synonym, all other taxonomic names are for the accepted name.
#'
#' @examples
#' # Load reference taxonomy in Darwin Core format
#' data(filmy_taxonomy)
#'
#' resolve_names(
#'   names_to_resolve = c(
#'     "Cephalomanes atrovirens Presl",
#'     "Gonocormus minutus (Blume) Bosch"),
#'   match_by = "scientific_name",
#'   taxonomic_standard = filmy_taxonomy,
#'   max_dist = 2)
#'
#' resolve_names(
#'   names_to_resolve = c(
#'     "Trichomanes infundibulare",
#'     "Gonocormus minutus"),
#'   match_by = "species",
#'   taxonomic_standard = filmy_taxonomy,
#'   max_dist = 2)
#'
#' resolve_names(
#'   names_to_resolve = c(
#'     "bgalkj",
#'     "Hymenophyllum polyanthos",
#'     "bgalkj",
#'     "Hymenophyllum polyanthop"),
#'   match_by = "species",
#'   taxonomic_standard = filmy_taxonomy,
#'   max_dist = 0)
#' @export
resolve_names <- function (names_to_resolve, taxonomic_standard,
                           match_by = c("species", "taxon", "scientific_name"),
                           max_dist,
                           exclude = NULL, mult_syn_selection = NULL) {

  # Check format of query
  assertthat::assert_that(is.character(names_to_resolve))

  # Check that format of taxonomic standard meets Darwin Core
  check_darwin_core_format(taxonomic_standard)

  # Do (fuzzy) match on selected columns
  match_results <- match_taxonomy(
    query = names_to_resolve,
    taxonomic_standard = taxonomic_standard,
    match_by = match_by,
    max_dist = max_dist
  ) %>%
  dplyr::filter(!is.na(taxonID))

  # Resolve the hits into accepted names or synonyms,
  # and look up parent name for synonyms.
  hit_resolution <- resolve_hits(
    hits = match_results,
    col_to_resolve = match_by,
    exclude = exclude,
    taxonomic_standard = taxonomic_standard,
    mult_syn_selection = mult_syn_selection
  )

  # Prepare final list
  # resolve_hits() only returns one row per unique name, so join bac
  # in to original input to return multiple rows if there were duplicate input names.
  final_list_resolved <-
    tibble::tibble(query = names_to_resolve) %>%
    dplyr::inner_join(hit_resolution$resolved_matches, by = "query")

  print(glue::glue("{nrow(final_list_resolved)} names resolved by {match_by}"))

  final_list_unresolved <-
    tibble::tibble(query = names_to_resolve) %>%
    dplyr::anti_join(hit_resolution$resolved_matches, by = "query")

  assertthat::assert_that(
    nrow(final_list_resolved) + nrow(final_list_unresolved) == length(names_to_resolve)
  )

  # Combine resolved and unresolved
  final_list <-
  dplyr::bind_rows(
    final_list_resolved,
    final_list_unresolved
  ) %>% unique

  # Reorder in same order as input
  dplyr::left_join(
    tibble::tibble(query = names_to_resolve),
    final_list,
    by = "query"
  )

}
