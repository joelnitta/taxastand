#' Resolve taxonomic names by fuzzy matching
#'
#' Single hits, the best-matching taxon for multiple hits, and single synonyms
#' matched to their accepted names are retained. These can be excluded using the
#' exclude list to prevent false positive matches.
#'
#' It may be useful to set `return_single_object` to FALSE the first time to
#' inspect the results of each step to set exclusion lists.
#'
#' @param names_to_resolve Dataframe of taxonomic names to resolve.
#' @param col_to_resolve Name of column including taxonomic name to resolve in
#' `names_to_resolve`.
#' @param taxonomic_standard Dataframe of standard names to match to.
#' @param col_standard Name of column including taxonomic name to match to in
#' `names_standard`.
#' @param max_dist Integer; maximum distance to use during fuzzy matching.
#' @param exclude Optional vector of taxa to exclude from matches. Names must match
#' names in `col_to_resolve` exactly.
#' @param mult_syn_selection Character vector; in the case that multiple synonyms
#' were detected during matching, use the names specified. Names must match names in
#' `scientificName` column of `names_standard` exactly.
#' @param return_mult_syns Logical; should only synonyms with multiple hits be returned?
#' Useful during initial testing to determine names to use for `mult_syn_selection`.
#'
#' @return Tibble. `col_to_resolve` is the original column selected for matching; all others
#' are the matched taxonomic names for hits, or NA if there was no match.
#'
#' @examples
#' # Load reference taxonomy in Darwin Core format
#' data(filmy_taxonomy)
#'
#' # Make tibble of names to query
#' names_df <- tibble::tibble(
#'   sciname = c("Cephalomanes atrovirens Presl", "Gonocormus minutus (Blume) Bosch"),
#'   species = c("Cephalomanes atrovirens", "Gonocormus minutus")
#' )
#' taxonomic_standard = filmy_taxonomy
#' resolve_names(
#'   names_to_resolve = names_df,
#'   col_to_resolve = "sciname",
#'   names_standard = filmy_taxonomy,
#'   col_standard = "scientificName",
#'   max_dist = 6)
#'
#' resolve_names(
#'   names_df,
#'   col_to_resolve = "species",
#'   names_standard = world_ferns_for_matching,
#'   col_standard = "speciesName",
#'   max_dist = 0)
resolve_names <- function (names_to_resolve, taxonomic_standard,
                           match_by = c("species", "taxon", "sciname"),
                           max_dist,
                           exclude = NULL, mult_syn_selection = NULL, return_mult_syns = FALSE) {

  col_to_resolve_quo <- rlang::enquo(col_to_resolve)

  # Do fuzzy match on selected columns
  match_results <- purrr::map_df(
    names_to_resolve %>% dplyr::pull(col_to_resolve),
    ~match_taxonomy(
      query = .,
      taxonomic_standard = taxonomic_standard,
      match_by = match_by,
      max_dist = max_dist
    )
  )

  match_results <- match_taxonomy (
    names_to_resolve = names_to_resolve,
    col_to_resolve = col_to_resolve,
    names_standard = names_standard,
    col_standard = col_standard,
    max_dist = max_dist,
    distance_col = "distance")

  # Resolve the hits into accepted names or synonyms,
  # and look up parent name for synonyms.
  hit_resolution <- resolve_hits(
    hits = match_results$hits,
    col_to_resolve = col_to_resolve,
    exclude = exclude,
    names_standard = names_standard,
    mult_syn_selection = mult_syn_selection
  )

  print(glue::glue("{hit_resolution$resolved_matches %>% pull(col_to_resolve) %>% n_distinct()} names resolved"))

  # Prepare final list
  final_list_resolved <-
    names_to_resolve %>%
    select(!!col_to_resolve_quo) %>%
    inner_join(hit_resolution$resolved_matches)

  final_list_unresolved <-
    names_to_resolve %>%
    select(!!col_to_resolve_quo) %>%
    anti_join(hit_resolution$resolved_matches) %>%
    mutate(taxonomicStatus = "unresolved")

  final_list <- bind_rows(
    final_list_resolved,
    final_list_unresolved
  ) %>%
    mutate(match_by = col_to_resolve)

  # If user wants mult syns, return list including mults syns and names list
  if (isTRUE(return_mult_syns)) {
    results <- list(
      mult_syns = hit_resolution$multiple_synonyms,
      names = final_list)
  } else {
    results <- final_list
  }

  return(results)

}
