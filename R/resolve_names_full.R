#' Resolve names by recursive matching
#'
#' The input (`names_to_resolve`) may include species names formatted
#' with or without author or infraspecific epithet. Names will be matched
#' recursively in order of scientific name (full name with author), taxon
#' name (name including infraspecific epithet but no author), then species
#' (genus plus specific epithet only).
#'
#' Requires gnparser (https://gitlab.com/gogna/gnparser/)
#' to be installed.
#'
#' @param names_to_resolve Character vector; taxonomic names to resolve.
#' @param taxonomic_standard Dataframe of standard names to match to.
#' @param max_dist Named integer vector; maximum distance to use during
#' fuzzy matching by taxonomic rank.
#' Names of `max_dis` must include 'scientific_name', 'taxon', and 'taxon'.
#' @param resolve_by_taxon Logical; should names be resolved using taxon?
#' @param resolve_by_species Logical; should names be resolved using species name?
#' @param gnparser_path String; path to gnparser executable.
#' Either this must be provided, or gnparser must be on $PATH.
#'
#' @return Tibble
#' @examples
#' # Load reference taxonomy in Darwin Core format
#' data(filmy_taxonomy)
#'
#' resolve_names_full(
#'   names_to_resolve = c(
#'     "Hymenophyllum polyanthos Sw.",
#'     "Crepidomanes minutum var. minutum",
#'     "Trichomanes crassum Copel.",
#'     "Hymenophyllum polyanthos Sw.",
#'     "whoops",
#'     "Nephrolepis brownii"),
#'   taxonomic_standard = filmy_taxonomy
#' )
#'
#'
#' @export
resolve_names_full <- function(names_to_resolve, taxonomic_standard,
                               max_dist = c(
                                 scientific_name = 0,
                                 taxon = 0,
                                 species = 0
                               ),
                               resolve_by_taxon = TRUE,
                               resolve_by_species = TRUE,
                               gnparser_path = NULL) {

  # Check that format of taxonomic standard meets Darwin Core
  check_darwin_core_format(taxonomic_standard)

  # Add non-DarwinCore columns to taxonomic_standard that
  # are needed for matching.
  taxonomic_standard <- add_non_darwin_core_cols(taxonomic_standard)

  # Prepare vectors for storing output
  resolved_names <- list(tibble::tibble(), tibble::tibble(), tibble::tibble())
  unresolved_names <- resolved_names

  # Resolve by scientific name first.
  sci_name_query <- names_to_resolve

  sci_name_results <- resolve_names(
    names_to_resolve = sci_name_query,
    match_by = "scientific_name",
    taxonomic_standard = taxonomic_standard,
    max_dist = max_dist["scientific_name"]
  ) %>%
    # Add "original query" column with original species name
    # for tracking and
    # combining results downstream
    dplyr::mutate(original_query = query)

  resolved_names[[1]] <- dplyr::filter(sci_name_results, taxonomicStatus != "unresolved")

  unresolved_names[[1]] <- dplyr::filter(sci_name_results, taxonomicStatus == "unresolved")

  assertthat::assert_that(nrow(resolved_names[[1]]) + nrow(unresolved_names[[1]]) == length(sci_name_query))

  # Next by taxon name
  if(nrow(unresolved_names[[1]]) > 0 && resolve_by_taxon == TRUE) {

    taxon_query <- unresolved_names[[1]] %>%
      dplyr::pull(query) %>%
      parse_names_batch(gnparser_path = gnparser_path) %>% dplyr::pull(taxon)

    taxon_results <- resolve_names(
      names_to_resolve = taxon_query,
      match_by = "taxon",
      taxonomic_standard = taxonomic_standard,
      max_dist = max_dist["taxon"]
    ) %>%
      # Add in original name for matching at the end
      # this works because order is conserved between the
      # original query string and output dataframe
      dplyr::mutate(original_query = unresolved_names[[1]]$original_query)

    resolved_names[[2]] <- dplyr::filter(taxon_results, taxonomicStatus != "unresolved")

    unresolved_names[[2]] <- dplyr::filter(taxon_results, taxonomicStatus == "unresolved")

    assertthat::assert_that(
      nrow(resolved_names[[2]]) + nrow(unresolved_names[[2]]) ==
        nrow(unresolved_names[[1]]))

  }

  # Last by species name
  if(nrow(unresolved_names[[2]]) > 0 && resolve_by_species == TRUE) {

    species_query <- unresolved_names[[2]] %>% dplyr::pull(original_query) %>%
      parse_names_batch(gnparser_path = gnparser_path) %>% dplyr::pull(taxon) %>% sp_name_only

    species_results <- resolve_names(
      names_to_resolve = species_query,
      match_by = "species",
      taxonomic_standard = taxonomic_standard,
      max_dist = max_dist["species"]
    ) %>%
      dplyr::mutate(original_query = unresolved_names[[2]]$original_query)

    resolved_names[[3]] <- dplyr::filter(species_results, taxonomicStatus != "unresolved")

    unresolved_names[[3]] <- dplyr::filter(species_results, taxonomicStatus == "unresolved")

    assertthat::assert_that(
      nrow(resolved_names[[3]]) + nrow(unresolved_names[[3]]) ==
        nrow(unresolved_names[[2]]))

  }

  # Compile unique resolved names with original query
  final_resolved <- dplyr::bind_rows(resolved_names) %>%
    dplyr::mutate(query = original_query) %>%
    dplyr::select(-original_query) %>%
    unique

  # Return results in same order as original query,
  # duplicating if needed with left_join
  tibble::tibble(
    query = names_to_resolve
  ) %>%
    dplyr::left_join(final_resolved, by = "query") %>%
  # Classify anything missing as not resolved
  dplyr::mutate(taxonomicStatus = tidyr::replace_na(taxonomicStatus, "unresolved"))


}
