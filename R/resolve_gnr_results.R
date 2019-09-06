#' Resolve output of GNR matching
#'
#' After names have been parsed and fuzzily matched to names in a taxonomic
#' standard databse using \code{\link{match_with_gnr}}, this function can be used to resolve
#' synonyms. **Very important:** the **same** taxonomic standard should be used
#' for \code{\link{match_with_gnr}} and \code{\link{resolve_gnr_results}}.
#'
#' Synonyms in matched names will be matched to the taxonomic standard.
#' If any are synonyms these will be replaced with the accepted name.
#'
#' @param matched_names Dataframe of names matched to a taxonomic standard
#' (output of match_with_gnr).
#' @param taxonomic_standard Dataframe of standard names in Darwin Core format
#'
#' @return Tibble
#'
#' @examples
#' # Load reference taxonomy in Darwin Core format
#' data(filmy_taxonomy)
#'
#' gnr_match_results <-
#' match_with_gnr(c(
#'   "Cephalomanes oblongifolia",
#'   "Crepidomanes minutum var. minutus",
#'   "Hymenophyllum polyanthos Sw",
#'   "Abrodictyum extravagans (Copel.)",
#'   "adhga adf")
#' )
#'
#' resolve_gnr_results(gnr_match_results, filmy_taxonomy)
#'
#' @export
resolve_gnr_results <- function(matched_names, taxonomic_standard) {

  # Check that format of taxonomic standard meets Darwin Core
  check_darwin_core_format(taxonomic_standard)

  # Check format of matched names
  assertthat::assert_that(nrow(matched_names) > 0)
  assertr::assert(matched_names, is.character, query, matched_name) %>%
    assertr::assert(assertr::not_na, query, success_fun = assertr::success_logical)

  # Merge GNR results and taxonomic standard data
  merged <-
    dplyr::left_join(
      matched_names,
      taxonomic_standard,
      by = c(matched_name = "scientificName"))

  # Split into not-resolved, resolved names, and synonyms (replaced with accepted name)
  not_resolved <- merged %>%
    dplyr::mutate(taxonomicStatus = tidyr::replace_na(taxonomicStatus, "no match")) %>%
    dplyr::filter(taxonomicStatus != "accepted name") %>%
    dplyr::filter(taxonomicStatus != "synonym") %>%
    dplyr::select(
      query,
      scientificName = matched_name,
      genus,
      specificEpithet,
      infraspecificEpithet,
      taxonomicStatus,
      fail_reason
    )

  # Keep accepted names as-is
  accepted_names <- merged %>%
    dplyr::filter(taxonomicStatus == "accepted name") %>%
    dplyr::select(
      query,
      scientificName = matched_name,
      genus,
      specificEpithet,
      infraspecificEpithet,
      taxonomicStatus
    )

  # Replace synonyms with their accepted name
  synonyms <- merged %>%
    dplyr::filter(taxonomicStatus == "synonym") %>%
    dplyr::select(query, matched_name, acceptedNameUsageID) %>%
    dplyr::left_join(taxonomic_standard, by = c(acceptedNameUsageID = "taxonID")) %>%
    dplyr::select(
      query,
      scientificName,
      genus,
      specificEpithet,
      infraspecificEpithet,
    ) %>%
    dplyr::mutate(taxonomicStatus = "synonym")

  # Combine and return result
  dplyr::bind_rows(accepted_names, synonyms, not_resolved) %>%
    dplyr::rename(query_taxonomic_status = taxonomicStatus)
}
