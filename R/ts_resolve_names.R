#' Resolve synonyms in matched taxonomic names
#'
#' After matching query names to a reference taxonomy, some may match synonyms.
#' This function resolves synonyms to their accepted names.
#'
#' `ref_taxonomy` must be taxonomic data adhering to the [Darwin Core standard](https://dwc.tdwg.org/terms/#taxon).
#' Darwin Core includes many terms, but only four (`taxonID`, `acceptedNameUsageID`,
#' `taxonomicStatus`, and `scientificName`) are required for this function.
#'
#' The scientific names in `ref_taxonomy` must be the same used as reference
#' with \code{\link{ts_match_names}()}.
#'
#' @param match_results Dataframe; results of taxonomic name matching, most likely
#' done with \code{\link{ts_match_names}()}.
#' @param ref_taxonomy Dataframe; taxonomic data adhering to the [Darwin Core standard](https://dwc.tdwg.org/terms/#taxon)
#' with the following columns:
#' - `taxonID`: [Unique identifier for each taxon](https://dwc.tdwg.org/terms/#dwc:taxonID).
#' - `acceptedNameUsageID`: [An identifier for the name usage](https://dwc.tdwg.org/terms/#dwc:acceptedNameUsageID)
#' (documented meaning of the name according to a source) of the currently valid (zoological) or
#' accepted (botanical) taxon.
#' - `taxonomicStatus`: [The status of the use of the scientificName as a label for a taxon](https://dwc.tdwg.org/terms/#dwc:taxonomicStatus).
#' Allowed values include `accepted name`, `ambiguous synonym`, `provisionally accepted name`, `synonym`
#' - `scientificName`: [The full scientific name](https://dwc.tdwg.org/terms/#dwc:scientificName),
#' with authorship and date information if known.
#'
#' @return Dataframe; results of resolving synonyms in matched taxonomic names.
#' Includes the following columns:
#' - `query`: Query name
#' - `accepted_name`: Accepted name after resolving synonyms
#' - `match_type`: Type of match (for a summary of match types, [see taxon-tools manual](https://github.com/camwebb/taxon-tools/blob/master/doc/matchnames.md#matching-rules-and-output-codes))
#' - `status`: Taxonomic status (same as `taxonomicStatus` in `ref_taxonomy`)
#' - `reference`: Matched reference name
#'
#' Names that could not be matched or resolve to multiple, different synonyms have `NA` for `accepted_name`.
#'
#' @autoglobal
#' @export
#' @examples
#' # Load reference taxonomy in Darwin Core format
#' data(filmy_taxonomy)
#'
#' # Query a misspelled name
#' match_results <- ts_match_names(
#'   query = "Gonocormus minutum",
#'   reference = unique(filmy_taxonomy$scientificName),
#'   simple = TRUE)
#'
#' # Resolve the synonyms
#' ts_resolve_names(match_results, filmy_taxonomy)
ts_resolve_names <- function(match_results, ref_taxonomy) {

  assertthat::assert_that(inherits(match_results, "data.frame"), msg = "match_results must be of class 'data.frame'")
  assertthat::assert_that(inherits(ref_taxonomy, "data.frame"), msg = "ref_taxonomy must be of class 'data.frame'")

  match_results_classified_with_taxonomy <-
    match_results %>%
    ts_classify_result() %>%
    dplyr::select(query, reference, match_type, result_type) %>%
    dplyr::left_join(ref_taxonomy, by = c(reference = "scientificName"))

  accepted_single_match <-
    match_results_classified_with_taxonomy %>%
    dplyr::filter(taxonomicStatus %in% c("accepted name", "provisionally accepted name") & result_type == "single_match") %>%
    dplyr::select(query, reference, accepted_name = reference, match_type, status = taxonomicStatus)

  accepted_single_synonyms <-
    match_results_classified_with_taxonomy %>%
    dplyr::filter(taxonomicStatus == "synonym") %>%
    dplyr::left_join(
      dplyr::select(ref_taxonomy, taxonID, accepted_name = scientificName),
      by = c(acceptedNameUsageID = "taxonID")) %>%
    dplyr::select(query, reference, accepted_name, match_type, status = taxonomicStatus) %>%
    dplyr::group_by(query) %>%
    # Add count of number of resolved, accepted names per query
    dplyr::mutate(n = dplyr::n_distinct(accepted_name)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(n == 1) %>%
    dplyr::select(-n)

  success <- dplyr::bind_rows(accepted_single_match, accepted_single_synonyms)

  failure <-
    match_results_classified_with_taxonomy %>%
    dplyr::select(query, reference, match_type, status = taxonomicStatus) %>%
    dplyr::anti_join(success, by = "query")

  dplyr::bind_rows(success, failure) %>%
    assertr::verify(all(query %in% match_results$query)) %>%
    assertr::verify(all(match_results$query %in% query))
}
