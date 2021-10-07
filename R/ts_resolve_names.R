#' Resolve synonyms in taxonomic names
#'
#' After matching taxonomic names to a reference, some may match synonyms. This
#' function resolves synonyms to their accepted names.
#'
#' `query` can take as input either a character vector of taxonomic names, or
#' the output of \code{\link{ts_match_names}()}. If the former, it will run
#' \code{\link{ts_match_names}()} to match the query to `ref_taxonomy`, then
#' resolve synonyms. If the latter, the scientific names in `ref_taxonomy`
#' should be the same used as reference with \code{\link{ts_match_names}()}
#' (this is not checked).
#'
#' `ref_taxonomy` must be taxonomic data adhering to the [Darwin Core standard](https://dwc.tdwg.org/terms/#taxon).
#' Darwin Core includes many terms, but only four (`taxonID`,
#' `acceptedNameUsageID`, `taxonomicStatus`, and `scientificName`) are required
#' for this function.
#'
#' @param query Character vector or dataframe; taxonomic names to be resolved.
#'   If a character vector, missing values not allowed and all values must be
#'   unique. If a dataframe, should be taxonomic names matched with
#'   \code{\link{ts_match_names}()}.
#' @param ref_taxonomy Dataframe; reference taxonomic data adhering to the
#'   [Darwin Core standard](https://dwc.tdwg.org/terms/#taxon) with the
#'   following columns:
#' - `taxonID`: [Unique identifier for each taxon](https://dwc.tdwg.org/terms/#dwc:taxonID).
#' - `acceptedNameUsageID`: [An identifier for the name usage](https://dwc.tdwg.org/terms/#dwc:acceptedNameUsageID)
#' (documented meaning of the name according to a source) of the currently valid
#' (zoological) or accepted (botanical) taxon.
#' - `taxonomicStatus`: [The status of the use of the `scientificName` as a label for a taxon](https://dwc.tdwg.org/terms/#dwc:taxonomicStatus).
#' Allowed values include `accepted name`, `ambiguous synonym`, `provisionally accepted name`, `synonym`.
#' - `scientificName`: [The full scientific name](https://dwc.tdwg.org/terms/#dwc:scientificName),
#' with authorship and date information if known.
#'
#' @return Dataframe; results of resolving synonyms in matched taxonomic names.
#' Includes the following columns:
#' - `query`: Query name
#' - `accepted_name`: Accepted name after resolving synonyms
#' - `match_type`: Type of match (for a summary of match types, [see taxon-tools manual](https://github.com/camwebb/taxon-tools/blob/master/doc/matchnames.md#matching-rules-and-output-codes))
#' - `status`: Taxonomic status of the matched reference name (same as `taxonomicStatus` in `ref_taxonomy`)
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
#' ts_resolve_names("Gonocormus minutum", filmy_taxonomy)
ts_resolve_names <- function(query, ref_taxonomy) {

  # Check input
  assertthat::assert_that(
    is.character(query) | inherits(query, "data.frame"),
    msg = "query must be of class 'data.frame' or a character vector")
  assertthat::assert_that(inherits(ref_taxonomy, "data.frame"), msg = "ref_taxonomy must be of class 'data.frame'")

  # If needed, match names first
  if(is.character(query)) {
    match_results <- ts_match_names(query, unique(ref_taxonomy$scientificName))
  } else if (is.data.frame(query)) {
    match_results <- query
  } else {
    stop("query must be of class 'data.frame' or a character vector")
  }

  # Classify results of matching
  match_results_classified_with_taxonomy <-
    match_results %>%
    ts_classify_result() %>%
    dplyr::select(query, reference, match_type, result_type) %>%
    dplyr::left_join(ref_taxonomy, by = c(reference = "scientificName"))

  # Separate out single matches to an accepted name (success type 1)
  accepted_single_match <-
    match_results_classified_with_taxonomy %>%
    dplyr::filter(taxonomicStatus %in% c("accepted name", "provisionally accepted name") & result_type == "single_match") %>%
    dplyr::select(query, reference, accepted_name = reference, match_type, status = taxonomicStatus)

  # Separate out matches to a single synonym (success type 2)
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

  # Combine name resolution successes
  success <- dplyr::bind_rows(accepted_single_match, accepted_single_synonyms)

  # Anything else is a failure
  failure <-
    match_results_classified_with_taxonomy %>%
    dplyr::select(query, reference, match_type, status = taxonomicStatus) %>%
    dplyr::anti_join(success, by = "query")

  # Combine into final results
  dplyr::bind_rows(success, failure) %>%
    assertr::verify(all(query %in% match_results$query)) %>%
    assertr::verify(all(match_results$query %in% query))
}
