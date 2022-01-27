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
#' - `acceptedNameUsageID`: If the taxon is a synonym, the [unique identifier for the accepted name](https://dwc.tdwg.org/terms/#dwc:acceptedNameUsageID)
#' - `taxonomicStatus`: [The status of the use of the `scientificName` as a label for the taxon](https://dwc.tdwg.org/terms/#dwc:taxonomicStatus).
#' - `scientificName`: [The full scientific name of the taxon](https://dwc.tdwg.org/terms/#dwc:scientificName),
#' with authorship and date information if known.
#' @param max_dist Max Levenshtein distance to allow during fuzzy matching
#' (total insertions, deletions and substitutions). Default: 10.
#' @param match_no_auth Logical; If no author is given in the query and the name (without author)
#' occurs only once in the reference, accept the name in the reference as a match.
#' Default: to not allow such a match (`FALSE`).
#' @param match_canon Logical; Allow a "canonical name" match if only the genus, species epithet,
#' and infraspecific epithet (if present) match exactly. Default: to not allow such a match (`FALSE`).
#' @param collapse_infra Logical; if the specific epithet and infraspecific epithet
#' are the same, drop the infraspecific rank and epithet from the query. For more
#' information, see \code{\link{ts_match_names}()}.
#' @param collapse_infra_exclude Character vector; taxonomic names to exclude
#' collapsing with `collapse_infra`. Any names used must match those in `query`
#' exactly, or they won't be excluded.
#' @param tbl_out Logical vector of length 1; should a tibble be returned?
#' If `FALSE` (default), output will be a data.frame. This argument can
#' be controlled via the option `ts_tbl_out`; see Examples.
#'
#' @return Dataframe; results of resolving synonyms in matched taxonomic names.
#' Includes the following columns:
#' - `query`: Query name
#' - `resolved_name`: Accepted name after resolving synonyms
#' - `matched_name`: Name matched to query
#' - `resolved_status`: Taxonomic status of the resolved name (same as `taxonomicStatus` in `ref_taxonomy`)
#' - `matched_status`: Taxonomic status of the matched name (same as `taxonomicStatus` in `ref_taxonomy`)
#' - `match_type`: Type of match (for a summary of match types, [see taxon-tools manual](https://github.com/camwebb/taxon-tools/blob/master/doc/matchnames.md#matching-rules-and-output-codes))
#'
#' Names that could not be matched or resolve to multiple, different synonyms have `NA` for `resolved_name`.
#'
#' @autoglobal
#' @export
#' @examples
#' # Load reference taxonomy in Darwin Core format
#' data(filmy_taxonomy)
#'
#' ts_resolve_names("Gonocormus minutum", filmy_taxonomy)
#' # If you always want tibble output without specifying `tbl_out = TRUE` every
#' # time, set the option:
#' options(ts_tbl_out = TRUE)
#' ts_resolve_names("Gonocormus minutum", filmy_taxonomy)
#'
ts_resolve_names <- function(
  query, ref_taxonomy,
  max_dist = 10, match_no_auth = FALSE, match_canon = FALSE,
  collapse_infra = FALSE,
  collapse_infra_exclude = NULL,
  tbl_out = getOption("ts_tbl_out", default = FALSE)) {

  # Check input
  assertthat::assert_that(
    is.character(query) | inherits(query, "data.frame"),
    msg = "query must be of class 'data.frame' or a character vector")
  assertthat::assert_that(inherits(ref_taxonomy, "data.frame"), msg = "ref_taxonomy must be of class 'data.frame'")
  assertthat::assert_that(assertthat::is.flag(tbl_out))
  if (!is.null(collapse_infra_exclude)) {
    assertthat::assert_that(is.character(collapse_infra_exclude))
  }

  # If needed, match names first
  if(is.character(query)) {
    match_results <- ts_match_names(
      query = query, reference = unique(ref_taxonomy$scientificName),
      max_dist = max_dist, match_no_auth = match_no_auth,
      match_canon = match_canon, collapse_infra = collapse_infra,
      collapse_infra_exclude = collapse_infra_exclude,
      simple = TRUE)
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
    # consider accepted names have either no acceptedNameUsageID or acceptedNameUsageID is same as taxonID
    dplyr::filter(
      (is.na(acceptedNameUsageID) | acceptedNameUsageID == "" | taxonID == acceptedNameUsageID) & result_type == "single_match"
    ) %>%
    dplyr::select(query, resolved_name = reference, matched_name = reference, resolved_status = taxonomicStatus, matched_status = taxonomicStatus, match_type)

  # Separate out matches to a single synonym (success type 2)
  accepted_single_synonyms <-
    match_results_classified_with_taxonomy %>%
    # Consider synonym anything with acceptedNameUsageID not matching taxonID
    dplyr::filter(!is.na(acceptedNameUsageID)) %>%
    dplyr::filter(acceptedNameUsageID != "") %>%
    dplyr::filter(acceptedNameUsageID != taxonID) %>%
    # Join resolved names via synonym
    dplyr::left_join(
      dplyr::select(ref_taxonomy, taxonID, resolved_name = scientificName, resolved_status = taxonomicStatus),
      by = c(acceptedNameUsageID = "taxonID")) %>%
    dplyr::select(query, resolved_name, matched_name = reference, resolved_status, matched_status = taxonomicStatus, match_type) %>%
    dplyr::group_by(query) %>%
    # Add count of number of resolved, accepted names per query
    dplyr::mutate(n = dplyr::n_distinct(resolved_name)) %>%
    dplyr::ungroup() %>%
    # Only keep those that resolve to the same name
    dplyr::filter(n == 1) %>%
    dplyr::select(-n)

  # Combine name resolution successes
  success <- dplyr::bind_rows(accepted_single_match, accepted_single_synonyms)

  # Anything else is a failure
  failure <-
    match_results_classified_with_taxonomy %>%
    dplyr::select(query, match_type, matched_status = taxonomicStatus, matched_name = reference) %>%
    dplyr::anti_join(success, by = "query")

  # Combine into final results
  results <- dplyr::bind_rows(success, failure) %>%
    assertr::verify(all(query %in% match_results$query)) %>%
    assertr::verify(all(match_results$query %in% query)) %>%
    dplyr::select(query, resolved_name, matched_name, resolved_status, matched_status, match_type)

  # Return as tibble or dataframe
  if(isTRUE(tbl_out)) return(tibble::as_tibble(results))

  results
}
