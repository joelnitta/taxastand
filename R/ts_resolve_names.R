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
#' (total insertions, deletions and substitutions). The distance is computed on
#' the whole name, including the author string, after removing punctuation and
#' converting to lower case. Default: 3.
#' @param max_dist_genus Max Levenshtein distance between the genus of a query
#' and a genus in the reference, for correcting misspelled genera. Only used
#' for queries whose genus is not in the reference; the candidates are then the
#' names in reference genera within this distance, and the whole name (or the
#' canonical name, for a query without an author) must still be within
#' `max_dist`. Matches have the type `auto_fuzzy_genus`. Set to 0 to turn
#' genus-level fuzzy matching off. See \code{\link{ts_match_names}()}. Default: 2.
#' @param match_no_auth Logical; If no author is given in the query and the name
#' (without author) occurs only once in the reference, accept the name in the
#' reference as a match. Default: to not allow such a match (`FALSE`).
#' @param match_canon Logical; Allow a "canonical name" match if only the genus,
#' species epithet, and infraspecific epithet (if present) match exactly.
#' Default: to not allow such a match (`FALSE`).
#' @param collapse_infra Logical; if the specific epithet and infraspecific
#' epithet are the same, drop the infraspecific rank and epithet from the query.
#' For more information, see \code{\link{ts_match_names}()}.
#' @param collapse_infra_exclude Character vector; taxonomic names to exclude
#' collapsing with `collapse_infra`. Any names used must match those in `query`
#' exactly, or they won't be excluded.
#' @param tbl_out Logical vector of length 1; should a tibble be returned?
#' If `FALSE` (default), output will be a data.frame. This argument can
#' be controlled via the option `ts_tbl_out`; see Examples.
#' @param strip_qualifiers Logical; if `TRUE`, remove standalone
#' open-nomenclature qualifiers (`cf.`, `aff.`, `s.l.`/`sensu lato`,
#' `s.str.`/`sensu stricto`) from names before parsing. See
#' \code{\link{ts_parse_names}()} and its References section. Only has an
#' effect when `query` is a character vector. Default: `FALSE`.
#' @param exclude_placeholders Logical; exclude placeholder names such as
#' `"Genus sp."` or `"Genus indet."` (identification stopped above species
#' rank) with a warning, instead of matching them as if they were species. See
#' \code{\link{ts_parse_names}()}. Only has an effect when `query` is a
#' character vector. Default: `TRUE`.
#'
#' @param fuzzy_canon_hits Either `"all"` (default) or `"closest"`. Controls
#' which hits are returned when a query *without an author* has no identical
#' canonical name in the reference and is fuzzy-matched on its canonical name
#' (match type `auto_fuzzy_canon`). `"all"` returns every reference name within
#' `max_dist`, which is the most informative but can include many unrelated
#' names when epithets are short. `"closest"` returns only the names at the
#' smallest distance found (ties are all kept, so real ambiguity still shows).
#' @return Dataframe; results of resolving synonyms in matched taxonomic names.
#' Includes the following columns:
#' - `query`: Query name
#' - `resolved_name`: Accepted name after resolving synonyms
#' - `matched_name`: Name matched to query
#' - `resolved_status`: Taxonomic status of the resolved name (same as `taxonomicStatus` in `ref_taxonomy`)
#' - `matched_status`: Taxonomic status of the matched name (same as `taxonomicStatus` in `ref_taxonomy`)
#' - `match_type`: Type of match (see the "Match types" section of \code{\link{ts_match_names}()})
#'
#' Names that could not be matched or resolve to multiple, different synonyms
#' have `NA` for `resolved_name`.
#'
#' @references Sigovini, M., Keppel, E., & Tagliapietra, D. (2016). Open
#' Nomenclature in the biodiversity era. Methods in Ecology and Evolution,
#' 7(10), 1217-1225. \doi{10.1111/2041-210X.12594}
#'
#' @autoglobal
#' @export
#' @examples
#' # Load reference taxonomy in Darwin Core format
#' data(filmy_taxonomy)
#'
#' ts_resolve_names("Gonocormus minutum", filmy_taxonomy)
#' # If you always want tibble output without specifying `tbl_out = TRUE`
#' # every time, set the option:
#' options(ts_tbl_out = TRUE)
#' ts_resolve_names("Gonocormus minutum", filmy_taxonomy)
#'
ts_resolve_names <- function(
  query,
  ref_taxonomy,
  max_dist = 3,
  max_dist_genus = 2,
  match_no_auth = FALSE,
  match_canon = FALSE,
  collapse_infra = FALSE,
  collapse_infra_exclude = NULL,
  tbl_out = getOption("ts_tbl_out", default = FALSE),
  strip_qualifiers = FALSE,
  exclude_placeholders = TRUE,
  fuzzy_canon_hits = c("all", "closest")
) {
  # Check input
  assertthat::assert_that(
    is.character(query) | inherits(query, "data.frame"),
    msg = "query must be of class 'data.frame' or a character vector"
  )
  assertthat::assert_that(
    inherits(ref_taxonomy, "data.frame"),
    msg = "ref_taxonomy must be of class 'data.frame'"
  )
  assertthat::assert_that(assertthat::is.flag(tbl_out))
  assertthat::assert_that(assertthat::is.flag(strip_qualifiers))
  fuzzy_canon_hits <- match.arg(fuzzy_canon_hits)
  if (!is.null(collapse_infra_exclude)) {
    assertthat::assert_that(is.character(collapse_infra_exclude))
  }

  # If needed, match names first
  if (is.character(query)) {
    match_results <- ts_match_names(
      query = query,
      reference = unique(ref_taxonomy$scientificName),
      max_dist = max_dist,
      max_dist_genus = max_dist_genus,
      match_no_auth = match_no_auth,
      match_canon = match_canon,
      collapse_infra = collapse_infra,
      collapse_infra_exclude = collapse_infra_exclude,
      simple = TRUE,
      strip_qualifiers = strip_qualifiers,
      exclude_placeholders = exclude_placeholders,
      fuzzy_canon_hits = fuzzy_canon_hits
    )
  } else if (is.data.frame(query)) {
    match_results <- query
  } else {
    stop("query must be of class 'data.frame' or a character vector")
  }

  # Classify results of matching
  match_results_classified <- ts_classify_result(match_results)
  match_results_classified <- match_results_classified[
    , c("query", "reference", "match_type", "result_type")
  ]
  ref_info <- data.frame(
    scientificName = ref_taxonomy$scientificName,
    taxonID = ref_taxonomy$taxonID,
    acceptedNameUsageID = ref_taxonomy$acceptedNameUsageID,
    taxonomicStatus = ref_taxonomy$taxonomicStatus,
    stringsAsFactors = FALSE
  )
  match_results_classified <- .ts_join(
    match_results_classified, ref_info, "reference", "scientificName"
  )
  aid <- match_results_classified$acceptedNameUsageID
  tid <- match_results_classified$taxonID

  # Separate out single matches to an accepted name (success type 1)
  # consider accepted names have either no acceptedNameUsageID or
  # acceptedNameUsageID is same as taxonID
  is_accepted <- (is.na(aid) | aid == "" | tid == aid) &
    match_results_classified$result_type == "single_match"
  accepted <- match_results_classified[is_accepted %in% TRUE, , drop = FALSE]
  accepted_single_match <- data.frame(
    query = accepted$query,
    resolved_name = accepted$reference,
    matched_name = accepted$reference,
    resolved_status = accepted$taxonomicStatus,
    matched_status = accepted$taxonomicStatus,
    match_type = accepted$match_type,
    stringsAsFactors = FALSE
  )

  # Separate out matches to a single synonym (success type 2)
  # Consider synonym anything with acceptedNameUsageID not matching taxonID
  is_syn <- !is.na(aid) & aid != "" & aid != tid
  syn <- match_results_classified[is_syn %in% TRUE, , drop = FALSE]
  syn <- .ts_join(
    syn,
    data.frame(
      acceptedNameUsageID = ref_info$taxonID,
      resolved_name = ref_taxonomy$scientificName,
      resolved_status = ref_taxonomy$taxonomicStatus,
      stringsAsFactors = FALSE
    ),
    "acceptedNameUsageID", "acceptedNameUsageID"
  )
  # Count of number of resolved, accepted names per query; only keep those
  # that resolve to the same name
  n_resolved <- ave(
    as.integer(factor(syn$resolved_name, exclude = NULL)),
    syn$query,
    FUN = function(z) length(unique(z))
  )
  syn <- syn[n_resolved == 1, , drop = FALSE]
  accepted_single_synonyms <- data.frame(
    query = syn$query,
    resolved_name = syn$resolved_name,
    matched_name = syn$reference,
    resolved_status = syn$resolved_status,
    matched_status = syn$taxonomicStatus,
    match_type = syn$match_type,
    stringsAsFactors = FALSE
  )

  # Combine name resolution successes
  success <- rbind(accepted_single_match, accepted_single_synonyms)

  # Anything else is a failure
  failure <- match_results_classified[
    !match_results_classified$query %in% success$query, ,
    drop = FALSE
  ]
  failure <- data.frame(
    query = failure$query,
    resolved_name = rep(NA_character_, nrow(failure)),
    matched_name = failure$reference,
    resolved_status = rep(NA_character_, nrow(failure)),
    matched_status = failure$taxonomicStatus,
    match_type = failure$match_type,
    stringsAsFactors = FALSE
  )

  # Combine into final results
  results_raw <- rbind(success, failure)
  assertthat::assert_that(
    all(results_raw$query %in% match_results$query),
    msg = "query values in results must all be present in match_results"
  )
  assertthat::assert_that(
    all(match_results$query %in% results_raw$query),
    msg = "query values in match_results must all be present in results"
  )
  results <- results_raw[
    , c(
      "query", "resolved_name", "matched_name", "resolved_status",
      "matched_status", "match_type"
    )
  ]
  rownames(results) <- NULL

  # Return as tibble or dataframe
  if (isTRUE(tbl_out)) {
    return(.ts_as_tibble(results))
  }

  results
}
