#' Resolve hits from taxonomic matching of a single name
#'
#' @param hits Dataframe; hits from taxonomic matching - output of
#' match_taxonomy().
#' @param col_to_resolve Name of column including taxonomic name to resolve.
#' @param taxonomic_standard Dataframe of standard names to match to.
#' @param exclude Character vector; taxa that should be exluded from the final
#' results. Useful to prevent false positive matches. Names must names in
#' `col_to_resolve` exactly.
#' @param simple Logical; should the results include only a limited set of
#' columns? If FALSE, all columns in `taxonomic_standard` will be returned.
#'
#' @return List including two items; `resolved_matches` is dataframe of resolved
#' names (including both accepted named and synonyms); `multiple_synonyms` are taxa
#' with multiple synonyms that could not be resolved.
#'
#' @examples
#' # Load reference taxonomy in Darwin Core format
#' data(filmy_taxonomy)
#'
#' taxonomic_standard <- filmy_taxonomy
#'
#' # Single accepted name
#' tax_matching_results <- match_taxonomy(
#'   "Hymenophyllum polyanthos",
#'   filmy_taxonomy, "species")
#' resolve_hits(tax_matching_results, "species", filmy_taxonomy)
#'
#' # Single synonym
#' tax_matching_results <- match_taxonomy(
#'   "Trichomanes crassum",
#'   filmy_taxonomy, "species")
#' resolve_hits(tax_matching_results, "species", filmy_taxonomy)
#'
#' # Fuzzy match, results in both accepted name and multiple synonyms
#' tax_matching_results <- match_taxonomy(
#'   "Cephalomanes javanicam",
#'   filmy_taxonomy, "species", max_dist = 2)
#' resolve_hits(tax_matching_results, "species", filmy_taxonomy)
#' @export
resolve_hits <- function (hits,
                          col_to_resolve = c("scientific_name", "taxon", "species"),
                          taxonomic_standard,
                          exclude = NULL,
                          simple = TRUE) {

  ### Setup ###
  # Prepare sets of column names to include in the output.
  # - Column names relevant to the match algorithm
  match_cols <- c(
    "query",    # input query
    "n_hits",   # number of hits to that query
    "distance", # number of characters different between query and match
    "match_to", # name that matched the query
    "match_by") # what type of matching was used (species, taxon, or sci. name)

  # - Selected DarwinCore taxonomy columns to include
  # if results are "simple"
  darwin_simple_cols <- c(
    "taxonID", "acceptedNameUsageID",
    "taxonomicStatus", "taxonRank",
    "scientificName", "genus", "specificEpithet", "infraspecificEpithet"
  )

  # Set name to use for resolving taxonomy.
  col_to_resolve <- switch(col_to_resolve,
                         species = "speciesName",
                         taxon = "taxonName",
                         scientific_name = "scientificName")

  col_to_resolve <- as.name(col_to_resolve)

  # Check that format of taxonomic standard meets Darwin Core
  # and add non-standard columns (genericName, speciesName, taxonName)
  check_darwin_core_format(taxonomic_standard)
  taxonomic_standard <- add_non_darwin_core_cols(taxonomic_standard)

  # Check that format of hits meets Darwin Core
  # and add non-standard columns (genericName, speciesName, taxonName)
  check_darwin_core_cols(hits)
  hits <- add_non_darwin_core_cols(hits)

  # Categorize hits
  #
  # ## Accepted names
  #
  # Those with a single accepted name are done
  accepted_singles <-
    hits %>%
    dplyr::filter(stringr::str_detect(taxonomicStatus, "accepted"), n_hits == 1)

  # For those with multiple hits to an accepted name, take the closest match.
  accepted_resolved_mults <-
    hits %>%
    dplyr::filter(stringr::str_detect(taxonomicStatus, "accepted"), n_hits > 1) %>%
    dplyr::arrange(!!col_to_resolve, distance) %>%
    dplyr::group_by(!!col_to_resolve) %>%
    dplyr::slice(1)

  accepted <- dplyr::bind_rows(accepted_singles, accepted_resolved_mults)

  # ## Synonyms
  #
  # Synonyms are anything with a hit that has not been resolved yet.
  resolved_names <- dplyr::pull(accepted, !!col_to_resolve)

  synonyms <-
    hits %>%
    dplyr::filter(!(!!col_to_resolve %in% resolved_names))

  # Pull single-hit synonyms. All we have to do is look these up.
  single_synonyms <-
    synonyms %>%
    dplyr::filter(n_hits == 1)

  # Multiple-hit synonyms include ambiguous ones.
  #
  # Need to resolve these manually.
  multiple_synonyms <-
    synonyms %>%
    dplyr::filter(n_hits > 1)

  # Match single synoyms to accepted names. Lookup by matching acceptedNameUsageID
  # of synonym to taxonID of accepted name.
  # This means taxonomicStatus as "synonym" refers to the original queried name,
  # but the rest of the taxonomic data is for the matched, accepted name.
  resolved_synonyms <-
    dplyr::left_join(
      dplyr::select(single_synonyms, query, n_hits, distance, match_to, match_by,
                    taxonID = acceptedNameUsageID, taxonomicStatus),
      dplyr::select(taxonomic_standard, -acceptedNameUsageID, -taxonomicStatus),
      by = "taxonID"
    )

  # Combine resolved matches
  resolved_matches <- dplyr::bind_rows(accepted, resolved_synonyms)

  # Optionally exclude names after checking if they should not be accepted
  if (!is.null(exclude)) {

    misfits <- exclude[!exclude %in% pull(resolved_matches, !!col_to_resolve)]

    assertthat::assert_that(
      length(misfits) == 0,
      msg = warning(
        glue::glue("The following names in 'exclude' NOT in 'col_to_resolve': {paste(misfits, collapse = ', ')}")
      )
    )

    resolved_matches <-
      resolved_matches %>%
      dplyr::filter(!(!!col_to_resolve %in% exclude))

  }

  # Optionally simplify columns
  if(isTRUE(simple)) resolved_matches <- dplyr::select(
    resolved_matches, c(match_cols,  darwin_simple_cols)
  )

  if(isTRUE(simple)) multiple_synonyms <- dplyr::select(
    multiple_synonyms, c(match_cols,  darwin_simple_cols)
  )

  # Combine these into final result
  list(
    resolved_matches = resolved_matches,
    multiple_synonyms = multiple_synonyms
  )

}
