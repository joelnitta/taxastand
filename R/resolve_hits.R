#' Resolve hits from taxonomic matching of a single name
#'
#' @param hits Dataframe; hits from taxonomic matching - output of
#' match_taxonomy().
#' @param col_to_resolve Name of column including taxonomic name to resolve.
#' @param taxonomic_standard Dataframe of standard names to match to.
#' @param accepted_singles_to_exclude Character vector; species that
#' should be excluded from single, accepted taxa.
#' @param accepted_mults_to_exclude Character vector; species that
#' should be excluded from multiple, accepted taxa.
#' @param synonym_singles_to_exclude Character vector; species that
#' should be excluded from single synonyms.
#' @param synonym_mults_to_exclude Character vector; species that
#' should be excluded from multiple synonyms.
#' @param mult_syn_selection Character vector of species names that should be
#' selected from those with multiple synonyms to use as the synonym
#' for that species.
#' @param exclude Character vector; taxa that should be exluded from the final
#' results.
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
                          accepted_singles_to_exclude = NULL,
                          accepted_mults_to_exclude = NULL,
                          synonym_singles_to_exclude = NULL,
                          synonym_mults_to_exclude = NULL,
                          mult_syn_selection = NULL,
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

  # Need to add check here for "normal" Dariwn Core standard

  # Add non-standard columns (genericName, speciesName, taxonName)
  taxonomic_standard <- add_non_darwin_core_cols(taxonomic_standard)
  hits <- add_non_darwin_core_cols(hits)

  # Convert ID columns to integer
  taxonomic_standard <- dplyr::mutate_at(
    taxonomic_standard,
    c("taxonID", "acceptedNameUsageID"),
    as.integer)

  hits <- dplyr::mutate_at(
    hits,
    c("taxonID", "acceptedNameUsageID"),
    as.integer)

  # Check format of names standard input
  checkr::check_data(taxonomic_standard, values = list(
    taxonID = 1L,
    acceptedNameUsageID = c(1L, NA),
    taxonomicStatus = "a",
    scientificName = "a",
    genericName = "a",
    specificEpithet = c("a", NA),
    infraspecificEpithet = c("a", NA),
    taxonName = "a"),
    key = "taxonID")

  # Check format of names standard input
  checkr::check_data(hits, values = list(
    taxonID = 1L,
    acceptedNameUsageID = c(1L, NA),
    taxonomicStatus = "a",
    scientificName = "a",
    genericName = "a",
    specificEpithet = c("a", NA),
    infraspecificEpithet = c("a", NA),
    taxonName = "a"),
    key = "taxonID")

  # Categorize hits
  #
  # ## Accepted names
  #
  # Those with a single accepted name are done
  accepted_singles <-
    hits %>%
    dplyr::filter(stringr::str_detect(taxonomicStatus, "accepted"), n_hits == 1)

  # Optionally exclude names after checking if they should not be accepted
  if (!is.null(accepted_singles_to_exclude)) {

    misfits <- accepted_singles_to_exclude[!accepted_singles_to_exclude %in% dplyr::pull(accepted_singles, !!col_to_resolve)]

    assertthat::assert_that(
      length(misfits) == 0,
      msg = warning(
        glue::glue("The following 'accepted_singles_to_exclude' NOT in 'col_to_resolve': {paste(misfits, collapse = ', ')}")
      )
    )

    accepted_singles <-
      accepted_singles %>%
      dplyr::filter(!(!!col_to_resolve %in% accepted_singles_to_exclude))

  }

  # For those with multiple hits to an accepted name, take the closest match.
  accepted_resolved_mults <-
    hits %>%
    dplyr::filter(stringr::str_detect(taxonomicStatus, "accepted"), n_hits > 1) %>%
    dplyr::arrange(!!col_to_resolve, distance) %>%
    dplyr::group_by(!!col_to_resolve) %>%
    dplyr::slice(1)

  # Optionally exclude names after checking if they should not be accepted
  if (!is.null(accepted_mults_to_exclude)) {

    misfits <- accepted_mults_to_exclude[!accepted_mults_to_exclude %in% dplyr::pull(accepted_resolved_mults, !!col_to_resolve)]

    assertthat::validate_that(
      length(misfits) == 0,
      msg = warning(
        glue::glue("The following 'accepted_mults_to_exclude' NOT in 'col_to_resolve': {paste(misfits, collapse = ', ')}")
      )
    )

    accepted_resolved_mults <-
      accepted_resolved_mults %>%
      dplyr::filter(!(!!col_to_resolve %in% accepted_mults_to_exclude))
  }

  accepted <- dplyr::bind_rows(accepted_singles, accepted_resolved_mults)

  # ## Synonyms
  #
  # Synonyms are anything with a hit that has not been resolved yet.
  resolved_names <- dplyr::pull(accepted, !!col_to_resolve)

  synonyms <-
    hits %>%
    dplyr::filter(!(!!col_to_resolve %in% resolved_names))

  # Tweak list to exclude species on exclude list
  if(!is.null(accepted_singles_to_exclude)) {
    synonyms <- dplyr::filter(synonyms, !(!!col_to_resolve %in% accepted_singles_to_exclude))
  }

  if(!is.null(accepted_mults_to_exclude)) {
    synonyms <- dplyr::filter(synonyms, !(!!col_to_resolve %in% accepted_mults_to_exclude))
  }

  # Pull single-hit synonyms. All we have to do is look these up.
  single_synonyms <-
    synonyms %>%
    dplyr::filter(n_hits == 1)

  # Optionally exclude names after checking if they should not be accepted
  if (!is.null(synonym_singles_to_exclude)) {

    misfits <- synonym_singles_to_exclude[!synonym_singles_to_exclude %in% dplyr::pull(single_synonyms, !!col_to_resolve)]

    assertthat::validate_that(
      length(misfits) == 0,
      msg = warning(
        glue::glue("The following 'synonym_singles_to_exclude' NOT in 'col_to_resolve': {paste(misfits, collapse = ', ')}")
      )
    )

    single_synonyms <-
      single_synonyms %>%
      dplyr::filter(!(!!col_to_resolve %in% synonym_singles_to_exclude))

  }

  # Multiple-hit synonyms include ambiguous ones.
  #
  # Need to resolve these manually.
  multiple_synonyms <-
    synonyms %>%
    dplyr::filter(n_hits > 1)

  # Optionally exclude names after checking if they should not be accepted
  if (!is.null(synonym_mults_to_exclude)) {

    misfits <- synonym_mults_to_exclude[!synonym_mults_to_exclude %in% pull(multiple_synonyms, !!col_to_resolve)]

    assertthat::validate_that(
      length(misfits) == 0,
      msg = warning(
        glue::glue("The following 'synonym_mults_to_exclude' NOT in 'col_to_resolve': {paste(misfits, collapse = ', ')}")
      )
    )

    multiple_synonyms <-
      multiple_synonyms %>%
      dplyr::filter(!(!!col_to_resolve %in% synonym_mults_to_exclude))

  }

  # In some cases, multiple syonyms may include only a single correct synonym.
  # Manually check this and add to "single" synonyms
  if (!is.null(mult_syn_selection)) {

    assertthat::assert_that(
      all(mult_syn_selection %in% multiple_synonyms$scientificName),
      msg = glue("Names in mult_syn_selection not in multiple_synonyms$scientificName: {paste(setdiff(mult_syn_selection, multiple_synonyms$scientificName), collapse = ', ' )}"))

    single_synonyms <-
      dplyr::bind_rows(single_synonyms,
                       dplyr::filter(
                         multiple_synonyms,
                         scientificName %in% mult_syn_selection)
      )

    multiple_synonyms <-
      dplyr::filter(
        multiple_synonyms,
        !(!!col_to_resolve %in% dplyr::pull(single_synonyms, !!col_to_resolve))
      )

  }

  # Match single synoyms to accepted names. Lookup by matching acceptedNameUsageID
  # of synonym to taxonID of accepted name.
  # This means taxonomicStatus as "synonym" refers to the original queried name,
  # but the rest of the taxonomic data is for the matched, accepted name.
  resolved_synonyms <-
    dplyr::left_join(
      dplyr::select(single_synonyms, query, n_hits, distance, match_to, match_by,
                    taxonID = acceptedNameUsageID, taxonomicStatus),
      dplyr::select(taxonomic_standard, -acceptedNameUsageID, -taxonomicStatus)
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
