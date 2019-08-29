#' Sort through and resolve hits from taxonomic matching
#'
#' @param hits Dataframe; hits from taxonomic matching - output of
#' match_taxonomy().
#' @param col_to_resolve Name of column including taxonomic name to resolve.
#' @param names_standard Dataframe of standard names to match to. Here we use
#' the World Ferns database.
#' @param accepted_singles_to_exclude Character vector of length one; species that
#' should be excluded from single, accepted taxa. Each taxon name should be
#' separated by |, e.g.: "Pteris cretica|Pteris vittata".
#' @param accepted_mults_to_exclude Character vector of length one; species that
#' should be excluded from multiple, accepted taxa.
#' @param synonym_singles_to_exclude Character vector of length one; species that
#' should be excluded from single synonyms.
#' @param synonym_mults_to_exclude Character vector of length one; species that
#' should be excluded from multiple synonyms.
#' @param mult_syn_selection Character vector of species names that should be
#' selected from those with multiple synonyms to use as the synonym
#' for that species.
#'
#' @return List including two items; `resolved_matches` is dataframe of resolved
#' names (including both accepted named and synonyms); `multiple_synonyms` are taxa
#' with multiple synonyms that could not be resolved.
#'
resolve_hits <- function (hits, col_to_resolve, names_standard,
                          accepted_singles_to_exclude = NULL,
                          accepted_mults_to_exclude = NULL,
                          synonym_singles_to_exclude = NULL,
                          synonym_mults_to_exclude = NULL,
                          mult_syn_selection = NULL,
                          exclude = NULL) {

  col_to_resolve <- as.name(col_to_resolve)

  checkr::check_data(names_standard, values = list(
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
  # Those with a single accepted name are good to go (check first though!)
  accepted_singles <-
    hits %>%
    filter(str_detect(taxonomicStatus, "accepted"), n == 1)

  # Optionally exclude names after checking if they should not be accepted
  if (!is.null(accepted_singles_to_exclude)) {

    misfits <- accepted_singles_to_exclude[!accepted_singles_to_exclude %in% pull(accepted_singles, !!col_to_resolve)]

    assertthat::assert_that(
      length(misfits) == 0,
      msg = warning(
        glue::glue("The following 'accepted_singles_to_exclude' NOT in 'col_to_resolve': {paste(misfits, collapse = ', ')}")
      )
    )

    accepted_singles <-
      accepted_singles %>%
      filter(!(!!col_to_resolve %in% accepted_singles_to_exclude))

  }

  # For those with multiple hits to an accepted name, take the closest match.
  accepted_resolved_mults <-
    hits %>%
    filter(str_detect(taxonomicStatus, "accepted"), n > 1) %>%
    arrange(!!col_to_resolve, distance) %>%
    group_by(!!col_to_resolve) %>%
    slice(1)

  # Optionally exclude names after checking if they should not be accepted
  if (!is.null(accepted_mults_to_exclude)) {

    misfits <- accepted_mults_to_exclude[!accepted_mults_to_exclude %in% pull(accepted_resolved_mults, !!col_to_resolve)]

    assertthat::validate_that(
      length(misfits) == 0,
      msg = warning(
        glue::glue("The following 'accepted_mults_to_exclude' NOT in 'col_to_resolve': {paste(misfits, collapse = ', ')}")
      )
    )

    accepted_resolved_mults <-
      accepted_resolved_mults %>%
      filter(!(!!col_to_resolve %in% accepted_mults_to_exclude))
  }

  accepted <- bind_rows(accepted_singles, accepted_resolved_mults)

  # ## Synonyms
  #
  # Synonyms are anything with a hit that has not been resolved yet.
  resolved_names <- pull(accepted, !!col_to_resolve)

  synonyms <-
    hits %>%
    filter(!(!!col_to_resolve %in% resolved_names))

  # Tweak list to exclude species on exclude list
  if(!is.null(accepted_singles_to_exclude)) {
    synonyms <- filter(synonyms, !(!!col_to_resolve %in% accepted_singles_to_exclude))
  }

  if(!is.null(accepted_mults_to_exclude)) {
    synonyms <- filter(synonyms, !(!!col_to_resolve %in% accepted_mults_to_exclude))
  }

  # Pull single-hit synonyms. All we have to do is look these up.
  single_synonyms <-
    synonyms %>%
    filter(n == 1)

  # Optionally exclude names after checking if they should not be accepted
  if (!is.null(synonym_singles_to_exclude)) {

    misfits <- synonym_singles_to_exclude[!synonym_singles_to_exclude %in% pull(single_synonyms, !!col_to_resolve)]

    assertthat::validate_that(
      length(misfits) == 0,
      msg = warning(
        glue::glue("The following 'synonym_singles_to_exclude' NOT in 'col_to_resolve': {paste(misfits, collapse = ', ')}")
      )
    )

    single_synonyms <-
      single_synonyms %>%
      filter(!(!!col_to_resolve %in% synonym_singles_to_exclude))

  }

  # Multiple-hit synonyms include ambiguous ones.
  #
  # Need to resolve these manually.
  multiple_synonyms <-
    synonyms %>%
    filter(n > 1)

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
      filter(!(!!col_to_resolve %in% synonym_mults_to_exclude))

  }

  # In some cases, multiple syonyms may include only a single correct synonym.
  # Manually check this and add to "single" synonyms
  if (!is.null(mult_syn_selection)) {

    assertthat::assert_that(
      all(mult_syn_selection %in% multiple_synonyms$scientificName),
      msg = glue("Names in mult_syn_selection not in multiple_synonyms$scientificName: {paste(setdiff(mult_syn_selection, multiple_synonyms$scientificName), collapse = ', ' )}"))

    single_synonyms <-
      bind_rows(single_synonyms,
                filter(
                  multiple_synonyms,
                  scientificName %in% mult_syn_selection)
      )

    multiple_synonyms <-
      filter(
        multiple_synonyms,
        !(!!col_to_resolve %in% pull(single_synonyms, !!col_to_resolve))
      )

  }

  # Match single synoyms to accepted names. Lookup by matching acceptedNameUsageID
  # of synonym to taxonID of accepted name.
  cols_drop <- colnames(names_standard)[!colnames(names_standard) %in% c("acceptedNameUsageID", "taxonomicStatus", "matched_name")]

  resolved_synonyms <-
    select(single_synonyms, -cols_drop) %>%
    left_join(
      select(names_standard, -acceptedNameUsageID, -taxonomicStatus),
      by = (c("acceptedNameUsageID" = "taxonID"))
    )

  resolved_matches <- bind_rows(accepted, resolved_synonyms)

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
      filter(!(!!col_to_resolve %in% exclude))

  }

  # Combine these into final result
  list(
    resolved_matches = resolved_matches,
    multiple_synonyms = multiple_synonyms
  )

}
