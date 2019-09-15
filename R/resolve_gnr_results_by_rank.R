resolve_gnr_results_by_rank <- function (
  matched_names,
  taxonomic_standard,
  resolve_by_taxon = TRUE,
  resolve_by_species = TRUE,
  resolve_to = c("species", "scientific_name")) {

  # Check that format of taxonomic standard meets Darwin Core
  check_darwin_core_format(taxonomic_standard)

  # Check format of matched names
  assertthat::assert_that(nrow(matched_names) > 0)
  assertr::assert(matched_names, is.character, query, matched_name) %>%
    assertr::assert(assertr::not_na, query, matched_name, success_fun = assertr::success_logical)

  # Rename "query" column of matched_names to "gnr_query", since output of
  # resolve_names_full() will also have a "query" column
  matched_names <- matched_names %>%
    dplyr::rename(gnr_query = query)

  ### Resolve names to taxonomic standard ###
  # scientificName is the names matched to in taxonomic standard.
  # If the original name was a
  # synonym, the accepted names are looked up and returned.
  # taxonomicStatus refers to the status of the matched name.
  resolve_results <- resolve_names_full (
    names_to_resolve = unique(matched_names$matched_name),
    taxonomic_standard = world_ferns,
    resolve_by_taxon = resolve_by_taxon,
    resolve_by_species = resolve_by_species,
    max_dist = c(scientific_name = 0, taxon = 0, species = 0)
    ) %>%
    dplyr::select(query, taxonomicStatus, scientificName) %>%
    check_unique(query) %>%
    # Join back in name used for matching with GNR
    dplyr::left_join(
      dplyr::select(matched_names, gnr_query, matched_name),
      by = c(query = "matched_name")
    ) %>%
    # Add species name
    add_parsed_names(sci_name = scientificName, species)

  ### Resolve names based on scientific name ###

  # Collapse names and remove names that didn't match
  collapsed_resolved_sci_names <-
    resolve_results %>%
    dplyr::group_by(gnr_query, scientificName) %>%
    dplyr::summarize(
      taxonomicStatus = paste(unique(taxonomicStatus), collapse = ", ")
    ) %>% dplyr::ungroup() %>%
    dplyr::filter(!is.na(scientificName))

  # Any gnr_query matching a single name is resolved
  resolved_sci_names <- collapsed_resolved_sci_names %>%
    dplyr::filter(assertr::is_uniq(gnr_query)) %>%
    check_unique(gnr_query)

  # Any gnr_query matching multiple distinct names is fails
  fail_mult_match_sci_names <- collapsed_resolved_sci_names %>%
    dplyr::filter(!assertr::is_uniq(gnr_query)) %>%
    dplyr::select(gnr_query) %>%
    unique() %>%
    dplyr::mutate(fail_reason = "Matches multiple distinct sci names") %>%
    check_unique(gnr_query)

  # These should be distinct
  assertthat::assert_that(
    nrow(dplyr::inner_join(fail_mult_match_sci_names, resolved_sci_names, by = "gnr_query")) == 0
  )

  # Any name not a resolved match or match to multiples is missing
  fail_match_no_sci_names <-
  matched_names %>%
    dplyr::anti_join(resolved_sci_names, by = "gnr_query") %>%
    dplyr::anti_join(fail_mult_match_sci_names, by = "gnr_query") %>%
    dplyr::select(gnr_query) %>%
    unique %>%
    dplyr::mutate(fail_reason = "Fuzzy match lacks col_plants sciname match") %>%
    check_unique(gnr_query)

  ### Resolve names based on species ###

  # Collapse names and remove names that didn't match
  collapsed_resolved_species <-
    resolve_results %>%
    dplyr::group_by(gnr_query, species) %>%
    dplyr::summarize(
      taxonomicStatus = paste(unique(taxonomicStatus), collapse = ", ")
    ) %>% dplyr::ungroup() %>%
    dplyr::filter(!is.na(species))

  # Any gnr_query matching a single name is resolved
  resolved_species <- collapsed_resolved_species %>%
    dplyr::filter(assertr::is_uniq(gnr_query)) %>%
    check_unique(gnr_query)

  # Any gnr_query matching multiple distinct names is fails
  fail_mult_match_species <- collapsed_resolved_species %>%
    dplyr::filter(!assertr::is_uniq(gnr_query)) %>%
    dplyr::select(gnr_query) %>%
    unique() %>%
    dplyr::mutate(fail_reason = "Match multiple distinct species") %>%
    check_unique(gnr_query)

  # These should be distinct
  assertthat::assert_that(
    nrow(dplyr::inner_join(resolved_species, fail_mult_match_species, by = "gnr_query")) == 0
  )

  # Any name not a resolved match or match to multiples is missing
  fail_match_no_species <-
    matched_names %>%
    dplyr::anti_join(resolved_species, by = "gnr_query") %>%
    dplyr::anti_join(fail_mult_match_species, by = "gnr_query") %>%
    dplyr::select(gnr_query) %>%
    unique %>%
    dplyr::mutate(fail_reason = "Fuzzy match lacks col_plants species match") %>%
    check_unique(gnr_query)

  ### Combine and return results ###
  results_sci_name <- dplyr::bind_rows(
    resolved_sci_names,
    fail_mult_match_sci_names,
    fail_match_no_sci_names
  )  %>%
    check_unique(gnr_query)

  results_species <- dplyr::bind_rows(
    resolved_species,
    fail_mult_match_species,
    fail_match_no_species
  )  %>%
    check_unique(gnr_query)

  # Select results
  results <- switch(resolve_to,
                    scientific_name = results_sci_name,
                    species =  results_species,
  )

  # Conduct final check: make sure all original names are in the results
  assertthat::assert_that(
    isTRUE(
      all.equal(
        sort(unique(results$gnr_query)),
        sort(unique(matched_names$gnr_query))
      )
    )
  )

  dplyr::rename(results, query = gnr_query)

}
