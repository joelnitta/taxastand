#' Resolve names of ferns and lycophytes (pteridophytes)
#'
#' Excludes hybrids and any non-pteridophyte genera, and resolves names to
#' the [World Ferns](https://worldplants.webarchiv.kit.edu/ferns/) list
#' contained within the [Catalog of Life](http://www.catalogueoflife.org/).
#'
#' This takes ca. 10 minutes for 25,000 names.
#'
#' @param col_plants Catalog of Life database subsetted to tracheophytes, in
#' Darwin Core format. This can be downloaded as part of a zip archive from
#' [here](http://www.catalogueoflife.org/DCA_Export/zip/archive-kingdom-plantae-phylum-tracheophyta-bl3.zip).
#' The unzipped file contains `taxa.txt`, which is a tab-separated file
#' including 1,168,025 observations of 31 variables, mostly taxonomic ranks
#' and names. `taxa.txt` should be read-in using `readr::read_tsv()` or similar,
#' and used as `col_plants`.
#' @param names Vector of scientific names to resolve
#' @param resolve_to String, either "scientific_name" or "species". The type of
#' name that should be resolved. If "species", queries matching different varieties
#' within the same species will be collapsed to a single species.
#'
#' @return Tibble with the following columns
#' - query: the query (`names`) after slight formatting (removing quotation marks)
#' - query_original: the original query (`names`)
#' - non_pterido_genus: logical flag if name matched a non-pterdidophyte genus
#' - taxonomicStatus: taxonomicStatus of the queried name
#' - scientificName, genus, specificEpithet, species, infraspecificEpithet:
#' matched, resolved names (i.e., queries that were synonyms have been
#' replaced with the accepted name),
#'
#' @examples
#' \dontrun{
#' # Specify location of Catalog of Life subsetted to tracheophytes
#' col_file <- "/Volumes/Transcend/Projects/pacific_ferns_biogeo/data_raw/archive-kingdom-plantae-phylum-tracheophyta-bl3/taxa.txt"
#'
#' # Read in CoL. First get colnames, and use these to specify column type.
#' # taxonID, acceptedNameUsageID, parentNameUsageID need to be numeric.
#' col_plants_cols <- readr::read_tsv(col_file, n_max = 1) %>% colnames
#'
#' col_spec <- dplyr::case_when(
#'   stringr::str_detect(col_plants_cols, "taxonID|acceptedNameUsageID|parentNameUsageID") ~ "n",
#'   TRUE ~ "c"
#' ) %>% paste(collapse = "")
#'
#' col_plants <- readr::read_tsv(
#'   col_file,
#'   col_types = col_spec)
#'
#' names <- c("abacopteris insularis",
#'   "abacopteris philippinarum",
#'   "abacopteris rubra var. hirsuta",
#'   "acrostichum aureum",
#'   "cystopteris fragilis",
#'   "Anemia stricta",
#'   "Athyrium macrocarpum",
#'   "Bulboschoenus fluviatilis",
#'   "Athyrium whazzat")
#'
#' resolve_fern_names(names, col_plants, "scientific_name")
#' resolve_fern_names(names, col_plants, "species")
#'
#' }
#'
#' @export
resolve_fern_names <- function (names, col_plants, resolve_to = c("species", "scientific_name")) {

  assertthat::assert_that(is.character(names))
  assertthat::assert_that(
    all(assertr::is_uniq(names)),
    msg = "`names` can only include unique values"
  )
  assertthat::assert_that(
    !anyNA(names),
    msg = "`names` cannot include any missing values")
  assertthat::assert_that(assertthat::is.string(resolve_to))
  assertthat::assert_that(
    resolve_to %in% c("species", "scientific_name"),
    msg = "`resolve_to` must be one of 'species' or 'scientific_name'")

  check_darwin_core_cols(col_plants)

  ### Format input data ###

  # Convert input name vector to tibble
  names_raw <- tibble::tibble(original_name = names)

  # Format names for resolving
  names <-
    names_raw %>%
    # Verify all scientific names are unique
    assertr::assert(assertr::is_uniq, original_name) %>%
    # Remove quotation marks
    dplyr::mutate(gnr_query = stringr::str_remove_all(original_name, '\\"')) %>%
    # Convert gnr_query to uppercase
    # (don't use stringr::str_to_sentence, will convert other letters to lower)
    dplyr::mutate(gnr_query = toupper_first(gnr_query)) %>%
    # Add genus
    dplyr::mutate(genus = genus_name_only(gnr_query))

  ### SUMMARY OF RESOLVING STRATEGY ###
  #
  # Each name on a line is a dataframe of names, which is split finer on the next line.
  # At the end, dataframes with (F) and (S) will be combined and returned.
  #
  # Key:
  # - (F) are failures
  # - (S) are successes
  # - * indicates dataframe that includes non-unique query names (`gnr_query`)
  #
  # names
  #	  excluded (F)
  #   pterido_names
  #     pterido_names_exact_match (S)
  #     pterido_names_no_exact_match
  #	      pterido_names_no_fuzzy_match (F)
  #	      pterido_names_fuzzy_match *
  #		      not_resolved (F)
  #		      pterido_names_resolved *
  #			       pterido_names_resolved_single_matches (S)
  #			       pterido_names_resolved_mult_matches *
  #
  #			  	   pterido_names_mult_matches_resolve_to_diff_name (F)
  #			  	   pterido_names_mult_matches_resolve_to_same_name (S)
  #
  #			  	                         OR
  #
  #			  	   pterido_names_mult_matches_resolve_to_diff_species (F)
  #			  	   pterido_names_mult_matches_resolve_to_same_species (S)

  ### Exclude non-pteridophyte genera ###

  # Make a tibble of all genera in original names
  genera <-
    names %>%
    dplyr::pull(genus) %>%
    unique %>%
    sort %>%
    tibble::tibble(genus = .) %>%
    dplyr::filter(genus != "")

  # Make a vector of pteridophyte genera
  # First pull out world ferns from all COL plants
  world_ferns <- dplyr::filter(
    col_plants,
    stringr::str_detect(datasetName, "World Ferns")
  )

  pterido_genera <- unique(world_ferns$genericName) %>% sort

  # Make a tibble of all genera of plants in Catalog of Life.
  # Add `exclude_non_pterido_genus` column to indicate that
  # name should be excluded because
  # it matched a non-pteridophyte genus.
  col_plants_genera <-
    col_plants %>%
    dplyr::filter(!is.na(genericName)) %>%
    dplyr::select(genus = genericName) %>%
    unique %>%
    dplyr::mutate(exclude_non_pterido_genus = dplyr::case_when(
      genus %in% pterido_genera ~ FALSE,
      TRUE ~ TRUE
    ))

  # Conservatively say that if a genus did not match to anything in
  # CoL plants that non_pterido_genus is FALSE
  names <- dplyr::left_join(names, col_plants_genera, by = "genus") %>%
    dplyr::mutate(exclude_non_pterido_genus = tidyr::replace_na(exclude_non_pterido_genus, FALSE))

  ### Exclude hybrids ###

  names <-
    names %>%
    dplyr::mutate(exclude_hybrid = dplyr::case_when(
      stringr::str_detect(gnr_query, " x ") ~ TRUE,
      stringr::str_detect(gnr_query, "\\u00d7") ~ TRUE, # unicode for batsu
      TRUE ~ FALSE
    ))

  ### Exclude anything not identified to species ###

  names <-
    names %>%
    dplyr::mutate(exclude_no_id_to_sp = dplyr::case_when(
      # Need at least one space in name (between genus and species)
      stringr::str_count(gnr_query, " ") == 0 ~ TRUE,
      stringr::str_detect(gnr_query, " sp$| sp\\.$") ~ TRUE,
      TRUE ~ FALSE
    ))

  ### Make df of excluded names to add back to results at end ###

  excluded <- names %>%
    dplyr::filter(exclude_non_pterido_genus | exclude_hybrid | exclude_no_id_to_sp) %>%
    dplyr::select(gnr_query, exclude_non_pterido_genus, exclude_hybrid, exclude_no_id_to_sp)

  ### Make list of pteridophyte names for matching ###

  pterido_names <-
    names %>%
    dplyr::filter(!exclude_non_pterido_genus) %>%
    dplyr::filter(!exclude_hybrid) %>%
    dplyr::filter(!exclude_no_id_to_sp) %>%
    dplyr::pull(gnr_query) %>%
    unique

  # Exit with error if no pteridophyte names to match
  if(length(pterido_names) == 0) {
    stop("No valid pteridophyte names detected")
  }

  ### Match by exact names ###
  print("Resolving names by exact matching")
  exact_names_resolve_results <- resolve_names_full(
    names_to_resolve = pterido_names,
    taxonomic_standard = world_ferns,
    resolve_by_species = FALSE) %>%
    dplyr::select(gnr_query = query, taxonomicStatus, scientificName)

  pterido_names_exact_match <-
    exact_names_resolve_results %>%
    dplyr::filter(!is.na(scientificName)) %>%
    check_unique(gnr_query)

  if(nrow(pterido_names_exact_match) > 0)
    pterido_names_exact_match <- add_parsed_names(
      pterido_names_exact_match, scientificName, species) %>%
    dplyr::mutate(match_type = "exact")

  pterido_names_no_exact_match <-
    exact_names_resolve_results %>%
    dplyr::filter(is.na(scientificName))

  # Early exit if all names have been resolved by exact matching
  if(nrow(pterido_names_no_exact_match) == 0) {

    # Version for returning species names (no infrasp. epithets)
    match_and_resolve_results_species <-
      dplyr::bind_rows(
        # Successes
        dplyr::select(pterido_names_exact_match, -scientificName),
        # Failures
        excluded
      )

    match_and_resolve_results_sciname <-
      dplyr::bind_rows(
        # Successes
        dplyr::select(pterido_names_exact_match, -species),
        # Failures
        excluded
      )

    # Choose what to type of resuls to return
    # (resolved to scientific name or species)
    results <- switch(resolve_to,
                      scientific_name = match_and_resolve_results_sciname,
                      species =  match_and_resolve_results_species,
    ) %>%
      # Join back in original query names
      dplyr::left_join(dplyr::select(names, gnr_query, query = original_name), by = "gnr_query") %>%
      dplyr::select(-gnr_query) %>%
      # Rearrange columns
      dplyr::select(
        query,
        exclude_non_pterido_genus, exclude_hybrid, exclude_no_id_to_sp,
        dplyr::everything()) %>%
      # Fill-in missing NAs
      dplyr::mutate_at(dplyr::vars(dplyr::contains("exclude")), ~tidyr::replace_na(., FALSE))

    # Conduct final check: make sure all original names are in the results
    assertthat::assert_that(
      isTRUE(
        all.equal(
          sort(results$query),
          sort(names_raw$original_name)
        )
      )
    )

    return(results)

  }

  ### Match pteridophyte names with GNR ###
  print("Resolving names by fuzzy matching")
  gnr_results <- match_with_gnr(pterido_names_no_exact_match$gnr_query, exclude_mult_matches = FALSE) %>%
    # Exit with error if no names matched
    assertr::verify(
      nrow(.) > 0,
      error_fun = function (errors, data = NULL) {stop("No names matched")}) %>%
    dplyr::rename(gnr_query = query)

  # Split GNR results into names not matched and matched
  pterido_names_no_fuzzy_match <- gnr_results %>%
    dplyr::filter(!is.na(fail_reason)) %>%
    dplyr::select(gnr_query, fail_reason) %>%
    check_unique(gnr_query)

  # (matched still includes things with multiple matches)
  pterido_names_matched <- gnr_results %>% dplyr::filter(is.na(fail_reason))

  ### Resolve synonyms ###
  # scientificName is the names matched to in World Ferns.
  # If the original name was a
  # synonym, the accepted names are looked up and returned.
  # taxonomicStatus refers to the status of the matched name.
  pterido_names_fuzzy_match <- resolve_names_full(
    unique(pterido_names_matched$matched_name), world_ferns) %>%
    dplyr::select(
      resolve_query = query, taxonomicStatus,
      scientificName, genus, specificEpithet, infraspecificEpithet) %>%
    # Add back in the gnr query column
    dplyr::left_join(
      dplyr::select(gnr_results, gnr_query, matched_name),
      by = c(resolve_query = "matched_name")) %>%
    dplyr::select(gnr_query, resolve_query, taxonomicStatus, scientificName)

  # Split into matched names that were resolved to something,
  # and those that could not be resolved.
  not_resolved <- pterido_names_fuzzy_match %>%
    dplyr::filter(is.na(scientificName)) %>%
    dplyr::select(gnr_query, taxonomicStatus) %>%
    unique %>%
    check_unique(gnr_query)

  # Add fail reason for unresolved names.
  if(nrow(not_resolved) > 0)
    not_resolved <- not_resolved %>%
    dplyr::mutate(fail_reason = "Fuzzy matched name not in col_plants")

  pterido_names_resolved <- pterido_names_fuzzy_match %>%
    dplyr::anti_join(not_resolved, by = "gnr_query")

  if(nrow(pterido_names_resolved) > 0)
    pterido_names_resolved <- pterido_names_resolved %>%
    add_parsed_names(scientificName, species) %>%
    dplyr::mutate(match_type = "fuzzy")

  # Split resolved names into those that had a single GNR match
  # and those that had multiple ones.
  pterido_names_resolved_single_matches <-
    pterido_names_resolved %>%
    dplyr::filter(assertr::is_uniq(gnr_query)) %>%
    check_unique(gnr_query)

  pterido_names_resolved_mult_matches <-
    pterido_names_resolved %>%
    dplyr::filter(!assertr::is_uniq(gnr_query))

  ### Split multiple matches resolving to the same or different scientific name
  # Those resolving to same name are successes,
  # those resolving to different names are failures

  # Populate some empty tibbles so we can bind_rows
  # on everything in the end, even if these have zero names.
  pterido_names_resolved_mult_matches_by_sciname <- tibble::tibble()
  pterido_names_mult_matches_resolve_to_diff_name <- tibble::tibble()
  pterido_names_mult_matches_resolve_to_same_name <- tibble::tibble()

  if (nrow(pterido_names_resolved_mult_matches) > 0)
    pterido_names_resolved_mult_matches_by_sciname <-
    pterido_names_resolved_mult_matches %>%
    dplyr::group_by(gnr_query, scientificName) %>%
    dplyr::summarize(
      taxonomicStatus = paste(unique(taxonomicStatus), collapse = ", ")
    ) %>% dplyr::ungroup()

  if(nrow(pterido_names_resolved_mult_matches_by_sciname) > 0)
    pterido_names_mult_matches_resolve_to_diff_name <-
    pterido_names_resolved_mult_matches_by_sciname %>%
    dplyr::filter(!assertr::is_uniq(gnr_query)) %>%
    dplyr::group_by(gnr_query) %>%
    dplyr::summarize(
      taxonomicStatus = paste(unique(taxonomicStatus), collapse = ", ")
    ) %>% dplyr::ungroup() %>%
    check_unique(gnr_query) %>%
    dplyr::mutate(
      fail_reason = "Fuzzy matched sci name matches multiple sci names in col_plants")

  if(nrow(pterido_names_resolved_mult_matches_by_sciname) > 0)
    pterido_names_mult_matches_resolve_to_same_name <-
    pterido_names_resolved_mult_matches_by_sciname %>%
    dplyr::filter(assertr::is_uniq(gnr_query)) %>%
    check_unique(gnr_query)

  ### Do same for species level:
  # Split multiple matches resolving to the same or different species.
  # Those resolving to same name are successes,
  # those resolving to different names are failures
  # In next step will choose to use scientific name or species based on `resolve_to`
  pterido_names_resolved_mult_matches_by_species <- tibble::tibble()
  pterido_names_mult_matches_resolve_to_diff_species <- tibble::tibble()
  pterido_names_mult_matches_resolve_to_same_species <- tibble::tibble()

  if(nrow(pterido_names_resolved_mult_matches) > 0)
    pterido_names_resolved_mult_matches_by_species <-
    pterido_names_resolved_mult_matches %>%
    dplyr::group_by(gnr_query, species) %>%
    dplyr::summarize(
      taxonomicStatus = paste(unique(taxonomicStatus), collapse = ", ")
    ) %>% dplyr::ungroup()

  if(nrow(pterido_names_resolved_mult_matches_by_species) > 0)
    pterido_names_mult_matches_resolve_to_diff_species <-
    pterido_names_resolved_mult_matches_by_species %>%
    dplyr::filter(!assertr::is_uniq(gnr_query)) %>%
    dplyr::group_by(gnr_query) %>%
    dplyr::summarize(
      taxonomicStatus = paste(unique(taxonomicStatus), collapse = ", ")
    ) %>% dplyr::ungroup() %>%
    check_unique(gnr_query) %>%
    dplyr::mutate(
      fail_reason = "Fuzzy matched species matches multiple species in col_plants")

  if(nrow(pterido_names_resolved_mult_matches_by_species) > 0)
    pterido_names_mult_matches_resolve_to_same_species <-
    pterido_names_resolved_mult_matches_by_species %>%
    dplyr::filter(assertr::is_uniq(gnr_query)) %>%
    check_unique(gnr_query)

  ### Combining matching and resolving results ###
  # Version for returning scientific names
  match_and_resolve_results_sciname <-
    dplyr::bind_rows(
      # Successes
      pterido_names_exact_match,
      pterido_names_mult_matches_resolve_to_same_name,
      pterido_names_resolved_single_matches,
      # Failures
      excluded,
      pterido_names_no_fuzzy_match,
      not_resolved,
      pterido_names_mult_matches_resolve_to_diff_name
    ) %>%
    dplyr::select(
      gnr_query,
      dplyr::contains("exclude"),
      dplyr::matches("^scientificName$"),
      dplyr::matches("^taxonomicStatus$"),
      dplyr::matches("^match_type$"),
      dplyr::matches("^fail_reason$")
    )

  # Version for returning species names (no infrasp. epithets)
  match_and_resolve_results_species <-
    dplyr::bind_rows(
      # Successes
      pterido_names_exact_match,
      pterido_names_mult_matches_resolve_to_same_species,
      pterido_names_resolved_single_matches,
      # Failures
      pterido_names_mult_matches_resolve_to_diff_species,
      not_resolved,
      pterido_names_no_fuzzy_match,
      excluded
    ) %>%
    dplyr::select(
      gnr_query,
      dplyr::contains("exclude"),
      dplyr::matches("^species$"),
      dplyr::matches("^taxonomicStatus$"),
      dplyr::matches("^match_type$"),
      dplyr::matches("^fail_reason$")
    )

  # Choose what to type of resuls to return
  # (resolved to scientific name or species)
  results <- switch(resolve_to,
                    scientific_name = match_and_resolve_results_sciname,
                    species =  match_and_resolve_results_species,
  ) %>%
    # Join back in original query names
    dplyr::left_join(dplyr::select(names, gnr_query, query = original_name), by = "gnr_query") %>%
    dplyr::select(-gnr_query) %>%
    # Rearrange columns
    dplyr::select(
      query,
      dplyr::contains("exclude"),
      dplyr::everything()) %>%
    # Fill-in missing NAs
    dplyr::mutate_at(dplyr::vars(dplyr::contains("exclude")), ~tidyr::replace_na(., FALSE))

  # Conduct final check: make sure all original names are in the results
  assertthat::assert_that(
    isTRUE(
      all.equal(
        sort(results$query),
        sort(names_raw$original_name)
      )
    )
  )

  results

}
