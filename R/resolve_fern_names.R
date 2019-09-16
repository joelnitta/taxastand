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
#'   "Cystopteris fragilis (L.) Bernh.",
#'   "Anemia stricta",
#'   "Athyrium macrocarpum",
#'   "Bulboschoenus fluviatilis",
#'   "Asplenium leptophyllum",
#'   "Asplenium leptophyllum baker",
#'   "Abrodictyum dentatum",
#'   "Abrodictyum dentatum Iwats.",
#'   "Polypodium vulgare",
#'   "Polypodium vulgare l. bak.",
#'   "Athyrium whazzat")
#'
#' resolve_fern_names(names, col_plants, "scientific_name")
#' resolve_fern_names(names, col_plants, "species")
#'
#' }
#'
#' @export
resolve_fern_names <- function (names, col_plants, resolve_to = c("species", "scientific_name")) {

  # Check input

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
    dplyr::mutate(clean_name = stringr::str_remove_all(original_name, '\\"')) %>%
    # Convert clean_name to uppercase
    # (don't use stringr::str_to_sentence, will convert other letters to lower)
    dplyr::mutate(clean_name = toupper_first(clean_name)) %>%
    # Add genus
    dplyr::mutate(genus = genus_name_only(clean_name))

  ### SUMMARY OF RESOLVING STRATEGY ###
  #
  # Each name on a line is a dataframe of names, which is split finer on the next line.
  # At the end, dataframes with (F) and (S) will be combined and returned.
  #
  # Key:
  # - (F) are failures
  # - (S) are successes
  # - * indicates dataframe that includes non-unique query names (`clean_name`)
  #
  # names
  #	  excluded (F)
  #   pterido_names
  #     pterido_names_exact_match (S)
  #     pterido_names_no_exact_match
  #	      pterido_names_no_fuzzy_match (F)
  #	      resolve_by_sciname_results *
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
  # First pull out world ferns from all COL plants,
  # only use names that include authorship
  world_ferns <- dplyr::filter(
    col_plants,
    stringr::str_detect(datasetName, "World Ferns")
  ) %>%
    dplyr::mutate(scientificNameAuthorship = dplyr::na_if(scientificNameAuthorship, "")) %>%
    dplyr::filter(!is.na(scientificNameAuthorship))

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
      stringr::str_detect(clean_name, " x ") ~ TRUE,
      stringr::str_detect(clean_name, "\\u00d7") ~ TRUE, # unicode for batsu
      TRUE ~ FALSE
    ))

  ### Exclude anything not identified to species ###

  names <-
    names %>%
    dplyr::mutate(exclude_no_id_to_sp = dplyr::case_when(
      # Need at least one space in name (between genus and species)
      stringr::str_count(clean_name, " ") == 0 ~ TRUE,
      stringr::str_detect(clean_name, " sp$| sp\\.$") ~ TRUE,
      TRUE ~ FALSE
    ))

  ### Make df of excluded names to add back to results at end ###

  excluded <- names %>%
    dplyr::filter(exclude_non_pterido_genus | exclude_hybrid | exclude_no_id_to_sp) %>%
    dplyr::select(clean_name, exclude_non_pterido_genus, exclude_hybrid, exclude_no_id_to_sp)

  ### Make list of pteridophyte names for matching ###

  pterido_names <-
    names %>%
    dplyr::filter(!exclude_non_pterido_genus) %>%
    dplyr::filter(!exclude_hybrid) %>%
    dplyr::filter(!exclude_no_id_to_sp) %>%
    dplyr::pull(clean_name) %>%
    unique

  # Exit with error if no pteridophyte names to match
  if(length(pterido_names) == 0) {
    stop("No valid pteridophyte names detected")
  }

  ### First pass using exact match on scientific name ###

  print("Resolving names by exact matching")

  exact_names_resolve_results <- resolve_names(
    names_to_resolve = pterido_names,
    taxonomic_standard = world_ferns,
    match_by = "scientific_name",
    max_dist = 0
    ) %>%
    dplyr::select(clean_name = query, taxonomicStatus, scientificName)

  pterido_names_exact_match <-
    exact_names_resolve_results %>%
    dplyr::filter(!is.na(scientificName)) %>%
    check_unique(clean_name)

  pterido_names_no_exact_match <-
    exact_names_resolve_results %>%
    dplyr::filter(is.na(scientificName))

  # Add species and drop sciname if needed based on resolve_to
  if(nrow(pterido_names_exact_match) > 0 && resolve_to == "species")
    pterido_names_exact_match <- add_parsed_names(
      pterido_names_exact_match, scientificName, species) %>%
    dplyr::select(-scientificName)

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
      dplyr::left_join(dplyr::select(names, clean_name, query = original_name), by = "clean_name") %>%
      dplyr::select(-clean_name) %>%
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
  gnr_results <- match_with_gnr(pterido_names_no_exact_match$clean_name, exclude_mult_matches = FALSE) %>%
    # Exit with error if no names matched
    assertr::verify(
      nrow(.) > 0,
      error_fun = function (errors, data = NULL) {stop("No names matched")})

  # Split GNR results into names not matched and matched
  pterido_names_no_fuzzy_match <- gnr_results %>%
    dplyr::filter(is.na(matched_name)) %>%
    dplyr::select(clean_name = query, fail_reason) %>%
    check_unique(clean_name)

  # (matched still includes things with multiple matches)
  pterido_names_matched <- gnr_results %>%
    dplyr::filter(!is.na(matched_name))

  # Resolve matched names to World Ferns first by scientific name,
  # then by taxon
  results_fuzzy_match <- tibble::tibble()

  results_fuzzy_match <- resolve_gnr_results_by_rank(
    pterido_names_matched,
    world_ferns,
    resolve_by_taxon = TRUE,
    resolve_by_species = FALSE,
    resolve_to = resolve_to
  ) %>%
    check_unique(query)

  if(nrow(results_fuzzy_match) > 0)
    results_fuzzy_match <- results_fuzzy_match %>%
    dplyr::rename(clean_name = query)

  ### Combining matching and resolving results ###
  results <-
    dplyr::bind_rows(
      excluded,
      pterido_names_no_fuzzy_match,
      pterido_names_exact_match,
      results_fuzzy_match
    ) %>%
    # Add back in original names
    dplyr::left_join(dplyr::select(names, original_name, clean_name), by = "clean_name") %>%
    # Rearrange columns
    dplyr::select(original_name, dplyr::everything()) %>%
    # Fill-in missing NAs
    dplyr::mutate_at(dplyr::vars(dplyr::contains("exclude")), ~tidyr::replace_na(., FALSE))

  # Conduct final check: make sure all original names are in the results
  assertthat::assert_that(
    isTRUE(
      all.equal(
        sort(results$original_name),
        sort(names_raw$original_name)
      )
    )
  )

  results

}
