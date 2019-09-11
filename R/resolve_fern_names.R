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
#'   "Bulboschoenus fluviatilis")
#'
#' resolve_fern_names(names, col_plants, "scientific_name")
#' resolve_fern_names(names, col_plants, "species")
#'
#' }
#'
#' @export
resolve_fern_names <- function (names, col_plants, resolve_to = c("species", "scientific_name")) {

  assertthat::assert_that(is.character(names))
  assertthat::assert_that(assertthat::is.string(resolve_to))
  assertthat::assert_that(
    resolve_to %in% c("species", "scientific_name"),
    msg = "`resolve_to` must be one of 'species' or 'scientific_name'")


  check_darwin_core_cols(col_plants)

  ### Format input data ###

  # Convert input name vector to tibble
  names_raw <- tibble::tibble(original_name = names)

  # Cleanup names for searching
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

  ### Remove non-pteridophyte genera ###

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

  # Check for and exclude hybrids from names to resolve
  names <-
    names %>%
    dplyr::mutate(exclude_hybrid = dplyr::case_when(
      stringr::str_detect(gnr_query, " x ") ~ TRUE,
      stringr::str_detect(gnr_query, "\\u00d7") ~ TRUE, # unicode for batsu
      TRUE ~ FALSE
    ))

  ### Match pteridophyte names with GNR ###
  pterido_names <-
    names %>%
    dplyr::filter(!exclude_non_pterido_genus) %>%
    dplyr::filter(!exclude_hybrid) %>%
    dplyr::pull(gnr_query)

  gnr_results <- match_with_gnr(pterido_names, exclude_mult_matches = FALSE) %>%
    dplyr::rename(gnr_query = query)

  # Split GNR results into names not matched and matched
  pterido_names_not_matched <- gnr_results %>% dplyr::filter(!is.na(fail_reason)) %>%
    dplyr::select(gnr_query, fail_reason)

  # (matched still includes things with multiple matches)
  pterido_names_matched <- gnr_results %>% dplyr::filter(is.na(fail_reason))

  # There should not be any queried names in both sets
  assertthat::assert_that(
    nrow(dplyr::inner_join(pterido_names_matched, pterido_names_not_matched, by ="gnr_query")) ==
      0
  )

  # Resolve synonyms:
  # scientificName is the names matched to in World Ferns.
  # If the original name was a
  # synonym, the accepted names are looked up and returned.
  # taxonomicStatus refers to the status of the matched name.
  pterido_names_resolve_results <- resolve_names_full(
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
  not_resolved <- pterido_names_resolve_results %>%
    dplyr::filter(is.na(scientificName))

  pterido_names_resolved <- pterido_names_resolve_results %>%
    dplyr::filter(!is.na(scientificName)) %>%
    add_parsed_names(scientificName, species)

  # Split resolved names into those that had a single GNR match
  # and those that had multiple ones.
  pterido_names_resolved_single_matches <-
    pterido_names_resolved %>%
    dplyr::filter(assertr::is_uniq(gnr_query))

  pterido_names_resolved_mult_matches <-
    pterido_names_resolved %>%
    dplyr::filter(!assertr::is_uniq(gnr_query))

  ### Check for multiple matches resolving to the same scientific name
  # We will keep these (or species, depending on `resolve_to`)
  pterido_names_mult_matches_resolve_to_same_name <-
    pterido_names_resolved_mult_matches %>%
    dplyr::add_count(gnr_query, scientificName) %>%
    dplyr::filter(n > 1)

  # Collapse taxonomicStatus for mult matches that resolve to same name
  if(nrow(pterido_names_mult_matches_resolve_to_same_name) > 0) {
    pterido_names_mult_matches_resolve_to_same_name <-
      dplyr::summarize(
        pterido_names_mult_matches_resolve_to_same_name,
        gnr_query = unique(gnr_query),
        scientificName = unique(scientificName),
        taxonomicStatus = paste(taxonomicStatus, collapse = ", ")
      ) }

  # Also make list of failures that matched to different names
  pterido_names_mult_matches_resolve_to_diff_name <-
    pterido_names_resolved_mult_matches %>%
    dplyr::add_count(gnr_query, scientificName) %>%
    dplyr::filter(n == 1) %>%
    dplyr::select(gnr_query) %>%
    unique() %>%
    dplyr::mutate(match_fail_reason = "multiple matches")

  ### Check for multiple matches resolving to the same species
  # We will keep these (or scientific name, depending on `resolve_to`)

  # Check if any of the multiple matches ended up resolving to the same
  # species name. In that case, keep them.
  pterido_names_mult_matches_resolve_to_same_species <-
    pterido_names_resolved_mult_matches %>%
    dplyr::add_count(gnr_query, species) %>%
    dplyr::filter(n > 1)

  # Collapse taxonomicStatus for mult matches that resolve to same name
  if(nrow(pterido_names_mult_matches_resolve_to_same_species) > 0) {
    pterido_names_mult_matches_resolve_to_same_species <-
      dplyr::summarize(
        pterido_names_mult_matches_resolve_to_same_species,
        gnr_query = unique(gnr_query),
        species = unique(species),
        taxonomicStatus = paste(taxonomicStatus, collapse = ", ")
      ) }

  # Also make list of failures that matched to different species
  pterido_names_mult_matches_resolve_to_diff_species <-
    pterido_names_resolved_mult_matches %>%
    dplyr::add_count(gnr_query, species) %>%
    dplyr::filter(n == 1) %>%
    dplyr::select(gnr_query) %>%
    unique() %>%
    dplyr::mutate(match_fail_reason = "multiple matches")

  ### Combining matching and resolving results ###
  match_and_resolve_results_sciname <-
    dplyr::bind_rows(
      # Successes
      pterido_names_mult_matches_resolve_to_same_name,
      pterido_names_resolved_single_matches,
      # Failures
      not_resolved,
      pterido_names_mult_matches_resolve_to_diff_name,
      pterido_names_not_matched
    )

  match_and_resolve_results_species <-
    dplyr::bind_rows(
      # Successes
      pterido_names_mult_matches_resolve_to_same_species,
      pterido_names_resolved_single_matches,
      # Failures
      not_resolved,
      pterido_names_mult_matches_resolve_to_diff_species,
      pterido_names_not_matched
    )

  # Choose what to type of resuls to return
  # (resolved to scientific name or species)
  match_and_resolve_results <- switch(resolve_to,
    scientific_name = match_and_resolve_results_sciname,
    species =  match_and_resolve_results_species,
  )

  # Clean up exluded names for final join
  excluded <- names %>% dplyr::filter(exclude_non_pterido_genus|exclude_hybrid) %>%
    dplyr::select(-genus, -original_name) %>%
    unique

  # Combine final results
  results <-
    dplyr::left_join(
      dplyr::select(names, original_name, gnr_query),
      dplyr::bind_rows(match_and_resolve_results, excluded),
      by = "gnr_query"
    ) %>%
    dplyr::rename(query = original_name)

  # Conduct final checks:
  # Make sure all original names are in the results
  assertthat::assert_that(
    dplyr::anti_join(names_raw, results, by = c(original_name = "query")) %>%
      nrow == 0)

  # Make sure number of names matches between input and results
  assertthat::assert_that(
    nrow(names_raw) == nrow(results)
  )

  results <-
  dplyr::select(
    results,
    query,
    exclude_non_pterido_genus, exclude_hybrid, match_fail_reason,
    taxonomicStatus, scientificName, species) %>%
    dplyr::mutate(exclude_non_pterido_genus = tidyr::replace_na(exclude_non_pterido_genus, FALSE)) %>%
    dplyr::mutate(exclude_hybrid = tidyr::replace_na(exclude_hybrid, FALSE))

  if(resolve_to == "scientific_name") results <- dplyr::select(results, -species)

  if(resolve_to == "species") results <- dplyr::select(results, -scientificName)

  results

}
