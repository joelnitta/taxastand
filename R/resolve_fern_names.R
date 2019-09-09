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
#' names <- c("abacopteris insularis", "abacopteris philippinarum", "abacopteris rubra var. hirsuta")
#'
#' }
#'
#' @export
resolve_fern_names <- function (names, col_plants) {

  assertthat::assert_that(is.character(names))

  check_darwin_core_cols(col_plants)

  ### Format input data ###

  # Convert input name vector to tibble
  names_raw <- tibble::tibble(query_original = names)

  # Cleanup names for searching
  names <-
    names_raw %>%
    # Verify all scientific names are unique
    assertr::assert(assertr::is_uniq, query_original) %>%
    # Remove quotation marks
    dplyr::mutate(query = stringr::str_remove_all(query_original, '\\"')) %>%
    # Convert query to uppercase
    # (don't use stringr::str_to_sentence, will convert other letters to lower)
    dplyr::mutate(query = toupper_first(query)) %>%
    # Add genus
    dplyr::mutate(genus = genus_name_only(query))

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
    dplyr::mutate(non_pterido_genus = dplyr::case_when(
      genus %in% pterido_genera ~ FALSE,
      TRUE ~ TRUE
    ))

  # Exclude non-pteridophyte genera
  names <- dplyr::left_join(names, col_plants_genera, by = "genus")

  ### Continue with matching and resolving only for pterido names ###

  pterido_names <- dplyr::filter(names, !non_pterido_genus) %>% dplyr::pull(query)

  # Match all pteridophyte names with GNR
  gnr_results <- match_with_gnr(pterido_names)

  # Resolve synonyms:
  # scientificName, genus, specificEpithet, infraspecificEpithet are
  # the names matched to in World Ferns. If the original name was a
  # synonym, the accepted names are looked up and returned.
  # taxonomicStatus refers to the status of the matched name.
  resolved_names <- resolve_names_full(gnr_results$query, world_ferns) %>%
    dplyr::select(query, taxonomicStatus,
           scientificName, genus, specificEpithet, infraspecificEpithet)

  # Combine results
  final_names <- dplyr::select(
    names,
    query,
    query_original,
    non_pterido_genus) %>%
    dplyr::left_join(resolved_names, by = "query") %>%
    dplyr::select(-query) %>%
    dplyr::rename(query = query_original) %>%
    dplyr::mutate(species = jntools::paste3(genus, specificEpithet))

  # Conduct final checks:
  # Make sure all original names are in the results
  assertthat::assert_that(
    dplyr::anti_join(names_raw, final_names, by = c(query_original = "query")) %>%
      nrow == 0)

  assertthat::assert_that(
    nrow(names_raw) == nrow(final_names)
  )

  final_names
}
