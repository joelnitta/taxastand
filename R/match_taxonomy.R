#' Match species names to a reference database
#'
#' Match a vector of species names to a user-provided taxonomic reference
#' database in the [Darwin Core format](https://dwc.tdwg.org/terms/).
#'
#' For an example of a taxonomic reference database in Darwin Core format,
#' run `data(filmy_taxonomy)`.
#'
#' Fuzzy matching can be used by setting `max_dist` > 0. Fuzzy matching is
#' particularly useful for full scientific names (including author), as author
#' abbreviations and formatting vary widely.
#'
#' It is assumed that all names in `query` are of the same type. Types of
#' species names include:
#' - species: Species name not including any names below the
#' species level, e.g., "Iris pallida"
#' - taxon: Species name including names below the species level (if any),
#' e.g., "Iris pallida cengialti". Note that the rank of the infraspecific
#' name (e.g., "var.") is not included.
#' - scientific name: The full species name including infraspecific name (if any)
#' and author, e.g., "Iris pallida subsp. cengialti (Ambrosi ex A.Kern.) Foster".
#'
#' @param query Character vector; taxonomic names to resolve.
#' @param taxonomic_standard Dataframe of standard names to match to.
#' Must follow [Darwin Core format](https://dwc.tdwg.org/terms/).
#' @param match_by One of "species", "taxon", or "sciname" (scientific name);
#' the type of standard name to use for matching.
#' Should match the type of names in `query`.
#' @param max_dist Integer; maximum distance to use during fuzzy matching.
#' @param simple Logical; should the results include only a limited set of
#' columns? If FALSE, all columns in `taxonomic_standard` will be returned.
#'
#' @return Dataframe. Includes the following columns:
#' - `query`: the search query
#' - `n_hits`: the number of matches to the query
#' - `distance`: the number of characters differing between the query
#' and the match
#' - `match_to`: the name in `taxonomic_standard` that matched the query
#' - `match_by`: the type of species name that was used for matching
#'
#' ... and either all columns in `taxonomic_standard` or only a subset of
#' these if `simple` is TRUE.
#'
#' @examples
#' data(filmy_taxonomy)
#' example_species <- c("Vandenboschia speciosa", "this species")
#' match_taxonomy(example_species, filmy_taxonomy, "species")
#' match_taxonomy(example_species, filmy_taxonomy, "species", max_dist = 5)
#'
#' @export
match_taxonomy <- function (query,
                            taxonomic_standard,
                            match_by = c("species", "taxon", "sciname"),
                            max_dist = 0, simple = TRUE) {

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
    "taxonID", "acceptedNameUsageID", "parentNameUsageID",
    "taxonomicStatus", "taxonRank",
    "scientificName", "genericName", "genus", "specificEpithet", "infraspecificEpithet",
    "scientificNameAuthorship"
  )

  # - All colnames in original taxonomic standard dataframe
  # (should all be DarwinCore columns).
  darwin_original_cols <- colnames(taxonomic_standard)

  # Convert input name vector to tibble.
  names_to_resolve <- tibble::tibble(query = query)

  # Set name to use for matching to taxonomic standard.
  col_standard <- switch(match_by,
                         species = "speciesName",
                         taxon = "taxonName",
                         sciname = "scientificName")

  # Add non-DarwinCore columns to taxonomic_standard that
  # are needed for matching.
  taxonomic_standard <-
    taxonomic_standard %>%
    dplyr::mutate(
      # Species name (without infrasp. taxon), e.g., "Homo sapiens"
      speciesName = jntools::paste3(genericName, specificEpithet),
      # Most specific taxon name (incl. infrasp. taxon if it exists),
      # e.g. "Trichomanes radicans andrewsii"
      taxonName = jntools::paste3(genericName, specificEpithet, infraspecificEpithet)
    )

  ### Find matches ###
  # Use fuzzy matching if max_dist > 0
  if (max_dist > 0) {

    hits <- fuzzyjoin::stringdist_inner_join(
      x = names_to_resolve,
      # fuzzyjoin can't handle NAs in match column
      y = taxonomic_standard %>% dplyr::filter(!is.na(!!rlang::sym(col_standard))),
      by = c(query = col_standard),
      max_dist = max_dist,
      distance_col = "distance"
    ) %>%
      # Add the number of matches per query
      dplyr::group_by(query) %>%
      dplyr::mutate(n_hits = length(query)) %>%
      dplyr::ungroup() %>%
      # Add name the query matched to
      dplyr::mutate(match_to = !!rlang::sym(col_standard)) %>%
      # Add match method
      dplyr::mutate(match_by = match_by)

    # Use exact matching otherwise
  } else {

    hits <- dplyr::inner_join(
      x = names_to_resolve,
      y = taxonomic_standard,
      by = c(query = col_standard)) %>%
      dplyr::group_by(query) %>%
      dplyr::mutate(n_hits = length(query)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(distance = 0) %>%
      # Add name the query matched to
      # Note inner_join drops the RHS column after matching
      dplyr::mutate(match_to = query) %>%
      # Add match method
      dplyr::mutate(match_by = match_by)
  }

  # Find non-matches
  # Use fuzzy matching if max_dist > 0
  if (max_dist > 0) {

    missing <- fuzzyjoin::stringdist_anti_join(
      x = names_to_resolve,
      y = taxonomic_standard %>% dplyr::filter(!is.na(!!rlang::sym(col_standard))),
      by = c(query = col_standard),
      max_dist = max_dist,
      distance_col = "distance"
    ) %>%
      dplyr::mutate(n_hits = 0) %>%
      # Add name the query matched to
      dplyr::mutate(match_to = NA_character_) %>%
      # Add match method
      dplyr::mutate(match_by = match_by)

    # Use exact matching otherwise
  } else {

    missing <- dplyr::anti_join(
      x = names_to_resolve,
      y = taxonomic_standard,
      by = c(query = col_standard)
    ) %>%
      dplyr::mutate(n_hits = 0) %>%
      # Add name the query matched to
      dplyr::mutate(match_to = NA_character_) %>%
      # Add match method
      dplyr::mutate(match_by = match_by)

  }

  # Make sure we didn't miss anything
  assertthat::assert_that(
    all(c(hits$query, missing$query) %in% names_to_resolve$query),
    msg = "One or more queried names not in `hits` or `missing`"
  )

  ### Combine and output results ###
  results <- dplyr::bind_rows(hits, missing) %>%
    dplyr::select(c(match_cols, darwin_original_cols))

  if(isTRUE(simple)) results <- dplyr::select(results, c(match_cols,  darwin_simple_cols))

  results

}
